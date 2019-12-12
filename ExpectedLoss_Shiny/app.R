#
# INB App for Expected Loss RMD
#

library(shiny)
library(knitr)
library(kableExtra)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(formattable)
library(ggthemes)
library(tidyverse)
library(fGarch)
library(scales)
library(DT)

shinyApp(
  ui <- fluidPage(
    fluidRow(
      column(3,
             chooseSliderSkin("Flat"),
             h3("Select Your INB Distribution Inputs"),
             sliderInput("mean", label = "Mean",
                         min = -100, max = 100, value = 100, step = 1),
             sliderInput("sd", label = "Standard Deviation",
                         min = 0, max = 1600, value = 800, step = 10),
             sliderInput("skew", label = "Skew",
                         min = -10, max = 10, value = -9, step = 0.2)
      ),
      column(9,
             plotOutput(outputId = "dense")
      )
      ),
    htmlOutput(outputId = "means"),
    htmlOutput(outputId = "table")
  ),
  
  server <- function(input, output){
    
    v <- reactive({
      if(input$skew != 0){
        df <- as.data.frame(rsnorm(10000, mean = input$mean, sd = input$sd, xi = input$skew))      # Data for plot
      } else {
        df <- as.data.frame(rnorm(10000, mean = input$mean, sd = input$sd))      # Data for plot
      }
     
      names(df) <- "INB"
      
      df
    })
    
    df <- reactive({
      drug_A_nmb <- rnorm(10000, mean = 2555322, sd = 300000) # create arbitrary drug_A_nmb for given WTP
      drug_B_nmb <- v() + drug_A_nmb
      nmb_df <- cbind.data.frame(drug_A_nmb, drug_B_nmb)
      
      ce_matrix <- matrix(0,
                          nrow = 1,
                          ncol = 2,
                          dimnames = list(c("Proportion Cost Effective"),
                                          c("DrugA", "DrugB")))
      
      el_matrix <- matrix(0,
                          nrow = 1,
                          ncol = 2,
                          dimnames = list(c("Expected Loss"),
                                          c("DrugA", "DrugB")))
      
      nmb <- nmb_df
      max_nmb <- max.col(nmb)                       # selecting column with highest nmb in each iteration
      prop_ce <- prop.table(table(max_nmb))                   # proportion in each column
      ce_matrix[1, as.numeric(names(prop_ce))] <- prop_ce  # filling in cost effective matrix created above with prop 
      
      loss_matrix <- nmb[cbind(1:10000,max_nmb)] - nmb  # calculating loss of sub-optimal strategy in each iteration                                       
      
      el_matrix[1,] <- colMeans(loss_matrix)              # average loss by strat for each wtp threshold
      f <- rbind.data.frame(el_matrix, ce_matrix) %>% rownames_to_column(var = "Analysis")
      
      f
    })
    
    
    output$dense <- renderPlot({
      
      m_v <- mean(v()$INB)
      med_v <- median(v()$INB)
      
      reactive_fill <- if(sign(m_v) != sign(med_v)){
        "firebrick1"
      } else {
        "grey"
      }
      ggplot(v()) +
        geom_density(aes(x = INB), fill = reactive_fill, alpha = 2/3) +
        theme_hc() +
        guides(fill = FALSE) +
        scale_x_continuous(labels = dollar_format()) +
        labs(
          title = "Incremental Net Benefit Distribution"
        ) +
        xlim(c(-3000,3000))
      
      
    })
    
    output$means <-  renderText({
      m_v <- mean(v()$INB)
      med_v <- median(v()$INB)
      
      data_f <- data.frame(
        "Mean" = m_v,
        "Median" = med_v
      ) %>%
        mutate(
          Mean = ifelse(Mean > 0, 
                        cell_spec(Mean, "html", color = "green"),
                        cell_spec(Mean, "html", color = "red")),
          Median = ifelse(Median > 0,
                          cell_spec(Median, "html", color = "green"),
                          cell_spec(Median, "html", color = "red"))
        ) %>% 
        kable("html", escape = FALSE) %>% 
        kable_styling(bootstrap_options = c("striped", "hover"),
                      full_width = FALSE)
      
    })
    
    
    output$table <- renderText({
      df() %>% 
        kable("html", escape = FALSE) %>% 
        kable_styling(bootstrap_options = c("striped", "hover"))
    })}
)

# Run the application 
shinyApp(ui = ui, server = server)

