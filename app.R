
library(shiny)
library(tidyverse)
library(bslib)
library(DT)
library(shinyWidgets)
library(conflicted)
library(plotly)

conflicts_prefer(DT::renderDT,
                 dplyr::filter,
                 dplyr::lag)

user_dates <-     
  sliderInput(inputId = "user_dates",
              label="Choose a Date Range",
              min=today()-years(1),
              max=today(),
              value=c(today()-months(6),today()))



ui <- page_navbar(
  
  title="South Fork Clearwater Steelhead",
  
  theme = bs_theme(preset="cyborg"),
  
  sidebar=sidebar(width=500,
    
    accordion(
      accordion_panel(
        
        "Options",
        user_dates
        
      )
      
    )
    
  )
    

  
  
)



shinyApp(ui, function(input, output) {})


