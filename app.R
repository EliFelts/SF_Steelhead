
library(shiny)
library(tidyverse)
library(bslib)
library(DT)
library(shinyWidgets)
library(conflicted)
library(plotly)
library(arrow)
library(dataRetrieval)

conflicts_prefer(DT::renderDT,
                 dplyr::filter,
                 dplyr::lag)


# do the stites data update in here?



# make the slider input for date range

user_dates <-     
  sliderInput(inputId = "user_dates",
              label="Choose a Date Range",
              min=today()-years(1),
              max=today(),
              value=c(today()-months(6),today()))



ui <- page_sidebar(
  
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

server <- function(input,output,session){
  
  
}

shinyApp(ui, server)


