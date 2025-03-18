
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


# read in data 

flow.dat <- read_feather("data/stites_flow")

daily.dat <- read_feather("data/daily")

individuals.dat <- read_feather("data/individuals")

lastweek <- individuals.dat %>% 
  filter(sf_entry_final>=today()-days(7))

# make the slider input for date range

slider_min <- as.Date(min(individuals.dat$sf_entry_final))

user_dates <-     
  sliderInput(inputId = "user_dates",
              label="Choose a Date Range",
              min=slider_min,
              max=today(),
              value=c(today()-months(1),today()))



ui <- page_navbar(
  
  title="South Fork Clearwater Steelhead",
  
  theme = bs_theme(preset="cyborg"),
  
  sidebar=sidebar(width=500,
    
    accordion(
      accordion_panel(
        
        "Explore Data",
        
        "Options",
        user_dates
        
      )
      
    )),
    
    nav_panel("Explore Data",
            
              #   
              # card(card_body(
              #   strong("Unique PIT Tags, Current Spawn Year: "),
              #   textOutput("unique_value")
              # )),
              
              layout_columns(
                
                value_box(
                  "Spawn Year to date",
                  nrow(individuals.dat)
                  
                  
                ),
                
                value_box(
                  "New in the Last Week",
                  nrow(lastweek)
                  
                )
                
                ),
              
              page_fillable(
               
              layout_columns(
                   
              card(card_header("Discharge at Stites"),
                               plotlyOutput("flow_plot"),
                   full_screen = T),
                
             card(card_header("Unique Fish In"),
                  plotlyOutput("entry_plot"),
                  full_screen = T)
              )
              )
              
    )
    
)

server <- function(input,output,session){
  
  # make the flow plot as a reactive
  
  flowplot_reactive <- reactive({
    
    plot_min <- min(input$user_dates)
    plot_max <- max(input$user_dates)
    
    flow.plot <- flow.dat %>% 
      mutate(date=as_date(date)) %>% 
      ggplot(aes(x=date,y=mean_discharge,group=group))+
      geom_line(aes(text=str_c(" Date:",date,
                               "<br>","Mean Discharge (cfs): ",mean_discharge,
                               sep=" ")))+
      scale_x_date(date_breaks = "1 month", date_labels="%b %Y",
                   limits=c(as.Date(plot_min),as.Date(plot_max)))+
      theme_bw()+
      labs(x="",y="Mean Discharge at Stites")
    
  })
  
  # Render the flow plot as a plotly object
  
  output$flow_plot <- renderPlotly({
    
    plot1 <- flowplot_reactive()
    
    ggplotly(plot1,
             tooltip=c("text"))
    
  })
  
  # make the plot for daily numbers of pit tags entering
  
  dailyentry_reactive <- reactive({
    
    plot_min <- min(input$user_dates)
    plot_max <- max(input$user_dates)
    
    entry.plot <- ggplot()+
      geom_col(data=daily.dat,fill="black",color="grey",
               aes(x=sf_final_date,y=n,group=spawn_year,
                   text=str_c(" Date:",sf_final_date,
                              "<br>","Number Steelhead Entered:",n,
                              sep=" ")))+
      scale_x_date(date_breaks = "1 month", date_labels="%b %Y",
                   limits=c(as.Date(plot_min),as.Date(plot_max)))+
      theme_bw()+
      labs(x="Latest entry date to SF Clearwater",
           y="Number of unique PIT-Tagged Steelhead")
    
  })
  
  # Render the daily tally plot as a plotly object
  
  output$entry_plot <- renderPlotly({
    
    plot1 <- dailyentry_reactive()
    
    ggplotly(plot1,
             tooltip=c("text"))
    
  })
  
  
}

shinyApp(ui, server)


