
library(shiny)
library(tidyverse)
library(bslib)
library(DT)
library(shinyWidgets)
library(conflicted)
library(plotly)
library(arrow)
library(dataRetrieval)
library(bsicons)
library(viridis)
library(fontawesome)

conflicts_prefer(DT::renderDT,
                 dplyr::filter,
                 dplyr::lag,
                 plotly::layout)


# read in data 

flow.dat <- read_feather("data/stites_flow")

daily.dat <- read_feather("data/daily")

individuals.dat <- read_feather("data/individuals")

alldaily.dat <- read_feather("data/alldaily")

projections.dat <- read_feather("data/projections")

# calculate estimate of how much of the run
# is complete

run_stats <- alldaily.dat %>% 
  filter(spawn_year<2025) %>% 
  group_by(spawn_year) %>% 
  mutate(total_n=sum(n),
         prop_complete=daily_cumulative_n/total_n) %>% 
  group_by(dummy_sfentry_date) %>% 
  summarize(median_percentcomplete=round(median(prop_complete)*100),
            min_percentcomplete=round(min(prop_complete)*100),
            max_percentcomplete=round(max(prop_complete)*100))

today_dummy <- tibble(date=today()) %>% 
  mutate(doy=yday(date),
         dummy=if_else(doy<182,
                       as.Date(doy,origin="1977-12-31"),
                       as.Date(doy,origin="1976-12-31"))) %>% 
  pull(dummy)


today_run <- run_stats %>% 
  filter(dummy_sfentry_date==today_dummy)

# calculate how many new tags passed in the last week

lastweek <- individuals.dat %>% 
  filter(sf_entry_final>=today()-days(7))

# make the slider input for date range

slider_min <- as.Date(min(individuals.dat$sf_entry_final))

user_dates <-     
  sliderInput(inputId = "user_dates",
              label="Choose a Date Range",
              min=slider_min,
              max=today(),
              value=c(as_date("2025-01-01"),today()))



ui <- page_navbar(
  
  title="South Fork Clearwater Steelhead",
  
  theme = bs_theme(preset="cyborg"),
  
  sidebar=sidebar(width=500,
    
    accordion(
      accordion_panel(
        
        "Explore Data",
        
        user_dates
        
      )
      
    )),
    
    nav_panel("Explore Data",
            
              
              layout_columns(
                
                value_box(
                  title="Spawn Year to date",
                  value=nrow(individuals.dat),
                  showcase=fa("fish-fins")
                  
                ),
                
                value_box(
                  title="New in the Last Week",
                  value=nrow(lastweek),
                  showcase=bs_icon("graph-up-arrow")
                  
                ),
                
                value_box(
                  
                  title="Estimated Percent of Run Complete",
                  value=str_c(today_run$median_percentcomplete, "%",
                        sep=" "),
                  showcase = bs_icon("circle-half"),
                  p(str_c("Range:",str_c(today_run$min_percentcomplete,"%",sep=" "),
                          "-",
                          str_c(today_run$max_percentcomplete,"%",sep=" "),
                          sep=" "))
                  
                )
                
                ),
              
              page_fillable(
               
              layout_columns(
                
                col_widths = c(6,6,6),
                   
              card(card_header("Discharge at Stites"),
                               plotlyOutput("flow_plot"),
                   full_screen = T),
              
              card(card_header("Year-to-date Totals"),
                   plotlyOutput("comp_plot"),
                   full_screen = T),
                
             card(card_header("Unique Fish In"),
                  plotlyOutput("entry_plot"),
                  full_screen = T),
             
       
             
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
      scale_x_date(date_breaks = "1 month", date_labels="%b",
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
      scale_x_date(date_breaks = "1 month", date_labels="%b",
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
  
  # make the plot comparing cumulative numbers of pit tags entering among
  # years and showing projections 
  
  compplot_reactive <- reactive({

    
    plot_lim.dat <- tibble(min_doy=yday(min(input$user_dates)),
                           max_doy=yday(max(input$user_date))) %>%
      mutate(plot_min=if_else(min_doy<182,
                              as.Date(min_doy,origin="1977-12-31"),
                              as.Date(min_doy,origin="1976-12-31")),
             plot_max=if_else(max_doy<182,
                              as.Date(max_doy,origin="1977-12-31"),
                              as.Date(max_doy,origin="1976-12-31")))
  
    
    comp.plot <- alldaily.dat %>% 
      ggplot(aes(x=dummy_sfentry_date,y=daily_cumulative_n,
                 group=spawn_year,color=as.factor(spawn_year)))+
      geom_line(aes(text=str_c(" Date:",format(dummy_sfentry_date, "%b %d"),
                               "<br>",
                               "Spawn Year:",spawn_year,
                               "<br>",
                               "Number In:",round(daily_cumulative_n),sep=" ")))+
      geom_point(data=projections.dat,
                 aes(x=dummy_sfentry_date,y=sy_total,
                     text=str_c(" Date:",format(dummy_sfentry_date, "%b %d"),
                                "<br>",
                                "Projected",projection_category,round(sy_total),
                                sep=" ")))+
      theme_bw()+
      scale_color_viridis(discrete = T)+
      scale_x_date(date_breaks = "1 month", date_labels="%b",
                   limits=c(as.Date(plot_lim.dat$plot_min),as.Date(plot_lim.dat$plot_max)))+
      labs(x="Date of entry to South Fork Clearwater",
           y="# PIT Tags in SF, Year-To-Date",
           color="")
    comp.plot
    
  })
  
  # Render the comp plot as a plotly object
  
  output$comp_plot <- renderPlotly({
    
    plot1 <- compplot_reactive()
    
    ggplotly(plot1,
             tooltip=c("text")) 
    
  })
  
}

shinyApp(ui, server)


