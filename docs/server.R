library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)
library(maps)
library(plotly)

# Load data
data = read.csv('COVID_data_2021-11-21.csv')
data("world.cities")

mydata <- world.cities %>% filter(capital == 1) %>%
      select(country = country.etc, lat, lng = long) %>%
      left_join(data, ., by = "country")

shinyServer(
      function(input, output){
            
            # get input values
            selected_outcome <- reactive({
                  ifelse(input$outval == '1', 'cumulative_cases', 
                         ifelse(input$outval == '2', 'cumulative_deaths', 
                                ifelse(input$outval == '3', 'cumulative_cases_per_million',
                                       ifelse(input$outval == '4','cumulative_deaths_per_million',''))))
            })
            
            outcome <- reactive({
                  ifelse(input$outval == '1', 'Cases:', 
                         ifelse(input$outval == '2', 'Deaths:', 
                                ifelse(input$outval == '3', 'Cases per million:',
                                       ifelse(input$outval == '4','Deaths per million:',''))))
            })
            
            inputcountry <- reactive({
                  input$countryval
            })
            
            inputdates <- reactive({
                  input$dateval
            })
            
            # set output leaflet map
            output$mymap <- renderLeaflet({
                  leaflet() %>%
                        addTiles() 
            })
            
            
            
            observe({
                  
                  # filter data for map
                  df <- reactive({
                        mydata %>%
                              dplyr::filter(country %in% inputcountry()[1:length(inputcountry())],
                                            as.Date(date) >= inputdates()[1],
                                            as.Date(date) <= inputdates()[2]) %>%
                              select("country", selected_outcome(), 'lat', 'lng') %>%
                              `colnames<-` (c('country', 'myoutcome', 'lat', 'lng')) %>%
                              group_by(country) %>%
                              filter(myoutcome == max(myoutcome)) %>%
                              mutate(content = paste("<p> <b>", country, "</b> </br>",
                                                     outcome(), myoutcome, "</p>"))
                  })
                  # filter data for plot
                  df2 <- reactive({
                        mydata %>%
                              dplyr::filter(country %in% inputcountry()[1:length(inputcountry())],
                                            as.Date(date) >= inputdates()[1],
                                            as.Date(date) <= inputdates()[2]) %>%
                              select("country", selected_outcome(), 'date') %>%
                              `colnames<-` (c('country', 'myoutcome', 'date')) 
                        # mutate(date = as.Date(date))
                  })
                  
                  
                  # Create map
                  mymap <- leafletProxy("mymap")
                  mymap %>% clearMarkers()
                  
                  
                  leafletProxy("mymap", data = df()) %>%
                        addCircleMarkers(
                              radius = 10,
                              color = '#E94F19',
                              stroke = FALSE,
                              fillOpacity = 0.5,
                              popup = df()$content) 
                  
                  # Create plot 
                  output$myplot <- renderPlotly({
                        if(nrow(df2())>0){
                              p <- ggplot(df2(), aes(x = as.Date(date), y = myoutcome,
                                                     color = country,
                                                     text = paste0(
                                                           date, '<br>', 
                                                           country, ': ', 
                                                           myoutcome))) + 
                                    geom_line() + 
                                    geom_point() +
                                    labs(x = 'Date', y = 'Cumulative', 
                                         title = gsub(':', '', outcome()))+
                                    theme_light()+
                                    theme(legend.position = 'right', 
                                          legend.title = element_blank(),
                                          panel.grid.minor = element_blank(),
                                          plot.title = element_text(hjust = 0, color = '#E94F19'))
                              ggplotly(p, tooltip = 'text') 
                        }else{
                              p <- plotly_empty(type = "scatter", mode = "markers") %>%
                                    config(
                                          displayModeBar = FALSE
                                    ) %>%
                                    layout(
                                          title = list(
                                                text = 'Select countries first',
                                                yref = "paper",
                                                y = 0.5
                                          )
                                    )
                              
                        }
                        })
                        
                        
                        
                        
                  })
                  
                  
                  # url and text data info
                  url <- a("Johns Hopkins Center for Systems Science and Engineering.", 
                           href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series")
                  output$myurl <- renderUI({
                        
                        tagList("Adapted from timeline data published by", url)
                  })
                  
                  output$mytext <- renderText({
                        "Data downloaded on 22nd November 2021"
                  })
            }
            )
            