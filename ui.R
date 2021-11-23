library(shiny)
library(shinyWidgets)
library(leaflet)
library(plotly)

# Load data
data = read.csv('COVID_data_2021-11-21.csv')

# Define UI for application that reports COVID-19 cases and deaths
shinyUI(fluidPage(
   theme = bslib::bs_theme(bootswatch = "united"),
   
   # Application title
   titlePanel('COVID-19 tracker'),
   
   # Sidebar with drop-down input for the countries, checkbox input for the 
   # outcome and picker input for the dates range
   
   sidebarLayout(
      sidebarPanel(
         helpText(h6("Create maps  and plots with COVID-19 information.")),
         pickerInput("countryval", "Select Countries", 
                     choices = unique(data$country[order(data$country)]), 
                     options = list(`actions-box` = TRUE), multiple = TRUE,
                     selected = 'UK'),
         radioButtons("outval", "Select Outcome",
                      c("Cases (total)" = "1",
                        "Deaths (total)" = "2",
                        "Cases per million" = "3",
                        "Deaths per million" = "4")),
         sliderInput('dateval', 'Select Dates Range', 
                     value = c(as.Date('2020-01-22'), as.Date('2021-11-21')), 
                     min = as.Date('2020-01-22'), max = as.Date('2021-11-21'),
                     timeFormat="%Y-%b-%d"
         )
      ),
      
      mainPanel(
         tabsetPanel(
            # Show map of selected countries with outcome
            tabPanel('Map', leafletOutput("mymap")),
            
            # Show cumulative plots of outcome
            tabPanel('Plot', plotlyOutput("myplot")),
            
            # Show data info
            tabPanel('Data', uiOutput("myurl"), textOutput("mytext"),
                     tags$head(tags$style("#mytext{
                                 font-size: 12px;
                                 font-style: italic;
                                 }")))
         )
      )
   )
)
)