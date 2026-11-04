library(shiny)
library(tidyverse)
library(bslib)

# climate_data <- placeholder for data loading; 

# starting to update app with basic layout and themes, will continue to add to this

ui <- navbarPage(
  title = "Local Climate Explorer", #name of the app
  position = "static-top", #keeps it at the top of the window
  collapsible = TRUE, #will get smaller if the screen is smaller
  fluid = TRUE, #keeps it proportional to the size of the window
  theme = bs_theme(version = 5, bootswatch = "flatly"), #theme
  
  #set up the different tabs
  tabPanel("About",
              h2("How has the climate changed where you live?"),
              p("A demonstration app for teachers and students. With this app, you can explore
                daily temperatures and precipitation data for cities around the U.S. since 1990,
                test whether trends are statistically meaningful, and compare NASA satellite-modeled
                data with NOAA weather station records."),
           #add cards on the "About" Panel
           card(
             card_header("Questions you can answer with this app"),
             card_body(
               layout_column_wrap(
                 width = "220px", 
                 card(class = "bg-light", card_body(
                   strong("Is my city warming?"),
                   p(class = "text-muted small mb-0", "Explore Tab, then Statistics > Trends")
                 )),
                 card(class = "bg-light", card_body(
                   strong("Does satellite data match ground station data?"),
                   p(class = "text-muted small mb-0", "Statistics > Satellite vs. Station")
                 )),
                 card(class = "bg-light", card_body(
                   strong("Are recent decades different?"),
                   p(class = "text-muted small mb-0", "Statistics > Decades and Regions")
                 )),
                 card(class = "bg-light", card_body(
                   strong("Was this summer unusual?"),
                   p(class = "text-muted small mb-0", "Statistics > Compare Two Time Periods")
                 )),
 #need to add additional cards with data sources, names, etc                
               )
             )
           )
    ),
  
  tabPanel("Explore",
           sidebarLayout(
             sidebarPanel(
               h4("Choose your Data"),
                      p(class = "text-mut4ed small mb-0", "Tab layout and controls: Role 3"),
               selectInput("cities", strong("Cities"), choices = NULL, multiple = TRUE), #multple cities or just 1?
               radioButtons("source", strong("Data source"), 
                            choices = c("NASA POWER (satellite)", "NOAA weather station", "Both, overlaid")
                            ),
               selectInput("variable", strong("Variable"),
                           choices = c("Mean temperature", "Max temperature",
                                       "Min temperature", "Precipitation")
                           ),
               dateRangeInput("dates", strong("Date range"),
                              start = "1990-01-01", #start of data
                              end = "2026-10-25" #change to whatever our end date is
                           )
               
             ),
             mainPanel(
               p("Map, time series, and summary table will go here.")
             )
           )
           ),
  navbarMenu("Statistics",
             tabPanel("Trends",                 p("Role 1")),
             tabPanel("Satellite vs. Station",  p("Role 2")),
             tabPanel("Decades and Regions",    p("Role 3")),
             tabPanel("Compare Two Periods",    p("Role 1"))
             )
  
  )

server <- function(input, output, session) {
  # Each stats panel's outputs go here, calling the functions from the R folder
  }

shinyApp(ui = ui, server = server)