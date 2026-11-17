library(shiny)
library(tidyverse)
library(bslib)

# climate_data <- placeholder for data loading; 

# starting to update app with basic layout and themes, will continue to add to this

ui <- navbarPage(
  title = "U.S.Cities Climate Explorer", #name of the app
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
                   strong("Is my city getting warmer?"),
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
                 ))
                 #need to add additional cards with data sources, names, etc                
               )
             )
           ),
           layout_columns(
             col_widths = c(8, 4),
             
             card(
               card_header(strong("Data Sources")),
               card_body(
                 p(strong("NASA POWER"), "daily point data, modeled from satellite and reanalysis products (grid-cell estimates)"),
                 p(strong("NOAA GHCN-Daily"), "weather station observations, quality-flagged values removed"),
                 p(strong("U.S. Census Gazetteer"), "city names and coordinates"),
                 p(class = "text-muted small mb-0", "Coverage: 48 cities from across the United States, January 1990 to November 2026. Date pulled [DATE]")
               )
             ),
             
             card(class = "bg-dark",
               card_header(strong("How to Use the App")),
               card_body(
                 p("1. Pick cities and a varible in Explore"),
                 p("2. Look for patterns in the map and charts"),
                 p("3. Test them in Statistics"),
                 p("4. Read the plain-language interpretation")
               )
             )
           ),
           p(class = "text-muted small mb-0", "DATA 613 Group Project. Prashant Choudhary, Brook Estifanos, Rachel Stagner. References listed in the vignette")
  ),
  
  
  tabPanel("Explore",
           sidebarLayout(
             sidebarPanel(
               h4("Choose your Data"),
               p(class = "text-muted small mb-0", "Tab layout and controls: Role 3"),
               selectInput("cities", strong("Cities"), choices = NULL, multiple = TRUE), #multiple cities or just 1?
               radioButtons("source", strong("Data source"), 
                            choices = c("NASA POWER (satellite)", "NOAA weather station", "Both, overlaid")
               ),
               selectInput("variable", strong("Variable"),
                           choices = c("Mean temperature (°C)", "Max temperature (°C)",
                                       "Min temperature (°C)", "Precipitation (mm)") #get rid of precip
               ),
               dateRangeInput("dates", strong("Date range"),
                              start = "1990-01-01", #start of data
                              end = "2026-10-25" #change to whatever our end date is
               ),
               actionButton("button", "Submit")
               
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