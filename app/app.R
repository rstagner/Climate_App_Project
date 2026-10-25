library(shiny)
library(tidyverse)

# climate_data <- placeholder for data loading; 

# right now, this is the base shiny app code; will update with 
# mockup of the app soon

ui <- fluidPage(
  titlePanel("App title"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins", "Number of bins:", min = 5, max = 50, value = 30)
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

server <- function(input, output, session) {
  output$distPlot <- renderPlot({
    hist(faithful$waiting, breaks = input$bins,
         main = "Waiting time between eruptions", xlab = "Minutes")
  })
}

shinyApp(ui = ui, server = server)