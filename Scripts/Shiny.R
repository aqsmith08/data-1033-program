library(dplyr)
library(lubridate)
library(shiny)
library(ggvis)

file <- "1033_DLA_data_as_of_march_2015.csv"
dla <- read.csv(file, stringsAsFactors = FALSE)

# Create a year column using the lubridate package
dla$real_ship_date <- mdy_hms(dla$Ship.Date)
dla$ship_year <- year(dla$real_ship_date)

#########################################################
# Begin Shiny App
ui <- fluidPage(
  sliderInput(inputId = "num", 
              label = "Choose a number", 
              value = 25, min = 1, max = 100),
  plotOutput("hist")
)

server <- function(input, output) {
  output$hist <- renderPlot({
    hist(rnorm(input$num))
  })
}

shinyApp(ui = ui, server = server)