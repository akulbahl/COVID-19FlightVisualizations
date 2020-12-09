library(shiny)

port <- Sys.getenv('PORT')
shiny::runApp(
  appDir = "C:\\Users\\Akul\\Desktop\\DSA\\Projects\\R Visualization\\RShinyFlightVisualizations",
  host = '0.0.0.0',
  port = as.numeric(port)
)