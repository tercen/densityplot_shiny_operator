library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Boxplot"),
  
  sidebarPanel(
    sliderInput("plotWidth", "Plot width (px)", 200, 2000, 500),
    sliderInput("plotHeight", "Plot height (px)", 200, 2000, 500),
    checkboxInput("log", "Log transformation", value = FALSE),
    sliderInput("transparency", "Colour transparency", 0, 1, 0.1),
    textInput("xlab", "x axis label", "Group"),
    textInput("ylab", "y axis label", "Value")
  ),
  
  mainPanel(
    uiOutput("reacOut")
  )
  
))