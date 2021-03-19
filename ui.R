library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Density plot"),
  
  sidebarPanel(
    sliderInput("plotWidth", "Plot width (px)", 200, 2000, 500),
    sliderInput("plotHeight", "Plot height (px)", 200, 2000, 500),
    checkboxInput("log", "Log transformation", value = FALSE),
    checkboxInput("free_y", "Free (non-fixed) axis scales", value = TRUE),
    sliderInput("transparency", "Colour transparency", 0, 1, 0.25),
    textInput("xlab", "x axis label", "Group"),
    textInput("ylab", "y axis label", "Value")
  ),
  
  mainPanel(
    uiOutput("reacOut")
  )
  
))