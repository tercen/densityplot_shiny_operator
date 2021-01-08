library(shiny)
library(tercen)
library(dplyr)
library(tidyr)
library(ggplot2)

############################################
#### This part should not be included in ui.R and server.R scripts
getCtx <- function(session) {
  ctx <- tercenCtx(stepId = "635b15c9-55d2-466d-bda9-6e9469b67532",
                   workflowId = "01cb95cd7b746443ed9f40625200ef4f")
  return(ctx)
}
####
############################################

ui <- shinyUI(fluidPage(
  
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

server <- shinyServer(function(input, output, session) {
  
  dataInput <- reactive({
    getValues(session)
  })
  
  output$reacOut <- renderUI({
    plotOutput(
      "main.plot",
      height = input$plotHeight,
      width = input$plotWidth
    )
  }) 
  
  output$main.plot <- renderPlot({
    
    values <- dataInput()
    
    df <- values$data
    in.col <- values$colors
    
    input.par <- list(
      log = input$log,
      transparency = input$transparency,
      xlab = input$xlab,
      ylab = input$ylab
    )
    
    if(input.par$log) df$.y <- log1p(df$.y)
    
    fill.col <- NULL
    if(length(unique(in.col)) > 1) fill.col <- as.factor(in.col)
    
    theme_set(theme_minimal())
    
    plt <- ggplot(df, aes(.y, fill = fill.col)) + 
      geom_density(alpha = input.par$transparency) + labs(x = input.par$xlab, y = input.par$ylab, fill = "Legend")
    
    if(!is.null(df$cnames)) plt <- plt + facet_wrap(~ cnames)
    
    plt
    
  })
  
})

getValues <- function(session){
  
  ctx <- getCtx(session)
  
  values <- list()
  
  values$data <- ctx %>% select(.y, .ri, .ci) %>%
    group_by(.ri)
  
  values$colors <- NA
  if(length(ctx$colors)) values$colors <- ctx$select(ctx$colors[[1]])[[1]]
  
  values$rnames <- ctx$rselect()[[1]]
  names(values$rnames) <- seq_along(values$rnames) - 1
  values$data$rnames <- values$rnames[as.character(values$data$.ri)]
  
  if(nchar(values$rnames) == 0) values$data$rnames <- values$colors
  
  values$cnames <- ctx$cselect()[[1]]
  names(values$cnames) <- seq_along(values$cnames) - 1
  values$data$cnames <- values$cnames[as.character(values$data$.ci)]
  
  return(values)
}

runApp(shinyApp(ui, server))  
