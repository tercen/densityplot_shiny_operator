library(shiny)
library(tercen)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)

############################################
#### This part should not be included in ui.R and server.R scripts
getCtx <- function(session) {
  ctx <- tercenCtx(stepId = "a336cd36-980d-4e0e-9cd8-7db30ff081ed",
                   workflowId = "2553cb89b6ec3bc593e238e0df01901f")
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
    checkboxInput("free_y", "Free (non-fixed) axis scales", value = TRUE),
    sliderInput("transparency", "Colour transparency", 0, 1, 0.25),
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
    in.lab <- values$labels
    
    input.par <- list(
      log = input$log,
      transparency = input$transparency,
      xlab = input$xlab,
      ylab = input$ylab,
      free_y = input$free_y
    )
    
    if(input.par$log) df$.y <- log1p(df$.y)
    
    fill.col <- NULL
    if(length(unique(in.col)) > 1) fill.col <- as.factor(in.col)
    grp <- NULL
    if(length(unique(in.lab)) > 1) grp <- as.factor(in.lab)
    
    theme_set(theme_minimal())

    groupColors <- NULL
    if(length(grp) > 1 && length(fill.col) > 1) {
      cate <- paste0(fill.col, " - ", grp)
      
      nb.cols <- length(unique(fill.col))
      groupColors <- colorRampPalette(brewer.pal(6, "Set2"))(nb.cols)
      names(groupColors) <- unique(fill.col)
      colo <- groupColors[fill.col]
      groupColors <- colo[!duplicated(cate)]
      names(groupColors) <- unique(cate)
      fill.col <- cate
    }

    plt <- ggplot(df, aes(x = .y, fill = fill.col)) + 
      geom_density(alpha = input.par$transparency) +
      labs(x = input.par$xlab, y = input.par$ylab, fill = "Legend")
    if(!is.null(groupColors)) plt <- plt + scale_fill_manual(values=groupColors)
    if(!is.null(df$cnames)) plt <- plt + facet_wrap(~ cnames, scales=ifelse(input.par$free_y, "free", "fixed"))
    
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
  values$labels <- NA
  if(length(ctx$labels)) values$labels <- ctx$select(ctx$labels[[1]])[[1]]
  
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
