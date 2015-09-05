library(shiny)
source("./graphs.R")
source("./graph_calls.R")

shinyServer(function(input, output) {

  output$binomialPlot1 <- renderPlot({
    binomial.static()
  }, height = 440, width = 400)
  
  output$binomialPlot2 <- renderPlot({
    binomial.dynamic(input$n, input$p)
  }, height = 400, width = 400)
  
  output$poissonPlot1 <- renderPlot({
    poisson.static()
  }, height = 440, width = 400)
  
  output$poissonPlot2 <- renderPlot({
    poisson.dynamic(input$k, input$lambda)
  }, height = 400, width = 400)
  
  output$betaPlot1 <- renderPlot({
    beta.static()
  }, height = 470, width = 400)
  
  output$betaPlot2 <- renderPlot({
    beta.dynamic(input$alpha, input$beta)
  }, height = 400, width = 400)
  
  output$exponentialPlot1 <- renderPlot({
    exponential.static()
  }, height = 440, width = 400)
  
  output$exponentialPlot2 <- renderPlot({
    exponential.dynamic(input$rate)
  }, height = 400, width = 400)
 
   output$normalPlot1 <- renderPlot({
    normal.static()
  }, height = 450, width = 400)
  
  output$normalPlot2 <- renderPlot({
    normal.dynamic(input$mean, input$std)
  }, height = 400, width = 400)
  
  output$uniformPlot1 <- renderPlot({
    uniform.static()
  }, height = 440, width = 400)
  
  output$uniformPlot2 <- renderPlot({
    uniform.dynamic(input$a, input$b)
  }, height = 400, width = 400)
  
 
})


