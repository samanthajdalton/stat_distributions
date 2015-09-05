library(shiny)

shinyUI(navbarPage("Math Brushups",


######################## BINOMIAL CHARTS #########################################                                                
        tabPanel("Binomial",
            sidebarLayout(
                    column(6,  
                       plotOutput("binomialPlot1", width = "100%")
                      ),

                    column(6,
                          mainPanel(     
                                    fixedRow(
                                            column(12, 
                                                   plotOutput(outputId = "binomialPlot2", width = "100%"), 
                                                  offset = 0)
                                            ),
                                    fixedRow(
                                             column(4, 
                                                    numericInput("n", 
                                                                label = h5("Number of Trials"),  
                                                                value = 5), 
                                                    offset = 4),
                                            column(4,    
                                                    numericInput("p", 
                                                                label = h5("Prob. of Success"),
                                                                min = 0, max=1,
                                                                value = 0.5)
                                                   ))
                  ))
          )),

######################## POISSON CHARTS #########################################  
          tabPanel("Poisson",
                sidebarLayout(
                    column(5,
                        plotOutput("poissonPlot1", width = "100%")
                          ),
                     column(6,
                           mainPanel(
                               fluidRow(
                                        column(12,
                                              plotOutput(outputId = "poissonPlot2", width = "100%"), 
                                              offset = 0)),
                              fluidRow(
                                       column(5, 
                                              numericInput("lambda", 
                                                          label = h5("lambda - mean, var"),  
                                                           value = 1,
                                                           min=0), 
                                              offset = 3),
                                        column(4,    
                                               numericInput("k", 
                                                            label = h5("k - # of occurrences"), 
                                                            value = 4,
                                                            min=0)
                                               ))
             
                      ))
        )),
        
######################## BETA CHARTS #########################################        
        tabPanel("Beta",
            sidebarLayout(
                  column(5,
                        plotOutput("betaPlot1", width = "100%")
                        ),
                  column(6,
                         mainPanel(
                                  fluidRow(
                                          column(12,
                                                plotOutput(outputId = "betaPlot2", width = "100%"), 
                                                offset = 0)),
                                  fluidRow(
                                          column(4, 
                                                numericInput("alpha", 
                                                            label = h5("alpha"),  
                                                            value = 1,
                                                            min = 0), 
                                                offset = 4),
                                          column(4,    
                                                 numericInput("beta", 
                                                            label = h5("beta"),
                                                            min=0,
                                                            value = 1)
                                                 ))
                           
                  ))
        )),

######################## EXPONENTIAL CHARTS #########################################        


      tabPanel("Exponential",
          sidebarLayout(
                column(5,
                       plotOutput("exponentialPlot1", width = "100%")
                      ),
                column(6,
                       mainPanel(
                         fluidRow(
                           column(12,
                                  plotOutput(outputId = "exponentialPlot2", width = "100%"), 
                                  offset = 0)),
                         fluidRow(
                                  column(4, 
                                        numericInput("rate", 
                                                    label = h5("lambda/rate"),  
                                                    value = 1,
                                                    min = 0), 
                                  offset = 7))
                         
                ))
        )),      
                
        
######################## NORMAL CHARTS #########################################                

        tabPanel("Normal",
            sidebarLayout(
                  column(5,
                        plotOutput("normalPlot1", width = "100%")
                        ),
                  
                  column(6,
                         mainPanel(
                              fluidRow(
                                    column(12,
                                          plotOutput(outputId = "normalPlot2", width = "100%"), 
                                          offset = 0)),
                              fluidRow(
                                    column(4, 
                                          numericInput("mean", 
                                                      label = h5("mean"),  
                                                      value = 0), 
                                          offset = 4),
                             column(4,    
                                    numericInput("std", 
                                                 label = h5("std dev"), 
                                                 value = 1,
                                                 min = 0)
                                    ))
                           
                  ))
       )),
            
######################## UNIFORM CHARTS #########################################        
      tabPanel("Uniform",
         sidebarLayout(
                column(5,
                      plotOutput("uniformPlot1", width = "100%")
                      ),
           
                column(6,
                      mainPanel(
                                fluidRow(
                                          column(12,
                                                plotOutput(outputId = "uniformPlot2", width = "100%"), 
                                          offset = 0)),
                                fluidRow(width="100%",
                                          column(4, 
                                                numericInput("a", 
                                                            label = h5("a"),  
                                                            value = 1), 
                                                offset = 4),
                                          column(4,    
                                                numericInput("b", 
                                                            label = h5("b"), 
                                                            value = 2)
                                                ))
                               ))
                    
                  ))

))

 

