## Load needed packages
library(shiny)
library(reticulate)
library(ggplot2)

## Import simulation functions from python
source_python('PhytonShinyInputDtt.py')


## Define the user interface
ui <- fluidPage(
  # titel of page
  titlePanel("Specify variables for selected problem"),
  
  # Slidebar for alpha
  sliderInput(inputId = "alp", 
              label = "Choose alpha level", 
              value = 0.05, min =0.00, max =1.00),
  # Fill in space for x axis range : how many datasets are under concideration
  numericInput(inputId = "maxsize", 
               label = "Maximal dataset size", 
               value = 75),
  # Fill in space for repetitions of power estimation
  numericInput(inputId = "testrep", 
               label = "Number of sampling repetitions for power estimation", 
               value = 100),
  plotOutput("powss")
                )

## Define back end
server <- function(input, output, session) {
  # define the reactive values used in calculations
  vals <- reactiveValues()
  observe({
    vals$testrep <- input$testrep
    vals$maxsize <- input$maxsize
    vals$alpha <- input$alp
  })
  
  # Define output plot
  output$powss <- renderPlot({ 
    # Simulate power via pythonplot -> Expand to non normal / pre-define distribution parameters
    chartData <- power_simulate(vals$testrep, vals$maxsize, vals$alpha, 0.80, 0.75, 0.05, 0.08)
    # Define plot estetics
    yrange <- c(0,1.0)
    xrange <- c(1.0,length(chartData))
    plot(xrange,yrange,type="n",xlab="dataset size",ylab="power",main=paste("Power over dataset size for alpha is", vals$alpha))
    # Plot the actual graph
    lines(1:length(chartData),chartData,col="firebrick3",lwd=3)
  })
}

## cary into shiny app
shinyApp(ui = ui, server = server)  
