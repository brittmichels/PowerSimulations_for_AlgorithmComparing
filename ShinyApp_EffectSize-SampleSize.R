## Load needed packages
library(shiny)
library(reticulate)
library(ggplot2)
library(rsconnect)
library(readxl)

#set working directory
setwd('~')

## Import result data
es_power <- read_excel('samplesizeForPowerTotal.xlsx')
# round data to 3 decimals
es_power$es <- signif(es_power$es, 3)

## Define the user interface
ui <- fluidPage(
  # titel of page
  titlePanel("Specify variables for selected problem"),

  # Choose effect size
  numericInput(inputId = "es", 
               label = "Choose effect size (3 decimals) to see sample size for 80% power", 
               value = 0.299, min = 0.072, max = 1.000, step = 0.001),
  #plotOutput("powss")
  plotOutput("effectSizePlot")
)

## Define back end
server <- function(input, output, session) {
  # define the reactive values used in calculations
  vals <- reactiveValues()
  observe({
    vals$testrep <- input$testrep
    vals$maxsize <- input$maxsize
    vals$alpha <- input$alp
    vals$effectsize <- input$es
  })
  
  
  # Define output effect size plot
  output$effectSizePlot <- renderPlot({ 
    # Load all effect sizes
    chartData <- es_power
    # Load choosen effect sizes
    esHline <- rep(vals$effectsize, 150)
    # Define range sample size
    nrange <- subset(es_power, es==vals$effectsize)
    # Define minimal sample size
    nmin <- min(nrange$n)
    nminLine <- rep(nmin, 2)
    # Define maximal sample size
    nmax <- max(nrange$n)
    nmaxLine <- rep(nmax, 2)
    # Define plot estetics
    yrange <- c(0.000,1.000)
    xrange <- c(0,150)
    plot(xrange,yrange,type="n",xlab="dataset size",ylab="effect size",main=paste("The sample size found for 80% power and an effect size of", vals$effectsize, "is minimal", nmin, "and maximal", nmax))
    # Plot the actual graph
    points(chartData$n,chartData$es,col="gray")
    lines(1:150,esHline,col="firebrick3")
    lines(nminLine,0:1,col="firebrick3")
    lines(nmaxLine,0:1,col="firebrick3")
  })
  
}

## cary into shiny app
shinyApp(ui = ui, server = server)  
