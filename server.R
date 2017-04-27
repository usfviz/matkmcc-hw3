library(ggplot2)
library(shiny)
library(ggvis)
library(plotly)

# source the local plots
source('smallmultiples.R')
source('bubblePlot.R')
source('parallelcoord.R')

shinyServer(function(input, output) {
  
      # genearte the small multiples
      output$smallmultiples <- renderPlot({
        small_multiples_data <- small_multiples_data[small_multiples_data$Status %in% input$vus, ]
        subset_smallMultiplesData <- small_multiples_data[small_multiples_data$time_in_hospital == input$lengthOfStay, ]
        smallMultiples <- generateSmallMultiples2(subset_smallMultiplesData)
        return(smallMultiples)
      }, height = 1200, width = 800)
      
      # generate the bubblePlot
      output$bubblePlot <- reactive({
          bubblePlot(bubbleData, input$michealBuble, input$y) %>% 
            set_options(width='auto', height=1000)
      }) %>% bind_shiny('bubblePlot')
      
     
        # generate the pcoord
          output$pcoordPlot <- renderPlotly({
            
            if(length(input$viableInput) > 1) {
                  # set input here
                  toPlot <- input$viableInput
                  sampleSize <- input$sampleSize
                  # set random sample selection here
                  paraCoord <- create_paracoord(diabetes[toPlot], toPlot, sampleSize)
                  return(paraCoord) 
            } else {
              "Enter More than 2 Coordinates"
           }
        }) 
})


colnames(diabetes)
