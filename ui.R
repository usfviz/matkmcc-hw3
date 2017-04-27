library(shiny)
library(ggplot2)
library(ggvis)
library(plotly)

shinyUI(
  bootstrapPage(
    
    # to suppress the error message on the paracoordinate plot
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    
    # select input...
    sidebarLayout(
      sidebarPanel(
        conditionalPanel(
          "input.currentTab == 'Small Multiples Plot'",
            # do a conditional thing here
            titlePanel('Glipizide or Examide'),
            sliderInput('lengthOfStay', 
                        label = 'Select Length of stay', 
                        min = 1, 
                        max = 14,
                        value = 1) , 
            selectInput('vus', 'Select Levels',
                        choices = c('Down', 'Steady', 'Up', 'No'), 
                        selected = c('Down', 'Steady', 'Up', 'No'), 
                        multiple = T)
            , width = 4
        ), # close conditionalPanel for smallMultiples
        
        conditionalPanel(
          "input.currentTab == 'Bubble Plot'",
          # do a conditional thing here
          titlePanel('Enjoy the Bubbles'),
          selectInput('michealBuble', 'Select Fill Variable',
                      choices = c('readmitted', 'age')), 
          selectInput('y', 'Select Vertical Axis', 
                      choices = c("num_lab_procedures", 
                                  "num_procedures",  
                                  "num_medications",
                                  "number_outpatient",
                                  "number_emergency", 
                                  "number_inpatient",  
                                  "number_diagnoses")
          )
          ), # close the conditional panel for bubbles
        
        conditionalPanel(
          "input.currentTab == 'Parallel Coordinate'",
          # do a conditional thing here
          titlePanel('Select More than 1 Coordinate'),
          sliderInput('sampleSize', 
                      label = 'Select the Size of the Sample', 
                      min = 0, 
                      max = 10000,
                      value = 100), 
          selectInput('viableInput', 'Select Parallel Coordinates',
                      choices = paraColumns, 
                      multiple = T, 
                      selectize = T
          )
          # in put coordinate selection - for valid coordinates
          , width = 4
        ) # close conditionalPanel for smallMultiples
      ), # close the sidebarPanel
      mainPanel(
        tabsetPanel(
          # Conditional Panel in here yo!
          # add selection of variable (color by this thing as well) 
          # restructure the data to be more imporessive
          tabPanel('Small Multiples Plot', plotOutput(outputId='smallmultiples'), id = 'smallmultiples'),
          # Size of the bubbles
          # add tool-tip for some information
          # labels
          # Color by different groups
          tabPanel('Bubble Plot', ggvisOutput('bubblePlot')),
          tabPanel('Parallel Coordinate', plotlyOutput(outputId='pcoordPlot')),
          id = 'currentTab'
        ) # Close tabsetPanel
      ) # Close MainPanel
      , position = 'left'
      , fluid = T
    ) # Close sidebarLayout
    
  ) # Close bootstrap
) # Close shinyUI