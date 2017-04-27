library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggvis)
  
bubblePlot <- function(bubbleData, fillValue, yValue) {
  
   tooltipFunction <- function(x) {
     if(is.null(x)) return(x) 
     print(x)
     paste0(
       paste0('<strong> Bubble Information </strong>'), '<br/>' ,
       paste0('Number of Patients: ', x$num_patients), '<br/>' ,
       paste0('Age: ', x$age), '<br/>' ,
       paste0('Readmitted: ', x$readmitted), '<br/>' ,
       paste0('Length of Stay: ', round(x$time_in_hospital, 2)), '<br/>'
       )
   }
   
   if (fillValue == 'age') {
     
     if (yValue == "num_lab_procedures") {
       
       plop <- ggvis(data = bubbleData, 
                     x=~time_in_hospital, 
                     y=~num_lab_procedures, 
                     stroke:='white',
                     size:=~num_patients,
                     fill=~age) %>% 
         layer_points() %>% 
         add_tooltip(tooltipFunction, on = c('hover')) %>%
         ggvis::hide_legend('size') %>% 
         add_axis(type = 'x', title = 'Length Of Stay', 
                  grid = F) %>%
         add_axis(type = 'y', title = 'Number of Lab Procedures', 
                  grid = F)
       
     } else if (yValue == "num_procedures") {
       
       plop <- ggvis(data = bubbleData, 
                     x=~time_in_hospital, 
                     y=~num_procedures, 
                     stroke:='white',
                     size:=~num_patients,
                     fill=~age) %>% 
         layer_points() %>% 
         add_tooltip(tooltipFunction, on = c('hover')) %>%
         ggvis::hide_legend('size') %>% 
         add_axis(type = 'x', title = 'Length Of Stay', 
                  grid = F) %>%
         add_axis(type = 'y', title = 'Number of Lab Procedures', 
                  grid = F)
       
     } else if (yValue == 'num_medications') {
       
       plop <- ggvis(data = bubbleData, 
                     x=~time_in_hospital, 
                     y=~num_medications, 
                     stroke:='white',
                     size:=~num_patients,
                     fill=~age) %>% 
         layer_points() %>% 
         add_tooltip(tooltipFunction, on = c('hover')) %>%
         ggvis::hide_legend('size') %>% 
         add_axis(type = 'x', title = 'Length Of Stay', 
                  grid = F) %>%
         add_axis(type = 'y', title = 'Number of Lab Procedures', 
                  grid = F)
       
     } else if (yValue == 'number_outpatient') {
       
       plop <- ggvis(data = bubbleData, 
                     x=~time_in_hospital, 
                     y=~number_outpatient, 
                     stroke:='white',
                     size:=~num_patients,
                     fill=~age) %>% 
         layer_points() %>% 
         add_tooltip(tooltipFunction, on = c('hover')) %>%
         ggvis::hide_legend('size') %>% 
         add_axis(type = 'x', title = 'Length Of Stay', 
                  grid = F) %>%
         add_axis(type = 'y', title = 'Number of Lab Procedures', 
                  grid = F)
       
       
     } else if (yValue == 'number_emergency') {
       
       plop <- ggvis(data = bubbleData, 
                     x=~time_in_hospital, 
                     y=~number_emergency, 
                     stroke:='white',
                     size:=~num_patients,
                     fill=~age) %>% 
         layer_points() %>% 
         add_tooltip(tooltipFunction, on = c('hover')) %>%
         ggvis::hide_legend('size') %>% 
         add_axis(type = 'x', title = 'Length Of Stay', 
                  grid = F) %>%
         add_axis(type = 'y', title = 'Number of Lab Procedures', 
                  grid = F)
       
       
     } else if (yValue == 'number_inpatient') {
       
       plop <- ggvis(data = bubbleData, 
                     x=~time_in_hospital, 
                     y=~number_inpatient, 
                     stroke:='white',
                     size:=~num_patients,
                     fill=~age) %>% 
         layer_points() %>% 
         add_tooltip(tooltipFunction, on = c('hover')) %>%
         ggvis::hide_legend('size') %>% 
         add_axis(type = 'x', title = 'Length Of Stay', 
                  grid = F) %>%
         add_axis(type = 'y', title = 'Number of Lab Procedures', 
                  grid = F)
       
     } else {
       
       plop <- ggvis(data = bubbleData, 
                     x=~time_in_hospital, 
                     y=~number_diagnoses, 
                     stroke:='white',
                     size:=~num_patients,
                     fill=~age) %>% 
         layer_points() %>% 
         add_tooltip(tooltipFunction, on = c('hover')) %>%
         ggvis::hide_legend('size') %>% 
         add_axis(type = 'x', title = 'Length Of Stay', 
                  grid = F) %>%
         add_axis(type = 'y', title = 'Number of Lab Procedures', 
                  grid = F)
       
     }
     
   } else {
     
     if (yValue == "num_lab_procedures") {
       
       plop <- ggvis(data = bubbleData, 
                     x=~time_in_hospital, 
                     y=~num_lab_procedures, 
                     stroke:='white',
                     size:=~num_patients,
                     fill=~readmitted) %>% 
         layer_points() %>% 
         add_tooltip(tooltipFunction, on = c('hover')) %>%
         ggvis::hide_legend('size') %>% 
         add_axis(type = 'x', title = 'Length Of Stay', 
                  grid = F) %>%
         add_axis(type = 'y', title = 'Number of Lab Procedures', 
                  grid = F)
       
     } else if (yValue == "num_procedures") {
       
       plop <- ggvis(data = bubbleData, 
                     x=~time_in_hospital, 
                     y=~num_procedures, 
                     stroke:='white',
                     size:=~num_patients,
                     fill=~readmitted) %>% 
         layer_points() %>% 
         add_tooltip(tooltipFunction, on = c('hover')) %>%
         ggvis::hide_legend('size') %>% 
         add_axis(type = 'x', title = 'Length Of Stay', 
                  grid = F) %>%
         add_axis(type = 'y', title = 'Number of Lab Procedures', 
                  grid = F)
       
     } else if (yValue == 'num_medications') {
       
       plop <- ggvis(data = bubbleData, 
                     x=~time_in_hospital, 
                     y=~num_medications, 
                     stroke:='white',
                     size:=~num_patients,
                     fill=~readmitted) %>% 
         layer_points() %>% 
         add_tooltip(tooltipFunction, on = c('hover')) %>%
         ggvis::hide_legend('size') %>% 
         add_axis(type = 'x', title = 'Length Of Stay', 
                  grid = F) %>%
         add_axis(type = 'y', title = 'Number of Lab Procedures', 
                  grid = F)
       
     } else if (yValue == 'number_outpatient') {
       
       plop <- ggvis(data = bubbleData, 
                     x=~time_in_hospital, 
                     y=~number_outpatient, 
                     stroke:='white',
                     size:=~num_patients,
                     fill=~readmitted) %>% 
         layer_points() %>% 
         add_tooltip(tooltipFunction, on = c('hover')) %>%
         ggvis::hide_legend('size') %>% 
         add_axis(type = 'x', title = 'Length Of Stay', 
                  grid = F) %>%
         add_axis(type = 'y', title = 'Number of Lab Procedures', 
                  grid = F)
       
       
     } else if (yValue == 'number_emergency') {
       
       plop <- ggvis(data = bubbleData, 
                     x=~time_in_hospital, 
                     y=~number_emergency, 
                     stroke:='white',
                     size:=~num_patients,
                     fill=~readmitted) %>% 
         layer_points() %>% 
         add_tooltip(tooltipFunction, on = c('hover')) %>%
         ggvis::hide_legend('size') %>% 
         add_axis(type = 'x', title = 'Length Of Stay', 
                  grid = F) %>%
         add_axis(type = 'y', title = 'Number of Lab Procedures', 
                  grid = F)
       
       
     } else if (yValue == 'number_inpatient') {
       
       plop <- ggvis(data = bubbleData, 
                     x=~time_in_hospital, 
                     y=~number_inpatient, 
                     stroke:='white',
                     size:=~num_patients,
                     fill=~readmitted) %>% 
         layer_points() %>% 
         add_tooltip(tooltipFunction, on = c('hover')) %>%
         ggvis::hide_legend('size') %>% 
         add_axis(type = 'x', title = 'Length Of Stay', 
                  grid = F) %>%
         add_axis(type = 'y', title = 'Number of Lab Procedures', 
                  grid = F)
       
     } else {
       
       plop <- ggvis(data = bubbleData, 
                     x=~time_in_hospital, 
                     y=~number_diagnoses, 
                     stroke:='white',
                     size:=~num_patients,
                     fill=~readmitted) %>% 
         layer_points() %>% 
         add_tooltip(tooltipFunction, on = c('hover')) %>%
         ggvis::hide_legend('size') %>% 
         add_axis(type = 'x', title = 'Length Of Stay', 
                  grid = F) %>%
         add_axis(type = 'y', title = 'Number of Lab Procedures', 
                  grid = F)
       
     }
     
   }
    
    
  
  return(plop)
}

str(bubbleData)
