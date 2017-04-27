library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(gridExtra)

# problems - 
# too slow
# plots need to be relevent to their time
# data description and mock plot

# facetwrap
generateSmallMultiples <- function(graph_data) {
  
  # create the plop
  plop <- ggplot(data = graph_data, 
                 aes(x = Status, y = diffToMean, fill=Color)) + 
    geom_col(color = 'black', alpha = .75) + 
    scale_fill_manual(values = c('firebrick3', 'dodgerblue3')) + 
    theme(panel.background = element_rect(fill = "transparent",colour = NA), 
          plot.background = element_rect(fill = "transparent",colour = NA), 
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = 'none') + 
    facet_wrap(~ medicalName, ncol = 4, scales = 'free_y')
  
  return(plop)
  
}

# xgrid
generatePlotList <- function(plotType, df) {
  graph_data <- df %>% filter(medicalName == plotType)
  plop <- ggplot(data = graph_data, 
                 aes(x = Status, y = diffToMean, fill=Color)) + 
    geom_col(color = 'black', alpha = .75) + 
    scale_fill_manual(values = c('dodgerblue3', 'firebrick3')) + 
    theme(panel.background = element_rect(fill = "transparent",colour = NA), 
          plot.background = element_rect(fill = "transparent",colour = NA), 
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = 'none', 
          plot.title = element_text(size = 12, face = 'bold')) + 
    ggtitle(plotType)
  return(plop)
}


generateSmallMultiples2 <- function(graph_data) {
  # use previously defined medicalNames from global
  smallPlotList <- lapply(medicalNames, generatePlotList, graph_data)
  names(smallPlotList) <- medicalNames
  plop <- do.call('grid.arrange', c(smallPlotList, ncol = 4))
  return(plop)
}


