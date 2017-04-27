library(plotly)

create_coordinate <- function(variable, df) {
  
  return(list(range = c(min(df[variable]), max(df[variable])),
              visible = TRUE, 
              colorscale = 'Hot',
              label = variable, # add lookup here
              values = as.vector(unlist(df[variable]))))
}

create_dimension <- function(df, variable_vector) {
  dimension <- lapply(variable_vector, create_coordinate, df)
  return(dimension)
}

# pass sample size 
# pass in columns to plot
create_paracoord <- function(data_, coords_, sampleSize_) {
  para_data <- paraCoordData_[sample(c(1:nrow( data_ )), sampleSize_ ),]
  
  dimensions <- create_dimension(para_data, coords_ )
  
  plop <- plot_ly(type = 'parcoords', dimensions = dimensions)
  return(plop)
}