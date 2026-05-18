# Define the reclassification function
reclassify_vector <- function(x, lookup_vector, new_values) {
  
  # Create a named vector for mapping
  lookup_map <- stats::setNames(new_values, lookup_vector)
  
  # Replace values in x based on the lookup map
  x_reclassified <- ifelse(x %in% names(lookup_map), lookup_map[x], x)
  
  return(x_reclassified)
}