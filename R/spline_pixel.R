#' Spline interpolation based on equi-distant time-series
#'
#' NOTE: 
#' Literature: 
#' @param y numeric vector. Time series for spline interpolation
#' @export
#' @examples
#' PLACEHOLDER()
#' 

############ smooth spline #############
spline_pixel <- function(y){
  
  valid_mask <- !is.na(y) #which values are present?
  if (sum(valid_mask) < 1) return(rep(NA, length(y))) #make a mask
  
  #Time series steps
  t <- seq_along(y)

  #fit the spline.
  fit <- tryCatch({
    smooth.spline(
    x = t[valid_mask], #drop values that are not valid
    y = y[valid_mask]
  )
    }, error = function(e) {return(NULL)})
  
  # Return NAs if spline fitting still fails (e.g., duplicate/identical x points)
  if (is.null(fit)) {
    return(rep(NA, length(y)))
  }
  
  predictions <- predict(fit, t)$y #make predictions
  
  # Return the smoothed time series
  return(predictions)
  
  
} 
