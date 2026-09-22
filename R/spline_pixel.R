#' Spline interpolation for a vector. Allows for setting a max gap and NA proportion for the vector. Moreover, there is also a clamp for both minimum and maximum values.
#'
#' NOTE: 
#' Literature: 
#' @param y numeric vector. Time series for spline interpolation
#' @param max_gap numeric (defaults to 5). Length of consecutive NA in the vector before interpolation to return the vector directly.
#' @param max_NA_prop proportion (defaults to 0.5). Proportion of NA in the vector before interpolation to return vector direclty.
#' @export
#' @examples
#' PLACEHOLDER()
#' 

############ smooth spline #############
spline_pixel <- function(y, max_gap = 5, max_NA_prop = 0.5, maxVal = NULL, minVal = NULL){
  
  valid_mask <- !is.na(y) #which values are present?
  n <- length(y)
  if(sum(valid_mask) < 1){return(y)} 
  if(sum(valid_mask)/n < max_NA_prop){return(y)}
  rle <- rle(valid_mask)
  gaps <- rle$lengths[rle$values==FALSE]
  if(length(gaps) > 0 && any(gaps > max_gap)){return(y)}
  
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
  
  #clamp
  if(!is.null(maxVal)){predictions[predictions > maxVal] <- maxVal}
  if(!is.null(minVal)){predictions[predictions < minVal] <- minVal}
  
  # Return the smoothed time series
  return(predictions)
  
  
} 
