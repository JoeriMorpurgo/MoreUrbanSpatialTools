#' HANTS curve for a vector or 1 pixel in a raster stack. Requires at least half of pixels to have values.
#'
#' NOTE: 
#' Literature: Roerink, G. J., Menenti, M., & Verhoef, W. (2000). Reconstructing cloudfree NDVI composites using Fourier analysis of time series. International Journal of Remote Sensing, 21(9), 1911–1917. https://doi.org/10.1080/014311600209814
#' @param y numeric vector. Time series for harmonic interpolation following HANTS algoritm.
#' @param freq integer. Number of harmonic frequencies to fit
#' @param max_iter integer. Max number of iterations for outlier rejection.
#' @param tolerance Numeric. Threshold for outlier rejection as absolute numeric value. Default is 0.10. In Roerink et al., (2000), the value used is 0.05.
#' @param max_gap Numeric (defaults to 5). The number of maximum NA gap allowed before the values are directly returned
#' @param max_NA_prop Numeric (defaults to 0.5). The proportion of NA that is allows in the vector for interpolation.
#' @export
#' @examples
#' PLACEHOLDER()
#' 

############ HANTS_curve #############
HANTS_pixel <- function(y, freq = 1, max_iter =3, tolerance = 0.1, max_gap = 5, max_NA_prop = 0.5){
  
  # If the pixel is completely empty (e.g., water/background), return NAs
  if (all(is.na(y))){return(y)}
  
  n <- length(y) #length of Y-time series
  n_cols <- 1 + (2 * freq) # 1 Intercept + (2 terms * freq)

  #check if enough values to model HANTS curve
  valid_mask <- !is.na(y)
  if (sum(valid_mask) < n_cols){return(y)}
  if(sum(valid_mask)/n < max_NA_prop){return(y)}
  
  #Asses NA gaps
  rle <- rle(valid_mask)
  gaps <- rle$lengths[rle$values==FALSE]
  if(length(gaps) > 0 && any(gaps > max_gap)){return(y)}
  
  #data matrix
  ts <- 1:n
  X <- matrix(1, nrow = n, ncol = n_cols)
  
  
  
  # add in harmonix pred data
  col_idx <- 2
  for (i in 1:freq) {
                      X[,col_idx] <- cos(2 * pi * i * ts / n)
                      X[,col_idx+1] <- sin(2 * pi * i * ts / n)
                      col_idx <- col_idx + 2
                    }
  
 #vector
 predictions <- rep(NA, n)
 HANTSweights <- as.numeric(valid_mask)
  
  #iterative fitting loop
  for (iter in 1:max_iter) {
    
    #check if we have enough data points
    valid <- HANTSweights > 0
    if (sum(valid) < n_cols) {
      break
    }
    
    fit <- lm.fit(
      x = X[valid, , drop = FALSE], #drop values that are not valid
      y = y[valid]
    )
    
    predictions <- as.vector(X %*% fit$coefficients)
    
    if (iter == max_iter) break
    
    # Calculate residuals (actual - predicted)
    # Penalize values far below the curve.
    residuals <- y - predictions
    
    #points outside of the tolerance
    reject_value <- (!is.na(residuals) & residuals < -tolerance)
    reject_data <- !valid | reject_value #either NA from start or rejected value
    
    # Update weights: ignore points that are lower than the fit by tolerance
    HANTSweights[reject_data] <- 0
  }
  
  #add in the predictions for the NA positions
  y[is.na(y)] <- predictions[is.na(y)]

 
  # Return the smoothed time series
  return(y)
  
  
} 
