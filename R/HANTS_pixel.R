#' HANTS curve for 1 pixel
#'
#' NOTE: 
#' Literature: Roerink, G. J., Menenti, M., & Verhoef, W. (2000). Reconstructing cloudfree NDVI composites using Fourier analysis of time series. International Journal of Remote Sensing, 21(9), 1911–1917. https://doi.org/10.1080/014311600209814
#' @param y numeric vector. Time series for harmonic interpolation following HANTS algoritm.
#' @param freq integer. Number of harmonic frequencies to fit
#' @param max_iter integer. Max number of iterations for outlier rejection.
#' @param tolerance Numeric. Threshold for outlier rejection as absolute numeric value. Default is 0.10. In Roerink et al., (2000), the value used is 0.05.
#' @export
#' @examples
#' PLACEHOLDER()
#' 

############ HANTS_curve #############
HANTS_pixel <- function(y, freq = 1, max_iter =3, tolerance = 0.1){
  
  # If the pixel is completely empty (e.g., water/background), return NAs
  if (all(is.na(y))) return(rep(NA, length(y)))
  
  n <- length(y)
  n_cols <- 1 + (2 * freq) # 1 Intercept + (2 terms * freq)
  time_steps <- 1:n
  
  #check if enough values
  valid_mask <- !is.na(y)
  if (sum(valid_mask) < n_cols) return(rep(NA, n))
  
  #data matrix
  t <- 1:n
  X <- matrix(1, nrow = n, ncol = n_cols)
  
  # add in harmonix pred data
  col_idx <- 2
  for (i in 1:freq) {
    X[,col_idx] <- cos(2 * pi * i * t / n)
    X[,col_idx+1] <- sin(2 * pi * i * t / n)
    col_idx <- col_idx + 2
  }
  
  #weights
  weights <- as.numeric(valid_mask)
  y_clean <- y
  y_clean[!valid_mask] <- 0
  
  predictions <- rep(NA, n)
  
  for (iter in 1:max_iter) {
    
    valid <- weights > 0
    
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
    
    # Update weights: ignore points that are lower than the fit by tolerance
    weights[valid_mask & (residuals < -tolerance)] <- 0
  }
  
  # Return the smoothed time series
  return(predictions)
  
  
} 
