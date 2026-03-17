#' build_dhm; build digital height model
#'
#' This function takes raster digital terrain models and digital surface models to calculate the digital height model. This can be understood as the height from the ground to a surface above it. NA values from the digital terrain model will be bilinearly interpolated based on a 3x3 grid. NOTE: this function currently only works with AHN data.
#' @param dtm raster. Digital terrain model
#' @param dsm raster. Digital surface model
#' @keywords Height model
#' @export
#' @examples
#' build_dhm()


#Function to calc DHM
build_dhm <- function(dtm, dsm) {

  #resample if needed
  if (!compareGeom(dtm, dsm, stopOnError = FALSE)) { 
    dsm <- resample(dsm, dtm, method = "bilinear")
  }
  
  # Interpoleer NAs in DTM
  if (anyNA(values(dtm))) { #interpolate the NA's
    mask_na <- is.na(dtm)
    dtm_filled <- terra::focal(dtm, w = 3, fun = mean, na.policy = "only", na.rm = TRUE)
    dtm[mask_na] <- dtm_filled[mask_na]
  }
  
  dhm <- dsm - dtm #estimate DHM
  
  #provide dhm
  return(dhm)
}
