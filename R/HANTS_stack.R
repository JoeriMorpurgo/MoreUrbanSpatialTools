#' HANTS for a spatRaster stack.
#'
#' NOTE: 
#' Literature: Roerink, G. J., Menenti, M., & Verhoef, W. (2000). Reconstructing cloudfree NDVI composites using Fourier analysis of time series. International Journal of Remote Sensing, 21(9), 1911–1917. https://doi.org/10.1080/014311600209814
#' @param stack stack raster Terra. Stack of rasters that need to be interpolated.
#' @export
#' @examples
#' PLACEHOLDER()
#' 

############ HANTS_curve #############
HANTS_stack <- function(stack, cores = 1, freq = 1, max_iter =3, tolerance = 0.1) {
  
  # Check if input is a SpatRaster
  if (!inherits(stack, "SpatRaster")) {
    stop("Input must be a terra SpatRaster.")
  }
  
  # Applies the HANTS algorithm to every pixel.
  smoothed_raster <- terra::app(
    x = stack, 
    fun = HANTS_pixel, 
    cores = cores,
    freq = freq,
    max_iter = max_iter,
    tolerance = tolerance
  )
  
  # Keep the original layer names (dates)
  names(smoothed_raster) <- names(stack)
  
  return(smoothed_raster)
}

