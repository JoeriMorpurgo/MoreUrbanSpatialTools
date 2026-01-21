#' Stacks and equalises rasters 
#'
#' This function takes a list of raster file path and equalises their extent. After it stacks the rasters.
#' NOTE: 
#' Literature: 
#' @param rasterFiles path to raster files.
#' @keywords classification, height, proportion
#' @export
#' @examples
#' mask_buildings_ohsome_robust()
#' 

#################### stack_rasters_equalise_extent() ##########
stack_rasters_equalise_extent <- function(rasterFiles) {
  
  # Load all rasters
  rasters <- lapply(rasterFiles, rast)
  
  # Check that CRS are the same
  crs_list <- sapply(rasters, crs)
  if (length(unique(crs_list)) > 1) {
    stop("Not all rasters have the same CRS.")
  }
  
  # Find the largest extent
  all_extents <- lapply(rasters, ext)
  combined_extent <- Reduce(union, all_extents)
  
  # Pad each raster to match the largest extent
  padded_rasters <- lapply(rasters, function(r) {
    ext_r <- ext(r)
    if (!ext_r == combined_extent) {
      extend(r, combined_extent)
    } else {
      r
    }
  })
  
  # Stack the padded rasters
  raster_stack <- rast(padded_rasters)
  return(raster_stack)
  
}