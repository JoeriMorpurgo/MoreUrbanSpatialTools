#' Crops list of rasters to AOI
#'
#' This function takes a list of rasters and check if the extent overlaps the AOI. If it does it mosaics them together and the crops them.
#' NOTE: 
#' Literature: 
#' @param raster_dir path to folder with raster files
#' @param aoi_file path to file that canbe loaded by vect() from Terra.
#' @param out_file Optional. Path to place where to put the output.
#' @keywords classification, height, proportion
#' @export
#' @examples
#' mosaic_and_mask()
#' 

#v2
mosaic_and_mask <- function(raster_dir, aoi_file, out_file = "mosaic_masked.tif") {
  
  cat("Reading AOI...\n")
  aoi <- vect(aoi_file)
  
  cat("Listing rasters...\n")
  ras_files <- list.files(raster_dir, pattern = "\\.tif$", full.names = TRUE)
  
  if (length(ras_files) == 0) {
    stop("No .tif files found in the directory.")
  }
  
  cat(length(ras_files), "rasters found. Checking overlaps with AOI...\n")
  
  pb <- txtProgressBar(min = 0, max = length(ras_files), style = 3)
  masked_list <- list()
  
  for (i in seq_along(ras_files)) {
    r <- rast(ras_files[i])
    r_ext_poly <- as.polygons(ext(r), crs = crs(r))
    
    if (relate(r_ext_poly, aoi, "intersects")) {
      # Crop and mask *before* storing
      cropped <- crop(r, aoi)
      masked <- mask(cropped, aoi)
      masked_list[[length(masked_list) + 1]] <- masked
    }
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  if (length(masked_list) == 0) {
    stop("No rasters overlap with the AOI.")
  }
  
  cat(length(masked_list), "rasters will be mosaicked...\n")
  mos <- do.call(mosaic, masked_list)
  
  cat("Saving output to:", out_file, "\n")
  writeRaster(mos, out_file, overwrite = TRUE)
  
  cat("Done!\n")
  return(mos)
}
