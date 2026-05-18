#' Make the Consolidated Urban Green Infrastructure Classification
#'
#' This function take in a ndvi map and classified height maps and combines to map the CUGIC. 
#' NOTE: 
#' Literature: CUGIC: The Consolidated Urban Green Infrastructure Classification for assessing ecosystem services and biodiversity. https://doi.org/10.1016/j.landurbplan.2023.104726
#' @param ndvi_stack rasterStack. Containing ndvi rasters
#' @param ndvi_layer_index numeric value. Indicating for which layer the function uses
#' @param classified_height rasterStack. Containing 3 layers corresponding to grass, shrub and tree proportions.
#' @param ndvi_min numeric value. Cut-off below which are considered non-vegetation
#' @param out_fcv character value. Path for saving fractional coverage of vegetation
#' @param out_cugic chracter value. Path for saving CUGIC output
#' @keywords classification, height, proportion
#' @export
#' @examples
#' make_cugic()
#' 

make_cugic <- function(ndvi_stack, ndvi_layer_index = 1, classified_height, 
                             ndvi_min = 0.25, out_fcv = NULL, out_cugic = NULL) {
  
  cat("Step 1/4: Selecting NDVI layer from stack...\n")
  ndvi_raster <- ndvi_stack[[ndvi_layer_index]]
  
  cat("Step 2/4: Calculating FCV...\n")
  ndvi_max <- ndvi_raster@pntr@.xData$range_max
  ndvi_raster[ndvi_raster > ndvi_max] <- NA #nonsense but okay...
  ndvi_raster[ndvi_raster < ndvi_min] <- NA
  fcv <- terra::app(ndvi_raster, fun=function(x){((x-ndvi_min)/(ndvi_max-ndvi_min))})
  
  #Make save if requested
  if (!is.null(out_fcv)) terra::writeRaster(fcv, out_fcv, overwrite = TRUE)
  
  
  #Combing NDVI and height data
  cat("Step 3/4: Preparing CUGIC classification...\n")
  CUGICveg <- terra::c(fcv, classified_height)
  CUGICveg <- terra::c(CUGICveg, CUGICveg[[1]]) #Create dummy raster
  CUGICveg[[5]][] <- NA #Empty the dummy
  names(CUGICveg[[5]]) <- "CUGIC"
  
  
  #Classifying to CUGIC classes
  cat("Step 4/4: Classifying vegetation classes with progress bar...\n")
  
  #Classifying
  d <- CUGICveg[[1]] #Density
  h1 <- CUGICveg[[2]] #Grass
  h2 <- CUGICveg[[3]] #Shrub
  h3 <- CUGICveg[[4]] #Tree
  
  gc(full = T)
  #ifel would be better, but lazy...
  CUGICveg[[5]] <- terra::ifel(h1 >= 0.1 & h2 >= 0.1 & h3 >= 0.1, #mixed
                               terra::ifel(d > 0.7, 28,
                                           terra::ifel(d >= 0.5, 27,
                                                       terra::ifel(d >= 0.1, 26,
                                                                   terra::ifel(d > 0, 25, NA)))),
                        
                               terra::ifel(h2 >= 0.1 & h3 >= 0.1, #shrub tree
                                           terra::ifel(d > 0.7, 24,
                                                       terra::ifel(d >= 0.5, 23,
                                                                   terra::ifel(d >= 0.1, 22,
                                                                               terra::ifel(d > 0, 21, NA)))),
                            
                                           terra::ifel(h1 >= 0.1 & h3 >= 0.1, #grass tree
                                                       terra::ifel(d > 0.7, 20,
                                                                   terra::ifel(d >= 0.5, 19,
                                                                               terra::ifel(d >= 0.1, 18,
                                                                                           terra::ifel(d > 0, 17, NA)))),
                            
                            
                                                       terra::ifel(h1 >= 0.1 & h2 >= 0.1, #grass shrub
                                                                   terra::ifel(d > 0.7, 16,
                                                                               terra::ifel(d >= 0.5, 15,
                                                                                           terra::ifel(d >= 0.1, 14,
                                                                                                       terra::ifel(d > 0, 13, NA)))),
                            
                                                                   terra::ifel(h1 > 0, #grass
                                                                               terra::ifel(d > 0.7, 4,
                                                                                           terra::ifel(d >= 0.5, 3,
                                                                                                       terra::ifel(d >= 0.1, 2,
                                                                                                                   terra::ifel(d > 0, 1, NA)))),
                            
                                                                               terra::ifel(h2 > 0, #shrub
                                                                                           terra::ifel(d > 0.7, 8,
                                                                                                       terra::ifel(d >= 0.5, 7,
                                                                                                                   terra::ifel(d >= 0.1, 6,
                                                                                                                               terra::ifel(d > 0, 5, NA)))),
                            
                                                                                           terra::ifel(h3 > 0,  #tree
                                                                                                       terra::ifel(d > 0.7, 12,
                                                                                                                   terra::ifel(d >= 0.5, 11,
                                                                                                                               terra::ifel(d >= 0.1, 10,
                                                                                                                                           terra::ifel(d > 0, 9, NA)))),
                NA
              )))))))
  
  if (!is.null(out_cugic)) terra::writeRaster(CUGICveg[[5]], out_cugic, overwrite = TRUE)
  
  cat("Processing complete.\n")
  return(list(fcv = fcv, cugic = CUGICveg[[5]]))
}
