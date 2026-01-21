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
  fcv <- app(ndvi_raster, fun=function(x){((x-ndvi_min)/(ndvi_max-ndvi_min))})
  
  #Make save if requested
  if (!is.null(out_fcv)) writeRaster(fcv, out_fcv, overwrite = TRUE)
  
  
  #Combing NDVI and height data
  cat("Step 3/4: Preparing CUGIC classification...\n")
  CUGICveg <- c(fcv, classified_height)
  CUGICveg <- c(CUGICveg, CUGICveg[[1]]) #Create dummy raster
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
  CUGICveg[[5]] <- ifel(h1 >= 0.1 & h2 >= 0.1 & h3 >= 0.1, #mixed
                          ifel(d > 0.7, 28,
                          ifel(d >= 0.5, 27,
                          ifel(d >= 0.1, 26,
                          ifel(d > 0, 25, NA)))),
                        
                        ifel(h2 >= 0.1 & h3 >= 0.1, #shrub tree
                            ifel(d > 0.7, 24,
                            ifel(d >= 0.5, 23,
                            ifel(d >= 0.1, 22,
                            ifel(d > 0, 21, NA)))),
                            
                        ifel(h1 >= 0.1 & h3 >= 0.1, #grass tree
                            ifel(d > 0.7, 20,
                            ifel(d >= 0.5, 19,
                            ifel(d >= 0.1, 18,
                            ifel(d > 0, 17, NA)))),
                            
                            
                        ifel(h1 >= 0.1 & h2 >= 0.1, #grass shrub
                            ifel(d > 0.7, 16,
                            ifel(d >= 0.5, 15,
                            ifel(d >= 0.1, 14,
                            ifel(d > 0, 13, NA)))),
                            
                        ifel(h1 > 0, #grass
                            ifel(d > 0.7, 4,
                            ifel(d >= 0.5, 3,
                            ifel(d >= 0.1, 2,
                            ifel(d > 0, 1, NA)))),
                            
                        ifel(h2 > 0, #shrub
                            ifel(d > 0.7, 8,
                            ifel(d >= 0.5, 7,
                            ifel(d >= 0.1, 6,
                            ifel(d > 0, 5, NA)))),
                            
                        ifel(h3 > 0,  #tree
                            ifel(d > 0.7, 12,
                            ifel(d >= 0.5, 11,
                            ifel(d >= 0.1, 10,
                            ifel(d > 0, 9, NA)))),
                NA
              )))))))
  
  if (!is.null(out_cugic)) writeRaster(CUGICveg[[5]], out_cugic, overwrite = TRUE)
  
  cat("Processing complete.\n")
  return(list(fcv = fcv, cugic = CUGICveg[[5]]))
}
