#' Transform NDVI values into both Fractional vegetation coverage and then into absolute coverage into m2.
#'
#' NOTE: 
#' Literature: Carlson, T. N., & Ripley, D. A. (1997). On the relation between NDVI, fractional vegetation cover, and leaf area index. Remote Sensing of Environment, 62(3), 241–252. https://doi.org/10.1016/S0034-4257(97)00104-1
#' @param NDVI_raster raster. This files contains the values of NDVI.
#' @export
#' @examples
#' PLACEHOLDER()
#' 

############ get_municipal_bbox #############
ndviToDensity <- function(NDVI_raster,
                          ndvi_min = 0.25, #minimum level of NDVI to be considered green
                          m2 = F){ #Which calc to do
  
    #calc fcv
    ndvi_max <- NDVI_raster@pntr@.xData$range_max
    NDVI_raster[NDVI_raster > ndvi_max] <- NA #nonsense but okay...
    NDVI_raster[NDVI_raster < ndvi_min] <- NA
    output <- app(NDVI_raster, fun=function(x){((x-ndvi_min)/(ndvi_max-ndvi_min))})
    output[is.na(output[])] <- 0
    
    #convert to m2
    if(m2){ # user wants m2
      size <- cellSize(output)
      output <- size*output
    }

  
  return(output)
}
