#' Temporal interpolation based on median normalized difference between two layers. This approach assumes both only positive values and constant effect across all cells. 
#'
#' NOTE: 
#' Literature: 
#' @param stack spat raster stack. Stack of rasters to be interpolated
#' @export
#' @examples
#' PLACEHOLDER()
#' 

############ temporal interpolation by spatial patterns #############
temporal_spatial_approximation <- function(stack){
  

  #clamp to make formula work for ND. The use original raster to apply
  stackFun <- terra::clamp(stack, lower = 0, upper = 1, values = F)
  
  for (i in seq_along(1:nlyr(stackFun))) {
    
    if(i == 1){next} #skip first layer, we can not calculate
    
    # calculate the proportional difference between current and previous layer. Adding small number to make it not explode by 0s.
    norm_diff <- ((stackFun[[i]] - stackFun[[i-1]])/(stackFun[[i]] + stackFun[[i-1]]))
    #take the median value of the normalized difference.
    median_norm_diff <- median(as.numeric(values(norm_diff)), na.rm = T)
    multiplier <- (1 + median_norm_diff) / (1 - median_norm_diff)
    
    # change the value that are NA in stack[[i]] by the normalized difference multiplier
    NAidx <- which(is.na(values(stack[[i]])) == T) #index of the values that are NA
    stack[[i]][NAidx] <- stack[[i-1]][NAidx] * multiplier #fill in these NA cells
    
    print(paste0("STATUS: Approximated layer ", i, " with a multiplier of ", round(multiplier, digits = 3)))
    
  }
  
  # return the stack
  return(stack)
  
  
} 
