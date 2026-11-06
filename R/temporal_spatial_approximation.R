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
  
  #clamp values
  stackFun <- terra::clamp(stack, lower = 0, upper = 1, values = FALSE)
  
  #calc norm diff
  diff_stack <- (stackFun[[-1]] - stackFun[[-terra::nlyr(stackFun)]]) / 
    (stackFun[[-1]] + stackFun[[-terra::nlyr(stackFun)]])
  
  #mean per diff layer
  ##memory intensive for big stack so looping
  median <- numeric(terra::nlyr(diff_stack))
  for(i in seq_along(median)){
    median[i] <- terra::global(diff_stack[[i]], fun = stats::median, na.rm = TRUE)[[1]]
  }

  
  #release memory
  rm(diff_stack, stackFun)
  gc(verbose = F)
  
  #multipliers
  #median[is.na(median) | is.nan(median)] <- 0
  multipliers <- (1 + median) / (1 - median)
  
  #list for calcs
  out_layers <- vector("list", nlyr(stack))
  out_layers[[1]] <- stack[[1]]
  
  #impute
  for (i in 2:nlyr(stack)) {
    mult <- multipliers[i - 1]
    imputed <- out_layers[[i - 1]] * mult
    out_layers[[i]] <- terra::cover(stack[[i]], imputed)
    print(paste0("STATUS: Approximated layer ", i, " with a multiplier of ", round(mult, digits = 3)))
  }
  
  out_stack <- terra::rast(out_layers)
  
  #release memory
  rm(out_layers)
  gc(verbose = F)
  
  # return the stack
  return(out_stack)
  
  
} 
