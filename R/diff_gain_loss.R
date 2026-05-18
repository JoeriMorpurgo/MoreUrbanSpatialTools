#' Map the gains and losses over a time series
#'
#' This function takes a raster stack, representing a time-series, to calculate the loss and gains per step in the time series.
#' It does so by calculating the difference per time step and creates a stack of differences, gains or losses in separate stacks.
#' The function returns 2 additional rasters which represent the cumulative gains and losses, solely based on the gains and losses stack.
#' NOTE: 
#' Literature: 
#' @param raster_stack A raster stack representing a time series
#' @keywords classification, height, proportion
#' @export
#' @examples
#' diff_gain_loss()
#' 


diff_gain_loss <- function(raster_stack){
  
    
    #pull NDVI stack for a city
    baseline <- raster_stack
    
    #Stacks of gain and loss
    diff_stack <- terra::rast()
    gains_stack <- terra::rast()
    losses_stack <- terra::rast()
    
    for (j in c(2:nlyr(baseline))) {
      prev <- baseline[[j - 1]] #take prev layer
      curr <- baseline[[j]] #Take current layer
      
      # Calculate change
      diff <- curr - prev
      
      # Extract gains and losses
      gain <- diff
      gain[diff <= 0] <- NA
      
      loss <- diff
      loss[diff >= 0] <- NA
      
      # Add to stacks
      diff_stack <- terra::c(diff_stack, diff)
      gains_stack <- terra::c(gains_stack, gain)
      losses_stack <- terra::c(losses_stack, loss)
      gc(full = T)
    }
    
    #Name layers
    names(diff_stack) <- paste("difference", names(diff_stack))
    names(gains_stack) <- paste("gains", names(gains_stack))
    names(losses_stack) <- paste("losses", names(losses_stack))
    
    #Cumulative gains/losses
    total_gain <- terra::app(gains_stack, sum, na.rm = TRUE)
    total_loss <- terra::app(losses_stack, sum, na.rm = TRUE)

    
  return(list(diff_stack, gains_stack, losses_stack, total_gain, total_loss))
  
}
