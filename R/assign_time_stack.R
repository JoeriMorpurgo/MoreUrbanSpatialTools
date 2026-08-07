#' Takes a .nc or rasterstack that was generated from openEO and assigns time to the stack.
#'
#' @param rastStack terra raster stack from openEO. 
#' @keywords stack, openEO, time
#' @export
#' @examples
#' PLACEHOLDER()
#' 
#' 
assign_time_stack <- function(rastStack){

days_since_1990 <- as.numeric(gsub("var_t=", "", names(rastStack)))
raster_dates <- as.Date("1990-01-01") + days_since_1990
time(rastStack) <- raster_dates

return(rastStack)

}