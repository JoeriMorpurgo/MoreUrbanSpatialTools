#' Fills in the temporally missing layers in a raster stack on monthly basis.
#'
#' @param r A terra SpatRaster object with time information (`terra::time(r)`).
#' @export
#' @examples
#' placeholder
#' @return The stack with NA for missing layers.
#' #' PLACEHOLDER()

fill_missing_layers <- function(r, interval = "month") {
  
  # Extract timestamps
  existing_times <- as.Date(time(r))
  if (all(is.na(existing_times))) {
    stop("The input SpatRaster has no valid time metadata assigned.")
  }
  
  #sorting the time variable so that if they are mixed up they are correct again
  existing_times <- sort(existing_times)
  
  #retrieve first and final date of the stack
  min_date <- min(existing_times)
  max_date <- max(existing_times)
  
  # Generate full time sequence
  time_sequence <- seq(from = min_date, to = max_date, by = interval)

  # Find where we miss the layers
  match_sequences <- match(format(time_sequence, "%Y-%m"), format(existing_times, "%Y-%m"))
  
  # Make empty baselayer
  blank_layer <- rast(r[[1]], vals = NA)
  
  layer_list <- lapply(match_sequences, function(idx) {
    if (is.na(idx)) {
      return(blank_layer)
    } else {
      return(r[[idx]])
    }
  })
  
  r_full <- rast(layer_list)
  time(r_full) <- time_sequence
  names(r_full) <- paste0(format(time_sequence, "%Y_%m_%d"))
  
  return(r_full)
}
