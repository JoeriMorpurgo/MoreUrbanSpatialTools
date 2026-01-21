#' Retry failed API requests
#'
#' This function retries failed API requests with increasing long wait periods. 
#' Wait period is calculated as attempt * wait time.
#' NOTE: 
#' Literature: 
#' @param osm_function function for sending API request
#' @param ... arguments for osm_function
#' @param max_attempts numeric value. Maximum number of attempts
#' @param wait_time numeric value. Wait between attempts. 
#' @keywords classification, height, proportion
#' @export
#' @examples
#' robust_api_request()
#' 

################### robust_osm_request #################
robust_api_request <- function(osm_function, ..., max_attempts = 6, wait_time = 10) {
  #Empty this vector
  result <- NULL
  attempt <- 1 #Start try counter
  
  while (is.null(result) && attempt <= max_attempts) {
    message("Attempt ", attempt)
    
    result <- tryCatch(
      {
        do.call(osm_function, list(...))
      },
      error = function(e) {
        message("Attempt ", attempt, " failed: ", e$message)
        NULL
      }
    )
    
    if (is.null(result)) {
      Sys.sleep(wait_time* attempt)
      attempt <- attempt + 1
    }
  }
  
  if (is.null(result)) {
    stop("Failed API query at after ", max_attempts, " attempts.")
  }
  
  message("API query succeeded")
  return(result)
}