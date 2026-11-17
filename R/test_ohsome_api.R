#' Test Ohsome V2 API with minimal request to see if servers are OK
#'
#' @param verbose Number. 0-3 to indicate the level of verbosity from req_perform()
#' @export
#' @examples
#' #PLACEHOLDER()
#' 

test_ohsome_api <- function(verbose = 3){

  #minimal API query
  test_payload <- list(filter = "amenity=school",
                       aoi    = c(5.24, 52.11, 5.26, 52.13), 
                       time   = "2024-01-01",
                       clip   = FALSE)

  cat("Sending query to Ohsome V2 API")
  tryCatch(resp <- httr2::request("https://api.heigit.org/ohsome-api/v2/extraction/features.parquet") |>
             httr2::req_headers("authorization" = get_ohsome_key(), "content-Type"  = "application/json") |>
             httr2::req_body_json(test_payload) |>
             httr2::req_perform(verbosity = verbose)
           , 
           error = function(e){return("Failed OHSOME API query. Please check https://status.heigit.org/ or your API key.")})
  

  return(cat("Succesful OHSOME API query"))

}
