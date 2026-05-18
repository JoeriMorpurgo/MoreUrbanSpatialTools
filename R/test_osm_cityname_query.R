#' Test city name(s) in OSM
#'
#' This function uses geo() to test city name(s) and then checks if the results are OK and which are pot missing or wrong.
#' NOTE: 
#' Literature: 
#' @param cities character vector. Value containing city names
#' @param map TRUE or FALSE. If map with points of the cities should be shown
#' @keywords classification, height, proportion
#' @export
#' @examples
#' diff_gain_loss()
#' 

test_osm_cityname_query <- function(cities, map = F){ #for geocoder.

result <- tidygeocoder::geo(cities, method = "osm", full_results = T)
  


#check if lat long are there
if(any(is.na(result$lat) | is.na(result$long))){
  print("Something is missing")
  missing <- which(is.na(result$lat)) | which(is.na(result$long))
  result$address[missing] #identify the ones that are missing
} else {
  print("All cities are found in OSM.")
}

if (map) {
  points <- data.frame(lon = result$long, lat = result$lat)
  points <- sf::st_as_sf(points, coords = c("lon", "lat"), crs = "EPSG:4326")
  m <- mapview::mapview(points)
  return(list(result = result, map = m))
} else {
  return(result)
}

}