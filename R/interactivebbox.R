#' Helper function that allows for interactive selection of municipalities
#'
#' This function takes municipal borders from OSM and then allows the user to select another municipal border via Mapview if the initial border by the function is wrong.
#'
#' NOTE: 
#' Literature: 
#' @param city_name character value. Name of a city
#' @param sleep numeric value. default = 1. This helps the function to be stable as mapview works via the viewer pane.
#' @keywords classification, height, proportion
#' @export
#' @examples
#' interactiveBbox()
#' 

########################### interactiveBbox() ############################
interactiveBbox <- function(city_name, sleep = 1){
  
  #Administrative borders for illustration purposes
  admin_border <- opq(city_name) %>%
    add_osm_feature(key = "boundary", value = "administrative") %>%
    add_osm_feature(key = "admin_level", value = "8") %>%
    osmdata_sf()
  
  
  selected <- selectFeatures(admin_border$osm_multipolygons) #Select the admin border that is relevant
  
  Sys.sleep(sleep)
  
  m <- mapview(selected, alpha.regions = 0.1)
  
  Sys.sleep(sleep)
  
  drawn <- editMap(m) #Cut off the irrelevant stuff manually
  
  Sys.sleep(sleep)
  
  drawnSelected <- st_intersection(drawn$drawn$geometry, selected) #Make sure the drawn is within geometry
  
  poly_coords <- as.matrix(unclass(st_geometry(drawnSelected))[[1]])
  
  if(is.null(drawn$drawn)){selectedBbox <- bbox} #Just the bbox
  if(!is.null(drawn$drawn)){selectedBbox <- poly_coords} #polygon with coordinates functioning as bbox
  
  Sys.sleep(sleep) #Seems to run too quick for mapview????
  
  return(selectedBbox)
}
