#' Retrieve municipal bbox as sf or GEE object
#'
#' This function is a wrapper around getbb and either returns an sf or EE object.
#' NOTE: 
#' Literature: 
#' @param city_name character value. name of a city to retrieve the bbox for.
#' @param EE TRUE or FALSE. If the returned object should be suitable for GEE
#' @export
#' @examples
#' PLACEHOLDER()
#' 

############ get_municipal_bbox #############
get_municipal_bbox <- function(city_name,
                               EE = F){ #If it needs to be for EE, 
  
  #Urban defined by OSM
  urban_bbox_matrix <-  getbb(city_name, format_out = "matrix")
  # Extract corners from bbox matrix
  xmin <- urban_bbox_matrix[1, 1]
  xmax <- urban_bbox_matrix[1, 2]
  ymin <- urban_bbox_matrix[2, 1]
  ymax <- urban_bbox_matrix[2, 2]
  
  # Define corners in clockwise order (and close the polygon)
  coords <- matrix(c(
    xmin, ymin,
    xmax, ymin,
    xmax, ymax,
    xmin, ymax,
    xmin, ymin  # close the polygon
  ), ncol = 2, byrow = TRUE)
  
  # Create sf polygon
  urban_bbox <- st_sf(
    geometry = st_sfc(
      st_polygon(list(coords)),
      crs = 4326
    )
  )
  
  if(EE){
    urban_bbox <- st_geometry(urban_bbox)
    urban_bbox <- sf_as_ee(urban_bbox)
  }
  
  return(urban_bbox) #To be used in ee_as_raster
}