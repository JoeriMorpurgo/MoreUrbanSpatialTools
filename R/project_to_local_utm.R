#' This function is made to check where the geospatial data is located on the globe and projects the local UTM to have a good approximation of meters for downstream analysis.
#'

#' NOTE: 
#' Literature: 
#' @param object Geospatial object. This can be sf, raster or terra object.
#' @export
#' @examples
#' PLACEHOLDER()
#' 


project_to_local_utm <- function(object) {
  # 1. Check the object type and dynamically find its geographic center
  if (inherits(object, c("sf", "sfc"))) { #if it is a sf object
    if (is.na(sf::st_crs(object))) stop("Object is missing a coordinate reference system (CRS).") #and had CRS
    
    # Get bounding box and find center point
    bbox <- sf::st_bbox(object) #take the bounding box
    center_pt <- sf::st_sfc( #designate a centre point based on the bbox and CRS
      sf::st_point(c(mean(bbox[c("xmin", "xmax")]), mean(bbox[c("ymin", "ymax")]))), 
      crs = sf::st_crs(object)
    )
    # Transform center point to long/lat to read degrees
    center_geo <- sf::st_transform(center_pt, "EPSG:4326")
    coords <- sf::st_coordinates(center_geo)
    lon <- coords[1]
    lat <- coords[2]
    
  } else if (inherits(object, c("SpatRaster", "SpatVector"))) {
    if (terra::crs(object) == "") stop("Object is missing a coordinate reference system (CRS).")
    
    # Get extent and find center point
    e <- terra::ext(object)
    center_pt <- terra::vect(matrix(c(mean(e[1:2]), mean(e[3:4])), ncol=2), crs = terra::crs(object))
    
    # Transform center point to long/lat to read degrees
    center_geo <- terra::project(center_pt, "EPSG:4326")
    coords <- terra::crds(center_geo)
    lon <- coords[1, 1]
    lat <- coords[1, 2]
    
  } else {
    stop("Unsupported format. Input must be an sf, sfc, SpatRaster, or SpatVector object.")
  }
  
  # 2. Dynamically calculate the modern UTM EPSG code
  utm_zone <- floor((lon + 180) / 6) + 1
  utm_zone <- min(max(utm_zone, 1), 60) # Guardrails for edge longitudes
  
  epsg_code <- if (lat >= 0) 32600 + utm_zone else 32700 + utm_zone
  target_crs <- paste0("EPSG:", epsg_code)
  
  message(paste("Projecting object to local UTM zone:", target_crs))
  
  # 3. Project the object back using its native package method
  if (inherits(object, c("sf", "sfc"))) {
    return(sf::st_transform(object, target_crs))
  } else {
    return(terra::project(object, target_crs))
  }
}
