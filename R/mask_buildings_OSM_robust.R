#' Retrieves historical building polygons from ohsome to mask
#'
#' This function takes the extent from a raster and the set time to request building data from ohsome. 
#' After it masks out cells from the raster that overlap with the buildings.
#' NOTE: 
#' Literature: 
#' @param rast raster object. Raster that you'd like to mask out if they verlap with buildings at some point in time
#' @param timestap date/time. For when to retrieve the building data.
#' @keywords classification, height, proportion
#' @export
#' @examples
#' mask_buildings_ohsome_robust()
#' 


mask_buildings_ohsome_robust <- function(rast, timestamp = "2017-01-01T00:00:00Z") { 
  cat("Getting AOI from raster extent...\n")
    
  e <- ext(rast)
  bbox <- st_bbox(e)
  bbox <- st_as_sfc(bbox)
  st_crs(bbox) <- st_crs(rast)
  
  #failsafe for engineering CRS, tile M_38CN2
  is_engi_crs <- grepl("^ENGCRS", st_crs(bbox)$wkt)
  if (is_engi_crs) {
    message("BBox has an Engineering CRS (STEP 1), assigning EPSG:28992 (RDnew, IF INCORRECT STOP!)")
    st_crs(bbox) <- 28992  # Amersfoort / RD New
  }
  
  bbox <- st_transform(bbox, crs = st_crs("EPSG:4326"))
  bbox <- st_geometry(bbox)
  bbox <- st_make_valid(bbox)
  
  cat("Creating ohsome query for buildings at", timestamp, "...\n")
  
  # define API query
  buildings_sf <- ohsome_elements_geometry(
    boundary = bbox,
    time = timestamp,
    filter = "building=*"
  )
  
  print(st_as_text(bbox))
  cat("Sending query to Ohsome")
  
  #Send API req with robust wrapper to retry upon fail  
  buildings_sf_poly <- robust_api_request(
    ohsome_post,
    buildings_sf
  )
  
    cat("Retrieved requested data")
    
  if (is.null(buildings_sf_poly) || nrow(buildings_sf_poly) == 0) {
    cat("No building data found at this timestamp.\n")
    return(rast)
  }
  
  cat(paste0("Masking raster with ", nrow(buildings_sf_poly), " building polygons at once...\n"))
  
  # Convert to terra vector and project to raster CRS
  buildings_vect <- vect(buildings_sf_poly$geometry)
  
  #failsafe
  is_engi_crs <- grepl("^ENGCRS", st_crs(rast)$wkt)
  if (is_engi_crs) {
    message("BBox has an Engineering CRS (STEP 2), assigning EPSG:28992 (RDnew, IF INCORRECT STOP!)")
    crs(rast) <- "epsg:28992"  # Amersfoort / RD New
  }
  
  buildings_vect <- project(buildings_vect, crs(rast))
  
  # Mask the raster: remove cells overlapping buildings
  rast_masked <- mask(rast, buildings_vect, inverse = TRUE)
  
  cat("Masking complete.\n")
  return(rast_masked)
}