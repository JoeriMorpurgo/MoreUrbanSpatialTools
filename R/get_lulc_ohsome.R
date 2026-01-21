#' Retrieves historic LULC data from Ohsome
#'
#' This function is a wrapper for ohsome_elements_geometry to retrieve historic LULC data from Ohsome. 
#' It improves it by making the request robust if it fails for some reason, omit extra data, combines poly and multipolygons, and allows for direct saving to disk.
#' NOTE: 
#' Literature: 
#' @param aoi SpatRaster, raster, sf or character value. Is the area of interest
#' @param keyval_df dataframe or matrix with 2 columns; 1) key 2) value. This corresponds to Ohsome datastructure.
#' @param year numeric value. Indicates the year of the data you request.
#' @param filename character value. Path to folder where the file should be saved as a .gpkg
#' @keywords OSM, Ohsome, Land-use, Land-cover, historic
#' @export
#' @examples
#' PLACEHOLDER()
#' 

get_lulc_ohsome <- function(aoi, keyval_df, year, filename = NULL) {
  
  # Validate input
  if (!is.data.frame(keyval_df) && !is.matrix(keyval_df)) {
    stop("keyval_df must be a data.frame or matrix with two columns: key, value")
  }
  if (ncol(keyval_df) < 2) {
    stop("keyval_df must have two columns: key, value")
  }
  
  
  # Extract bbox depending on class
  if (inherits(aoi, "SpatRaster") || inherits(aoi, "SpatVector")) {
    bbox_vals <- terra::ext(aoi)
    
  } else if (inherits(aoi, "Raster")) {
    bbox_vals <- raster::extent(aoi)
    
  } else if (inherits(aoi, "sf")) {
    bbox_vals <- sf::st_bbox(aoi)
    
  } else if(is.character(aoi)){
    cat("Using, ", aoi, " to query bbox from OSM")
    bbox_vals <- get_municipal_border(aoi, historic = F, interactive = F)
  } else {
    stop("Unsupported spatial object. Use terra, raster, sf, or character value.")
  }
  
  #Save name for later
  name <- aoi
  
  # Build filter string from key/value pairs
  keyval_df <- as.data.frame(keyval_df, stringsAsFactors = FALSE)
  filter_parts <- paste0(keyval_df[[1]], "=", keyval_df[[2]])
  filter_str <- paste(filter_parts, collapse = " or ")
  
  # Convert year to ohsome time format
  time_str <- sprintf("%s-01-01", year)
  
  # define API query
  query_lulc_sf <- ohsome_elements_geometry(
    boundary = bbox_vals,
    time = time_str,
    filter = filter_str,
    properties = "tags",
    clipGeometry = TRUE
  )
  
  cat("Sending query to Ohsome")
  
  #Send API req with robust wrapper to retry upon fail  
  lulc_sf <- robust_api_request(
    ohsome_post,
    query_lulc_sf
  )
  
  cat("Received Ohsome data")
  
  #Fix the geometries
  lulc_sf <- st_make_valid(lulc_sf) #first fix
  lulc_sf_poly <- lulc_sf[st_geometry_type(lulc_sf) %in% c("POLYGON", "MULTIPOLYGON"), ] #omit points
  lulc_sf_poly <- lulc_sf_poly[st_is_valid(lulc_sf_poly),] #Hard check
  if(is.character(aoi)){aoi <- st_make_valid(bbox_vals)} else {aoi <- st_make_valid(aoi)} #if the input was character use the bbox to make valid
  
  
  #mask the retrieved LULC to the AOI
  cat("masking ohsome lulc to border")
  lulc_sf_masked <- st_intersection(lulc_sf_poly, aoi)
  
  #omitting columns that are "extra"
  lulc_sf_masked <- lulc_sf_masked[unique(keyval_df[[1]])] #reduce to what has been requested in the start
  lulc_vect_masked <- vect(lulc_sf_masked) #to use writeVector, which seems much quicker than st_write?
  #OKay about 100x faster atleast.
  
  if(is.character(filename)){
    cat("writing gpkg") #let user know that the LULC is being saved.
    writeVector(lulc_vect_masked, paste0(filename,".gpkg"),
                overwrite = T, insert = F, filetype = "GPKG")
    }
  
  
  #hand back the sf
  return(lulc_vect_masked)
}