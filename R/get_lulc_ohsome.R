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

get_lulc_ohsome <- function(aoi, date) {

#time
year <- format(as.Date(date),"%Y") 
if(is.character(aoi)){city_name <- aoi}

#Check if we already have this data downloaded
pre_download_check_result <- pre_download_check(city_name = aoi,
                                                object = paste0("lulc",year))
if(is.null(pre_download_check_result)){ #No LULC object found
  
  
  # Extract bbox depending on class
  if (inherits(aoi, "SpatRaster") || inherits(aoi, "SpatVector")) {
    bbox_vals <- terra::ext(aoi)
    
  } else if (inherits(aoi, "Raster")) {
    bbox_vals <- raster::extent(aoi)
    
  } else if (inherits(aoi, "sf") || inherits(aoi, "sfc")) {
    bbox_vals <- sf::st_bbox(aoi)
    
  } else if(is.character(aoi)){
    cat("Using, ", aoi, " to query bbox from OSM")
    bbox_vals <- get_municipal_border(city_name = aoi, historic = T, date = date)
  } else {
    stop("Unsupported spatial object. Use terra, raster, sf, or character value.")
  }
  
  #Save name for later
  name <- aoi
  
  # Build filter string from key/value pairs
  keyval_df <- as.data.frame(lulc_info, stringsAsFactors = FALSE)
  filter_parts <- paste0(keyval_df[[1]], "=", keyval_df[[2]]) #Mostly full dataset from OSM/OHSOME
  filter_str <- paste(filter_parts, collapse = " or ")
  
  # Convert year to ohsome time format
  time_str <- sprintf(date)
  
  # define API query
  query_lulc_sf <- ohsome::ohsome_elements_geometry(
    boundary = bbox_vals,
    time = time_str,
    filter = filter_str,
    properties = c("metadata","tags"),
    clipGeometry = FALSE
  )
  
  cat("Sending query to Ohsome")
  
  #Send API req with robust wrapper to retry upon fail  
  lulc_sf <- robust_api_request(
    ohsome_post,
    query_lulc_sf
  )
  
  cat("Received Ohsome data")
  
  #Fix the geometries
  lulc_sf <- sf::st_make_valid(lulc_sf) #first fix
  lulc_sf_poly <- lulc_sf[sf::st_geometry_type(lulc_sf) %in% c("POLYGON", "MULTIPOLYGON"), ] #omit points
  lulc_sf_poly <- lulc_sf_poly[sf::st_is_valid(lulc_sf_poly),] #Hard check
  if(is.character(aoi)){aoi <- sf::st_make_valid(bbox_vals)} else {aoi <- sf::st_make_valid(aoi)} #if the input was character use the bbox to make valid
  
  
  #mask the retrieved LULC to the AOI
  cat("masking ohsome lulc to border")
  lulc_sf_masked <- sf::st_intersection(lulc_sf_poly, aoi)
  
  #omitting columns that are "extra"
  subsetNames <- base::unique(lulc_info[[1]])[base::unique(lulc_info[[1]]) %in% colnames(lulc_sf_masked)]
  lulc_sf_masked <- lulc_sf_masked[subsetNames] #reduce to what has been requested in the start
  lulc_vect_masked <- terra::vect(lulc_sf_masked) #to use writeVector, which seems much quicker than st_write?
  #OKay about 100x faster atleast.
  
  #coalesce LULC
  lulc_vect_masked$lulc <- dplyr::coalesce(lulc_vect_masked$landuse,
                                           lulc_vect_masked$leisure,
                                           lulc_vect_masked$natural,
                                           lulc_vect_masked$water,
                                           lulc_vect_masked$amenity)
  #remove double columns
  lulc_vect_masked <- lulc_vect_masked["lulc"]
  
  terra::writeVector(lulc_vect_masked, 
              file = paste0("MUST_downloaded_data/",city_name,"/lulc",year,".gpkg"))
  
  
  #hand back the sf
  return(lulc_vect_masked)
    }
  else
  {
    return(pre_download_check_result)
  }
}
