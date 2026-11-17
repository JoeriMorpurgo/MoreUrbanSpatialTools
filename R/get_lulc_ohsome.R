#' Retrieves historic LULC data from Ohsome
#'
#' This function is a wrapper for ohsome_elements_geometry to retrieve historic LULC data from Ohsome. 
#' It improves it by making the request robust if it fails for some reason, omit extra data, combines poly and multipolygons, and allows for direct saving to disk.
#' NOTE: 
#' Literature: 
#' @param aoi SpatRaster, raster, sf or character value. Is the area of interest
#' @param date Charater or as.Date. String either containing a year, or a date that is recogninzed by as.Date. Required to pull data from the correct year.
#' @param keyval_df dataframe or matrix with 2 columns; 1) key 2) value. A dataframe with colnames (Key; Value) may be provided to pull different data from Ohsome.
#' @param filepath character value. Path to folder where the file should be saved as a .gpkg
#' @keywords OSM, Ohsome, Land-use, Land-cover, historic
#' @export
#' @examples
#' #PLACEHOLDER()
#' 

get_lulc_ohsome <- function(aoi, date,
                            keyval_df = NULL,
                            filepath = NULL) {

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
    bbox_vals <- get_municipal_border(city_name = aoi, date = date)
  } else {
    stop("Unsupported spatial object. Use terra, raster, sf, or character value.")
  }
  
  #Save name for later
  name <- aoi
  
  # Build filter string from key/value pairs
  if(is.null(keyval_df)){keyval_df <- as.data.frame(lulc_info, stringsAsFactors = FALSE)}
  filter_parts <- paste0(keyval_df[[1]], "=", keyval_df[[2]]) #Mostly full dataset from OSM/OHSOME
  filter_str <- paste(filter_parts, collapse = " or ")
  

  # Convert year to ohsome time format
  time_str <- as.character(date)
  
  ### OLD V1 OHSOME API REQUEST ###
  # # define API query
  # query_lulc_sf <- ohsome::ohsome_elements_geometry(
  #   boundary = sf::st_bbox(bbox_vals), #this simplifies the bbox, as large complex bboxes can gives issues....
  #   time = time_str,
  #   filter = filter_str,
  #   properties = c("metadata","tags"),
  #   clipGeometry = FALSE
  # )
  # 
  # #quick test
  # #test <- ohsome_post(query_lulc_sf)
  # 
  # cat("Sending query to Ohsome")
  # 
  # #Send API req with robust wrapper to retry upon fail  
  # lulc_sf <- robust_api_request(
  #   ohsome::ohsome_post,
  #   query_lulc_sf
  # )
  
  ### V2 OHSOME API REQUEST
  # 3. Define the v2 API payload
  payload <- list(filter = filter_str,
                  aoi    = as.numeric(sf::st_bbox(bbox_vals)), # simplified bounding box
                  time   = time_str,
                  clip   = FALSE)
  tmp_parquet <- tempfile(fileext = ".parquet")
  
  cat("Sending LULC query to Ohsome v2...\n")
  resp <- httr2::request("https://api.heigit.org/ohsome-api/v2/extraction/features.parquet") |>
    httr2::req_headers(
      "Authorization" = get_ohsome_key(), 
      "Content-Type"  = "application/json"
    ) |>
    httr2::req_body_json(payload) |>
    httr2::req_retry(max_tries = 3, max_seconds = 180) |>
    httr2::req_perform(path = tmp_parquet)
  
  # Pull from storage and transform into sf object
  lulc_sf <- arrow::read_parquet(resp$body)
  lulc_sf$geometry <- sf::st_as_sfc(lulc_sf$geom, crs = 4326)
  lulc_sf$geom <- NULL
  lulc_sf <- sf::st_as_sf(lulc_sf)
  
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
                     if(is.null(filepath)){file = paste0("./MUST_downloaded_data/",city_name,"/lulc",year,".gpkg")}else{
                       file = paste0(filepath,city_name,"/lulc",year,".gpkg")
                     })
  
  
  #hand back the sf
  return(lulc_vect_masked)
    }
  else
  {
    return(pre_download_check_result)
  }
}
