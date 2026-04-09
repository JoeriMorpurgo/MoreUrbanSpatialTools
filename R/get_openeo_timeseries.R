#' Retrieve Time Series Image Collection from openEO
#'
#' This function requests all available images within a time frame without temporal aggregation.
#' 
#' @param aoi an object with an extent (e.g., sf object)
#' @param city_name character. Name of the city for file naming
#' @param startdate date/character. Start date (YYYY-MM-DD)
#' @param enddate date/character. End date (YYYY-MM-DD)
#' @param satellite character. "sentinel2" or "landsat8"
#' @param indicator character. "NDVI", "EVI", "MSAVI", "LST", or "NDWI"
#' @param cloud_threshold numeric. Max cloud cover percentage (0-100)
#' @export

get_timeseries_openeo <- function(aoi, 
                                  city_name, 
                                  startdate, 
                                  enddate,
                                  satellite, 
                                  indicator,
                                  cloud_threshold = 50) {
  
  # 0. Check for existing data (Note: extension is now .nc for NetCDF)
  id_string <- paste0(startdate, "_", enddate, "_", satellite, "_", indicator, "_timeseries")
  pre_download_check_result <- pre_download_check(city_name = city_name, object = id_string)
  
  if(!is.null(pre_download_check_result)){
    return(pre_download_check_result)
  }
  
  ## 1. Satellite Settings
  if(satellite == "sentinel2"){
    idSat <- "SENTINEL2_L2A"
    mask_band <- "SCL"
    mask_values <- c(1,3,8,9,10,11) # Filter clouds, shadows, etc.
  }
  
  if(satellite == "landsat8"){
    idSat <- "LANDSAT8_L2"
    mask_band <- "QA_PIXEL"
    mask_values <- c(22280,23888,24088,24200,24328,24456)
  }
  
  # Set bands for indicator
  bandsIndicator <- switch(indicator,
                           "NDVI"  = c("B08", "B04"),
                           "EVI"   = c("B02", "B04", "B08"),
                           "MSAVI" = c("B08", "B04"),
                           "LST"   = c("ST_B10"),
                           "NDWI"  = c("B8A", "B11"))
  
  ## 2. Connection & Login
  if(is.null(active_connection())){
    con = connect(host = "https://openeo.dataspace.copernicus.eu")
  }
  if(is.null(list_jobs())){ login() } 
  
  if(!exists("p")){ p = processes() }
  
  # Get bbox
  bbox_aoi <- st_bbox(aoi)
  
  ## 3. Data Loading & Masking
  data = p$load_collection(
    id = idSat,
    spatial_extent = list(west = bbox_aoi[1], south = bbox_aoi[2],
                          east = bbox_aoi[3], north = bbox_aoi[4]),
    temporal_extent = c(startdate, enddate),
    bands = c(bandsIndicator, mask_band),
    properties = list("eo:cloud_cover" = function(x) x <= cloud_threshold)
  )
  
  # Masking logic (removes pixels based on SCL/QA_PIXEL)
  mask_cube = p$filter_bands(data, bands = mask_band)
  boolean_mask = p$apply(data = mask_cube, process = function(x, context) {
    p$array_contains(data = mask_values, value = x)
  })
  
  data = p$mask(data = data, mask = boolean_mask)
  data = p$filter_bands(data, bands = bandsIndicator)
  
  ## 4. Calculate Indicator (per time-step)
  # reduce_dimension on "bands" keeps the "t" (time) dimension intact
  data = p$reduce_dimension(
    data = data,
    dimension = "bands",
    reducer = function(bands, context) {
      if(indicator == "NDVI")  return(p$normalized_difference(bands[1], bands[2]))
      if(indicator == "NDWI")  return(p$normalized_difference(bands[1], bands[2]))
      if(indicator == "EVI")   return((2.5 * (bands[3]/10000 - bands[2]/10000)) / (bands[3]/10000 + 6 * bands[2]/10000 - 7.5 * bands[1]/10000 + 1))
      if(indicator == "MSAVI") return((2 * bands[1] + 1 - ((2 * bands[1] + 1)^2 - 8 * (bands[1] - bands[2]))^0.5) / 2)
      if(indicator == "LST")   return((bands[1] * 0.00341802) + 149 - 273.15)
    }
  )
  
  ## 5. Save and Download (NetCDF for Time Series)
  # We do NOT use p$reduce_dimension(dimension = "t") here
  
  output_filename <- paste0(id_string, ".nc")
  output_path <- file.path("./MUST_downloaded_data", city_name, output_filename)
  
  if (!dir.exists(dirname(output_path))) dir.create(dirname(output_path), recursive = TRUE)
  
  result = p$save_result(data = data, format = "netCDF")
  
  print("Computing Job... this may take a while for full time series.")
  compute_result(result, output_file = output_path)
  
  ## 6. Post-processing with terra
  # Loading NetCDF into R
  resultingRast <- terra::rast(output_path)
  
  # Apply Clamping if necessary
  if(indicator %in% c("NDVI", "NDWI", "MSAVI", "EVI")){
    resultingRast <- terra::clamp(resultingRast, lower = -1, upper = 1)
  }
  
  # Overwrite with clamped version
  terra::writeRaster(resultingRast, filename = output_path, overwrite = TRUE)
  
  return(resultingRast)
}