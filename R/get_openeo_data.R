#' API request for GEE data
#'
#' This function works with openeo to request data via API requests to R. 
#' 
#' NOTE: https://openeo.dataspace.copernicus.eu/ For Sentinel2 data is harmonizes the values before and after baseline shift occuring in 2025. 
#' Literature: 
#' @param aoi an object with an extent
#' @param city_name character. Name of the city for the data retrieval
#' @param startdate date. Start of the requesting images
#' @param enddate date. End of the requesting images
#' @param sattelite character value. Sentinel or (future; Landsat)
#' @param indicator Character value; NDVI (future; EVI or LST)
#' @param method Character value. max, 90th, median, monthly_max, monthly_90th, monthly_median or none (for all images). 90th indicates the 90th percentile of a pixel value over the time period. Note: none will retrieve massive amounts of images.
#' @param cloud_threshold numeric. Standard at 50. 
#' @keywords API, Remote Sensing, Indicators, openEO
#' @export
#' @examples
#' get_openeo_data()
#' 

get_data_openeo <- function(aoi, #some aoi object/border
                            city_name, #to check if we have data already or save name with
                            startdate, enddate,#Start and end analysis in time
                            satellite, #SENT-2, LSAT5/7/8
                            indicator, #NDVI, NDWI, MSAVI EVI, LST
                            method, #max, 90th, median, monthly_max, monthly_90th, monthly_median or none
                            cloud_threshold = 80) {
  
#Check if we already have this data downloaded
pre_download_check_result <- pre_download_check(city_name = city_name,
                                                object = paste0(startdate,"_",enddate,"_",satellite,"_",indicator,"_",method),
                                                indicator = indicator) #this checks if the file is present in subfolder
if(is.null(pre_download_check_result)){
  
  ##set satellite settings
  #Sentinel 2
  if(satellite == "sentinel2"){
    idSat <- "SENTINEL2_L2A"
    mask_band <- "SCL"
    mask_values <- c(1,3,8,9,10)
    cloud_property <- "eo:cloud_cover"
    }
  
  #Landsat 8
  if(satellite == "landsat"){
    idSat <- "LANDSAT_BIMONTHLY_MOSAIC"
    dn_offset <- 0
    }
  
  #set bands for indicator
  if(indicator == "NDVI"){bandsIndicator <- c("B08", "B04")} #NIR then red
  if(indicator == "EVI"){bandsIndicator <- c("B02", "B04", "B08")} #
  if(indicator == "MSAVI"){bandsIndicator <- c("B08", "B04")} # doi.org/10.1016/0034-4257(94)90134-1
  if(indicator == "LST"){bandsIndicator <- c("B07")} #10.7717/peerj.18585
  if(indicator == "NDWI"){bandsIndicator <- c("B8A", "B11")} # doi.org/10.1016/j.ecolind.2025.113757
  
  
  # 1. Conditional connection to Copernicus Data Space Ecosystem (CDSE)
  if(is.null(openeo::active_connection())){
                                  con = openeo::connect(host = "https://openeo.dataspace.copernicus.eu")}
  if(is.null(openeo::list_jobs())){ #if we can access jobs, assume we need to login
                          openeo::login()} 
  Sys.sleep(5) #for robustness?
  
  if(is.null(openeo::active_process_collection())){
                                          p = openeo::processes()} #functions to be used
  if(!exists("p")){p = openeo::processes()}
  print("Logged in and connected")
  
  # 1.5 Get the bbox
  aoi <- sf::st_bbox(aoi)
  
  print("Set bounding box")
  
  # 2. Gather the images
  data = p$load_collection(
            id = idSat,
            spatial_extent = list(west = aoi[1], south = aoi[2],
                                  east = aoi[3], north = aoi[4]),
            temporal_extent = c(startdate, enddate),
            bands = if(idSat == "SENTINEL2_L2A"){c(bandsIndicator, mask_band)}else{bandsIndicator},
              if(idSat == "SENTINEL2_L2A"){properties = list("eo:cloud_cover" = function(x) x <= cloud_threshold) #not for landsat
            }
  )
  print("Found image collection")
  
  #2.5 data masking
  if(idSat == "SENTINEL2_L2A"){ #landsat is already masked etc.
  mask_cube = p$filter_bands(data, bands = mask_band)
  
  boolean_mask = p$apply(
    data = mask_cube,
    process = function(x, context) {
      p$array_contains(data = mask_values, value = x)
    }
  )
  data = p$mask(data = data, mask = boolean_mask) 
  data = p$filter_bands(data, bands = bandsIndicator) 
  print("Masked sentinel2 images")
  }
  
  # 3. Calculate the indicator
  if(indicator == "NDVI") {
    data = p$reduce_dimension(
      data = data,
      dimension = "bands",
      reducer = function(bands, context) {
        
        p$normalized_difference(bands[1], bands[2])
      }
    )
  }
  
  if(indicator == "EVI") {
    data = p$reduce_dimension(
      data = data,
      dimension = "bands",
      reducer = function(bands, context) {
        blue <- bands[1]/10000
        red <- bands[2]/10000
        nir <- bands[3]/10000
        
        #evi formula
        (2.5*(nir-red))/((nir+6*red-7.5*blue)+1)
      }
    )
  }
  
  if(indicator == "MSAVI") {
    data = p$reduce_dimension(
      data = data,
      dimension = "bands",
      reducer = function(bands, context) {
        nir <- bands[1]/10000
        red <- bands[2]/10000
        
        #function
        (2 * nir + 1 - ((2 * nir + 1)^2 - 8 * (nir - red))^0.5) / 2
      }
    )
  }
  
  if(indicator == "LST") {
    data = p$reduce_dimension(
      data = data,
      dimension = "bands",
      reducer = function(bands, context) {
        bands[1] - 150
      }
    )
  }
  
  if(indicator == "NDWI") {
    data = p$reduce_dimension(
      data = data,
      dimension = "bands",
      reducer = function(bands, context) {
        
        p$normalized_difference(bands[1], bands[2])
      }
    )
  }
  
  # 4. Calculate the method
  if (method == "max") { #maximum of a cell in a year
    data = p$reduce_dimension(
      data = data,
      dimension = "t",
      reducer = p$max
    )
  }
  
  if (method == "90th") { # 90th percentile of a cell in a year
    data = p$reduce_dimension(
      data = data,
      dimension = "t",
      reducer = function(data, context) {p$quantiles(data = data, probabilities = c(0.9))}
    )
  }
  
  
  if (method == "median") { # median of a cell in a year
    data = p$reduce_dimension(
      data = data,
      dimension = "t",
      reducer = p$median
    )
  }
  
  if (method == "monthly_max") {
    # Aggregates your timeline into monthly maximum composites
    data = p$aggregate_temporal_period(
      data = data,
      period = "month",
      reducer = p$max
    )
  }
  
  if (method == "monthly_90th") { # 90th percentile of a cell in a year
    data = p$aggregate_temporal_period(
      data = data,
      period = "month",
      reducer = function(data, context) {p$quantiles(data = data, probabilities = c(0.9))}
    )
  }
  
  if (method == "monthly_median") {
    # Aggregates your timeline into monthly median composites
    data = p$aggregate_temporal_period(
      data = data,
      period = "month",
      reducer = p$median
    )
  }
  
  #no temporal aggregation of the datacube
  if (method == "none") {} #skip temporal aggregatopm
  
  
  
  # 5. Save and Download
  # Check if 'data' actually exists before calling save_result
  print("Sending data request")
  if (!is.null(data)) {
    
    # check if the request is a timeseries and change .suffix accordingly
    is_timeseries <- method %in% c("none", "monthly_max", "monthly_90th", "monthly_median")
    file_ext <- if (is_timeseries) ".nc" else ".tif"
    export_format <- if (is_timeseries) "NetCDF" else "GTiff"
    
    # paths
    save_dir <- file.path("./MUST_downloaded_data", city_name, indicator)
    if(!dir.exists(save_dir)){dir.create(save_dir, recursive =T)}
    base_name <- paste0(startdate, "_", enddate, "_", satellite, "_", indicator, "_", method)
    raw_path <- file.path(save_dir, paste0(base_name, "_raw", file_ext))
    final_path <- file.path(save_dir, paste0(base_name, file_ext))
    
    #keep running the request until the file is here
    attempt <- 1
    max_attempts <- 5
    
    #compute and retrieve result
    while (!file.exists(raw_path) && attempt <= max_attempts) { #repeat this task until file exists
      
      #calc result  
      if (is_timeseries) {
        result = p$save_result(
          data = data,
          format = export_format
        )
      } else {
        result = p$save_result(
          data = data,
          format = export_format,
          options = list(datatype = "float32") # A named list correctly translates to a dictionary
        )
      }
      
      # compute and retrieve
      openeo::compute_result(result, output_file = raw_path)
      
      #add to the attemptcounter
      Sys.sleep(10)
      attempt <- attempt + 1
      cat("attempt(s)",attempt)
    }
  } else {
    stop("The openEO 'data' object is NULL")
  }
  
  
  #pulling data and indicator specific clamping
  cat("Data saved now pulling in R environment")
  resultingRast <- terra::rast(raw_path)
  
  if(indicator %in% c("NDVI", "NDWI","MSAVI", "EVI")){ #values above 1 are urnealistic
    resultingRast <- terra::clamp(resultingRast, lower = -1, upper = 1, values = F)
    } 
  
  if(file_ext == ".nc"){
  #fix the names  
  raw_names <- names(resultingRast)
  num_days <- as.numeric(gsub("var_t=","",raw_names))
  actual_dates <- as.Date("1990-01-01") + num_days
      
    #split temp aggregation
    if(method == "none"){
      names(resultingRast) <- format(actual_dates, "%Y-%m-%d")
    }else{
      names(resultingRast) <- format(actual_dates, "%Y-%m")
      }
  
  #save stack
  terra::writeCDF(resultingRast, filename = final_path, overwrite = T)
  }else{
  terra::writeRaster(resultingRast, filename = final_path, overwrite = T)
  }
  
  #return object
  return(resultingRast)
  unlink(raw_path)
}
else
{
  return(pre_download_check_result)
}
  
}
