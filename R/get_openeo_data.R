#' API request for GEE data
#'
#' This function works with openeo to request data via API requests to R. 
#' 
#' NOTE: https://openeo.dataspace.copernicus.eu/ For Sentinel2 data is harmonizes the values before and after baseline shift occuring in 2025. 
#' Literature: 
#' @param aoi an object with an extent
#' @param city_name character. Name of the city for the data retrieval
#' @param start_date date. Start of the requesting images
#' @param end_date date. End of the requesting images
#' @param sattelite character value. Sentinel or (future; Landsat)
#' @param indicator Character value; NDV (future; EVI or LST)
#' @param method Character value. Max or Mediam
#' @param cloud_threshold numeric. Standard at 50. 
#' @keywords API, Remote Sensing, Indicators, openEO
#' @export
#' @examples
#' get_openeo_data()
#' 

get_data_openeo <- function(aoi, #some aoi object/border
                            city_name, #to check if we have data already or save name with
                            startdate, enddate,#Start and end analysis in time
                            satellite, #SENT-2, LSAT8
                            indicator, #NDVI, EVI, LST
                            method,
                            cloud_threshold = 50) {
  
#Check if we already have this data downloaded
pre_download_check_result <- pre_download_check(city_name = city_name,
                                                object = paste0(startdate,"_",enddate,"_",satellite,"_",indicator,"_",method))
if(is.null(pre_download_check_result)){
  
  ##set satellite settings
  #Sentinel 2
  if(satellite == "sentinel2"){
    idSat <- "SENTINEL2_L2A"
    mask_band <- "SCL"
    mask_values <- c(1,3,8,9,10,11)
    cloud_property <- "eo:cloud_cover"
    #To account for shift, i.e. in GEE harmonized dataset
    dn_offset <- 1000
    shift_date <- "2022-01-25"
    }
  
  #Landsat 8
  if(satellite == "landsat8"){
    idSat <- "LANDSAT8_L2"
    mask_band <- "QA_PIXEL"
    mask_values <- c(22280,23888,24088,24200,24328,24456)
    cloud_property <- "eo:cloud_cover"
    dn_offset <- 0
    }
  
  #set bands for indicator
  if(indicator == "NDVI"){bandsIndicator <- c("B08", "B04")} #NIR then red
  if(indicator == "EVI"){bandsIndicator <- c("B02", "B04", "B08")} #check formula
  if(indicator == "LST"){bandsIndicator <- c("ST_B10")}
  
  
  # 1. Conditional connection to Copernicus Data Space Ecosystem (CDSE)
  if(is.null(active_connection())){
                                  con = connect(host = "https://openeo.dataspace.copernicus.eu")}
  if(is.null(list_jobs())){ #if we can access jobs, assume we need to login
                          login()} 
  Sys.sleep(5) #for robustness?
  
  if(is.null(active_process_collection())){
                                          p = processes()} #functions to be used
  
  print("Logged in and connected")
  
  # 1.5 Get the bbox
  aoi <- st_bbox(aoi)
  
  print("Set bounding box")
  
  # 2. Gather the images
  data = p$load_collection(
            id = idSat,
            spatial_extent = list(west = aoi[1], south = aoi[2],
                                  east = aoi[3], north = aoi[4]),
            temporal_extent = c(startdate, enddate),
            bands = c(bandsIndicator, mask_band),
            properties = list(
              "eo:cloud_cover" = function(x) x <= cloud_threshold
            )
  )
  print("Found image collection")
  
  #2.5 data masking
  mask_cube = p$filter_bands(data, bands = mask_band)
  
  boolean_mask = p$apply(
    data = mask_cube,
    process = function(x, context) {
      p$array_contains(data = mask_values, value = x)
    }
  )
  
  # Apply the boolean mask to the original data
  data = p$mask(data = data, mask = boolean_mask)
  
  print("Masked images")
  
  # Remove the mask band before index calculation to keep the cube clean
  data = p$filter_bands(data, bands = bandsIndicator)
  
  # 2.6 Harmonization of Sentinel data
  if(satellite == "sentinel2") {
    print("Harmonizing: Moving pre-2022 images to Baseline 04.00 (adding 1000 DN)")
    
        if (as.Date(enddate) < as.Date(shift_date)) {
        # Data req is pre-shit
          data = p$apply(
            data = data,
            process = function(x, context) {
              p$add(x, 1000)
            }
          )
        }
    
    
        if (as.Date(startdate) >= as.Date(shift_date)) {
        # Data req is post-shift. Data is OK
        }
    
        if (as.Date(startdate) < as.Date(shift_date) && as.Date(enddate) >= as.Date(shift_date)) {
        # Data req covers the shift.
          data_pre = p$filter_temporal(
            data,
            extent = c(startdate, as.character(shift_date - 1))
          )
          
          data_post = p$filter_temporal(
            data,
            extent = c(as.character(shift_date), enddate)
          )
          
          data_pre = p$apply(
            data = data_pre,
            process = function(x, context) {
              p$add(x, 1000)
            }
          )
          
          data = p$merge_cubes(
            cube1 = data_pre,
            cube2 = data_post
          )
        }
    
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
  
  # 4. Calculate the method
  if (method == "max") {
    data = p$reduce_dimension(
      data = data,
      dimension = "t",
      reducer = p$max
    )
  }
  
  if (method == "median") {
    data = p$reduce_dimension(
      data = data,
      dimension = "t",
      reducer = p$median
    )
  }
  
  print("Sending data request")
  
  # 5. Save and Download
  # Check if 'data' actually exists before calling save_result
  if (!is.null(data)) {
    result = p$save_result(data = data,
                           format = "GTiff",
                           options = list(datatype = "float32"))
    compute_result(result,
                   output_file = paste0("./MUST_downloaded_data/",city_name,"/",startdate,"_",enddate,"_",satellite,"_",indicator,"_",method,".tif"))
  } else {
    stop("The openEO 'data' object is NULL")
  }
  
  cat("Data saved now pulling in R environment")
  resultingRast <- terra::rast(paste0("MUST_downloaded_data/",city_name,"/",startdate,"_",enddate,"_",satellite,"_",indicator,"_",method,".tif"))
  
  #Change it to RDS
  saveRDS(resultingRast,
           file = paste0("MUST_downloaded_data/",city_name,"/",startdate,"_",enddate,"_",satellite,"_",indicator,"_",method))
  resultingRast <- readRDS(paste0("MUST_downloaded_data/",city_name,"/",startdate,"_",enddate,"_",satellite,"_",indicator,"_",method))
  
  #removing .tif file for this package.
  file.remove(paste0("MUST_downloaded_data/",city_name,"/",startdate,"_",enddate,"_",satellite,"_",indicator,"_",method,".tif"))
  
  #return object
  return(resultingRast)
  
}
else
{
  return(pre_download_check_result)
}
  
}
