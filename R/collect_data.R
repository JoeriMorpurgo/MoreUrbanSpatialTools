#' Retrieve data objects for a city from 2017 to current.
#'
#' This function is a wrapper around several functions in the MUST package. The intended use is to simplify the data gathering process. 
#' The data retrieved are annual max values. 
#' NOTE: 
#' Literature: 
#' @param city_name character value. name of a city and country to retrieve data for. Format should be "city, country". 
#' @param path character value. path to where data should be stored.
#' @export
#' @examples
#' PLACEHOLDER()
#' 

collect_data <- function(city_name,
                         path){

#to revert the WD after function  
originalPath <- getwd()  
#set WD for this function
if(getwd()!=path){setwd(path)}

#starting date of Sentinel data
startdate <- "2017-01-01"

for(timestep in 0:8) { #2025 currently, but should scale in future automatically

  #start and end time
  startdate_time <- as.character(as.POSIXct(startdate)+years(timestep))
  enddate_time <- as.character(as.POSIXct(startdate)+years(timestep+1))
    
  #request municipal border
  print(paste0("Retrieving municipal border for ",city_name, " in year ", startdate_time))
  municipal_border <- get_municipal_border(city_name = city_name,
                                           historic = T,
                                           date = startdate_time)
  
  
      #sentinel2 data retrieval
      print(paste0("Retrieving RS S2 data for ",city_name, " in year ", startdate_time))
      for (RS in c("NDVI","MSAVI","NDWI")) {
        get_data_openeo(municipal_border,
                        city_name = city_name,
                        startdate = startdate_time,
                        enddate = enddate_time,
                        satellite = "sentinel2",
                        indicator = RS,
                        method = "max",
                        cloud_threshold = 50)
      }
  
      #landsat data retrieval
      get_data_openeo(municipal_border,
                      city_name = city_name,
                      startdate = startdate_time,
                      enddate = enddate_time,
                      satellite = "landsat",
                      indicator = "LST",
                      method = "max",
                      cloud_threshold = 50)
      
      
      #get LULC data
      print(paste0("Retrieving LULC data for ",city_name, " in year ", startdate_time))
      lulc <- get_lulc_ohsome(aoi = city_name,
                              date = startdate_time)
      
      #get veg structure data
      print(paste0("Calculcating vegetation structure ",city_name, " in year ", startdate_time))
      if(str_detect(city_name, "Netherland")){
        get_data_ahn(city_name = city_name,#this only works for the netherlands
                     date = startdate_time)}else{veg_structure_via_lulc(lulc)}
      
  }

#revert WD
setwd(originalPath)

print(paste0("Data retrievel is done for ", city_name, " at ", date()))

}
