#' Retrieve data objects for a city from 2017 to current.
#'
#' This function is a wrapper around several functions in the MUST package. The intended use is to simplify the data gathering process. 
#' The data retrieved are annual max values. 
#' NOTE: 
#' Literature: 
#' @param city_name character value. name of a city and country to retrieve data for. Format should be "city, country". 
#' @param path character value. path to where data should be stored.
#' @param method character value. Either max or median.
#' @param indicators vector with character values. One or multiple of "NDVI", "MSAVI", "EVI", "LST","NDWI". Vegetation indices are NDVI, MSAVI and EVI. LST is a proxy for temperature via Land Surface Temperature. NDWI, Normalized Difference Water Indicator, is a proxy for water stress. 
#' @param lulc logic. True to fetch Ohsome data on Landuse and Landcover. 
#' @param AHN logic. Should always be F, unless the analysis is for a city in the Netherlands and AHN data is wanted.
#' @param startdate charachter. Standard is 2017-01-01, but may be later if wanted.
#' @param enddate charachter. standard is result of Sys.Date(). This fetches year-month-day of the system and removes 1 year as not all data is available. This means 2026-03-12 would result in a 2025 enddate. Alternatively, a enddate may be entered with the "year-month-day" format.
#' @export
#' @examples
#' PLACEHOLDER()
#' 

collect_data <- function(city_name,
                         path,
                         indicators, 
                         lulc = T,
                         method, #max, 90th, median, monthly_max, monthly_90th, median_90th or none.
                         startdate = "2017-01-01",
                         enddate = Sys.Date(), 
                         AHN = F){

  
  
#to revert the WD after function  
originalPath <- getwd()  
#set WD for this function
if(getwd()!=path){setwd(path)}

#temporal selection of  data
years_diff <- seq(lubridate::year(enddate)-lubridate::year(startdate))
steps <- seq_len(max(1, years_diff))-1
is_same_year <- (lubridate::year(startdate) == lubridate::year(enddate))

for(timestep in steps) { #steps depend on startdate and enddate

  #start and end time
  startdate_time <- as.character(as.POSIXct(startdate)+lubridate::years(timestep))
  enddate_time <- as.character(as.POSIXct(startdate)+lubridate::years(timestep+1))
    
  #request municipal border
  print(paste0("Retrieving municipal border for ",city_name, " in year ", startdate_time))
  municipal_border <- get_municipal_border(city_name = city_name,
                                           historic = T,
                                           date = startdate_time)
  
  
      ##sentinel2 data retrieval
      #indicator subselection for s2
      sen2RSoptions <- c("NDVI","MSAVI","NDWI", "EVI") #indicator options currently in get_data_openeo
      sen2RS <- sen2RSoptions[sen2RSoptions %in% indicators] #pick the one from indicators
  
      print(paste0("Retrieving RS S2 data for ",city_name, " in year ", startdate_time))
      for (RS in sen2RS) {
        get_data_openeo(municipal_border,
                        city_name = city_name,
                        startdate = if(is_same_year){startdate} else {startdate_time},
                        enddate = if(is_same_year){enddate} else{enddate_time},
                        satellite = "sentinel2",
                        indicator = RS,
                        method = method,
                        cloud_threshold = 50)
      }
  
      #landsat data retrieval
      if("LST" %in% indicators){ #if LST is in the indicators request, fetch it.
      get_data_openeo(municipal_border,
                      city_name = city_name,
                      startdate = if(is_same_year){startdate} else {startdate_time},
                      enddate = if(is_same_year){enddate} else{enddate_time},
                      satellite = "landsat",
                      indicator = "LST",
                      method = method,
                      cloud_threshold = 50)
      }
      
      #get LULC data
      if(lulc == T){ #should we fetch LULC data?
      print(paste0("Retrieving LULC data for ",city_name, " in year ", startdate_time))
      lulcFetch <- get_lulc_ohsome(aoi = city_name,
                              date = startdate_time)
      }
      
      #get veg structure data
      print(paste0("Calculcating vegetation structure ",city_name, " in year ", startdate_time))
      if(stringr::str_detect(city_name, "Netherland") & AHN == T){
        get_data_ahn(city_name = city_name, date = startdate_time)}else{veg_structure_via_lulc(lulc_map = lulcFetch)}
        }

#revert WD
setwd(originalPath)

print(paste0("Data retrievel is done for ", city_name, " at ", date()))

}
