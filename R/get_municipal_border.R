#' Retrieve current or historic municipal border limited to annual temporal resolution
#'
#' This function is retrieves the municipal border for either current time (Open Street Maps) or historic (Ohsome). The Ohsome fnction also iteratively check lower administrative levels if no data is found at level 8. This is more robust and will be present in the OSM argument at a later point.
#' NOTE: 
#' Literature: 
#' @param city_name character value. name of a city to retrieve the bbox for.
#' @param historic TRUE or FALSE. Wether to use Ohsome or OSM
#' @param date date/time. Which point in time to retrieve data from Ohsome
#' @export
#' @examples
#' PLACEHOLDER()
#' 


############################ get_municipal_border #################
get_municipal_border <- function(city_name,
                                 historic = FALSE,
                                 date = NULL) {

#date to year  transformation
dateYear <- year(as.POSIXct(date))
    
#Check if we already have this data downloaded
pre_download_check_result <- pre_download_check(city_name = city_name,
                                                object = paste0("municipal_border_",dateYear))
if(is.null(pre_download_check_result)){ #if there is no file already, run the code.  
  
  

  #Historic or current border?
  if(historic){  #Historic Ohsome data
    
          #bbox
          city_bbox <- MUST::get_municipal_bbox(city_name)
          city_bbox <- sf::st_simplify(city_bbox, dTolerance = 0.1, preserveTopology = TRUE)
          city_bbox <- sf::st_make_valid(city_bbox)
          Sys.sleep(1)
          
                #Robust API query
                #sometimes municipal borders are on different levels so have it check them iteratively.
                for (level in c(8,7,6)) {
                  
                  #query itself  
                  query <- ohsome::ohsome_elements_geometry(
                    boundary = city_bbox, 
                    filter = paste0("boundary=administrative and admin_level=",level), 
                    time = date,
                    properties = "tags", 
                    clipGeometry = T)
                  
                    #Send request
                    message("querying municipal borders")
                    res <- ohsome::ohsome_post(query) 
                    
                    #check if we have a result
                    if(nrow(res)>0){break}else{cat("No result for query on administrative level ", level)}
                }
          message("Received border from query.")        
          
          
          #Code to select best fit for city.... This is a bit roundabout but OK....
          max_dist = 0.95
          element <- integer(0)
          city <- sub(",.*", "", city_name)
          if(city %in% res$name){element <- which(res$name %in% city)
            }else{
              while (length(element) == 0 && max_dist > 0) {
                max_dist <- max_dist - 0.05
                element <- agrep(tolower(city), tolower(res$name), max.distance = max_dist, value = F)
              }
              if (length(element) > 1) {
                d <- utils::adist(tolower(city), tolower(res$name[element]))
                element <- element[which.min(d)]
              }
            }
    
    
          #Select the row with the correct geometry
          geometry <- res[element,]
          admin_border <- geometry$geometry
          admin_border <- sf::st_sf(geometry = admin_border)
          
  }else{ 
    
          #Current OSM data
          city_bbox <- osmdata::getbb(city_name)
          

          #Query
          message("API request OSM border")
          query <- osmdata::opq(bbox = city_bbox) %>%
            osmdata::add_osm_feature(key = "boundary", value = "administrative") %>%
            osmdata::add_osm_feature(key = "admin_level", value = "8")
          
          # Send the request robustly
          admin_border <- MUST::robust_api_request(
            osm_function = osmdata_sf,  # function to call
            q = query,                   # the opq query object
            quiet = FALSE                # optional argument to osmdata_sf
          )
          
          
          message("Received borders")
          admin_border <- admin_border$osm_multipolygons
          
          #Code to select best fit for city.... This is a bit roundabout but OK....
          max_dist = 0.9
          element <- integer(0) #This is terrible coding. But works.
          city <- sub(",.*", "", city_name)
          if(city %in% admin_border$name){element <- which(admin_border$name %in% city)} else{
            while (length(element) == 0 && max_dist > 0) {
              max_dist <- max_dist - 0.05
              element <- agrep(tolower(city_name), tolower(admin_border$name), max.distance = max_dist, value = F)
              print(element)
            }
            if (length(element) > 1) {
              d <- utils::adist(tolower(city), tolower(admin_border$name[element]))
              element <- element[which.min(d)]
            }
          }
          message(admin_border$name[element])
          
          
          geometry <- admin_border[element,]
          admin_border <- sf::st_geometry(geometry) #Just the geometry
    
  }
  
  #save the result
  base::saveRDS(admin_border,
              file = paste0("MUST_downloaded_data/",city_name,"/municipal_border_",dateYear))
  
  message("Requested border has been returned and saved")
  Sys.sleep(1)
  return(admin_border)
  
}
else
{
  return(pre_download_check_result)
  }
  
}
