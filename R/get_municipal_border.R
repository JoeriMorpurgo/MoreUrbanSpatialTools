#' Retrieve current or histroric municipal border
#'
#' This function is retrieves the municipal border for either current time (Open Street Maps) or historic (Ohsome).
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
                                 interactive = F,
                                 historic = FALSE,
                                 date = NULL) {
  
#Check if we already have this data downloaded
pre_download_check_result <- pre_download_check(city_name = city_name,
                                                object = "municipal_border")
if(is.null(pre_download_check_result)){ #if there is no file already, run the code.  
  
  
  #Administrative borders for illustration purposes
  message("Querying administrative boundary of the municipality")
  
  #Historic or current border?
  if(historic){  #Historic Ohsome data
    
          #bbox
          message("Querying historic Municipal bbox")
          city_bbox <- get_municipal_bbox(city_name)
          city_bbox <- st_simplify(city_bbox, dTolerance = 0.1, preserveTopology = TRUE)
          city_bbox <- st_make_valid(city_bbox)
          Sys.sleep(1)
          
          #API query
          query <- ohsome_elements_geometry(
            boundary = city_bbox, 
            filter = "boundary=administrative and admin_level=8", 
            time = date,
            properties = "tags", 
            clipGeometry = T)
          
          #Send request
          res <- ohsome_post(query) 
          
          message("Received historic municipal border. Making geometry file...")
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
                d <- adist(tolower(city), tolower(res$name[element]))
                element <- element[which.min(d)]
              }
            }
    
    
          #Select the row with the correct geometry
          geometry <- res[element,]
          admin_border <- geometry$geometry
          admin_border <- st_sf(geometry = admin_border)
          
  }else{ 
    
          #Current OSM data
          message("Getting bbox")
          city_bbox <- getbb(city_name)
          message("Sending request for OSM border")
          # admin_border <- opq(bbox = city_bbox) %>%
          #   add_osm_feature(key = "boundary", value = "administrative") %>%
          #   add_osm_feature(key = "admin_level", value = "8") %>%
          #   osmdata_sf(quiet = F)
          
          #Query
          query <- opq(bbox = city_bbox) %>%
            add_osm_feature(key = "boundary", value = "administrative") %>%
            add_osm_feature(key = "admin_level", value = "8")
          
          # Send the request robustly
          message("Sending request for OSM border")
          admin_border <- robust_api_request(
            osm_function = osmdata_sf,  # function to call
            q = query,                   # the opq query object
            quiet = FALSE                # optional argument to osmdata_sf
          )
          
          
          message("Received OSM borders")
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
              d <- adist(tolower(city), tolower(admin_border$name[element]))
              element <- element[which.min(d)]
            }
          }
          message(admin_border$name[element])
          
          
          geometry <- admin_border[element,]
          admin_border <- st_geometry(geometry) #Just the geometry
    
  }
  
  #save the result
  saveRDS(admin_border,
              file = paste0("MUST_downloaded_data/",city_name,"/municipal_border"))
  
  message("Requested border has been returned and saved")
  Sys.sleep(1)
  return(admin_border)
  
}
else
{
  return(pre_download_check_result)
  }
  
}
