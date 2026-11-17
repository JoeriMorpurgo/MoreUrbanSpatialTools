#' Retrieve current or historic municipal border limited to annual temporal resolution
#'
#' This function is retrieves the municipal border for either current time (Open Street Maps) or historic (Ohsome). The Ohsome fnction also iteratively check lower administrative levels if no data is found at level 8. This is more robust and will be present in the OSM argument at a later point.
#' NOTE: 
#' Literature: 
#' @param city_name character value. name of a city to retrieve the bbox for.
#' @param date date/time. Which point in time to retrieve data from Ohsome
#' @export
#' @examples
#' #PLACEHOLDER()
#' 


############################ get_municipal_border #################
get_municipal_border <- function(city_name,
                                 date = NULL) {

#date to year  transformation
dateYear <- lubridate::year(as.POSIXct(date))
    
#Check if we already have this data downloaded
pre_download_check_result <- pre_download_check(city_name = city_name,
                                                object = paste0("municipal_border_",dateYear))
if(is.null(pre_download_check_result)){ #if there is no file already, run the code.  
  
  

    
          #bbox
          city_bbox <- MUST::get_municipal_bbox(city_name)
          city_bbox <- sf::st_simplify(city_bbox, dTolerance = 0.1, preserveTopology = TRUE)
          city_bbox <- sf::st_make_valid(city_bbox)
          Sys.sleep(1)
          
          
                #Robust API query
                #sometimes municipal borders are on different levels so have it check them iteratively.
                for (level in c(8,7,6)) {
                  
                  ### V1 OHSOME API REQUEST ###
                  # #query itself  
                  # query <- ohsome::ohsome_elements_geometry(
                  #   boundary = city_bbox, 
                  #   filter = paste0("boundary=administrative and admin_level=",level), 
                  #   time = date,
                  #   properties = "tags", 
                  #   clipGeometry = F)
                  # 
                  #   #Send request
                  #   message("querying OHSOME for municipal borders")
                  #   res <- ohsome::ohsome_post(query, validate = T, strict = T) 
                    
                  ### V2 OHSOME API REQUEST ###
                  payload <- list(
                    filter = paste0("boundary=administrative and admin_level=", level),
                    aoi    = as.numeric(sf::st_bbox(city_bbox)),
                    time   = date,      # ISO-8601 string, e.g., "2024-01-01"
                    clip   = FALSE 
                  )
                  tmp_parquet <- tempfile(fileext = ".parquet")
                  
                  message("querying OHSOME v2 for municipal borders")
                  resp <- httr2::request("https://api.heigit.org/ohsome-api/v2/extraction/features.parquet") |>
                    httr2::req_headers(
                              "Authorization" = get_ohsome_key(), 
                              "Content-Type"  = "application/json"
                            ) |>
                    httr2::req_body_json(payload) |>
                    httr2::req_retry(max_tries = 1, max_seconds = 180) |>
                    httr2::req_perform(path = tmp_parquet)
                  
                  #pull from storage
                  res <- arrow::read_parquet(resp$body) #translate to legacy output
                  res$geometry <- sf::st_as_sfc(res$geom, crs = 4326)
                  res$geom <- NULL
                  res <- sf::st_as_sf(res)
                  
                    #check if we have a result
                    if(nrow(res)>0){break}else{cat("No result for query on administrative level ", level)}
                }
          message("Received border from query.")        
          
          
          #Code to select best fit for city.... This is a bit roundabout but OK....
          max_dist = 0.95
          element <- integer(0)
          city <- sub(",.*", "", city_name)
          element_names <- sapply(res$tags, function(t) t$value[t$key == "name"][1])
          if(city %in% element_names){element <- which(element_names %in% city)
            }else{
              while (length(element_names) == 0 && max_dist > 0) {
                max_dist <- max_dist - 0.05
                element <- agrep(tolower(city), tolower(element_names), max.distance = max_dist, value = F)
              }
              if (length(element) > 1) {
                d <- utils::adist(tolower(city), tolower(element_names[element]))
                element <- element[which.min(d)]
              }
            }
    
    
          #Select the row with the correct geometry
          geometry <- res[element,]
          admin_border <- geometry$geometry
          admin_border <- sf::st_sf(geometry = admin_border)
          
  
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
