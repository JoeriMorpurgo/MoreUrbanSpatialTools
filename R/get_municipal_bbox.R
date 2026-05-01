#' Retrieve municipal bbox as sf
#'
#' This function is a wrapper around getbb and returns an sf object saved as an .rds on drive.
#' NOTE: 
#' Literature: 
#' @param city_name character value. name of a city to retrieve the bbox for.
#' @export
#' @examples
#' PLACEHOLDER()
#' 

############ get_municipal_bbox #############
get_municipal_bbox <- function(city_name){ 
  
#Check if we already have this data downloaded
pre_download_check_result <- pre_download_check(city_name = city_name,
                   object = "municipal_bbox")
if(is.null(pre_download_check_result)){ #if there is no file already, run the code.
  
  #Urban defined by OSM
  print("Querying municipal bbox")
  urban_bbox_matrix <-  getbb(city_name, format_out = "matrix")
  
  # Extract corners from bbox matrix
  xmin <- urban_bbox_matrix[1, 1]
  xmax <- urban_bbox_matrix[1, 2]
  ymin <- urban_bbox_matrix[2, 1]
  ymax <- urban_bbox_matrix[2, 2]
  
  # Define corners in clockwise order (and close the polygon)
  coords <- matrix(c(
    xmin, ymin,
    xmax, ymin,
    xmax, ymax,
    xmin, ymax,
    xmin, ymin  # close the polygon
  ), ncol = 2, byrow = TRUE)
  
  # Create sf polygon
  urban_bbox <- st_sf(
    geometry = st_sfc(
      st_polygon(list(coords)),
      crs = 4326
    )
  )
  
  #saving the border
  print("Saving requested bbox to wd")
  saveRDS(urban_bbox,
              file = paste0("MUST_downloaded_data/",city_name,"/municipal_bbox.rds"))
  
  #bbox to return
  return(urban_bbox) 
  
}
else
{return(pre_download_check_result)}
}
