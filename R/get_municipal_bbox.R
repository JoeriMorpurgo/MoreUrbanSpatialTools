#' Retrieve municipal bbox as sf
#'
#' This function is a wrapper around getbb and returns an sf object saved as an .rds on drive.
#' NOTE: 
#' Literature: 
#' @param city_name character value. name of a city to retrieve the bbox for.
#' @param OSM_check logical. Standard is FALSE. When true it also retrieves the bbox from OSM and merges the two bboxes. This can be helpful when the lat/long are flipped. However, the OSM API tends to be more picky in rejecting requests and is therefor this argument defaults to F.
#' @export
#' @examples
#' PLACEHOLDER()
#' 

############ get_municipal_bbox #############
get_municipal_bbox <- function(city_name, OSM_check = F){ 
  
#Check if we already have this data downloaded
pre_download_check_result <- pre_download_check(city_name = city_name,
                   object = "municipal_bbox")
if(is.null(pre_download_check_result)){ #if there is no file already, run the code.
  
  #Urban defined by OSM
  print("Querying municipal bbox")
  urban_bbox_matrix <-  osmdata::getbb(city_name, format_out = "matrix")
  
  
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
  urban_bbox <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(coords)),
      crs = 4326
    )
  )
  
  if(OSM_check == T){
  ##additional safety net and calc of bbox, which can capture flips in lat/long. Otherwise redundant and more/too specific.
      #retrieve ID number
      urban_bbox_id <- osmdata::getbb(city_name, format_out = "osm_type_id")
      numbers <- gregexpr("[0-9]+", urban_bbox_id)
      resultNum <- as.numeric(regmatches(urban_bbox_id, numbers))
      
      #request the OSM sf file
      urban_bbox_id <- osmdata::opq_osm_id(id = resultNum, type = "relation") |> osmdata::osmdata_sf()
      urban_bbox_id <- urban_bbox_id$osm_multipolygons
      
      #reduce to bbox
      urban_bbox_id <- sf::st_as_sfc(sf::st_bbox(urban_bbox_id))
  
  #merge the bboxes
  urban_bbox <- sf::st_union(urban_bbox, urban_bbox_id)
  }
  
  #saving the border
  print("Saving requested bbox to wd")
  base::saveRDS(urban_bbox,
              file = paste0("MUST_downloaded_data/",city_name,"/municipal_bbox.rds"))
  
  #bbox to return
  return(urban_bbox) 
  
}
else
{return(pre_download_check_result)}
}
