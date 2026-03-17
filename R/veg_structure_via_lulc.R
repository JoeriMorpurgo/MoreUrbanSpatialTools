#' Generates a classified height map based on LULC and tree points from OSM.
#'
#' NOTE: 
#' Literature: 
#' @param city_name character. Name of city and country.
#' @param date character. DD-MM-YYYY for date to retrieve data from 
#' @keywords OSM, Ohsome, Land-use, Land-cover, historic
#' @export
#' @examples
#' PLACEHOLDER()
#' 

veg_structure_via_lulc <- function(lulc_map) {
  
  lookup <- data.frame(
    old = lulc_info$Value,
    new = lulc_info$CUGIC.height
  )
  
  lulc_map$CUGIC_height <- lookup$new[match(lulc_map$lulc, lookup$old)]
  
  heightmap <- lulc_map["CUGIC_height"]

  return(heightmap)  
}
