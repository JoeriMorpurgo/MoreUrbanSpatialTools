#' Generates a classified height map based on LULC and tree points from OSM.
#'
#' NOTE: 
#' Literature: 
#' @param lulc_map sf object. Should contain classes that relate to a CUGIC height class.
#' @param alternative_lookup dataframe. A dataframe that functions as a lookup-table, where the first column contains the names of the classes of your lulc_map and the second column contains the names of the CUGIC height classes (grass, shrub or wooded). 
#' @keywords OSM, Ohsome, Land-use, Land-cover, historic
#' @export
#' @examples
#' PLACEHOLDER()
#' 

veg_structure_via_lulc <- function(lulc_map,
                                   alternative_lookup = NULL) {
  if(is.null(alternative_lookup)){
    lookup <- data.frame(
      old = lulc_info$Value,
      new = lulc_info$CUGIC.height
    )
  } else {
    lookup <- alternative_lookup
    colnames(lookup) <- c("old", "new")
  }
  
  
  lulc_map$CUGIC_height <- lookup$new[match(lulc_map$lulc, lookup$old)]
  
  heightmap <- lulc_map["CUGIC_height"]

  return(heightmap)  
}
