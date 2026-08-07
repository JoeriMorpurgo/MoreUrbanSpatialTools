#' Generates a classified height map based on LULC from OSM/OHSOME data.
#'
#' NOTE: 
#' Literature: 
#' @param lulc_map spatVector object from Terra. Should contain classes that relate to a CUGIC height class.
#' @param alternative_lookup dataframe. A dataframe that functions as a lookup-table, where the first column contains the names of the classes of your lulc_map and the second column contains the names of the CUGIC height classes (grass, shrub or wooded). 
#' @keywords OSM, Ohsome, Land-use, Land-cover, historic
#' @export
#' @examples
#' PLACEHOLDER()
#' 

veg_structure_via_lulc <- function(lulc_map,
                                   alternative_lookup = NULL) {
  if(is.null(alternative_lookup)){
    #standard LULC lookup via CUGIC method
    lookup <- data.frame(
      old = lulc_info$Value,
      new = lulc_info$CUGIC.height
    )
  } else {
    #alternative option provided by the user.
    lookup <- alternative_lookup
    colnames(lookup) <- c("old", "new")
  }
  
  #reclassify based on vegetation structure names by LULC map.
  lulc_map$CUGIC_height <- lookup$new[match(lulc_map$lulc, lookup$old)]
  
  #extract it as a seperate map.
  heightmap <- lulc_map["CUGIC_height"]

  return(heightmap)  
}
