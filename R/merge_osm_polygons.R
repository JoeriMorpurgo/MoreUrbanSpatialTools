#' Helper function to make OSM/Ohsome output consistent
#'
#' This function takes POLYGON and MULTIPOLYGON and unifies it to polygons.
#' NOTE: 
#' Literature: 
#' @param osmdata_obj osm_output. The output from OSM API request.
#' @keywords classification, height, proportion
#' @export
#' @examples
#' merge_osm_polygons()

########### merge_osm_polygons ##################
merge_osm_polygons <- function(osmdata_obj) {
  poly <- osmdata_obj$osm_polygons
  multipoly <- osmdata_obj$osm_multipolygons
  
  # If both are missing, return NULL
  if (is.null(poly) && is.null(multipoly)) return(NULL)
  
  # If only one is present, return it
  if (is.null(poly)) return(multipoly)
  if (is.null(multipoly)) return(poly)
  
  # Find common columns to avoid bind_rows() errors
  common_cols <- intersect(names(poly), names(multipoly))
  
  poly <- poly[, common_cols]
  multipoly <- multipoly[, common_cols]
  
  # Combine both as an sf object
  dplyr::bind_rows(poly, multipoly)
}
