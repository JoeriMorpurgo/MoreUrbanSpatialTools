#' Vegetation structure lookup table
#'
#' Lookup table containing information on vegetation
#' structure used by the package functions.
#'
#' @format A data frame containing vegetation structure information.
#' @source None
"lookuptable_veg_struc"


#' Land use and land cover information
#'
#' A table that is designed to classify granuler LULC data into larger aggregated classes. 
#' The original data pulled from either OpenStreetMaps or Ohsome are highly granular and variable.
#' These aggregated classes feed in to much of the downstream analysis.
#' 
#' @format A data frame with 54 rows and 7 variables. Simplified CUGIC classes; Cugic number; CUGIC classes; CUGIC height; infrastructure class):
#' \describe{
#'   \item{Key}{Key relating to OpenStreetMaps data infrastructure}
#'   \item{Value}{Value relating to OpenStreetMaps data infrastructure}
#'   \item{Simplified_CUGIC_class}{OSM-like classes aggregated to a simplified CUGIC classification}
#'   \item{CUGIC_number}{OSM-like classes number to align with the simpligied CUGIC classification}
#'   \item{CUGIC_classes}{OSM-like classes aggregated to the CUGIC classification}
#'   \item{CUGIC.height}{OSM-like classes aggregated to height according to CUGIC classification}
#'   \item{infrastructure_class}{OSM-like classes reclassified to green/grey/blue}
#' }
#' @source https://taginfo.openstreetmap.org/ and 10.1016/j.landurbplan.2023.104726
"lulc_info"