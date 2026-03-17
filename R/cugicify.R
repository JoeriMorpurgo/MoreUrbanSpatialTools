#' Make the Consolidated Urban Green Infrastructure Classification
#'
#' This function take in a ndvi map and classified height maps and combines to map the CUGIC. 
#' NOTE: 
#' Literature: CUGIC: The Consolidated Urban Green Infrastructure Classification for assessing ecosystem services and biodiversity. https://doi.org/10.1016/j.landurbplan.2023.104726
#' @param city_name character. Name of the city for the analysis
#' @param date character. dd-mm-yyyy of the analysis
#' @param veg_coverage raster. Fractional coverage by vegetation or absolute coverage by vegetation.
#' @param veg_structure spatialObject. Indicating vegetation height. Either raster with numeric values OR vector with values ("grass","shrub","wooded").
#' @param lulc vector. Contains LULC that relates to the lulc_info data table associated with the package
#' @keywords classification, height, proportion
#' @export
#' @examples
#' make_cugic()
#' 

cugicify <- function(city_name, date,
                     veg_coverage, veg_structure, lulc) {

year <- format(as.Date(date),"%Y")
  
#Check if we already have this data downloaded
pre_download_check_result <- pre_download_check(city_name = city_name,
                                                object = paste0("CUGIC",year))
if(is.null(pre_download_check_result)){ #if there is no file already, run the code.
  

#prep vegetation coverage
#back transform absolute values
  if(any(values(veg_coverage) > 1)){
  cell_area <- prod(res(veg_coverage))
  
  veg_coverage <- veg_coverage / cell_area

}

#account for some small rounding error possible.
veg_coverage[veg_coverage > 1] <- 1



#prep vegation structure
veg_structure <- project(veg_structure, crs(veg_coverage))
if(class(veg_structure) == "SpatRaster"){ #if dhm
  grass  <- veg_structure <= 1
  grass <- as.numeric(grass)
  grass <- resample(grass, veg_coverage) #asess propotional grass cov.
  shrub  <- veg_structure > 1 & veg_structure <= 5
  shrub <- as.numeric(shrub)
  shrub <- resample(shrub, veg_coverage)
  wooded <- veg_structure > 5
  wooded <- as.numeric(wooded)
  wooded <- resample(wooded, veg_coverage)
}

if(class(veg_structure) == "SpatVector"){ #if from lulc
  veg_r <- project(veg_structure, veg_coverage)
  veg_r <- rasterize(veg_r, veg_coverage, field="CUGIC_height") #might need to be flipped?
  
  #individual maps
  grass <- veg_r == "grass"
  grass <- as.numeric(grass)
  shrub <- veg_r == "shrub"
  shrub <- as.numeric(shrub)
  wooded <- veg_r == "wooded"
  wooded <- as.numeric(wooded)
}


#Classifying vegetation structure
veg_stack <- c(veg_coverage, grass, shrub, wooded)
names(veg_stack) <- c(
  "coverage",
  "grass",
  "shrub",
  "wooded"
)

#function to classify
cugic_fun <- function(cover, grass, shrub, wooded) {

  # Initialize output with 0 (No Vegetation/Other)
  out <- rep(NA, length(cover))
  
  # --- Step A: Identify Height Type ---
  g <- grass > 0.1  # < 1m
  s <- shrub > 0.1  # 1-5m
  w <- wooded > 0.1  # > 5m
  
  # Logic for Height Classes
  h_type <- rep(0, length(cover))
  h_type[!g & !s & !w] <- 0 #unclassified height
  h_type[g & !s & !w] <- 1 #grass
  h_type[!g & s & !w] <- 2 #shrub
  h_type[!g & !s & w] <- 3 #wooded
  h_type[g & s & !w]  <- 4 #grass & shrub
  h_type[g & !s & w]  <- 5 #grass & wooded
  h_type[!g & s & w]  <- 6 #shrub and wooded
  h_type[g & s & w]   <- 7 #All
  
  # --- Step B: Identify Coverage Class ---
  # 10: Dense (>70%), 20: Closed (50-70%), 30: Open (10-50%), 40: Sparse (<10%)
  c_type <- rep(0, length(cover))
  c_type[cover > 0.7]                <- 40
  c_type[cover > 0.5 & cover <= 0.7]   <- 30
  c_type[cover > 0.1 & cover <= 0.5]   <- 20
  c_type[cover <= 0.1]               <- 10
  
  # --- Step C: Combine into unique IDs ---
  # Example: 11 = Dense Grassland, 43 = Sparse Forest
  # Valid only if a coverage class 
  valid <- !is.na(cover) & cover > 0
  out[valid] <- c_type[valid] + h_type[valid]
  
  return(out)
  
}

#run the function
cugic <- lapp(veg_stack, fun = cugic_fun)
levels(cugic) <- lookuptable_veg_struc
names(cugic) <- "veg_struc_"

#prep lulc 
lookup <- data.frame(
  old = lulc_info$Value,
  new = lulc_info$CUGIC.classes
)

lulc$CUGIC_classes <- lookup$new[match(lulc$lulc, lookup$old)]

cugic_class_vect <- lulc["CUGIC_classes"]
cugic_class_vect <- project(cugic_class_vect, veg_coverage)
cugic_class_raster <- rasterize(cugic_class_vect, veg_coverage, field="CUGIC_classes")

cugic <- c(cugic, cugic_class_raster)
names(cugic) <- paste0(names(cugic), year)

#save the raster
writeRaster(cugic, filename = paste0("MUST_downloaded_data/",city_name,"/CUGIC",year,".tif"))

    return(cugic) #return to the environment
  }
else{return(pre_download_check_result)}
}
