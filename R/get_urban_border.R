#' Define urban border
#'
#' This function take the LULC file, creates a binary raster of urban environment through excluding green LULC. After it fill gaps between urban cell up to 1km and then define the urban border as 50% of space having a urban LULC in either (default) 500m, 300m or 1000m moving window. NOTE: 1000m will be slow, and can crash, for larger cities.
#' NOTE: 
#' Literature: 
#' @param city_name character value. Name of a city. This should correspond exactly to the name used in other functions of the package.
#' @param date character value. Which date/time to retrieve the snapshot from Ohsome
#' @param bufferSize numeric value. Defaults to 500, user input needs to be divisible by 10. This indicate meters size of the circular moving average window to define urban border.
#' @param thresholdUrban numeric proportional value. Between 0-1 (defaults to 0.5). This value indicates the threshold proportion for the moving window to be considered urban. i.e. 0.5 will classify a cell as urban when 50% of the cells from the buffer are classified as urban.
#' @param gap_bridge_distance numeric value. Defaults to 1000, user input needs to be divisible by 10. Indicating meters, that borders will grow and shrink with the intend to close gaps. The basic value of this is 1000 (1km), which relates to approx 15min walking. This makes strips of lands classified as non-urban in the city (parks, nature reserves, etc.) that span 2 km urban, but does not affect anything else.
#' @param gap_filling Logical. TRUE indicates to fill gaps within the urban border after all other operations. This may help with large parks or non-urban classified land.
#' @param LULC .tif. Map from Terra having LULC. The following LULC cats are recoginized as non-urban c("agriculture", "water", "river", "canal", "stream", "sea", "natural", "Green buffer zones").
#' @param save FALSE or path. Where to save the define urban border. Defaults to T to save border in the standard folder. F to no save. define path (character) to  place to in particular folder.
#' @keywords urban, border
#' @export
#' @examples
#' PLACEHOLDER()
#' 

####################### get_urban_aoi #####################
get_urban_border <- function(city_name = NULL,
                              date = NULL,
                              bufferSize = 500,
                              thresholdUrban = 0.5,
                              gap_bridge_distance = 1000,
                              gap_filling = T,
                              LULC = NULL, #user provided LULC
                              save = T
                              ){

#date to year  transformation
if(is.character(date)){dateYear <- lubridate::year(as.POSIXct(date))}else{dateYear <- date}
  
#Check if we already have this data downloaded
if(is.character(city_name) & is.character(date)){
pre_download_check_result <- pre_download_check(city_name = city_name,
                                                subfolder = "borders/urban",
                                                object = paste0("urban_",bufferSize,"border_",dateYear))} else (pre_download_check_result <- NULL)
if(is.null(pre_download_check_result)){ #if there is no file already, run the code.
  
  #check param input
  if(bufferSize %% 10 != 0){return(print("Buffersize needs to be divisible by 10"))}else{windowSize <- bufferSize/10+1}
  if(gap_bridge_distance %% 10 != 0){return(print("Gap_bridge_distance needs to be divisible by 10"))}else{gb_dist <- gap_bridge_distance/10+1}
    
  ##Retrieve the LULC and reproject
  if(is.null(LULC)){LULC <- terra::vect(paste0(getwd(),"/MUST_downloaded_data/",city_name,"/lulc",dateYear,".gpkg"))} #retrieve
  LULC <- project_to_local_utm(LULC)
  
  #Define all non-urban LULC and remove them.
  nonUrban <- lulc_info$Value[lulc_info$Simplified_CUGIC_class %in% c("agriculture", "water", "river", "canal", "stream", "sea", "natural", "Green buffer zones")]
  urbanLULC <- LULC[!c(LULC$lulc %in% nonUrban)]
  
  #template raster and rasterisation of the urbanLULC
  template <- terra::rast(urbanLULC)
  terra::res(template) <- 10 #this should be approx 1m
  urbanLULCrast <- terra::rasterize(urbanLULC, template,
                   field = 1,
                   background = 0)
    

  #running the moving window
  for(q in windowSize){
  
    #closing small gaps
    w_close <- focalMat(urbanLULCrast, d = gb_dist, type = "circle") #weighted focal matrix
    w_close <- ifelse(w_close > 0, 1, NA) #binary matrix
    dilated <- focal(urbanLULCrast, w = w_close, fun = "max", na.rm = T)
    closed <- focal(dilated, w = w_close, fun = "min", na.rm = T)
    
    #moving window
    urbanBorder <- terra::focal(closed, w = q, fun = "mean", expand = T)
  
    #from numerical to binary
    urbanBorder <- urbanBorder>thresholdUrban #half needs to be considered urban in a km
    urbanBorder <- urbanBorder*1 #from T/F to binary. This helps with other functions.
    urbanBorder[urbanBorder==0] <- NA #omit 0 from the rasters
  
      #for name to save object
      names(urbanBorder) <- bufferSize
      #save object
      assign(paste0("urbanBorderVect",bufferSize),
             fillHoles(as.polygons(urbanBorder)))#from raster to border vector
  }
  
  #put in 1 file
  urbanBorderVect <- vect(c(mget(ls()[grep("urbanBorderVect",ls())])))
  
  #name for saving object
  fileWindowName <- stringr::str_flatten(as.character(bufferSize))

  #save the urban_border
  if(save){
    dir.create(paste0(getwd(),"/MUST_downloaded_data/",city_name,"/borders/urban/"), showWarnings = F, recursive = T) #to make the directory
  
    terra::writeVector(urbanBorderVect, file = paste0(getwd(),"/MUST_downloaded_data/",city_name,"/borders/urban/urban_",fileWindowName,"border_",dateYear,".gpkg"),
                     overwrite = T)
  }
  if(is.character(save)){terra::writeVector(urbanBorderVect, file = path, overwrite = T)}
  
  #Return the vector
  return(urbanBorderVect) 
}
  else
  {
    return(pre_download_check_result)
  }

}
