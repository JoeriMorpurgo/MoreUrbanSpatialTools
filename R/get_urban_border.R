#' Define urban border
#'
#' This function take the LULC file, creates a binary raster of urban environment through excluding green LULC. After it fill gaps between urban cell up to 1km and then define the urban border as 50% of space having a urban LULC in either (default) 500m, 300m or 1000m moving window. NOTE: 1000m will be slow, and can crash, for larger cities.
#' NOTE: 
#' Literature: 
#' @param city_name character value. Name of a city. This should correspond exactly to the name used in other functions of the package.
#' @param date character value. Which date/time to retrieve the snapshot from Ohsome
#' @param bufferSize numeric value. Defaults to 500. Either, or combination, of 300, 500 or 1000. These indicate meters size of the moving average window to define urban border.
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
                              bufferSize = NULL,
                              LULC = NULL, #user provided LULC
                              save = T
                              ){

#date to year  transformation
if(is.character(date)){dateYear <- lubridate::year(as.POSIXct(date))}else{dateYear <- date}
  
#Check if we already have this data downloaded
if(is.character(city_name) & is.character(date)){
pre_download_check_result <- pre_download_check(city_name = city_name,
                                                object = paste0("urban_border_",dateYear))} else (pre_download_check_result <- NULL)
if(is.null(pre_download_check_result)){ #if there is no file already, run the code.
  
    
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
    
  #moving window 50% of 1km as urban
  ##check user input
  if (is.null(bufferSize)){bufferSize <- 500}
  
  #lookup table
  input_meters <- c(300, 500, 1000)
  output_cells <- c(31,  51, 101)
  windowSize <- output_cells[match(bufferSize, input_meters)]
  
  #running the moving window
  for(q in windowSize){
  
    #closing small gaps
    w_close <- focalMat(urbanLULCrast, d = 101, type = "circle") #weighted focal matrix
    w_close <- ifelse(w_close > 0, 1, NA) #binary matrix
    dilated <- focal(urbanLULCrast, w = w_close, fun = "max", na.rm = T)
    closed <- focal(dilated, w = w_close, fun = "min", na.rm = T)
    
    #moving window
    urbanBorder <- terra::focal(closed, w = q, 
                              fun = "mean", expand = T)
  
    #from numerical to binary
    urbanBorder <- urbanBorder>0.5 #half needs to be considered urban in a km
    urbanBorder <- urbanBorder*1 #from T/F to binary. This helps with other functions.
    urbanBorder[urbanBorder==0] <- NA #omit 0 from the rasters
  
      #for name to save object
      name <- input_meters[match(q,output_cells)]
      names(urbanBorder) <- name
      #save object
      assign(paste0("urbanBorderVect",name),
             as.polygons(urbanBorder))#from raster to border vector
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
