#' Checks if files are in cache and retrieves if ther
#'
#' @param city_name folder it will look for
#' @param object file to look for
#' @keywords OSM, Ohsome, Land-use, Land-cover, historic
#' @export
#' @examples
#' PLACEHOLDER()
#' 
#' 
pre_download_check <- function(city_name, object){
  
  #check if a MUST folder has been made
  if(!("./MUST_downloaded_data" %in% list.dirs())){
    #To let user know
    print(paste0("No MUST download folder found. Will create new folder for downloads at ", getwd(),"/MUST_downloaded_data"))
    
    #create folder
    dir.create("MUST_downloaded_data")
    }
  
  #Check if the city had requests before
  if(!(city_name %in% list.dirs("MUST_downloaded_data/", full.names = F, recursive = F))){
    #let the user know
    print(paste0("No folder found for ", city_name, " making one in the ", getwd(),"/MUST_downloaded_data"))
    
    #create folder
    dir.create(paste0("MUST_downloaded_data/",city_name))
  }
  
  #Check if the object/file is already present. This means we can skip API req.
  objectPath <- list.files(paste0("MUST_downloaded_data/",city_name),
                       pattern = object, full.names = T)
  
  #If the object is not found
  if(length(objectPath) == 0){
    print(paste0("The ", object, " is not found. Will start data request."))
    return(NULL)
  } 
  
  #If there are multiple objects found
  if(length(objectPath) > 1){
    print("Multiple objects found. Trying to return object.")
    if("tif" %in% file_ext(objectPath)){retrievedObject <- rast(objectPath[grepl("\\.tif$", objectPath)])}
    if("gpkg" %in% file_ext(objectPath)){retrievedObject <- vect(objectPath)}
    if("" %in% file_ext(objectPath)){retrievedObject <- readRDS(objectPath)}
    
    return(retrievedObject)
  }
  
  #Found the object
  if(length(objectPath) == 1){
    print("Object has been downloaded earlier. Retrieving from the folder.")
    
    if(file_ext(objectPath) == "tif"){retrievedObject <- rast(objectPath)}
    if(file_ext(objectPath) == "gpkg"){retrievedObject <- vect(objectPath)}
    if(file_ext(objectPath) == ""){retrievedObject <- readRDS(objectPath)}
    
    return(retrievedObject)
  }
  
  
}
