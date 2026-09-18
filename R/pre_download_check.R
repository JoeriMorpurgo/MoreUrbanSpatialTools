#' Checks if files are in cache and retrieves if present.
#'
#' @param city_name folder to check in the MUST_downloaded_data folder.
#' @param subfolder sub-folder to look for the object
#' @param object file to retrieve
#' @keywords OSM, Ohsome, Land-use, Land-cover, historic
#' @export
#' @examples
#' PLACEHOLDER()
#' 
#' 
pre_download_check <- function(city_name,
                               object,
                               subfolder = NULL){
  
  ##check if a MUST folder has been made
  if(!("./MUST_downloaded_data" %in% list.dirs(recursive = F))){
    print(paste0("No MUST download folder found. Will create new folder for downloads at ", getwd(),"/MUST_downloaded_data"))
    dir.create("MUST_downloaded_data") #create folder if no folder
    }

  
  ##Check if the city had requests before
  city_path <- file.path("./MUST_downloaded_data/",city_name)
  if(!dir.exists(city_path)){
    print(paste0("No folder found for ", city_name, " making one at ", getwd(), "/MUST_downloaded_data/",city_name))
    dir.create(city_path)
    }
  
  #check if subfolder for subfolder exists
  target_dir <- city_path
  if(!is.null(subfolder)){
    target_dir <- file.path("./MUST_downloaded_data/",city_name,"/",subfolder)
    if(!dir.exists(target_dir)){
      print(paste0("Creating subfolder subfolder at ",target_dir))
      dir.create(target_dir)
    }
  }
  
  
  ##Check if the object/file is already present. This means we can skip API req.
  objectPath <- list.files(target_dir, pattern = object, full.names = T)
  
  
  ##If the object is not found
  if(length(objectPath) == 0){
    print(paste0("The ", object, " is not found. Will start data request."))
    return(NULL)
  } 
  
  #If there are multiple objects found
  if(length(objectPath) > 1){
    print("Multiple objects found. Trying to return object.")
    if("tif" %in% xfun::file_ext(objectPath)){retrievedObject <- terra::rast(objectPath[grepl("\\.tif$", objectPath)])}
    if("gpkg" %in% xfun::file_ext(objectPath)){retrievedObject <- terra::vect(objectPath)}
    if("" %in% xfun::file_ext(objectPath)){retrievedObject <- base::readRDS(objectPath)}
    if("nc" %in% xfun::file_ext(objectPath)){retrievedObject <- terra::rast(objectPath[grepl("\\.nc$", objectPath)])}
    
    return(retrievedObject)
  }
  
  #Found the object
  if(length(objectPath) == 1){
    print("Object has been downloaded earlier. Retrieving from the folder.")
    
    if(xfun::file_ext(objectPath) == "tif"){retrievedObject <- terra::rast(objectPath)}
    if(xfun::file_ext(objectPath) == "gpkg"){retrievedObject <- terra::vect(objectPath)}
    if(xfun::file_ext(objectPath) == ""| xfun::file_ext(objectPath) == "rds"){retrievedObject <- base::readRDS(objectPath)}
    if(xfun::file_ext(objectPath) == "nc"){retrievedObject <- terra::rast(objectPath)}
    
    return(retrievedObject)
  }
  
  
}
