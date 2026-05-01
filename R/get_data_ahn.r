#' Download AHN files
#'
#' This function downloads both DTM and DSM of Actueel Hoogte Bestand Nederland (AHN) version 3-5 to make a digital height model.
#' NOTE: 
#' Literature: 
#' @param date Character value; dd-mm-yyyy
#' @param city_name Character value; city and country ideally. e.g. "Alkmaar, the Netherlands"
#' @keywords download, request, netherlands, height
#' @export
#' @examples
#' download_ahn_file()
#' 

#Function to download
get_data_ahn <- function(date, city_name) {

  options(timeout = max(900, getOption("timeout")))
  
  #year to version
  year <- str_extract(date, "\\d{4}")
  if(year %in% c(2024:2026)){version <- "AHN5"}
  if(year %in% c(2020:2023)){version <- "AHN4"}
  if(year %in% c(2014:2019)){version <- "AHN3"}
  
#Check if we already have this data downloaded
pre_download_check_result <- pre_download_check(city_name = city_name,
                                                object = paste0(version,"_dhm"))
if(is.null(pre_download_check_result)){ #if there is no file already, run the code.
  
  #get names of the squares to download
  ahn_sheets <- ahn_sheets_info(AHN = "AHN4", dem = "DSM", resolution = "0.5") #shouldn't matter to much as names don't change
  bbox <- get_municipal_bbox(city_name)
  bbox <- sf::st_transform(bbox, st_crs(ahn_sheets))
  sheets <- ahn_sheets[st_intersects(ahn_sheets, bbox, sparse = FALSE), ]
  kaartbladen  <- sheets$kaartbladNr  #what we need to download.
  

  for(is_dsm in c(T, F)){ #run code for DSM and DTM
    
    # Prefix R_ for DSM, M_ for DTM
    prefix <- if (is_dsm) "R_" else "M_"  
      
    # Subfolder (path on server)
    subfolder <- switch(
      version,
      "AHN3" = if (is_dsm) "DSM_50cm" else "DTM_50cm",
      "AHN4" = if (is_dsm) "03a_DSM_0.5m" else "02a_DTM_0.5m",
      "AHN5" = if (is_dsm) "03a_DSM_50cm" else "02a_DTM_50cm"
    )
    
    
    #get names of the squares to download
    ahn_sheets <- ahn_sheets_info("AHN4", dem = "DSM", resolution = "0.5")
    bbox <- get_municipal_bbox(city_name)
    bbox <- sf::st_transform(bbox, st_crs(ahn_sheets))
    sheets <- ahn_sheets[st_intersects(ahn_sheets, bbox, sparse = FALSE), ]
    kaartbladen  <- sheets$kaartbladNr  
    
    for (blad in kaartbladen) {
      
    
      # Bestandstype & naam
      filename <- if (version == "AHN5") {
          sprintf("2023_%s%s.TIF", prefix, blad)
        } else {
          sprintf("%s%s.zip", prefix, blad)
      }
      
      # Volledige URL
      if(version == "AHN4"){url <- sprintf("https://fsn1.your-objectstorage.com/hwh-ahn/%s/%s/%s", tolower(version), subfolder, filename)}
      if(version == "AHN3"){url <- sprintf("https://fsn1.your-objectstorage.com/hwh-ahn/%s/%s/%s", toupper(version), subfolder, filename)}
      if(version == "AHN5"){url <- sprintf("https://fsn1.your-objectstorage.com/hwh-ahn/%s/%s/%s", toupper(version), subfolder, filename)}
      
      # Output directory
      outdir <- file.path(paste0("MUST_downloaded_data/",city_name,"/AHN"))
      dir_create(outdir)
      
      # Bestemmingspad
      destfile <- file.path(outdir, paste0(version,is_dsm,blad,".zip"))
      
      # Check of bestand of uitgepakt map al bestaat
      unzip_dir <- file.path(outdir, paste0(version,is_dsm,blad))
      if (file_exists(destfile) || (dir_exists(unzip_dir))) {
        cat("Bestaat al of al uitgepakt:", destfile, "\n")
        return()
      }
      
      # Download
      download.file(url, destfile = destfile)
      cat("Gedownload:", destfile, "\n")
      unzip(destfile, exdir = unzip_dir) #unzip
      cat("Uitgepakt naar:", unzip_dir, "\n")
      file.remove(destfile)             #remove
      cat("ZIP verwijderd:", destfile, "\n")
  
    } 

    
  }
  
  #find directories with ahn data
  directories <- list.dirs(path = paste0("MUST_downloaded_data/",city_name,"/AHN/"))
  dtm_dirs <- grep(pattern = "FALSE", directories)
  dsm_dirs <- grep(pattern = "TRUE", directories)
  dtm_dirs <- directories[dtm_dirs]
  dsm_dirs <- directories[dsm_dirs]
  
  #go through them to build the dhm
  for (i in seq(length(dtm_dirs))) {
    
    bladName <- str_extract(dtm_dirs[i], "(?<=FALSE).*")
    
    #building the dhm
    dhm <- build_dhm(dtm = rast(list.files(dtm_dirs[i], full.names = T)),
                     dsm = rast(list.files(dsm_dirs[i], full.names = T)))
    
    
    #masking out points that are on buildings
    dhm_masked <- mask_buildings_ohsome_robust(dhm,
                                               date = date)
    
    #write it away and retrieve later
    writeRaster(dhm_masked,
                file = paste0("MUST_downloaded_data/",city_name,"/AHN/",version,"_dhm_",bladName,".tif"),
                )
  }
  
  #retrieve all .tifs in the folder and combine them into a big Geotiff to retrieve upon request
  dhm_tot <- vrt(list.files(paste0("MUST_downloaded_data/",city_name,"/AHN/"), pattern = ".tif", full.names = T))
  
  #save the stack for later.
  terra::writeRaster(dhm_tot, file = paste0("MUST_downloaded_data/",city_name,"/",version,"_dhm.tif"))
  
  #remove all files that were used to get the dhm
  unlink(list.dirs(paste0("MUST_downloaded_data/",city_name,"/AHN/"), recursive = F),
         force = T, recursive = T) #removes dirs
  unlink(list.files(paste0("MUST_downloaded_data/",city_name,"/AHN/"), pattern = "_dhm_", full.names = T))
  
  #return the finished raster
  return(dhm_tot)
  
 }else{ return(pre_download_check_result) }
  
}





