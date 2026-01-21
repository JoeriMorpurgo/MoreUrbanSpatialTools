#' Download AHN files
#'
#' This function is able to download both DTM and DSM of Actueel Hoogte Bestand Nederland (AHN) version 3-5.
#' The function also is able to unzips the downloads and removes the .zip. 
#' NOTE: 
#' Literature: 
#' @param versie A character value; being AHN3, AHN4 or AHN5
#' @param type A character value; being either DSM_50cm or DTM_50cm
#' @param blad A character value; Indicating the tile you want to download
#' @param base_url A character value; Defaults to https://fsn1.your-objectstorage.com/hwh-ahn
#' @param base_dir A character value; path to folder where downloads need to go
#' @param remove_zip TRUE or FALSE; to remove the downloaded .zip 
#' @keywords download, request, netherlands, height
#' @export
#' @examples
#' download_ahn_file()
#' 

#Function to download
download_ahn_file <- function(versie, type, blad, base_dir, base_url = "https://fsn1.your-objectstorage.com/hwh-ahn", remove_zip = TRUE) {
  
  #unify
  versie_upper <- toupper(versie)
  versie_lower <- tolower(versie)
  
  
  is_ahn5 <- versie_upper == "AHN5"
  is_ahn4 <- versie_upper == "AHN4"
  is_ahn3 <- versie_upper == "AHN3"
  is_dsm <- grepl("dsm", tolower(type))
  
  # Prefix R_ for DSM, M_ for DTM
  prefix <- if (is_dsm) "R_" else "M_"
  
  # Subfolder (path on server)
  subfolder <- switch(
    versie_upper,
    "AHN3" = if (is_dsm) "DSM_50cm" else "DTM_50cm",
    "AHN4" = if (is_dsm) "03a_DSM_0.5m" else "02a_DTM_0.5m",
    "AHN5" = if (is_dsm) "03a_DSM_50cm" else "02a_DTM_50cm"
  )
  
  # Bestandstype & naam
  filename <- if (is_ahn5) {
    sprintf("2023_%s%s.TIF", prefix, blad)
  } else {
    sprintf("%s%s.zip", prefix, blad)
  }
  
  # Volledige URL
  if(is_ahn4){url <- sprintf("https://fsn1.your-objectstorage.com/hwh-ahn/%s/%s/%s", versie_lower, subfolder, filename)}
  if(is_ahn3){url <- sprintf("https://fsn1.your-objectstorage.com/hwh-ahn/%s/%s/%s", versie_upper, subfolder, filename)}
  if(is_ahn5){url <- sprintf("https://fsn1.your-objectstorage.com/hwh-ahn/%s/%s/%s", versie_upper, subfolder, filename)}
  
  # Output directory
  outdir <- file.path(base_dir, versie_upper, subfolder)
  dir_create(outdir)
  
  # Bestemmingspad
  destfile <- file.path(outdir, filename)
  
  # Check of bestand of uitgepakt map al bestaat
  unzip_dir <- file.path(outdir, tools::file_path_sans_ext(filename))
  if (file_exists(destfile) || (!is_ahn5 && dir_exists(unzip_dir))) {
    cat("Bestaat al of al uitgepakt:", destfile, "\n")
    return()
  }
  
  # Download
  res <- tryCatch(GET(url), error = function(e) NULL)
  if ((is.null(res) || status_code(res) != 200) && is_ahn5) {
    url <- str_replace(url, "2023", "2024")
    res <- tryCatch(GET(url), error = function(e) NULL)
  }

  
  if (!is.null(res) && status_code(res) == 200) {
    writeBin(content(res, "raw"), destfile)
    cat("Gedownload:", destfile, "\n")
    
    # Alleen unzippen bij AHN3 & AHN4
    if (!is_ahn5) {
      tryCatch({
        unzip(destfile, exdir = unzip_dir)
        cat("Uitgepakt naar:", unzip_dir, "\n")
        if (remove_zip) {
          file.remove(destfile)
          cat("ZIP verwijderd:", destfile, "\n")
        }
      }, error = function(e) {
        cat("Fout bij uitpakken van:", destfile, "\n")
      })
    }
  } else {
    cat("Niet gevonden of fout:", url, "\n")
  }
}





