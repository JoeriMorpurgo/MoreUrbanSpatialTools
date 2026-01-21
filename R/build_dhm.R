#' build_dhm; build digital height model
#'
#' This function takes raster digital terrain models and digital surface models to calculate the digital height model. This can be understood as the height from the ground to a surface above it. NA values from the digital terrain model will be bilinearly interpolated based on a 3x3 grid. NOTE: this function currently only works with AHN data.
#' @param versie AHN3,AHN4 or AHN5.
#' @param blad A code that relates to a block using the coding form AHN.
#' @keywords Height model
#' @export
#' @examples
#' build_dhm()


#Function to calc DHM
build_dhm <- function(versie, blad) {
  versie_upper <- toupper(versie)
  is_ahn5 <- versie_upper == "AHN5"
  is_ahn4 <- versie_upper == "AHN4"
  is_ahn3 <- versie_upper == "AHN3"
  
  # Correcte submappen en prefixes
  dtm_subfolder <- switch(
    versie_upper,
    "AHN3" = "DTM_50cm",
    "AHN4" = "02a_DTM_0.5m",
    "AHN5" = "02a_DTM_50cm"
  )
  dsm_subfolder <- switch(
    versie_upper,
    "AHN3" = "DSM_50cm",
    "AHN4" = "03a_DSM_0.5m",
    "AHN5" = "03a_DSM_50cm"
  )
  dtm_prefix <- if (is_ahn5) "2023_M_" else "M_"
  dsm_prefix <- if (is_ahn5) "2023_R_" else "R_"
  
  # Uitvoerpad
  pad <- file.path(base_dir, versie_upper)
  out_path <- file.path(pad, "DHM_50cm")
  dir_create(out_path)
  out_file <- file.path(out_path, paste0("M_", blad, "_DHM.tif"))
  
  if (file_exists(out_file)) {
    cat("DHM bestaat al:", out_file, "\n")
    return()
  }
  
  # Zoek .tif-bestanden binnen de juiste mappen (recurse = TRUE)
  dtm_file <- dir_ls(file.path(pad, dtm_subfolder), glob = paste0("*", dtm_prefix, blad, ".TIF"), recurse = TRUE)
  dsm_file <- dir_ls(file.path(pad, dsm_subfolder), glob = paste0("*", dsm_prefix, blad, ".TIF"), recurse = TRUE)
  
  if (length(dtm_file) == 0 || length(dsm_file) == 0) {
    cat("Ontbreekt (DTM of DSM):", blad, "\n")
    return(NULL)
  }
  
  cat("🔧 Verwerk:", blad, "\n")
  
  dtm <- rast(dtm_file[1])
  dsm <- rast(dsm_file[1])
  
  if (!compareGeom(dtm, dsm, stopOnError = FALSE)) {
    dsm <- resample(dsm, dtm, method = "bilinear")
  }
  
  # Interpoleer NAs in DTM
  if (anyNA(values(dtm))) {
    mask_na <- is.na(dtm)
    dtm_filled <- terra::focal(dtm, w = 3, fun = mean, na.policy = "only", na.rm = TRUE)
    dtm[mask_na] <- dtm_filled[mask_na]
  }
  
  dhm <- dsm - dtm
  writeRaster(dhm, out_file, overwrite = TRUE)
  
  cat("DHM opgeslagen:", out_file, "\n")
}