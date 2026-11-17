#' Requests the indexing codes of the tiles for the AHN from basisdata.nl/hwh-ahn/AUX/bladwijzer.gpkg.
#'
#' @export
#' @return Returns a sf-object with codes for the AHN tiles and the spatial geometry associated
#' @examples 
#' #PLACEHOLDER()

get_ahn_sheets_info <- function() {
    
  # Official AHN sheet index (Bladwijzer) hosted online
  gpkg_url <- "/vsicurl/https://basisdata.nl/hwh-ahn/AUX/bladwijzer.gpkg"
  
  # Read the spatial file directly over HTTP using GDAL
  sheets <- sf::st_read(gpkg_url, quiet = TRUE)
  
  # Normalize column names to lowercase for safe matching
  names(sheets) <- tolower(names(sheets))

  # Map the sheet identifier column to 'kaartbladNr' so your loop works seamlessly
  if ("blad" %in% names(sheets)) {
    sheets$kaartbladNr <- tolower(sheets$blad)
  } else if ("kaartblad" %in% names(sheets)) {
    sheets$kaartbladNr <- tolower(sheets$kaartblad)
  } else if ("bladnr" %in% names(sheets)) {
    sheets$kaartbladNr <- tolower(sheets$bladnr)
  }

  sheets <- sheets[,c("ahn","geom")]
  
return(sheets)
}