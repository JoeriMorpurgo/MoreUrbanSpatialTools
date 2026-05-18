#' API request for GEE data
#'
#' This function is a wrapper around the RGEE function ee_as_rast and allows the user to also switch between some basic arguments for rapid data requests.
#' 
#' NOTE: 
#' Literature: 
#' @param aoi ee.object 
#' @param start_date date. Start of the requesting images
#' @param end_date date. End of the requesting images
#' @param sattelite character value. Sentinel or Landsat
#' @param method Character value; NDVI, EVI or LST
#' @param indicator character value; Median or Max
#' @param temporal_resolution character value; Currently only annual
#' @param CLOUD_FILTER numeric value; between 0 - 100
#' @param quiet TRUE or FALSE. 
#' @param lazy TRUE or FALSE, to send the request and not wait for it to finish
#' @param container = character value. Where to store
#' @param via character value. where to store
#' @param dsn character value. How to call the file
#' @param maxPixels numeric value. maximum pixels in your request
#' @param scale numeric value. resolution for data requested.
#' @keywords API, Remote Sensing, Indicators, GEE
#' @export
#' @examples
#' get_GEE_data()
#' 

get_GEE_data <- function(aoi,
                         start_date, end_date, 
                         sattelite = c("Sentinel","Landsat"), #sentinel/landsat
                         method = c("NDVI", "EVI", "LST"),
                         indicator = c("median", "max"),
                         temporal_resolution = c("none", "annual", "monthly"),
                         CLOUD_FILTER = 60,
                         quiet = T, lazy = T,
                         container = NULL, via = NULL,
                         dsn = NULL, #Base value,
                         maxPixels = 1e+10, #Base value
                         scale = 1000) {#Base value
  
  #Set arguments
  indicator <- match.arg(indicator)
  method <- match.arg(method)
  temporal_resolution <- match.arg(temporal_resolution)
  
  # Default dsn if not provided
  if (is.null(dsn)) {
    dsn <- paste0(method, "_", indicator, "_", temporal_resolution, "_raster.tif")
  }
  
  #Get image collection Sentinel
  if(sattelite == "Sentinel"){
    dataCollec <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")$ #Sentinel data collection
      filterBounds(aoi)$ #with AOI
      filterDate(start_date, end_date)$ #Within that date range
      filter(ee$Filter$lte("CLOUDY_PIXEL_PERCENTAGE", CLOUD_FILTER))$ #omit pixels with too muhc cloud coverage
      map(mask_scl) #omit SCL classed pixels.
    }
  
  #Get image collection Landsat
  if(sattelite == "Landsat"){ #Retrieve landsat
    dataCollec <- ee$ImageCollection("LANDSAT/LC08/C02/T1_L2")$ #high quality datset
      filterBounds(aoi)$ #With AOI
      filterDate(start_date, end_date)$ #Within date range
      map(maskL8sr)
    }
  
  
  #indicators
  method <- switch(
    method,
    
    "NDVI" = dataCollec <- dataCollec$
      map(function(image){
          image$normalizedDifference(c("B8","B4"))$
          rename("NDVI")
      }),
    
    "EVI" = dataCollec <- dataCollec$
      map(function(image){ #For every image
        image$expression( #do this function
          expresiion = "2.5 * ((NIR - RED) / (NIR + 6 * RED - 7.5 * BLUE + 1))",
          opt_map = list( #using these bands
            NIR = dataCollec$select("B8"),
            RED = dataCollec$select("B4"),
            BLUE = dataCollec$select("B2")))$
          rename("EVI") #And call it EVI
      }),
    
    "LST" = dataCollec <- dataCollec$
      map(function(image) {
        image$select("ST_B10")$
          multiply(0.00341802)$
          add(149.0)$
          subtract(273.15)$ #from kelvin to Celsius
          rename("LST")
      })
)

  
  

    # --- Apply method ---
    rasterData <- switch(
      indicator, #depending om method argument
      "median" = dataCollec$median()$clip(aoi),
      "max"    = dataCollec$max()$clip(aoi)
    )

  #print(rasterData)
    
  # --- Export raster ---
  raster <- ee_as_rast(
    rasterData,
    dsn = dsn,
    lazy = lazy,
    maxPixels = maxPixels,
    scale = scale,
    container = container,
    via = via,
    quiet = quiet
  )
  
  return(raster)
 
}
