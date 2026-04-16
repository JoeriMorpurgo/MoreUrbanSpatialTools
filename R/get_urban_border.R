#' Calculate urban border
#'
#' This function retrieves the municipal border and land-uses from OpenStreetMaps. After it transforms the land-use polygons into points
#' every 100m^2. These points are put in the DBSCAN algorithm to cluster points. After all clusters in the largest 90% are considered urban.
#' Finally, a concave hull is drawn around the remaining urban points which represents an estimate of the urban area of interest (AOI)
#' NOTE: 
#' Literature: 
#' @param city_name character value. Name of a city
#' @param interactive TRUE or FALSE. Allows user to selct municipal borders through Mapview. This is not recommended, but can help with problems if this function returns borders for the wrong city.
#' @param historic TRUE or FALSE. Whether to retrieve current data from OpenStreetMaps or Ohsome for historic data
#' @param date character value. Which date/time to retrieve the snapshot from Ohsome
#' @param dbscan TRUE or FALSE. Use Density-based spatial clustering of applications with noise (DBSCAN) to find urban clusters.
#' @param eps Distance to be considered
#' @param minPts Minimum points for consideration of cluster
#' @param plot legacy. Can be set to TRUE
#' @keywords urban, border, DBSCAN
#' @export
#' @examples
#' PLACEHOLDER()
#' 

####################### get_urban_aoi #####################
get_urban_border <- function(city_name,
                              historic = F,
                              date = NULL,
                              eps = 1000, #1000m
                              minPts = 100 #Approx 50% should be built up.
                              ){

#date to year  transformation
dateYear <- year(as.POSIXct(date))  
  
#Check if we already have this data downloaded
pre_download_check_result <- pre_download_check(city_name = city_name,
                                                object = paste0("urban_border_",dateYear))
if(is.null(pre_download_check_result)){ #if there is no file already, run the code.
  
    
  ##Find the municipal border to define the AOI
  cat("Getting municipal borders")
  municipal_aoi <- get_municipal_border(city_name)
  municipal_aoi <- st_make_valid(municipal_aoi)
  Sys.sleep(1)
  
  ##Retrieve the LULC
      # if(historic){
        
        message("Creating Ohsome boundary")
        municipal_aoi_wip <- st_simplify(municipal_aoi, dTolerance = 0.1, preserveTopology = TRUE)
        municipal_aoi_wip <- st_make_valid(municipal_aoi_wip)
        aoi <- ohsome_boundary(municipal_aoi_wip)
        
        message("Requesting historic Ohsome data")
        query <- ohsome_elements_geometry(
          boundary = aoi,  
          filter = paste(
            "landuse=residential or",
            "landuse=commercial or",
            "landuse=construction or",
            "landuse=industrial or",
            "landuse=retail"
          ),
          time = date,
          properties = "tags",
          clipGeometry = TRUE)
        AOI <- ohsome_post(query) #Send request
        
        #fix receieved qeury
        AOI <- AOI[st_geometry_type(AOI) %in% c("POLYGON", "MULTIPOLYGON"), ]
        AOI <- st_cast(AOI, "MULTIPOLYGON")
        AOI <- st_make_valid(AOI)
        message("Retrieved historic Ohsome data for ", city_name)
        
      # }
      # else
      # {
      #   #Transform it to a poly bbox for OSM opq
      #   cat("Prepping polygon bbox for overpass API query")
      #   municipal_aoi_poly_bbox <- as.matrix(unclass(st_geometry(municipal_aoi))[[1]])
      #   
      #   #Request OSM data
      #   message("Requesting OSM LULC data")
      #   AOI <- opq(bbox = municipal_aoi_poly_bbox) %>%
      #     add_osm_features(features = c(
      #       "\"landuse\"~\"residential|commercial|construction|industrial|retail\""
      #     )) %>%
      #     osmdata_sf()
      #   message("Retrieved the OSM LULC data for ", city_name)
      #   
      #   #Prep the AOI
      #   AOI <- merge_osm_polygons(AOI) #Merge multipoly and poly together
      #   AOI <- st_make_valid(AOI)
      #   
      # }
  
  ##Define urban border
  #Hardcore omit bad geometry.... Shouldn've been fixed already.
  AOI <- AOI[which(st_is_valid(AOI)),]
  municipal_aoi <- municipal_aoi[which(st_is_valid(municipal_aoi)),]
  
  #Somee stuff
  AOI <- suppressWarnings(st_intersection(AOI, municipal_aoi))
  AOI <- st_make_valid(AOI)
  
  
  #DBscan 
    set.seed(0)
    message("Using DBscan to define urban border")
    AOI <- AOI[!st_is_empty(AOI),]
    
    #Estimate cell size for raster via area of shapefile
    extentInMeters <- ext(st_transform(AOI, 3857)) # grab the extent
    xMeters <- as.numeric(abs(extentInMeters[1] - extentInMeters[2])) #x in meters
    yMeters <- as.numeric(abs(extentInMeters[3] - extentInMeters[4])) #y in meters
    
    #LULC to points for DBSCAN
    templateRaster <- raster(AOI,
                             nrows = xMeters/100, ncols = yMeters/100) #1 point per 10000m2
    raster <- fasterize(AOI, templateRaster)
    points <- rasterToPoints(raster, spatial = T)
    points <- st_as_sf(points)
    points <- st_transform(points, 3857)
    coords <- as.data.frame(st_coordinates(points))
    
    #DBSCAN
    message("Assigning clusters based on density")
    clusters <- dbscan::dbscan(coords, eps = eps, #1000 meters to consider
                               minPts = minPts) #100 is fully built area, given 1pts/ha
    points$clusterdb <- clusters$cluster

    
    #filter to right clusters
    points <- points %>%
      group_by(clusterdb) %>%
      count() %>%
      filter(clusterdb != 0) #omit non-clusters 

    
    aoi_list <- list() #to save subAOIs
    for (i in seq(length(unique(points$clusterdb)))) {
      
      #Make AOI concave
      con_aoi <- st_convex_hull(points$geometry[points$clusterdb==i]
                                #, ratio = 0.75
                                )
      con_aoi <- st_transform(con_aoi, 4326)
      
      #save it to join later
      aoi_list[[i]] <- con_aoi
      rm(con_aoi)
    }
    AOI_merged <- do.call(c, aoi_list)
    AOI_merged <- st_sf(clusters = seq(1:length(AOI_merged)), geometry = AOI_merged)
    #plot(AOI_merged, main = "almere 2025 convex")
    
    AOI <- st_intersection(AOI_merged, municipal_aoi) #Remove extra area sometime generated by concave
    message("DBSCAN succesful")
  

  #save the urban_border
  saveRDS(AOI,
          file = paste0("MUST_downloaded_data/",city_name,"/urban_border_",dateYear))
  
  #Return the shapefile
  return(AOI) 
}
  else
  {
    return(pre_download_check_result)
  }

}
