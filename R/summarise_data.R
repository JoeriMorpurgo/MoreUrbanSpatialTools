#' Summarise data that was processed via process_data()
#'
#' This function is a wrapper around several functions in the MUST package. 
#' The intended use is to provide standard the data processing part.
#' 
#' NOTE: 
#' Literature: 
#' @param VIstack OPTIONAL spatRaster stack. Stack that is to be used for the calculations
#' @param city_name character value. Name of a city and country to retrieve data for. Format should be "city, country". 
#' @param VI Character value(s). Name of the VI(s) to use during processing. Can be one or more of the following c("NDVI","EVI","MSAVI"), defaults to all three.
#' @param scenarios Integer(s). Available options range from -2 - 2. Defaults to 0, reflecting base approximations for NA values.
#' @param VIt_min Numeric or character value. Providing a numeric value sets the minimum threshold value for estimating vegetation coverage. For character, the following are possible c("w_youden","youden","95sens"), per which calculates and uses the median value across the VIt estimated for the city, vegetation indicator and border used in the VIt estimation in the process_data() function
#' @param processBorder Either or both c("u","m"). Relate to the border used for analysis in process_data and the used VIt_min.
#' @param mask spatVector. Masks the data by the provided spatVector to further restrict the analysis.
#' @param TotalVegetationCoverage logical. TRUE (default) to estimate Total vegetation coverage for the stack.
#' @param connectivityAnalysis logical. TRUE (default) to assess connectivity of Largest Patch Index and Aggregation Index across the borders at >30%, 50% and 70% of vegetation coverage.
#' @param deltaStatistics logical. TRUE (default) to assess the magnitude changes of VI in relationship to the urban border. Creates Moran's I table and pixel counts of delta per magnitude bin (per 0.1) per distance bin (per 0.1; where 0 is urban border and 1 is urban core) 
#' @export
#' @returns placeholder
#' @examples
#' #PLACEHOLDER()


summarise_data <- function(VIstack = NULL,
                           city_name,
                           VI = c("NDVI", "EVI", "MSAVI"),
                           scenarios = 0,
                           VIt_min = "w_youden",
                           processBorder = c("u","m"),
                           mask = NULL,
                           TotalVegetationCoverage = T,
                           connectivityAnalysis = T,
                           deltaStatistics = T){
  

##############################
### Pull the relevant data ###
##############################
dataVIt <- data.table::fread(paste0("./MUST_downloaded_data/",city_name,"/result/thresholds/data_VIt.csv"))  

#list all the stacks
if(is.null(VIstack)){ #if user does not provide a data stack then search for it.
  stacks <- list()
  for(q in VI){
    for(scenario in scenarios){
      for(border in processBorder){
      
        #adjust to correct name
        if(scenario == 0){scenario <- "base"}
        subStack <- terra::rast(paste0("MUST_downloaded_data/",city_name,"/result/scenarios/",
                                q,"_",border,"_",scenario,".tiff"))
        
        #VI to fcv
        if(is.numeric(VIt_min)){ #user provides numeric VIt min
          subStack <- MUST::VItoDensity(VI=subStack, VI_min = VIt_min)
          subStack[is.na(subStack)] <- 0 #stack now represents 0-100$ fcv for all cells.
        }else{ #user did not provide a number and so requests a dynamic approach, which could be 1 of 3.
          
          if(VIt_min == "w_youden"){subStackVIt <- stats::median(dataVIt$threshold_wy[dataVIt$VI==q & dataVIt$border==border], na.rm = T)}
          if(VIt_min == "youden"){subStackVIt <- stats::median(dataVIt$threshold_y[dataVIt$VI==q & dataVIt$border==border], na.rm = T)}
          if(VIt_min == "95sens"){subStackVIt <- stats::median(dataVIt$threshold_95[dataVIt$VI==q & dataVIt$border==border], na.rm = T)}
          
          subStack <- MUST::VItoDensity(VI=subStack, VI_min = round(subStackVIt, digits = 7))
          subStack[is.na(subStack)] <- 0 #stack now represents 0-100$ fcv for all cells.
          }
        
        #mask
        for(year in unique(lubridate::year(terra::time(subStack)))){
        idx <- which(lubridate::year(terra::time(subStack))==year) #find layers corresponding to the year
            #select border, probably faster if first pull all yrs, instead of pulling up per stack
            if(border == "m"){
              maskBorder <- readRDS(paste0("./MUST_downloaded_data/",city_name,"/municipal_border_",year))
              maskBorder <- MUST::project_to_local_utm(maskBorder)
            }else{
              maskBorder <- MUST::get_urban_border(city_name = city_name, date = year, bufferSize = 500)
            }
            
            subStack[[idx]] <- terra::mask(subStack[[idx]], maskBorder, #dynamic border maks
                                         filename = tempfile(fileext = ".tif"),
                                         overwrite = T)
            message("Dynamic masked for ", year)
        }
        message(paste0("Finish dynamic masking for scenario ",scenario, " using VI:",q, " at border:",border))
        
        
        
        #save to list with fcv stacks
        stacks[[paste0(q,"_",border,"_",scenario)]] <- subStack
   
      }#end BORDER loop
     }#END SCENARIOS LOOP
  }#VI loop
  message("Collected all relevant spatRaster stacks")
} else { #if user provides stack 
  stacks <- list() #open list
  VIstack <- MUST::VItoDensity(VI=VIstack, VI_min = VIt_min)
  stacks[[]] <- VIstack #put in the stack
}

##############################
### Optional extra masking ###
##############################
if(is.null(mask)){}else{
  if(methods::is(mask,"SpatVector")){
    message("Using optional user-provided mask")
    stacks <- lapply(stacks, terra::mask, mask)
    }else{message("Mask is not a SpatVector")}}



message("Starting the spatio-temporal analysis")
  ########################################      
  ### GLOBAL ASSESSMENT OF URBAN GREEN ###
  ########################################
if(TotalVegetationCoverage){
  #df to save results to
  densityDf <- data.frame(tvc = numeric(),
                          month = character(),
                          VI = character(),
                          city = character(),
                          border = character(),
                          scenario = numeric())
    
    # Pull density for each stack and put it in the dataframe
    for (nm in names(stacks)) {
      
      #pull name
      r <- stacks[[nm]]
      nameParts <- stringr::str_split(nm, pattern = "_")

      #grab density
      meanDensity <- as.numeric(terra::global(r[[1:terra::nlyr(r)]], "mean", na.rm = T)[,1])
      
    
      #make df 
      densityDf <- rbind(densityDf, data.frame(tvc = meanDensity,
                                               month = terra::time(r),
                                               VI = nameParts[[1]][1],
                                               city = city_name,
                                               border = nameParts[[1]][2],
                                               scenario = nameParts[[1]][3]))
    }
  
  #write to disk and append later
  dir.create(paste0("./MUST_downloaded_data/",city_name,"/result/tvc/"),
             showWarnings = F, recursive = T) #to make the directory
  data.table::fwrite(densityDf, file = paste0("./MUST_downloaded_data/",city_name,"/result/tvc/tvcData_",
                                      format(Sys.time(), "%d-%b-%Y %H.%M"),"_.csv"))
}

  ##########################################
  ### DYNAMIC ASSESSMENT OF CONNECTIVITY ###
  ##########################################
if(connectivityAnalysis){
  
  #params for multiprocess
  thresholds <- c(0.3, 0.5, 0.7)
  n_workers <- min(terra::nlyr(stacks[[1]]), #check the layers of the first stack
                   max(1, parallel::detectCores() - 2))
  cl <- parallel::makeCluster(n_workers)
  
  # Guaranteed cleanup of cluster and temporary disk files upon completion or exit
  on.exit({
    try(parallel::stopCluster(cl), silent = TRUE)
    gc()
  }, add = TRUE)
  
  # Explicitly load libraries inside every worker environment
  parallel::clusterEvalQ(cl, {
    library(terra)
    library(landscapemetrics)
    library(dplyr)
    library(tidyr)
  })
  
  for(nm in names(stacks)){
  
      r <- stacks[[nm]]
      # parameters
      n_layers <- terra::nlyr(r)
      time_vec <- terra::time(r)
      
      
      # Create isolated directory for individual layer files
      temp_dir <- file.path(tempdir(), paste0("rast_split_", Sys.getpid()))
      if (!dir.exists(temp_dir)) dir.create(temp_dir, recursive = TRUE)
      layer_files <- sprintf(file.path(temp_dir, "layer_%03d.tif"), seq_len(n_layers))
      for (i in seq_len(n_layers)) {
        terra::writeRaster(r[[i]], filename = layer_files[i], overwrite = TRUE)
      }
      
      # Export strictly necessary variables to workers
      parallel::clusterExport(
        cl = cl, 
        varlist = c("layer_files", "thresholds", "time_vec"), 
        envir = environment()
      )
      
      results_list <- parallel::parLapply(cl, seq_len(n_layers), function(i) {
        tryCatch({
          # Each worker opens its own dedicated file handle
          lyr <- terra::rast(layer_files[i])
          
          res_i <- lapply(thresholds, function(thresh) {
            binRast <- lyr > thresh
            
            res <- landscapemetrics::calculate_lsm(
              binRast, 
              what = c("lsm_c_lpi", "lsm_c_ai"), 
              directions = 8,
              progress = FALSE
            )
            
            if (is.null(res) || nrow(res) == 0) return(NULL)
            
            res_class1 <- res |> dplyr::filter(class == 1)
            if (nrow(res_class1) == 0) return(NULL)
            
            res_class1 |>
              dplyr::select(metric, value) |>
              tidyr::pivot_wider(names_from = metric, values_from = value) |>
              dplyr::mutate(
                layer = i,
                month = time_vec[i],
                threshold = thresh
              )
          })
          
          dplyr::bind_rows(res_i)
          
        }, error = function(e) {
          warning(sprintf("Layer %d failed: %s", i, e$message))
          return(NULL)
        })
      })
      
      # Combine results into final data frame
      connectivity_df <- dplyr::bind_rows(results_list)
      connectivity_df$city <- city_name
      nameParts <- stringr::str_split(nm, pattern = "_")
      connectivity_df$VI <- nameParts[[1]][1]
      connectivity_df$border <- nameParts[[1]][2]
      connectivity_df$scenario <- nameParts[[1]][3]
      
      #save to disk and later append all.
      dir.create(paste0("./MUST_downloaded_data/",city_name,"/result/connectivity/"),
                 showWarnings = F, recursive = T) #to make the directory
      data.table::fwrite(as.data.frame(connectivity_df), file = paste0("./MUST_downloaded_data/",city_name,"/result/connectivity/",
                                                           nameParts[[1]][1],"_",nameParts[[1]][2],"_",nameParts[[1]][3],".csv"))
  }
  
}
  ############################################
  ### Calculate structural metric annually ###
  ############################################
if(deltaStatistics){

  for(nm in names(stacks)){
  
  stack <- stacks[[nm]] #pull the stack
  
  #pull out names
  nameParts <- stringr::str_split(nm, pattern = "_")
  VI <- nameParts[[1]][1]
  border <- nameParts[[1]][2]
  scenario <- nameParts[[1]][3]
  
  #Proportion annually
  meanDensityAnn <- terra::tapp(stack, index = "years", fun = "mean")
  
  #other stats quickly
  cellsWithNumber <- as.numeric(terra::global(meanDensityAnn, fun = "notNA")[,1]) #cells with a value
  year <- terra::time(meanDensityAnn)
  
  stats <- data.frame(cellWithValues = cellsWithNumber,
                      year = year,
                      city = city_name,
                      border = border,
                      VI = VI,
                      scenario = scenario)
  
  #save result
  dir.create(path = paste0("./MUST_downloaded_data/",city_name,"/result/global_stats/"),
             showWarnings = F, recursive = T)
  data.table::fwrite(stats, file = paste0("./MUST_downloaded_data/",city_name,"/result/global_stats/",VI,"_",border,"_",scenario,".csv"))
  
  
  ### delta metrics annually ###
  #prep stack to store
  diff_stack <- terra::rast()
  for(j in c(2:terra::nlyr(meanDensityAnn))){
    #status
    print(paste0("STATUS: Computing deltas for layer ", j," and ", j-1))
    
    #define current and previous
    prev <- meanDensityAnn[[j - 1]] #take prev layer
    curr <- meanDensityAnn[[j]] #Take current layer
    
    # Calculate change
    diff <- curr - prev
    
    # Add to stacks
    diff_stack <- c(diff_stack, diff)
  }#calced a stack of deltas
  
  ###############################################
  ### MAGNITUDE OF CHANGE BY DISTANCE PER BIN ###
  ###############################################
  
  #magnitude change bins
  mag_breaks <- c(seq(-1, 0, by = 0.1), seq(0.1, 1, by = 0.1))
  diff_classed <- terra::classify(diff_stack, rcl = mag_breaks)
  
  #distance border bins
  distRast <- diff_stack[[1]]
  distRast[] <- 1
  distRast <- terra::distance(terra::mask(distRast, diff_stack[[1]]), target = 1)
  distRast <- distRast/as.numeric(terra::global(distRast, "max", na.rm = T))
  dist_breaks <- seq(0, 1, by = 0.1)
  dist_classed <- terra::classify(distRast, rcl = dist_breaks)
  
  counts_list <- list()
  for(lyr_idx in 1:terra::nlyr(diff_classed)) {
    ct <- terra::crosstab(c(dist_classed, diff_classed[[lyr_idx]]), long = TRUE)
    colnames(ct) <- c("dist_bin", "mag_bin", "count")
    ct$year_index <- lyr_idx
    counts_list[[lyr_idx]] <- ct
  }
  all_counts <- as.data.frame(data.table::rbindlist(counts_list))
  
  #################################################
  ### CLUSTERING OF MAGNITUDE OF CHANGE PER BIN ###
  #################################################
  
  # Define bin boundaries (e.g., 10 positive and 10 negative bands)
  bin_starts <- seq(0, 0.9, by = 0.1)
  autocor_results <- list()
  
  for (b in bin_starts) { #bins
    #brackets for rasters
    lower_pos <- b
    upper_pos <- b + 0.1
    
    lower_neg <- -b - 0.1
    upper_neg <- -b
    
    for (lyr in 1:terra::nlyr(diff_stack)) {
      curr_diff <- diff_stack[[lyr]] #pull layer
      
      # 1. Binary Indicator for Gains: 1 inside magnitude bin, 0 everywhere else
      gains_bin <- (curr_diff > lower_pos & curr_diff <= upper_pos) 
      # Replace NA background values with 0 to keep spatial structure intact
      gains_bin[is.na(gains_bin)] <- 0
      
      # Calculate Moran's I on the binary presence map
      gains_moran <- tryCatch({
        terra::autocor(gains_bin, method = "moran", global = TRUE)
      }, error = function(e) NA)
      
      # 2. Binary Indicator for Losses
      losses_bin <- (curr_diff >= lower_neg & curr_diff < upper_neg)
      losses_bin[is.na(losses_bin)] <- 0
      
      losses_moran <- tryCatch({
        terra::autocor(losses_bin, method = "moran", global = TRUE)
      }, error = function(e) NA)
      
      # Store results
      autocor_results[[length(autocor_results) + 1]] <- data.table::data.table(
        year_idx = lyr,
        bin_group = b,
        gains_moranI = gains_moran,
        losses_moranI = losses_moran
      )
    }
  }
  
  moran_dt <- as.data.frame(data.table::rbindlist(autocor_results))
  
  #write away
  dir.create(path = paste0("./MUST_downloaded_data/",city_name,"/result/deltaStats/"))
  data.table::fwrite(moran_dt, file = paste0("./MUST_downloaded_data/",city_name,"/result/deltaStats/moran_",VI,"_",border,"_",scenario,".csv"))
  data.table::fwrite(all_counts, file = paste0("./MUST_downloaded_data/",city_name,"/result/deltaStats/counts_",VI,"_",border,"_",scenario,".csv"))
  
  
  #gc 
  gc(verbose = FALSE)
  }
} #end delta statistics


return(message("Finished the spatio-temporal summary. Please check the result folder for the requested output"))
#####################################
### PLOT AND SUMMARISE STATISTICS ###
#####################################
  
} #end function
