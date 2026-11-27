#' Processes data that was retrieved via collect_data()
#'
#' This function is a wrapper around several functions in the MUST package. 
#' The intended use is to provide standard the data processing part.
#' 
#' NOTE: 
#' Literature: 
#' @param city_name character value. name of a city and country to retrieve data for. Format should be "city, country". 
#' @param VI Character value(s). Name of the VI(s) to use during processing. Can be one or more of the following c("NDVI","EVI","MSAVI"), defaults to all three.
#' @param VI_interval Character value. Either c("monthly","aggregated","none") to indicate the temporal aggregation of the VI data. Defaults to finding the most abundant monthly or aggregated files.
#' @param VI_method Character value. Name of the method used for temporally aggregating. Defaults to most abundant files present. Accepted values are c("max","median","90th","none").
#' @param VIt_min Character or numeric value. Numeric indicates a constant for the VI in the order of the VI parameter. Alternatively, provide a character value such as "dynamic" for estimation of VIt.
#' @param VIt_range Numeric vector. Vector numbers relating the growing season which are used to estimate the Vegetation Index Thresholds (VIt). For example c(5,6,7,8,9) or 5:9. Not applicable for annually aggregated data.
#' @export
#' @returns placeholder
#' @examples
#' #PLACEHOLDER()
#' 

process_data <- function(city_name,
                         VI = c("NDVI", "EVI", "MSAVI"),
                         VI_interval = NULL,
                         VI_method = NULL,
                         VIt_min = "w_youden",
                         VIt_range = NULL){
  
  ##################
  ### Data check ###
  ##################
  
    # Check basic files
    municipal_files <- list.files(path = paste0("./MUST_downloaded_data/",city_name), 
                                  pattern = "municipal_border_", full.names = T)
    lulc_files <- list.files(path = paste0("./MUST_downloaded_data/",city_name), 
                             pattern = "lulc", full.names = T)
  
    # Check VI files
    for(VIsearch in VI){
      if(is.null(VI_interval)){
        #Status
        message("No VI_interval set by user. Will identify most abundant temporal aggregation.")
        
        #find files
        VI_files <- list.files(path = paste0("./MUST_downloaded_data/",city_name,"/",VIsearch),
                               full.names = T)
        VI_files <- VI_files[stringr::str_detect(VI_files, pattern = "_raw", negate = T)] #remove raw files
        
        #Identify temporal interval (max,median,90th; monthly(monthly_max,monthly_median,monthly_90th); or none)
        monthly_VI_files <- VI_files[stringr::str_detect(VI_files, pattern = "monthly")]
        VI_files <- VI_files[stringr::str_detect(VI_files, pattern = "monthly", negate = T)] #omit monthly from the list
        
        #identify aggregation
        VI_files_aggre <- VI_files[grepl(c("max|median|90th"), VI_files)]
        VI_files <- VI_files[!VI_files_aggre] #remaining are none-aggregated files
        
        #compare for which most files are present
        vec_list <- list( #favours the first of the list.
          monthly = monthly_VI_files,
          aggre = VI_files_aggre,
          none = VI_files
        )
        max_idx <-which.max(length(vec_list))
        largest_vec <- vec_list[[max_idx]]
        
        #Status message
        message(paste0("Found ", length(monthly_VI_files), " monthly files and ",
                       length(VI_files_aggre), " aggregated files and ", 
                       length(VI_files), " unaggregated files for ", VIsearch))
        message(paste0("--- Using ", names(vec_list)[max_idx], " files for ", VIsearch, " in processing and analysis ---"))
        VI_interval_guess <- names(vec_list)[max_idx] #for later
        
                      #fail-safe if no files are found
                      if(length(largest_vec)==0){
                        message(paste0("Did not find files for the combination of VI: ",
                                       VIsearch, " and VI_interval: ", VI_interval))
                        break #stop the loop for this VI
                      }
        
        #assign file names to an object
        assign(paste0(VIsearch,"_files"),
               largest_vec)
        
        #clean objects
        rm(largest_vec, max_idx, vec_list, VI_files, VI_files_aggre, monthly_VI_files)
        
      }else{ #user wants files used based on these options: c("monthly","aggregated","none")
        
        # retrieve all VI files
        VI_files <- list.files(path = paste0("./MUST_downloaded_data/",city_name,"/",VIsearch),
                                     full.names = T)
        VI_files <- VI_files[stringr::str_detect(VI_files, pattern = "_raw", negate = T)] #remove raw files
        
          ##Collect right files for each method
          ### Monthly files ###
          if(VI_interval == "monthly"){
            
            #pull monthly files
            VI_files <- VI_files[stringr::str_detect(VI_files, pattern = "monthly")]
            
                      #fail-safe
                      if(length(VI_files)==0){
                        message(paste0("Did not find files for the combination of VI: ",
                                       VIsearch, " and VI_interval: ", VI_interval))
                        break #stop the loop for this VI
                      }
            #assign
            assign(paste0(VIsearch,"_files"),
                    VI_files )}
        
          ### Aggregated files ###
          if(VI_interval == "aggregated"){
            #identify if aggregated
            VI_files <- VI_files[stringr::str_detect(VI_files, pattern = "monthly", negate = T)] #remove monthly
            VI_files <- VI_files[grepl(c("max|median|90th"), VI_files)] #pull aggregated files
                    
                    #fail-safe
                    if(length(VI_files)==0){
                      message(paste0("Did not find files for the combination of VI: ",
                                     VIsearch, " and VI_interval: ", VI_interval))
                      break #stop the loop for this VI
                    }
            
            #assign
            assign(paste0(VIsearch,"_files"),
                   VI_files)
          }
        
          ### un-aggregated files ###
          if(VI_interval == "none"){
            
            VI_files <- VI_files[!grepl(c("max|median|90th"), VI_files)]
            
                    #fail-safe
                    if(length(VI_files)==0){
                      message(paste0("Did not find files for the combination of VI: ",
                                     VIsearch, " and VI_interval: ", VI_interval))
                      break #stop the loop for this VI
                    }
            
            assign(paste0(VIsearch,"_files"),
                   VI_files) #all files with any aggregation method
          }
      } # end block finding temporal aggregation interval
      
      
      #Pick user-assigned method OR most common aggregation method as files to use in processing and analysis
      if(is.null(VI_method)){
        
        #status
        message("No VI_method set by user. Will identify most abundant VI_method.")
        #pull vector from previous block.
        VIsearch_files <- get(paste0(VIsearch,"_files"))
        
        #identify method of aggregation
        maxCount <- stringr::str_detect(VIsearch_files, pattern = "max")
        medianCount <- stringr::str_detect(VIsearch_files, pattern = "median")
        `90thCount` <- stringr::str_detect(VIsearch_files, pattern = "90th")
        
        #compare for which most files are present
        vec_list <- list( #favours the first of the list.
          median = sum(medianCount),
          max = sum(maxCount),
          `90th` = sum(`90thCount`)
        )
        max_idx <-which.max(length(vec_list))
        largest_vec <- vec_list[[max_idx]]
        
        #Status message
        message(paste0("Found ", sum(medianCount), " median files and ",
                       sum(maxCount), " max files and ", 
                       sum(`90thCount`), " 90th percentile files for ", VIsearch))
        message(paste0("--- Using ", names(vec_list)[max_idx], " files for ", VIsearch, " in processing and analysis ---"))
        VI_method_guess <- names(vec_list)[max_idx]
        VIsearch_files <- VIsearch_files[stringr::str_detect(VIsearch_files, pattern = names(vec_list)[max_idx])]
        
                #fail-safe
                if(length(VIsearch_files)==0){
                  message(paste0("Did not find files for the combination of VI: ",
                                 VIsearch, ", VI_interval: ", VI_interval, " and VI_method: ", VI_method))
                  break #stop the loop for this VI
                }
        
        assign(paste0(VIsearch,"_files"),
               VIsearch_files)
        
        #clean objects
        rm(maxCount, medianCount, vec_list, `90thCount`, max_idx, VIsearch_files)
        
      } else { #user assigned temporal aggregation method
        #pull vector from previous block
        VIsearch_files <- get(paste0(VIsearch,"_files"))
        VIsearch_files <- stringr::str_detect(VIsearch_files, pattern = VI_method)
        
        
                #fail-safe
                if(length(VIsearch_files)==0){
                  message(paste0("Did not find files for the combination of VI: ",
                                 VIsearch, ", VI_interval: ", VI_interval, " and VI_method: ", VI_method))
                  break #stop the loop for this VI
                }
        
        assign(paste0(VIsearch,"_files"),
               VIsearch_files)
        
      }#end block for temporal aggregation method
    } #end code block for finding VI files from cache
  rm(VI_files)
      
  #identify the temporal range of the data
  filename_objects <- ls()[stringr::str_detect(ls(), pattern= "_files")]
  dataRanges <- data.frame(datatype = character(),
                           start = numeric(),
                           end = numeric())
  for(i in filename_objects){
    
    #pull the date out of the files
    filenames <- get(i) #pull names
    filenames <- basename(filenames)
    vector <- readr::parse_number(filenames)
    
    #find the data title
    dataLabel <- gsub(pattern = "_files", replacement = "", i)
    
    if(!is.numeric(min(vector)) | is.infinite(min(vector))){next} #if the range is not a number of invalid go to the next loop
    
    #add in the data
    dataRanges <- rbind(dataRanges, c(dataLabel, min(vector), max(vector))) 
    
    message(paste0(dataLabel, " data covers ",min(vector),"-",max(vector)))
  }
  
  #identify coverage of the data to process
  temporalRange <- max(as.numeric(dataRanges[,2])):min(as.numeric(dataRanges[,3]))
  
  #cleanup
  rm(dataLabel, i, vector, filenames, filename_objects)
  
  ###########################################
  ### RETRIEVE AND ALIGN SPATIAL PRODUCTS ###
  ###########################################
  for(year in temporalRange){ #pull data
  
    #get the municipal border  
    assign(paste0("mborder_", city_name,year),
           readRDS(paste0("./MUST_downloaded_data/",city_name,"/municipal_border_",year)))
    
    #get lulc
    assign(paste0("lulc_",city_name,year),
           terra::vect(paste0("./MUST_downloaded_data/",city_name,"/lulc",year,".gpkg")))

    #status
    message(paste("Estimating the urban border for", city_name, "for", year))
    
    #define urban border   
    assign(paste0("uborder_", city_name,year),
           MUST::get_urban_border(city_name = city_name,
                            date = year,
                            bufferSize = 500,
                            thresholdUrban = 0.5,
                            gap_bridge_distance = 1000,
                            gap_filling = T,
                            LULC = get(paste0("lulc_",city_name,year)), #user provided LULC
                            save = T))
         
  }
  
  ####################
  ### IDENTIFY VIt ###
  ####################
  dataVIt <- data.frame(VI = character(),
                      year = numeric(),
                      border = character(),
                      city = character(),
                      sclMasked = numeric(),
                      cells = numeric(),
                      cellsV = numeric(),
                      threshold_wy = numeric(),
                      threshold_y = numeric(),
                      threshold_95 = numeric(),
                      countGrey = numeric(),
                      countGreen = numeric(),
                      countBlue = numeric(),
                      medianVIgrey = numeric(),
                      medianVIgreen = numeric(),
                      medianVIblue = numeric())
  
  
  #check the tVI for every VI
  for (q in VI[2]){ # VI loop
    for(year in temporalRange[3:5]){
    
    #status
    message(paste("Estimating the VI threshold (VIt) for",q,"in",city_name,"for",year))
      
    #get the VI data stack
    rast <- terra::rast(get(paste0(q,"_files"))[which(year == temporalRange)])
    
    #temporal subset
    if(is.null(VI_interval)){
      if(VI_interval_guess=="monthly"|VI_interval_guess=="none"){
        #assign time to subset and then subset to summer months to limit seasonality pattern
        month_idx <- as.numeric(format(terra::time(rast), "%m")) #pull date out of the raster data and format it to numeric months
        growing_idx <- month_idx %in% VIt_range # User defined growing season
        rast <- rast[[growing_idx]] # Only include rasters within the growing season
        
        #tmeporally aggregate the data
        rastAnn <- terra::tapp(rast, index = "years", fun = "median", na.rm = T)
      }
    }else{
      if(VI_interval == "monthly"|VI_interval == "none"){ #redundant, but not sure how to solve rn....
        #assign time to subset and then subset to summer months to limit seasonality pattern
        month_idx <- as.numeric(format(terra::time(rast), "%m")) #pull date out of the raster data and format it to numeric months
        growing_idx <- month_idx %in% VIt_range # User defined growing season
        rast <- rast[[growing_idx]] # Only include rasters within the growing season
        
        #tmeporally aggregate the data
        rastAnn <- terra::tapp(rast, index = "years", fun = "median", na.rm = T)
      }
    }
      
    #get thresholds pet border type
    for(x in c("u","m")){ #border loop
      
      #mask the raster to the border of choice
      maskedRast <- terra::mask(rastAnn, MUST::project_to_local_utm(get(paste0(x,"border_",city_name,year)))) 
      sclMasked <- as.numeric(terra::global(maskedRast, "isNA")) #border + scl masked out cells
      
      #lulc
      lulc <- MUST::project_to_local_utm(get(paste0("lulc_",city_name,year))) #reproject
      lulcRast <- terra::rasterize(lulc, y = rastAnn, field = "lulc")
      lulcRast <- terra::mask(lulcRast, project_to_local_utm(get(paste0(x,"border_",city_name,year))))
      countGrey <- lulcRast$lulc %in% lulc_info$Value[lulc_info$infrastructure_class=="grey"] 
      countGreen <- lulcRast$lulc %in% lulc_info$Value[lulc_info$infrastructure_class=="green"]
      countBlue <- lulcRast$lulc %in% lulc_info$Value[lulc_info$infrastructure_class=="blue"]
      
      #scale VI and omit non vegetated cells according to threshold
      VIdens <- MUST::VItoDensity(city_name = city_name, #city
                            date = year, #year
                            VI = maskedRast, #rast to put in
                            lulc = lulc, #LULC to decide things
                            VI_min = VIt_min, #approach to scale
                            figure = paste0("./MUST_downloaded_data/",city_name,"/result/VItoDensity/VIt_",x,"_",q,"_",year),
                            save_fcv = c("95_sens","w_youden","youden"),
                            save_path = paste0("./MUST_downloaded_data/",city_name,"/result/VItoDensity/VIt_",x,"_",q,"_",year))
      
      #indicator of cells
      VIdens[VIdens == 0] <- NA
      cellNum <- terra::ncell(VIdens)
      
      
      #save outcome
      dataVIt <- dataVIt |> tibble::add_row(VI = q,
                                 year = year,
                                 border = x,
                                 city = city_name,
                                 sclMasked = sclMasked,
                                 cells = cellNum,
                                 threshold_wy = as.numeric(terra::metags(VIdens)[[1,2]]),
                                 threshold_y = as.numeric(terra::metags(VIdens)[[2,2]]),
                                 threshold_95 = as.numeric(terra::metags(VIdens)[[3,2]]),
                                 countGrey = as.numeric(terra::global(as.numeric(countGrey)==1, "sum", na.rm = T)),
                                 countGreen = as.numeric(terra::global(as.numeric(countGreen)==1, "sum", na.rm = T)),
                                 countBlue = as.numeric(terra::global(as.numeric(countBlue)==1, "sum", na.rm = T)),
                                 medianVIgrey = as.numeric(terra::metags(VIdens)[[4,2]]),
                                 medianVIgreen = as.numeric(terra::metags(VIdens)[[5,2]]),
                                 medianVIblue = as.numeric(terra::metags(VIdens)[[6,2]])
      )

      } #Border loop
    } # Year loop
  } # VI loop
  dir.create(paste0("./MUST_downloaded_data/",city_name,"/result/thresholds"), showWarnings = F, recursive = T)
  data.table::fwrite(dataVIt, file = paste0("./MUST_downloaded_data/",city_name,"/result/thresholds/data_VIt.csv"))


  
  ##############################################
  ### APPROXIMATE VI VALUES AND ERROR ASSESS ###
  ##############################################
  
  for(q in VI){ #create scenarios for the VIs
    for(border in c("u","m"))
    
    #Pull full VI stack for imputation and scenario creation
    VIstack <- terra::rast(get(paste0(q,"_files")))
    # fill in missing timesteps
    VIstackTime <- MUST::fill_missing_layers(VIstack)
    
    for(year in temporalRange) { #dynamically mask every layer of the stack by annual masks
      
      #grab the border
      if(border == "m"){
        assign("maskBorder",
               get(paste0("mborder_",city_name,year)))
        maskBorder <- MUST::project_to_local_utm(maskBorder)
      }else{
        assign("maskBorder",
               MUST::get_urban_border(city_name = city_name, date = year, bufferSize = 500))
      }
      
      #grab the LULC to mask out water cells
      assign("lulc",
             get(paste0("lulc_",city_name,year)))
      lulc <- MUST::project_to_local_utm(lulc)
      blueLULC <-  subset(lulc, lulc$lulc %in% c(lulc_info$Value[lulc_info$infrastructure_class== "blue"]))
      
      
      #idx of the layers relevant --> this only links to annual. Need to test with monthly or none aggregated data.
      ## However, the lulc and borders are annually, so I suppose this makes sense
      idx <- which(lubridate::year(terra::time(VIstackTime))==year)
      for(layer in idx){ #mask per layer and have the calcs write to disk instead of memory
        VIstackTime[[layer]] <- terra::mask(VIstackTime[[layer]], maskBorder, #dynamic border maks
                                 filename = tempfile(fileext = ".tif"),
                                 overwrite = T)
        VIstackTime[[layer]] <- terra::mask(VIstackTime[[layer]], blueLULC, inverse = T, #dynamic water mask
                                 filename = tempfile(fileext = ".tif"),
                                 overwrite = T)
      }
      
      if(year == utils::tail(unique(lubridate::year(terra::time(VIstackTime))), n = 1)){print("Finished masking dyanmically")}
    } #end loop annual dyanmic masking
    
    
    # Here, VIstackTime reflects the VI stacks that are dynamically masked and go into the benchmarking and scenario creation
    
    # Pull the estimated VI threshold
    if(is.numeric(VIt_min)){thresholdVI <- VIt_min}else{ #take user constant or if it was dynamic, take w-youden
    thresholdVI <- stats::median(dataVIt$threshold_wy[dataVIt$city == city_name &
                                                 dataVIt$VI == q &
                                                 dataVIt$border == border], na.rm = T)}
    
    
    # Clamp stack
    VIstackCl <- terra::clamp(VIstackTime, lower = thresholdVI, upper = 1, values = F)
    
    #benchmark interpolation methods for the stack and put them in a list
    resultBenchmark <- MUST::benchmark_approximation(
      VIstackCl,
      methods = c("linear",
                  "spline",
                  "HANTS",
                  "ND"),
      prop_NA = 0.01,
      verbose = T,
      ncore = 8,
      HANTS_freq = length(unique(lubridate::year(terra::time(VIstackTime)))), #data is monthly, so 12 for 1 freq (annual hz)
      max_gap = 4,
      max_NA_prop = 0.5,
      maxVal = 1, minVal = 0
    )
    
    
    dir.create(paste0("./MUST_downloaded_data/",city_name,"/result/approximationMetrics/"),
               showWarnings = F, recursive = T)
    data.table::fwrite(resultBenchmark$metrics,
           file = paste0("./MUST_downloaded_data/",city_name,"/result/approximationMetrics/",city_name,"_",q,"_",border,".csv"))
    print(paste0("Calculated best approximation method for ",border," ", city_name, " and indicator ", q))
    
    #scenario parameters
    benchmarks <- resultBenchmark$metrics
    approxMethod <- benchmarks$Method[which.max(benchmarks$R2)]
    RMSE <- benchmarks$RMSE[benchmarks$Method == approxMethod]
    MAE <- benchmarks$MAE[benchmarks$Method == approxMethod]
    Bias <- benchmarks$Bias[benchmarks$Method == approxMethod]
    print(paste0(approxMethod, " chosen as best approximation method with an RMSE:", RMSE, " and MAE:", MAE, " and R2:", benchmarks$R2[benchmarks$Method == approxMethod]))
    
    #pull best approximated stack
    VIstackApprox <- resultBenchmark[[paste0("stack_pred_", approxMethod)]]
    resultBenchmark <- NULL #drop spatRasters from memory! IMPORTANT
    
    #create dir and write base scenario
    dir.create(paste0("./MUST_downloaded_data/",
                      city_name,"/result/scenarios/"),
               showWarnings = F, recursive = T) #to make the directory
    terra::clamp(VIstackApprox, lower = -1, upper = 1, values = T, #THIS IS BASE VI SCENARIO (NOT FCV)
                 filename = paste0("./MUST_downloaded_data/",
                                   city_name,"/result/scenarios/",q,"_",border,"_base.tiff"), overwrite = T)
    print("Saved best stack as base scenario")
    
    
        ### Alternative scenarios ###
        # Which cells were approximated (i.e. changed)?
        is_interpolated <- is.na(VIstackCl) & !is.na(VIstackApprox)
        for(scenario in c(-2, -1, 1, 2)){ #range -2 - 2x RMSE
          
          #status
          print(paste0("Creating scenario ", scenario))
          
          #adjustment
          altScenario <- terra::ifel(is_interpolated, #check if the cell was approximated
                              VIstackApprox + (RMSE * scenario) + Bias, #scen
                              VIstackApprox) #or keep same
          
          
          terra::clamp(altScenario, lower = -1, upper = 1, values = T,
                       filename = paste0("./MUST_downloaded_data/",
                                         city_name,"/result/scenarios/",q,"_",border,"_",scenario,".tiff"),
                       overwrite = T)
          rm(altScenario) #cleanup
          
        } #end scenarios loop
  }
  
  
  
}
