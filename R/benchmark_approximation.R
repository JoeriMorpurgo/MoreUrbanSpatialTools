#' Temporal remote sensing data often has NA by cloud and other types of masking. 
#' This function approximates linear, spline, HANTS and median normalized difference to asses performance of these methods.
#' It returns a small data frame with the performance of approximation with Mean Absolute Error (MAE), Root Mean Square Error (RMSE), Bias and R-squared (R2).
#' Optionally, users can get a smoothScatter for the fit of the approximated against the real values.
#'
#' NOTE: This function can run into std::bad_alloc errors. Reducing terraOptions(mem_frac) tends to help.
#' Literature: 
#' @param stack stack. Stack of rasters that need to be interpolated.
#' @param methods character vector. Names of the method to approximate NAs.Available are c("linear","spline","HANTS","ND").
#' @param HANTS_freq Integer. Frequency parameter for HANTS interpolation.  Should be the frequency of phenology.
#' @param max_gap integer defaults to 4. Number of NA consequetively allowed before returning the original values of the pixel. Applies to HANTS and spline
#' @param max_NA_prop numeric defaults to 0.5. Proportion of maximimum NAs allowed per pixel for calculcations. Applies to HANTS and spline
#' @param maxVal numeric. Maximum value allowed by spline interpolation. Values higher will become maxVal.
#' @param minVal numeric. Minimum value allowed by spline interpolation. Values lower will become minVal.
#' @param prop_NA numeric (0-1). Defaults to 0.1. Proportion of non-NA cells to artificially mask
#' @param seed integer. defaults to random, but can be set to be reproducable.
#' @param verbose logical. True to get extra status updates.
#' @param figure_path character. defaults to NULL. Set to path to save a smoothScatter figure between approximate values and true values.
#' @export
#' @examples
#' PLACEHOLDER()
#' 

############ get_municipal_bbox #############
benchmark_approximation <- function(stack,
                                    methods,
                                    HANTS_freq,
                                    max_gap = 5,
                                    max_NA_prop = 0.5,
                                    maxVal = 1,
                                    minVal = NULL,
                                    prop_NA = 0.1,
                                    seed = 43,
                                    verbose = F,
                                    ncore = 1,
                                    figure_path = NULL){
  
  #seed
  if (!is.null(seed)){set.seed(seed)}
  
  # params
  nlyrs <- nlyr(stack) 
  stack_NA <- stack #copy
  
  # Track sample positions and true values
  track_df <- list()
  
  #############################
  ### Identify valid cells  ###
  #############################
  for (i in 1:nlyrs) {
    
    #check the NA number
    NA_samples <- round(as.numeric(terra::global(!is.na(stack[[i]]), "sum", na.rm = TRUE)[1, 1])*prop_NA, digits =0)
    
    if(NA_samples<10){print(paste0("Found less than 100 cells to sample in layer ",i))}
    if(NA_samples <10){next}
    
    #retrieve random cells that have a value
    sampled_cells <- terra::spatSample(
      stack[[i]], 
      size     = NA_samples, 
      method   = "random", 
      na.rm    = TRUE, #dont take NA cells
      cells    = TRUE, #grab cell=number
      values   = TRUE  #grab actual value
    )
    
    if (length(sampled_cells) > 0) {
      
      # Record ground truth
      track_df[[i]] <- data.frame(layer = i,
                                  cell = sampled_cells[,1], 
                                  y_true = sampled_cells[,2]
                                  )
      
      if(verbose){print(paste0("STATUS: Selected NA locations for layer ",i))}
    }
  }
  
  # Combine list elements into a data frame safely
  if (length(track_df) > 0) {
    track_df <- do.call(rbind, track_df)
  } else {
    track_df <- data.frame(layer = integer(), cell = integer(), y_true = numeric())
  }
  
  #error check if anything was present at all.
  if (nrow(track_df) == 0) {
    warning("No valid cells were sampled. Check prop_NA or raster non-NA cell counts.")
    return(NULL)
  }
  
  ####################################
  ### Apply NA to identified cells ###
  ####################################
  if (nrow(track_df) > 0) {
    
    # Pre-allocate a list to hold temporary file-backed rasters
    masked_layers <- list()
    tmpdir <- terraOptions(print = F)$tempdir
    for (u in seq_len(nlyrs)) {
      if(verbose) print(paste("Applying NAs to layer", u))
      
      # 1. Isolate the single layer and extract values
      lyr <- stack[[u]]
      v <- terra::values(lyr)
      
      # 3. Apply NAs using standard R indexing (instantaneous)
      NAidx <- track_df$cell[track_df$layer == u]
      if (length(NAidx) > 0) {v[NAidx] <- NA}
      
      # 4. Put the modified values back into the layer
      terra::values(lyr) <- v
      
      # 5. Write to a temporary file on disk to completely clear it from RAM
      tmp_file <- tempfile(tmpdir = tmpdir, fileext = ".tif")
      masked_layers[[u]] <- terra::writeRaster(lyr, tmp_file, overwrite = TRUE)
      
      # 6. Force R to clean up memory before the next loop starts
      rm(lyr, v)
      gc(verbose = FALSE)
    }
    
    # 7. Rebuild the stack directly from the lightweight disk files
    stack_NA <- terra::rast(masked_layers)
    
    if (verbose) message("STATUS: Random NAs applied across stack")
  }
  
  ########################################
  ### INTERPOLATION METHODS BELOW HERE ###
  ########################################
  
    ###LINEAR INTERPOLATION###
    if("linear" %in% methods){
        if(verbose){message("Evaluating: Linear approximation")}  
      
        #predict linearly
        stack_pred_lin <- terra::approximate(stack_NA, method = "linear")
    
        # Make vector to store predictions
        track_df$y_pred_linear <- 0
        
        #retrieve predictions
        for (i in unique(track_df$layer)) {
          NAidx <- track_df$cell[track_df$layer == i]
          track_df$y_pred_linear[track_df$layer == i] <- as.numeric(stack_pred_lin[[i]][NAidx][,1])
        }
        
    }
    
    ###SPLINE INTERPOLATION###
    if("spline" %in% methods){
      if(verbose){message("Evaluating: Spline approximation")}  
      
      #spline prediction
      stack_pred_spline <- terra::app(stack_NA, fun = spline_pixel,
                                      max_gap = max_gap, max_NA_prop = max_NA_prop,
                                      maxVal = 1, minVal = minVal,
                                      cores = ncore)
      
      # Make vector to store predictions
      track_df$y_pred_spline <- 0
      
      #retrieve predictions
      for (i in unique(track_df$layer)) {
        NAidx <- track_df$cell[track_df$layer == i]
        track_df$y_pred_spline[track_df$layer == i] <- as.numeric(stack_pred_spline[[i]][NAidx][,1])
      }
    }
  
    ###HANTS INTERPOLATION###
    if("HANTS" %in% methods){
      if(verbose){message("Evaluating: HANTS approximation")}  
      
      #HANTS
      stack_pred_HANTS <- HANTS_stack(stack_NA, cores = 8,
                                      freq = HANTS_freq, max_iter = 5, tolerance = 0.1,
                                      max_gap = max_gap, max_NA_prop = max_NA_prop)
      
      # Make vector to store predictions
      track_df$y_pred_HANTS <- 0
      
      #retrieve predictions
      for (i in unique(track_df$layer)) {
        NAidx <- track_df$cell[track_df$layer == i]
        track_df$y_pred_HANTS[track_df$layer == i] <- as.numeric(stack_pred_HANTS[[i]][NAidx][,1])
      }
    }
  
    ###SPATIAL INTERPOLATION
    if("ND" %in% methods){
      if(verbose){message("Evaluating: Normalized Difference approximation")}  
      
      #spat interpolation
      stack_pred_ND <- temporal_spatial_approximation(stack_NA)
      
      # Extract predicted values at the sampled cell/layer positions
      track_df$y_pred_ND <- 0
      
      #retrieve predictions
      for (i in unique(track_df$layer)) {
        NAidx <- track_df$cell[track_df$layer == i]
        track_df$y_pred_ND[track_df$layer == i] <- as.numeric(stack_pred_ND[[i]][NAidx][,1])
      }
    }
    
  #########################################
  ### INTERPOLATION ACCURACY ASSESSMENT ###
  #########################################
  results<- list()
  for (method in methods) {
    
    #subset column based on method
    ApproxColNm <- paste0("y_pred_",method)
    
    # Filter valid pairs
    valid <- !is.na(track_df[[ApproxColNm]]) & !is.na(track_df$y_true)
    yt <- track_df$y_true[valid]
    yp <- track_df[[ApproxColNm]][valid]
    
    if (length(yt) == 0) {
      mae <- NA; rmse <- NA; bias <- NA; r2 <- NA
      q_2.5  <- NA; q_16   <- NA; q_84   <- NA; q_97.5 <- NA
    } else {
      #error
      err  <- yp - yt
      mae  <- mean(abs(err))
      rmse <- sqrt(mean(err^2))
      bias <- mean(err) # Over/under-estimation
      r2   <- suppressWarnings(cor(yp, yt, use = "complete.obs")^2)
      
      #quantile
      q_err <- quantile(err, probs = c(0.025, 0.16, 0.84, 0.975), na.rm = TRUE)
      q_2.5  <- q_err["2.5%"]
      q_16   <- q_err["16%"]
      q_84   <- q_err["84%"]
      q_97.5 <- q_err["97.5%"]
    }
    
    results[[method]] <- data.frame(
      Method       = method,
      MAE          = round(mae, 4),
      RMSE         = round(rmse, 4),
      Bias         = round(bias, 4),
      R2           = round(r2, 4),
      Q2.5         = round(q_2.5, 4),  
      Q16          = round(q_16, 4),   
      Q84          = round(q_84, 4),   
      Q97.5        = round(q_97.5, 4), 
      Unfilled_NAs = sum(is.na(track_df[[ApproxColNm]]))
    )
  }
    
  #combine results
  results <- do.call(rbind, results)
  
  #######################
  ### ENSEMBLE METHOD ###
  #######################
  
  #sapply methods to assign weights
  weights <- sapply(methods, function(m) {
    idx <- which(results$Method == m)
    # Give a weight of 0 if method failed entirely
    if (length(idx) == 0 || is.na(results$RMSE[idx]) || results$RMSE[idx] == 0) return(0)
    
    (1 / results$RMSE[idx]) * (1 / results$MAE[idx]) * results$R2[idx]
  })
  
  #matrices
  val_mat <- as.matrix(track_df[, c(paste0("y_pred_",methods))])
  
  # 2. Create weight matrix matching NAs in the data
  weight_mat <- matrix(weights, nrow = nrow(val_mat), ncol = length(weights), byrow = TRUE)
  weight_mat[is.na(val_mat)] <- 0
  
  # 3. Replace NAs with 0 in data matrix for clean matrix multiplication
  val_mat_clean <- val_mat
  val_mat_clean[is.na(val_mat)] <- 0
  
  # 4. Compute weighted sum / sum of weights per row
  numerator <- rowSums(val_mat_clean * weight_mat)
  denominator <- rowSums(weight_mat)
  
  # 5. Assign result (returns NA if all 4 methods are NA for a row)
  track_df$y_pred_ensemble <- ifelse(denominator > 0, numerator / denominator, NA)

  # check performance of the ensemble method
  valid_ens <- !is.na(track_df$y_pred_ensemble) & !is.na(track_df$y_true)
  err_ens <- track_df$y_pred_ensemble[valid_ens] - track_df$y_true[valid_ens]
  q_ens   <- quantile(err_ens, probs = c(0.025, 0.16, 0.84, 0.975), na.rm = TRUE)
  results <- results %>% dplyr::add_row(Method = "ensemble",
                                 MAE = round(mean(abs(err_ens)),4),
                                 RMSE = round(sqrt(mean(err_ens^2)),4),
                                 Bias = round(mean(err_ens),4),
                                 Q2.5 = round(q_ens["2.5%"], 4),
                                 Q16 = round(q_ens["16%"], 4),
                                 Q84= round(q_ens["84%"], 4),
                                 Q97.5 = round(q_ens["97.5%"], 4),
                                 R2 = round(suppressWarnings(cor(track_df$y_pred_ensemble, track_df$y_true, use = "complete.obs")^2),4)
                                 )
  row.names(results)[nrow(results)] <- "ensemble" #fix this rowname.
  ######################################
  ### CREATION OF THE ENSEMBLE STACK ###
  ######################################
  # 1. Collect all generated method stacks into a named list
  method_stacks <- list()
  if ("linear" %in% methods && exists("stack_pred_lin"))    method_stacks[["linear"]] <- stack_pred_lin
  if ("spline" %in% methods && exists("stack_pred_spline")) method_stacks[["spline"]] <- stack_pred_spline
  if ("HANTS"  %in% methods && exists("stack_pred_HANTS"))  method_stacks[["HANTS"]]  <- stack_pred_HANTS
  if ("ND"     %in% methods && exists("stack_pred_ND"))     method_stacks[["ND"]]     <- stack_pred_ND
  
  # 2. Extract matching scalar weights calculated in your performance step
  active_methods <- names(method_stacks)
  active_weights <- weights[active_methods]
  
  # 3. Compute pixel-wise weighted sum and weight denominator
  # Initialize zero-valued rasters matching the dimensions/crs of the input stack
  weighted_sum_stack <- terra::rast(stack_NA, vals = 0)
  weight_denom_stack <- terra::rast(stack_NA, vals = 0)
  
  for (m in active_methods) {
    w <- active_weights[[m]] #retrieve weight
    r_stack <- method_stacks[[m]] #retrieve stack
    
    if (w > 0) {
      # Identify where predictions are non-NA
      valid_mask <- !is.na(r_stack)
      
      # Set NAs to 0 in copy for clean addition
      r_clean <- terra::classify(r_stack, cbind(NA, 0))
      
      # Accumulate weighted values and weight mask
      weighted_sum_stack <- weighted_sum_stack + (r_clean * w) # value * weight 
      weight_denom_stack <- weight_denom_stack + (valid_mask * w) # always the weight (i.e. will be total weight of approx cells)
      
      rm(r_clean, valid_mask) #clean-up
    }
  }
  
  # 4. Divide numerator by denominator (returns NA where weight_denom_stack is 0)
  weight_denom_stack[weight_denom_stack == 0] <- NA
  stack_pred_ensemble <- weighted_sum_stack / weight_denom_stack
  names(stack_pred_ensemble) <- paste0(names(stack), "_ensemble")
  
  rm(weighted_sum_stack, weight_denom_stack) #cleanup
  
  ########################################
  ### FIGURE TO SAVE APPROXIMATION FIT ###
  ########################################
  if(!is.null(figure_path)){
      for (i in results$Method) {
        predName <- paste0("y_pred_",i)
        dir.create(paste0(figure_path,"_",i,".png"), showWarnings = F, recursive = T) #to make the directory
        png(file = paste0(figure_path,"_",i,".png"), width = 2000, height = 2000, res = 300)
        smoothScatter(track_df[[predName]] ~ track_df$y_true,
                      bandwidth = 0.1, nrpoints=10000,
                      xlab = "True Y", ylab = paste0("Predicted Y by ",i),
                      main = i)
        dev.off()
      }
  }
  
  #################################
  ### CONSTRUCT THE OUTPUT LIST ###
  #################################
  # Initialize the output list with 'results' as the first element
  output <- list(metrics = results)
  
  # Conditionally add stacks if they were computed
  if ("linear" %in% methods && exists("stack_pred_lin")) {output$stack_pred_linear <- stack_pred_lin}
  if ("spline" %in% methods && exists("stack_pred_spline")) {output$stack_pred_spline <- stack_pred_spline}
  if ("HANTS" %in% methods && exists("stack_pred_HANTS")) {output$stack_pred_HANTS <- stack_pred_HANTS}
  if ("ND" %in% methods && exists("stack_pred_ND")) {output$stack_pred_ND <- stack_pred_ND}
  if (exists("stack_pred_ensemble")) {output$stack_pred_ensemble <- stack_pred_ensemble}
  
  
  gc(verbose = F)
  
  return(output)
 
  
} 
