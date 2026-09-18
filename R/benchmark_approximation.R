#' Temporal remote sensing data often has NA by cloud and other types of masking. 
#' This function approximates linear, spline, HANTS and median normalized difference to asses performance of these methods.
#' It returns a small data frame with the performance of approximation with Mean Absolute Error (MAE), Root Mean Square Error (RMSE), Bias and R-squared (R2).
#' Optionally, users can get a smoothScatter for the fit of the approximated against the real values.
#'
#' NOTE: This function can run into std::bad_alloc errors. Reducing terraOptions(mem_frac) tends to help.
#' Literature: 
#' @param stack stack. Stack of rasters that need to be interpolated.
#' @param methods character vector. Names of the method to approximate NAs.Available are c("linear","spline","HANTS","spatial")
#' @param HANTS_freq Integer. Frequency parameter for HANTS interpolation.  Should be estimated periodicity + 3. 
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
                                    HANTS_freq = 4,
                                    prop_NA = 0.1,
                                    seed = 43,
                                    verbose = F,
                                    ncore = 1,
                                    figure_path = NULL){
  
  #seed
  if (!is.null(seed)) set.seed(seed)
  
  # params
  nlyrs <- nlyr(stack) 
  stack_NA <- stack #copy
  
  # Track sample positions and true values
  track_df <- list()
  
  # Step 1: Use spatSample to quickly sample valid cells & introduce NAs
  for (i in 1:nlyrs) {
    
    #check the NA number
    NA_samples <- round(sum(!is.na(values(stack[[i]])))*prop_NA, digits =0)
    
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
  
  ### ALTERNATIVE CELL MASKING ###
  # mask sampled cells.
  if (nrow(track_df) > 0) {
    
    # Pre-allocate a list to hold temporary file-backed rasters
    masked_layers <- list()
    
    for (u in seq_len(nlyrs)) {
      if(verbose) print(paste("Applying NAs to layer", u))
      
      # 1. Isolate the single layer
      lyr <- stack[[u]]
      
      # 2. Extract to a native R numeric vector (bypasses C++ spatial overhead)
      v <- terra::values(lyr)
      
      # 3. Apply NAs using standard R indexing (instantaneous)
      NAidx <- track_df$cell[track_df$layer == u]
      if (length(NAidx) > 0) {
        v[NAidx] <- NA
      }
      
      # 4. Put the modified values back into the layer
      terra::values(lyr) <- v
      
      # 5. Write to a temporary file on disk to completely clear it from RAM
      tmp_file <- tempfile(fileext = ".tif")
      masked_layers[[u]] <- terra::writeRaster(lyr, tmp_file, overwrite = TRUE)
      
      # 6. Force R to clean up memory before the next loop starts
      rm(lyr, v)
      gc(verbose = FALSE)
    }
    
    # 7. Rebuild the stack directly from the lightweight disk files
    stack_NA <- terra::rast(masked_layers)
    
    if (verbose) message("STATUS: Random NAs applied across stack")
  }
  
  
  ### INTERPOLATION METHODS BELOW HERE ###
  
    ###LINEAR INTERPOLATION###
    if("linear" %in% methods){
        if(verbose){message("Evaluating: Linear approximation")}  
      
        #predict linearly
        stack_pred <- terra::approximate(stack_NA, method = "linear")
    
        # Extract predicted values at the sampled cell/layer positions
        track_df$y_pred_linear <- 0
        
        #retrieve predictions
        for (i in unique(track_df$layer)) {
          NAidx <- track_df$cell[track_df$layer==i]
          y_pred <- as.numeric(values(stack_pred[[i]])[NAidx])
          track_df$y_pred_linear[track_df$layer==i] <- y_pred
          if(verbose){print(paste0("copying predictions to df for layer ",i))}
        }
        
    }
    
    ###SPLINE INTERPOLATION###
    if("spline" %in% methods){
      if(verbose){message("Evaluating: Spline approximation")}  
      
      #spline prediction
      stack_pred <- terra::app(stack_NA, fun = spline_pixel, cores = ncore)
      
      # Extract predicted values at the sampled cell/layer positions
      track_df$y_pred_spline <- 0
      
      #retrieve predictions
      for (i in unique(track_df$layer)) {
        NAidx <- track_df$cell[track_df$layer==i]
        y_pred <- as.numeric(values(stack_pred[[i]])[NAidx])
        track_df$y_pred_spline[track_df$layer==i] <- y_pred
        if(verbose){print(paste0("copying predictions to df for layer ",i))}
      }
    }
  
    ###HANTS INTERPOLATION###
    if("HANTS" %in% methods){
      if(verbose){message("Evaluating: HANTS approximation")}  
      
      #HANTS
      stack_pred <- HANTS_stack(stack_NA, cores = 8, freq = HANTS_freq, max_iter = 10, tolerance = 0.1)
      
      # Extract predicted values at the sampled cell/layer positions
      track_df$y_pred_HANTS <- 0
      
      #retrieve predictions
      for (i in unique(track_df$layer)) {
        NAidx <- track_df$cell[track_df$layer==i]
        y_pred <- as.numeric(values(stack_pred[[i]])[NAidx])
        track_df$y_pred_HANTS[track_df$layer==i] <- y_pred
        if(verbose){print(paste0("copying predictions to df for layer ",i))}
      }
    }
  
    ###SPATIAL INTERPOLATION
    if("spatial" %in% methods){
      if(verbose){message("Evaluating: spatial approximation")}  
      
      #spat interpolation
      stack_pred <- temporal_spatial_approximation(stack_NA)
      
      # Extract predicted values at the sampled cell/layer positions
      track_df$y_pred_spatial <- 0
      
      #retrieve predictions
      for (i in unique(track_df$layer)) {
        NAidx <- track_df$cell[track_df$layer==i]
        y_pred <- as.numeric(values(stack_pred[[i]])[NAidx])
        track_df$y_pred_spatial[track_df$layer==i] <- y_pred
        if(verbose){print(paste0("copying predictions to df for layer ",i))}
      }
    }
    
  
  ### INTERPOLATION METHODS DONE. TIME TO ASSESS ###
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
    } else {
      err  <- yp - yt
      mae  <- mean(abs(err))
      rmse <- sqrt(mean(err^2))
      bias <- mean(err) # Over/under-estimation
      r2   <- suppressWarnings(cor(yp, yt, use = "complete.obs")^2)
    }
    
    results[[method]] <- data.frame(
      Method       = method,
      MAE          = round(mae, 4),
      RMSE         = round(rmse, 4),
      Bias         = round(bias, 4),
      R2           = round(r2, 4),
      Unfilled_NAs = sum(is.na(track_df[[ApproxColNm]]))
    )
  }
    
  #combine results
  results <- do.call(rbind, results)
  
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
  err <- track_df$y_pred_ensemble[valid_ens] - track_df$y_true[valid_ens]
  results <- results %>% add_row(Method = "ensemble",
                                 MAE = round(mean(abs(err)),4),
                                 RMSE = round(sqrt(mean(err^2)),4),
                                 Bias = round(mean(err),4),
                                 R2 = round(suppressWarnings(cor(track_df$y_pred_ensemble, track_df$y_true, use = "complete.obs")^2),4)
                                 )
  
  # Save a figure of fit of the approximation
  if(!is.null(figure_path)){
      for (i in results$Method) {
        predName <- paste0("y_pred_",i)
        
        agg_png(file = paste0(figure_path,".png"), width = 2000, height = 2000, res = 300)
        smoothScatter(track_df[[predName]] ~ track_df$y_true,
                      bandwidth = 0.1, nrpoints=10000,
                      xlab = "True Y", ylab = paste0("Predicted Y by ",i),
                      main = i)
        dev.off()
      }
  }
  
  return(results)
 
  
} 
