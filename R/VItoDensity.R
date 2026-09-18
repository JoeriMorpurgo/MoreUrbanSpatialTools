#' Transform Vegetation Index (VI) values into Fractional vegetation coverage and optionally into absolute coverage into m2. 
#' This function provides two options to rescale VI into Fractional vegetation coverage.
#' Either provide a threshold for the VI_min argument or the character value "dynamic"" to use green and grey from LULC maps to determine a threshold via a classification problem. 
#' Threshold by dynamic classification is calculated using a Weighted Youden threshold with sensitivity-weight 0.75 and specificity-weight of 0.25 
#' The dynamic threshold argument also clamps all values below 0, to make the thresholding more reliable in case of odd patterns or data.
#' The function returns either a raster or stack if save_fcv used with multiple values.
#'
#' NOTE: 
#' Literature: Carlson, T. N., & Ripley, D. A. (1997). On the relation between NDVI, fractional vegetation cover, and leaf area index. Remote Sensing of Environment, 62(3), 241–252. https://doi.org/10.1016/S0034-4257(97)00104-1; Li, D., Shen, F., Yin, Y., Peng, J., & Chen, P. (2013). Weighted Youden index and its two-independent-sample comparison based on weighted sensitivity and specificity. Chinese Medical Journal, 126(6), 1150–1154. https://doi.org/10.3760/cma.j.issn.0366-6999.20123102
#' @param city_name character. Relates to the place where to look for data.
#' @param date character. The year of the VI (and optionally LULC-map) to consider. Works only with YYYY-MM-DD format. or numeric year as YYYY format.
#' @param VI character or terra rast. Character needs to be one of the follow: c("NDVI", "EVI", "MSAVI") and will retrieve this file. If a rast is provided this will be used as VI input.
#' @param VI_min numeric OR "Dynamic". Numeric (default = 0.25) denotes the minimal threshold value of NDVI to be considered 0% vegetation coverage. The dynamic option requires LULC to be downloaded and draws random point in LULC that is and isn't green to determine a threshold. This also produces a plot and some extra information. NOTE: The dynamic approach only takes 1 raster layer. Providing a numeric value allows for the processing of a full Stack.
#' @param weight numeric. Defaults to 0.75. Weight to be assigned to the Weighted Youden Index. 
#' @param figure logical or path. Defaults to FALSE to not save a figure. TRUE save figure in the results folder where it shows A) distributions of values for green, grey and blue classes and the calculated threshold and B) Optimization curves with specificity, sensitivity and curves for (weighted) Youden Index. Alternatively, a path can be given to store the figure there.
#' @param maskLayer spatVector. Object to mask the VI raster by.
#' @param m2 logical. standard is F. When set to T, an extra multiplication is done by cell size to estimate the m2 covered by vegetation.
#' @param save_fcv character vector. default is NULL. Provide one or more of the following to scale the VI to that threshold c("95_sens","w_youden","youden"). Raster, or stack, is saved to results folder.
#' @param save_path character vector. Defaults to the results folder. Users may specify path to store the fcv raster or stack in a different location or way.
#' @export
#' @examples
#' PLACEHOLDER()
#' 

############ get_municipal_bbox #############
VItoDensity <- function(city_name,
                          date = NULL,
                          VI,
                          lulc = NULL,
                          VI_min = 0.25, #minimum level of NDVI to be considered green
                          weight = 0.75,
                          maskLayer = NULL,
                          m2 = F, #Which calc to do
                          figure = F,
                          save_fcv = NULL,
                          save_path = NULL){ 
    
  

#Grab year structure from character vector
if(is.character(date)){year <- lubridate::year(date)} else{year <- date}
VItype <- "userInput"

# Capture time sequence to add back in later
orig_time <- terra::time(VI)
  
        #Retrieve VI
        if(is.character(VI)){ #is the VI a character, which indicates retrieval.
          if(VI %in% c("NDVI", "MSAVI", "EVI")){ #this function needs to be upgraded to be more flexible
          VItype <- as.character(VI) #save type
          tifFile <- list.files(paste0(getwd(),"/MUST_downloaded_data/",city_name,"/",VI,"/"),
                                pattern = paste0(VI,"_median.tif"), #this grab always file with ending in the next year.
                                full.names = T)
          VI <- terra::rast(tifFile)
          }
        }
    
  #masks the VI to limit the values picked.
  if(!is.null(maskLayer)){VI <- terra::mask(VI, maskLayer)}
     
    
    #Calc max NDVI
    VI_maxes <- as.numeric(unlist(terra::global(VI, fun = "max", na.rm = T)))
    VI_max <- max(VI_maxes, na.rm=T)
    print(paste0("Found and using the max value of ",VI_max, " for scaling. With a SD of: ",round(sd(VI_maxes, na.rm = T), digits = 4)))
    
    #Threshold options
    if(is.numeric(VI_min)){
      
      print(paste0("VI_min is set at: ", VI_min))
      
      ###MINIMUM THRESHOLD SET BY THE USER###
      VI[VI < VI_min] <- NA
      output <- terra::app(VI, fun=function(x){((x-VI_min)/(VI_max-VI_min))})
      output <- terra::subst(output, NA, 0)
      names(output) <- paste0(names(output),"_FcV_",VI_min)
      
    } else {
      
      ### THRESHOLD SET DYANMICALLY ###
      #remove values below 0. These are assumed to never be vegetation.
      VI <- terra::clamp(VI, lower = 0, upper = Inf, value = F)
      
      
      #lulc to validate
      if(is.null(lulc)){lulc <- terra::vect(paste0(getwd(),"/MUST_downloaded_data/",city_name,"/lulc",year,".gpkg"))}
      water_lulc <- c(lulc_info$Value[lulc_info$infrastructure_class== "blue"])
      green_lulc <- c(lulc_info$Value[lulc_info$infrastructure_class== "green"])
      grey_lulc <- c("parking_space", "construction", "prison", "commercial", "parking", "industrial")
      
      #5000 random points from NDVI raster
      pts <- terra::spatSample(VI, size = 10000000, method = "random", na.rm = TRUE, as.points = TRUE, warn = F)
      VI_vals <- terra::extract(VI, pts)[, 2] #vector with the values
      pts <- terra::project(pts, lulc) #project the points to the LULC map
      
      #check NDVI values by LULC
      ##Green
      green_lulc_ndvi <- terra::extract(lulc[lulc$lulc%in%green_lulc], pts)
      green_lulc_ndvi <- na.omit(green_lulc_ndvi)
      green_lulc_ndvi$ndvi <- VI_vals[green_lulc_ndvi$id.y]
      density_green <- density(green_lulc_ndvi$ndvi)
      ngreen <- length(green_lulc_ndvi$ndvi)
      ##Water
      water_lulc_ndvi <- terra::extract(lulc[lulc$lulc%in%water_lulc], pts)
      water_lulc_ndvi <- na.omit(water_lulc_ndvi) #NDVI outside of LULC file or no LULC present.
      water_lulc_ndvi$ndvi <- VI_vals[water_lulc_ndvi$id.y]
      density_water <- density(water_lulc_ndvi$ndvi)
      nwater <- length(water_lulc_ndvi$ndvi)
      ##Grey
      grey_lulc_ndvi <- terra::extract(lulc[lulc$lulc%in%grey_lulc], pts)
      grey_lulc_ndvi <- na.omit(grey_lulc_ndvi)
      grey_lulc_ndvi$ndvi <- VI_vals[grey_lulc_ndvi$id.y]
      density_grey <- density(grey_lulc_ndvi$ndvi)
      ngrey <- length(grey_lulc_ndvi$ndvi)
      
      
      ### THRESHOLD OPTIMIZATION ####
      pos_vals <- green_lulc_ndvi$ndvi
      neg_vals <- grey_lulc_ndvi$ndvi
      n_pos <- as.numeric(ngreen)
      n_neg <- as.numeric(ngrey)
      
      if(n_pos > 0 && n_neg > 0) {
        # Fast mathematical calculation of AUC via Wilcoxon Rank Sum (removes package dependency)
        all_vals <- c(pos_vals, neg_vals)
        labels <- c(rep(1, n_pos), rep(0, n_neg))
        ranks <- rank(all_vals)
        auc_score <- (sum(as.numeric(ranks[labels == 1])) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
        
        # Iterate over a range of 100 plausible threshold cuts
        threshold_range <- seq(min(all_vals), max(all_vals), length.out = 100)
        metrics <- data.frame(threshold = threshold_range, sensitivity = NA, specificity = NA, youden_j = NA, weighted_j = NA)
        
        for(i in 1:nrow(metrics)) {
          t <- metrics$threshold[i]
          tp <- sum(pos_vals >= t)
          fn <- sum(pos_vals < t)
          tn <- sum(neg_vals < t)
          fp <- sum(neg_vals >= t)
          
          metrics$sensitivity[i] <- tp / (tp + fn)
          metrics$specificity[i] <- tn / (tn + fp)
          metrics$youden_j[i] <- metrics$sensitivity[i] + metrics$specificity[i] - 1 # Yao Index / Youden's J
          metrics$weighted_j[i] <- 2*(weight * metrics$sensitivity[i] + (1-weight) * metrics$specificity[i])-1 #favour sensitivity
        }
        
        # Select optimal threshold based on Youden Index
        opt_idx <- which.max(metrics$youden_j)
        thresholdGreen_j <- metrics$threshold[opt_idx]
        opt_j <- metrics$youden_j[opt_idx]
        opt_sens <- metrics$sensitivity[opt_idx]
        opt_spec <- metrics$specificity[opt_idx]
        
        # optimal threshold based on weighted Youden
        opt_idx_w <- which.max(metrics$weighted_j)
        thresholdGreen_w <- metrics$threshold[opt_idx_w]
        
        # High sensitivity threshold
        High_sens_opt <- metrics$threshold[metrics$sensitivity >=0.95]
        if(length(High_sens_opt) > 0){
          thresholdHigh95 <- max(High_sens_opt)
        } else {
          thresholdHigh95 <- min(metrics$threshold)
        }
      } 

      
      ### PLOTS ###
      #plot params
      x_min <- min(c(density_green$x, density_grey$x, density_water$x))
      x_max <- max(c(density_green$x, density_grey$x, density_water$x))
      y_max <- max(c(density_green$y, density_grey$y, density_water$y))
      
      # place to save the figrues
      if(figure){ #standard location. AKA figure == T
        dir.create(paste0("MUST_downloaded_data/",city_name,"/result/VItoDensity/"), recursive = T, showWarnings = F)
        pdf(file = paste0("MUST_downloaded_data/",city_name,"/result/VItoDensity/",year,"_",VItype,"_thresholds.pdf"), width = 14, height = 6)
        }
      #user-specified location
      if(is.character(figure)){
        pdf(file = paste0(figure,".pdf"), width = 14, height = 6)
      }
      
      #specify how to panel the plots.
      par(mfrow = c(1, 2))
      
      # PANEL 1: Histograms
        plot(density_green, col = "forestgreen", lwd = 2, 
             main = paste0("VI Distribution by LULC for ",city_name, " ",year), 
             xlab = "VI", xlim = c(x_min, x_max), ylim = c(0, y_max))
        polygon(density_green, col = rgb(0.13, 0.55, 0.13, 0.3), border = NA)
        
        if(length(grey_lulc_ndvi$ndvi) > 2) {
          lines(density_grey, col = "darkgrey", lwd = 2)
          polygon(density_grey, col = rgb(0.66, 0.66, 0.66, 0.3), border = NA)
        }
        if(length(water_lulc_ndvi$ndvi) > 2) {
          lines(density_water, col = "dodgerblue", lwd = 2)
          polygon(density_water, col = rgb(0.12, 0.56, 1, 0.3), border = NA)
        }
        
        #vertical lines for the thresholds
        #abline(v = thresholdGreen, col = "forestgreen", lty = 2, lwd = 2)
        abline(v = thresholdGreen_j,col = "darkgreen", lty = 2, lwd  = 2)
        abline(v = thresholdGreen_w, col = "darkorange", lty = 2, lwd = 2)
        abline(v = thresholdHigh95, col = "forestgreen", lty = 2, lwd = 2)
        #if(exists("thresholdGrey")) abline(v = thresholdGrey, col = "darkgrey", lty = 2, lwd = 2)
        
        #legend
        legend("topleft", legend = c(paste0("Green, n = ",ngreen),
                                     paste0("Grey, n = ",ngrey),
                                     paste0("Blue, n = ",nwater),
                                     paste("95% Sens Threshold = ",round(thresholdHigh95, 3)),
                                     paste("Youden Threshold = ",round(thresholdGreen_j, 3)),
                                     paste("Weighted Youden Threshold =", round(thresholdGreen_w, 3))),
               fill = c(rgb(0.13, 0.55, 0.13, 0.3),
                        rgb(0.66, 0.66, 0.66, 0.3),
                        rgb(0.12, 0.56, 1, 0.3),
                        NA, NA, NA),
               border = c("forestgreen", "darkgrey", "dodgerblue", NA, NA, NA),
               col = c(NA, NA, NA, "forestgreen", "darkgreen", "darkorange"),
               lty = c(NA, NA, NA, 2, 2, 2),
               lwd = 2,
               cex = 0.75,
               bty = "n",
               y.intersp = 0.8)
      
      
      # PANEL 2: Metrics Curve (The r.class.threshold visualization)
        plot(metrics$threshold, metrics$sensitivity, type = "l", col = "forestgreen", lwd = 2,
             ylim = c(min(metrics$youden_j, 0), 1), xlab = "Threshold Value", ylab = "Metric Score",
             main = paste0("Optimization Curves (AUC = ", round(auc_score, 3), ")"))
        lines(metrics$threshold, metrics$specificity, col = "darkgreen", lwd = 2)
        lines(metrics$threshold, metrics$youden_j, col = "firebrick", lwd = 2)
        lines(metrics$threshold, metrics$weighted_j, col = "darkorange", lwd = 2)
        abline(v = thresholdGreen_j, col = "black", lty = 2, lwd = 1.5)
        abline(v = thresholdHigh95, col = "darkgreen", lty = 2, lwd = 1.5)
        abline(v = thresholdGreen_w, col = "darkorange", lty = 2, lwd = 1.5)
        
        legend("bottomleft",
               legend = c(paste0("Sensitivity (", round(opt_sens, 2), ")"),
                          paste0("Specificity (", round(opt_spec, 2), ")"),
                          paste0("Yao / Youden J (", round(opt_j, 2), ")"),
                          paste0("Youden Thresh = ", round(thresholdGreen_j, 3)),
                          paste0("95% Sens Thresh = ", round(thresholdHigh95, 3)),
                          paste0("Weighted Youden Thresh = ", round(thresholdGreen_w, 3))),
               col = c("forestgreen", "dodgerblue", "firebrick", "black", "darkgreen", "darkorange"), 
               lty = c(1, 1, 1, 2, 2, 2),
               lwd = 2, bg = "white",
               cex = 0.75,
               bty = "n",
               y.intersp = 0.8)

      
      dev.off()
    }
    
    
    if(!is.null(save_fcv)){
      
      #scale VI to fcv according to the thresholds requested
      #Proportionally scale VI
      if(VI_min == "dynamic"){ #If dynamic and we want to save the fcv let's scale the rasters here
        #copy for scaling interatively
        VItoScale <- VI #copy
        
        for (i in save_fcv) { #one by one the rasters wanted
          
          #pick the right threshold
          threshold <- "undefined"
          threshold <- if(i == "95_sens"){threshold <- thresholdHigh95}else{threshold <- threshold}
          threshold <- if(i == "w_youden"){threshold <- thresholdGreen_w}else{threshold <- threshold}
          threshold <- if(i == "youden"){threshold <- thresholdGreen_j}else{threshold <- threshold}
          
          #scale to Vi to fcv
          VItoScale[VItoScale < threshold] <- NA
          nameForLayer <- paste0("FcV_",i,"_",round(threshold, digits = 3))
          assign(paste0("VI_scaled_",i),
                 terra::app(VItoScale, fun=function(x){((x-threshold)/(VI_max-threshold))}))
          
          #really inefficient way to assign name...
          tmp <- get(paste0("VI_scaled_",i))
          names(tmp) <- as.character(nameForLayer)
          assign(paste0("VI_scaled_",i),
                 tmp)
          
        }
        #stack the individual layers to 1 output
        output <- terra::rast(mget(c(paste0("VI_scaled_",save_fcv))))
        output[is.na(output)] <- 0
      }

      
      #multiply proportional by cell size to get m2
      if(m2){ # user wants m2
        size <- terra::cellSize(output)
        output <- size*output
      }
      
      #add some attributes
      metags(output) <- c(w_youden_threshold = thresholdGreen_w) #add attribute of the threshold used.
      metags(output) <- c(youden_threshold = thresholdGreen_j)
      metags(output) <- c(threshold_95 = thresholdHigh95)
      metags(output) <- c(medGrey = median(grey_lulc_ndvi$ndvi))
      metags(output) <- c(medGreen = median(green_lulc_ndvi$ndvi))
      metags(output) <- c(medBlue = median(water_lulc_ndvi$ndvi))
      
      #save the fcv raster
      if(is.null(save_path)){
      dir.create(paste0("MUST_downloaded_data/",city_name,"/result/"), showWarnings = F)
      dir.create(paste0("MUST_downloaded_data/",city_name,"/result/VItoDensity/"), showWarnings = F)
      terra::writeRaster(output, filename = paste0("MUST_downloaded_data/",city_name,"/result/VItoDensity/",year,"_",VItype,"_scaled_",VI_min,".tif"),
                         overwrite = T)
      }else{(terra::writeRaster(output, filename = paste0(save_path,".tif")))}
      
      }
  
    #add in time back
    terra::time(output) <- orig_time
    
  print("Returning scaled raster")
  return(output)
}
