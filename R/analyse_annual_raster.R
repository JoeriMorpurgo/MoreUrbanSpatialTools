analyze_annual_raster <- function(r_current, r_prior = NULL, veg_threshold = 0.3) {
  cell_area <- prod(res(r_current)) # Cell area in m^2
  total_cells <- ncell(r_current) - freq(is.na(r_current))[2, "count"]
  total_area <- total_cells * cell_area
  
  # A. Base Coverage Metrics
  base_cov_abs <- sum(values(r_current), na.rm = TRUE) * cell_area # Absolute green area
  base_cov_prop <- base_cov_abs / total_area #green area proportional to the total area
  
  # B. Annual Change Metrics (requires prior year raster)
  loss_abs <- NA
  gain_abs <- NA
  diff_abs <- NA
  morans_i <- NA
  center_edge_ratio <- NA
  
  if (!is.null(r_prior)) {
    diff_r <- r_current - r_prior
    diff_vals <- values(diff_r)
    
    # Absolute Loss, Gain, Difference (m^2)
    diff_abs <- sum(diff_vals, na.rm = TRUE) * cell_area
    loss_abs <- sum(abs(diff_vals[diff_vals < 0]), na.rm = TRUE) * cell_area
    gain_abs <- sum(diff_vals[diff_vals > 0], na.rm = TRUE) * cell_area
    
    # Spatial Clumping: Global Moran's I on continuous change raster
    morans_i <- as.numeric(terra::autocor(diff_r, method = "moran"))
    
    patches <- patches(diff_r)
    inverseRast <- patches
    inverseRast[is.na(inverseRast)] <- -9999
    inverseRast[inverseRast>-.1] <- NA
    distRast <- distance(inverseRast) #distance to cell with value

    distdf <- data.frame(i = numeric(),
               distPos = numeric(),
               distNeg = numeric(),
               sum_dist_pos_change = numeric(),
               sum_dist_neg_change = numeric())
    
    for (i in seq(from = 0, to = 0.9, length.out = 10)) {
      
      pos_change_r <- diff_r > i & diff_r < (i+0.1)
      neg_change_r <- diff_r < -i & diff_r < (-i-0.1)
      
      mean_dist_pos_change <- mean((distRast[pos_change_r]), na.rm = T)
      sum_dist_pos_change <- sum(diff_r[pos_change_r], na.rm = T)
      mean_dist_neg_change <- mean((distRast[neg_change_r]), na.rm = T)
      sum_dist_neg_change <- sum(diff_r[neg_change_r], na.rm = T)
      
      distdf <- distdf %>% add_row(i = i,
                                   i_end = i+0.1,
                                   distPos = mean_dist_pos_change,
                                   distNeg = mean_dist_neg_change,
                                   sum_dist_pos_change = sum_dist_pos_change,
                                   sum_dist_neg_change = sum_dist_neg_change)
      
    }

    #plot
    if(!is.null(figure)){
    pdf(file = paste0(figure,"/test.pdf"))
    plot(distdf$i, distdf$distPos, type = "n", ylim = c(min(c(distdf$distPos, distdf$distNeg), na.rm = T),
                                                            max(c(distdf$distPos, distdf$distNeg), na.rm = T)),
         xlab = "Vegetation difference threshold", ylab = "Mean distance to border (m)")
    
    #lines
    lines(distdf$i, distdf$distPos, type = "p", col = "darkgreen")
    lines(distdf$i, distdf$distNeg, type = "p", col = "darkred")
    #legend
    legend("topright", legend = c("Positive changes", "Negative changes"),
           col = c("darkgreen", "darkred"), pch = 1)
    dev.off()
    }
    
  }
  
  # C. Landscape Metrics (Requires binary patch raster)
  binary_r <- r_current >= veg_threshold
  lpi <- lsm_l_lpi(binary_r)$value
  ai <- lsm_l_ai(binary_r)$value
  edge_ratio <- lsm_l_ed(binary_r)$value # Edge Density (m/ha)
  
  return(data.frame(
    total_area_m2 = total_area,
    base_cov_m2 = base_cov_abs,
    base_cov_prop = base_cov_prop,
    diff_m2 = diff_abs,
    loss_m2 = loss_abs,
    gain_m2 = gain_abs,
    lpi_pct = lpi,
    ai_pct = ai,
    edge_density_m_ha = edge_ratio,
    change_morans_i = morans_i,
    center_edge_ratio = center_edge_ratio
  ))
}