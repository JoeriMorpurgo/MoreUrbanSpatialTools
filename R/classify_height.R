#' Classifying height and proportionalise to template
#'
#' This function takes a raster representing height and a template to generate three rasters of proportional grass, shrub and trees.
#' First, This function classifies height as grass (-0.1,1), shrub (1 - 5) or tree (5 +).
#' After this function calculates the proportion of the height class through average resampling 
#' NOTE: 
#' Literature: CUGIC: The Consolidated Urban Green Infrastructure Classification for assessing ecosystem services and biodiversity, https://doi.org/10.1016/j.landurbplan.2023.104726
#' @param clean_dhm A raster representing a digital height model.
#' @param template A raster (of coarser resolution) that can be used to calculate proportions per height class.
#' @keywords classification, height, proportion
#' @export
#' @examples
#' classify_height()
#' 


classify_height <- function(clean_dhm, template){
  
  cat("Classifying height, grass 0-1, shrub 1-5, tree 5+...\n")
  reclass_df <- matrix(c(-9999, -0.1, NA,
                         -0.1, 1, 1,   # Grass
                         1, 5, 2,     # Shrub
                         5, 99999, 3  # Tree
  ), ncol = 3, byrow = TRUE)
  height_class <- classify(clean_dhm, reclass_df)
  
  cat("Separating height classes...\n")
  height1 <- ifel(height_class == 1, 1, 0) #Make binary
  height1 <- subst(height1, NA, 0) #So all cells are filled for proportional calcs.
  height2 <- ifel(height_class == 2, 1, 0)
  height2 <- subst(height2, NA, 0)
  height3 <- ifel(height_class == 3, 1, 0)
  height3 <- subst(height3, NA, 0)
  
  cat("Resampling height classes to template grid...\n")
  height1 <- resample(height1, template, method = "average", threads = T)
  height2 <- resample(height2, template, method = "average", threads = T)
  height3 <- resample(height3, template, method = "average", threads = T)
  
  cat("Combining proportional layers")
  density_height <- c(height1, height2, height3)
  
  cat("Masking to the template")
  density_height <- terra::mask(density_height, template,
                                maskvalues = NA)
  
  
  return(density_height)
}
