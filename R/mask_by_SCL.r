mask_scl <- function(image) {
  scl <- image$select("SCL")
  
  # Good pixels are anything *not* cloud, cloud shadow, or snow.
  mask <- scl$neq(3)$  # Cloud shadow
          And(scl$neq(8))$   # Cloud (medium probability)/ not including 7 as it may not be a cloud
          And(scl$neq(9))$   # Cloud (high probability)
          And(scl$neq(10))$  # Cirrus
          And(scl$neq(11))   # Snow/Ice
  
  # Apply the mask
  image$updateMask(mask)
}
