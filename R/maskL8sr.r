maskL8sr <- function(image) {
  qa <- image$select("QA_PIXEL")
  
  # Bit masks
  cloud_conf_mask  <- bitwShiftL(3, 8)
  shadow_conf_mask <- bitwShiftL(3, 10)
  snow_conf_mask   <- bitwShiftL(3, 12)
  cirrus_conf_mask <- bitwShiftL(3, 14)
  
  # Use ee$Image$rightShift for rgee
  cloud_conf  <- ee$Image$rightShift(qa$bitwiseAnd(cloud_conf_mask), 8)
  shadow_conf <- ee$Image$rightShift(qa$bitwiseAnd(shadow_conf_mask), 10)
  snow_conf   <- ee$Image$rightShift(qa$bitwiseAnd(snow_conf_mask), 12)
  cirrus_conf <- ee$Image$rightShift(qa$bitwiseAnd(cirrus_conf_mask), 14)
  
  # Keep only pixels with None/Low (0 or 1)
  mask <- cloud_conf$lt(2)$And(
    shadow_conf$lt(2)$And(
      snow_conf$lt(2)$And(
        cirrus_conf$lt(2)
      )
    )
  )
  
  image$updateMask(mask)
}

