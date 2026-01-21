#' Find closest character match
#'
#' This function is an iterative wrapper around agrep. It matches strings and does so until a best answer is found.
#' NOTE: 
#' Literature: 
#' @param stringX Character value; The character value you are looking for or similar in a list.
#' @param stringY Character values; A vector containing the multiple character values of which you want to find the most similar to stringX.
#' @param max.dist Numeric value; Indicating maximum distance allowed between X and the result. Value must be between 0 - 1.
#' 0 needs the result to be the same as X. 1 Allows for maximum difference between X and the result.
#' @keywords words, similarity
#' @export
#' @examples
#' find_closest_chr_match()
#' 

find_closest_chr_match <- function(max.dist = 0.9,
                                   stringX = NULL,
                                   stringY = NULL){
  
  #sets param.
  element <- c(1,2) #This is terrible coding. But works.
  
  while (length(element) > 1 & max.dist > 0) {
    max.dist <- max.dist - 0.05
    element <- agrep(tolower(stringX), tolower(stringY), max.distance = max.dist, value = F)
  }
  
  #the character value that will be returned
  message(stringY[element])
  
  return(element)
}