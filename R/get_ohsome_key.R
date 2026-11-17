#' Internal helper function to fetch the API key
#'
#' @export
#' @examples
#' #PLACEHOLDER()
#' 


get_ohsome_key <- function() {
  # 1. Check if option is set
  key <- getOption("ohsome.api_key")
  
  # 2. If not, check environment variable
  if (is.null(key) || key == "") {
    key <- Sys.getenv("OHSOME_API_KEY")
  }
  
  # 3. If still empty, throw a clear instruction error
  if (key == "") {
    stop(
      "No ohsome API key found.\n",
      "Please set it using: Sys.setenv(OHSOME_API_KEY = 'your_key_here')\n",
      "or add it to your .Renviron file.",
      call. = FALSE
    )
  }
  
  return(key)
}
