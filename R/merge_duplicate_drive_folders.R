#' Check duplicate drive folder and merge them
#'
#' This function checks duplicate drive folders and merges them. WARNING: This will remove empty folder (i.e. folder that are having this name. Use with caution)
#' NOTE: 
#' Literature: 
#' @param folder_name character value. Name of the folder you want to be checked for duplication and merged.
#' @keywords classification, height, proportion
#' @export
#' @examples
#' merge_duplicate_drive_folders()
#' 

merge_duplicate_drive_folders <- function(folder_name) {

  
  # Search for all folders with the given name
  folders <- drive_find(type = "folder", q = paste0("name = '", folder_name, "'"))
  
  if (nrow(folders) <= 1) {
    message("Only one folder found. Nothing to merge.")
    return(invisible(NULL))
  }
  
  message("Found ", nrow(folders), " folders named '", folder_name, "'.")
  
  # Pick the first folder as the main one
  main_folder <- folders[1, ]
  duplicates <- folders[-1, ]
  
  for (i in seq_len(nrow(duplicates))) {
    dup_folder <- duplicates[i, ]
    
    # List all files in the duplicate folder
    files_in_dup <- drive_ls(as_id(dup_folder$id))
    
    # Move each file to the main folder
    for (j in seq_len(nrow(files_in_dup))) {
      file <- files_in_dup[j, ]
      message("Moving: ", file$name)
      drive_mv(file, path = as_id(main_folder$id))
    }
    
    # Delete the now-empty duplicate folder
    message("Deleting empty folder: ", dup_folder$name)
    drive_trash(dup_folder)
  }
  
  message("Merging complete.")
}