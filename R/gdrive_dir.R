#' Show the Folder Structure of the Shared Google Drive
#'
#' Prints the structure of the Shared Google Drive to the console. It provides a count of the files within
#' each folder and all subfolders. Items in the 'path' column can be copy-pasted to use as the
#' `gdrive_path` argument of the `gdrive_set_dribble(gdrive_path)` function. Highly recommended to use the `folder`
#' argument as it will speed up your search. Note that this function only works for shared drives in which you have
#' member-level access, not just folder-level access.
#'
#' @param shared_id An alias of a Shared Google Drive. By default, searches for the `default_gdrive_id` variable in your
#' .Renviron file.
#' @param folder A dribble or a character file path of folders, i.e., "Projects", that will narrow down your search.
#'
#' @details
#' Use this function to retrieve the file paths of every folder in the Shared Google Drive.
#' Once you have found the folder you would like to upload to or download from, copy its `path` to use
#' as the `gdrive_path` argument of the `gdrive_set_dribble()` function.
#'
#' @return Returns a data frame of the folder structure of the Shared Google Drive.
#'
#' @seealso `gdrive_set_dribble()`
#'
#' @examples
#' \dontrun{
#'    gdrive_dir()
#' }
#'
#' @export
gdrive_dir <- function(shared_id = "default_gdrive_id", folder = NULL) {

  if( !is.character(shared_id) | length(shared_id) != 1) stop("'id' needs to be a length = 1 character string.")

  # Ensure googledrive token is active
  if(!gdrive_token()) return(invisible())

  # Check the address of the shared_id
  id <- gdrive_get_shared_id(shared_id)

  tryCatch(
    gdrive_head <-  googledrive::with_drive_quiet(googledrive::shared_drive_get(id = id)),
    error = function(e)  {
      stop(paste("Either your shared_id was incorrect or you do not have root access to the shared drive."))
    }
  )

  # If 'folder' is specified, only dig through file structure of that folder.
  if( !is.null(folder) ){
    # If the folder is a dribble, use it
    if(googledrive::is_dribble(folder)) {
      gdrive_dribble <- folder
      folder <- gdrive_dribble$path
    } else {
      gdrive_dribble <- googledrive::with_drive_quiet(googledrive::drive_get(path = folder, shared_drive = gdrive_head))
    }
    # Remove any trailing slashes
    folder <- sub("[.]*/$", "", folder)
  } else {
    # Get the dribble of the shared drive
    gdrive_dribble <- gdrive_head
  }

  parent <- gdrive_dribble
  parent$shared_drive_id <- id
  parent$path <- NULL
  parent_search <- dir_search(parent)
  parent_search$shared_drive_id <- id

  while( nrow(parent_search$child) > 0 ){
    new_parent <- parent_search$child
    new_parent$shared_drive_id <- id
    new_parent_search <- dir_search(new_parent)
    parent_search <- list(
      parent = rbind(parent_search$parent, new_parent_search$parent),
      child = new_parent_search$child,
      fill = T
    )
  }

  # Omit the shared folder from the output
  folders_df <- parent_search$parent[-1, ]

  # Omit the shared folder from name, and add a "/" to the path names to specify these are folders and not files
  folders_df$gdrive_path <- sub(paste0(gdrive_dribble$name, "/"), "", paste0(folders_df$name, "/"))

  # Print the results
  cat("Shared Drive:", paste0(gdrive_head$name, "/", folder), "\n")

  # Format the output
  out <- folders_df[, c("gdrive_path", "files")]
  out$gdrive_path <- paste0(folder, "/", out$gdrive_path)
  out <- out[order(out$gdrive_path), ]
  out$abbr_name <- gsub("([^/]+)(?=/.+)", "..", out$gdrive_path, perl = T)
  out$nchar <- nchar(out$abbr_name)
  out$ws <- max(out$nchar) - out$nchar
  out$Directory <- apply(out, 1, function(x) paste0(x["abbr_name"], paste(rep(" ", times = x["ws"]), collapse = "")))
  print(out[, c("Directory", "files", "gdrive_path")], n = Inf)
}
