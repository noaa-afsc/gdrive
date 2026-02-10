#' Designate a Gdrive Folder as a Target for Uploads and Downloads
#'
#' Given a folder path of the folder structure of the Shared Google Drive, this function will create a `dribble`
#' class object for use by key functions to interact with the drive, including `gdrive_upload()` and
#' `gdrive_download()`. By default, it searches the FMA Analytical Services Program shared drive.
#'
#' @param gdrive_path a character string of the folder path, with each folder name followed by `/`
#' @param folder_id a character string of the folder's ID, which must be used instead of \code{gdrive_path} for
#' collaborators with only shared access to a shared drive's subfolder and not root access to the shared drive.
#' @param shared_id a character string of the shared drive's ID or an alias for the Shared Drive to connect to. The
#' default alias, `Analytics`, is so far the only alias that is currently recognized.
#'
#' @details If you do not know the folder path to the folder you want to interact with, run `gdrive_dir()` to get
#' the view the folder structure of the Share Google Drive and copy-paste the `path`
#'
#' @return Returns a dribble class object
#'
#' @examples
#' \dontrun{
#'    gdrive_set_dribble(gdrive_path = 'Google Drive Test/')
#' }
#'
#' @export
gdrive_set_dribble <- function(gdrive_path = NULL, shared_id = "Analytics", folder_id = NULL){

  # folder_id is required if working with collaborators who have shared access to a subfolder but not root access.
  if(is.null(gdrive_path) & is.null(folder_id)) stop("Either gdrive_path and/or folder_id must be specified!")
  if(!is.null(gdrive_path) & !is.null(shared_id)) {
    if( !is.character(gdrive_path) | length(gdrive_path) != 1) stop("'id' needs to be a length = 1 character string.")
    if( !is.character(shared_id) | length(shared_id) != 1) stop("'shared_id' needs to be a length = 1 character string.")
  }

  # Ensure googledrive token is active
  gdrive_token()

  # Get the dribble object from the gdrive_path. It will contain rows for all enclosed folders and files.
  if(!is.null(folder_id)) {
    # If using the folder_id, derive the shared drive ID from the folder's dribble
    dribble_out <- googledrive::with_drive_quiet(
      googledrive::drive_get(id = googledrive::as_id(folder_id))
    )
    id <- dribble_out$drive_resource[[1]]$driveId
  } else {
    # If you want to specify the project folder path, you also need the shared folder's id and access to it!
    # Recall hard coded shared folder ids from the alias
    if( shared_id == "Analytics") {
      id <- "0AJcHJWlPgKIgUk9PVA"
    } else {id <- shared_id}
    dribble_out <- googledrive::with_drive_quiet(
      googledrive::drive_get(path = gdrive_path, shared_drive = googledrive::as_id(id))
    )
  }

  # Only allow folders to be set as targets (exclude files)
  dribble_out <- dribble_out[
    sapply(dribble_out$drive_resource, "[[", "mimeType") == "application/vnd.google-apps.folder",
  ]
  if( nrow(dribble_out) == 0 ){
    stop("The specified gdrive_path or folder_id does not direct to a folder!")
  } else if( nrow(dribble_out) > 1 ){
    stop({
      cat(paste0(
        "Path name ", crayon::yellow(gdrive_path), " is not specific enough. ", nrow(dribble_out), " matches found.\n"
      ))
    })
  } else if ( nrow(dribble_out) == 1 ){
    dribble_out$shared_drive_id <- id
    dribble_out
  }
}
