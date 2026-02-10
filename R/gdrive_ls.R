#' Show All Files in a Dribble
#'
#' A reference function to print the names and metadata of files contained in a dribble.
#'
#' @param gdrive_dribble a 1-row dribble class object created by `gdrive_set_dribble()`
#'
#' @details If you do not know the folder path to the folder you want to interact with, run `gdrive_dir()` to get
#' the view the folder structure of the Share Google Drive and copy-paste the `path`
#'
#' @return Prints a data.frame of the files contained in the specified Gdrive folder.
#'
#'#' @seealso `gdrive_set_dribble()`
#'
#' @examples
#' \dontrun{
#'    gdrive_ls(gdrive_set_dribble(gdrive_path = 'Google Drive Test/'))
#' }
#'
#' @export
gdrive_ls <- function(gdrive_dribble){

  if( !googledrive::is_dribble(gdrive_dribble) | nrow(gdrive_dribble) != 1) {
    stop("'gdrive_dibble' needs to be a nrow = 1 dribble.")
  }

  # Ensure googledrive token is active
  gdrive_token()

  # Get all items in gdrive_dibble
  # dribble_items <- googledrive::drive_ls(gdrive_dribble)  # doesn't work for collaborators with shared access
  dribble_items <- shared_drive_ls(gdrive_dribble)

  # Subset to only files
  dribble_items <- dribble_items[
    sapply(dribble_items$drive_resource, "[[", "mimeType") != "application/vnd.google-apps.folder",
  ]

  if( nrow(dribble_items) == 0 ){
    cat(paste0("No files exist in ", crayon::yellow(gdrive_dribble$path), ".\n"))
  } else {
    # Include the create dates in the output
    create_dates <- create_dates <- as.POSIXlt(
      sapply(dribble_items$drive_resource, "[[", "createdTime"),
      format = "%Y-%m-%dT%H:%M:%OS", tz = "GMT")
    # Convert create dates to local timezone
    dribble_items$create_date <- as.POSIXct(format(as.POSIXct(create_dates), tz = Sys.timezone(), usetz = T))
    # Draw the file sizes from the drive_resources
    dribble_items$size <- sapply(
      as.integer(sapply(dribble_items$drive_resource, "[[", "size")),
      function(x) format(structure(x, class = "object_size"), units = "auto")
    )

    # Return a data.frame without drive_resources so this object can't be abused as a dribble
    dribble_items$file_name <- dribble_items$name
    as.data.frame(
      dribble_items[order(dribble_items$create_date, decreasing = T), c("file_name", "create_date", "size")]
    )
  }
}
