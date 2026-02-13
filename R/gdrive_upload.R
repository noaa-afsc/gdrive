#' Upload a File to the Google Shared Drive
#'
#' `gdrive_upload()` is used to either upload a new file to the specified Gdrive folder, or, if the file already exists,
#' updates the file by creating a new version. In the background, it assigns the modified dates of the local file to the
#' gdrive file/version, ensures older versions are kept forever, and checks to see whether uploading a new version is
#' necessary. Currently only works with `.rdata` and `.rds` type files. Nearly all other files we use should be
#' maintained the project's GitHub repository.
#'
#' @param local_path the file path to the local file that you wish to upload or update.
#' @param gdrive_dribble the `dribble` object of the Gdrive folder you wish to upload to, created by `gdrive_set_dribble`.
#' @export
#'
#' @return Returns messages regarding whether the upload was completed, and if so, the version number.
#'
#' @examples
#' \dontrun{
#'   gdrive_upload(local_path, gdrive_dribble)
#' }
gdrive_upload <- function(local_path, gdrive_dribble) {

  # Ensure googledrive token is active
  if(!gdrive_token()) return(invisible())

  # Parse the local path to get the directory, file name, extension, and whether a version flag exists
  l_path <- parse_local_path(local_path)
  # If the local path doesn't exist, abort!
  if( !l_path$local_exists ) stop(paste0(crayon::cyan(local_path), " cannot be found."))
  # Prevent uploading files with a version suffix.
  if( l_path$ver_flag ) stop("Do not upload files with a version suffix!")
  # Parse the path to the google drive, looking for the files specified in local_path
  g_path <- parse_dribble(gdrive_dribble, l_path)

  # Get the modify time of the local file
  local_mtime <- file.info(local_path)$mtime
  # convert this to the format Google wants
  new_mtime <- paste0(sub("(?<=[0-9])( )(?=[0-9])", "T", format(local_mtime, tz = "GMT"), perl = T), ".000Z")

  # Either upload or update the file
  if( nrow(g_path$gdrive_item) == 0 ) {
    # Upload File - If the file does not yet exist on the gdrive, upload the file to the gdrive
    cat(paste0(
      crayon::cyan(local_path), " will be uploaded to ", crayon::yellow(gdrive_dribble$path),
      " as ", crayon::yellow("[ver1]"), ".\n"
    ))
    upload_response <- rstudioapi::showPrompt(
      title = "Notice!",
      message = "Proceed with initial upload? (Y/N)"
    )
    if( is.null(upload_response) ) upload_response <- "N"
    upload_response <- toupper(upload_response)
    if( upload_response == "Y" ){
      # Upload the file. Make the modifiedTime match that of the local file
      googledrive::drive_upload(
        media = local_path, path = gdrive_dribble, overwrite = FALSE,
        modifiedTime = new_mtime, keepRevisionForever = TRUE
      )
    } else if ( upload_response == "N" ){
      return(cat("Aborting upload."))
    } else {
      stop("Aborting upload. Response was neither 'Y' or 'N'.")
    }
  } else {
    # Update File - If the file already exists on the gdrive, and the local is ahead, update the file

    # Compare the file sizes and modified dates of the local file and the head gdrive file
    compare_res <- compare_local_and_gdrive(l_path, g_path)

    # Decide whether to update the existing file
    if( compare_res$identical ) {
      # *If the files are identical, don't bother updating!*
      return(cat(paste0(
        "Local copy of ", crayon::bold(l_path$name), " is ", crayon::green(compare_res$local_status), " the Gdrive on ",
        crayon::yellow(paste0("[ver", g_path$current_ver, "]")), ". Skipping upload.\n"
      )))
    } else {

      if( compare_res$local_status == "behind" ){
        # * If local is behind, prompt the user to check before proceeding*
        local_behind_response <- rstudioapi::showPrompt(
          title = "Notice!",
          message = paste0(
            "!!! WARNING !!! Local copy of ", l_path$name,
            " appears to be BEHIND the Gdrive. Are you sure you want to continue with the upload? (Y/N)"
          )
        )
        if( is.null(local_behind_response) ) local_behind_response <- "N"
        local_behind_response <- toupper(local_behind_response)
        if( local_behind_response == "N" ){
          return(cat("Aborting upload."))
        } else if( local_behind_response != "Y" ){
          stop("Aborting upload. Response was neither 'Y' or 'N'.")
        }
      }

      # Local must be ahead of the gdrive.
      cat(paste0("Local is ", crayon::bold("ahead of")), paste0("the gdrive: [ver", g_path$current_ver, "]\n"))
      # Prepare to update
      cat(paste0(
        crayon::bold(l_path$name), " in ", crayon::yellow(gdrive_dribble$path), " will be updated to ",
        crayon::yellow(paste0("[ver", g_path$current_ver + 1,  "]")), ".\n"
      ))
      update_response <- rstudioapi::showPrompt(
        title = "Notice!",
        message = "Proceed with upload and update? (Y/N)"
      )
      if( is.null(update_response) ) update_response <- "N"
      update_response <- toupper(update_response)
      if ( update_response == "N"){
        return(cat("Aborting upload."))
      } else if ( update_response != "Y") {
        stop("Aborting upload. Response was neither 'Y' or 'N'.")
      } else if( update_response == "Y" ){
        # Perform the update, assigning the modified time of the file
        googledrive::drive_update(
          file = g_path$gdrive_item, media = local_path, modifiedTime = new_mtime, keepRevisionForever = TRUE
        )
      }
    }
  }
}
