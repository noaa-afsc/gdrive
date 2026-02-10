#' View the Revision History of a Data File on the Gdrive
#'
#' @param local_path the name of the .rdata or .rds file on the Gdrive
#' @param gdrive_dribble the `dribble` class object of the folder where your requested file resides.
#'
#' @return Returns a data.frame showing the file's revision history, including modification dates and users and file size
#' @export
#'
#' @examples
#' \dontrun{
#'   gdrive_versions(
#'      "geoff_mayhew_tutorial.rdata",
#'      gdrive_set_dribble("Google Drive Test/")
#'   )
#' }
gdrive_versions <- function(local_path, gdrive_dribble){

  # Ensure googledrive token is active
  gdrive_token()

  # This function takes the local path and removes any directories preceding the file name.
  local_path <- basename(local_path)

  # Make the dribble of the local_path
  gdrive_item <- googledrive::with_drive_quiet(
    googledrive::drive_get(
      path = paste0(gdrive_dribble$path, local_path),
      shared_drive = googledrive::as_id(gdrive_dribble$shared_drive_id)
    )
  )

  if( nrow(gdrive_item) == 0 ) {
    stop(paste0(
      "No file named ", crayon::yellow(local_path), " found in gdrive folder ",
      crayon::yellow(gdrive_dribble$path), "."
    ))
  }

  # Get the revision list
  revision_lst <- googledrive::do_request(
    googledrive::request_generate(
      endpoint = "drive.revisions.list", params = list(fileId = gdrive_item$id, fields = "*")
    )
  )$revisions

  # Subset the information of the versions to useful information
  rev_info <- lapply(revision_lst, "[", c("modifiedTime", "size", "keepForever"))
  rev_user <- lapply(lapply(revision_lst, "[[", "lastModifyingUser"), "[[", "displayName")
  rev_info <- mapply(function(x, y) append(x, c(modifiedBy = y)), x = rev_info, y = rev_user, SIMPLIFY = F)

  # Format the modified dates and file sizes
  for(i in seq_along(rev_info)) {
    rev_info[[i]]$modifiedTime <- format(
      as.POSIXct(rev_info[[i]]$modifiedTime , format = "%Y-%m-%dT%H:%M:%OSZ", tz = "GMT" , origin = "1970-01-01"),
      tz = Sys.timezone(), usetz = T
    )
    rev_info[[i]]$size <- sapply(
      as.numeric(rev_info[[i]]$size),
      function(x) format(structure(x, class = "object_size"), units = "auto")
    )
  }

  # Convert to data.frame
  rev_info_df <- do.call(rbind, lapply(rev_info , as.data.frame))
  rev_info_df$Version <- seq_along(rev_info)
  rev_info_df <- rev_info_df[, c("Version", "modifiedTime", "modifiedBy", "size", "keepForever")]

  # Outputs
  cat(crayon::yellow(paste0(gdrive_dribble$path, local_path)), "\n")
  rev_info_df[rev(seq_along(rev_info)), ]

}
