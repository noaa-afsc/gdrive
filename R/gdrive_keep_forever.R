#' Set all versions of a file's keepForever option to TRUE
#'
#' If a file on the drive is ever manually uploaded (instead of using `gdrive_upload()`), it's `keepForever` option 
#' will not be set to TRUE automatically. This will cause checks in the package to throw warnings. This function will
#' scan through all versions of a file where keepForever is FALSE and set them to TRUE.
#' 
#' @param local_path the local path to the file you wish to download, including the directory (folder paths) and the name of the file, which should match the name of the file on the Gdrive.
#' @param gdrive_dribble the dribble class object representing the folder in the Gdrive where your desired file resides.
#' @param ...	not used, but allows you to easily replace calls to gdrive_versions without removing additional args.
#'
#' @return Returns a message whether updates to `keepForever` were successful.
#'
#' @export
gdrive_keep_forever <- function(local_path, gdrive_dribble, ...) {
  
  # Ensure googledrive token is active
  if(!gdrive_token()) return(invisible())
  
  # This function takes the local path and removes any directories preceding the file name.
  local_path <- basename(local_path)
  
  # Make the dribble of the local_path
  # Escaping single quotes in case your file name has them (e.g., "User's Data")
  clean_name <- gsub("'", "\\\\'", local_path)
  # Build a case-insensitive query string. 'name = ...' in the Google Drive API v3 is case-INsensitive by default!
  query_str <- paste0("name = '", clean_name, "' and '", gdrive_dribble$id, "' in parents and trashed = false")
  # Query the API directly (blazing fast)
  gdrive_item <- googledrive::with_drive_quiet(
    googledrive::drive_find(
      q = query_str,
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
  rev_info <- lapply(revision_lst, "[", c("id", "modifiedTime", "size", "keepForever"))
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
  
  # Identify which versions to update
  keep_forever_fix_vec <- which(sapply(rev_info, "[[", "keepForever") == FALSE)
  
  # If all versions have keepForever = TRUE, return
  if(length(keep_forever_fix_vec) == 0 ) {
    cat(paste0("All ", length(rev_info), " versions already have keepForever set to TRUE."))
    return()
  }
  
  # Loop through any versions where keepForever = FALSE and update them.
  for(i in keep_forever_fix_vec) {
    
    req_update <- googledrive::request_generate(
      endpoint = "drive.revisions.update",
      params = list(
        fileId = gdrive_item$id,
        revisionId = rev_info[[i]]$id,
        keepForever = TRUE
      )
    )
    res_update <- googledrive::request_make(req_update)
    if (res_update$status_code == 200) {
      cat(paste0("Version ", i, "now has keepForever set to TRUE.\n"))
    } else {
      cat(paste0("FAILURE!!! Version ", i, "could not have keepForever to set to TRUE!.\n"))
    }
  }
  
}