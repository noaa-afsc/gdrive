#' For gdrive_ functions: check for googledrive token and if not active, use NOAA e-mail
#'
#' \code{drive_token()} is used by all \code{gdrive_} functions to ensure the user has authenticated their token to
#' communicate with the googledrive. Checks to see if the user is on a Google Cloud Workstation and helps the user
#' set up their token if one is not found.
#'
#' @return Automatically uses the user's stored NOAA e-mail to connect if no token is already established.
#' @keywords internal
gdrive_token <- function() {
  # To deauthorize in testing: googledrive::drive_deauth()
  
  if(isFALSE(googledrive::drive_has_token())) {
    
    # Setup for Google Cloud Workstation
    home_dir <- "/home/user"
    secrets_dir <- file.path(home_dir, ".secrets")
    gargle_dir <- file.path(secrets_dir, "gargle")
    # Determine whether we are in cloud environment or not
    is_cloud <- Sys.info()[["sysname"]] == "Linux" && dir.exists(home_dir)
    
    if(is_cloud) {
      
      # Set-up instructions for adding your token
      token_setup_msg <- paste0(
        "\033[1mPlease upload your token from your non-cloud local machine to your cloud's secrets folder.\033[22m\n\n",
        "1: \033[1mOn your local machine\033[22m, run:     ", crayon::green("gargle:::gargle_oauth_sitrep()"), "\n",
        "   to locate your most recently-created token ending in '@noaa.gov'\n",
        "   Just take note of where this is, as you're going to navigate there in a moment.\n\n",
        "2: \033[1mOn your cloud instance\033[22m, navigate to your 'files' pane ", "\u2192 More \u2192 and enable 'Show Hidden Files'\n",
        "   Click on 'Home' and navigate to your newly-created `.secrets/gargle` folder.\n",
        "   Click `Upload`, then 'Choose File' and navigate to your token from your local machine.\n\n"
      )
      
      # If on the cloud workstation, see if a token already exists
      if (dir.exists(gargle_dir)) {
        # the secrets directory exists, look for a token
        
        token_file <- list.files(gargle_dir, pattern = "*noaa.gov")[1]
        
        # If a token exists, use it
        if(!is.na(token_file)) {
          token <- readRDS(paste0(gargle_dir, "/", token_file))
          token$cache_path <- NULL
          googledrive::drive_auth(token = token)
          if(is.null(googledrive::drive_user())) {
            stop("A token was found but it didn't work. Double-check your setup!")
          } else {
            cli::cli_inform(paste0(
              "i" = "The {.pkg googledrive} package is using a provided token for {.email {token$email}}."
            ))
            return(invisible(TRUE))
          }
        } else {
          # If the .secrets/gargle/ folder exists but there is no token, ask the user to add them 
          # Instruct the user for how to manually add their token to their cloud instance
          cat(token_setup_msg)
          return(invisible(FALSE))
        }
        
      } else {
        
        # first see if a .secrets folder exists and if not, create it.
        if(!dir.exists(secrets_dir) | !dir.exists(gargle_dir)) {
          # If the secrets directly doesn't exist, create it and the gargle subfolder
          dir.create(gargle_dir, recursive = TRUE)
          # chmod 700: rwx for user, nothing for anyone else
          system(paste("chmod 700", secrets_dir))
          message("Secrets folder created at: ",  file.path("/.secrets", "gargle"))
          # See if this refreshes the files pane to show the newly created /.secrets folder
          rstudioapi::filesPaneNavigate(home_dir)
        } 
        
        # Instruct the user for how to manually add their token to their cloud instance
        cat(token_setup_msg)
        return(invisible(FALSE))
      }
    } else {
      # If NOT on the cloud workstation, use locally saved token
      googledrive::drive_auth(email = "*@noaa.gov")
      return(invisble(TRUE))
    }
  } else return(invisible(TRUE))
}


#' Specify a Google Shared Drive
#'
#' @param shared_id Either an alias for a shared_id stored in your .Renviron file or the drive_id of Google Shared Drive
#' that you have access to.
#'
#' @details
#' By default, the shared_id argument of the gdrive_set_dribble() and gdrive_dir() functions check your .Renviron file
#' for a value named `default_gdrive_id` that contains the drive_id of a Google Shared Drive. 
#'
#' @return Returns the drive_id of a shared google drive
#' @keywords internal
gdrive_get_shared_id <- function(shared_id) {

  if(shared_id == "default_gdrive_id") {
    shared_id.pull <- Sys.getenv("default_gdrive_id")
    if(nchar(shared_id.pull) == 0) {
      cat("You have not yet specified a Google Shared Drive as your default.\n\n")
      cat(paste0(
        "Run ", crayon::green("usethis::edit_r_environ()"), " to add your default shared drive ID using the syntax:\n",
        "\033[1m  default_gdrive_id = \"<drive_id>\"\033[22m\n\n",
        "The \033[1m<drive_id>\033[22m can be pasted from the shared drive's URL:\n",
        "   https://drive.google.com/drive/folders/\033[1m<drive_id>\033[22m\n\n",
        "Then, save your .Renviron file and restart your session [Ctrl + Shift + F10]\n\n" 
      ))
      stop("Set up a 'default_gdrive_id' in your .Renviron file.")
    } else {
      return(shared_id.pull)
    }
  } else {
    # If it's not the `default_drive_id`, see if it either matches another alias in the .Renviron file
    shared_id.pull <- Sys.getenv(shared_id)
    if(nchar(shared_id.pull) == 0) {
      # If it doesn't match an alias, either it was not specified in .Renviron or OR it is already a shared_id!

      # If the formatting is correct for a shared drive ID, let it pass
      if(grepl("^0A[a-zA-Z0-9_-]{17}$", shared_id)) {
        return(shared_id)
      } else {
        cat(paste0("The alias '", shared_id,  "' is not specified in your .Renviron file.\n"))
        cat(paste0(
          "Run ", crayon::green("usethis::edit_r_environ()"), " to add your default shared drive ID using the syntax:\n",
          "\033[1m", shared_id, " = \"<drive_id>\"\033[22m\n\n",
          "The \033[1m<drive_id>\033[22m can be pasted from the shared drive's URL:\n",
          "   https://drive.google.com/drive/folders/\033[1m<drive_id>\033[22m\n\n",
          "Then, save your .Renviron file and restart your session [Ctrl + Shift + F10]\n\n" 
        ))
      }
      return()
    } else {
      return(shared_id)
    }
  }
}




#' \code{shared_dribe_ls()} is used in place of \code{googledrive::drive_ls()} which does not work for users with only
#' shared access to a subfolder of the shared drive and not root access.
#'
#' @param gdrive_dribble a \code{dribble} containing the contents of a Shared Gdrive folder
#'
#' @return Returns a dribble of all items within the specified folder.
#' @keywords internal
shared_drive_ls <- function(gdrive_dribble) {
  
  # Make a custom API request
  query <- glue::glue("'{gdrive_dribble$id}' in parents and trashed = false")
  req <- googledrive::request_generate(
    endpoint = "drive.files.list",
    params = list(
      q = query,
      supportsAllDrives = TRUE,
      includeItemsFromAllDrives = TRUE,
      fields = "files(id, name, kind, mimeType, createdTime, size)",
      corpora = "drive",
      orderBy = "recency desc",
      driveId = gdrive_dribble$shared_drive_id
    )
  )
  # Make the request and convert to dribble class
  googledrive::with_drive_quiet(
    googledrive::do_paginated_request(req) |>
      purrr::map("files") |> purrr::flatten() |>
      googledrive::as_dribble()
  )
}

#' \code{dir_search()} is used recursively by \code{gdrive_ls()} to identify the folder structure of the Shared Google Drive
#'
#' @param dribble a \code{dribble} containing the contents of a Shared Gdrive folder
#'
#' @return Returns a list of two dribbles, the first is the parent folder and the second is the children folders
#' @keywords internal
dir_search <- function(dribble) {
  
  dribble_lst <- vector(mode = "list", length = nrow(dribble))
  
  # Keep all parent folders
  parent <- dribble
  # Initialize count of files
  parent$files <- 0
  
  for( i in 1:length(dribble_lst) ) {
    
    drive_items <- shared_drive_ls(dribble[i, ])
    
    if( nrow(drive_items) == 0 ) {
      dribble_lst[[i]] <- list(
        parent = parent[i,],
        child = parent[0, c("name", "id", "drive_resource")]
      )
    } else {
      
      x1 <- drive_items
      # check which items are folders
      x1_check <- sapply(x1$drive_resource, function(x) x$mimeType == "application/vnd.google-apps.folder")
      parent[i,]$files <- sum(!x1_check)
      
      # Make new child folder names and their ids
      child <- x1[x1_check, ]
      
      if( nrow(child) != 0 ){
        for(j in 1:nrow(child)) {
          child[j, "name"] <- paste0(parent[i, ]$name, "/", child[j, ]$name)
        }
      }
      
      dribble_lst[[i]] <- list(
        parent = parent[i,],
        child = child
      )
    }
  }
  
  list(
    parent = do.call(rbind, lapply(dribble_lst, "[[", "parent")),
    child = do.call(rbind, lapply(dribble_lst, "[[", "child"))
  )
  
}


#' Split a local path into the directory, filename, and extension
#'
#' A background helper function for `gdrive_download()` and `gdrive_upload()` to identify the local files to interact with.
#'
#' @param local_path a character string of the local file path to the `.rdata` or `.rds` file to either upload or download.
#'
#' @return Returns a list of the parsed path as well as logical checks for whether the path was found or contains a version suffix.
#' @keywords internal
parse_local_path <- function(local_path){
  
  # Error checks
  # local_path must be a length 1 character string
  if( length(local_path) != 1 | !is.character(local_path) ) stop("`local_path` must a length = 1 character string.")
  # Can a file be found at the specified local path? If not, return
  if( !utils::file_test(op = "-f",  local_path) ) local_exists <- F else local_exists <- T
  
  # Get the name of the file (remove the directory, anything left if the final "/")
  name <- basename(local_path)
  
  # Identify the extension and exclude it from the file name
  extension <- paste0(".", tools::file_ext(name))
  if( extension == ".") stop(paste0("'local_path' needs to have the file extension specified."))
  name_no_ext <- sub(extension, "", name)
  
  # Determine whether a version suffix is present in the file name
  ver_flag <- grepl("(.+)(?=_v[0-9]{3}[.])", name, perl = T)
  
  # Get the directory, if present
  directory <- sub(name, "", local_path)
  
  # Return the parsed path
  list(
    path = local_path, name = name, directory = directory, name_no_ext = name_no_ext,
    extension = extension, ver_flag = ver_flag, local_exists = local_exists
  )
}


#' Find a File in the Gdrive Folder
#'
#' A background helper function for `gdrive_download()` and `gdrive_upload()` to identify the Gdrive files to interact with.
#'
#' @param gdrive_dribble the 1-row `dribble` class objet identifying the Gdrive folder to upload to or download from
#' @param l_path the output of `parse_local_path(local_path)`, which identifies the name of the file to search for within the `dribble`.
#'
#' @return Returns a list of `dribble` for the Gdrive file, a list of the file's revision history, and the file's current version number.
#' @importFrom crayon bold
#' @keywords internal
parse_dribble <- function(gdrive_dribble, l_path) {
  
  # Can the Gdrive folder be found?
  if( !googledrive::is_dribble(gdrive_dribble) ) {
    stop(paste0(
      "'gdrive_dribble' must be specified as a dribble. Use ", crayon::bold("gdrive_set_dribble()"), " to do this."
    ))
  }
  if( nrow(gdrive_dribble) != 1 ) stop("'gdrive_dribble' must be 1 row.")
  
  # use drive_get to see if the file already exists on the gdrive, and store it as a dribble. We don't want to use
  # case-sensitivity with these checks. Could also use drive_get using path (ignores case sensitivity), but is slower.
  gdrive_folder_items <- shared_drive_ls(gdrive_dribble)
  gdrive_item <- gdrive_folder_items[which(tolower(gdrive_folder_items$name) == tolower(l_path$name)), ]
  
  # If nrow(gdrive_item) > 0, then check to see if the file has a version history
  if( nrow(gdrive_item) > 1 ){
    # If more than one files are matched, big problem!
    stop(paste0("More than one file in the specified Gdrive folder has the name: ", crayon::bold(l_path$name), " ! Need to delete one!"))
  } else if ( nrow(gdrive_item) == 1 ) {
    # If the file already exists, check how many versions (revisions) exist and get info for each
    revision_lst <- googledrive::do_request(
      googledrive::request_generate(
        endpoint = "drive.revisions.list", params = list(fileId = gdrive_item$id, fields = "*")
      )
    )$revisions
    
    # Format the modified datetimes
    rev_mtime_vec <- sapply(revision_lst, "[[", "modifiedTime")
    rev_mtime_vec <- as.POSIXct(sapply(rev_mtime_vec, function(x) {
      format(
        as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "GMT" , origin = "1970-01-01"),
        tz = Sys.timezone(), usetz = T
      )
    }, USE.NAMES = F))
    for(i in seq_along(revision_lst)) revision_lst[[i]]$modifiedTime <- rev_mtime_vec[i]
    # Check to make sure revisions are listed in order of modifiedTime, the most recent first and oldest last!
    if( any(diff(rev_mtime_vec) < 0) ) {
      stop(paste0(
        "parse_dribble(): ", crayon::bold(l_path$name), "'s revision_lst is not ordered by modifiedTime! Fix this!"
      ))
    }
    # Check to make sure keepForever is checked for all versions
    if( any(sapply(revision_lst, "[[", "keepForever") != T) ){
      warning(cat(paste0(
        "Versions ", paste0(which(sapply(revision_lst, "[[", "keepForever") != T), collapse = ", "),
        "have keepForever = FALSE!\n"
      )))
    }
    
  } else if ( nrow(gdrive_item) == 0 ){
    # If no files are found, set modified times to null
    revision_lst <- NULL
  }
  
  # Prepare outputs
  list(
    gdrive_item = gdrive_item,
    revision_lst = revision_lst,
    current_ver = length(revision_lst)
  )
}


#' Compare the Local File and the Current Version of the Gdrive File
#'
#' A background helper function for `gdrive_upload()` and `gdrive_download()` that helps determine whether those
#' operations are necessary (e.g., skips these actions if the files are already identical).
#'
#' @param l_path the output of `parse_local_path()`, identifying the local file.
#' @param g_path the output of `parse_dribble()`, identifiying the Gdrive file
#'
#' @return Returns a list of checks, including whether the local and Gdrive files were identical, have the same size or
#' modification times, and if needed, the raw bytes of the Gdrive file.
#' @importFrom crayon bold
#' @importFrom crayon red
#' @importFrom crayon yellow
#' @keywords internal
compare_local_and_gdrive <- function(l_path, g_path){
  
  # Get information of local file
  local_info <- file.info(l_path$path)
  
  # Get the most recent gdrive version
  gdrive_head <- g_path$revision_lst[[g_path$current_ver]]
  # compare file size
  size_match <- gdrive_head$size == file.size(l_path$path)
  # compare modified time (gdrive - local)
  mtime_match <- (gdrive_head$modifiedTime - trunc(local_info$mtime))
  
  # Compare files
  if( size_match & mtime_match == 0 ){
    # If modified times and byte lengths are the same, treat them as identical
    identical <- T
    local_status <- "up to date with"
    gdrive_raw <- NULL
  } else if( size_match ){
    # If the sizes match, compare the bytes. This may be time consuming for large files.
    local_raw <- readBin(l_path$path, what = "raw", n = local_info$size)
    gdrive_raw <- gargle::request_make(gargle::request_build(
      method = "GET",
      path = "drive/v3/files/{fileId}",
      params = list(
        fileId = g_path$gdrive_item$id, supportsAllDrives = TRUE, alt = "media"
      ),
      token = googledrive::drive_token()
    ))
    
    # If the bytes are the same, we know the files are identical
    if( all(gdrive_raw$content == local_raw) ) {
      identical <- T
      local_status <- "up to date with"
    } else {
      identical <- F
      local_status <- ifelse(mtime_match > 0, "behind", "ahead of")
    }
  } else {
    # If the files aren't identical, declare whether the local or the gdrive is ahead
    identical <- F
    local_status <- ifelse(mtime_match > 0, "behind", "ahead of")
    gdrive_raw <- NULL
    
    # Print the modified dates of the local and gdrive versions
    cat(paste0(
      "The `Date modified` of ", crayon::bold(l_path$name), ":\n- Local:  ", round(local_info$mtime),
      "\n- Gdrive: ", gdrive_head$modifiedTime, "\n"
    ))
  }
  
  # If the local is behind, check to see if the local_mtime matches any prior gdrive versions
  if( local_status == "behind" ){
    local_match_ver <- (sapply(g_path$revision_lst, "[[", "modifiedTime") == trunc(local_info$mtime))
    # If there is a match, print the version
    if( any(local_match_ver) ){
      cat(paste0(
        "Local copy of ", crayon::bold(l_path$name), " appears to be on ",
        crayon::yellow(paste0("[ver", which(local_match_ver), "]")),
        " whereas the Gdrive is on ", crayon::yellow(paste0("[ver", g_path$current_ver, "]")), ".\n"
      ))
    }
  }
  
  # Check to see if the file size is identical to an older version
  if ( length(g_path$revision_lst) > 1) {
    local_match_ver <- which(
      as.numeric(sapply(g_path$revision_lst[-g_path$current_ver], "[[", "size")) == trunc(local_info$size)
    )
    if( length(local_match_ver) ) {
      # If the file size matches, compare the bytes
      
      local_raw <- readBin(l_path$path, what = "raw", n = local_info$size)
      
      bytes.lst <- lapply(local_match_ver, function(x) {
        gargle::request_make(gargle::request_build(
          method = "GET",
          path = "drive/v3/files/{fileId}/revisions/{revisionId}",
          params = list(
            fileId = g_path$gdrive_item$id, revisionId = g_path$revision_lst[[x]]$id,
            supportsAllDrives = TRUE, alt = "media"
          ),
          token = googledrive::drive_token()
        ))$content
      })
      bytes.match <- sapply(bytes.lst, function(x) identical(local_raw,  x))
      # If there is a match of bytes, print a warning in the console
      if( any(bytes.match) ){
        cat(paste0(
          crayon::red("!!! Warning !!!  "), " Local copy of ", crayon::bold(l_path$name), " is identical to the old ",
          crayon::yellow(paste0("[ver", max(local_match_ver[bytes.match]), "]")),
          " whereas the Gdrive is on ", crayon::yellow(paste0("[ver", g_path$current_ver, "]")), ".\n"
        ))
      }
    }
  }
  
  # Outputs
  list(
    identical = identical,
    mtime_match = mtime_match,
    local_status = local_status,
    gdrive_bytes = gdrive_raw$content
  )
}

