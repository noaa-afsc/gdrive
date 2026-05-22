#' Show the Folder Structure of the Shared Google Drive
#'
#' Prints the structure of the Shared Google Drive to the console. It provides a count of the files within
#' each folder and all subfolders. Items in the 'path' column can be copy-pasted to use as the
#' `gdrive_path` argument of the `gdrive_set_dribble(gdrive_path)` function. Highly recommended to use the `folder`
#' argument as it will speed up your search. Note that this function only works for shared drives in which you have
#' member-level access, not just folder-level access.
#'
#' @param folder A dribble or a character file path of folders, i.e., "Projects", that will narrow down your search.
#' @param shared_id An alias of a Shared Google Drive. By default, searches for the `default_gdrive_id` variable in your
#' .Renviron file.
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
  
  if (!is.character(shared_id) | length(shared_id) != 1) {
    stop("'id' needs to be a length = 1 character string.")
  }
  
  # Ensure googledrive token is active
  if (!gdrive_token()) {return(invisible())}
  
  # Check the address of the shared_id
  id <- gdrive_get_shared_id(shared_id)
  
  tryCatch(
    gdrive_head <- googledrive::with_drive_quiet(googledrive::shared_drive_get(id = id)),
    error = function(e) {
      stop("Either your shared_id was incorrect or you do not have root access to the shared drive.")
    }
  )
  
  # Set up parameters for API call to fetch ALL folders globally
  api_params <- list(
    q = "mimeType = 'application/vnd.google-apps.folder' and trashed = false",
    supportsAllDrives = TRUE,
    includeItemsFromAllDrives = TRUE,
    corpora = "drive",
    driveId = id,
    pageSize = 1000,
    fields = "files(id, name, parents)"
  )
  
  req_folders <- googledrive::request_generate(
    endpoint = "drive.files.list",
    params = api_params
  )
  all_folders_raw <- googledrive::with_drive_quiet(googledrive::do_paginated_request(req_folders))
  all_folders_list <- do.call(c, lapply(all_folders_raw, function(x) x$files))
  
  if (length(all_folders_list) == 0) {
    cat("Shared Drive: ", crayon::yellow(gdrive_head$name), "\n", sep = "")
    return(invisible())
  }
  
  all_folders <- data.frame(
    id = sapply(all_folders_list, function(f) f$id),
    name = sapply(all_folders_list, function(f) f$name),
    parent_id = sapply(all_folders_list, function(f) {
      if (is.null(f$parents) || length(f$parents) == 0) return(NA_character_)
      return(f$parents[[1]])
    }),
    stringsAsFactors = FALSE
  )
  
  # Resolve the target_root_id LOCALLY inside R memory instead of using drive_get()
  if (!is.null(folder)) {
    if (googledrive::is_dribble(folder)) {
      target_root_id <- folder$id
      folder_name <- folder$name
    } else {
      clean_folder <- sub("[.]*/$", "", folder)
      path_parts <- unlist(strsplit(clean_folder, "/"))
      folder_name <- path_parts[length(path_parts)]
      
      current_parent <- id
      matched_id <- NULL
      
      for (part in path_parts) {
        match_idx <- which(all_folders$name == part & all_folders$parent_id == current_parent)
        if (length(match_idx) == 0) {
          stop(paste0("Could not find folder path: '", folder, "' inside this Shared Drive."))
        }
        current_parent <- all_folders$id[match_idx[1]]
        matched_id <- current_parent
      }
      target_root_id <- matched_id
    }
  } else {
    target_root_id <- id
    folder_name <- ""
  }
  
  # Format paths locally across our folder collection
  get_folder_path <- function(start_id, folder_df, root_boundary_id) {
    path_segments <- c()
    current_id <- start_id
    
    while (!is.na(current_id) && current_id != root_boundary_id) {
      match_idx <- which(folder_df$id == current_id)
      if (length(match_idx) == 0) {
        break
      }
      path_segments <- c(folder_df$name[match_idx], path_segments)
      current_id <- folder_df$parent_id[match_idx]
    }
    
    if (length(path_segments) == 0) return("")
    paste(path_segments, collapse = "/")
  }
  
  all_folders$full_path <- sapply(
    all_folders$id, 
    get_folder_path, 
    folder_df = all_folders, 
    root_boundary_id = id
  )
  
  if (!is.null(folder)) {
    is_downstream <- sapply(all_folders$id, function(fid) {
      curr <- fid
      while(!is.na(curr)) {
        if (curr == target_root_id) return(TRUE)
        idx <- which(all_folders$id == curr)
        if (length(idx) == 0) break
        curr <- all_folders$parent_id[idx]
      }
      return(FALSE)
    })
    out <- all_folders[is_downstream & all_folders$id != target_root_id, ]
  } else {
    out <- all_folders[all_folders$full_path != "", ]
  }
  
  if (nrow(out) == 0) {
    target_full_path <- if (folder_name != "") all_folders$full_path[all_folders$id == target_root_id][1] else ""
    cat("Shared Drive: ", crayon::yellow(gdrive_head$name), "\n", sep = "")
    if (!is.null(folder)) {
      cat("Folder Target: ", crayon::yellow(paste0("/", target_full_path)), "\n", sep = "")
    }
    cat("No subfolders found.\n")
    return(invisible())
  }
  
  # Targeted file query using chunks of our isolated target subfolders
  valid_parent_ids <- c(target_root_id, out$id)
  chunk_size <- 40
  id_chunks <- split(valid_parent_ids, ceiling(seq_along(valid_parent_ids) / chunk_size))
  
  all_files_raw <- data.frame()
  
  for (chunk in id_chunks) {
    parent_clause <- paste0("'", chunk, "' in parents", collapse = " or ")
    chunk_query <- paste0("(", parent_clause, ") and mimeType != 'application/vnd.google-apps.folder' and trashed = false")
    
    chunk_files <- googledrive::with_drive_quiet(
      googledrive::drive_find(
        shared_drive = gdrive_head,
        q = chunk_query
      )
    )
    if (nrow(chunk_files) > 0) {
      all_files_raw <- rbind(all_files_raw, chunk_files)
    }
  }
  
  # Process and map counts locally
  if (nrow(all_files_raw) > 0) {
    all_files_revealed <- googledrive::drive_reveal(all_files_raw, "parent")
    file_counts <- table(all_files_revealed$id_parent)
    out$files <- as.numeric(file_counts[out$id])
  } else {
    out$files <- 0
  }
  out$files[is.na(out$files)] <- 0
  
  out$gdrive_path <- paste0(out$full_path, "/")
  out <- out[order(out$gdrive_path), ]
  
  # Calculate relative paths for display purposes
  if (folder_name != "") {
    target_full_path <- all_folders$full_path[all_folders$id == target_root_id][1]
    path_for_abbr <- gsub(paste0("^", target_full_path, "/"), "", out$gdrive_path)
  } else {
    path_for_abbr <- out$gdrive_path
  }
  
  # Compute depths based on structural slash counts
  path_split <- strsplit(sub("/$", "", path_for_abbr), "/")
  depths <- sapply(path_split, length)
  names_only <- sapply(path_split, function(x) x[length(x)])
  
  # Interlocking Tree Render Engine (With ASCII-compliant Unicode Escapes)
  n <- nrow(out)
  tree_strings <- character(n)
  
  if (n > 0) {
    for (i in seq_len(n)) {
      d <- depths[i]
      
      if (d == 1) {
        has_later_sibling <- any(depths[(i + 1):n] == 1, na.rm = TRUE) && (i < n)
        prefix <- if (has_later_sibling) " \u251c\u2500\u2500 " else " \u2514\u2500\u2500 "
        tree_strings[i] <- paste0(prefix, names_only[i], "/")
      } else {
        indent <- ""
        for (j in 1:(d - 1)) {
          has_later_sibling <- FALSE
          if (i < n) {
            for (k in (i + 1):n) {
              if (depths[k] < j) break
              if (depths[k] == j) {
                has_later_sibling <- TRUE
                break
              }
            }
          }
          indent <- paste0(indent, if (has_later_sibling) " \u2502   " else "     ")
        }
        
        is_last_child <- TRUE
        if (i < n) {
          for (k in (i + 1):n) {
            if (depths[k] < d) break
            if (depths[k] == d) {
              is_last_child <- FALSE
              break
            }
          }
        }
        
        branch <- if (is_last_child) " \u2514\u2500\u2500 " else " \u251c\u2500\u2500 "
        tree_strings[i] <- paste0(indent, branch, names_only[i], "/")
      }
    }
  }
  
  out$clean_tree_dir <- tree_strings
  out$nchar <- nchar(out$clean_tree_dir)
  out$ws <- max(out$nchar, na.rm = TRUE) - out$nchar
  
  out$Directory <- apply(out, 1, function(x) {
    paste0(x["clean_tree_dir"], paste(rep(" ", times = as.numeric(x["ws"])), collapse = ""))
  })
  out$row <- seq_len(nrow(out))
  
  # Print headers and output matrix frame
  sd_name <- gdrive_head$name
  cat("Shared Drive: ", crayon::yellow(sd_name), "\n", sep = "")
  
  if (!is.null(folder)) {
    display_folder <- paste0("/", target_full_path)
    cat("Folder Target: ", crayon::yellow(display_folder), "\n", sep = "")
  }
  
  print(out[, c("row", "Directory", "files", "gdrive_path")], row.names = FALSE, right = FALSE)
}
