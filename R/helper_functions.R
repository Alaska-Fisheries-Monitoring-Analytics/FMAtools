# Helper functions
#' Which Names Are Missing From Data.Frame
#'
#' A low level wrapper to check for differences in names between two data.frames.
#'
#' @param df1 first data frame
#' @param df2 second data frame
#'
#' @details
#' Checks for names of columns in each data.frame that does not appear in the other data.frame.  Prints the missing column names for each data frame.
#' @return Returns character vector
#'
#' @export
chk_missing_nms <- function(df1, df2) {
  print("Found in df1, missing in df2:")

  print(names(df1)[!names(df1) %in% names(df2)])

  print("Found in df2, missing in df1:")

  print(names(df2)[!names(df2) %in% names(df1)])
}

#' Round Numeric Columns of a data.frame
#'
#' Rounds only numeric (i.e., \code{is.double()== TRUE}) columns of a data.frame to the level specified in \code{digits}.
#'
#' @param df Data.frame with numeric columns for rounding.
#' @param digits the number of digits to round, passed to \code{base::round()}.
#' @param prnt Do you want to print these values in a table format? If \code{FALSE} (default), no trailing zeros will be printed.
#' @param sym A character vector of (length == 1) used as a symbol for any 'NA's.
#'
#' @details
#' Only columns that satisfy \code{is.double() == TRUE} will be rounded.  Thus, integers are not rounded.
#'
#' @return Returns a data.frame with numeric columns rounded to \code{digits}
#'
#' @export

round_df <- function(df, digits, prnt = FALSE, sym = "NA") {
  nums <- vapply(df, is.double, FUN.VALUE = logical(1))
  if (!any(nums)) {
    return(df)
  }

  nm.df <- names(nums[nums == T])

  if (prnt == F) {
    df[, nums] <- round(df[, nums], digits = digits)
  } else {
    df[, nums] <- unlist(lapply(df[, nums], FUN = function(x) {
      sprintf(paste("%.", digits, "f", sep = ""), round(x, digits = digits) + 0)
    }))
    if (sym != "NA") {
      for (i in 1:length(nm.df)) {
        df[df[, nm.df[i]] == "NA", nm.df[i]] <- as.character(sym)
      }
    }
  }
  return(df)
}

#' Is Any NA Present?
#'
#' A low level wrapper for \code{any(is.na(x))}.
#'
#' @param x any R object that can be checked for NA
#'
#' @details
#' Wrapper for \code{any(is.na(x))}.
#' @return Returns logical
#'
#' @examples
#' \dontrun{
#' neisna(x)
#' }
#'
#' @export

neisna <- function(x) {
  any(is.na(x))
}


#' Quick Data Frame
#'
#' Function to return a data.frame mainly from piping
#'
#' @param ... data.frame, see details.
#' @param saf strings as factors? Default = FALSE
#'
#' @details
#' A low level function, used in many of the functions in this package. Usage is to return a data.frame without factors from piping in the tidyverse.
#'
#' @return Returns the \code{data.frame}, with strings as characters, mainly from piping in the tidyverse.
#'
#' @examples
#' \dontrun{
#' df <- df %>%
#'   filter(...) %>%
#'   mutate(...) %>%
#'   select(...) %>%
#'   qdf()
#' }
#'
#' @export

qdf <- function(..., saf = FALSE) {
  data.frame(..., stringsAsFactors = saf)
}

#' Quick Data Read CSV
#'
#' A simple wrapper function for \code{utils::read.csv(file =..., header = TRUE, stringsAsFactors = FALSE)}
#'
#' @param pth path to a \code{.csv} file.
#'
#' @return Returns the \code{.csv}, headers and with strings as characters.
#'
#' @examples
#' \dontrun{
#' df <- qdr(pth = "/path/to/file/filename.csv")
#' }
#'
#' @export

qdr <- function(pth) {
  utils::read.csv(file = pth, header = TRUE, stringsAsFactors = FALSE)
}

#' Write Multiple CSVs from a list
#'
#'  \code{write_csv_list()} Takes a list and writes each element to a csv.
#'
#' @param liste A list whose elements will be written to a csv.
#' @param fnms A character vector of file names (which can include a path, if desired) for each csv, where \code{length(fnms) == length(liste)}.
#' @param rnms Optional logical specifying whether or not to add the \code{row.names} to the output file. Default = \code{FALSE}.
#'
#' @return Nothing.
#'
#' @examples
#' \dontrun{
#' write_csv_list(liste = listofdf, fnms = vectoroffilenames)
#' }
#'
#' @export

write_csv_list <- function(liste, fnms, rnms = FALSE) {
  lapply(
    1:length(liste),
    function(x) {
      utils::write.csv(liste[[x]], file = fnms[x], row.names = rnms)
    }
  )
}

#' Sort Columns of Data.Frame by Names of Another Data.Frame
#'
#' Function to return a data.frame with columns ordered in the same manner as another data.frame, in prep for \code{rbind()}
#'
#' @param x data.frame whose columns will be sorted.
#' @param y data.frame whose columns will be used as template for sorting.
#' @param suppress suppress the printing of column names? Default = FALSE
#'
#' @details
#' A low level function, used in many of the functions in this package. Usage is to return a data.frame (\code{x}) whose columns are sorted according to the template data.frame columns (\code{y}).
#'
#' @return Returns \code{x}, with columns sorted according to \code{y}.
#'
#' @examples
#' \dontrun{
#' df2 <- colord(df2, df1)
#' }
#'
#' @export

colord <- function(x, y, suppress = FALSE) {
  col.ord <- names(y)
  x <- x[, col.ord]
  if (suppress == FALSE) {
    print(colnames(x))
    print(colnames(y))
  }
  return(x)
}

#' Makes Column Names of Output Data.Frame Consistent
#'
#' \code{namefix} fixes the column names for consistency across functions.
#'
#' @param df data.frame whose columns names will be fixed.
#'
#' @details
#' A low level function, used in many packages. Usage is to fix column names for consistency across functions.
#'
#' @return Returns \code{df}, with columns names fixed.
#'
#' @examples
#' \dontrun{
#' names(df) <- namefix(df)
#' }
#'
#' @export

namefix <- function(df) {
  tmp <- names(df) %>%
    tolower() %>%
    gsub("\\.", "_", .)
  return(tmp)
}

#' A Helper Function That Sorts the Object Names
#'
#' @description
#' \code{scol} sorts and prints the column names in alphabetical order.
#'
#' @param x any object that has names.
#'
#' @return Names sorted alphabetically.
#'
#' @examples
#' \dontrun{
#' scol(x)
#' }
#'
#' @export

scol <- function(x) {
  sort(names(x))
}


#' Makes Column Classes of two Data.Frames Consistent Across Common Column Names
#'
#' \code{matchColClasses} identifies columns that share the same name,, uses the master data frame (df1) to identify the shared columns classes, and then re-assigns column classes in df2 to match df1.
#'
#' @param df1 data.frame whose columns classes will be used as reference.
#' @param df2 data.frame with at least 1 column name that matches a column name in df1. For all column names that match, columns classes will be  modified to match corresponding column classes in df1.
#'
#' @details
#' A low level function, used to ensure proper joins between data.frames within functions.
#'
#' @return Returns \code{df2}, with columns classes modified to match classes of shared column names in df1.
#'
#' @examples
#' \dontrun{
#' df2 <- matchColClasses(df1, df2)
#' }
#'
#' @export


match_col_classes <- function(df1, df2) {
  shared_col_names <- names(df1)[names(df1) %in% names(df2)]
  shared_col_types <- sapply(df1[, shared_col_names], class)

  for (n in shared_col_names) {
    class(df2[, n]) <- shared_col_types[n]
  }

  return(df2)
}



#' Creates Character Vector from ...
#'
#' \code{dots2char} converts \code{...} into a character vector.
#'
#' @param ... from a function.
#'
#' @return Returns a character vector
#'
#' @export
#'
dots2char <- function(...) {
  chr <- as.character(unlist(rlang::exprs(...)))
  return(chr)
}

#' Order Unique Values
#'
#' \code{uni()} Pulls the unique values (using \code{unique()}) and orders them (using \code{sort()}).
#'
#' @param .x an R object with a class or a numeric, complex, character or logical vector.
#'
#' @return Returns a vector of ordered unique values.
#'
#' @export
#'
uni <- function(.x) {
  out <- sort(unique(.x))
  return(out)
}

# Shared Google Drive ----

#' For gdrive_ functions: check for googledrive token and if not active, use NOAA e-mail
#'
#' \code{drive_token()} is used by all \code{gdrive_} functions to ensure the user has authenticated their token to
#' communicate with the googledrive
#'
#' @return Automatically uses the user's stored NOAA e-mail to connect if no token is already established
gdrive_token <- function() {
  # To deauthorize in testing: googledrive::drive_deauth()

  if(isFALSE(googledrive::drive_has_token())) googledrive::drive_auth(email = "*@noaa.gov")

}

#' \code{shared_dribe_ls()} is used in place of \code{googledrive::drive_ls()} which does not work for users with only
#' shared access to a subfolder of the shared drive and not root access.
#'
#' @param gdrive_dribble a \code{dribble} containing the contents of a Shared Gdrive folder
#'
#' @return Returns a dribble of all items within the specified folder.
shared_drive_ls <- function(gdrive_dribble) {

  # Make a custom API request
  query <- glue("'{gdrive_dribble$id}' in parents and trashed = false")
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
  googledrive::do_paginated_request(req) %>% purrr::map("files") %>% purrr::flatten() %>% googledrive::as_dribble()
}

#' \code{dir_search()} is used recursively by \code{gdrive_ls()} to identify the folder structure of the Shared Google Drive
#'
#' @param dribble a \code{dribble} containing the contents of a Shared Gdrive folder
#'
#' @return Returns a list of two dribbles, the first is the parent folder and the second is the children folders
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
parse_local_path <- function(local_path){

  # Error checks
  # local_path must be a length 1 character string
  if( length(local_path) != 1 | !is.character(local_path) ) stop("`local_path` must a length = 1 character string.")
  # Can a file be found at the specified local path? If not, return
  if( !file_test(op = "-f",  local_path) ) local_exists <- F else local_exists <- T

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
    # *If modified times and byte lengths are the same, treat them as identical*
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
        fileId = g_path$gdrive_item$id, revisionId = gdrive_head$id, supportsAllDrives = TRUE, alt = "media"
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
    # *If the files aren't identical, declare whether the local or the gdrive is ahead*
    identical <- F
    local_status <- ifelse(mtime_match > 0, "behind", "ahead of")
    gdrive_raw <- NULL
  }

  if( local_status == "behind" ){
    # *If the local is behind, check to see if the local_mtime matches any prior gdrive versions*
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

  # Print the modified dates of the local and gdrive versions
  cat(paste0(
    "Modified datetimes of ", crayon::bold(l_path$name), ":\n- Local:  ", round(local_info$mtime),
    "\n- Gdrive: ", gdrive_head$modifiedTime, "\n"
  ))


  # Outputs
  list(
    identical = identical,
    mtime_match = mtime_match,
    local_status = local_status,
    gdrive_bytes = gdrive_raw$content
  )
}
