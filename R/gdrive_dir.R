#' Show the Folder Structure of the Shared Google Drive
#'
#' Prints the structure of the Shared Google Drive to the console. It provides a count of the files within
#' each folder and all subfolders. Items in the 'path' column can be copy-pasted to use as the
#' `gdrive_path` argument of the `gdrive_set_dribble(gdrive_path)` function.
#'
#' @param shared_id An alias of a Shared Google Drive. By default, the drive for FMA Analytics.
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
gdrive_dir <- function(shared_id = c("Analytics")) {

  if( !is.character(shared_id) | length(shared_id) != 1) stop("'id' needs to be a length = 1 character string.")

  # Recall Hard coded dids from an alias
  if( shared_id == "Analytics") {
    id <- "0AJcHJWlPgKIgUk9PVA"
  } else stop("")

  # Get the dribble of the shared drive
  gdrive_dribble <- googledrive::shared_drive_get(id = id)
  parent <- gdrive_dribble
  parent_search <- dir_search(parent)

  #while( !is.null(parent_search$child) ) {
  while( nrow(parent_search$child) > 0 ) {
    new_parent <- parent_search$child
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
  cat("Shared Drive:", gdrive_dribble$name, "\n")

  # Format the output
  out <- folders_df[, c("gdrive_path", "files")]
  out <- out[order(out$gdrive_path), ]
  out$abbr_name <- gsub("([^/]+)(?=/.+)", "..", out$gdrive_path, perl = T)  # close, just a bit too much
  out$nchar <- nchar(out$abbr_name)
  out$ws <- max(out$nchar) - out$nchar
  out$Directory <- apply(out, 1, function(x) paste0(x["abbr_name"], paste(rep(" ", times = x["ws"]), collapse = "")))
  out[, c("Directory", "files", "gdrive_path")]
}
