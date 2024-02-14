#' Designate a Gdrive Folder as a Target for Uploads and Downloads
#'
#' Given a folder path of the folder structure of the Shared Google Drive, this function will create a `dribble`
#' class object for use by key functions to interact with the drive, including `gdrive_upload()` and
#' `gdrive_download()`. By default, it searches the FMA Analytical Services Program shared drive.
#'
#' @param gdrive_path a character string of the folder path, with each folder name followed by `/`
#' @param shared_id an alias for the Shared Drive to connect to. The default, `Analytics`, is so far the only alias that is recognized.
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
gdrive_set_dribble <- function(gdrive_path, shared_id = "Analytics"){

  if( !is.character(gdrive_path) | length(gdrive_path) != 1) stop("'id' needs to be a length = 1 character string.")
  if( !is.character(shared_id) | length(shared_id) != 1) stop("'shared_id' needs to be a length = 1 character string.")

  # Recall Hard coded ids from an alias
  if( shared_id == "Analytics") {
    id <- "0AJcHJWlPgKIgUk9PVA"
  }

  # Get the dribble object from the gdrive_path. It will contain roww for all enclosed folders and files.
  dribble_out <- googledrive::drive_get(path = gdrive_path, shared_drive = googledrive::as_id(id))

  # Only allow folders to be set as targets (exclude files)
  dribble_out <- dribble_out[
    sapply(dribble_out$drive_resource, "[[", "mimeType") == "application/vnd.google-apps.folder",
  ]
  if( nrow(dribble_out) == 0 ){
    stop(paste0("Path ", crayon::yellow(gdrive_path), " was not found!"))
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
