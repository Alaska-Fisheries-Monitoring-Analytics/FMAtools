#' Download a File from the Shared Google Drive
#'
#' `gdrive_download()` is used to download the most recent version of a file in the Gdrive, or, if specified, an older
#'  version of a file. Currently only works with `.rdata` and `.rds` type files. Nearly all other files we use should be
#' maintained the project's GitHub repository.
#'
#' @param local_path the local path to the file you wish to download, including the directory (folder paths) and the
#' name of the file, which should match the name of the file on the Gdrive.
#' @param gdrive_dribble the `dribble` class object representing the folder in the Gdrive where your desired file
#' resides.
#' @param ver Optional, default is NULL. If specified as a length 1 number, will download the requested version of the
#'  file.
#'
#' @return Returns a message telling whether the download was performed and if so, where the file was saved.
#' @export
#'
#' @examples
#' \dontrun{
#'   gdrive_download(local_path, gdrive_dribble)
#' }
gdrive_download <- function(local_path, gdrive_dribble, ver = NULL) {
  # `local_path` is the local path to where you want to save the file and contains the name of the file.
  # `gdrive_dribble` is the dribble of the folder containing the file you want to pull

  # Make sure ver, if specified, is numeric
  if( !is.null(ver) ) if ( !is.numeric(ver) ) stop("Version number 'ver' needs to be numeric!")

  # Parse the local path to determine whether the file exists locally and what to search for on the Gdrive.
  l_path <- parse_local_path(local_path)
  # Parse the Gdrive path, using l_path to determine what to search for in the Gdrive.
  g_path <- parse_dribble(gdrive_dribble, l_path)
  
  else if(nrow(g_path$gdrive_item) == 0) {
    stop(paste0(
      "No file '", crayon::bold(l_path$name), "' was found in Gdrive folder", crayon::bold(gdrive_dribble$path), 
      ". Check spelling!
    ))
  } 
  
  # Can the directory in local_path be found?
  if( nchar(l_path$directory) > 0 ) {
    if( !file_test(op = "-d",  l_path$directory) ) {
      stop(paste0("Local path, ", crayon::bold(l_path$directory), ", not found."))}
  }
  
  if( is.null(ver) ){
    # *Downloading the most recent version*
    if( l_path$local_exists ){
      # If a local version already exists, compare it with the gdrive version
      compare_res <- compare_local_and_gdrive(l_path, g_path)

      if( compare_res$local_status %in% c("up to date with", "ahead of") ){
        # *If the local is ahead or up to date, skip the download*

        return(cat(paste0(
          "Local copy of ", crayon::bold(l_path$name), " is ", crayon::green(compare_res$local_status),
          " the Gdrive. Skipping download.\n"
        )))
      } else if( compare_res$local_status == "behind" ){
        # *If the local is behind, prompt to download and overwrite to bring the local version up to date*
        cat(paste0("Local version of ", crayon::bold(l_path$name), " is ", crayon::red(compare_res$local_status), " the Gdrive!\n"))
        download_response <- toupper(rstudioapi::showPrompt(
          title = "Notice!",
          message = paste0(
            "Overwrite and update your local version of ", l_path$name, " to ", "[ver",
            g_path$revision_lst[[1]]$version, "]", "? (Y/N)"
          )
        ))
        if( is.null(download_response) ){
          return(cat(paste0("Aborting download of ", crayon::bold(l_path$name), ".")))
        } else if( toupper(download_response) == "N" ) {
          return(cat(paste0("Aborting download of ", crayon::bold(l_path$name), ".")))
        } else if ( toupper(download_response) != "Y") {
          stop("You didnt enter `Y` or `N`! Aborting download.")
        }
      }
    } else {
      # *If a local version does not exist*
      cat(paste0(
        "No local copy of ", crayon::bold(l_path$name), " found. Downloading the most recent version: ",
        crayon::yellow(paste0("[ver", g_path$current_ver, "]")), ".\n"
      ))
      download_message <- paste0(
        "Overwrite and update your local version of ", l_path$name, " to ", "[ver",
        g_path$current_ver, "]", "? (Y/N)"
      )
    }
    # *Download the most recent version and set the modified time.*
    googledrive::drive_download(file = g_path$gdrive_item, path = local_path, overwrite = T)
    Sys.setFileTime(local_path, g_path$revision_lst[[g_path$current_ver]]$modifiedTime)

  } else {
    # *Downloading a prior version*

    # check to see if the file already exists
    ver_path <- paste0(
      l_path$directory, l_path$name_no_ext, "_v", formatC(ver, width = 3, digits = 0, flag = "0" ), l_path$extension
    )

    if( !file_test(op = "-f", ver_path) ){
      # *If the file doesn't exist, download it*

      # Grab the desired version
      if( !(ver %in% seq_along(g_path$revision_lst)) ){
        stop(cat(paste0(
          "No ", crayon::yellow(paste0("[ver", ver, "]")), " of ", crayon::bold(l_path$name),
          " exists! Currently on ", crayon::yellow(paste0("[ver", g_path$current_ver, "]")), ". Aborting download.\n"
        )))
      } else {

        # Subset the revision list to the desired revision
        revision_i <- g_path$revision_lst[[ver]]

        # If the version is found, download it as raw bytes
        revision_raw <- gargle::request_make(gargle::request_build(
          method = "GET",
          path = "drive/v3/files/{fileId}",
          params = list(
            fileId = g_path$gdrive_item$id, revisionId = revision_i$id, supportsAllDrives = TRUE, alt = "media"
          ),
          token = googledrive::drive_token()
        ))
        # Write the file the local path and set its modified date
        cat(paste0(
          "Downloading ", crayon::yellow(paste0(l_path$name, " [ver", ver, "]")), " as ", crayon::cyan(ver_path), ".\n"
        ))
        writeBin(revision_raw$content, con = ver_path)
        Sys.setFileTime(ver_path, revision_i$modifiedTime)
      }
    } else {
      # *If the file already exists, skip the download*
      # TODO `Could also check to make sure that the local version and the gdrive version matches? Perhaps overkill?`
      return(cat(paste0(crayon::cyan(ver_path), " already exists locally. Skipping download.\n")))
    }
  }
}
