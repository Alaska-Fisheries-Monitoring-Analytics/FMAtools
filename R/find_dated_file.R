#' Find and (optionally) Import a File with a Date in the File Name
#'
#' \code{find_dated_file} searches a path for a file with a particular date in the file name, or the file name with the most recent date, and optionally, imports the file.
#'
#' @param dte A character vector (length = 1) of either the word "recent", which will be used to find the file name with the most recent date OR a date (YYYY-MM-DD) which will be used to find the file name that contains the specified date.
#' @param path A character vector (length = 1) of the path to search for files. Converted to \code{base::file.path()} and then passed to \code{base::list.files(path = path)}.
#' @param filename A character vector (length = 1) specifying the file name or parts thereof, including regular expressions, but excluding \code{dte}. Passed to\code{base::list.files(pattern = filename)}.
#' @param imprt A logical (default = TRUE). Do you wish to import the file? A \code{.csv} file will be imported using \code{base::read.csv()}, an R data file (\code{.Rda, .Rdata}) will be imported using \code{base::load()}.
#' @param ... arguments passed to \code{base::read.csv()} or \code{base::load()}.
#'
#' @details
#' \code{FUNCTION NAME} DESCRIPTION
#'
#' @note Most recent in this case, means the file name that contains the most recent date. The dte argument, if not 'recent', must be in the format of YYYY-MM-DD. Imported files can either be of \code{.csv} or \code{.Rda, .Rdata}.
#'
#' @seealso \code{base::list.files()}, \code{IsDate() (in R/helper_functions.R)}
#'
#' @return Minimally, prints the name of the file(s). If \code{impt == TRUE}, imports a file.
#'
#' @examples
#' \dontrun{
#'  # print filenames of file(s) with most recent date
#'  find_dated_file(dte = "recent", path = "path2file", filename = "filename_or_partialname", imprt = FALSE)
#'
#'  # import file with most recent date in file name
#'  # path + filename + most recent date (determined by \code{find_dated_file()})
#'  #  must lead to a *single* file name which can be imported.  Otherwise, error.
#'  find_dated_file(dte = "recent", path = "path2file", filename = "filename_or_partialname", imprt = TRUE)
#'
#' }
#'
#' @export

find_dated_file <- function(dte, path, filename, imprt = TRUE, ...){

  dte <- tolower(dte)
  if(all(dte != "recent" && !grepl("-", dte))){
    stop("The dte argument must either be the word 'recent' or a date of format YYYY-MM-DD")
  }
    #check to ensure YYYY-MM-DD format of date
  if(dte != "recent"){
    isDTE <- IsDate(mydate = dte, date.format = "%F")
    if(!isDTE){
      stop("The dte argument must be in the format of YYYY-MM-DD.")
    }
  }

  #----------------------------------------------------
  # Find the files
  #----------------------------------------------------
  path2file <- file.path(path)

  filenms <- list.files(path = paste0(path, "/"),
                        pattern = paste0("^(?i)", filename),
                        full.names = FALSE, ignore.case = TRUE)

  #----------------------------------------------------
  # Find the most recent version of a file by date
  # OR find the file with the specified dte argument
  #----------------------------------------------------

  if(dte == "recent"){
    #limit to files with most recent year as part of file name
    filenms <- filenms[grepl(format(Sys.Date(), "%Y"), filenms)]

      dts <- gsub("[A-Za-z]*", "", filenms)
      dts <- gsub("(_)*|(\\.)*", "", dts)
      dts <- gsub(paste0("(.*)(", data.year+1, ")(.)"), "\\2\\3", dts)

      dts <- lubridate::ymd(dts)

      requested_file <- filenms[grepl(dts[which.max(dts)], filenms)]

      print(requested_file)
  }else{
    requested_file <- filenms[grepl(dte, filenms)]
  }

  #----------------------------------------------------
  # Import the requested file, if you like
  #----------------------------------------------------
  if(imprt){
    if(length(requested_file)>1){
      stop("There are multiple files that match the criteria. Refine the file name so that criteria are met by a single file only OR import the file(s) needed from the list above in a separate step.")
    }
    # figure out if it's a csv or Rda
    ext <- ifelse(grepl("(?i)\\.csv", requested_file), "CSV", "RDATA")
    if(ext == "CSV"){
      read.csv(file = paste0(path,"/", requested_file),...)
    }else{
      load(file = paste0(path, "/",requested_file), ...)
    }
  }

}
