#' @title Function to Import All Sheets from MSExcel File
#'
#' @description \code{read_xl_allsheets} reads in all the sheets within a single MSExcel file.
#'
#' @param filename A MSExcel file name (with path)
#' @param tibble A logical that determines if the output is a tibble (TRUE) or a regular R data.frame (default = FALSE)
#'
#' @return Returns a list of objects (likely data.frames) from multiple sheets in an MSExcel spreadsheet.
#'
#' @seealso \code{readxl::excel_sheets}
#'
#' @examples
#' \dontrun{
#' mysheets <- read_excel_allsheets("foo.xls")
#' }
#'
#' @export

read_xl_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if (!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  return(x)
}

# Lifted from https://stackoverflow.com/questions/12945687/read-all-worksheets-in-an-excel-workbook-into-an-r-list-with-data-frames#12945838
