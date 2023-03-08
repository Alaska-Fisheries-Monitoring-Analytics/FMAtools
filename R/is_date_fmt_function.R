#' Check Date Format
#'
#' \code{is_date_fmt()} checks a date against a format to insure date is in
#' the correct format. This function should correctly handle NAs.
#'
#' @param dte a date string
#' @param dtefmt a date format (see \code{strptime} for formats)
#'
#' @note Based on function found here:
#' https://gist.github.com/micstr/69a64fbd0f5635094a53?permalink_comment_id=3432683#gistcomment-3432683
#'
#' @return Returns a logical.
#'
#' @seealso \code{as.Date, Date, strptime}
#'
#' @export
#'

is_date_fmt<- function(dte, dtefmt) {
  tryCatch(!is.na(as.Date(as.character(dte), dtefmt)),
           error = function(err) {FALSE})
}
