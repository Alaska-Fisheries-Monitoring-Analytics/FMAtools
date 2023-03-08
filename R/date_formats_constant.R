#' @title Date Formats
#'
#' @description \code{date_formats} is a character string containing the many
#' possible formats that a date could take.  For use with \code{pkg-lubridate}
#' functions.
#'
#' @details
#' \code{date_formats}is a constant that should not be exported from NAMESPACE.
#'
#' @return Returns a character vector of date formats.

date_formats <- c("ymd HMS", "mdy HMS", "dmy HMS", "ymd HM", "mdy HM",
                  "dmy HM", "ymd", "mdy", "dmy", "yBd", "Bdy", "dBy",
                  "yBd HMS", "Bdy HMS", "dBy HMS", "yBd HM", "Bdy HM", "dBy HM")
