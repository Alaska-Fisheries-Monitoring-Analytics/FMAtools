#' @title Modified \code{dplyr::slice} to Work with Oracle Database
#'
#' @description \code{slice.tbl_Oracle} operates similar to
#' \code{dplyr::slice()} by selecting rows using their position number.
#'
#' @param .data A lazy data frame backed by a database query.
#' @param ordby A required variable or function of variables to order by.
#' @param ... Provide either positive values (i.e., row indicies) to keep,
#' or negative values to drop. The values provided must be either all positive
#' or all negative. Indices beyond the number of rows in the input are
#' silently ignored.
#'
#' @details
#' \code{dplyr::slice()} does not work with relational databases because
#' they have no intrinsic notion of row order. This function attempts to mimic
#' the workings of \code{dplyr::slice()} for cases where the data are being
#' pulled from an sql database.  This scenario has been adapted for the Oracle
#' flavor of SQL.
#'
#' An sql \code{ORDER BY} statement is required because there is no inherent
#' ordering of rows in SQL databases.  Thus, the \code{ordby} argument is
#' required.
#'
#' Adapted from
#' https://stackoverflow.com/questions/59217666/dbplyr-dplyr-and-functions-with-no-sql-equivalents-eg-slice#59221761
#'
#' @return Returns an object of the same type as \code{.data}.
#'
#' @seealso \code{dplyr::slice()}, \code{dbplyr::slice_min(), e.g.}
#'
#' @examples
#' \dontrun{
#' db <- memdb_frame(x = 1:3, y = c(1, 1, 2))
#' db %>% slice(2) # Throws an error - dplyr::slice is not supported
#' db %>%
#'   slice.tbl_Oracle(., y, 2) %>%
#'   show_query()
#' }
#'
#' @export

slice.tbl_Oracle <- function(.data, ordby, ...) {
  rows <- c(...)
  ordby <- rlang::enquo(ordby)

  .data %>%
    dbplyr::window_order(dplyr::desc(!!ordby)) %>%
    dplyr::mutate(...row_id = dplyr::row_number()) %>%
    dplyr::filter(...row_id %in% !!rows) %>%
    dplyr::select(-...row_id)
}
