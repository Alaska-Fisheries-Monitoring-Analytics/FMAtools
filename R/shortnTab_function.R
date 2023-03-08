#' Shorten Tables for Printing
#'
#' \code{shortnTab} cuts off the bottom portion of a table where all the values are == 0 which makes for easier printing/reporting.
#'
#' @param dat A data.frame that will be printed as a table with zeros at bottom that can be cut-off (i.e., zeros that are uninformative).
#'
#' @details
#' The most common usage is in a length frequency table where the last *n* rows are length bins with uninformative zeros.
#'
#' @return Returns the range of rows (e.g., x:y) to be **included** in the data.frame.
#'
#' @examples
#' \dontrun{
#' shorter_table <- long_table[shortnTab(long_table), ]
#' }
#'
#' @export

shortnTab <- function(dat) {
  oldnms <- names(dat)

  dat <- dat %>%
    janitor::clean_names() %>%
    qdf()

  excludecols <- c(
    "year", "ryear", "sector", "gear", "bin",
    "lenbin", "length_bin_cm", "lenbin_1"
  )

  numCols <- vapply(dat, is.numeric, FUN.VALUE = F)

  # could be more efficient with (.)apply function
  # Lower table cutoff
  for (i in 1:nrow(dat)) {
    if (sum(dat[i, !names(numCols) %in% excludecols]) > 0) {
      if (i == 1) {
        lwrcutoff <- 1
        break
      } else {
        lwrcutoff <- i - 1
        message(paste0("The row number for the lower cutoff (lwrcutoff) is = ", i - 1))
        break
      }
    } else {
      next
    }
  }

  for (i in floor(nrow(dat) / 2):nrow(dat)) {
    # in the instance where their are values all the way to the bottom
    if (i == nrow(dat)) {
      uprcutoff <- nrow(dat)
      message(paste0("The upper cutoff (uprcutoff) is = nrow(dat) = ", nrow(dat)))
      # print(paste(i, uprcutoff, sep = " "))
      break
    }
    if (sum(dat[i, !names(numCols) %in% excludecols]) == 0) {
      uprcutoff <- i
      # check that there isnt just a weird set of zeros
      # between values greater than zero
      # print(paste(i, uprcutoff, sep = " "))
      if (sum(dat[(i + 1):nrow(dat), !names(numCols) %in% excludecols]) == 0) {
        message(paste0("The row number for the upper cutoff (uprcutoff) is = ", i - 1))
        break
      } else {
        next
      }
    } else {
      next
    }
  }
  return(c(lwrcutoff:uprcutoff))
}
