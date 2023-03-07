#' Formats Tables for Output to \code{.csv} Files with a Caption
#'
#' Function formats table headers and adds a caption to the table for output to \code{.csv}. The caption appears in the top left cell and the column headers become part of the data.frame.
#'
#' @param tb The table to be formatted. (data.frame, required)
#' @param cp The caption to be added to the table.(optional, character vector of \code{length == 1})
#'
#' @return Returns a data.frame
#'
#' @seealso \code{footn_fxn}
#'
#' @examples
#' \dontrun{
#' tab_with_cap <- format_table_csv(df_to_format, cp = caption_to_add)
#' }
#'
#' @export

format_table_csv <- function(tb, cp = NULL) {
  # if in some cases, the names have already been moved to a row...
  if (all(grepl("^X", names(tb)))) {
    names(tb) <- rep(" ", ncol(tb))
  }

  # get names for addition to data.frame
  nmstb <- data.frame(matrix(names(tb), nrow = 1, ncol = length(names(tb))),
    stringsAsFactors = FALSE
  )

  # clean names
  nmstb <- gsub("(*)(\\.[0-9]$)", "\\1", nmstb)

  # assign new col names to original names for rbinding
  attr(nmstb, "col.names") <- paste0("X", 1:ncol(tb))

  if (!is.null(cp)) {
    # create a data.frame that includes the caption and give it names for rbinding
    cpt <- data.frame(X1 = cp, stringsAsFactors = FALSE) %>%
      cbind(., data.frame(matrix(" ", nrow = 1, ncol = ncol(tb)), stringsAsFactors = FALSE) %>%
        dplyr::select(-one_of("X1")) %>% qdf()) %>%
      qdf()
  }

  # rename the original columns in the data.frame for rbinding
  names(tb) <- paste0("X", 1:ncol(tb))

  # rbind
  if (!is.null(cp)) {
    tb_fmt <- rbind(cpt, nmstb, tb)
  } else {
    tb_fmt <- rbind(nmstb, tb)
  }

  # produce blank headers - the "col names" are part of the data.frame
  names(tb_fmt) <- rep(" ", ncol(tb_fmt))

  return(tb_fmt)
}
