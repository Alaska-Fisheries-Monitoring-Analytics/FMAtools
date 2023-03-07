#' Mask Confidential Data within Strata
#'
#' A low level convenience function to mask data within confidential strata.
#'
#' @param confs A data.frame of confidential strata, usually from \code{stratify_4phlb}.
#' @param df A data.frame whose strata will be masked upon output.
#' @param sybl A character vector (\code{length == 1}) giving the replacement text for confidential data. Defaults to \code{*}.
#' @param rnd A logical indicating if the values should be rounded using \code{round_df}. Default = FALSE. See details.
#' @param decv Only applies if \code{rnd == TRUE}. An integer indicating the number of decimal places to round, passed to \code{base::round} via \code{round_df}.
#' @param ... Optional arguments \code{prnt, sym} passed to \code{round_df}.  See \code{?round_df} for specifications.
#'
#' @details
#' \code{mask_conf_data} will mask confidential data in the strata identified in the \code{confs} data.frame, which most often will be included in the output of \code{stratify_4phlb}. \code{mask_conf_data} replaces all numeric variables (\code{is.numeric}) with an asterisk (default), or optionally, with user supplied character(s), and in the process, convert all values to character.  If you want to preserve the values as numeric, use \code{sybl = NA}. Alternatively, set \code{rnd == TRUE} and the corresponding \code{decv} and optionally \code{...} to round all the output for printing as character.
#'
#' Note: this function does *not* test for confidentaility - it assumes that the strata provided in \code{confs} are correctly specified.
#'
#' @return Returns a \code{data.frame} with confidential strata masked.
#'
#' @seealso \code{test_conf} to test for confidential strata.
#'
#' @examples
#' \dontrun{
#' masked_df <- mask_conf_data(conf_strata, conf_df, sybl = "CONFIDENTIAL DATA")
#' }
#'
#' @export

mask_conf_data <- function(confs, df, sybl = NULL, rnd = FALSE, decv = NULL) {
  if (is.character(confs)) {
    message(confs)
    return(df)
  }

  if (is.null(sybl)) {
    sybl <- "*"
  }

  # sort confs so that strata line up properly
  origdfnms <- names(df)
  names(df) <- namefix(df)
  names(df) <- gsub("^(?i)ryear", "year", names(df))
  names(confs) <- gsub("^(?i)ryear", "year", names(confs))
  dfnms <- names(df)[names(df) %in% names(confs)]
  names(dfnms) <- dfnms

  confs <- confs %>%
    colord(., dfnms, suppress = TRUE) %>%
    dplyr::mutate(strata = do.call(paste, c(.[1:ncol(.)], sep = "_"))) %>%
    qdf()

  confsnms <- names(confs)

  if (rnd) {
    df <- df %>%
      dplyr::mutate(year = as.character(year)) %>%
      round_df(., decv) %>%
      qdf()
  }

  confscols <- names(confs)[names(confs) != "strata"] # 1:(ncol(confs) - 1)

  out <- df %>%
    dplyr::mutate(strata = do.call(paste, c(.[confscols], sep = "_"))) %>%
    dplyr::mutate(dplyr::across(names(.)[!names(.) %in% confsnms], ~ ifelse((strata %in% confs$strata & is.numeric(.x)), sybl, .))) %>%
    dplyr::mutate(dplyr::across(names(.)[!names(.) %in% confsnms], ~ ifelse((strata %in% confs$strata & grepl("%", .x)), sybl, .))) %>%
    dplyr::select(-tidyselect::one_of("strata")) %>%
    qdf()


  # restore original names
  names(out) <- origdfnms

  return(out)
}
