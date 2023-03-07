#' Test for confidentiality
#'
#' A low level convenience function to test for confidential strata.
#'
#' @param .df data.frame for which confidentiality will be tested, typically used after/while running \code{stratify_4phlb}.
#' @param ... Specification of columns to group/stratify.  Confidentiality will be tested for each group/strata.
#'
#' @details
#' The data must contain at least either a vessel identifier, e.g., \code{DRVID} or a count of unique vessels, e.g., code{no_Observed_Vessels}
#'
#' @return Returns a \code{data.frame} of confidential strata
#'
#' @seealso \code{mask_conf_data}
#'
#' @examples
#' \dontrun{
#' strata <- test_conf(df, fishery, state, RYEAR, sector, gear, area, depth)
#' }
#'
#' @export

test_conf <- function(.df, ...) {
  if (!any(stringr::str_detect(names(.df), "(?i)vess|(?i)drvid"))) {
    stop("Data is missing a unique vessel identifier")
  }

  if (any(stringr::str_detect(names(.df), "(?i)vess|(?i)drvid"))) {
    tmpnms <- names(.df)[stringr::str_detect(names(.df), "(?i)vess|(?i)drvid")]
    tmpnms <- sort(tmpnms)
    vid <- tmpnms[1]

    if (stringr::str_detect(vid, "(?i)drvid")) {
      testdf <- .df %>%
        # dplyr::rename(year = names(.df)[!is.na(stringr::str_extract(names(.df), "(?i)ryear|(^(?i)year)"))][1]) %>%
        dplyr::group_by(...) %>%
        dplyr::summarise(num_obs_vessels = dplyr::n_distinct(get(vid))) %>%
        dplyr::ungroup() %>%
        dplyr::filter(num_obs_vessels < 3) %>%
        dplyr::select(...) %>%
        dplyr::distinct() %>%
        qdf()
    } else {
      testdf <- .df %>%
        # dplyr::rename(year = names(.df)[!is.na(stringr::str_extract(names(.df), "(?i)ryear|(^(?i)year)"))][1]) %>%
        dplyr::group_by(...) %>%
        dplyr::filter(get(vid) < 3) %>%
        dplyr::select(...) %>%
        dplyr::distinct() %>%
        qdf()
    }

    if (nrow(testdf) == 0) {
      testdf <- "No confidential strata"
    }
  }
  return(testdf)
}
