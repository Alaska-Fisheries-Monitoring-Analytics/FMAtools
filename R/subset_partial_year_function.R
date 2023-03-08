#' Subset Data to Partial Years
#'
#' \code{subset_partial_year} subsets each year included in the input data set to a portion of the year specified.
#'
#' @param .df A data.frame with at least one date field.
#' @param .finalmonth A numeric value (0-12) representing the final month to be *included* in each partial year's data. Defaults to 12.
#' @param .finalday A numeric value (1-31) representing the final day of the month to be *included* in each partial year's data. Defaults to 31.
#'
#' @return Returns a data.frame with each year subset to the appropriate portion of the year.
#'
#' @export
#'
subset_partial_year <- function(.df, .finalmonth = 12, .finalday = 31) {
  if (!.finalmonth %in% 1:12) {
    stop(".finalmonth must be a numeric between 1-12", call. = FALSE)
  }

  if (!.finalday %in% 1:31) {
    stop(".finalday must be a numeric between 1-31", call. = FALSE)
  }

  # Year Field
  yrfield <- names(.df)[grepl("(?i)^year$", names(.df))]

  if (length(yrfield) == 0) {
    yrfield <- names(.df)[grepl("(?i)(.*?)(year)", names(.df))]
    if (any(grepl("(?i)(R_|R)(YEAR)", yrfield))) {
      yrfield <- yrfield[grepl("(?i)(R_|R)(YEAR)", yrfield)]
    } else {
      yrfield <- yrfield[1]
    }
  }

  yrs <- sort(unique(.df[, yrfield]))
  yrfield <- rlang::sym(yrfield)


  # Date Field
  datefield <- names(.df)[grepl("(?i)^date$", names(.df))]
  if (length(datefield) == 0) {
    datefield <- names(.df)[grepl("(?i)(.*?)(date)", names(.df))]
    if (any(grepl("(?i)(UP_|UP)(DATE)", datefield))) {
      datefield <- datefield[grepl("(?i)up", datefield)]
    } else {
      if (any(grepl("(?i)(R_|R)(DATE)", datefield))) {
        datefield <- datefield[grepl("(?i)(R_|R)(DATE)", datefield)][1]
      } else {
        datefield <- datefield[1]
      }
    }
  }
  datefield <- rlang::sym(datefield)

  for (i in 1:length(yrs)) {
    tmpyr <- .df %>%
      dplyr::filter(!!yrfield == yrs[i]) %>%
      dplyr::mutate(
        date = lubridate::parse_date_time(!!datefield, order = date_formats),
        month = lubridate::month(date),
        day = lubridate::day(date)
      ) %>%
      qdf()

    complete_months <- tmpyr %>%
      dplyr::filter(month < .finalmonth) %>%
      qdf()

    partial_month <- tmpyr %>%
      dplyr::filter(
        month == .finalmonth,
        day <= .finalday
      ) %>%
      qdf()

    partial_year <- rbind(complete_months, partial_month)

    if (i == 1) {
      out <- partial_year
    } else {
      out <- rbind(out, partial_year)
    }
  }
  return(out)
}
