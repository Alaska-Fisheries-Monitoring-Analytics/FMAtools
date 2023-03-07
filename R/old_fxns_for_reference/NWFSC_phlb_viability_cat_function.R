#' Creates Complete Cases for PHLB Viability Data
#'
#' \code{complete_viabs} Turns implicit missing PHLB Viability Categories into explicit zeros.
#'
#' @param V data.frame, typically of PHLB Biospecimens, containing the following fields: \code{VIABILITY} and \code{LENGTH} (from PHLB Biospecimens data).
#'
#' @details Viability categories differ by gear type. For Bottom Trawl and Pot, codes are \code{E, P, D}, for excellent, poor, and dead respectively.  Hook & Line uses injury assessment codes \code{MI, MO, S, D}, for minor, moderate, severe, or dead assessments.  Midwater trawl and other gear types do not have viability or injury assessments at this time.
#'
#' @return Returns \code{V}, with explicit missing viability categories set to zero.
#'
#' @examples
#' \dontrun{
#' ob$gear <- gear.type(ob, "S", "S")
#' }
#'
#' @export

# A function for completing viability categories
complete_viabs <- function(V, ...) {
  # - Complete Cases for all Viability categories
  # ==============================================

  distinct_viab_cats <- dplyr::distinct(V, VIABILITY) %>% pull()

  # first a check to throw an error if mixing EPD with MIMOSD

  if (any(distinct_viab_cats %in% hkl_injury_cats[1:3]) & any(distinct_viab_cats %in% btwlpot_viab_cats[1:2])) {
    stop("Error: Viabilities (E, P, D) are mixed with hook-&-line injuries (MI, MO, S, D).\nCheck to make sure that the data only represents a single gear type.")
  }

  if (any(hkl_injury_cats[1:3] %in% distinct_viab_cats)) {
    if (all(hkl_injury_cats %in% distinct_viab_cats)) {
      return(V)
    } else {
      mis_cats <- hkl_injury_cats[!hkl_injury_cats %in% distinct_viab_cats]

      all_viabs <- V %>%
        dplyr::distinct(..., LENGTH) %>%
        tidyr::expand(..., LENGTH,
          VIABILITY = mis_cats
        ) %>%
        dplyr::anti_join(., distinct(V, ..., VIABILITY, LENGTH)) %>%
        qdf()

      V <- V %>%
        dplyr::full_join(., all_viabs) %>%
        # dplyr::mutate(FREQUENCY = ifelse(is.na(FREQUENCY), 0, FREQUENCY))%>%
        qdf()
      return(V)
    }
  }

  if (any(btwlpot_viab_cats[1:2] %in% distinct_viab_cats)) {
    if (all(btwlpot_viab_cats %in% distinct_viab_cats)) {
      return(V)
    } else {
      mis_cats <- btwlpot_viab_cats[!btwlpot_viab_cats %in% distinct_viab_cats]

      all_viabs <- V %>%
        dplyr::distinct(..., LENGTH) %>%
        tidyr::expand(..., LENGTH,
          VIABILITY = mis_cats
        ) %>%
        dplyr::anti_join(., distinct(V, ..., VIABILITY, LENGTH)) %>%
        qdf()

      V <- V %>%
        dplyr::full_join(., all_viabs) %>%
        # dplyr::mutate(FREQUENCY = ifelse(is.na(FREQUENCY), 0, FREQUENCY))%>%
        qdf()
      return(V)
    }
  }
}
