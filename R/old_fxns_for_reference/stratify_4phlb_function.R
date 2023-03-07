#' Stratify and Subset Data for Pacific halibut Bycatch Estimates
#'
#' A wrapper function to subset and stratify the NWFSC West Coast Groundfish Observer Program data in preparation for estimating Pacific halibut bycatch in the Catch Share and non-Catch Share fisheries. For data from At-sea Pacific hake fisheries, see the \code{estimate_ashop_phlb} function.
#'
#' @param df WCGOP pre-processed data set, typically \code{OBOrig_Pre} (but note that \code{OBOrig_Proc} will likely work too). If \code{OBOrig_Pre} is used, you *must* have the \code{SPC} file loaded in the global environment. See details.
#' @param sct a character string indicating the specific fishery sector which the data will be subset. See \code{subset_sector} for valid options.
#' @param spc this is the current FOS \code{SPC} file which adds important fields to the \code{OBOrig_Pre} object.
#' @param gr a character string indicating the specific gear type which the data will be subset. See \code{subset_gear} for valid options.
#' @param a_lat a numeric vector of latitudes, in decimal format.
#' @param a_names a character vector of labels (minimum 2) to be associated with each latitudinal break. The length of \code{a_names} must be equal to \code{length(a_lat) + 1}.
#' @param d_dpth a numeric vector of depths.
#' @param d_names a character vector of labels (minimum 2) to be associated with each depth bin. The length of \code{d_names} must be equal to \code{length(d_dpth) + 1}.
# @param pot.conf.yrs Years where vessels using pot gear need to be combined across areas to make non-confidential.
#' @param state a logical indicating if the data should be stratified by state. Default (FALSE) produces a \code{state} column with \code{WA,OR,CA} for each row in the data.
#' @param ... other arguments.  See \code{base::cut} for finer control of area stratification.
#' @param notes Any notes associated with the run.  Will be appended to the meta-data.  (character vector, default = NULL)
#'
#' @details
#' WCGOP data set (typically , \code{OBOrig_Pre}). If \code{OBOrig_Pre} is passed then \code{OB.processing} is run on the object before being acted upon.  \code{OBOrig_Pre} requires that the most recent \code{SPC} file is loaded in the global environment.
#' If using \code{OBOrig_Proc}, the data must have the following data fields:
#' \itemize{
##' \item \code{sector}
##'  \item \code{gear} - Note: unlike all other headers, this header is case sensitive.
##'  \item \code{AVG_LAT}
##'  \item \code{AVG_DEPTH}
##'  \item \code{R_STATE}
#' }
#'
#' @return Returns a list comprised of:
##' \itemize{
##'  \item out: the subsetted data (data.frame) with state, area, and depth strata applied.
##'  \item meta: a list of meta-data, as produced either by this function or the \code{add_meta} function.
#' }
#'
#' @seealso \code{OB.processing, add_meta, rename_fxn, order_cols_4phlb, subset_sector, subset_gear, stratify_area, stratify_depth base::cut, OB.processing, get.sector}
#'
#' @examples
#' \dontrun{
#' cs_btwl <- stratify_4phlb(df = OBOrig_Proc, sct = "Catch Shares", gr = "Bottom Trawl", a_lat = c(40.1667, 46.88825), a_names = c(s4010, ptch4010, nptch), d_dpth = c(60), d_names = c("0-60", ">60"), state = FALSE, notes = NULL)
#'
#' distinct(cs_btwl$out, fishery, state, sector, gear, area, depth)
#' }
#'
#' @export

stratify_4phlb <- function(df, sct = NULL, spc = SPC, gr = NULL,
                           a_lat = NULL, a_names = NULL,
                           d_dpth = NULL, d_names = NULL,
                           state = FALSE,
                           notes = NULL) {
  # Dont use KS version of OB.processing & get.sector, throws an error on 'count``
  source(paste0(path_R, "ob_processing_function.R"), local = FALSE)
  source(paste0(path_R, "get_sector_function.R"), local = FALSE)

  # Meta Data
  #------------
  # save all the inputs for meta data
  fun.args <- as.list(environment())
  # drop the input data
  fun.args["df"] <- NULL
  fun.args["spc"] <- NULL
  nameofdata <- deparse(substitute(df))

  # Function Argument Checks
  #--------------------------
  if (grepl("_Proc", deparse(substitute(df)))) {
    tmpnms <- names(df)
    sctrnm <- any(stringr::str_detect(tmpnms, "^(?i)sector"))
    grnm <- all(any(stringr::str_detect(tmpnms, "gear"), is.character(df$gear)))
    alatnm <- any(stringr::str_detect(tmpnms, "^(?i)AVG_LAT|(?i)Latitude"))
    adpthnm <- any(stringr::str_detect(tmpnms, "^(?i)AVG_DEPTH"))
    rstenm <- any(stringr::str_detect(tmpnms, "^(?i)R_STATE|(?i)AGENCY_CODE"))

    # Fish tickets do not have depth
    # therefore, this is added to override the stop check below'
    if (grepl("FT", deparse(substitute(df)))) {
      adpthnm <- TRUE # it's a "lie" but necessary to override stop-check
    }

    if (!all(c(sctrnm, grnm, alatnm, adpthnm, rstenm))) {
      stop("One of the data fields needed for stratification is missing.")
    }
  }
  # OB.processing, if needed
  #---------------------------
  # Process CS differently because we need unsampled objects for CS coverage stats
  if (grepl("_Pre", deparse(substitute(df)))) {
    # For CS EM
    # include ALL PROGRAM = 17, incl. PSMFC Discard Handling Research
    if (sct == "Catch Shares EM") {
      df2 <- df %>%
        dplyr::mutate(
          OBsubsec = ifelse((PROGRAM_ID == 17 & sector == "Catch Shares"),
            "Catch Shares EM",
            OBsubsec
          ),
          sector = ifelse((PROGRAM_ID == 17 & sector == "Catch Shares"),
            "Catch Shares EM",
            sector
          )
        ) %>%
        qdf()

      df1 <- OB.processing(df2, sct, spc)
    } else {
      df1 <- OB.processing(df, sct, spc)
    }
    if (grepl("Catch Shares|LE CA Halibut|Shoreside Hake|Midwater Hake|Midwater Rockfish", sct)) {
      df1 <- df1 %>%
        filter(DATATYPE %in% c(
          "Analysis Data",
          "Unsampled ZMIS",
          "Unsampled IFQ",
          "Trip Without Catch",
          "Failed Data"
        )) %>%
        qdf()
    } else {
      df1 <- df1 %>%
        filter(DATATYPE %in% c(
          "Analysis Data",
          "Trip Without Catch"
        )) %>%
        qdf()
    }
  }

  # NO OB.processing
  #---------------------------
  if (grepl("_Proc", deparse(substitute(df)))) {
    df1 <- df
  }

  # Set default state here - changed below if need be
  out <- df1 %>%
    subset_sector(., sct = sct) %>%
    subset_gear(., gr = gr) %>%
    dplyr::mutate(area = stratify_area(., a_lat = a_lat, a_names = a_names)) %>%
    dplyr::mutate(depth = stratify_depth(., d_dpth = d_dpth, d_names = d_names)) %>%
    dplyr::mutate(state = "WA, OR, CA") %>%
    qdf()

  if (grepl("OB", deparse(substitute(df)))) {
    out <- out %>%
      dplyr::mutate(phlb = ifelse(SPID_EQV == "PHLB", "Pacific halibut", NA)) %>%
      qdf()
  }

  if (grepl("EM", deparse(substitute(df)))) {
    out <- out %>%
      dplyr::mutate(
        phlb = ifelse(species == "Pacific Halibut",
          "Pacific halibut", NA
        ),
        RYEAR = YEAR
      ) %>%
      qdf()
  }

  if (grepl("FT", deparse(substitute(df)))) {
    out <- out %>%
      dplyr::rename(RYEAR = YEAR) %>%
      qdf()
  }

  # apply correct states
  if (all(grepl("OB|EM", deparse(substitute(df))) && state)) {
    out <- out %>%
      dplyr::mutate(state = R_STATE) %>%
      dplyr::mutate(area = dplyr::case_when(
        state == "CA" ~ "California",
        state == "OR" ~ "Oregon",
        state == "WA" ~ "Washington",
        TRUE ~ "coastwide"
      )) %>%
      qdf()
  }


  if (all(grepl("FT", deparse(substitute(df))) && state)) {
    out <- out %>%
      dplyr::mutate(state = ifelse(AGENCY_CODE == "C",
        "CA",
        ifelse(AGENCY_CODE == "O",
          "OR",
          "WA"
        )
      )) %>%
      qdf() %>%
      dplyr::mutate(area = dplyr::case_when(
        state == "CA" ~ "California",
        state == "OR" ~ "Oregon",
        state == "WA" ~ "Washington",
        TRUE ~ "coastwide"
      )) %>%
      qdf()
  }

  if (any(unique(out$area) == "north of Pt. Chehalis, WA")) {
    out <- out %>%
      dplyr::mutate(state = ifelse(area == "north of Pt. Chehalis, WA",
        "WA",
        state
      )) %>%
      qdf()
  }


  if (sct %in% c("Ridgeback Prawn", "Sea Cucumber")) {
    out <- out %>%
      dplyr::mutate(
        area = "California",
        state = "CA"
      ) %>%
      qdf()
  }

  # Meta Data -----------------------------

  # get confidential strata
  # 2022-01-12 JJ: removed state && from the if(all(...grepl...)) below
  # not sure why we needed to limit to state = TRUE.
  if (all(grepl("OB|EM", deparse(substitute(df))))) {
    conf_strata <- test_conf(out, fishery, state, RYEAR, sector, gear, area, depth)

    if (is.data.frame(conf_strata)) {
      conf_strata <- conf_strata %>%
        dplyr::arrange(sector, gear, area, depth, RYEAR, state, fishery) %>%
        qdf()
    }
  } else {
    conf_strata <- "Fish Tickets = not applicable"
  }
  # capture the function's execution environment, which is normally ephemeral
  curenv <- new.env()
  # reset the add_meta enclosing environment to be equal to the functions enclosing
  #  this permits `add-meta` to access all variables created inside the function
  environment(add_meta) <- curenv

  # capture the meta data
  strata <- out %>%
    dplyr::distinct(., fishery, state, sector, gear, RYEAR, area, depth) %>%
    dplyr::arrange(fishery, state, RYEAR, sector, gear, area, depth) %>%
    qdf()
  fishery <- unique(out$fishery)
  state <- unique(out$state)
  sector <- unique(out$sector)
  gear <- unique(out$gear)
  area <- unique(out$area)
  depth <- unique(out$depth)


  if (grepl("OB|EM", deparse(substitute(df)))) {
    lat_range <- paste0(
      "The range of mean latitudes (N. lat.) is: ",
      out %>% dplyr::distinct(AVG_LAT) %>% filter(!is.na(.)) %>% min(.), "-",
      out %>% dplyr::distinct(AVG_LAT) %>% filter(!is.na(.)) %>% max(.)
    )
    depth_range <- paste0(
      "The range of mean depths (ftm) is: ",
      out %>% dplyr::distinct(AVG_DEPTH) %>% filter(!is.na(.)) %>% min(.), "-",
      out %>% dplyr::distinct(AVG_DEPTH) %>% filter(!is.na(.)) %>% max(.)
    )
  } else {
    lat_range <- "Fish Tickets = not applicable"
    depth_range <- "Fish Tickets = not applicable"
  }

  meta <- add_meta()
  meta <- c(fun.args, meta, latitudes = lat_range, depths = depth_range)

  obj <- list(meta = meta, out = out)

  return(obj)
}
