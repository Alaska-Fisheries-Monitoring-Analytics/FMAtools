' Adds a Gear Type Categorical Variable to Observer or Fish Ticket Data
#'
#' \code{gear.type} adds a gear type column to the FOS Observer or Fish Ticket data. Gear type is a categorical variable that lumps gear codes into larger categories.
#'
#' @param df data.frame containing the following fields: \code{GEAR} and either \code{sector} (from observer data) or \code{GRGROUP} (from fish ticket data).
#' @param twl argument S= separates bottom from midwater, B= combines them
#' @param fg argument S= separates Non Nearshore hook & line from pot, B = combines them
#' @param cs argument S= separates Catch Shares hook & line from pot, B = combines them
#' @param ns argument S= separates Nearshore hook & line from pot, B = combines them
#'
#' @details Gear categories include: Bottom Trawl, Midwater Trawl, Shrimp Trawl, Hook & Line, Pot, and Fixed Gear. Fixed Gear combines Hook & Line with Pot. Other somewhat rarer gear types are also flagged; see the code near the end of the function for these gear types.
#'
#' @return Returns \code{df}, with a new column of gear types.
#'
#' @examples
#' \dontrun{
#' ob$gear <- gear.type(ob, "S", "S")
#' }
#'
#' @export
#*******************************************************************************
# History
#-----------
# Jannot Nov 2014 Converts numeric gear codes to gear types
# Somers Mar 2017 - Updating & QAQC'ing application of gear codes to gear types in FT data
# Jannot July 2019 - Updating to add CA Ridgeback gear
#-------------------------------------------------------------------------------
# set Gear codes
btm.twl <- c(1, 2, 4, 5, 17, "TWL")
mid.twl <- c(3, "MID")
all.twl <- c(1:5, 17, "MID", "TWL")
shmp.twl <- c(11:13, "TWS")
fixed.gear <- c(6:10, 15, 18:20, "HKL", "H&L", "POT")
hl <- c(6:9, 15, 19, 20, "HKL", "H&L")
pot <- c(10, 18, "POT")


gear.type <- function(df, twl = "S", cs = "S", fg = "S", ns = "B") {
  if (any(names(df) == "GRGROUP")) {
    # Corrects FT$GRGROUP where TWL was listed but really a midwater trip
    df$GRGROUP[df$sector %in% c("Midwater Hake", "Midwater Hake EM", "Midwater Rockfish", "Midwater Rockfish EM", "Shoreside Hake", "Shoreside Hake Pre-CS") & df$GRGROUP == "TWL"] <- "MID"
    df$GRGROUP[df$GRID == "MDT"] <- "MID"
    df$GEAR <- df$GRGROUP
  }

  if (any(names(df) == "PACFIN_GROUP_GEAR_CODE")) {
    # Corrects FT$GRGROUP where TWL was listed but really a midwater trip
    df$PACFIN_GROUP_GEAR_CODE[df$sector %in% c("Midwater Hake", "Midwater Hake EM", "Midwater Rockfish", "Midwater Rockfish EM", "Shoreside Hake", "Shoreside Hake Pre-CS") & df$GRGROUP == "TWL"] <- "MID"
    df$PACFIN_GROUP_GEAR_CODE[df$PACFIN_GEAR_CODE == "MDT"] <- "MID"
    df$GEAR <- df$PACFIN_GROUP_GEAR_CODE
  }

  x <- rep(NA, nrow(df))

  if (any(df$sector == "" | is.na(df$sector))) {
    message("For <BLANK> sectors, gears will be assigned separately regardless of the input arguments to the function. Be sure to check this is appropriate")
    x[is.na(x) & (df$sector == "" | is.na(df$sector)) & df$GEAR %in% btm.twl] <- "Bottom Trawl"
    x[is.na(x) & (df$sector == "" | is.na(df$sector)) & df$GEAR %in% mid.twl] <- "Midwater Trawl"
    x[is.na(x) & (df$sector == "" | is.na(df$sector)) & df$GEAR %in% shmp.twl] <- "Shrimp Trawl"
    x[is.na(x) & (df$sector == "" | is.na(df$sector)) & df$GEAR %in% hl] <- "Hook & Line"
    x[is.na(x) & (df$sector == "" | is.na(df$sector)) & df$GEAR %in% pot] <- "Pot"
  }

  if (twl == "S") {
    message("Catch Shares non-hake Bottom and Midwater trawls are separate for all years")
    x[is.na(x) & df$sector %in% c(
      "Catch Shares", "Catch Shares EM", "Limited Entry Trawl",
      "LE CA Halibut", "Electronic Monitoring EFP"
    ) & df$GEAR %in% btm.twl] <- "Bottom Trawl"
    x[is.na(x) & df$sector %in% c(
      "Catch Shares", "Catch Shares EM", "Limited Entry Trawl", "Electronic Monitoring EFP",
      "Tribal"
    ) & df$GEAR %in% mid.twl] <- "Midwater Trawl"
    x[is.na(x) & df$sector %in% c(
      "Shoreside Hake", "Shoreside Hake Pre-CS", "Midwater Hake", "Midwater Hake EM",
      "Midwater Rockfish", "Midwater Rockfish EM"
    )] <- "Midwater Trawl"
  } else {
    message("Catch Shares non-hake Bottom and Midwater trawls are combined in 2011, separate since 2012")
    x[is.na(x) & df$sector %in% c("Limited Entry Trawl") & df$GEAR %in% all.twl] <- "Bottom & Midwater Trawl"
    x[is.na(x) & df$sector %in% c("Catch Shares", "LE CA Halibut") & df$GEAR %in% all.twl & df$RYEAR == 2011] <- "Bottom & Midwater Trawl"
    x[is.na(x) & df$sector %in% c("Catch Shares", "LE CA Halibut") & df$GEAR %in% all.twl & df$RYEAR > 2011] <- "Bottom Trawl"
    x[is.na(x) & df$sector %in% c("LE CA Halibut") & df$GEAR %in% all.twl & df$RYEAR < 2011] <- "Bottom Trawl"
    x[is.na(x) & df$sector %in% c("Catch Shares EM", "Electronic Monitoring EFP") & df$GEAR %in% all.twl] <- "Bottom Trawl"
    x[is.na(x) & df$sector %in% c(
      "Shoreside Hake", "Shoreside Hake Pre-CS", "Midwater Hake", "Midwater Hake EM",
      "Midwater Rockfish", "Midwater Rockfish EM", "Tribal"
    ) & df$GEAR %in% all.twl] <- "Midwater Trawl"
  }

  # in 2016 one vessel fishes in CS EM with midwater trawl, but we consider it to be
  # in the bottom trawl sector, so change gears here
  x[df$sector %in% c("Catch Shares EM") & df$GEAR %in% mid.twl & df$RYEAR == 2016] <- "Bottom Trawl"

  #   x[is.na(x) & df$sector %in% c("Shoreside Hake", "Midwater Hake", "Midwater Hake EM", "Midwater Rockfish"
  #                                 , "Midwater Rockfish EM", "Electronic Monitoring EFP"
  #                                 , "Mothership Catcher-Vessel") & df$GEAR %in% mid.twl] <- "Midwater Trawl"
  x[is.na(x) & df$sector %in% c("OA CA Halibut") & df$GEAR %in% btm.twl] <- "Bottom Trawl"

  if (cs == "S") {
    message("Catch Shares Hook & Line and Pot gears are separate")
    x[is.na(x) & df$sector %in% c("Catch Shares", "Catch Shares EM", "Electronic Monitoring EFP") & df$GEAR %in% hl] <- "Hook & Line"
    x[is.na(x) & df$sector %in% c("Catch Shares", "Catch Shares EM", "Electronic Monitoring EFP") & df$GEAR %in% pot] <- "Pot"
  } else {
    message("Catch Shares Hook & Line and Pot gears are combined")
    x[is.na(x) & df$sector %in% c("Catch Shares", "Catch Shares EM", "Electronic Monitoring EFP") & df$GEAR %in% fixed.gear] <- "Fixed Gears"
  }

  if (fg == "S") {
    message("Non_Nearshore Hook & Line and Pot gears are separate")
    x[is.na(x) & df$sector %in% c(
      "Limited Entry Sablefish",
      "LE Fixed Gear DTL",
      "Limited Entry Zero Tier",
      "Limited Entry Zero Tier WCGOP",
      "OA Fixed Gear",
      "WC Open Access Fixed Gear"
    ) & df$GEAR %in% hl] <- "Hook & Line"
    x[is.na(x) & df$sector %in% c(
      "Limited Entry Sablefish",
      "LE Fixed Gear DTL",
      "Limited Entry Zero Tier",
      "Limited Entry Zero Tier WCGOP",
      "OA Fixed Gear",
      "WC Open Access Fixed Gear"
    ) & df$GEAR %in% pot] <- "Pot"
  } else {
    message("Non_Nearshore Hook & Line and Pot gears are combined")
    x[is.na(x) & df$sector %in% c(
      "Limited Entry Sablefish",
      "LE Fixed Gear DTL",
      "Limited Entry Zero Tier",
      "Limited Entry Zero Tier WCGOP",
      "OA Fixed Gear",
      "WC Open Access Fixed Gear"
    ) & df$GEAR %in% fixed.gear] <- "Fixed Gears"
  }
  if (ns == "S") {
    message("Nearshore Hook & Line and Pot gears are separate")
    x[is.na(x) & df$sector %in% c(
      "CA Nearshore",
      "Nearshore",
      "OR Blue/Black Rockfish",
      "OR Blue/Black Rockfish Nearshore",
      "Nearshore"
    ) & df$GEAR %in% hl] <- "Hook & Line"
    x[is.na(x) & df$sector %in% c(
      "CA Nearshore",
      "Nearshore",
      "OR Blue/Black Rockfish",
      "OR Blue/Black Rockfish Nearshore",
      "Nearshore"
    ) & df$GEAR %in% pot] <- "Pot"
  } else {
    message("Nearshore Hook & Line and Pot gears are combined")
    x[is.na(x) & df$sector %in% c(
      "CA Nearshore",
      "Nearshore",
      "OR Blue/Black Rockfish",
      "OR Blue/Black Rockfish Nearshore",
      "Nearshore"
    ) & df$GEAR %in% fixed.gear] <- "Fixed Gears"
  }

  x[is.na(x) & df$GEAR %in% shmp.twl] <- "Shrimp Trawl"

  # update cuke & ridgeback for ease, as they are sometimes called shrimp and sometimes bottom but inconsistently
  x[grepl("Cucumber", df$sector)] <- "Bottom Trawl"

  message("Ridgeback Prawn Bottom and Shrimp Trawl gears are combined")
  x[df$sector %in% c("Ridgeback Prawn", "CA Ridgeback Prawn")] <- "Shrimp Trawl"

  # Directed PHLB is all HKL, despite a few pot gear tickets from the early 2000's
  x[df$sector %in% c("Directed P Halibut", "IPHC Directed Commercial Halibut")] <- "Hook & Line"

  # EFPs
  # if(df$sector=="EFP" | df$sector=="Other Fisheries" | df$sector=="Non-EM EFP"){
  efps <- c(
    "Trawl Gear Modification EFP",
    "EFP",
    "Other Fisheries",
    "Non-EM EFP"
  )

  if (twl == "S") {
    message("Bottom and Midwater trawls are separate")
    x[is.na(x) & df$sector %in% c(
      efps, "Research",
      "Tribal"
    ) & df$GEAR %in% btm.twl] <- "Bottom Trawl"
    x[is.na(x) & df$sector %in% c("EFP") & df$TRIP_ID == 4999] <- "Midwater Trawl"
    x[is.na(x) & df$GEAR %in% mid.twl] <- "Midwater Trawl"
  } else {
    message("Bottom and Midwater trawls are combined")
    x[is.na(x) & df$sector %in% efps & df$GEAR %in% all.twl] <- "Bottom & Midwater Trawl"
    x[is.na(x) & df$sector %in% c("EFP") & df$TRIP_ID == 4999] <- "Bottom & Midwater Trawl"
  }

  if (fg == "S") {
    message("Hook & Line and Pot gears are separate")
    x[is.na(x) & df$sector %in% c(efps, "Tribal", "Research") & df$GEAR %in% hl] <- "Hook & Line"
    x[is.na(x) & df$sector %in% c(efps, "Tribal", "Research") & df$GEAR %in% pot] <- "Pot"
  } else {
    message("Hook & Line and Pot gears are combined")
    x[is.na(x) & df$sector %in% c(efps, "Tribal", "Research") & df$GEAR %in% fixed.gear] <- "Fixed Gears"
  }

  x[is.na(x) & df$sector %in% c(efps, "Tribal", "Research") & df$GEAR %in% shmp.twl] <- "Shrimp Trawl"
  # }

  # odd gear types
  if (any(df$GEAR %in% c("TLS", "NET", "DRG", "MSC"))) {
    message("Some odd gear types are included,\n
            you should check to make sure label is as expected.")
  }
  x[is.na(x) & df$GEAR == "TLS"] <- "Other Fixed Gears" # technically trolling gear
  x[df$GEAR %in% c("NET", "DRG")] <- "Other Net Gears"
  x[df$GEAR %in% c("MSC")] <- "Other Miscellaneous Gears"
  # if gear is unknown
  if (any(is.na(df$GEAR))) {
    message("Some GEAR are NA,
            you should check to make sure label is as expected.")
  }
  x[is.na(df$GEAR) & is.na(x)] <- "None or Unknown"
  return(x)
}
