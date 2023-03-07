#' Rename Column Headers with Standard Naming Conventions
#'
#' A low level convenience function to rename the column headers in a standardized format.  See \code{/scripts/Standard_Formats.Rmd} for list of standardization of names.
#'
#' @param dat data frame whose names need to be changed
#' @param ttl turns on/off the str_to_title() (T/F, F = default)
#'
#' @return Returns a data frame with new column headers
#'
#' @examples
#' \dontrun{
#' df2 <- rename_fxn(df)
#' }
#'
#' @export

rename_fxn <- function(dat, ttl = FALSE) {
  if (nrow(dat) == 0) {
    message("Dataframe has 0 (zero) rows.")
    return(dat)
  }

  # Length Frequency renames ------------------
  if (any(grepl("lenbin|(L|l)ength.(B|b)in..cm.", names(dat)))) {
    dat <- dat %>%
      dplyr::rename(Length_bin_cm = names(.)[grepl("lenbin|(L|l)ength.(B|b)in..cm.", names(.))]) %>%
      qdf()
  }
  if (any(names(dat) == "midpts")) {
    dat <- dat %>%
      dplyr::rename(Midpoint_cm = midpts) %>%
      qdf()
  }
  if (any(names(dat) == "freq")) {
    dat <- dat %>%
      dplyr::rename(Total = freq) %>%
      qdf()
  }
  if (any(names(dat) == "freq.all")) {
    dat <- dat %>%
      dplyr::rename(Total = freq.all) %>%
      qdf()
  }
  if (any(names(dat) == "freq.dead")) {
    dat <- dat %>%
      dplyr::rename(Dead = freq.dead) %>%
      qdf()
  }
  if (any(names(dat) == "HKLtot")) {
    dat <- dat %>%
      dplyr::rename(Total = HKLtot) %>%
      qdf()
  }
  if (any(names(dat) == "HKLdead")) {
    dat <- dat %>%
      dplyr::rename(Dead = HKLdead) %>%
      qdf()
  }
  if (any(names(dat) == "POTtot")) {
    dat <- dat %>%
      dplyr::rename(Total = POTtot) %>%
      qdf()
  }
  if (any(names(dat) == "POTdead")) {
    dat <- dat %>%
      dplyr::rename(Dead = POTdead) %>%
      qdf()
  }

  # Viability Renames -------------------------
  if (any(names(dat) == "no..of.Minr")) {
    dat <- dat %>%
      dplyr::rename(no_Minor = no..of.Minr) %>%
      qdf()
  }
  if (any(names(dat) == "no..of.Modr")) {
    dat <- dat %>%
      dplyr::rename(no_Moderate = no..of.Modr) %>%
      qdf()
  }
  if (any(names(dat) == "no..of.Sers")) {
    dat <- dat %>%
      dplyr::rename(no_Serious = no..of.Sers) %>%
      qdf()
  }
  if (any(names(dat) == "Minr.Perc")) {
    dat <- dat %>%
      dplyr::rename(Minor_Percent = Minr.Perc) %>%
      qdf()
  }
  if (any(names(dat) == "Modr.Perc")) {
    dat <- dat %>%
      dplyr::rename(Moderate_Percent = Modr.Perc) %>%
      qdf()
  }
  if (any(names(dat) == "Sers.Perc")) {
    dat <- dat %>%
      dplyr::rename(Serious_Percent = Sers.Perc) %>%
      qdf()
  }
  if (any(names(dat) == "Dead.Perc")) {
    dat <- dat %>%
      dplyr::rename(Dead_Percent = Dead.Perc) %>%
      qdf()
  }
  if (any(names(dat) == "Minr_ratio")) {
    dat <- dat %>%
      dplyr::rename(mi_rate = Minr_ratio) %>%
      qdf()
  }
  if (any(names(dat) == "Modr_ratio")) {
    dat <- dat %>%
      dplyr::rename(mo_rate = Modr_ratio) %>%
      qdf()
  }
  if (any(names(dat) == "Sers_ratio")) {
    dat <- dat %>%
      dplyr::rename(s_rate = Sers_ratio) %>%
      qdf()
  }
  if (any(names(dat) == "Dead_ratio")) {
    dat <- dat %>%
      dplyr::rename(d_rate = Dead_ratio) %>%
      qdf()
  }
  if (any(grepl("(?i)minr", names(dat)))) {
    names(dat) <- gsub("(?i)minr", "mi", names(dat))
  }
  if (any(grepl("(?i)modr", names(dat)))) {
    names(dat) <- gsub("(?i)modr", "mo", names(dat))
  }
  if (any(grepl("(?i)sers", names(dat)))) {
    names(dat) <- gsub("(?i)sers", "s", names(dat))
  }
  if (all((any(grepl("(?i)modr", names(dat)))) && unique(dat[, grepl("?i)^gear$", names(dat))]) == "Hook & Line")) {
    names(dat) <- gsub("(?i)dead", "d", names(dat))
  }
  if (any(grepl("(N|n)o..(E|e)x|(N|n)o.(E|e)x", names(dat)))) {
    dat <- dat %>%
      dplyr::rename(Observed_no_Exclnt = names(.)[grepl("(N|n)o..(E|e)x|(N|n)o.(E|e)x", names(.))]) %>%
      qdf()
  }
  if (any(grepl("(N|n)o..(P|p)oo|(N|n)o.(P|p)oo", names(dat)))) {
    dat <- dat %>%
      dplyr::rename(Observed_no_Poor = names(.)[grepl("(N|n)o..(P|p)oo|(N|n)o.(P|p)oo", names(.))]) %>%
      qdf()
  }
  if (any(grepl("(N|n)o..(D|d)ead|(N|n)o.(D|d)ead|(N|n)o..of.(D|d)ead$", names(dat)))) {
    dat <- dat %>%
      dplyr::rename(Observed_no_Dead = names(.)[grepl("(N|n)o..(D|d)ead|(N|n)o.(D|d)ead|(N|n)o..of.(D|d)ead$", names(.))]) %>%
      qdf()
  }
  if (any(grepl("(E|e)xc.(W|w)t", names(dat)))) {
    dat <- dat %>%
      dplyr::rename(Observed_Exclnt_mt = names(.)[grepl("(E|e)xc.(W|w)t", names(.))]) %>%
      qdf()
  }
  if (any(grepl("(P|p)oor.(W|w)t", names(dat)))) {
    dat <- dat %>%
      dplyr::rename(Observed_Poor_mt = names(.)[grepl("(P|p)oor.(W|w)t", names(.))]) %>%
      qdf()
  }
  if (any(grepl("(D|d)ead.(W|w)t", names(dat)))) {
    dat <- dat %>%
      dplyr::rename(Observed_Dead_mt = names(.)[grepl("(D|d)ead.(W|w)t", names(.))]) %>%
      qdf()
  }
  if (any(grepl("Tot.(W|w)t", names(dat)))) {
    dat <- dat %>%
      dplyr::rename(Observed_Total_mt = names(.)[grepl("Tot.(W|w)t", names(.))]) %>%
      qdf()
  }
  if (any(grepl("(E|e)st.(E|e)xc", names(dat)))) {
    dat <- dat %>%
      dplyr::rename(Estimated_gross_Exclnt_mt = names(.)[grepl("(E|e)st.(E|e)xc", names(.))]) %>%
      qdf()
  }
  if (any(grepl("(E|e)st.(P|p)oor", names(dat)))) {
    dat <- dat %>%
      dplyr::rename(Estimated_gross_Poor_mt = names(.)[grepl("(E|e)st.(P|p)oor", names(.))]) %>%
      qdf()
  }
  if (any(grepl("(E|e)st.(D|d)ead", names(dat)))) {
    dat <- dat %>%
      dplyr::rename(Estimated_gross_Dead_mt = names(.)[grepl("(E|e)st.(D|d)ead", names(.))]) %>%
      qdf()
  }
  if (any(grepl("(E|e)st.(T|t)otal", names(dat)))) {
    dat <- dat %>%
      dplyr::rename(Estimated_gross_Total_mt = names(.)[grepl("(E|e)st.(T|t)otal", names(.))]) %>%
      qdf()
  }
  if (any(grepl("m_(E|e)xc", names(dat)))) {
    dat <- dat %>%
      dplyr::rename(Mortality_Exclnt_mt = names(.)[grepl("m_(E|e)xc", names(.))]) %>%
      qdf()
  }
  if (any(grepl("m_(P|p)oor", names(dat)))) {
    dat <- dat %>%
      dplyr::rename(Mortality_Poor_mt = names(.)[grepl("m_(P|p)oor", names(.))]) %>%
      qdf()
  }
  if (any(grepl("m_(D|d)ead", names(dat)))) {
    dat <- dat %>%
      dplyr::rename(Mortality_Dead_mt = names(.)[grepl("m_(D|d)ead", names(.))]) %>%
      qdf()
  }
  if (any(grepl("m_Total", names(dat)))) {
    dat <- dat %>%
      dplyr::rename(Mortality_Total_mt = names(.)[grepl("m_Total", names(.))]) %>%
      qdf()
  }
  if (any(grepl("TODwt", names(dat)))) {
    dat <- dat %>%
      dplyr::rename(Mortality_Time_on_Deck_Total_mt = names(.)[grepl("TODwt", names(.))]) %>%
      qdf()
  }
  if (any(grepl("DMRobs|^DMR$", names(dat)))) {
    dat <- dat %>%
      dplyr::rename(Observer_Discard_Mortality_Rate = names(.)[grepl("DMRobs|^DMR$", names(.))]) %>%
      qdf()
  }
  if (any(grepl("DMRtod", names(dat)))) {
    dat <- dat %>%
      dplyr::rename(Time_on_Deck_Mortality_Rate = names(.)[grepl("DMRtod", names(.))]) %>%
      qdf()
  }

  if (any(grepl("Dead.exc", names(dat)))) {
    dat <- dat %>%
      dplyr::rename(Exclnt_died = names(.)[grepl("Dead.exc", names(.))]) %>%
      qdf()
  }
  if (any(grepl("Dead.poor", names(dat)))) {
    dat <- dat %>%
      dplyr::rename(Poor_died = names(.)[grepl("Dead.poor", names(.))]) %>%
      qdf()
  }
  if (any(grepl("Dead.dead", names(dat)))) {
    dat <- dat %>%
      dplyr::rename(Dead_died = names(.)[grepl("Dead.dead", names(.))]) %>%
      qdf()
  }
  if (any(grepl("SumDead", names(dat)))) {
    dat <- dat %>%
      dplyr::rename(Total_Dead = names(.)[grepl("SumDead", names(.))]) %>%
      qdf()
  }


  # Observer Coverage renames----------------------------
  if (any(grepl("Fleet.Obs.Cov.Rate|Coverage|FltCov", names(dat)))) {
    dat <- dat %>%
      dplyr::rename(Observed_Coverage = names(.)[grepl("Fleet.Obs.Cov.Rate|Coverage|FltCov", names(.))]) %>%
      qdf()
  }
  # 2020-07-01 possibly deprecated - JEJ
  # Sector renames ------------------------
  # if(any(names(dat)=="Fishery")){
  #   dat <- dat%>%
  #     dplyr::rename(Sector = Fishery)%>%
  #     qdf()
  # }

  # Unique Vessel, Trips Sets renames ------------------------
  if (any(grepl("(N|n)o..(V|v)essels|(N|n)o.(V|v)essels|(V|v)essels.UNI", names(dat)))) {
    dat <- dat %>%
      dplyr::rename(no_Observed_Vessels = names(.)[grepl("(N|n)o..(V|v)essels|(N|n)o.(V|v)essels|(V|v)essels.UNI", names(.))]) %>%
      qdf()
  }
  if (any(grepl("(T|t)rips.UNI|(N|n)o.(T|t)rips|(N|n)o..(T|t)rips", names(dat)))) {
    dat <- dat %>%
      dplyr::rename(no_Observed_Trips = names(.)[grepl("(T|t)rips.UNI|(N|n)o.(T|t)rips|(N|n)o..(T|t)rips", names(.))]) %>%
      qdf()
  }
  if (any(grepl("(S|s)ets.UNI|(N|n)o..(H|h)auls|(N|n)o.(H|h)auls|(N|n)o.(O|o)bs.(S|s)ets|(N|n)o.(O|o)bs.(H|h)auls", names(dat)))) {
    dat <- dat %>%
      dplyr::rename(no_Observed_Sets = names(.)[grepl("(S|s)ets.UNI|(N|n)o..(H|h)auls|(N|n)o.(H|h)auls|(N|n)o.(O|o)bs.(S|s)ets|(N|n)o.(O|o)bs.(H|h)auls", names(.))]) %>%
      qdf()
  }

  # Observed Retained renames ----------------------------------------
  if (any(names(dat) == "Obs.Nearsh.Retained.mt")) {
    dat <- dat %>%
      dplyr::rename(Obseved_Nearshore_Retained_mt = Obs.Nearsh.Retained.mt) %>%
      qdf()
  }
  if (any(names(dat) == "ObsCUKE.Ret")) {
    dat <- dat %>%
      dplyr::rename(Obseved_Cucumber_Retained_mt = ObsCUKE.Ret) %>%
      qdf()
  }
  if (any(names(dat) == "ObsPRWN.Ret")) {
    dat <- dat %>%
      dplyr::rename(Obseved_Prawn_Retained_mt = ObsPRWN.Ret) %>%
      qdf()
  }
  if (any(names(dat) == "ObsPHLB.Ret")) {
    dat <- dat %>%
      dplyr::rename(Observed_Phalibut_Retained_mt = ObsPHLB.Ret) %>%
      qdf()
  }
  if (any(names(dat) == "ObsRet")) {
    dat <- dat %>%
      dplyr::rename(Observed_Retained_mt = ObsRet) %>%
      qdf()
  }


  # Observed Discard renames ----------------------------------------
  if (any(names(dat) == "PHLBMT.Sum")) {
    if (!grepl("halibut", unique(dat$Sector))) {
      dat <- dat %>%
        dplyr::rename(Obseved_Phalibut_Discarded_mt = PHLBMT.Sum) %>%
        qdf()
    }
    if (grepl("halibut", unique(dat$Sector))) {
      dat <- dat %>%
        dplyr::rename(Phalibut_Landed_Fleetwide_mt = PHLBMT.Sum) %>%
        qdf()
    }
  }

  if (any(names(dat) == "P..halibut.discarded..mt.")) {
    if (!grepl("EFP", unique(dat$Sector))) {
      dat <- dat %>%
        dplyr::rename(Obseved_Phalibut_Discarded_mt = P..halibut.discarded..mt.) %>%
        qdf()
    }
    if (grepl("EFP", unique(dat$Sector))) {
      dat <- dat %>%
        dplyr::rename(Phalibut_Discarded_mt = P..halibut.discarded..mt.) %>%
        qdf()
    }
  }
  if (any(grepl("ObsDis|PHLB.(B|b)ycatch.mt", names(dat)))) {
    dat <- dat %>%
      dplyr::rename(Observed_Discarded_mt = names(.)[grepl("ObsDis|PHLB.(B|b)ycatch.mt", names(.))]) %>%
      qdf()
  }


  # Fleetwide Landings renames -------------------------------
  if (any(names(dat) == "Nearsh.Landed.mt")) {
    dat <- dat %>%
      dplyr::rename(Nearshore_Landed_Fleetwide_mt = Nearsh.Landed.mt) %>%
      qdf()
  }
  if (any(names(dat) == "CUKEMT.Sum")) {
    dat <- dat %>%
      dplyr::rename(Cucumber_Landed_Fleetwide_mt = CUKEMT.Sum) %>%
      qdf()
  }
  if (any(names(dat) == "MT.Sum")) {
    dat <- dat %>%
      dplyr::rename(Total_Landed_Fleetwide_mt = MT.Sum) %>%
      qdf()
  }

  if (any(names(dat) == "PRWNMT.Sum")) {
    dat <- dat %>%
      dplyr::rename(Prawn_Landed_Fleetwide_mt = PRWNMT.Sum) %>%
      qdf()
  }
  if (any(names(dat) == "TotRet")) {
    dat <- dat %>%
      dplyr::rename(Total_Landed_Fleetwide_mt = TotRet) %>%
      qdf()
  }
  if (any(grepl("P..halibut.land", names(dat)))) {
    if (!grepl("EFP|Other Fisheries", unique(dat$Sector))) {
      dat <- dat %>%
        dplyr::rename(Obseved_Phalibut_Landed_mt = names(.)[grepl("P..halibut.land", names(.))]) %>%
        qdf()
    }
    if (grepl("EFP|Other Fisheries", unique(dat$Sector))) {
      dat <- dat %>%
        dplyr::rename(Phalibut_Landed_mt = names(.)[grepl("P..halibut.land", names(.))]) %>%
        qdf()
    }
  }


  # Ratio renames -------------------------------------------------
  if (any(grepl("ObsRatio|PHLB.(B|b)ycatch.(R|r)ate", names(dat)))) {
    dat <- dat %>%
      dplyr::rename(Observed_Ratio = names(.)[grepl("ObsRatio|PHLB.(B|b)ycatch.(R|r)ate", names(.))]) %>%
      qdf()
  }
  if (any(grepl("PHLB.bycatch.se", names(dat)))) {
    dat <- dat %>%
      dplyr::rename(SE_Observed_Ratio = names(.)[grepl("PHLB.bycatch.se", names(.))]) %>%
      qdf()
  }

  # Discard Estimate renames --------------------------------------
  if (any(grepl("GrossEst|Total.bycatch.mt", names(dat)))) {
    dat <- dat %>%
      dplyr::rename(Gross_Discards_mt = names(.)[grepl("GrossEst|Total.bycatch.mt", names(dat))]) %>%
      qdf()
  }

  if (any(grepl("Total.discards.mt", names(dat)))) {
    dat <- dat %>%
      dplyr::rename(Total_Mortality_mt = names(.)[grepl("Total.discards.mt", names(dat))]) %>%
      qdf()
  }
  if (any(names(dat) == "estimate")) {
    dat <- dat %>%
      dplyr::rename(Estimate = estimate) %>%
      qdf()
  }

  if (any(grepl("(?i)^gear$", names(dat)))) {
    if (unique(dat[, grepl("(?i)^gear$", names(dat))]) == "Hook & Line") {
      names(dat) <- gsub("(?i)_mt$", "_wt", names(dat))
    }
  }
  # Legal-sized Renames -------------------------
  if (any(grepl("legal.mortality.mt", names(dat)))) {
    dat <- dat %>%
      dplyr::rename(Mortality_mt_legalSized = names(.)[grepl("legal.mortality.mt", names(.))]) %>%
      qdf()
  }
  if (any(grepl("Est.perc.leg.dis.wt", names(dat)))) {
    dat <- dat %>%
      dplyr::rename(Percent_Mortality_legalSized = names(.)[grepl("Est.perc.leg.dis.wt", names(.))]) %>%
      qdf()
  }


  # Sectors as Names renames --------------------------------------------
  if (any(names(dat) == "letwl_mt")) {
    dat <- dat %>%
      dplyr::rename(LE_Bottom_Trawl_2002_2010 = letwl_mt) %>%
      qdf()
  }
  if (any(names(dat) == "ifq.btwl")) {
    dat <- dat %>%
      dplyr::rename(IFQ_Bottom_Trawl = ifq.btwl) %>%
      qdf()
  }
  if (any(names(dat) == "ifq.lechlb")) {
    dat <- dat %>%
      dplyr::rename(IFQ_LE_CA_Halibut = ifq.lechlb) %>%
      qdf()
  }
  if (any(names(dat) == "ifq.hkl")) {
    dat <- dat %>%
      dplyr::rename(IFQ_Hook_Line = ifq.hkl) %>%
      qdf()
  }
  if (any(names(dat) == "ifq.pot")) {
    dat <- dat %>%
      dplyr::rename(IFQ_Pot = ifq.pot) %>%
      qdf()
  }
  if (any(names(dat) == "ifq.ssh")) {
    dat <- dat %>%
      dplyr::rename(IFQ_Midwater_Hake = ifq.ssh) %>%
      qdf()
  }
  if (any(names(dat) == "ifq.mid")) {
    dat <- dat %>%
      dplyr::rename(IFQ_Midwater_Rockfish = ifq.mid) %>%
      qdf()
  }
  if (any(names(dat) == "lesab")) {
    dat <- dat %>%
      dplyr::rename(LE_Sablefish_Endorsed = lesab) %>%
      qdf()
  }
  if (any(names(dat) == "ledtl")) {
    dat <- dat %>%
      dplyr::rename(LE_Sablefish_Non_Endorsed = ledtl) %>%
      qdf()
  }
  if (any(names(dat) == "oafg")) {
    dat <- dat %>%
      dplyr::rename(OA_Fixed_Gear = oafg) %>%
      qdf()
  }
  if (any(names(dat) == "ns")) {
    dat <- dat %>%
      dplyr::rename(Nearshore = ns) %>%
      qdf()
  }
  if (any(names(dat) == "ps")) {
    dat <- dat %>%
      dplyr::rename(Pink_Shrimp = ps) %>%
      qdf()
  }
  if (any(names(dat) == "chlb")) {
    dat <- dat %>%
      dplyr::rename(CA_Halibut = chlb) %>%
      qdf()
  }
  if (any(names(dat) == "derby")) {
    dat <- dat %>%
      dplyr::rename(Phalibut_Directed = derby) %>%
      qdf()
  }
  if (any(names(dat) == "ashop")) {
    dat <- dat %>%
      dplyr::rename(At_sea_Hake = ashop) %>%
      qdf()
  }

  # Hooks lost, total ---------------------------------------------
  if (any(names(dat) == "LostHooks")) {
    dat <- dat %>%
      dplyr::rename(no_Observed_Lost_Hooks = LostHooks) %>%
      qdf()
  }
  if (any(names(dat) == "TotalHooks")) {
    dat <- dat %>%
      dplyr::rename(no_Observed_Total_Hooks = TotalHooks) %>%
      qdf()
  }


  # Misc renames --------------------------------------------------
  if (any(names(dat) == "YEAR")) {
    dat <- dat %>%
      dplyr::rename(Year = YEAR) %>%
      qdf()
  }

  # fix _ and . in colname and make title format
  # x <- stringr::str_replace_all(x, "(.*?)([P|p]erc |[P|p]ercent[ |$])(.*?)", "\\1% \\3")


  dat <- dat %>%
    setNames(., nm = gsub("(^|.*?)(R|r)(et)(\\.|_| |$)(.*?)", "\\1\\2etained\\4\\5", names(.))) %>%
    setNames(., nm = gsub("(^|.*?)(D|d)(is)(\\.|_| |$)(.*?)", "\\1\\2iscard\\4\\5", names(.))) %>%
    setNames(., nm = gsub("(^|.*?)(M|m)(ort)(\\.|_| |$)(.*?)", "\\1\\2ortality\\4\\5", names(.))) %>%
    setNames(., nm = gsub("(^|.*?)(T|t)(ot)(\\.|_| |$])(.*?)", "\\1\\2otal\\4\\5", names(.))) %>%
    setNames(., nm = gsub("(^|.*?)(P|p)(erc)(\\.|_| |$])(.*?)", "\\1\\2ercent\\4\\5", names(.))) %>%
    setNames(., nm = gsub("no..of.|no_of_", "no_", names(.))) %>%
    setNames(., nm = gsub("Minr.|Minr_", "Minor_", names(.))) %>%
    setNames(., nm = gsub("Modr.|Modr_", "Moderate_", names(.))) %>%
    setNames(., nm = gsub("Sers.|Sers_", "Serious_", names(.))) %>%
    setNames(., nm = gsub("\\.", "_", names(.))) %>%
    setNames(., nm = gsub("__", "_", names(.))) %>%
    setNames(., nm = gsub("_$", "", names(.))) %>%
    qdf()

  if (ttl == TRUE) {
    dat %>%
      setNames(., nm = stringr::str_to_title(names(.))) %>%
      qdf()
  }

  return(dat)
}
