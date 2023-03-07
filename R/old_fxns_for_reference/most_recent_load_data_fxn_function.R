#' Get the Most Recent Load Data Function from NWFSC Observer Program
#'
#' \code{most_recent_load_data_fxn} creates a *function*.  This function is the most recent \code{load_data()} function created by the FOS Analysts.
#'
#' @param none This function accepts no arguments.
#'
#' @details
#' This function looks for the most recent  \code{load_data()} function. It looks for this function in  \code{~/observer/Input/load_data_<YYYY-MM-DD>.R}.  It uses the date portion (\code{<YYYY-MM-DD>}) of the file name to determine which \code{load_data()} function is the most recent.
#' After determining the most recent function file, it reads the names of the data objects that could be loaded from the file, for example, \code{FTD_2002-2020_PreFunc_2021-07-17.RDA}.  This is necessary because the names of the data objects contain dates.
#' The function then "recreates" the most recent \code{load_data()} function using the names of the data objects.
#' \code{most_recent_load_data_fxn()} retains all the arguments and functionality of the most recent \code{load_data()} function.
#'
#' @return Returns the most recent \code{load_data()} function.
#'
#' @examples
#' \dontrun{
#' # To re-create the load data function
#'
#' newest_load_data <- most_recent_load_data_fxn()
#' }
#' @author Jason E. Jannot
#'
#' @export


most_recent_load_data_fxn <- function() {

  #----------------------------------------------------
  # Find the most recent file date
  #----------------------------------------------------
  start_path2loaddatafxn <- ifelse((grepl("Windows", Sys.getenv("OS")) && any(grepl("RSTUDIO", names(Sys.getenv())))),
    "V:/", "~/observer"
  )
  path2loaddatafxn <- file.path(start_path2loaddatafxn, "Input")

  filenms <- list.files(paste0(path2loaddatafxn, "/"),
    pattern = "^(?i)load",
    full.names = FALSE, ignore.case = TRUE
  )
  if (length(filenms[grepl(format(Sys.Date(), "%Y"), filenms)]) == 0) {
    filenms[grepl(as.character(as.numeric(format(Sys.Date(), "%Y")) - 1), filenms)]
  } else {
    filenms <- filenms[grepl(format(Sys.Date(), "%Y"), filenms)]
  }
  dts <- filenms[grepl("(?i)load_data_202[0-9]{1}-[0-9]{2}-[0-9]{2}\\.R", as.vector(filenms))]
  dts <- lubridate::ymd(gsub("\\.(?i)R", "", gsub("load_data_", "", dts)))

  most_recent <- filenms[grepl(dts[which.max(dts)], filenms)]

  #----------------------------------------------------
  # Read in the most_recent load_data_YYYY_MM_DD.R file
  #----------------------------------------------------
  fp <- file.path(path2loaddatafxn, most_recent)

  startread <- "Define file paths for data objects"
  endread <- "Load selected data"

  lines <- readLines(fp)
  startline <- which(grepl(startread, lines))
  endline <- which(grepl(endread, lines))

  goodlines <- lines[startline:endline]

  #----------------------------------------------------
  # Now lets create the new load_data function
  # based on the load_data_YYYY_MM_DD.R file.
  # We start with the normal function
  #----------------------------------------------------
  function(data_vec, verbose = TRUE, load_ref_files = TRUE,
           drive.load = paste0(path2loaddatafxn, "/"),
           load_analyst_functions = FALSE) {
    allowed_data <- c(
      "FT",
      "FT_pre",
      "FT_proc",
      "FT_pre_full",
      "FT_proc_full",
      "WCGOP",
      "WCGOP_pre",
      "WCGOP_proc",
      "WCGOP_pre_full",
      "WCGOP_proc_full",
      "ASHOP",
      "ASHOP_pre",
      "ASHOP_proc",
      "BIO",
      "BIO_pre",
      "BIO_proc",
      "LBK",
      "EM",
      "EM_pre",
      "EM_proc",
      "ref_only"
    )

    if (sum(!data_vec %in% allowed_data) > 0) {
      bad <- paste(data_vec[!data_vec %in% allowed_data], collapse = " ")

      message(paste0("Warning: ", bad, " not recognized. Please choose only from the following:"))
      message("     ASHOP")
      message("     ASHOP_pre")
      message("     ASHOP_proc")
      message("     BIO")
      message("     BIO_pre")
      message("     BIO_proc")
      message("     EM")
      message("     EM_pre")
      message("     EM_proc")
      message("     FT")
      message("     FT_pre")
      message("     FT_proc")
      message("     FT_pre_full")
      message("     FT_proc_full")
      message("     LBK")
      message("     WCGOP")
      message("     WCGOP_pre")
      message("     WCGOP_proc")
      message("     WCGOP_pre_full")
      message("     WCGOP_proc_full")
      message("     ref_only")
    }

    YEAR.DATA <- as.numeric(format(Sys.Date(), "%Y")) - 1

    #----------------------------------------------------
    # This part creates the NEW file paths with
    # the most recent date
    #----------------------------------------------------
    part1 <- "  "
    part3 <- " <- paste\\(drive.load,'"
    part4 <- ".*?RDA"
    part5 <- "',sep=\\'\\'\\)"

    drvpths <- c(
      "FT_pre_path",
      "FT_proc_path",
      "FT_pre_full_path",
      "FT_proc_full_path",
      "ASHOP_pre_path",
      "ASHOP_proc_path",
      "WCGOP_pre_path",
      "WCGOP_proc_path",
      "WCGOP_pre_full_path",
      "WCGOP_proc_full_path",
      "BIO_pre_path",
      "BIO_proc_path",
      "LBK_path",
      "EM_pre_path",
      "EM_proc_path"
    )

    for (i in 1:length(drvpths)) {
      assign(paste0(drvpths[i], 1), paste0(
        "(", part1,
        drvpths[i], ")(",
        part3, ")(", part4, ")(", part5, ")"
      ))
      fxd <- FALSE
      prl <- FALSE

      assign(
        paste0(drvpths[i], 2),
        gsub(get(paste0(drvpths[i], 1)), "\\3",
          goodlines[grepl(drvpths[i], goodlines)],
          fixed = fxd, perl = prl
        )
      )


      assign(drvpths[i], paste0(drive.load, get(paste0(drvpths[i], 2))))
    }

    #----------------------------------------------------
    # Dynamically load functions and ref files
    #----------------------------------------------------

    if (load_analyst_functions) {
      source(paste(drive.load, "functions.r", sep = ""))
    }

    if (load_ref_files) {
      source(paste(drive.load, "match.f.r", sep = ""))

      fospfdte <- gsub("(.*?)(FOSProc_Func_)(.*?)(\\.R)(.*?\\)\\))", "\\3", goodlines[grepl("FOSProc_Func_", goodlines)])
      fosprocfunc <- paste0("FOSProc_Func_", fospfdte, ".R")

      source(paste0(drive.load, fosprocfunc), local = FALSE)
      # Dont use KS version of OB.processing, throws an error on 'count``
      rm(OB.processing)
      source(paste0(path_R, "ob_processing_function.R"), local = FALSE)

      spcdte <- gsub("(.*?)(species groupings - master )(.*?)(\\.csv', stringsAsFactors=F\\))", "\\3", goodlines[grepl("species groupings - master", goodlines)])

      spcfle <- paste0("species groupings - master ", spcdte, ".csv")


      SPC <<- read.csv(paste0(drive.load, spcfle),
        stringsAsFactors = FALSE
      )

      area.pcid <<- read.csv(paste0(drive.load, "All_Ports_011714_wAREAIFQ.csv"))

      mldte <- gsub("(.*?)(Master_legend_)(.*?)(\\.csv', sep = ''\\))", "\\3", goodlines[grepl("Master_legend_", goodlines)])

      mlfle <- paste0("Master_legend_", mldte, ".csv")

      ML <<- read.csv(
        file = paste0(drive.load, mlfle),
        header = TRUE,
        stringsAsFactors = FALSE
      )
    }


    #----------------------------------------------------
    # Remainder is the normal load_data_YYYY_MM_DD.R file
    #----------------------------------------------------

    if ("ref_only" %in% data_vec) { # only load the reference files above
      message("Only reference files will be loaded")
      message("       reference files")
      message("   are now loaded")
    }

    if ("FT" %in% data_vec) { # Load FT data (bot pre and proc, NOT full)
      if (verbose) message("FT data is loading...")
      load(file = FT_pre_path, envir = globalenv()) #
      load(file = FT_proc_path, envir = globalenv())
      if (verbose) {
        message(paste("   Fish Ticket Data 2002-", YEAR.DATA, sep = ""))
        message("     with the following R.object names:")
        message("       FTOrig_Pre")
        message("       FTOrig_Proc")
        message("   are now loaded")
      }
    }

    if ("FT_pre" %in% data_vec) { # Load FT_pre data
      if (verbose) message("FT_pre data is loading...")
      load(file = FT_pre_path, envir = globalenv()) #
      if (verbose) {
        message(paste("   Pre-processed Fish Ticket Data 2002-", YEAR.DATA, sep = ""))
        message("     with the following R.object name:")
        message("       FTOrig_Pre")
        message("   is now loaded")
      }
    }

    if ("FT_proc" %in% data_vec) { # Load FT_proc data
      if (verbose) message("FT_proc data is loading...")
      load(file = FT_proc_path, envir = globalenv()) #
      if (verbose) {
        message(paste("   Processed Fish Ticket Data 2002-", YEAR.DATA, sep = ""))
        message("     with the following R.object name:")
        message("       FTOrig_Proc")
        message("   is now loaded")
      }
    }

    if ("FT_pre_full" %in% data_vec) { # Load FT_pre_full data
      if (verbose) message("FT_pre_full data is loading...")
      load(file = FT_pre_full_path, envir = globalenv())
      if (verbose) {
        message(paste("   Pre-processed Full Fish Ticket Data 2002-", YEAR.DATA, sep = ""))
        message("     with the following R.object name:")
        message("       FTOrig_Pre_Full")
        message("   is now loaded")
      }
    }

    if ("FT_proc_full" %in% data_vec) { # Load FT_proc_full data
      if (verbose) message("FT_proc_full data is loading...")
      load(file = FT_proc_full_path, envir = globalenv())
      if (verbose) {
        message(paste("   Processed Full Fish Ticket Data 2002-", YEAR.DATA, sep = ""))
        message("     with the following R.object name:")
        message("       FTOrig_Proc_Full")
        message("   is now loaded")
      }
    }

    if ("WCGOP" %in% data_vec) { # Load OB pre/proc data
      if (verbose) message("WCGOP data is loading...")
      load(file = WCGOP_pre_path, envir = globalenv())
      load(file = WCGOP_proc_path, envir = globalenv())
      if (verbose) {
        message(paste("   WCGOP Data 2002-", YEAR.DATA, sep = ""))
        message("     with the following R.object names:")
        message("       OBOrig_Pre")
        message("       OBOrig_Proc")
        message("   are now loaded")
      }
    }

    if ("WCGOP_pre" %in% data_vec) { # Load OB pre data
      if (verbose) message("WCGOP_pre data is loading...")
      load(file = WCGOP_pre_path, envir = globalenv())
      if (verbose) {
        message(paste("   Pre-processed WCGOP Data 2002-", YEAR.DATA, sep = ""))
        message("     with the following R.object name:")
        message("       OBOrig_Pre")
        message("   is now loaded")
      }
    }

    if ("WCGOP_proc" %in% data_vec) { # Load OB proc data
      if (verbose) message("WCGOP_proc data is loading...")
      load(file = WCGOP_proc_path, envir = globalenv())
      if (verbose) {
        message(paste("   Processed WCGOP Data 2002-", YEAR.DATA, sep = ""))
        message("     with the following R.object name:")
        message("       OBOrig_Proc")
        message("   is now loaded")
      }
    }

    if ("WCGOP_pre_full" %in% data_vec) { # Load OB pre full data
      if (verbose) message("WCGOP_pre_full data is loading...")
      load(file = WCGOP_pre_full_path, envir = globalenv())
      if (verbose) {
        message(paste("   Pre-processed Full WCGOP Data 2002-", YEAR.DATA, sep = ""))
        message("     with the following R.object name:")
        message("       OBOrig_Pre_Full")
        message("   is now loaded")
      }
    }

    if ("WCGOP_proc_full" %in% data_vec) { # Load OB proc full data
      if (verbose) message("WCGOP_proc_full data is loading...")
      load(file = WCGOP_proc_full_path, envir = globalenv())
      if (verbose) {
        message(paste("   Processed Full WCGOP Data 2002-", YEAR.DATA, sep = ""))
        message("     with the following R.object name:")
        message("       OBOrig_Proc_Full")
        message("   is now loaded")
      }
    }

    if ("ASHOP" %in% data_vec) { # Load ASHOP data
      if (verbose) message("ASHOP data is loading...")
      load(file = ASHOP_pre_path, envir = globalenv())
      load(file = ASHOP_proc_path, envir = globalenv())
      if (verbose) {
        message(paste("   ASHOP Data 2002-", YEAR.DATA, sep = ""))
        message("     with the following R.object names:")
        message("       ASOrig_Pre")
        message("       ASOrig_Proc")
        message("   are now loaded")
      }
    }

    if ("ASHOP_proc" %in% data_vec) { # Load ASHOP proc data
      if (verbose) message("ASHOP_proc data is loading...")
      load(file = ASHOP_proc_path, envir = globalenv())
      if (verbose) {
        message(paste("   Processed ASHOP Data 2002-", YEAR.DATA, sep = ""))
        message("     with the following R.object name:")
        message("       ASOrig_Proc")
        message("   is now loaded")
      }
    }

    if ("ASHOP_pre" %in% data_vec) { # Load ASHOP pre data
      if (verbose) message("ASHOP_pre data is loading...")
      load(file = ASHOP_pre_path, envir = globalenv())
      if (verbose) {
        message(paste("   ASHOP Data 2002-", YEAR.DATA, sep = ""))
        message("     with the following R.object name:")
        message("       ASOrig_Pre")
        message("   is now loaded")
      }
    }

    if ("BIO" %in% data_vec) { # Load BIO data
      if (verbose) message("BIO data is loading...")
      load(file = BIO_pre_path, envir = globalenv())
      load(file = BIO_proc_path, envir = globalenv())
      if (verbose) {
        message(paste("   Biological Data 2002-", YEAR.DATA, sep = ""))
        message("     with the following R.object names:")
        message("       BIOOrig_Pre")
        message("       BIOOrig_Proc")
        message("   are now loaded")
      }
    }

    if ("BIO_proc" %in% data_vec) { # Load proc BIO data
      if (verbose) message("BIO data is loading...")
      load(file = BIO_proc_path, envir = globalenv())
      if (verbose) {
        message(paste("   Processed Biological Data 2002-", YEAR.DATA, sep = ""))
        message("     with the following R.object name:")
        message("       BIOOrig_Proc")
        message("   is now loaded")
      }
    }

    if ("BIO_pre" %in% data_vec) { # Load pre BIO data
      if (verbose) message("BIO data is loading...")
      load(file = BIO_pre_path, envir = globalenv())
      if (verbose) {
        message(paste("   Pre-processed Biological Data 2002-", YEAR.DATA, sep = ""))
        message("     with the following R.object name:")
        message("       BIOOrig_Pre")
        message("   is now loaded")
      }
    }

    if ("LBK" %in% data_vec) { # Load LB data
      if (verbose) message("LBK data is loading...")
      load(file = LBK_path, envir = globalenv())
      if (verbose) {
        message(paste("   Logbook Data 2002-", (YEAR.DATA - 1), sep = ""))
        message("     with the following R.object name:")
        message("       LBK.final")
        message("   is now loaded")
      }
    }

    if ("EM" %in% data_vec) { # load EM data
      if (verbose) message("EM data is loading...")
      load(file = EM_pre_path, envir = globalenv())
      load(file = EM_proc_path, envir = globalenv())
      if (verbose) {
        message(paste("   EM Logbook Data 2015-", YEAR.DATA, sep = ""))
        message("     with the following R.object names:")
        message("       EMOrig_Pre")
        message("       EMOrig_Proc")
        message("   are now loaded")
      }
    }

    if ("EM_proc" %in% data_vec) { # load EM proc data
      if (verbose) message("EM_proc data is loading...")
      load(file = EM_proc_path, envir = globalenv())
      if (verbose) {
        message(paste("   Processed EM Logbook Data 2015-", YEAR.DATA, sep = ""))
        message("     with the following R.object name:")
        message("       EMOrig_Proc")
        message("   is now loaded")
      }
    }

    if ("EM_pre" %in% data_vec) { # load EM data
      if (verbose) message("EM data is loading...")
      load(file = EM_pre_path, envir = globalenv())
      if (verbose) {
        message(paste("   EM Logbook Data 2015-", YEAR.DATA, sep = ""))
        message("     with the following R.object name:")
        message("       EMOrig_Pre")
        message("   is now loaded")
      }
    }
  }
}
