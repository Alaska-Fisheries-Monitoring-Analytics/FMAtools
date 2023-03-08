# Helper functions
#' Which Names Are Missing From Data.Frame
#'
#' A low level wrapper to check for differences in names between two data.frames.
#'
#' @param df1 first data frame
#' @param df2 second data frame
#'
#' @details
#' Checks for names of columns in each data.frame that does not appear in the other data.frame.  Prints the missing column names for each data frame.
#' @return Returns character vector
#'
#' @export
chk_missing_nms <- function(df1, df2) {
  print("Found in df1, missing in df2:")

  print(names(df1)[!names(df1) %in% names(df2)])

  print("Found in df2, missing in df1:")

  print(names(df2)[!names(df2) %in% names(df1)])
}

#' Round Numeric Columns of a data.frame
#'
#' Rounds only numeric (i.e., \code{is.double()== TRUE}) columns of a data.frame to the level specified in \code{digits}.
#'
#' @param df Data.frame with numeric columns for rounding.
#' @param digits the number of digits to round, passed to \code{base::round()}.
#' @param prnt Do you want to print these values in a table format? If \code{FALSE} (default), no trailing zeros will be printed.
#' @param sym A character vector of (length == 1) used as a symbol for any 'NA's.
#'
#' @details
#' Only columns that satisfy \code{is.double() == TRUE} will be rounded.  Thus, integers are not rounded.
#'
#' @return Returns a data.frame with numeric columns rounded to \code{digits}
#'
#' @export

round_df <- function(df, digits, prnt = FALSE, sym = "NA") {
  nums <- vapply(df, is.double, FUN.VALUE = logical(1))
  if (!any(nums)) {
    return(df)
  }

  nm.df <- names(nums[nums == T])

  if (prnt == F) {
    df[, nums] <- round(df[, nums], digits = digits)
  } else {
    df[, nums] <- unlist(lapply(df[, nums], FUN = function(x) {
      sprintf(paste("%.", digits, "f", sep = ""), round(x, digits = digits) + 0)
    }))
    if (sym != "NA") {
      for (i in 1:length(nm.df)) {
        df[df[, nm.df[i]] == "NA", nm.df[i]] <- as.character(sym)
      }
    }
  }
  return(df)
}

#' Is Any NA Present?
#'
#' A low level wrapper for \code{any(is.na(x))}.
#'
#' @param x any R object that can be checked for NA
#'
#' @details
#' Wrapper for \code{any(is.na(x))}.
#' @return Returns logical
#'
#' @examples
#' \dontrun{
#' neisna(x)
#' }
#'
#' @export

neisna <- function(x) {
  any(is.na(x))
}


#' Quick Data Frame
#'
#' Function to return a data.frame mainly from piping
#'
#' @param ... data.frame, see details.
#' @param saf strings as factors? Default = FALSE
#'
#' @details
#' A low level function, used in many of the functions in this package. Usage is to return a data.frame without factors from piping in the tidyverse.
#'
#' @return Returns the \code{data.frame}, with strings as characters, mainly from piping in the tidyverse.
#'
#' @examples
#' \dontrun{
#' df <- df %>%
#'   filter(...) %>%
#'   mutate(...) %>%
#'   select(...) %>%
#'   qdf()
#' }
#'
#' @export

qdf <- function(..., saf = FALSE) {
  data.frame(..., stringsAsFactors = saf)
}

#' Quick Data Read CSV
#'
#' A simple wrapper function for \code{utils::read.csv(file =..., header = TRUE, stringsAsFactors = FALSE)}
#'
#' @param pth path to a \code{.csv} file.
#'
#' @return Returns the \code{.csv}, headers and with strings as characters.
#'
#' @examples
#' \dontrun{
#' df <- qdr(pth = "/path/to/file/filename.csv")
#' }
#'
#' @export

qdr <- function(pth) {
  utils::read.csv(file = pth, header = TRUE, stringsAsFactors = FALSE)
}

#' Write Multiple CSVs from a list
#'
#'  \code{write_csv_list()} Takes a list and writes each element to a csv.
#'
#' @param liste A list whose elements will be written to a csv.
#' @param fnms A character vector of file names (which can include a path, if desired) for each csv, where \code{length(fnms) == length(liste)}.
#' @param rnms Optional logical specifying whether or not to add the \code{row.names} to the output file. Default = \code{FALSE}.
#'
#' @return Nothing.
#'
#' @examples
#' \dontrun{
#' write_csv_list(liste = listofdf, fnms = vectoroffilenames)
#' }
#'
#' @export

write_csv_list <- function(liste, fnms, rnms = FALSE) {
  lapply(
    1:length(liste),
    function(x) {
      utils::write.csv(liste[[x]], file = fnms[x], row.names = rnms)
    }
  )
}

#' Sort Columns of Data.Frame by Names of Another Data.Frame
#'
#' Function to return a data.frame with columns ordered in the same manner as another data.frame, in prep for \code{rbind()}
#'
#' @param x data.frame whose columns will be sorted.
#' @param y data.frame whose columns will be used as template for sorting.
#' @param suppress suppress the printing of column names? Default = FALSE
#'
#' @details
#' A low level function, used in many of the functions in this package. Usage is to return a data.frame (\code{x}) whose columns are sorted according to the template data.frame columns (\code{y}).
#'
#' @return Returns \code{x}, with columns sorted according to \code{y}.
#'
#' @examples
#' \dontrun{
#' df2 <- colord(df2, df1)
#' }
#'
#' @export

colord <- function(x, y, suppress = FALSE) {
  col.ord <- names(y)
  x <- x[, col.ord]
  if (suppress == FALSE) {
    print(colnames(x))
    print(colnames(y))
  }
  return(x)
}

#' Makes Column Names of Output Data.Frame Consistent
#'
#' \code{namefix} fixes the column names for consistency across functions.
#'
#' @param df data.frame whose columns names will be fixed.
#'
#' @details
#' A low level function, used in many packages. Usage is to fix column names for consistency across functions.
#'
#' @return Returns \code{df}, with columns names fixed.
#'
#' @examples
#' \dontrun{
#' names(df) <- namefix(df)
#' }
#'
#' @export

namefix <- function(df) {
  tmp <- names(df) %>%
    tolower() %>%
    gsub("\\.", "_", .)
  return(tmp)
}

#' A Helper Function That Sorts the Object Names
#'
#' @description
#' \code{scol} sorts and prints the column names in alphabetical order.
#'
#' @param x any object that has names.
#'
#' @return Names sorted alphabetically.
#'
#' @examples
#' \dontrun{
#' scol(x)
#' }
#'
#' @export

scol <- function(x) {
  sort(names(x))
}


#' Makes Column Classes of two Data.Frames Consistent Across Common Column Names
#'
#' \code{matchColClasses} identifies columns that share the same name,, uses the master data frame (df1) to identify the shared columns classes, and then re-assigns column classes in df2 to match df1.
#'
#' @param df1 data.frame whose columns classes will be used as reference.
#' @param df2 data.frame with at least 1 column name that matches a column name in df1. For all column names that match, columns classes will be  modified to match corresponding column classes in df1.
#'
#' @details
#' A low level function, used to ensure proper joins between data.frames within functions.
#'
#' @return Returns \code{df2}, with columns classes modified to match classes of shared column names in df1.
#'
#' @examples
#' \dontrun{
#' df2 <- matchColClasses(df1, df2)
#' }
#'
#' @export


match_col_classes <- function(df1, df2) {
  shared_col_names <- names(df1)[names(df1) %in% names(df2)]
  shared_col_types <- sapply(df1[, shared_col_names], class)

  for (n in shared_col_names) {
    class(df2[, n]) <- shared_col_types[n]
  }

  return(df2)
}



#' Creates Character Vector from ...
#'
#' \code{dots2char} converts \code{...} into a character vector.
#'
#' @param ... from a function.
#'
#' @return Returns a character vector
#'
#' @export
#'
dots2char <- function(...) {
  chr <- as.character(unlist(rlang::exprs(...)))
  return(chr)
}

#' Order Unique Values
#'
#' \code{uni()} Pulls the unique values (using \code{unique()}) and orders them (using \code{sort()}).
#'
#' @param .x an R object with a class or a numeric, complex, character or logical vector.
#'
#' @return Returns a vector of ordered unique values.
#'
#' @export
#'
uni <- function(.x) {
  out <- sort(unique(.x))
  return(out)
}

