#' Format Table or Figure Captions for use with \code{captioner::captioner()}
#'
#' \code{format_caption} generates a prefix to be added to a table or figure caption.  See details.
#'
#' @param x A data.frame with the \code{table_tags} or \code{figure_tags} field. See details.
#' @param captype A character string indicating the type of captions to be formatted; either \code{"tab"} for tables or \code{"fig"} for figure captions.
#' @param caplist A list of table/figure captions.
#'
#' @details
#' \code{format_caption} is designed to a table or figure caption.  The format is a function defined by \code{captioner::captioner()} in \code{000_Setup.Rmd}.  \code{format_caption} attaches the \code{table_tags} or \code{figure_tags} field of \code{x} to the \code{name = tab:<table_tags>} or \code{name = fig:<figure_tags>} value passed to the \code{captioner::captioner()}.
#' For tables, \code{x} will always be \code{table_file_name_df} which holds \code{table_file_name_df$table_tags} and is produced in \code{/scripts/Table_File_Name_List.R}.
#' For figures, \code{x} will always be \code{figure_file_name_df} which holds \code{figure_file_name_df$figure_tags} and is produced in \code{/scripts/Figure_File_Name_List.R}.
#'
#' @note Typical usage of \code{format_caption()} to write captions to \code{phlbbycatcher::002_Table_Captions.Rmd} or \code{phlbbycatcher::003_Figure_Captions.Rmd}.
#'
#' @return Returns a list of captions formatted as a \code{captioner} function, as represented in \code{phlbbycatcher::000_Setup.Rmd}.
#'
#' @seealso \code{tables_2xl()},  \code{table_file_name_df},  \code{table_tags} \code{/scripts/Table_File_Name_List.R}, \code{captioner::captioner()}, \code{phlbbycatcher::000_Setup.Rmd}
#'
#' @examples
#' \dontrun{
#' list_of_formatted_captions <- format_captions(x = table_file_name_df)
#' }
#'
#' @export

format_caption <- function(x, captype, caplist) {
  # safety
  captype <- tolower(captype)
  if (!any(grepl("tab|fig", captype))) {
    stop("Please enter a correct value for 'captype':\n    tab = table captions\n    fig = figure captions")
  }


  if (grepl("(?i)tab", captype)) {
    pre_tag <- "tab"
  } else {
    pre_tag <- "fig"
  }

  tags_list <- x[, names(x)[grepl("^(?i).*?_tag(s)", names(x))]]

  caplist <- gsub("CAPTION: ", "", caplist)

  code_hdrs <- paste0(
    "<!-- ",
    gsub("\\.csv", "", x$table_file_names),
    " -->\n"
  )

  cptr_frmtd <- paste0(
    '`r table_nums(name = "',
    pre_tag, ":",
    tags_list, '", ',
    'caption = "',
    caplist, '")`\n'
  )

  cptr_frmtd <- paste0(code_hdrs, cptr_frmtd)

  return(cptr_frmtd)
}
