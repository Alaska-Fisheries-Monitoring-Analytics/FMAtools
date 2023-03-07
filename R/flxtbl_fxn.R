#' Format Long Complicated Tables for P. halibut Report
#'
#' A wrapper function to format data for the executive summary table that summarizes Pacific halibut bycatch in the U.S. west coast groundfish fisheries.  Typically, this output will be used in a MS Word document (\code{.docx}).
#'
#' @param df A data.frame to be formatted
#' @param hdrs a named list of headers for the table, e.g., \code{list( OlD.Hdr1 = "New Header", old_headr2 = "New Header 2")}
#' @param themFUN  a flextable theme.  See \code{flextable} for options.
#' @param pd a padding value (0, 1) to add space between table entries.
#' @param pd_part which part of the table should be padded? one of 'all', 'body', 'header', 'footer'.
#' @param aln an alignment position (character),  one of 'left', 'right', 'center', 'justify'.
#' @param aln_part which part of the table should be aligned? one of 'all', 'body', 'header', 'footer'
#' @param bd_part which part of the table should appear in *bold*? one of 'all', 'body', 'header', 'footer'
#'
#' @details
#' A wrapper around \code{flextable}.
#'
#' @seealso \code{flextable::flextable()}
#'
#' @examples
#' \dontrun{
#' table_flex <- flxtbl_fxn(df)
#' }
#'
#' @export

flxtbl_fxn <- function(df = NULL, hdrs = NULL, themFUN = flextable::theme_zebra,
                       pd = 1, pd_part = "all", aln = "center", aln_part = "all",
                       bd_part = "header") {
  if (is.null(df)) {
    message("ERROR: Did you forget to enter a data set?\nIt is required.")
    stop()
  }
  if (is.null(hdrs)) {
    message("Warning: Did you forget to enter column headers?\nThey are *not* required, this is only a reminder.")
  }


  out <- flextable::flextable(df)
  if (!is.null(themFUN)) {
    out <- themFUN(out)
  }
  if (!is.null(hdrs)) {
    out <- flextable::set_header_labels(out, values = hdrs)
  }
  out <- flextable::bold(out, part = bd_part)
  out <- flextable::align(out, align = aln, part = aln_part)
  out <- flextable::padding(out, padding = pd, part = pd_part)

  return(out)
}
