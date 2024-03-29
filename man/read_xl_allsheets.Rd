% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_xl_allsheets_function.R
\name{read_xl_allsheets}
\alias{read_xl_allsheets}
\title{Function to Import All Sheets from MSExcel File}
\usage{
read_xl_allsheets(filename, tibble = FALSE)
}
\arguments{
\item{filename}{A MSExcel file name (with path)}

\item{tibble}{A logical that determines if the output is a tibble (TRUE) or a regular R data.frame (default = FALSE)}
}
\value{
Returns a list of objects (likely data.frames) from multiple sheets in an MSExcel spreadsheet.
}
\description{
\code{read_xl_allsheets} reads in all the sheets within a single MSExcel file.
}
\examples{
\dontrun{
mysheets <- read_excel_allsheets("foo.xls")
}

}
\seealso{
\code{readxl::excel_sheets}
}
