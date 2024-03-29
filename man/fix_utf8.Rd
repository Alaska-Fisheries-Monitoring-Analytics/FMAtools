% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fix_utf8_function.R
\name{fix_utf8}
\alias{fix_utf8}
\title{Fix UTF-8 Encoding in a data.frame}
\usage{
fix_utf8(dat)
}
\arguments{
\item{dat}{A data.frame.}
}
\value{
Returns a data.frame with all character columns UTF-8 encoded.
}
\description{
\code{fix_utf8} converts all character columns of a data.frame to UTF-8.
}
\examples{
\dontrun{
df_utf8 <- fix_utf8(dat_not_utf8)
}

}
\seealso{
\code{utf8::as_utf8()}
}
