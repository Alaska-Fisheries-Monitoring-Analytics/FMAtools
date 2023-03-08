#' Fix UTF-8 Encoding in a data.frame
#'
#' \code{fix_utf8} converts all character columns of a data.frame to UTF-8.
#'
#' @param dat A data.frame.
#'
#' @return Returns a data.frame with all character columns UTF-8 encoded.
#'
#' @seealso \code{utf8::as_utf8()}
#'
#' @examples
#' \dontrun{
#' df_utf8 <- fix_utf8(dat_not_utf8)
#' }
#'
#' @export

fix_utf8 <- function(dat) {
  tmpenc <- rep(NA, ncol(dat))
  for (i in 1:ncol(dat)) {
    if (is.character(dat[, i])) {
      dat[!is.na(dat[, i]), i] <- utf8::as_utf8(dat[!is.na(dat[, i]), i])
      tmpenc[i] <- unique(stringi::stri_enc_isutf8(dat[!is.na(dat[, i]), i]))
    } else {
      next
    }
  }
  if (unique(tmpenc[!is.na(tmpenc)])) {
    print("All UTF-8")
  } else {
    print("There was an error")
  }
  return(dat)
}
