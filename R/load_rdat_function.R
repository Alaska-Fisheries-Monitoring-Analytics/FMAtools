#' Load and Rename Object in Single Step
#'
#' \code{load_rdat} loads an object and assigns a new name in a single step
#'
#' @param fileName The path and filename to the object to be loaded
#'
#' @return Loads a object into .GlobalEnv and assigns a name
#'
#' @seealso \code{load}, \code{env}
#'
#' @examples
#' \dontrun{
#' d <- load_rdat("~/blah/somegood.Rda")
#' }
#'
#' @export

load_rdat <- function(fileName) {
  env <- new.env()
  nm <- load(fileName, env)[1]
  env[[nm]]
}
