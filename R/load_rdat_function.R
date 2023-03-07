#' Load and Rename Object in Single Step
#'
#' \code{load_rdat} loads an object and assigns a new name in a single step
#'
#' @param flnm The path and filename to the object to be loaded
#'
#' @return Loads a object into .GlobalEnv and assigns a name
#'
#' @seealso \code{load}, \code{get}
#'
#' @examples
#' \dontrun{
#' d <- load_rdat("~/blah/somegood.Rda")
#' }
#'
#' @export


load_rdat <- function(fileName) {
  # loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}
