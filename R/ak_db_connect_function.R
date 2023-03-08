#' Connect to AFSC database using \code{odbc}
#'
#' \code{ak_db_connect} creates a connection to the AFSC database.
#'
#' This function takes no arguments.
#'
#' @details
#' \code{ak_db_connect} takes no arguements.  It will ask for your AFSC username
#'  and password and then create and test a connection to the database.
#'
#'
#' @note The function uses \code{odbc} to connect to the database. Currently
#' only connects to AFSC Oracle database. Might be modified in the future to
#' include other databases, e.g., AKRO.
#' Note also that this will *not* create a connection in Rstudio.
#'
#' @seealso \code{odbc::dbConnect, odbc::dbGetQuery()}
#'          \code{rstudioapi::askForPassword()}
#'
#' @return A connection to the AFSC database.
#'
#' @examples
#' \dontrun{
#' con1 <- ak_db_connect()
#' tst <- odbc::dbGetQuery(con1, "SELECT * FROM norpac.atl_haul
#'                                                  FETCH FIRST 10 ROWS ONLY")
#' nrow(tst)
#' }
#'
#' @export

ak_db_connect <- function() {
  #-----------------------------------------------#
  # Define odbc connection to the Oracle database
  # default connection schema = your schema, with ability to connect to NORPAC
  #     and other schemas within AFSC database
  # Change "AFSC" to whatever the TNS alias is, that is defined on your machine,
  #   in the 'ORACLE' directory.  Contact IT if assistance is needed!

  channel <- odbc::dbConnect(odbc::odbc(), "AFSC",
    UID    = rstudioapi::askForPassword("Enter your NORPAC Username: "),
    PWD    = rstudioapi::askForPassword("Enter your NORPAC Password: ")
  )

  #-------------------#
  # Test with a query
  test_haul_query <-
    odbc::dbGetQuery(
      channel,
      "SELECT * FROM norpac.atl_haul WHERE rownum = 1"
    )

  if (nrow(test_haul_query) == 1) {
    message("Success - you are connected!")
  } else {
    message("Sorry, something went wrong. Try again.")
  }
  return(channel)
}
