#' Query a database
#'
#' Run a database query with an automatic check for your connection.
#'
#' @param query the character string
#' @param dsn the dsn as specified in your .Renviron file
#'
#' @details
#' Runs a database query. This function will automatically see if you have an open connect to the database, and if not,
#' will re-establish the connection.
#'
#' Uses the credential stored in your local `.Renviron file`. By default, this function assumes you're connecting to the
#' NORPAC database but the `dsn` argument can be used to specify another alias.
#'
#' As a reminder, you can edit your `.Renviron` file via `usethis::edit_r_environ()`.
#'
#' @return Returns the output of from you query, typically a table. Also assigns your `dsn` to the global environment.
#'
#' @seealso `usethis::edit_r_environ()`

#' @examples
#' \dontrun{
#'    db_query(query = paste0("SELECT * FROM norpac.atl_lov_vessel"))
#' }
#'
#' @export
db_query <- function(query, dsn = "channel_afsc") {
  # DEBUG: dsn <- "channel_afsc"; query <- paste0("SELECT * FROM norpac.atl_lov_vessel")

  # First, check to see if the connection exists in the global environment, and if so if it is active.
  conn_exists <- exists(dsn, envir = .GlobalEnv)
  if(conn_exists) {
    # Run a simple query to test if the connection is active. If it throws an error, the connection is inactive.
    conn_active <- tryCatch({
      dbGetQuery(get(dsn), paste0("SELECT sysdate from dual"))
      TRUE
      }, error = function(e) return(F)
    )
  } else conn_active <- F

  # If the connection does not exists or exists but is inactive, re-connect.
  if(!conn_exists | !conn_active) {
    cat("Re-establishing connection")
    # Assign the dsn to the global environment, drawing the connection from the .Renviron file
    assign(as.character(dsn), eval(parse(text = Sys.getenv(dsn))), envir =  .GlobalEnv)
  }

  # Run the query
  DBI::dbGetQuery(conn = get(dsn), statement = query)

}
