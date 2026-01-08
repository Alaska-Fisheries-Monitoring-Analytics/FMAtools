# This script is loaded when library() loads FMA tools and does a quick check to see if your package is up-to-date
# The funky name of the script `zzz.R` ensures that the script is processed last.

.onAttach <- function(libname, pkgname) {
  if (interactive()) {

    local_sha <- packageDescription(pkgname)$RemoteSha
    if (is.null(local_sha)) return()

    # Define your base repo path
    repo_path <- "Alaska-Fisheries-Monitoring-Analytics/FMAtools"

    # Use that path to build the API URL
    github_api_url <- paste0("https://api.github.com/repos/", repo_path, "/commits/main")

    try({
      con <- url(github_api_url)
      on.exit(close(con))

      suppressWarnings(line <- readLines(con, n = 15))
      latest_sha <- gsub('.*"sha": "([^"]+)".*', "\\1", line[grep('"sha"', line)[1]])

      if (substr(local_sha, 1, 7) != substr(latest_sha, 1, 7)) {
        packageStartupMessage("── Update Available ───────────────────────────────────────")
        packageStartupMessage("  A newer version of ", pkgname, " is available.")
        # We use the repo_path variable here too!
        packageStartupMessage("  Run: devtools::install_github('", repo_path, "')")
        packageStartupMessage("───────────────────────────────────────────────────────────")
      }
    }, silent = TRUE)
  }
}
