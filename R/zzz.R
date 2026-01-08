# This script is loaded when library() loads FMA tools and does a quick check to see if your package is up-to-date
# The funky name of the script `zzz.R` ensures that the script is processed last.

.onAttach <- function(libname, pkgname) {
  # This only runs during interactive sessions
  if (interactive()) {

    # 1. Get local SHA (pkgname is provided by R automatically)
    local_sha <- packageDescription(pkgname)$RemoteSha

    # If the package wasn't installed via GitHub, stop here
    if (is.null(local_sha)) return()

    # 2. Check GitHub (CHANGE 'user/repo' to your actual GitHub path)
    github_api_url <- "https://github.com/noaa-afsc/observer-deployment"

    try({
      # Use a short timeout so library() doesn't hang on poor connections
      con <- url(github_api_url)
      on.exit(close(con))

      # Read the first few lines of the API response
      suppressWarnings(line <- readLines(con, n = 15))

      # Extract the SHA string using regex
      latest_sha <- gsub('.*"sha": "([^"]+)".*', "\\1", line[grep('"sha"', line)[1]])

      # 3. Compare the first 7 characters of the SHAs
      if (substr(local_sha, 1, 7) != substr(latest_sha, 1, 7)) {
        packageStartupMessage(paste0("── Update Available for ", pkgname, " ──────────────────────────"))
        packageStartupMessage("  A newer version is available on GitHub.")
        packageStartupMessage(paste0("  Run: devtools::install_github('Alaska-Fisheries-Monitoring-Analytics/FMAtools')"))
        packageStartupMessage("──────────────────────────────────────────────────────────")
      }
    }, silent = TRUE)
  }
}
