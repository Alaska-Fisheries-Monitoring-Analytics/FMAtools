# This script is loaded when library() loads FMA tools and does a quick check to see if your package is up-to-date
# The funky name of the script `zzz.R` ensures that the script is processed last.

.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    # 1. Get local SHA
    local_sha <- packageDescription(pkgname)$RemoteSha
    if (is.null(local_sha)) return()

    # Define your base repo path
    repo_path <- "Alaska-Fisheries-Monitoring-Analytics/FMAtools"

    # Use that path to build the API URL
    github_api_url <- paste0("https://api.github.com/repos/", repo_path, "/commits/main")

    try({
      con <- url(github_api_url)
      on.exit(close(con))

      # Read everything available and collapse it into one single string
      raw_text <- paste(readLines(con, warn = FALSE), collapse = "")

      # 2. Extract the SHA using a more precise pattern
      # This looks for "sha": followed by a 40-character hex string
      latest_sha <- gsub('.*"sha"\\s*:\\s*"([a-f0-9]{40})".*', "\\1", raw_text)

      # 3. Final Check: Ensure we actually extracted a valid 40-char hex string
      # If the gsub fails, it returns the whole raw_text, so we check the length.
      if (nchar(latest_sha) == 40) {
        if (substr(local_sha, 1, 7) != substr(latest_sha, 1, 7)) {
          packageStartupMessage("── Update Available for ", pkgname, " ──")
          packageStartupMessage("  Newer commits are available on GitHub.")
          packageStartupMessage("  Run: devtools::install_github('", repo_path, "')")
        }
      }
    }, silent = TRUE)
  }
}
