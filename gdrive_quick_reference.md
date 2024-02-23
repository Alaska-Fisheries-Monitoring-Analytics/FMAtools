# Shared Google Drive - Quick Reference

These functions can be used to streamline our workflow by using our
Shared Google Drive as repository for all of our `.rdata` and `.rds`
files that we cannot store in our GitHub/GitLab repositories due to
restrictions with confidentiality or file size. These functions do not
support any other file types - all other file types should be contained
in the GitHub/GitLab repositories.

If you have not already, install the `FMAtools` package (and the
`devtools` package if you don't have it!), and load the library

``` r
devtools::install_github("Alaska-Fisheries-Monitoring-Analytics/FMAtools")
library(FMAtools)
```

In addition, you'll need to authorize the underlying `googldrive`
package to access your NOAA account's Google Drives. This following line
will open a browswer window to accomplish this. You'll only have to do
this once.

``` r
googledrive::drive_auth()
```

------------------------------------------------------------------------

# Using the Google Drive Tools

In short, all of the functions start with the prefix `gdrive_`. There
are three main functions:

-   [`gdrive_set_dribble()`](#gdrive_set_dribble) : to create an object
    that acts as our 'address' to a folder in our Shared GDrive
-   [`gdrive_upload()`](#gdrive_upload) : to upload files from our local
    to the specified Gdrive folder
-   [`gdrive_download()`](#gdrive_download) : to download files from the
    specified Gdrive folder to our local

The upload and download functions will only execute when necessary. This
prevents consecutive identical versions uploaded to the Gdrive or
overwriting local files when they are already up-to-date. They should be
paired with any `save()` and `load()` operations, respectively.

Three more functions are useful for reference:

-   [`gdrive_dir()`](#gdrive_dir) : prints the file structure of the
    Shared Gdrive
-   [`gdrive_ls()`](#gdrive_ls) : prints the contents a specified folder
    on the Shared Gdrive
-   [`gdrive_versions()`](#gdrive_versions) : prints the version history
    of a file on the Shared Gdrive

All of the above functions have detailed help pages for future
reference.

------------------------------------------------------------------------

### gdrive_set_dribble()

**Syntax:** `gdrive_set_dribble(gdrive_path, shared_id = "Analytics")`

Use this function to specify the Shared Google Drive folder for upload
and download operations. Specifying the folder path as a character
string to the `gdrive_path` argument. Use the `gdrive_dir()` reference
function to look up the folder path for your desired folder.

------------------------------------------------------------------------

### gdrive_upload()

**Syntax:** `gdrive_upload(local_path, gdrive_dribble)`

Uploads a single *local* file to a folder on the shared Gdrive specified
by the `gdrive_dribble` argument, defined using `gdrive_set_dribble`. If
the file already exists on the Gdrive, it will update the existing
version. This function will also ensure that all archived versions will
be retained forever (i.e., the 'Keep forever?' option will always be
checked).

------------------------------------------------------------------------

### gdrive_download()

**Syntax:** `gdrive_download(local_path, gdrive_dribble)`

`gdrive_download()` is syntactically identical to `gdrive_upload()`, but
the optional `ver` argument can be used to grant more control over what
data you want to save locally: omit it (leaving `ver` as `NULL`) will
attempt to download the most up-to-date version, or specifying a version
number will download the desired version.

Use the reference function `gdrive_version()` to view the version
history of a file.

------------------------------------------------------------------------

### gdrive_dir()

**Syntax:** `gdrive_dir(shared_id = "Analytics", folder = NULL)`

Prints the file structure of a shared drive. By default
(`shared_id = "Analytics"`), it will show the folder structure of the
*FMA Analytical Services Program* shared drive. Optionally, use the
`folder` argument by feeding a character string of the folder paths to
narrow down the search, .e.g. `Projects/Your_Project_Name`. Use the
`gdrive_path` column of the output as the input to
`gdrive_set_dribble()`.

------------------------------------------------------------------------

### gdrive_ls()

**Syntax:** `gdrive_ls(gdrive_dribble)`

Prints information about the files contained in the Google Drive folder.
Simply feed it the 'dribble' of the folder you want to peek into. It
does not print the name of enclosed folders.

------------------------------------------------------------------------

### gdrive_versions()

**Syntax:** `gdrive_versions(gdrive_file, gdrive_dribble)`

Display the version history of a file. The `gdrive_file` is the file
name of the file (no paths!) and `gdrive_dribble` is the dribble of the
Gdrive folder it resides in. Once you've identified the version you
need, save a local copy using `gdrive_download()` and its `ver`
argument!
