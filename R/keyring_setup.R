# For those using R to directly access AFSC and AKFIN might I recommend the
# 'keyring' package for logging in without providing info on public domains
# (e.g., github)

# using keyring
# install.packages('keyring')
library(keyring)

# store password and user name for a database at a high level (aka not publicly accessible)
keyring::key_set_with_value(service="afsc", username="SULLIVANJ", password = "Zbfabfj$12!#$856")
keyring::key_set_with_value(service="akfin", username="jsullivan", password = "sculja22")

# the username and password can then be called with 
db <- "afsc"
keyring::key_list(db)$username
keyring::key_get(db, keyring::key_list(db)$username)

# this can be password on to a server connection:
DBI::dbConnect(odbc::odbc(),
               dsn = db,
               uid = keyring::key_list(db)$username,
               pwd =  keyring::key_get(db, keyring::key_list(db)$username))

db <- "akfin"
keyring::key_list(db)$username
keyring::key_get(db, keyring::key_list(db)$username)

DBI::dbConnect(odbc::odbc(),
               db,
               uid = keyring::key_list(db)$username,
               pwd =  keyring::key_get(db, keyring::key_list(db)$username))

# when your afsc pwd is updated you can easily change it using 
keyring::key_set_with_value(service=db, username="KINGHAMA", password = "MyPassword")
