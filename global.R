
# Packages
source("preload/load_packages.R")
shintoshiny::check_for_version("softui","0.4")
shintoshiny::check_for_version("shintousers","0.4-2")
shintoshiny::load_modules()

# Sources


# data definition ("admin tables")
#.db <- dataDefinition$new(sqlite = "data/db.sqlite")

# End.
#onStop(function() {
  #.db$close()
  #dbDisconnect(con)
#})




