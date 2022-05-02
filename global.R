
# Packages
source("preload/load_packages.R")
shintoshiny::check_for_version("softui","0.4")
shintoshiny::check_for_version("shintousers","0.4-2")
shintoshiny::load_modules()

# Sources
source("R/registrationDataWarehouseR6.R")

# Database connection

.reg <- registrationDataWarehouseR6$new(sqlite = "data/registraties.sqlite")


# End.
onStop(function() {
  .reg$close()
  #dbDisconnect(con)
})




