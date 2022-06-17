
# Packages
devtools::load_all()
# library(shintoregistraties)


source("preload/load_packages.R")
shintoshiny::check_for_version("softui","0.4")
shintoshiny::check_for_version("shintousers","0.4-2")
shintoshiny::check_for_version("shintocatman","0.1-5")

# Database connection
.reg <- formClass$new(sqlite = "data/registraties.sqlite",
                              def_table = "formulier_velden",
                              data_table = "registraties")

# End.
onStop(function() {
  .reg$close()
})




