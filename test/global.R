
# Packages
devtools::load_all()
# library(shintoregistraties)


source("preload/load_packages.R")
shintoshiny::check_for_version("softui","0.4")
shintoshiny::check_for_version("shintousers","0.4-2")
shintoshiny::check_for_version("shintocatman","0.1-4")


#shintoshiny::load_modules()


# Database connection
.reg <- registrationClass$new(sqlite = "data/registraties.sqlite",
                              table = "formulier_velden")

# End.
onStop(function() {
  .reg$close()
  #dbDisconnect(con)
})




