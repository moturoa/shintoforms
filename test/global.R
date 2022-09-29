
# Packages
devtools::load_all()
#library(shintoforms)

source("preload/load_packages.R")
shintoshiny::check_for_version("softui","0.4")
shintoshiny::check_for_version("shintousers","0.4-2")
shintoshiny::check_for_version("shintocatman","0.1-5")

# Database connection
.reg <- formClass$new(sqlite = "data/registraties.sqlite",
                              def_table = "formulier_velden",
                              def_columns = list(
                                id_form = "id_formulierveld",
                                column_field = "kolomnaam_veld",
                                label_field = "label_veld",
                                type_field = "type_veld",
                                order_field = "volgorde_veld",
                                form_section = "formulier_sectie",
                                visible = "zichtbaar",
                                date_deleted = "datum_uitgeschakeld",
                                options = "opties",
                                order_options = "volgorde_opties",
                                colors = "kleuren",
                                removable = "kan_worden_verwijderd"
                                # make_filter = "make_filter",
                                # tooltip = "tooltip"
                              ),
                              data_table = "registraties",
                              data_columns = list(
                                id = "id_registratie",
                                name = "naam_registratie",
                                time_created = "invoerdatum",
                                time_modified = "wijzigdatum",
                                user = "user_id",
                                status = "status"
                              ))

# End.
onStop(function() {
  .reg$close()
})




