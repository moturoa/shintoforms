



#library(shintoforms)

devtools::load_all()

options(shintodb_config_file = "c:/repos/wbm3.0/conf/config.yml")
con <- shintodb::connect("DEMO")

.juno <- shintoforms::formClass$new(db_connection = con,
                                    class_type = 'juno',
                                    schema = "wonmon",
                                    def_table = "data_definition",
                                    def_columns = list(
                                      id_form = "id",
                                      column_field = "column_name",
                                      label_field = "column_label",
                                      type_field = "column_type",
                                      options = "category_labels",
                                      order_options = "category_order",
                                      colors = "category_colors"
                                    ))

.juno$get_input_fields()



options(shiny.fullstacktrace = TRUE)

ui <- softui::simple_page(
  
  formAdminUI("test",
              tab_deleted_input_fields = FALSE,
              option_layout = FALSE,
              option_add_field = TRUE,
              option_delete_field = FALSE)
  
)

server <- function(input, output, session) {
  
  callModule(formAdminModule, "test", .reg = .juno, 
             allow_delete_option = TRUE,
             option_layout = FALSE,
             show_fields = c(
               "Kenmerk" = "column_field", 
               "Naam" = "label_field", 
               "Type" = "type_field", 
               "Keuzewaarden" = "options", 
               #"Volgorde keuzew." = "order_options", 
               "Kleuren" = "colors"
             ))
  
}

shinyApp(ui, server)


