


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
              option_add_field = FALSE,
              option_delete_field = FALSE)

)

server <- function(input, output, session) {
  
  callModule(formAdminModule, "test", .reg = .juno, allow_delete_option = TRUE)
  
}

shinyApp(ui, server)



