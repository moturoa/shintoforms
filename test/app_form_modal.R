


withr::with_dir("test", source("global.R"))

ui <- softui::simple_page(
  shinytoastr::useToastr(),
  
  softui::action_button("btn_do_reg", "Signaal invoeren", status = "success")
  
)

server <- function(input, output, session) {
  
  
  observeEvent(input$btn_do_reg,{
    
    
    showModal(
      softui::modal(
        title = "Signaal invoeren", size = "xl",confirm_button = FALSE, close_button = FALSE,
        formUI(session$ns("nieuweRegistratie"))
      )
    )
    
    new_form_saved_ping <- callModule(formModule, "nieuweRegistratie", 
                                      .reg = .reg, 
                                      ping_update = reactive(NULL),
                                      current_user = "devuser",
                                      data = list(
                                        aantal_brommers = 100,
                                        datum_inschrijving_gemeente = "2022-1-10",
                                        kolom_rechts = "Dit is in een modal gemaakt",
                                        crimineel_verleden = FALSE
                                      ),
                                      callback_confirm = function()removeModal(),
                                      callback_cancel = function()removeModal())
    
  })
  
}

shinyApp(ui, server)
