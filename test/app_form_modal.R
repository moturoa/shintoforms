


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
        nieuweRegistratieUI(session$ns("nieuweRegistratie"))
      )
    )
    
    new_form_saved_ping <- callModule(nieuweRegistratieModule, "nieuweRegistratie", 
                                      .reg = .reg, 
                                      ping_update = reactive(NULL),
                                      current_user = "devuser",
                                      callback_confirm = function()removeModal(),
                                      callback_cancel = function()removeModal())
    
  })
  
}

shinyApp(ui, server)
