
#' Nieuwe registratie Shiny module
#' @rdname registratie
#' @export
formUI <- function(id){
  
  ns <- NS(id)
  
  softui::fluid_page(

    softui::fluid_row(
      column(6,
        uiOutput(ns("ui_input_left"))
      ),
      column(6,
        uiOutput(ns("ui_input_right"))
      )
    ),
    
    softui::fluid_row(class = "justify-content-end",
      
      tags$hr(),
     
      column(6,
         softui::action_button(ns("btn_cancel"), 
                               "Annuleren", 
                               icon = bsicon("x-lg"), 
                               status = "danger"),      
         softui::action_button(ns("btn_register_new_signal"), 
                      "Opslaan", 
                      icon = bsicon("cloud-arrow-up"), 
                        status = "success")
      )
    )
  )
   
  
}

#' @rdname registratie
#' @export
formModule <- function(input, output, session, .reg = NULL, 
                                    ping_update = reactive(NULL),
                                    current_user, 
                                    data = NULL,
                                    callback_confirm = function(){},
                                    callback_cancel = function(){}) {
  
  ns <- session$ns
  
  nieuwe_registratie_ping <- reactiveVal()
  
  # reactive maken zodat ie update als er iets wordt veranderd in admin, zie admin scherm hoe dat moet. 
  cfg_left <- reactive({
    ping_update()
    .reg$get_form_fields(1)
  })
  
  cfg_right <- reactive({
    ping_update()
    .reg$get_form_fields(2)
  })
  
  
  output$ui_input_left <- renderUI({

    formSectionModuleUI(session$ns("form_left"), cfg = cfg_left(), .reg = .reg, data = data)
    
  })
  
  output$ui_input_right <- renderUI({
    
    formSectionModuleUI(session$ns("form_right"), cfg = cfg_right(), .reg = .reg, data = data)
    
  })
  
  edit_left <- callModule(formSectionModule, "form_left", cfg = cfg_left, .reg = .reg)
  edit_right <- callModule(formSectionModule, "form_right", cfg = cfg_right, .reg = .reg)
  
  
  edits <- reactive({
    
    req(edit_left())
    
    c(lapply(edit_left(), function(x)x()),
      lapply(edit_right(), function(x)x()))
    
  })
  
  observeEvent(input$btn_register_new_signal, {
    
    showModal(
      softui::modal(
        title = "Opslaan",
        id_confirm = "btn_confirm_new_registration",
      
        tags$p("Je gaat deze registratie opslaan."),
        tags$p("Geef deze registratie een naam zodat je deze terug kunt vinden in het systeem."),
        textInput(session$ns("txt_registration_name"), NULL, value = "")
      )
    )
    
  })
  
  
  out_ping <- reactiveVal()
  observeEvent(input$btn_cancel, {
    out_ping(runif(1))
    callback_cancel()
  })
  
  observeEvent(input$btn_confirm_new_registration, {
   
    resp <- .reg$write_new_registration(edits(), input$txt_registration_name, current_user)
    if(resp){
      toastr_success("Registratie opgeslagen")
    } else {
      toastr_error("Er is een fout opgetreden")
    }
    
    out_ping(runif(1))
    
    callback_confirm()
  })
  
  
  
return(out_ping)
}





  
  


#---- Testing

test_formModule <- function(){
  library(withr)
  with_dir("test", source("global.R"))
  
  
  
  library(shiny)
  
  ui <- softui::simple_page(
    
    softui::box(
      formUI("test")
    )
  )
  
  server <- function(input, output, session) {
    callModule(formModule, "test", .reg = .reg)
  }
  
  shinyApp(ui, server)
  
  
  
}














