
#' Formulier Shiny Module
#' @rdname formulier
#' @export
formPageUI <- function(id,
                        title = "Formulier",
                        icon = softui::bsicon("ui_checks"),
                        header_ui = NULL,
                        tag = "Opties",
                        new_existing_labels = c("Nieuwe registratie","Registratie bewerken"),
                        edit_button = FALSE  # not yet implemented .... 
                        ){
  
  ns <- NS(id)
  
  softui::fluid_page(
    softui::fluid_row(
      
       softui::box(title = title, icon = icon, tag = tag,
         
          header_ui,
          
          softui::action_button(ns("btn_new_registration"), new_existing_labels[1],
                                class = "btn-lg",
                                status = "success", icon = bsicon("plus-lg")),
          
          if(edit_button){
            softui::action_button(ns("btn_old_registration"), new_existing_labels[2],
                                class = "btn-lg",
                                status = "info", icon = bsicon("check"))
          }
          
       )
      
    ),
    
    uiOutput(ns("ui_new_registration"))

  )
  
  
}


#' @rdname formulier
#' @export
formPageModule <- function(input, output, session, .reg = NULL, ping_update = reactive(NULL),
                            current_user, ...){
  
  show_form <- reactiveVal(FALSE)
  trigger <-reactiveVal() 
  observeEvent(input$btn_new_registration,ignoreInit = TRUE, {
    print("AAAA")
    show_form(TRUE)
    trigger(runif(1))
  })
  
  observeEvent(new_form_saved_ping(), {
    show_form(FALSE)
  })
  
  output$ui_new_registration <- renderUI({
    
    req(show_form())
    
    softui::box(
       width = 12,
       title = "",
       tag = "Formulier",
       formUI(session$ns("nieuweRegistratie"))
    )
  })
  
  
  new_form_saved_ping <- callModule(formModule, "nieuweRegistratie", 
                                    .reg = .reg, 
                                    trigger= trigger,
                                    ping_update = ping_update,
                                    current_user = current_user, ...)
 
return(new_form_saved_ping)
}





