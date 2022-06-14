
#' Formulier Shiny Module
#' @rdname formulier
#' @export
formulierUI <- function(id,
                        title = "Formulier",
                        icon = softui::bsicon("ui_checks"),
                        header_ui = NULL,
                        tag = "Opties",
                        new_existing_labels = c("Nieuwe registratie invoeren","Bestaande registratie bekijken")
                        ){
  
  ns <- NS(id)
  
  softui::fluid_page(
    softui::fluid_row(
      
       softui::box(title = title, icon = icon, tag = tag,
         
          header_ui,
          radioButtons(ns("rad_nieuw_wijzigen_registratie"), 
                       label = NULL, 
                       choices = setNames(c("nieuw", "bestaand"), new_existing_labels),
                       selected = "nieuw",
                       inline = FALSE)
       )
      
    ),
    softui::fluid_row(
      column(12,
             shinyjs::hidden(
               tags$div(id = ns("box_bestaande_registratie"),
                        softui::box(
                          width = 12,
                          title = "",
                          
                          tags$p("Bestaande registratie")
                        )
               )
             ),
             tags$div(id = ns("box_nieuwe_registratie"),
                      softui::box(
                        width = 12,
                        title = "",
                        tag = "Formulier",
                        
                        nieuweRegistratieUI(ns("nieuweRegistratie"))
                      )
             )
      )
    )
  )
  
  
}


#' @rdname formulier
#' @export
formulierModule <- function(input, output, session, .reg = NULL){
  
  ns <- session$ns
  
  observe({
    req(input$rad_nieuw_wijzigen_registratie)
    
    if(input$rad_nieuw_wijzigen_registratie == "bestaand"){
      
      shinyjs::show("box_bestaande_registratie")
      shinyjs::hide("box_nieuwe_registratie")
      
    } else {
      
      shinyjs::show("box_nieuwe_registratie")
      shinyjs::hide("box_bestaande_registratie")
      
    }
  })
  
  ping <- callModule(nieuweRegistratieModule, "nieuweRegistratie", .reg = .reg)
  
return(ping)
}





