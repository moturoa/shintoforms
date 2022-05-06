
formulierUI <- function(id){
  
  ns <- NS(id)
  
  div(
    softui::fluid_row(
      column(12,
             softui::box(
               width = 12,
               title = "Formulier",
               
               softui::fluid_row(
                 column(12,
                        radioButtons(ns("rad_nieuw_wijzigen_registratie"), 
                                     label = "Wilt u een nieuwe registratie invoeren of een bestaande registratie bekijken/wijzigen?", 
                                     choices = c("Nieuwe registratie invoeren" = "nieuw", 
                                                 "Bestaande registratie bekijken" = "bestaand"), 
                                     selected = "nieuw",
                                     inline = TRUE),
                 )
               )
             )
      )
    ),
    softui::fluid_row(
      column(12,
             shinyjs::hidden(
               tags$div(id = ns("box_bestaande_registratie"),
                        softui::box(
                          width = 12,
                          title = "",
                          
                          softui::fluid_row(
                            column(12,
                                   tags$p("Bestaande registratie")
                            )
                          )
                        )
               )
             ),
             tags$div(id = ns("box_nieuwe_registratie"),
                      softui::box(
                        width = 12,
                        title = "",
                        
                        softui::fluid_row(
                          column(12,
                                 nieuweRegistratieUI(ns("nieuweRegistratie"))
                          )
                        )
                      )
             )
      )
    )
  )
  
  
}

formulierModule <- function(input, output, session){
  
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
  
  nieuw_registratie_ping <- callModule(nieuweRegistratieModule, "nieuweRegistratie")
  
  
}
