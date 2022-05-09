
nieuweRegistratieUI <- function(id){
  
  ns <- NS(id)
  
  div(
    softui::fluid_page(
      softui::fluid_row(
        column(6,
               tags$p("Links altijd") 
        ),
        column(6,
               tags$p("rechts altijd") 
        )
      ),
      softui::fluid_row(
        column(12,
               tags$hr()
        )
      ),
      softui::fluid_row(
        column(6,
               tags$div(id = ns("eigen_invoer_links"),
                        tags$p("Links eigen"),
                        uiOutput(ns("invoervelden_links"))
               )
               
        ),
        column(6,
               tags$p("rechts eigen") 
        )
      ),
      softui::fluid_row(
        column(12,
               div(style = "display:inline-block; float: right",
                   actionButton(ns("btn_register_new_signal"), 
                                "Opslaan", 
                                icon = icon("cloud-upload"), 
                                class = "btn-success")
               )
               
        )
        
      )
    )
    
  )
  
}

nieuweRegistratieModule <- function(input, output, session){
  
  ns <- session$ns
  
  nieuwe_registratie_ping <- reactiveVal()
  
  # reactive maken zodat ie update als er iets wordt veranderd in admin, zie admin scherm hoe dat moet. 
  kolomnamen_links <- c("test_1", "test_2", "test_3")
  
  output$invoervelden_links <- renderUI({
    lapply(kolomnamen_links, function(x){
      new_id <- paste0("invoer_", uuid::UUIDgenerate())
      assign(x, callModule(formulierInvoerveldModule, new_id, label = x, nieuwe_registratie_ping = nieuwe_registratie_ping))
      insertUI(glue("#{ns('eigen_invoer_links')}"), where = "beforeEnd", ui = formulierInvoerveldUI(ns(new_id)))
    })
    
    softui::fluid_row(
      column(12,
      )
    )
  })
  
  observeEvent(input$btn_register_new_signal, {
    nieuwe_registratie_ping(runif(1))
    
  })
  
}
