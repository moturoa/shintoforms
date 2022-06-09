
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
               tags$div(id = ns("linkerkant_formulier"),
                        tags$div(id = ns("fomuliervelden_links"))
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

nieuweRegistratieModule <- function(input, output, session, .reg = NULL){
  
  ns <- session$ns
  
  nieuwe_registratie_ping <- reactiveVal()
  
  # reactive maken zodat ie update als er iets wordt veranderd in admin, zie admin scherm hoe dat moet. 
  kolomnamen_links <- reactive({
    session$userData$db_ping()
    .reg$get_velden_form("links")
  })
  
  observeEvent(kolomnamen_links(), {
    
    removeUI(selector = glue("#{ns('fomuliervelden_links')}"))
    
    new_ui_elements <- div(fluidRow(
      column(12,
             tags$div(id = ns("fomuliervelden_links"))
      ))
    )
    
    insertUI(glue("#{ns('linkerkant_formulier')}"), where = "beforeEnd", ui = new_ui_elements)
    
    kolomnamen_links <- kolomnamen_links()
    
    lapply(1:nrow(kolomnamen_links), function(x){
      
      form_var <- kolomnamen_links$kolomnaam_veld[x]
      form_label <- kolomnamen_links$label_veld[x]
      new_id <- paste0("invoer_", uuid::UUIDgenerate())
      callModule(formulierInvoerveldModule, new_id, label = form_label, nieuwe_registratie_ping = nieuwe_registratie_ping)
      #assign(form_var, callModule(formulierInvoerveldModule, new_id, label = form_label, nieuwe_registratie_ping = nieuwe_registratie_ping))
      insertUI(glue("#{ns('fomuliervelden_links')}"), where = "beforeEnd", ui = formulierInvoerveldUI(ns(new_id)))
    })
    
  })
  
  
  
  observeEvent(input$btn_register_new_signal, {
    nieuwe_registratie_ping(runif(1))
    #browser()
    # ophalen info als data frame, query opstellen op een slimme manier zie documentatie
  })
  
}
