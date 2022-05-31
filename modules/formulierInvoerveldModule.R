
formulierInvoerveldUI <- function(id){
  
  ns <- NS(id)
  
  div(
    softui::fluid_page(
      softui::fluid_row(
        column(12,
               uiOutput(ns("invoer_ui"))
        )
      )
    )
    
  )
  
}

formulierInvoerveldModule <- function(input, output, session, label, nieuwe_registratie_ping = reactive(NULL)){
  
  ns <- session$ns
  
  output$invoer_ui <- renderUI({
    
    softui::fluid_row(
      column(12,
             selectInput(ns("invoer_input"), label = label, choices = c(letters))
      )
    )
    
  })
  
  observeEvent(nieuwe_registratie_ping(), {
    print(label)
    print(input$invoer_input)
    
    return(reactive({paste(label, input$invoer_input)}))
  })
  
  
}
