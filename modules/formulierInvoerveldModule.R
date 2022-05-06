
formulierInvoerveldUI <- function(id){
  
  ns <- NS(id)
  
  div(
    softui::fluid_page(
      softui::fluid_row(
        column(12,
               tags$p("Invoerveld") 
        )
      )
    )
    
  )
  
}

formulierInvoerveldModule <- function(input, output, session){
  
  ns <- session$ns
  
  
}
