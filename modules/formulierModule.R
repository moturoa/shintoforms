
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
                        tags$p("Test") 
                 )
               )
             )
      )
    )
  )
  
  
}

formulierModule <- function(input, output, session){
  
  ns <- session$ns
  
  
}
