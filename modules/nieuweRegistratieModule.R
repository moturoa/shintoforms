
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
               tags$p("Links eigen") 
        ),
        column(6,
               tags$p("rechts eigen") 
        )
      )
    )
    
  )
  
}

nieuweRegistratieModule <- function(input, output, session){
  
  ns <- session$ns
  
  
}
