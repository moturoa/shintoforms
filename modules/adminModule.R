
adminUI <- function(id){
  
  ns <- NS(id)
  
  div(
    softui::fluid_row(
      column(12,
             softui::box(
               width = 12,
               title = "Admin",
               
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

adminModule <- function(input, output, session){
  
  ns <- session$ns
  
  
}
