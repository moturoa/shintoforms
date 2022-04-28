
dashboardUI <- function(id){
  
  ns <- NS(id)
  
  div(
    softui::fluid_row(
      column(12,
             softui::box(
               width = 12,
               title = "Dashboard",
               
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

dashboardModule <- function(input, output, session){
  
  ns <- session$ns
  
  
}
