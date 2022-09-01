#' Shiny module for process mining visualization and analysis of status development of registration
#' @param id Shiny input id
#' @export
#' @rdname registrationProcessMiningModule

registrationProcessMiningUI <- function(id){
  
  ns <- NS(id)
  
  softui::fluid_row(
    column(12,
           tags$p("Process Mining Registration")
    )
  )
  
  
}

#' @export
#' @param .reg formClass for which the process mining is being done (call to formClass does not have event_data set as NULL)
#' @param registration_id registration about we want to see event traces and process mining analyses
#' @rdname registrationProcessMiningModule
registrationProcessMiningModule <- function(input, output, session, .reg, registration_id = reactive(NULL)){
  
  
}