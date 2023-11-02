
#' Disable all inputs in a container. Not used anymore (?)
#' @param id Id of the HTML container 
#' @param session Shiny session object, leave alone
#' @export
disable_inputs <- function(id, session = shiny::getDefaultReactiveDomain()){
  session$sendCustomMessage("disableInputs", list(id = id))  
}

