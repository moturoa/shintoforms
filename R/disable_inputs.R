
#' Disable all inputs in a container
#' @export
disable_inputs <- function(id, session = shiny::getDefaultReactiveDomain()){
  session$sendCustomMessage("disableInputs", list(id = id))  
}

