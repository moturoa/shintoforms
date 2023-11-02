
#' @importFrom htmltools htmlDependency
shintoforms_dependencies <- function() {
  
  list(
    htmltools::htmlDependency(name = "disable", version = "0.1",
                              package = "shintoforms",
                              src = "disable",
                              script = "disable_inputs.js"
    )
  )
}
