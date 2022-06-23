#' Make a default value for an input, based on data and column name
#' @export
make_default_value <- function(x, data, default = character(0), array = FALSE){
  
  val <- data[[x]]
  
  if(is.null(val) || length(val) == 0 || is.na(val)){
    default
  } else {
    
    if(isTRUE(array)){
      
      val <- jsonlite::fromJSON(val)
    }
    
    val
  }
  
}
