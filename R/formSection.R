


#--- formSection: 
# a vertical column with input fields (and nothing else)
# typically right or left side of a formUI

formSectionModuleUI <- function(id, cfg, data = NULL, .reg, 
                                inject = list(),
                                disabled = FALSE,
                                shintousers_object = NULL,
                                shintousers_groups = NULL,
                                shintousers_ignore_groups = NULL){
  
  ns <- NS(id)
  
  # Configured UI from definition table
  els <- split(cfg, 1:nrow(cfg))
  
  # reorder elements based on form order
  els <- els[order(cfg$order_field)]
  
  ui <- lapply(els, function(el){
    if(el$type_field == "nestedselect"){
      chc <- jsonlite::fromJSON(el$options)
    } else {
      chc <- try(.reg$choices_from_json(el$options),silent=TRUE)
      if(inherits(chc, "try-error")){
        chc <- NA
      }
    }
    
    # reorder the choices based on the 'order' field
    c_ord <- .reg$from_json(el[["order_options"]])
    
    if(length(chc) > 1){
      len_check <- length(c_ord) == length(chc)
      if(!len_check){
        cli::cli_alert_danger("Wrong order variable for column {el$column_field}, not using ordering of input field")
      } else {
        chc <- chc[c_ord]  
      }
      
    }
    
    # remove the inactive options
    if(is.null(el$option_active)){
      option_statuses <- jsonlite::fromJSON('[]')
    } else {
      option_statuses <- jsonlite::fromJSON(el$option_active)
    }
    
    if(length(chc) > 1){
      len_check <- length(option_statuses) == length(chc)
      if(!len_check){
        cli::cli_alert_info("Wrong status variable for column {el$column_field}, not using status activation of input field")
      } else {
        active_options <- names(option_statuses[option_statuses == TRUE])
        chc <- chc[chc %in% active_options] 
      }
      
    }
    
    editFieldModuleUI(ns(el$id_form), 
                      column = el$column_field, 
                      label = el$label_field,
                      options = chc,
                      type = el$type_field,
                      #type_options = el$type_options,  # --> moet ook naar db
                      default = if(el$type_field == "boolean")TRUE else "", 
                      data = data, 
                      disabled = disabled,
                      shintousers_object = shintousers_object,
                      shintousers_groups = shintousers_groups,
                      shintousers_ignore_groups = shintousers_ignore_groups)
  })  
  
  
  
  if(length(inject)){
    last_pos <- 0
    
    input_padding <- getOption("shintoforms_input_padding_px", "30px")
    
    ui_fun <- function(ui){
      tags$div(
        style = glue::glue("padding-left: {input_padding}; padding-right: {input_padding};"),
        ui
      )
    }
    
    for(i in seq_along(inject)){
      
      obj <- inject[[i]]
      html_tag <- ui_fun(obj$html)
      position_tag <- obj$position
      
      if(position_tag < last_pos){
        stop("inject argument in formSectionModuleUI has to be in order of 'position'")
      }
      
      last_pos <- as.integer(position_tag)
      
      # insert the  ui.
      # +(i-1) because after first insert, increment the position
      ui <- append(ui, values = list(html_tag), after = as.integer(position_tag) + (i-1))
    }
    
  }
  
  ui
  
}


formSectionModule <- function(input, output, session, cfg = reactive(NULL), 
                              .reg, data = reactive(NULL),
                              trigger = reactive(NULL)){
  

  values <- reactive({
    
    cfg <- cfg()
    
    if(nrow(cfg) == 0){
      return(NULL)
    }
    
    lapply(split(cfg, 1:nrow(cfg)), function(el){
      col <- el$column_field
      id <- el$id_form
      callModule(editFieldModule, id, .reg = .reg, type = el$type_field, cfg = el, 
                 data = data, trigger = trigger)
    }) %>% setNames(cfg$column_field)
  })
  
  observeEvent(values(), {
    
    invisible()
  })
  
  
  return(values)
  
}


