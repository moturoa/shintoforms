


#---- editFieldmodule: module voor een enkele input (bv numeric, text, oid)

editFieldModuleUI <- function(id, column, data, 
                              type, 
                              default = NULL,
                              options = NULL,
                              label = NULL,
                              disabled = FALSE){
  
  ns <- NS(id)
  
  #assert_input_field_type(type)
  
  value <- make_default_value(column, data, 
                              default = default,
                              array = type == "multiselect")
  
  # integer
  # text
  # boolean
  # select (gebaseerd op tabel met keuze opties)
  
  if(type == "numeric"){
    
    if(!isTruthy(value))value <- 0
    
    ui <- numericInput(ns("value"), label, 
                 value = value)
    
    
  } else if(type == "freetext"){
    
    ui <- textInput(ns("value"), label, value = value)
    
  } else if(type == "boolean"){
    
      ui <- radioButtons(ns("value"), label, inline = TRUE, 
                   choices = setNames(c(TRUE,FALSE),names(options)),
                   selected = as.character(value)
      )
    
  } else if(type == "singleselect"){
    
      ui <- selectizeInput(ns("value"), label, choices = c("", options), 
                     selected = value, 
                     multiple = FALSE)
    
  } else if(type == "multiselect"){
    
      ui <- selectizeInput(ns("value"), label, choices = c("", options),
                     selected = value, 
                     multiple = TRUE, 
                     options = list(plugins = list("remove_button"))
      )
    
  } else if(type == "date"){
    
    if(length(value) == 0 || as.character(value) == ""){
      value <- Sys.Date()
    }
    
    ui <- dateInput(ns("value"), label, language = "nl", value = value, format = "dd-mm-yyyy")
    
  } else if(type == "singlecheck"){
    if(length(value) == 0 || as.character(value) == ""){
      value <- FALSE
    }
    ui <- checkboxInput(ns("value"), label, value = value)
    
  }
  
  # } else if(type == "html"){
  # 
  #   shintocatman::htmlInput(ns("value"), value = value)
  # 
  # }
  
  if(isTRUE(disabled)){
    ui <- shinyjs::disabled(ui)
  }
  
ui
}

editFieldModule <- function(input, output, session, .reg, type){
  
  # #---- Input validator
  # val_i <- shinyvalidate::InputValidator$new()
  # 
  # if(isTRUE(b$max == 0)){
  #   val_i$add_rule("value", sv_lte(0, message = "Voer een negatief getal in!"))
  # }
  # 
  # val_i$enable()
  
  # #---- HTML edit field
  # if(b$type == "html_editor"){ 
  #   html_edit <- callModule(freetextEditModule, "value", 
  #                           data = data,
  #                           kolom = column,
  #                           label = b$label)
  # }
  
  #--- Output reactive
  value <- reactive({
    
    # if(b$type == "html_editor"){
    #   return(html_edit())
    # }
    
    out <- input$value
    
    if(type == "boolean"){
      out <- as.logical(out)
      
      # array optie.
    } else if(type == "multiselect"){
      
      out <- .reg$to_json(out)
      
    } else if(type == "date"){
      out <- as.character(out)
    }
    
    out
    
  })
  
  
  value
}



# editFieldModuleUI("one", "this_thing", data = NULL,
#                   type = "boolean", default = TRUE, options = NULL, label = "hello")



