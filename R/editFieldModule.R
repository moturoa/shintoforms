
#-- configure possible input field types
configured_field_types <- c("Tekstinvoer" = "freetext",
                            "Numerieke invoer" = "numeric",
                            "Ja/Nee" = "boolean",
                            "Keuzemenu (enkele optie)" = "singleselect",
                            "Keuzemenu (meerdere opties)" = "multiselect",
                            "Keuzemenu (gekoppelde opties)" = "nestedselect",
                            "Datum" = "date",
                            "Checkbox" = "singlecheck",
                            "Tekst met opmaak" = "html",
                            "Gebruiker lijst (enkele optie)" = "single_user",
                            "Gebruiker lijst (meerdere opties)" = "multi_user"
                            )

assert_input_field_type <- function(type){
  if(!type %in% configured_field_types){
    stop(paste("type_field must be one of:", paste(configured_field_types,collapse=",")))
  }  
}


amend_type_inputfield <- function(type){
  if(type == "single_user")type <- "singleselect"
  if(type == "multi_user")type <- "multiselect"
  type
}



#---- editFieldmodule: module voor een enkele input (bv numeric, text, oid)



editFieldModuleUI <- function(id, column, data, 
                              type, 
                              #type_options = list(),
                              default = NULL,
                              options = NULL,
                              label = NULL,
                              disabled = FALSE,
                              input_width = getOption("shintoforms_input_width_percent", "80%"),
                              input_padding = getOption("shintoforms_input_padding_px", "30px"),
                              shintousers_object = NULL
                              ){
  
  ns <- shiny::NS(id)
  
  # doen we niet omdat dan bij een onbekende setting de boel crasht;
  # beter om gewoon door te gaan en NULL in te vullen
  #assert_input_field_type(type)

  # If a shintouser list, make it here
  if(type %in% c("single_user","multi_user")){
    if(is.null(shintousers_object) || !isTRUE(shintousers_object$has_connection())){
      options <- "Gebruiker lijst sync fout"
    } else {
      # hier kun je ook als nog op group filteren
      user_table <- shintousers_object$list_application_users(active_only = TRUE)  
      options <- sort(user_table[["username"]])
    }
    
    # multiselect verwacht een array met names (voor singleselect either way)
    names(options) <- options
  }
  
  
  # shintouser <--> shintoforms
  # intern is dit gewoon een singleselect/multiselect,
  # enige verschil is dat deze velden bij init gesyncd worden met shintousers
  # single_user --> singleselect
  # multi_user --> multiselect
  type <- amend_type_inputfield(type)
  

  # Make selected value based on default, data, settings
  value <- make_default_value(column, data, 
                              default = default,
                              array = type == "multiselect")
 
  has_option <- function(what){
    what %in% names(type_options)
  }
  
  read_option <- function(what){
    type_options[[what]]
  }
  
   
  # integer
  # text
  # boolean
  # select (gebaseerd op tabel met keuze opties)
  
  # dont crash if type not in list below
  ui <- NULL
  
  if(type == "numeric"){
    
    if(!shiny::isTruthy(value))value <- 0
    
    ui <- shiny::numericInput(ns("value"), label, value = value, width = input_width)
    
    
  } else if(type == "freetext"){
    
    ui <- shiny::textInput(ns("value"), label, value = value, width = input_width)
    
  } else if(type == "boolean"){
    
    ui <- shiny::radioButtons(ns("value"), label, inline = TRUE, 
                 choices = setNames(c(TRUE,FALSE),names(options)),
                 selected = as.character(value))
    
  } else if(type == "singleselect"){
        
    outval <- tryCatch(options[[value]], 
      error = function(cond) { 
        value 
      })
        
    ui <- shiny::selectizeInput(ns("value"), label, choices = c("", options), 
                     selected = outval, 
                     multiple = FALSE,
                     width = input_width)
    
  } else if(type == "multiselect"){
    
      sels <- options[match(value,names(options))]
    
      ui <- shiny::selectizeInput(ns("value"), label, choices = c("", options),
                     selected = sels, 
                     multiple = TRUE, 
                     width = input_width,
                     options = list(plugins = list("remove_button"))
      )
    
  } else if(type == "nestedselect"){
    
    ui <- nestedSelectModuleUI(ns("value"), label, data, column, value, options,
                               width = input_width)
    
  } else if(type == "date"){
    
    if(length(value) == 0){
      value <- Sys.Date()
    }
    
    ui <- shiny::dateInput(ns("value"), label, language = "nl", 
                    weekstart = 1,  # start on monday; should be configurable at some point
                    width = input_width,
                    value = value, format = "dd-mm-yyyy")
    
  } else if(type == "singlecheck"){
    if(length(value) == 0 || as.character(value) == ""){
      value <- FALSE
    }
    ui <- shiny::checkboxInput(ns("value"), label, value = value)
    
  } else if(type == "html"){

    # container <- if(is.null(read_option("container"))){
    #   tags$div
    # } else {
    #   
      container <- function(ui){
        tags$div(
          style = "padding-bottom: 24px;",
          softui::sub_box(ui, title = label, 
                          icon = bsicon("pencil-square"),
                          collapsible = TRUE, grey_level = 0.2)  
        )
        
      }
      
    #}
    
    ui <- shintocatman::htmlInput(ns("value"), label = NULL, value = value)
    ui <- container(ui)
  }
  
  if(isTRUE(disabled)){
    ui <- shinyjs::disabled(ui)
  }
  
tags$div(
  style = glue::glue("padding-left: {input_padding}; padding-right: {input_padding};"),
  ui
)
  
}


editFieldModule <- function(input, output, session, .reg, type, cfg = NULL, 
                            data = shiny::reactive(NULL), 
                            trigger = shiny::reactive(NULL)){
  
  # #---- Input validator
  # val_i <- shinyvalidate::InputValidator$new()
  # 
  # if(isTRUE(b$max == 0)){
  #   val_i$add_rule("value", sv_lte(0, message = "Voer een negatief getal in!"))
  # }
  # 
  # val_i$enable()
  
  # shintouser <--> shintoforms
  # intern is dit gewoon een singleselect/multiselect,
  # enige verschil is dat deze velden bij init gesyncd worden met shintousers
  type <- amend_type_inputfield(type)
  
  
  #--- Output reactive
  value <- shiny::reactive({
    
    if(type == "nestedselect"){
      out <- shiny::callModule(nestedSelectModule, "value", cfg = cfg, data = data, trigger = trigger)
    } else {
      out <- input$value  
    }
    
    # Extra processing of output
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
  
  shiny::observeEvent(value(), {

    invisible()
  })
  
  
  value
}







