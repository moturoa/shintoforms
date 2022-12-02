
#-- configure possible input field types
configured_field_types <- c("Tekstinvoer" = "freetext",
                            "Numerieke invoer" = "numeric",
                            "Ja/Nee" = "boolean",
                            "Keuzemenu (enkele optie)" = "singleselect",
                            "Keuzemenu (meerdere opties)" = "multiselect",
                            "Datum" = "date",
                            "Checkbox" = "singlecheck",
                            "Tekst met opmaak" = "html")

assert_input_field_type <- function(type){
  if(!type %in% configured_field_types){
    stop(paste("type_field must be one of:", paste(configured_field_types,collapse=",")))
  }  
}


#---- editFieldmodule: module voor een enkele input (bv numeric, text, oid)

# TODO type / subtype:
# select: single, multiple
# check: single, multiple
# boolean: radio, switch
# text: short, long, html
# numeric: single, range
# date: single, range


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
  
  # dont crash if type not in list below
  ui <- NULL
  
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
        outval <- tryCatch({ 
           options[[value]]
         
        }, error=function(cond) { 
           value 
        }) 
      ui <- selectizeInput(ns("value"), label, choices = c("", options), 
                     selected = outval, 
                     multiple = FALSE)
    
  } else if(type == "multiselect"){
    
      ui <- selectizeInput(ns("value"), label, choices = c("", options),
                     selected = value, 
                     multiple = TRUE, 
                     options = list(plugins = list("remove_button"))
      )
    
  } else if(type == "date"){
    
    if(length(value) == 0){
      value <- Sys.Date()
    }
    
    ui <- dateInput(ns("value"), label, language = "nl", 
                    weekstart = 1,  # start on monday; should be configurable at some point
                    value = value, format = "dd-mm-yyyy")
    
  } else if(type == "singlecheck"){
    if(length(value) == 0 || as.character(value) == ""){
      value <- FALSE
    }
    ui <- checkboxInput(ns("value"), label, value = value)
    
  } else if(type == "html"){

    ui <- shintocatman::htmlInput(ns("value"), label = label, value = value)

  }
  
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
  
  
  value
}



# editFieldModuleUI("one", "this_thing", data = NULL,
#                   type = "boolean", default = TRUE, options = NULL, label = "hello")


test_editFieldModule <- function(){
  
  devtools::load_all()
  
  ui <- softui::simple_page(
    softui::box(
      uiOutput("uiout")
    )
  )
  
  server <- function(input, output, session) {
    
    output$uiout <- renderUI({
      # editFieldModuleUI("one", "this_thing", data = NULL,
      #                   type = "html", default = TRUE, options = NULL, 
      #                   label = "hello")
      shintocatman::htmlInput("test", value = "some text")
    })
    
  }
  
  shinyApp(ui, server)
  
}





