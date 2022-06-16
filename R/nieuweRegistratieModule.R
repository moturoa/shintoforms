
#' Nieuwe registratie Shiny module
#' @rdname registratie
#' @export
nieuweRegistratieUI <- function(id){
  
  ns <- NS(id)
  
  softui::fluid_page(

    softui::fluid_row(
      column(6,
        uiOutput(ns("ui_input_left"))
      ),
      column(6,
        uiOutput(ns("ui_input_right"))
      )
    ),
    
    softui::fluid_row(class = "justify-content-end",
      
     
      column(4,
         softui::action_button(ns("btn_cancel"), 
                               "Annuleren", 
                               icon = bsicon("x-lg"), 
                               status = "danger"),      
         softui::action_button(ns("btn_register_new_signal"), 
                      "Opslaan", 
                      icon = bsicon("cloud-arrow-up"), 
                        status = "success")
      )
    )
  )
   
  
}

#' @rdname registratie
#' @export
nieuweRegistratieModule <- function(input, output, session, .reg = NULL, ping_update = reactive(NULL)){
  
  ns <- session$ns
  
  nieuwe_registratie_ping <- reactiveVal()
  
  # reactive maken zodat ie update als er iets wordt veranderd in admin, zie admin scherm hoe dat moet. 
  cfg_left <- reactive({
    ping_update()
    .reg$get_form_fields("links")
  })
  
  cfg_right <- reactive({
    ping_update()
    .reg$get_form_fields("rechts")
  })
  
  
  output$ui_input_left <- renderUI({
    
    formSectionModuleUI(session$ns("form_left"), cfg = cfg_left(), .reg = .reg)
    
  })
  
  output$ui_input_right <- renderUI({
    
    formSectionModuleUI(session$ns("form_right"), cfg = cfg_right(), .reg = .reg)
    
  })
  
  edit_left <- callModule(formSectionModule, "form_left", cfg = cfg_left, .reg = .reg)
  edit_right <- callModule(formSectionModule, "form_right", cfg = cfg_right, .reg = .reg)
  
  
  edits <- reactive({
    
    req(edit_left())
    
    c(lapply(edit_left(), function(x)x()),
      lapply(edit_right(), function(x)x()))
    
  })
  
  observeEvent(input$btn_register_new_signal, {
    
    # TODO
    # database methode om rij aan een tabel toe te voegen
    # dit hier laat in een modal zien wat de huidige edits zijn, alleen om te testen
    
    data <- edits()
    data[sapply(data,is.null)] <- NA
    data[sapply(data,length)==0] <- NA
    
    data <- lapply(data, function(x){
      if(length(x) > 1){
        x <- as.character(.reg$to_json(x))
      } 
    
      if(class(x) == "json"){
        x <- as.character(x)
      }
      x
    })
    
    showModal(
      softui::modal(
        title = "Debug",
        id_confirm = "btn_confirm_new_registration",
        
        HTML(kable(t(as.data.frame(data)), format = "html"))
      )
    )
    
  })
  
  out_ping <- reactiveVal()
  observeEvent(input$btn_cancel, out_ping(runif(1)))
  observeEvent(input$btn_confirm_new_registration, out_ping(runif(1)))
  
return(out_ping)
}





  
  
  
#---- editFieldmodule: module voor een enkele input (bv numeric, text, oid)
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

editFieldModuleUI <- function(id, column, data, 
                              type, 
                              default = NULL,
                              options = NULL,
                              label = NULL){
  
  ns <- NS(id)
  
  assert_input_field_type(type)
  
  value <- make_default_value(column, data, 
                              default = default,
                              array = type == "multiselect")
  
  # integer
  # text
  # boolean
  # select (gebaseerd op tabel met keuze opties)
  
  if(type == "numeric"){
    
    if(!isTruthy(value))value <- 0
    
    numericInput(ns("value"), label, 
                 value = value)
    
  } else if(type == "freetext"){
    
    textInput(ns("value"), label, value = value)
    
  } else if(type == "boolean"){
    
    radioButtons(ns("value"), label, inline = TRUE, 
                 choices = setNames(c(TRUE,FALSE),names(options)),
                 selected = as.character(value)
    )
    
  } else if(type == "singleselect"){
    
    selectizeInput(ns("value"), label, choices = options, 
                   selected = value, 
                   multiple = FALSE)
    
  } else if(type == "multiselect"){
    
    selectizeInput(ns("value"), label, choices = options, 
                   selected = value, 
                   multiple = TRUE, 
                   options = list(plugins = list("remove_button"))
                   )
    
  } else if(type == "date"){
    
    dateInput(ns("value"), label, language = "nl", format = "dd-mm-yyyy")
    
  }
  # else if(b$type == "html_editor"){
  #   
  #   freetextEditUI(ns("value"))
  #   
  # }
  

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
      
    }
    
    out
    
  })
  
  
  value
}



# editFieldModuleUI("one", "this_thing", data = NULL,
#                   type = "boolean", default = TRUE, options = NULL, label = "hello")



#--- formSection: serie input velden onder elkaar (bv. linker kolom van een form)

formSectionModuleUI <- function(id, cfg, data = NULL, .reg){
  
  ns <- NS(id)
  
  els <- split(cfg, 1:nrow(cfg))[cfg$order_field]
  
  lapply(els, function(el){
    editFieldModuleUI(ns(el$id_form), 
                      column = el$column_field, 
                      label = el$label_field,
                      options = .reg$choices_from_json(el$options),
                      type = el$type_field,
                      default = if(el$type_field == "boolean")TRUE else "", #el$default,
                      data = data)
  })  
  
  
  
}


formSectionModule <- function(input, output, session, cfg = reactive(NULL), .reg){
  
  values <- reactive({
    cfg <- cfg()
    
    lapply(split(cfg, 1:nrow(cfg)), function(el){
      col <- el$column_field
      id <- el$id_form
      callModule(editFieldModule, id, .reg = .reg, type = el$type_field)
    }) %>% setNames(cfg$column_field)
  })
  
  return(values)
  
}




#---- Testing

test_nieuweRegistratie <- function(){
  library(withr)
  with_dir("test", source("global.R"))
  
  
  
  library(shiny)
  
  ui <- softui::simple_page(
    
    softui::box(
      nieuweRegistratieUI("test")
    )
  )
  
  server <- function(input, output, session) {
    callModule(nieuweRegistratieModule, "test", .reg = .reg)
  }
  
  shinyApp(ui, server)
  
  
  
}


test_formSection <- function(){
  
  library(withr)
  with_dir("test", source("global.R"))
  data <- .reg$get_form_fields("links")
  
  ui <- softui::simple_page(
    
    softui::box(
      formSectionModuleUI("form", cfg = data, .reg = .reg)
    ),
    
    verbatimTextOutput("txt_out")
  )
  
  server <- function(input, output, session) {
    
    out <- callModule(formSectionModule, "form", cfg = data, .reg = .reg)
    
    output$txt_out <- renderPrint({
      lapply(out(), function(x)x())
    })
  }
  
  shinyApp(ui, server)
  

}












