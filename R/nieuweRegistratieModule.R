
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
    
    
    softui::fluid_row(
      verbatimTextOutput(ns("txt_out"))
    ),
    
    softui::fluid_row(class = "justify-content-end",
      
      column(2,
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
nieuweRegistratieModule <- function(input, output, session, .reg = NULL){
  
  ns <- session$ns
  
  nieuwe_registratie_ping <- reactiveVal()
  
  # reactive maken zodat ie update als er iets wordt veranderd in admin, zie admin scherm hoe dat moet. 
  cfg_left <- reactive({
    #session$userData$db_ping()
    .reg$get_velden_form("links")
  })
  
  cfg_right <- reactive({
    .reg$get_velden_form("rechts")
  })
  
  
  output$ui_input_left <- renderUI({
    
    formSectionModuleUI(session$ns("form_left"), cfg = cfg_left(), .reg = .reg)
    
  })
  
  output$ui_input_right <- renderUI({
    
    formSectionModuleUI(session$ns("form_right"), cfg = cfg_right(), .reg = .reg)
    
  })
  
  edit_left <- callModule(formSectionModule, "form_left", cfg = isolate(cfg_left()), .reg = .reg)
  edit_right <- callModule(formSectionModule, "form_right", cfg = isolate(cfg_right()), .reg = .reg)
  
  
  edits <- reactive({
    
    req(edit_left())
    
    c(lapply(edit_left(), function(x)x()),
      lapply(edit_right(), function(x)x()))
    
  })
  
  output$txt_out <- renderPrint({
    edits()
  })
  
  
  
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
                              type = c("freetext",
                                       "numeric",
                                       "boolean",
                                       "singleselect",
                                       "multiselect",
                                       "date"), 
                              default = NULL,
                              options = NULL,
                              label = NULL){
  
  ns <- NS(id)
  
  type <- match.arg(type)
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

editFieldModule <- function(input, output, session, .reg, 
                            type){                            #data = reactive(NULL)){
  
  
  # b <- uncolumn_edit_fields(.cc$get("beheer/edit_fields"))[[column]]
  # 
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

#' @param cfg One or more rows of the form definition
formSectionModuleUI <- function(id, cfg, data = NULL, .reg){
  
  ns <- NS(id)
  
  els <- split(cfg, 1:nrow(cfg))[cfg$volgorde_veld]
  
  lapply(els, function(el){
    editFieldModuleUI(ns(el$id_formulierveld), 
                      column = el$kolomnaam_veld, 
                      label = el$label_veld,
                      options = .reg$choices_from_json(el$opties),
                      type = el$type_veld,
                      default = if(el$type_veld == "boolean")TRUE else "", #el$default,
                      data = data)
  })  
  
  
  
}


formSectionModule <- function(input, output, session, cfg, .reg){
  
  values <- list()
  
  for(el in split(cfg, 1:nrow(cfg))){
    
    col <- el$kolomnaam_veld
    id <- el$id_formulierveld
    
    values[[col]] <- callModule(editFieldModule, id, .reg = .reg, type = el$type_veld)
  }
  
  
  return(reactive(values))
  
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
  data <- .reg$get_velden_form("links")
  
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












