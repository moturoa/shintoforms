



#--- formSection: 
# a vertical column with input fields (and nothing else)
# typically right or left side of a formUI

formSectionModuleUI <- function(id, cfg, data = NULL, .reg, inject = list()){
  
  ns <- NS(id)
  
  # Configured UI from definition table
  els <- split(cfg, 1:nrow(cfg))[cfg$order_field]
  
  ui <- lapply(els, function(el){
    editFieldModuleUI(ns(el$id_form), 
                      column = el$column_field, 
                      label = el$label_field,
                      options = .reg$choices_from_json(el$options),
                      type = el$type_field,
                      default = if(el$type_field == "boolean")TRUE else "", #el$default,
                      data = data)
  })  
  
  
  
  if(length(inject)){
    last_pos <- 0
    for(i in seq_along(inject)){
      
      obj <- inject[[i]]
      html_tag <- obj$html
      position_tag <- obj$position
      
      if(position_tag < last_pos){
        stop("inject argument in formSectionModuleUI has to be in order of 'position'")
      }
      
      # insert the  ui.
      # +(i-1) because after first insert, to increment the position
      ui <- append(ui, values = list(html_tag), after = as.integer(position_tag) + (i-1))
    }
    
  }
  
  
  
  ui
  
}


formSectionModule <- function(input, output, session, cfg = reactive(NULL), 
                              .reg){
  

  values <- reactive({
    cfg <- cfg()
    
    if(nrow(cfg) == 0){
      return(NULL)
    }
    
    lapply(split(cfg, 1:nrow(cfg)), function(el){
      col <- el$column_field
      id <- el$id_form
      callModule(editFieldModule, id, .reg = .reg, type = el$type_field)
    }) %>% setNames(cfg$column_field)
  })
  
  return(values)
  
}



test_formSection <- function(){
  
  library(withr)
  with_dir("test", source("global.R"))
  data <- .reg$get_form_fields(1)
  
  
  testmoduleui <- function(id){
    ns <- NS(id)
    
    radioButtons(ns("value"), "Keuze", choices = LETTERS[1:4])
  }
  testmodule <- function(input,output,session){
    reactive(input$value)
  }
  
  ui <- softui::simple_page(
    
    softui::box(
      formSectionModuleUI("form", cfg = data, .reg = .reg,
                          inject = list(
                            list(position = 2, html = tags$h2("Dit is een tekst")),
                            list(position = 5, html = testmoduleui("testmod"))
                          ))
    ),
    
    verbatimTextOutput("txt_out")
  )
  
  server <- function(input, output, session) {
    
    basic <- callModule(formSectionModule, "form", 
                        cfg = reactive(data), .reg = .reg)
    
    extra <- callModule(testmodule, "testmod")
    
    out <- reactiveVal()

    
    output$txt_out <- renderPrint({
      lapply(out(), function(x)x())
    })
  }
  
  shinyApp(ui, server)
  
  
}
