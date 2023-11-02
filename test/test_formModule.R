



#---- Testing

test_formModule <- function(){
  library(withr)
  with_dir("test", source("global.R"))
  
  
  testmoduleui <- function(id){
    ns <- NS(id)
    
    tagList(
      radioButtons(ns("value1"), "Keuze 1", choices = LETTERS[1:4]),
      radioButtons(ns("value2"), "Keuze 2", choices = LETTERS[1:4])
    )
    
  }
  
  testmodule <- function(input,output,session,columns){
    reactive(setNames(list(input$value1,input$value2),columns))
  }
  
  
  library(shiny)
  library(shinyWidgets)
  
  ui <- softui::simple_page(
    shinyjs::useShinyjs(),
    softui::box(
      shinyWidgets::materialSwitch("toggle", "Aan/Uit", value = FALSE),  
      formUI("test")
    )
  )
  
  server <- function(input, output, session) {
    
    callModule(formModule, "test", .reg = .reg,
               current_user = "devuser",
               disabled = reactive(input$toggle))
  }
  
  shinyApp(ui, server)
  
  
  
}




