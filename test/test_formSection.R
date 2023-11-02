

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

