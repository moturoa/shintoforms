
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
