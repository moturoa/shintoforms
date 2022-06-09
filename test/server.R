
function(input, output, session){
  #softui::populate_header(username = shintoshiny::get_user(default = "devuser"))
  
  session$userData$db_ping <- reactiveVal()
  
  callModule(adminModule, "admin", .reg = .reg)
  callModule(formulierModule, "formulier",  .reg = .reg)

}





