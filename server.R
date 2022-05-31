
function(input, output, session){
  #softui::populate_header(username = shintoshiny::get_user(default = "devuser"))
  
  session$userData$db_ping <- reactiveVal()
  
  callModule(adminModule, "admin")
  callModule(formulierModule, "formulier")
  callModule(dashboardModule, "dashboard")
  
}





