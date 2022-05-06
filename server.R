
function(input, output, session){
  #softui::populate_header(username = shintoshiny::get_user(default = "devuser"))
  
  callModule(adminModule, "admin")
  callModule(formulierModule, "formulier")
  callModule(dashboardModule, "dashboard")
  
}





