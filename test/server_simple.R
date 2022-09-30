
function(input, output, session){
  #softui::populate_header(username = shintoshiny::get_user(default = "devuser"))
  
  db_ping <- callModule(formAdminModule, "admin", .reg = .reg)
  
  callModule(formPageModule, "formulier",  .reg = .reg,
             ping_update = db_ping,
             current_user = "devuser")

}





