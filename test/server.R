
function(input, output, session){
  #softui::populate_header(username = shintoshiny::get_user(default = "devuser"))
  
  db_ping <- callModule(adminModule, "admin", .reg = .reg)
  
  callModule(formulierModule, "formulier",  .reg = .reg,
             ping_update = db_ping)

}





