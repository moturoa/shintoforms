

sidebar <- softui::dashboard_sidebar(
  softui::sidebar_menu(
    softui::menu_item("Admin", tabName = "admin", icon = bsicon("gear-fill")),
    softui::menu_item("Formulier", tabName = "formulier", icon = bsicon("ui-checks")),
    softui::menu_item("Overzicht", tabName = "overzicht", icon = bsicon("list")),
    if(GLOBAL_AUDIT){
      softui::menu_item("Audit", tabName = "audit", icon = bsicon("list-nested")) 
    }
  )
)



body <- softui::dashboard_body(
  
  includeCSS("www/extra_css.css"),

  softui::tab_items(
    softui::tab_item("admin",
                     formAdminUI("admin")
    ),

    softui::tab_item("formulier",
                     formPageUI("formulier")
    ),
    softui::tab_item("overzicht", 
                     
                     reactable::reactableOutput( "dt_signalen_overzicht") 
    ),
    if(GLOBAL_AUDIT){
      softui::tab_item("audit",
                       
                     tags$h1("Event log"),  
                     reactable::reactableOutput( "dt_events"), 
                     tags$br(),  
                     tags$h1("Ruwe audit"),  
                     reactable::reactableOutput( "dt_audit")
      ) 
    }
  )
)


header <- softui::dashboard_header(
  tag_line = "Registratie formulieren opstellen, invullen en visualiseren"
)


softui::dashboard_page(title = "Registraties",
                       icon = bsicon("pencil-square"),
                       loadingscreen_time = 2,
                       header = header,
                       sidebar = sidebar,
                       body = body)


