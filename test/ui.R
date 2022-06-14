

sidebar <- softui::dashboard_sidebar(
  softui::sidebar_menu(
    softui::menu_item("Admin", tabName = "admin", icon = bsicon("gear-fill")),
    softui::menu_item("Formulier", tabName = "formulier", icon = bsicon("ui-checks"))
  )
)



body <- softui::dashboard_body(
  
  includeCSS("www/extra_css.css"),

  softui::tab_items(
    softui::tab_item("admin",
                     adminUI("admin")
    ),

    softui::tab_item("formulier",
                     formulierUI("formulier")
    )
    
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


