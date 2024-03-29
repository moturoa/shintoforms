
function(input, output, session){
 
  #--- Global session pings
  gargoyle::init(
    "db_signal"  
  )
  
  
  # -------- Administratie
  db_ping <- callModule(formAdminModule, "admin", .reg = .reg)
  
  observeEvent(db_ping(), {
    gargoyle::trigger("db_signal")
  })
  
  
  # -------- Nieuwe invoer
  nieuw_ping <- callModule(formPageModule, "formulier",  .reg = .reg,
             ping_update = db_ping,
             current_user = "devuser")
  
  observeEvent(nieuw_ping(), {
    gargoyle::trigger("db_signal")
  })
  
  
  # -------- Overzicht  
  

  signal_data <- reactive({ 
    edit_ping()
    .reg$read_registrations()
  })
  
  
  output$dt_signalen_overzicht <- renderReactable({ 
    
    signal_data() %>% 
      mutate(registration_id=id_registratie) %>% 
      filter(deleted == 0) %>%
      reactable(
        highlight = TRUE, defaultPageSize = 5,
        onClick = reactable_click_handler("row_signal_select") ) 
  })  
  
  
  observeEvent(input$row_signal_select, { 
    showModal(
      softui::modal(
        title = "Signaal bewerken", size = "fullscreen",
        id_confirm = "btn_confirm_registration_edit",
        confirm_button = FALSE, close_button = FALSE,
        
        
        softui::fluid_row(class = "justify-content-center",
                          
                          column(7,
                                 
                                 tags$h5("Signaal"),
                                 tags$div(id = "form_view_container",
                                          shintoforms::formUI(session$ns("form_selection"), 
                                                              buttons = TRUE, 
                                                              deletable = TRUE)                 
                                 )
                                 
                          )
        )
      )
    )
    
  })
  
  selected_signal <- reactive({
    req(input$row_signal_select) 
    .reg$get_registration_by_id(input$row_signal_select$registration_id)
  })
  
  
  
  observeEvent(edit_ping(), {
    gargoyle::trigger("db_signal")
  }) 
  edit_ping <- callModule(shintoforms::formModule, "form_selection", 
                          .reg = .reg,
                          data = selected_signal,
                          disabled = reactive(FALSE),  # werkt nog niet
                          write_method = reactive("edit"),
                          confirm = reactive(input$btn_confirm_registration_edit),
                          current_user = "devuser")
   
  # Reactable heeft geen ingebouwde click observer.
  # Deze handler (zoals https://glin.github.io/reactable/articles/examples.html#custom-action)
  # geeft de hele rij data terug in een id naar keuze.
  # Letten op issue: 
  reactable_click_handler <- function(id = NULL, what = "data", session = shiny::getDefaultReactiveDomain()){
    
    what <- match.arg(what)
    
    id <- session$ns(id)
    fun_ <- glue::glue("function(rowInfo, column) {
              let values = rowInfo.values;
              let ranval = Math.random();
              let vallis = Object.assign(values,ranval);
              Shiny.setInputValue('<<<id>>>', vallis, { priority: 'event' })
              }", .open = "<<<", .close = ">>>")
    htmlwidgets::JS(fun_)
  }
   
  # -------- Audit
  if(GLOBAL_AUDIT){
    timeseries <- .reg$create_timeseries(columns=NULL,table=NULL) 
    
    # VOORBEELD 1: alleen events voor de kolom aantal_brommers (+ creaties)
    #timeseries <- .reg$create_timeseries(columns=c("aantal_brommers"),table=NULL) 
    # VOORBEELD 2: hergebruik van reeds ingeladen data object 
    #timeseries <- .reg$create_timeseries(columns=NULL,table=signal_data()) 
    
    output$dt_audit <- renderReactable({  
      head(timeseries) %>%  
        reactable()
    })
    
    output$dt_events <- renderReactable({  
      head(.reg$create_events(timeseries)) %>%  
        dplyr::mutate(action = case_when(type == 'U' ~ glue('kolom {variable} van {ifelse(is.na(old_val) | old_val == "", "leeg", old_val)} naar {new_val}'),
                                  type == 'C' ~ glue('Aanmaak registratie'))) %>%
        reactable::reactable()
    })
    
  }
  
}


 

