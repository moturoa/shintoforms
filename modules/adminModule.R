
adminUI <- function(id){
  
  ns <- NS(id)
  
  div(
    softui::fluid_row(
      column(12,
             softui::tab_box(
               
               softui::tab_panel(
                 title = "Formulieropstelling", icon = bsicon("pencil-square"),
                 
                 softui::fluid_row(
                   column(12,
                          softui::action_button(ns("btn_add_formfield"), "Toevoegen", 
                                                status = "secondary", icon = bsicon("plus")),
                          softui::action_button(ns("btn_edit_formfield"), "Wijzig invoerveld", 
                                                class = "btn-info",
                                                icon = bsicon("pencil-square"))
                   )
                 ),
                 softui::fluid_row(
                   column(12,
                          DT::dataTableOutput(ns("dt_form_invoervelden"))
                   )
                 )
                 
                 
                 
                 
                 
               ),
               softui::tab_panel(
                 title = "Verwijderde invoervelden", icon = bsicon("recycle"),
                 
                 
                 
               )
             )
      )
    )
  )
  
  
}

adminModule <- function(input, output, session){
  
  ns <- session$ns
  
  db_ping <- reactiveVal()
  
  form_invul_data <- reactive({
    db_ping()
    .reg$get_invulvelden(TRUE)
  })
  
  output$dt_form_invoervelden <- DT::renderDataTable({
    
    form_invul_data() %>%
      select("ID" = id_formulierveld, 
             "Kolomnaam" = kolomnaam_veld, 
             "Label" = label_veld, 
             "Type" = type_veld, 
             "Zijde op formulier" = formulier_kant,
             "Volgordenummer" = volgorde_veld,
             "Opties" = opties, 
             "Volgorde opties" = volgorde_opties, 
             "Kleuren" = kleuren, 
             "Kan door gebruiker worden verwijderd" = kan_worden_verwijderd) %>%
      softui::datatafel(selection = "single", dom = "t", pageLength = 30)
    
  })
  
  selected_row <- reactive({
    ii <- input$dt_form_invoervelden_rows_selected
    req(ii)
    form_invul_data() %>% slice(ii)
  })
  
  observeEvent(input$btn_add_formfield, {
    showModal(
      softui::modal(
        title = "Invoerveld toevoegen aan formulier",
        
        textInput(session$ns("txt_column_name"), "Database kolom naam"),
        radioButtons(session$ns("rad_type_formfield"), "Selecteer het soort invoerveld",
                     choices = c("Tekstinvoer" = "freetext",
                                 "Numerieke invoer" = "numeric",
                                 "Ja/Nee" = "boolean",
                                 "Keuzemenu (enkele optie)" = "singleselect",
                                 "Keuzemenu (meerdere opties)" = "multiselect")),
        radioButtons(session$ns("rad_side_formfield"), "Selecteer de zijde van het invoerveld op het formulier",
                     choices = c("Links" = "links",
                                 "Rechts" = "rechts")),
        
        
        id_confirm = "btn_confirm_add_formfield"
      )
    )
  })
  
  observeEvent(input$btn_confirm_add_formfield, {
    
    .reg$add_invoerveld_formulier(input$txt_column_name, input$rad_type_formfield, input$rad_side_formfield)
    
    db_ping(runif(1))
    removeModal()
    
  })
  
  observeEvent(input$btn_edit_formfield, {
    
    showModal(
      softui::modal(
        title = glue("Kolom label: {selected_row()$label_veld}"),
        
        textInput(session$ns("txt_edit_formfield_label"), "Label", 
                  value = selected_row()$label_veld),
        
        id_confirm = "btn_confirm_edit_label"
      )
    )
  })
  
  observeEvent(input$btn_confirm_edit_label, {
    
    #.reg$set_label(selected_id(), input$txt_column_label)
    #db_ping(runif(1))
    #removeModal()
    
  })
  
  
}
