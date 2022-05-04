
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
                          
                          tags$span(id = ns("span_edit_formorder"),
                                    jsonFormSetupUI(ns("edit_formorder"), 
                                                    icon = bsicon("pencil-square"),
                                                    label = "Wijzig formulier",
                                                    class = "bg-gradient-primary")
                          ),
                          
                          shinyjs::hidden(
                            tags$span(id = ns("span_edit_formfield"),
                                      softui::action_button(ns("btn_edit_formfield"), "Wijzig invoerveld", 
                                                            class = "btn-info",
                                                            icon = bsicon("pencil-square"))
                            )
                          ),
                          
                          
                          shinyjs::hidden(
                            tags$span(id = ns("span_edit_options"),
                                      jsonEditModuleUI(ns("edit_options"), 
                                                       icon = bsicon("pencil-square"),
                                                       label = "Opties",
                                                       class = "bg-gradient-primary")  
                            )
                          ),
                          shinyjs::hidden(
                            tags$span(id = ns("span_edit_colors"),
                                      colorVectorPickModuleUI(ns("edit_colors"))  
                            )
                          ),
                          shinyjs::hidden(
                            tags$span(id = ns("span_edit_order_options"),
                                      jsonOrderModuleUI(ns("edit_order_options"), class = "warning")
                            )),
                          
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
  
  selected_id <- reactive({
    req(selected_row())
    selected_row()$id_formulierveld
  })
  
  selected_type <- reactive({
    req(selected_row())
    selected_row()$type_veld
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
  
  callModule(jsonFormSetupModule, "edit_formorder", 
             data = form_invul_data,
             side_column = reactive("formulier_kant"),
             order_column = reactive("volgorde_veld"),
             id_column = reactive("id_formulierveld"),
             label_column = reactive("label_veld")
  )
  
  
  
  observe({
    
    sel <- selected_row()
    type <- selected_type()
    if(type == "freetext" || type == "numeric" || type == "boolean"){
      show_edit_options <- FALSE
    } else {
      show_edit_options <- TRUE
    }
    shinyjs::toggleElement("span_edit_formfield", condition = !is.null(sel))
    shinyjs::toggleElement("span_edit_options", condition = (!is.null(sel) && show_edit_options))
    shinyjs::toggleElement("span_edit_colors", condition = (!is.null(sel) && show_edit_options))
    shinyjs::toggleElement("span_edit_order_options", condition = (!is.null(sel) && show_edit_options))
  })
  
  
  
  observeEvent(input$btn_edit_formfield, {
    
    showModal(
      softui::modal(
        title = glue("Kolom label: {selected_row()$kolomnaam_veld}"),
        
        textInput(session$ns("txt_edit_formfield_label"), "Label", 
                  value = selected_row()$label_veld),
        
        id_confirm = "btn_confirm_edit_label"
      )
    )
  })
  
  observeEvent(input$btn_confirm_edit_label, {
    
    .reg$edit_label_invulveld(selected_id(), input$txt_edit_formfield_label)
    db_ping(runif(1))
    removeModal()
    
  })
  
  opties <- callModule(jsonEditModule, "edit_options", 
                       options = reactive("add"),   # nooit categorieen verwijderen, anders DB problemen!
                       edit = reactive("value"),
                       widths = c(2,10),
                       value = reactive(selected_row()$opties))
  
  observeEvent(opties(), {
    
    .reg$edit_opties_invulveld(selected_id(), opties())
    .reg$amend_optie_order(selected_id(), opties())
    .reg$amend_optie_colors(selected_id(), opties())
    db_ping(runif(1))
    
  })
  
  current_colors <- reactive({
    from_json(selected_row()$kleuren)
  })
  
  colors <- callModule(colorVectorPickModule, "edit_colors",
                       n_colors = reactive(length(current_colors())), 
                       current_colors = current_colors,
                       labels = reactive(from_json(selected_row()$opties)),
                       show_order = reactive(from_json(selected_row()$volgorde_opties)))
  
  observeEvent(colors(), {
    
    .reg$set_optie_colors(selected_id(), colors())
    
    db_ping(runif(1))
  })
  
  
  ordering_opties <- callModule(jsonOrderModule, "edit_order_options",
                                data = selected_row,
                                order_column = reactive("volgorde_opties"),
                                label_column = reactive("opties")
  )
  
  observeEvent(ordering_opties(), {
    
    .reg$set_optie_order(selected_id(), ordering_opties())
    db_ping(runif(1))
  })
  
}
