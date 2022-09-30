
#' Shiny module for form configuration
#' @param id Shiny input id
#' @param title Title for the tab panel with settings
#' @param title_deleted Title for the tab panel with deleted fields
#' @param header_ui UI to add at the top of the panel with settings
#' @export
#' @rdname formAdminModule
formAdminUI <- function(id, 
                        title = "Formulieropstelling",
                        title_deleted = "Verwijderde invoervelden",
                        header_ui = NULL){
  
  ns <- NS(id)
  
  softui::tab_box(
    
    softui::tab_panel(
      title = title, 
      icon = bsicon("pencil-square"),
      
      header_ui,
      
      softui::fluid_row(
        column(12,
               softui::action_button(ns("btn_add_formfield"), "Toevoegen", 
                                     status = "light", icon = bsicon("plus")),
               
               tags$span(id = ns("span_edit_formorder"),
                         jsonFormSetupUI(ns("edit_formorder"), 
                                         icon = bsicon("columns"),
                                         label = "Layout",
                                         status = "secondary")
               ),
               
               shinyjs::hidden(
                 tags$span(id = ns("span_edit_formfield"),
                           softui::action_button(ns("btn_edit_formfield"), "Wijzig veld", 
                                                 status = "secondary",
                                                 icon = bsicon("pencil-square"))
                 )
               ),
               
               
               shinyjs::hidden(
                 tags$span(id = ns("span_edit_options"),
                           
                           softui::action_button(ns("btn_edit_options"),
                                                 label = "Keuzelijst", 
                                                 icon = bsicon("pencil-square"), 
                                                 status = "secondary")
                           
                 )
               ),
               shinyjs::hidden(
                 tags$span(id = ns("span_edit_order_options"),
                           
                           softui::action_button(ns("btn_edit_order_options"),
                                                 label = "Volgorde keuzelijst", 
                                                 icon = bsicon("pencil-square"), 
                                                 status = "secondary")
                           
                 )
               ),
               shinyjs::hidden(
                 tags$span(id = ns("span_edit_colors"),
                           
                           softui::action_button(ns("btn_edit_colors"),
                                                 label = "Kleuren", 
                                                 icon = bsicon("palette-fill"), 
                                                 status = "primary")
                 )
               ),
               
               shinyjs::hidden(
                 tags$span(id = ns("span_delete_formfield"),
                           softui::action_button(ns("btn_delete_formfield"), "Verwijder", 
                                                 status = "danger",
                                                 icon = bsicon("trash3"))
                 )
               )
               
        )
      ),
      softui::fluid_row(
        column(12,
               DT::dataTableOutput(ns("dt_form_invoervelden"))
        )
      )
      
    ),
    softui::tab_panel(
      title = title_deleted, icon = bsicon("recycle"),
      shinyjs::hidden(
        tags$span(id = ns("span_restore_formfield"),
                  softui::fluid_row(
                    column(12,
                           softui::action_button(ns("btn_restore_formfield"), 
                                                 "Invoerveld terugzetten", 
                                                 status = "secondary", 
                                                 icon = bsicon("arrow-return-left"))
                    )
                  )
        )
      ),
      softui::fluid_row(
        column(12,
               DT::dataTableOutput(ns("dt_deleted_invoervelden"))
        )
      )
      
      
      
    )
  )
  
}


#' @export
#' @rdname formAdminModule
formAdminModule <- function(input, output, session, .reg = NULL){
  
  
  db_ping <- reactiveVal()
  
  ns <- session$ns
  
  form_invul_data <- reactive({
    db_ping()
    .reg$get_input_fields(TRUE)
  })
  
  output$dt_form_invoervelden <- DT::renderDataTable({
    if(.reg$filterable){
      admin_table <- form_invul_data() %>%
        select(#"ID" = id_formulierveld, 
          "Kolomnaam" = column_field, 
          "Label" = label_field, 
          "Type" = type_field, 
          "Kolom" = form_section,
          "Volgorde" = order_field,
          "Opties" = options, 
          "Volgorde opties" = order_options, 
          "Kleuren" = colors, 
          "Verwijderbaar" = removable,
          "Filter" = make_filter,
          "Tooltip" = tooltip)
    } else {
      admin_table <- form_invul_data() %>%
        select(#"ID" = id_formulierveld, 
          "Kolomnaam" = column_field, 
          "Label" = label_field, 
          "Type" = type_field, 
          "Kolom" = form_section,
          "Volgorde" = order_field,
          "Opties" = options, 
          "Volgorde opties" = order_options, 
          "Kleuren" = colors, 
          "Verwijderbaar" = removable)
    }
    admin_table %>%
      softui::datatafel(selection = "single", dom = "tp", 
                        pageLength = 30, scrollX = TRUE, 
                        extensions = list())
    
  })
  
  selected_row <- reactive({
    ii <- input$dt_form_invoervelden_rows_selected
    if(is.null(ii)){
      return(NULL)
    }
    form_invul_data() %>% slice(ii)
  })
  
  selected_id <- reactive({
    req(selected_row())
    selected_row()$id_form
  })
  
  observeEvent(input$btn_add_formfield, {
    showModal(
      softui::modal(
        title = "Invoerveld toevoegen aan formulier",
        id_confirm = "btn_confirm_add_formfield",
        close_txt = "Annuleren",
        remove_modal_on_confirm = FALSE,
        
        textInput(session$ns("txt_column_name"), "Naam invoerveld"),
        radioButtons(session$ns("rad_type_formfield"), "Type invoerveld",
                     choices = configured_field_types),
        radioButtons(session$ns("rad_side_formfield"), "Links of rechts op het formulier?",
                     choices = c("Links" = 1,
                                 "Rechts" = 2)),
        if(.reg$filterable){
          radioButtons(session$ns("rad_make_filter"), "Wilt u op deze eigenschap kunnen filteren?",
                       choices = c("Ja" = TRUE,
                                   "Nee" = FALSE))
        },
        if(.reg$filterable){
          textInput(session$ns("txt_tooltip"), "Tooltip")
        }
        ,
        
      )
    )
  })
  
  observeEvent(input$btn_confirm_add_formfield, {
    
    if(.reg$filterable){
      make_filter <- input$rad_make_filter
      tooltip <- input$txt_tooltip
    } else {
      make_filter <- NULL
      tooltip <- NULL
    }
    
    if(stringr::str_trim(input$txt_column_name, side = "both") != ""){
      resp <- .reg$add_input_field_to_form(input$txt_column_name, 
                                           input$rad_type_formfield, 
                                           as.integer(input$rad_side_formfield),
                                           make_filter, tooltip)
      
      if(resp < 0){
        toastr_error("Deze kolom naam bestaat al, kies een andere naam.")
      } else {
        db_ping(runif(1))
        removeModal()  
      }
      
      
    } else {
      toastr_error("Vul een label in")
    }
    
  })
  
  form_setup <- callModule(jsonFormSetupModule, "edit_formorder", 
                           data = form_invul_data,
                           .reg = .reg,
                           side_column = reactive("form_section"),
                           order_column = reactive("order_field"),
                           id_column = reactive("id_form"),
                           label_column = reactive("label_field")
  )
  
  observeEvent(form_setup(), {
    .reg$edit_formulier_setup(form_setup())
    db_ping(runif(1))
  })
  
  observeEvent(selected_row(), ignoreNULL = FALSE, {
    sel <- selected_row()
    type <- sel$type_field
    
    # These fields don't have any editable options
    show_edit_options <- isTRUE(!type %in% c("freetext","numeric","date", "singlecheck"))
    
    shinyjs::toggleElement("span_edit_formfield", condition = !is.null(sel))
    shinyjs::toggleElement("span_edit_options", condition = (!is.null(sel) && show_edit_options))
    shinyjs::toggleElement("span_edit_colors", condition = (!is.null(sel) && show_edit_options))
    shinyjs::toggleElement("span_edit_order_options", condition = (!is.null(sel) && show_edit_options))
    shinyjs::toggleElement("span_delete_formfield", condition = (!is.null(sel) && sel$removable))
  })
  
  
  
  observeEvent(input$btn_edit_formfield, {
    
    req(selected_row())
    
    showModal(
      softui::modal(
        title = glue("Kolom label: {selected_row()$column_field}"),
        remove_modal_on_confirm = FALSE,
        
        textInput(session$ns("txt_edit_formfield_label"), "Label", 
                  value = selected_row()$label_field),
        
        if(.reg$filterable){
          
          tagList(
            radioButtons(session$ns("rad_edit_make_filter"), "Wilt u op deze eigenschap kunnen filteren?",
                         choices = c("Ja" = TRUE,
                                     "Nee" = FALSE),
                         selected = selected_row()$make_filter),
            textInput(session$ns("txt_edit_tooltip"), "Tooltip", value = selected_row()$tooltip)  
          )
          
        },
        
        id_confirm = "btn_confirm_edit_label"
      )
    )
  })
  
  
  observeEvent(input$btn_confirm_edit_label, {
    req(selected_row())
    
    if(stringr::str_trim(input$txt_edit_formfield_label, side = "both") != ""){
      .reg$edit_label_field(selected_id(), input$txt_edit_formfield_label)
      if(.reg$filterable){
        .reg$edit_filterable_column(selected_id(), input$rad_edit_make_filter, input$txt_edit_tooltip)
      }
      db_ping(runif(1))
      removeModal()
    } else {
      toastr_error("Vul een label in")
    }
    
  })
  
  # Reactive maken die de add/delete argument reactively doorgeeft gebaseerd op het type
  edit_options <- reactive({
    req(selected_row())
    type <- selected_row()$type_field
    
    if(type %in% c("singleselect","multiselect")){
      "add"
    } else {
      NULL
    }
    
  })
  
  edited_options <- softui::modalize(trigger_open = reactive(input$btn_edit_options),
                                     header_ui = tags$p("Pas de keuzelijst aan voor dit formulierveld."),
                                     ui_module = shintocatman::jsonEditModuleUI,
                                     server_module = shintocatman::jsonEditModule,
                                     title = "Keuzelijst",
                                     server_pars = list(
                                       options = edit_options,
                                       edit = reactive("value"),
                                       widths = c(2,10),
                                       value = reactive(selected_row()$options)
                                     ))
  
  observeEvent(edited_options(), {
    
    .reg$edit_options_field(selected_id(), edited_options())
    .reg$amend_options_order(selected_id(), edited_options())
    .reg$amend_options_colors(selected_id(), edited_options())
    db_ping(runif(1))
    
  })
  
  current_colors <- reactive({
    from_json(selected_row()$colors)
  })
  
  colors <- softui::modalize(trigger_open = reactive(input$btn_edit_colors),
                             ui_module = shintocatman::colorVectorPickModuleUI,
                             server_module = shintocatman::colorVectorPickModule,
                             title = "Kies kleuren",
                             server_pars = list(
                               n_colors = reactive(length(current_colors())), 
                               current_colors = current_colors,
                               labels = reactive(from_json(selected_row()$options)),
                               show_order = reactive(from_json(selected_row()$order_options))
                             ))
  
  
  observeEvent(colors(), {
    .reg$edit_options_colors(selected_id(), colors())
    db_ping(runif(1))
  })
  
  
  ordering_opties <- softui::modalize(trigger_open = reactive(input$btn_edit_order_options),
                                      header_ui = tags$p("Pas hier de volgorde van de keuzelijst aan voor dit formulierveld"),
                                      ui_module = shintocatman::jsonOrderModuleUI,
                                      server_module = shintocatman::jsonOrderModule,
                                      title = "Volgorde keuzelijst",
                                      server_pars = list(
                                        data = selected_row,
                                        label_column = reactive("options"),
                                        order_column = reactive("order_options")
                                      ))
  
  observeEvent(ordering_opties(), {
    
    .reg$set_options_order(selected_id(), ordering_opties())
    db_ping(runif(1))
  })
  
  observeEvent(input$btn_delete_formfield, {
    
    showModal(
      softui::modal(
        title = glue("'{selected_row()$label_field}' verwijderen?"),
        
        tags$p("Verwijderde invoervelden kunnen worden teruggezet in het formulier via de herstelknop op het tabblad 'Verwijderde invoervelden'."),
        
        id_confirm = "btn_confirm_delete_field",
        close_txt = "Annuleren"
      )
    )
  })
  
  observeEvent(input$btn_confirm_delete_field, {
    
    .reg$edit_zichtbaarheid_invoerveld(selected_id(), FALSE)
    .reg$edit_verwijder_datum(selected_id(), today())
    .reg$amend_formfield_order(selected_row()$form_section, selected_row()$order_field)
    db_ping(runif(1))
    
  })
  
  form_deleted_data <- reactive({
    db_ping()
    .reg$get_input_fields(FALSE)
  })
  
  output$dt_deleted_invoervelden <- DT::renderDataTable({
    
    form_deleted_data() %>%
      select(#"ID" = id_formulierveld, 
        "Kolomnaam" = column_field, 
        "Label" = label_field, 
        "Type" = type_field, 
        "Kolom" = form_section,
        "Verwijderd op" = date_deleted,
        "Volgorde" = order_field,
        "Opties" = options, 
        "Volgorde opties" = order_options, 
        "Kleuren" = colors, 
        "Verwijderbaar" = removable) %>%
      softui::datatafel(selection = "single", dom = "tp", 
                        pageLength = 30, scrollX = TRUE, extensions = list())
    
  })
  
  selected_row_deleted <- reactive({
    ii <- input$dt_deleted_invoervelden_rows_selected
    if(is.null(ii)){
      return(NULL)
    }
    form_deleted_data() %>% slice(ii)
  })
  
  selected_id_deleted <- reactive({
    selected_row_deleted()$id_form
  })
  
  observe({
    sel <- selected_row_deleted()
    shinyjs::toggleElement("span_restore_formfield", condition = !is.null(sel))
  })
  
  observeEvent(input$btn_restore_formfield, {
    
    req(selected_row_deleted())
    
    volg_veld_reset <- .reg$get_next_formorder_number(selected_row_deleted()$form_section)
    .reg$reset_volgorde_invoerveld(selected_id_deleted(), volg_veld_reset)
    .reg$edit_zichtbaarheid_invoerveld(selected_id_deleted(), TRUE)
    db_ping(runif(1))
  })
  
  
  return(db_ping)  
}