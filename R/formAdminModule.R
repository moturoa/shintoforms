
#' Shiny module for form configuration
#' @param id Shiny input id
#' @param title Title for the tab panel with settings
#' @param title_deleted Title for the tab panel with deleted fields
#' @param option_layout Show button for form layout? (default TRUE)
#' @param option_add_field Show button to add a field (default TRUE)
#' @param option_delete_field Show button to delete a field? (default TRUE)
#' @param show_fields Named vector selecting column names from the renamed definition table, to
#' display columns in admin table
#' @param show_fields_deleted Same, for page with deleted form fields
#' @param header_ui UI to add at the top of the panel with settings
#' @export
#' @importFrom shinytoastr toastr_error toastr_success
#' @importFrom shintocatman htmlInput jsonEditModuleUI jsonEditModule
#' @importFrom shintocatman colorVectorPickModule colorVectorPickModuleUI jsonOrderModule jsonOrderModuleUI
#' @importFrom shintocatman from_json to_json useHtmlInput
#' @importFrom shinyjs hidden toggleElement disabled
#' @importFrom dplyr filter arrange sym transmute slice any_of select bind_rows mutate between group_by ungroup
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom stringr str_trim
#' @rdname formAdminModule
formAdminUI <- function(id, 
                        title = "Formulieropstelling",
                        title_deleted = "Verwijderde invoervelden",
                        tab_deleted_input_fields = TRUE,
                        
                        option_layout = TRUE,
                        option_add_field = TRUE,
                        option_delete_field = TRUE,
                        
                        btn_add_text = "Toevoegen",
                        btn_add_status = "light",
                        
                        btn_layout_text = "Layout",
                        btn_layout_status = "secondary",
                        
                        btn_edit_text = "Wijzig veld",
                        btn_edit_status = "secondary",
                        
                        btn_nested_text = "Gekoppelde keuzelijst",
                        btn_nested_status = "secondary",
                        
                        btn_select_text = "Keuzelijst",
                        btn_select_status = "secondary",
                        
                        btn_optionsorder_text = "Volgorde keuzelijst",
                        btn_optionsorder_status = "secondary",
                        
                        btn_colors_text = "Kleuren",
                        btn_colors_status = "primary",
                        
                        btn_delete_text = "Verwijderen",
                        btn_delete_status = "danger",
                        
                        header_ui = NULL){
  
  ns <- shiny::NS(id)
  
  softui::tab_box(
    
    softui::tab_panel(
      title = title, 
      icon = softui::bsicon("pencil-square"),
      
      header_ui,
      
      softui::fluid_row(
        column(12, style = "height: 40px;",
               
               if(option_add_field){
                 softui::action_button(ns("btn_add_formfield"), btn_add_text, 
                                       status = btn_add_status, icon = bsicon("plus"))  
               },
               
               if(option_layout){
                 shiny::tags$span(id = ns("span_edit_formorder"),
                           jsonFormSetupUI(ns("edit_formorder"), 
                                           icon = bsicon("columns"),
                                           label = btn_layout_text,
                                           status = btn_layout_status)
                 )  
               },
               
               shinyjs::hidden(
                 shiny::tags$span(id = ns("span_edit_formfield"),
                           softui::action_button(ns("btn_edit_formfield"), 
                                                 btn_edit_text, 
                                                 status = btn_edit_status,
                                                 icon = bsicon("pencil-square"))
                 )
               ),

               shinyjs::hidden(
                 shiny::tags$span(id = ns("span_edit_nested_options"),
                           
                           softui::modal_action_button(ns("btn_edit_nested_options"),
                                                       ns("modal_nested_options"),
                                                       label = btn_nested_text, 
                                                       icon = softui::bsicon("pencil-square"), 
                                                       status = btn_nested_status),
                           
                           softui::ui_modal(
                             title = "Gekoppelde keuzelijst",
                             id = ns("modal_nested_options"),
                             size = "l", 
                             confirm_txt = "Opslaan", close_txt = "Annuleren",
                             id_confirm = ns("btn_confirm_nested_options"),
                             
                             subChoiceEditorUI(ns("mod_edit_nested_options"))
                          )
                           
                 )
               ),
               
               shinyjs::hidden(
                 shiny::tags$span(id = ns("span_edit_options"),
                           
                           softui::modal_action_button(ns("btn_edit_options"),
                                                       ns("modal_edit_options"),
                                                 label = btn_select_text, 
                                                 icon = softui::bsicon("pencil-square"), 
                                                 status = btn_select_status),
                           
                           softui::ui_modal(id = ns("modal_edit_options"), ns = ns,
                                            title = "Keuzelijst",
                                            id_confirm = "btn_confirm_edit_options",
                                            tags$p("Pas de keuzelijst aan voor dit formulierveld."),
                                            shintocatman::jsonEditModuleUI(ns("modal_json_edit_options")))
                             
                             
                           )
                           
                           
               ),
               shinyjs::hidden(
                 shiny::tags$span(id = ns("span_edit_order_options"),
                           
                           softui::action_button(ns("btn_edit_order_options"),
                                                 label = btn_optionsorder_text, 
                                                 icon = softui::bsicon("pencil-square"), 
                                                 status = btn_optionsorder_status)
                           
                 )
               ),
               shinyjs::hidden(
                 shiny::tags$span(id = ns("span_edit_colors"),
                           
                           softui::action_button(ns("btn_edit_colors"),
                                                 label = btn_colors_text, 
                                                 icon = softui::bsicon("palette-fill"), 
                                                 status = btn_colors_status)
                 )
               ),
               
               if(option_delete_field){
                 shinyjs::hidden(
                   shiny::tags$span(id = ns("span_delete_formfield"),
                             softui::action_button(ns("btn_delete_formfield"), btn_delete_text, 
                                                   status = btn_delete_status,
                                                   icon = softui::bsicon("trash3"))
                   )
                 )  
               }
               
               
        )
      ),
      softui::fluid_row(
        shiny::column(12,
               DT::dataTableOutput(ns("dt_form_invoervelden"))
        )
      )
      
    ),
    
    if(tab_deleted_input_fields){
      softui::tab_panel(
        title = title_deleted, icon = bsicon("recycle"),
        shinyjs::hidden(
          tags$span(id = ns("span_restore_formfield"),
                    softui::fluid_row(
                      column(12,
                             softui::action_button(ns("btn_restore_formfield"), 
                                                   "Invoerveld terugzetten", 
                                                   status = "secondary", 
                                                   icon = bsicon("arrow-return-left")),
                             softui::action_button(ns("btn_reallydelete_formfield"), 
                                                   "Permanent verwijderen", 
                                                   status = "danger", 
                                                   icon = bsicon("x-square-fill"))
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
    }
  )
  
}


#' @export
#' @rdname formAdminModule
formAdminModule <- function(input, output, session, .reg = NULL,
                            option_layout = TRUE,
                            show_fields = c(
                              "Kolomnaam" = "column_field", 
                              "Label" = "label_field", 
                              "Type" = "type_field", 
                              "Kolom" = "form_section",
                              "Volgorde" = "order_field",
                              "Opties" = "options", 
                              "Volgorde opties" = "order_options", 
                              "Kleuren" = "colors", 
                              "Verwijderbaar" = "removable"
                            ),
                            
                            show_fields_deleted = c(
                              "Kolomnaam" = "column_field",
                              "Label" = "label_field",
                              "Type" = "type_field",
                              "Kolom" = "form_section",
                              "Verwijderd op" = "date_deleted",
                              "Volgorde" = "order_field",
                              "Opties" = "options",
                              "Volgorde opties" = "order_options",
                              "Kleuren" = "colors",
                              "Verwijderbaar" = "removable"),
                            
                            allow_delete_option = FALSE,
                            reload_ping = reactive(NULL)){
  
  
  db_ping <- reactiveVal()
  
  ns <- session$ns
  
  form_invul_data <- reactive({
    db_ping()
    reload_ping()
    
    req(.reg$has_connection())
    
    .reg$get_input_fields(TRUE) %>%
      dplyr::arrange(column_field)
  })
  
  observeEvent(form_invul_data(), {
    cols <- form_invul_data() %>% pull(column_field) %>% sort
    updateSelectInput(session, "sel_nested_column", choices = cols)
  })
  
  output$dt_form_invoervelden <- DT::renderDataTable({
    
    reload_ping()
    
    data <- form_invul_data()
    
    # Format options field in HTML
    data$options <- apply(data, 1, function(row){
      
      x <- .reg$from_json(row[["options"]])
      
      if(length(x) == 0){
        
        ""
        
      } else if(row[["type_field"]] == "nestedselect"){
        
        as.character(tags$span(style = "margin: 2px; border-radius:6px; padding: 8px; background-color: #f29102; color: white;", 
                  "Gekoppelde keuzelijst"))
        
      } else{
        
        # rearrange order. if filled in.
        o <- .reg$from_json(row[["order_options"]])
        
        if(!is.list(o) && length(o) == length(x)){
          x <- x[o]  
        }
        
        
        len <- length(x)
        if(len > 5){
          x <- c(x[1:5], list(paste("+", len-5, "opties")))
        }
        
        l <- lapply(x, function(el){
          tags$span(style = "margin: 2px; border-radius:6px; padding: 8px; background-color: #0d6efd; color: white;", el)
        })
        
        
        do.call(paste,l)
        
      }
      
    })
    
    # Format color blocks
    data$colors <- apply(data, 1, function(row){
      
      x <- .reg$from_json(row[["colors"]])
      
      if(length(x) == 0){
        
        ""
        
      } else {
      
        o <- .reg$from_json(row[["order_options"]])
        x <- x[o]
        
        len <- length(x)
        if(len > 5){
          x <- x[1:5]
        }
        
        l <- lapply(x, function(el){
          tags$div(style = glue("margin: 2px; height: 24px; width: 24px;",
                                "display: inline-block;",
                                "background-color: {el}; border: 1px solid black;"))
        })
        
        do.call(paste,l)  
      }
      
    })
    

    data %>%
      dplyr::select(dplyr::any_of(show_fields)) %>% 
      softui::datatafel(selection = "single", dom = "tp", 
                        pageLength = 30, scrollX = TRUE, 
                        extensions = list())
    
  })
  
  selected_row <- shiny::reactive({
    ii <- input$dt_form_invoervelden_rows_selected
    if(is.null(ii)){
      return(NULL)
    }
    
    form_invul_data() %>% slice(ii)
  })
  
  selected_id <- shiny::reactive({
    req(selected_row())
    selected_row()$id_form
  })
  
  observeEvent(input$btn_add_formfield, {
    shiny::showModal(
      softui::modal(
        title = "Invoerveld toevoegen aan formulier",
        id_confirm = "btn_confirm_add_formfield",
        close_txt = "Annuleren",
        remove_modal_on_confirm = FALSE,
        
        textInput(session$ns("txt_column_name"), "Naam invoerveld"),
        radioButtons(session$ns("rad_type_formfield"), "Type invoerveld",
                     choices = configured_field_types),
        
        uiOutput(session$ns("ui_nested_select_options")),
        
        if(option_layout){
          radioButtons(session$ns("rad_side_formfield"), "Links of rechts op het formulier?",
                       choices = c("Links" = 1,
                                   "Rechts" = 2))  
        }
        
        # if(.reg$filterable){
        #   radioButtons(session$ns("rad_make_filter"), "Wilt u op deze eigenschap kunnen filteren?",
        #                choices = c("Ja" = TRUE,
        #                            "Nee" = FALSE))
        # },
        # if(.reg$filterable){
        #   textInput(session$ns("txt_tooltip"), "Tooltip")
        # }
        # ,
        
      )
    )
  })
  
  
  output$ui_nested_select_options <- shiny::renderUI({
    
    req(isTRUE(input$rad_type_formfield == "nestedselect"))
    
    shiny::textInput(session$ns("txt_secondary_column_name"), "Naam gekoppeld invoerveld")
    
    
  })
  
  
  shiny::observeEvent(input$btn_confirm_add_formfield, {
    
    if(.reg$filterable){
      make_filter <- input$rad_make_filter
      tooltip <- input$txt_tooltip
    } else {
      make_filter <- NULL
      tooltip <- NULL
    }
    
    colname <- stringr::str_trim(input$txt_column_name, side = "both")
    
    if(colname == ""){
      shinytoastr::toastr_error("Vul een label in")
    }
    req(colname)
    
    req(.reg$has_connection())
    
    resp <- .reg$add_input_field_to_form(label_field = input$txt_column_name, 
                                         type_field = input$rad_type_formfield, 
                                         form_section = as.integer(input$rad_side_formfield),
                                         filterable = make_filter, 
                                         tooltip = tooltip,
                                         column_2_name = input$txt_secondary_column_name)
    
    if(resp < 0){
      shinytoastr::toastr_error("Deze kolom naam bestaat al, kies een andere naam.")
    } else {
      db_ping(runif(1))
      shiny::removeModal()  
    }
    
  })
  
  form_setup <- shiny::callModule(jsonFormSetupModule, "edit_formorder", 
                           data = form_invul_data,
                           .reg = .reg,
                           side_column = shiny::reactive("form_section"),
                           order_column = shiny::reactive("order_field"),
                           id_column = shiny::reactive("id_form"),
                           label_column = shiny::reactive("label_field")
  )
  
  shiny::observeEvent(form_setup(), {
    .reg$edit_formulier_setup(form_setup())
    db_ping(runif(1))
  })
  
  shiny::observeEvent(selected_row(), ignoreNULL = FALSE, {
    sel <- selected_row()
    type <- sel$type_field
    
    # These fields don't have any editable options
    show_edit_options <- isTRUE(!type %in% c("freetext","numeric","date", "singlecheck"))
    
    shinyjs::toggleElement("span_edit_formfield", condition = !is.null(sel))
    shinyjs::toggleElement("span_edit_options", condition = (!is.null(sel) && show_edit_options && type != "nestedselect"))
    shinyjs::toggleElement("span_edit_colors", condition = (!is.null(sel) && show_edit_options && type != "nestedselect"))
    shinyjs::toggleElement("span_edit_order_options", condition = (!is.null(sel) && show_edit_options && type != "nestedselect"))
    shinyjs::toggleElement("span_delete_formfield", condition = (!is.null(sel) && isTRUE(sel[["removable"]])))
    
    shinyjs::toggleElement("span_set_nested_key_column", condition = (!is.null(sel) && type == "nestedselect"))
    shinyjs::toggleElement("span_edit_nested_options", condition = (!is.null(sel) && type == "nestedselect"))
  })
  

  cur_column_2_label <- shiny::reactive({
    row <- selected_row()
    if(row$type_field == "nestedselect"){
      opts <- .reg$from_json(row$options)
      opts$label
    }
  })


  shiny::observeEvent(input$btn_edit_formfield, {

    row <- selected_row()
    req(row)

    shiny::showModal(
      softui::modal(
        title = glue("Kolom label: {selected_row()$column_field}"),
        remove_modal_on_confirm = FALSE,

        textInput(session$ns("txt_edit_formfield_label"), "Label",
                  value = row$label_field),

        if(row$type_field == "nestedselect"){
          textInput(session$ns("txt_edit_column_2_label"), "Gekoppelde keuze label",
                    value = cur_column_2_label())
        },

        if(row$type_field %in% c("freetext","html")){
          selectInput(session$ns("sel_new_formfield_type"), "Selecteer nieuw input veld type",
                      choices = c("freetext","html","singleselect","multiselect"),
                      selected = row$type_field)
        },

        id_confirm = "btn_confirm_edit_label"
      )
    )
  })
  
  
  shiny::observeEvent(input$btn_confirm_edit_label, {

    row <- selected_row()
    req(row)

    if(stringr::str_trim(input$txt_edit_formfield_label, side = "both") != ""){

      .reg$edit_label_field(selected_id(), input$txt_edit_formfield_label)

      if(row$type_field == "nestedselect"){
        .reg$edit_nested_column_label(selected_id(), row$options, input$txt_edit_column_2_label)
      }

      # if(.reg$filterable){
      #   .reg$edit_filterable_column(selected_id(), input$rad_edit_make_filter, input$txt_edit_tooltip)
      # }

      sel_new <- input$sel_new_formfield_type
      if(!is.null(sel_new) && sel_new != ""){
        allowed_new <- c("html","freetext","singleselect","multiselect")
        if(sel_new %in% allowed_new){
          .reg$edit_field_type(selected_id(), sel_new)
        } else {
          shinytoastr::toastr_error("Ongeldig nieuw veld type")
        }

      }

      db_ping(runif(1))
      shiny::removeModal()
    } else {
      shinytoastr::toastr_error("Vul een label in")
    }

  })
  

  # Reactive maken die de add/delete argument reactively doorgeeft gebaseerd op het type
  edit_options <- shiny::reactive({
    req(selected_row())
    type <- selected_row()$type_field

    if(type %in% c("singleselect","multiselect","nestedselect")){
      if(allow_delete_option) c("add","delete") else "add"
    } else {
      NULL
    }

  })

  edited_options <- callModule(shintocatman::jsonEditModule, "modal_json_edit_options",
                               options = edit_options,
                               edit = reactive("value"),
                               widths = c(2,10),
                               value = reactive(selected_row()$options)
                               )

  observeEvent(input$btn_confirm_edit_options, {

    .reg$edit_options_field(selected_id(), edited_options())
    #.reg$amend_nested_options_key(selected_id(), edited_options())
    .reg$amend_options_order(selected_id(), edited_options())
    .reg$amend_options_colors(selected_id(), edited_options())
    db_ping(runif(1))

  })


  #----- Nested select choices

  # setting the main column  (level 1 choices)
  shiny::observeEvent(input$btn_confirm_set_nested_key_column, {

    col_sel <- input$sel_nested_column
    .reg$prepare_nested_choice_column(selected_id(),
                                      name = col_sel,
                                      options = .reg$get_field_choices(col_sel))

    db_ping(runif(1))
  })


  # setting level 2 choices
  nested_options <- shiny::reactive({
    if(is.null(selected_row()) || nrow(selected_row()) == 0){
      return(NULL)
    }
    .reg$from_json(selected_row()$options)
  })

  nested_choices_out <- shiny::callModule(subChoiceEditor, "mod_edit_nested_options", data = nested_options, json = FALSE)


  shiny::observeEvent(input$btn_confirm_nested_options, {

    .reg$edit_options_field(selected_id(), nested_choices_out())

    db_ping(runif(1))
  })

  current_colors <- shiny::reactive({
    .reg$from_json(selected_row()$colors)
  })

  colors <- softui::modalize(trigger_open = shiny::reactive(input$btn_edit_colors),
                             ui_module = shintocatman::colorVectorPickModuleUI,
                             server_module = shintocatman::colorVectorPickModule,
                             title = "Kies kleuren",
                             server_pars = list(
                               n_colors = shiny::reactive(length(current_colors())),
                               current_colors = current_colors,
                               labels = shiny::reactive(.reg$from_json(selected_row()$options)),
                               show_order = shiny::reactive(.reg$from_json(selected_row()$order_options))
                             ))


  shiny::observeEvent(colors(), {
    .reg$edit_options_colors(selected_id(), colors())
    db_ping(runif(1))
  })


  ordering_opties <- softui::modalize(trigger_open = shiny::reactive(input$btn_edit_order_options),
                                      header_ui = tags$p("Pas hier de volgorde van de keuzelijst aan voor dit formulierveld"),
                                      ui_module = shintocatman::jsonOrderModuleUI,
                                      server_module = shintocatman::jsonOrderModule,
                                      title = "Volgorde keuzelijst",
                                      server_pars = list(
                                        data = selected_row,
                                        label_column = reactive("options"),
                                        order_column = reactive("order_options")
                                      ))

  shiny::observeEvent(ordering_opties(), {
    .reg$set_options_order(selected_id(), ordering_opties())
    db_ping(runif(1))
  })

  shiny::observeEvent(input$btn_delete_formfield, {

    shiny::showModal(
      softui::modal(
        title = glue("'{selected_row()$label_field}' verwijderen?"),

        tags$p("Verwijderde invoervelden kunnen worden teruggezet in het formulier via de herstelknop op het tabblad 'Verwijderde invoervelden'."),

        id_confirm = "btn_confirm_delete_field",
        close_txt = "Annuleren"
      )
    )
  })

  shiny::observeEvent(input$btn_confirm_delete_field, {

    .reg$edit_zichtbaarheid_invoerveld(selected_id(), FALSE)
    .reg$edit_verwijder_datum(selected_id(), today())
    .reg$amend_formfield_order(selected_row()$form_section, selected_row()$order_field)
    db_ping(runif(1))

  })

  form_deleted_data <- shiny::reactive({
    db_ping()
    req(.reg$has_connection())
    .reg$get_input_fields(FALSE)
  })

  output$dt_deleted_invoervelden <- DT::renderDataTable({

    form_deleted_data() %>%
      dplyr::select(dplyr::any_of(show_fields_deleted)) %>%
      softui::datatafel(selection = "single", dom = "tp",
                        pageLength = 30, scrollX = TRUE, extensions = list())

  })

  selected_row_deleted <- shiny::reactive({
    ii <- input$dt_deleted_invoervelden_rows_selected
    if(is.null(ii)){
      return(NULL)
    }
    form_deleted_data() %>% slice(ii)
  })

  selected_id_deleted <- shiny::reactive({
    selected_row_deleted()$id_form
  })

  shiny::observe({
    sel <- selected_row_deleted()
    shinyjs::toggleElement("span_restore_formfield", condition = !is.null(sel))
  })

  shiny::observeEvent(input$btn_restore_formfield, {

    req(selected_row_deleted())

    volg_veld_reset <- .reg$get_next_formorder_number(selected_row_deleted()$form_section)
    .reg$reset_volgorde_invoerveld(selected_id_deleted(), volg_veld_reset)
    .reg$edit_zichtbaarheid_invoerveld(selected_id_deleted(), TRUE)
    db_ping(runif(1))
  })


  shiny::observeEvent(input$btn_reallydelete_formfield, {
    id <- selected_id_deleted()
    req(id)
    .reg$really_delete_formfield(id)
    db_ping(runif(1))

  })

  
  
  return(db_ping)  
}