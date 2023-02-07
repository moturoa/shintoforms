
#' Nieuwe registratie Shiny module
#' @rdname registratie
#' @export
formUI <- function(id, buttons = TRUE, deletable = FALSE){
  
  ns <- shiny::NS(id)
  
  ui <- softui::fluid_page(
    
    # Extra block with any UI describing the signal (when editing only)
    softui::fluid_row(
      shiny::uiOutput(ns("ui_registration_description"))
    ),
    
    softui::fluid_row(id = ns("form_container"),
        shiny::column(6,
            shiny::uiOutput(ns("ui_input_left"))
        ),
        shiny::column(6,
            shiny::uiOutput(ns("ui_input_right"))
        )
    ),
    softui::fluid_row(id = ns("form_container_btm"),
        shiny::column(12,
            shiny::uiOutput(ns("ui_input_bottom"))
        )
    ),
    
    # tags$div(style = "display: none;",
    #   verbatimTextOutput(ns("txt_out"))
    # ),
    # 
    
    if(buttons){
      
      softui::fluid_row(class = "justify-content-end",
                        
                        shiny::tags$hr(),
                        
                        shiny::column(3,
                          if(deletable){
                            softui::action_button(ns("btn_delete_registratie"), 
                                                  "Verwijderen ...", 
                                                  icon = bsicon("trash"), 
                                                  status = "danger")
                          }
                        ),
                        shiny::column(5),
                        shiny::column(2,
                               softui::action_button(ns("btn_cancel"), 
                                                     "Annuleren", 
                                                     icon = bsicon("x-lg"), 
                                                     status = "warning")
                        ),
                        shiny::column(2,
                               softui::action_button(ns("btn_register_new_signal"), 
                                                     "Opslaan", 
                                                     icon = bsicon("cloud-arrow-up"), 
                                                     status = "success")
                        )
      )
      
      
    }

    
  )
  
  shiny::tagList(ui, shintoforms_dependencies())
  
}

#' @rdname registratie
#' @export
formModule <- function(input, output, session, .reg = NULL, 
                       ping_update = reactive(NULL),
                       current_user, 
                       trigger = reactive(NULL),
                       data = reactive(NULL),
                       bucket_data = reactive(NULL),
                       write_method = reactive("new"),
                       
                       registration_description_function = function(data)NULL,
                       
                       confirm = reactive(NULL),
                       cancel = reactive(NULL),
                       
                       disabled = reactive(FALSE),
                       
                       callback_confirm = function(){},
                       callback_cancel = function(){},
                       
                       message_success = "Registratie opgeslagen",
                       message_error = "Er is een fout opgetreden",
                       message_deleted = "Registratie verwijderd"
                       ) {
  
  ns <- session$ns
  
  current_reg_id <- reactive({ 
    
    req(trigger())
    
    if(write_method() == "new"){
      return(uuid::UUIDgenerate())
    } else {
      return(data()[[.reg$data_columns$id]])
    } 
  })
  
  # Extra  block with description / other UI / whatever / of the registration
  output$ui_registration_description <- renderUI({
    
    registration_description_function(data())
    
  })

  # reactive maken zodat ie update als er iets wordt veranderd in admin, zie admin scherm hoe dat moet.
  cfg_left <- reactive({
      ping_update()
    .reg$get_form_fields(1)
  })
  
  cfg_right <- reactive({
    ping_update()
    .reg$get_form_fields(2)
  })
  
  cfg_bottom <- reactive({
    ping_update()
    .reg$get_form_fields(3)
  })
  
  # prepare inject object: module functions must get an ID and HTML
  inject_prep <- reactive({
    
    inj <- .reg$inject
    if(is.null(inj))return(NULL)
    
    trigger()
    
    inj <- lapply(inj, function(x){ 
      if(is.null(x$html) & !is.null(x$ui_module)){
        x$id <- uuid::UUIDgenerate()
        x$html <- x$ui_module(ns(x$id), data = data(), columns = x$columns, x$module_ui_pars)
      }
      x
    })
    
    inj
    
  })
  
  
 
  inject_left <- reactive({
    
    obj <- inject_prep()
    if(is.null(obj))return(NULL)
    obj[sapply(obj, "[[", "section") == 1]
    
  })
  
  inject_right <- reactive({
    
    obj <- inject_prep()
    if(is.null(obj))return(NULL)
    obj[sapply(obj, "[[", "section") == 2]
    
  })
  
  inject_bottom <- reactive({
    
    obj <- inject_prep()
    if(is.null(obj))return(NULL)
    obj[sapply(obj, "[[", "section") == 3]
    
  })
  
  ui_ping <- reactiveVal()
  output$ui_input_left <- renderUI({
    
    ui_ping(runif(1))
    trigger()
    formSectionModuleUI(session$ns("form_left"), cfg = cfg_left(), .reg = .reg,
                        data = data(), disabled = disabled(),
                        inject = inject_left())
    
  })
  
  output$ui_input_right <- renderUI({
    
    ui_ping(runif(1))
    trigger()
    formSectionModuleUI(session$ns("form_right"), cfg = cfg_right(), .reg = .reg,
                        data = data(), disabled = disabled(),
                        inject = inject_right())
    
  })
  
  
  output$ui_input_bottom <- renderUI({
    
    ui_ping(runif(1))
    trigger()
    formSectionModuleUI(session$ns("form_bottom"), cfg = cfg_bottom(), .reg = .reg,
                        data = data(), disabled = disabled(),
                        inject = inject_bottom())
    
  })
  
  
  
  edit_left <- callModule(formSectionModule, "form_left", cfg = cfg_left, .reg = .reg, data = data, trigger = trigger)
  edit_right <- callModule(formSectionModule, "form_right", cfg = cfg_right, .reg = .reg, data = data, trigger = trigger)
  edit_bottom <- callModule(formSectionModule, "form_bottom", cfg = cfg_bottom, .reg = .reg, data = data, trigger = trigger)
  
  
  
  edits_configured <- reactive({
    
    req(edit_left())
    
    out <- c(lapply(edit_left(), function(x)x()),
             lapply(edit_right(), function(x)x()))
    
    i_rea <- which(sapply(out, is.reactive))
    if(length(i_rea)){
      names(out)[i_rea] <- ""
      out <- lapply(out, function(x)if(is.reactive(x)){x()}else{x})
      out <- do.call(c, out)
    }
    
    out
    
  })
  
  modules_extra <- reactiveVal()
  modules_relations <- reactiveVal()
  
  # fill in the object relation
  bucket_data_formatted <- reactiveVal(NULL)
  observeEvent(current_reg_id(),{
    if(is.null(bucket_data())){
      bucket_data_formatted(NULL)
    } else {
      buckid <-   uuid::UUIDgenerate()
      
      bucketinfo <- bucket_data()
      bucketinfo$collector_id <- current_reg_id()
      bucketinfo$collector_type <- .reg$class_type
      bucketinfo$id <- buckid
      bucketinfo$relation_id <- buckid
      bucketinfo$relation_type <- 'primary'
      bucketinfo$comment <- ''
      bucketinfo$timestamp <- .reg$postgres_now()
      bucketinfo$status <- "1" 
      
      bucket_data_formatted(as.data.frame(bucketinfo))
    }
    
  })
  observe({
     
    extra <- inject_prep()
    req(extra)
    req(ui_ping())
    
    withmod <- which(!sapply(sapply(extra, "[[", "ui_module"), is.null) & 
                       sapply(sapply(extra, "[[", "relation"), is.null))
 
    
    if(length(withmod)){
      
      extra_values <- lapply(seq_along(withmod), function(i){
        j <- withmod[i] 
        lis_call <- c(list(
          module = extra[[j]]$server_module,
          id = extra[[j]]$id,
          columns = extra[[j]]$columns,
          data = data
        ), extra[[j]]$module_server_pars)
        
        do.call(callModule, lis_call)
      })
      
      modules_extra(extra_values)
    }
    
    # for all relations
    withmod_rel <- which(!sapply(sapply(extra, "[[", "ui_module"), is.null) & 
                     !sapply(sapply(extra, "[[", "relation"), is.null))
    
    if(length(withmod_rel)){
      
      relation_values <- lapply(seq_along(withmod_rel), function(i){
        j <- withmod_rel[i]
        
        lis_call <- c(list(
          module = extra[[j]]$server_module,
          id = extra[[j]]$id,
          columns = extra[[j]]$columns,
          data = data,
          bucket_data = bucket_data_formatted,
          reg_id=current_reg_id
        ), extra[[j]]$module_server_pars)
        
        do.call(callModule, lis_call)
      }) 
      
      modules_relations(relation_values)
    }
     
  })
  
  edits <- reactive({
    ext <- edits_extra() 
    
    # configured are all 
    out <- edits_configured()
    
    if(length(ext)){
      for(i in seq_along(ext)){
        out[names(ext[[i]])] <- ext[[i]]
      }
    }
    out
  })
  
  edits_extra <- reactive({
    req(length(modules_extra()))
    lapply(modules_extra(), function(x)x())
  })
  
  edits_relations <- reactive({
    req(length(modules_relations()))  
    
    rel <- lapply(modules_relations(), function(x)x())  
    
    dplyr::bind_rows(rel) %>% 
      mutate(username = current_user)
    
  })
  
  observeEvent(input$btn_register_new_signal, {
    
    showModal(
      softui::modal(
        title = "Opslaan",
        id_confirm = "btn_confirm_new_registration",
        
        tags$p("Je gaat deze registratie opslaan. Weet je het zeker?"),
        tags$p("Gebruiker: ", current_user),
        tags$p(format(Sys.time(), "%m/%d/%Y %H:%M"))
      )
    )
    
  })
  
  
  out_ping <- reactiveVal()
  
  observeEvent(input$btn_cancel, {
    out_ping(list(ping = runif(1), action = "cancel"))
    callback_cancel()
  })
  
  
  confirm_new_reg <- reactiveVal()
  observeEvent(confirm(), confirm_new_reg(runif(1)))
  observeEvent(input$btn_confirm_new_registration, confirm_new_reg(runif(1)))
  
  
  observeEvent(confirm_new_reg(), {
 
    if(write_method() == "new"){ 
      resp <- .reg$write_new_registration(edits(), 
                                          user_id = current_user, 
                                          current_reg_id=current_reg_id())
      resp2 <- .reg$write_new_relations(data = edits_relations(),
                                        registration_id = current_reg_id())
    } else { 
      resp <- .reg$edit_registration(old_data = data(), 
                                     new_data = edits(), 
                                     user_id = current_user, 
                                     current_reg_id=current_reg_id()) 
      resp2 <- .reg$update_relations(edits_relations(),
                                     registration_id = current_reg_id())
    }
    

    if(resp & resp2){
      toastr_success(message_success)
    } else {
      toastr_error(message_error)
    }

    out_ping(list(ping = runif(1), action = "save"))
    
    callback_confirm()
  })
  
  
  
  observeEvent(input$btn_delete_registratie, {
    showModal(
      softui::modal(
        title = "Registratie verwijderen",
        id_confirm = "btn_confirm_delete_registration", confirm_txt = "Ja, verwijderen", confirm_icon = softui::bsicon("trash"),
        close_txt = "Annuleren", 
        close_icon = softui::bsicon("x-lg"),
        tags$p(glue("U staat op het punt om registratie {data()[[.reg$data_columns$name]]} (id: {data()[[.reg$data_columns$id]]}) te verwijderen. 
        Als u dit zeker weet kunt u dit hier bevestigen, anders kunt u de keuze annuleren.")),
        tags$p("Gebruiker: ", current_user),
        tags$p(format(Sys.time(), "%m/%d/%Y %H:%M"))
      )
    )
  })

  observeEvent(input$btn_confirm_delete_registration, {
    
    .reg$delete_registration(data()[[.reg$data_columns$id]],
                             method = "soft")
    
    if(!is.null(.reg$event_data)){
      .reg$delete_event(data()[[.reg$data_columns$id]])
    }

    # delete relation data
    # current_relations = .reg$get_objects_for_collector(collector_id = current_reg_id())
    # .reg$soft_delete_relations(current_relations$id) 
    
    toastr_success(message_deleted)
    
    out_ping(list(ping = runif(1), action = "delete"))
    callback_confirm()
  })
  
  
  return(out_ping)
}






#---- Testing

test_formModule <- function(){
  library(withr)
  with_dir("test", source("global.R"))
  
  
  testmoduleui <- function(id){
    ns <- NS(id)
    
    tagList(
      radioButtons(ns("value1"), "Keuze 1", choices = LETTERS[1:4]),
      radioButtons(ns("value2"), "Keuze 2", choices = LETTERS[1:4])
    )
    
  }
  
  testmodule <- function(input,output,session,columns){
    reactive(setNames(list(input$value1,input$value2),columns))
  }
  
  
  library(shiny)
  library(shinyWidgets)
  
  ui <- softui::simple_page(
    shinyjs::useShinyjs(),
    softui::box(
      shinyWidgets::materialSwitch("toggle", "Aan/Uit", value = FALSE),  
      formUI("test")
    )
  )
  
  server <- function(input, output, session) {
    
    callModule(formModule, "test", .reg = .reg,
               current_user = "devuser",
               disabled = reactive(input$toggle))
  }
  
  shinyApp(ui, server)
  
  
  
}














