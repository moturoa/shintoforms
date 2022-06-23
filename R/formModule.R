
#' Nieuwe registratie Shiny module
#' @rdname registratie
#' @export
formUI <- function(id){
  
  ns <- NS(id)
  
  softui::fluid_page(

    softui::fluid_row(
      column(6,
        uiOutput(ns("ui_input_left"))
      ),
      column(6,
        uiOutput(ns("ui_input_right"))
      )
    ),
    
    # tags$div(style = "display: none;",
    #   verbatimTextOutput(ns("txt_out"))
    # ),
    # 
    softui::fluid_row(class = "justify-content-end",
      
      tags$hr(),
     
      column(6,
         softui::action_button(ns("btn_cancel"), 
                               "Annuleren", 
                               icon = bsicon("x-lg"), 
                               status = "danger"),      
         softui::action_button(ns("btn_register_new_signal"), 
                      "Opslaan", 
                      icon = bsicon("cloud-arrow-up"), 
                        status = "success")
      )
    )
  )
   
  
}

#' @rdname registratie
#' @export
formModule <- function(input, output, session, .reg = NULL, 
                                    ping_update = reactive(NULL),
                                    current_user, 
                                    data = reactive(NULL),
                                    callback_confirm = function(){},
                                    callback_cancel = function(){},
                                    inject = reactive(NULL)) {
  
  ns <- session$ns
  
  nieuwe_registratie_ping <- reactiveVal()
  
  # reactive maken zodat ie update als er iets wordt veranderd in admin, zie admin scherm hoe dat moet. 
  cfg_left <- reactive({
    ping_update()
    .reg$get_form_fields(1)
  })
  
  cfg_right <- reactive({
    ping_update()
    .reg$get_form_fields(2)
  })
  
  # prepare inject object: module functions must get an ID and HTML
  inject_prep <- reactive({
    
    inj <- inject()
    if(is.null(inj))return(NULL)
    
    inj <- lapply(inj, function(x){
        
      if(is.null(x$html) & !is.null(x$ui_module)){
        x$id <- uuid::UUIDgenerate()
        x$html <- x$ui_module(ns(x$id), data = data(), columns = x$columns)
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
  
  output$ui_input_left <- renderUI({

    formSectionModuleUI(session$ns("form_left"), cfg = cfg_left(), .reg = .reg, 
                        data = data(),
                        inject = inject_left())
    
  })
  
  output$ui_input_right <- renderUI({
    
    formSectionModuleUI(session$ns("form_right"), cfg = cfg_right(), .reg = .reg, 
                        data = data(),
                        inject = inject_right())
    
  })
  
  edit_left <- callModule(formSectionModule, "form_left", cfg = cfg_left, .reg = .reg)
  edit_right <- callModule(formSectionModule, "form_right", cfg = cfg_right, .reg = .reg)
  
  
  edits_configured <- reactive({
    
    req(edit_left())
    
    out <- c(lapply(edit_left(), function(x)x()),
      lapply(edit_right(), function(x)x()))

    out
    
  })

  modules_extra <- reactiveVal()
    
  observe({
    
    extra <- inject_prep()
    withmod <- which(!sapply(sapply(extra, "[[", "ui_module"), is.null))
    
    if(length(withmod)){
      
      values <- lapply(seq_along(withmod), function(i){
        
        j <- withmod[i]
        
        lis_call <- c(list(
          module = extra[[j]]$server_module, 
          id = extra[[j]]$id,
          columns = extra[[j]]$columns
        ), extra[[j]]$module_server_pars)
        
        do.call(callModule, lis_call)
      })
      
      modules_extra(values)
    }
    
    
  })
  
  edits_extra <- reactive({
    req(length(modules_extra()))
    lapply(modules_extra(), function(x)x())
  })
  
  
  edits <- reactive({
    
    ext <- edits_extra()
    out <- edits_configured()
    
    if(length(ext)){
      for(i in seq_along(ext)){
        out[names(ext[[i]])] <- ext[[i]]
      }
    }
    out
  })
  
  # output$txt_out <- renderPrint({
  #   edits()
  # })
  
  
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
    out_ping(runif(1))
    callback_cancel()
  })
  
  observeEvent(input$btn_confirm_new_registration, {

    resp <- .reg$write_new_registration(edits(), current_user)
    if(resp){
      toastr_success("Registratie opgeslagen")
    } else {
      toastr_error("Er is een fout opgetreden")
    }
    
    out_ping(runif(1))
    
    callback_confirm()
  })
  
  
  
return(out_ping)
}



#--- Utils
unlist_module_output <- function(data, what){
  if(is.null(data[[what]]) || !is.list(data[[what]])){
    return(data)
  }
  
  c(data[[what]], data[-match(what, names(data))])  
  
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
  
  ui <- softui::simple_page(
    
    softui::box(
      formUI("test")
    )
  )
  
  server <- function(input, output, session) {
    callModule(formModule, "test", .reg = .reg,
               current_user = "devuser",
               inject = reactive(list(
                 list(position = 4, section = 1, 
                        ui_module = testmoduleui,
                        server_module = testmodule, 
                        columns = c("letterkeuze1","letterkeuze2")),
                 list(position = 2, section = 2,
                      html = tags$h2("Dit is een tekst"))
                 
               )))
  }
  
  shinyApp(ui, server)
  
  
  
}














