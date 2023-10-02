
library(shiny)

library(softui)



subChoiceEditorRowUI <- function(id, label = NULL, values = NULL){
  
  ns <- NS(id)
  
  softui::fluid_row(
    column(5,
           
           textInput(ns("txt_label"), label = NULL, value = label)
           
           ),
    column(7,
           shinyWidgets::virtualSelectInput(ns("sel_value"),
                                            NULL,
                                            choices = values,
                                            multiple = TRUE,
                                            search = FALSE,
                                            showValueAsTags = TRUE,
                                            allowNewOption = TRUE)
           # selectizeInput(ns("sel_value"),NULL,
           #             choices = values, 
           #             selected = values,
           #             multiple = TRUE, options = list(create = TRUE))
          # )
    )
  )
  
  
  
  
}


subChoiceEditorRow <- function(input, output, session){
  
  reactive(list(
    key = input$txt_label,
    value = input$sel_value))
  
}



subChoiceEditorUI <- function(id){
  ns <- NS(id)
  
  tagList(
    tags$div(id = ns("div_placeholder")),
    softui::action_button(ns("btn_add_row"), "Toevoegen", icon = bsicon("plus-lg"), status = "info")
    #actionButton(ns("btnbrowse"),"browser()")
  )
  
  
  
}

subChoiceEditor <- function(input, output, session, data = reactive(NULL), json = TRUE){
  
  
  clear_reactivevalues <- function(x){
    
    stopifnot(is.reactivevalues(x))
    
    nms <- isolate(names(x))
    for(nm in nms){
      x[[nm]] <- NULL
    }
    
    x
  } 
  
  #observeEvent(input$btnbrowse, browser())
  
  data_input <- reactive({
    out <- data()
    if(json)out <- jsonlite::fromJSON(out)
    out
  })
  
  data_current <- reactiveValues()
  
  cur_ids <- reactiveVal()

  observeEvent(data_input(), {
    
    data <- data_input()
    
    removeUI(selector = paste0("#",session$ns("div_placeholder"), " > div"), multiple = TRUE, immediate = TRUE)
    
    column_choices <- names(data$key[[1]])
    data_current <- clear_reactivevalues(data_current)
    
    lapply(column_choices, function(el){
    
      
      id <- paste0("mod_",el)
      lab <- data$key[[1]][[el]]
      prev_choices <- data$value[[el]]
      ui <- subChoiceEditorRowUI(session$ns(id),
                                 label = lab,
                              values = prev_choices)  
      insertUI(selector = paste0("#",session$ns("div_placeholder")),
               ui = ui, where = "beforeEnd")
      
      data_current[[id]] <- callModule(subChoiceEditorRow, paste0("mod_",el))
      cur_ids(c(cur_ids(),id))
    })
    
  })
  
  observeEvent(input$btn_add_row, {
    
    ids <- cur_ids()
    new_id <- paste0("mod_",length(ids)+1)
    if(new_id %in% ids)stop("iets niet lekker met IDs")
    
    ui <- subChoiceEditorRowUI(session$ns(new_id),
                               label = "", values = NULL)  
    insertUI(selector = paste0("#",session$ns("div_placeholder")),
             ui = ui, where = "beforeEnd")
    
    data_current[[new_id]] <- callModule(subChoiceEditorRow, new_id)
    cur_ids(c(cur_ids(),new_id))
    
  })
  
  
  data_out <- reactive({
    x <- lapply(reactiveValuesToList(data_current), function(x)x())  
    column_name <- names(data_input()$key)
    
    slots <- stringr::str_extract(names(x), "[0-9]+")
    keys <- lapply(x, "[[", "key")
    vals <- lapply(x, "[[", "value")
    ii <- order(as.integer(slots))
    out <- list(key = setNames(list(setNames(keys,slots)[ii]),column_name),
         value = setNames(vals,slots)[ii])
    
    out$value <- lapply(out$value, function(el){
      if(is.null(el)){
        return(list())
      } else {
        el
      }
    })
    out
    
  })
  
data_out
}


ui <- softui::simple_page(

  #subChoiceEditorUI("test"),
  #tags$hr(),
  verbatimTextOutput("txtout"),
  
  modal_action_button("btngo", "Data bewerken", modalId = "modal_data_test"),
  
  softui::ui_modal(id = "modal_data_test", ns = function(x)x,
                   size = "l",
                   confirm_txt = "Opslaan",
                   id_confirm = "btn_confirm_edits",
                   subChoiceEditorUI("test"))
  

    
)


server <- function(input, output, session) {
  
  val <- "{\"key\":{\"type_huis\":{\"1\":\"rijtjeshuis\",\"2\":\"hoekhuis\",\"3\":\"vrijstaand huis\",\"4\":\"appartement\",\"5\":\"kamer in huis\",\"6\":\"boomhut\",\"7\":\"s\",\"8\":\"enorme torenflat\",\"9\":\"csdsdsv\",\"10\":\"hier nog een soort woning\",\"11\":\"hhhhhhhhhhhhhhhhh\",\"12\":\"aaaaaaaaaaaaa\"}},\"value\":{\"1\":{},\"2\":{},\"3\":{},\"4\":{},\"5\":{},\"6\":{},\"7\":{},\"8\":{},\"9\":{},\"10\":{},\"11\":{},\"12\":{}}}"

  data_in <- reactiveVal({
    jsonlite::fromJSON(val)
  })  
  
  out <- callModule(subChoiceEditor, "test", data = data_in, json = FALSE)

  observeEvent(input$btn_confirm_edits, {
    
    data_in(out())
    
  })
  
    
}

options(shiny.fullstacktrace = TRUE)
shinyApp(ui, server)


