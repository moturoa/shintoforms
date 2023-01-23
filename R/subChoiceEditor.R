
#---> naar shintocatman

subChoiceEditorRowUI <- function(id, label = NULL, values = NULL){
  
  ns <- NS(id)
  
  if(length(values) == 0)values <- NULL
  
  tags$div(id = id,
    softui::fluid_row(
      column(5,
             
             textInput(ns("txt_label"), label = NULL, value = label)
             
      ),
      column(7,
             shinyWidgets::virtualSelectInput(ns("sel_value"),
                                              NULL,
                                              choices = values,
                                              selected = values,
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
    
    softui::fluid_row(
      column(6,
             softui::action_button(ns("btn_add_row"), "Toevoegen", icon = bsicon("plus-lg"), status = "info"),
             ),
      column(6,
             softui::action_button(ns("btn_remove_row"), "Laatste verwijderen", icon = bsicon("dash-lg"), status = "warning")
             )
    )
    
    
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
  
  observeEvent(input$btn_remove_row, {
    ids <- cur_ids()
    last_id <- ids[length(ids)]
    data_current[[last_id]] <- NULL
    cur_ids(setdiff(cur_ids(),last_id))
    
    box_id <- paste0("#", session$ns(last_id))
    
    removeUI(box_id, immediate = TRUE)
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
    
    data <- lapply(reactiveValuesToList(data_current), function(x){
      if(is.reactive(x)){
        x()  
      } else {
        NULL # verwijderde velden
      }
      
    })  
    
    i_is_null <- sapply(data, is.null)
    data <- data[!i_is_null]
    
    column_name <- names(data_input()$key)
    
    slots <- stringr::str_extract(names(data), "[0-9]+")
    keys <- lapply(data, "[[", "key")
    vals <- lapply(data, "[[", "value")
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
