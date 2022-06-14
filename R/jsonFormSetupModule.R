jsonFormSetupUI <- function(id, class = "", icon = NULL, label = "Edit values"){
  
  ns <- NS(id)
  actionButton(ns("btn"), label, icon = icon, class = class)
  
}



#' @rdname jsonFormSetup
#' @importFrom 
#' @export
jsonFormSetupModule <- function(input, output, session, 
                                data = reactive(NULL), 
                                .reg = NULL,
                                side_column = reactive(NULL), 
                                order_column = reactive(NULL), 
                                id_column = reactive(NULL), 
                                label_column = reactive(NULL), 
                                callback = function(data){}){
  
  ns <- session$ns
  
  if(is.null(.reg)){
    stop("Provide .reg argument, e.g. .reg <- registrationClass$new(sqlite = 'data/registraties.sqlite')")
  }
  
  data_ordered_left <- reactive({
    
    data() %>%
      filter(!!sym(side_column()) == "links") %>%
      arrange(!!sym(order_column()))
  })
  
  labels_left <- reactive({
    
    .reg$make_choices(values_from = label_column(),
                      names_from = id_column(),
                      data = data_ordered_left(),
                      sort = FALSE)
    
  })
  
  data_ordered_right <- reactive({
    
    data() %>%
      filter(!!sym(side_column()) == "rechts") %>%
      arrange(!!sym(order_column()))
  })
  
  labels_right <- reactive({
    
    .reg$make_choices(values_from = label_column(),
                      names_from = id_column(),
                      data = data_ordered_right(),
                      sort = FALSE)
    
  })
  
  
  observeEvent(input$btn, {
    
    showModal(
      modalDialog(title = "Drag to change the order and the side of the form", size = "l",
                  sortable::bucket_list(header = NULL,
                                        group_name = ns("bucket_list_form"), 
                                        orientation = "horizontal",
                                        add_rank_list(
                                          text = "Linkerkolom formulier",
                                          labels = labels_left(),
                                          input_id = ns("form_kolom_links")
                                        ),
                                        add_rank_list(
                                          text = "Rechterkolom formulier",
                                          labels = labels_right(),
                                          input_id = ns("form_kolom_rechts")
                                        )
                  ),
                  footer = tagList(actionButton(session$ns("btn_confirm"), 
                                                "Opslaan", 
                                                icon = icon("check"), 
                                                class = "btn-success"), 
                                   htmltools::tagAppendAttributes(modalButton("Annuleren"), 
                                                                  class = "btn-danger"))
      )
    )
  })
  
  output_vector <- reactiveVal()
  
  observeEvent(input$btn_confirm, {
    
    id <- input$form_kolom_links
    side <- c(rep("links", length(input$form_kolom_links)))
    order <- c(1:length(input$form_kolom_links))
    data_links <- data.frame(id, side, order)
    
    id <- input$form_kolom_rechts
    side <- c(rep("rechts", length(input$form_kolom_rechts)))
    order <- c(1:length(input$form_kolom_rechts))
    data_rechts <- data.frame(id, side, order)
    
    data_res <- rbind(data_links, data_rechts)
    
    output_vector(data_res)
    
    callback()
    removeModal()
  })
  return(output_vector)
}