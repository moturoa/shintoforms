jsonFormSetupUI <- function(id, class = "", icon = NULL, label = "Edit values"){
  
  ns <- NS(id)
  actionButton(ns("btn"), label, icon = icon, class = class)
  
}


jsonFormSetupModule <- function(input, output, session, data = reactive(NULL), side_column = reactive(NULL), order_column = reactive(NULL), 
                                id_column = reactive(NULL), label_column = reactive(NULL), callback = function(data){}){
  
  data_ordered_left <- reactive({
    
    data() %>%
      filter(!!sym(side_column()) == "links") %>%
      arrange(!!sym(order_column()))
  })
  
  labels_left <- reactive({
    
    .reg$make_choices(values_from = id_column(),
                      names_from = label_column(),
                      data = data_ordered_left(),
                      sort = FALSE)
    
  })
  
  
  
  observeEvent(input$btn, {
    browser()
    showModal(
      modalDialog(title = "Drag to change the order and the side of the form", size = "l",
                  sortable::bucket_list(header = NULL,
                                        group_name = "bucket_list_form", 
                                        orientation = "horizontal",
                                        add_rank_list(
                                          text = "Linkerkolom formulier",
                                          labels = NULL,
                                          input_id = "form_kolom_links"
                                        ),
                                        add_rank_list(
                                          text = "Rechterkolom formulier",
                                          labels = NULL,
                                          input_id = "form_kolom_rechts"
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
  
  observeEvent(input$btn_confirm, {
    #output_vector(to_json(as.list(order_int())))
    #callback()
    removeModal()
  })
}