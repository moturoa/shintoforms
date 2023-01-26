

nestedSelectModuleUI <- function(id, label, data, columns, value, options, width){
  
  ns <- NS(id)
  
  main_chc <- unlist(options$key[[1]]) 
  main_chc <- setNames(names(main_chc),unlist(main_chc))
  
  sec_chc <- options$value[[1]][[main_chc[value]]]
  if(length(sec_chc) == 0)sec_chc <- NULL # virtualSelectInput crasht met choices=list()
  sec_sel <- data[[names(options$value)]]
  
  tagList(
    
    # tags$h5("Label"),
    # textInput(ns("txt_column_2_label"), "Label voor sub-keuze kolom", value = options$label),
    # 
    softui::virtual_select_input(ns("sel_column_1"), label, choices = main_chc, 
                                 width = width,
                                 selected = main_chc[value], autoSelectFirstOption = FALSE),
    softui::virtual_select_input(ns("sel_column_2"), 
                                 options$label, 
                                 width = width,
                                 choices = sec_chc, 
                                 selected = sec_sel,
                                 autoSelectFirstOption = FALSE)
  )
  
  
}



nestedSelectModule <- function(input, output, session, cfg, 
                               data = reactive(NULL),
                               trigger = reactive(NULL)){
  
  opts <- reactive({
     jsonlite::fromJSON(cfg$options)
  })
  
  observe({

      #trigger()
      req(input$sel_column_1)
      #cur_val <- input$sel_column_2

      colname <- names(opts()$value)
      chc <- opts()$value[[1]][[input$sel_column_1]]
      if(length(chc) == 0)chc <- NULL

      sel_val <- data()[[colname]]
      
      # print(paste("chc:", paste(chc, collapse= ", ")))
      # print(paste("sel:", sel_val))
      
      updateVirtualSelect("sel_column_2", choices = chc, selected = sel_val)
      
  })
  
  
  reactive(
    list(
      value_1 = input$sel_column_1,
      value_2 = input$sel_column_2
    ) %>%
      setNames(c(names(opts()$key), names(opts()$value)))
  )
  
}
