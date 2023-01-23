

nestedSelectModuleUI <- function(id, label, data, columns, value, options, width){
  
  ns <- NS(id)
  
  main_chc <- unlist(options$key[[1]])
  
  main_chc <- setNames(names(main_chc),unlist(main_chc))
  
  tagList(
    softui::virtual_select_input(ns("sel_column_1"), label, choices = main_chc, 
                                 width = width,
                                 selected = main_chc[value], autoSelectFirstOption = FALSE),
    softui::virtual_select_input(ns("sel_column_2"), "Sub-keuze", 
                                 width = width,
                                 choices = NULL, autoSelectFirstOption = FALSE)
  )
  
  
}



nestedSelectModule <- function(input, output, session, cfg, data){
  
  opts <- reactive({
     jsonlite::fromJSON(cfg$options)
  })
  
  observeEvent(input$sel_column_1, {

      colname <- names(opts()$value)
      chc <- opts()$value[[1]][[input$sel_column_1]]
      sel_val <- data[[colname]]
      
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
