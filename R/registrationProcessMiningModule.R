#' Shiny module for process mining visualization and analysis of status development of registration
#' @param id Shiny input id
#' @export
#' @rdname registrationProcessMiningModule

registrationProcessMiningUI <- function(id){
  
  ns <- NS(id)
  
  softui::fluid_row(
    column(12,
           uiOutput(ns("trace_registration_id"))
    )
  )
  
  
}

#' @export
#' @param .reg formClass for which the process mining is being done (call to formClass does not have event_data set as NULL)
#' @param registration_id registration about we want to see event traces and process mining analyses
#' @rdname registrationProcessMiningModule
#' @importFrom DiagrammeR grVizOutput
registrationProcessMiningModule <- function(input, output, session, .reg, registration_id = reactive(NULL)){
  
  event_data <- reactive({
    .reg$get_eventdata_registration(registration_id())
  })
  
  output$trace_registration_id <- renderUI({
    
    event_data <- event_data()
    
    softui::fluid_row(
      column(12,
             softui::box(title = glue("Proces van {registration_id()}"), width = 12,
                         softui::fluid_row(
                           column(6,
                                  tags$h3("Stappenspoor"),
                                  plotOutput(session$ns("trace_explorer_plot"))
                           ),
                           column(6,
                                  tags$h3("Stappenkaart"),
                                  DiagrammeR::grVizOutput(session$ns("process_map_plot"))
                                  
                           )
                         ),
                         softui::fluid_row(
                           column(6,
                                  tags$h3("Actoren"),
                                  DiagrammeR::grVizOutput(session$ns("resource_map_plot"))
                                  
                           ),
                           column(6,
                                  tags$h3("Overdracht-matrix"),
                                  plotOutput(session$ns("process_matrix_plot"))
                                  # TODO: RESOURCE MATRIX?
                           )
                         )
             )
      )
    )
    
  })
  
  output$trace_explorer_plot <- renderPlot({
    
    event_data() %>% processmapR::trace_explorer() %>% plot()
    
  })
  
  output$process_map_plot <- DiagrammeR::renderGrViz({
    
    event_data() %>% processmapR::process_map(render = TRUE)
    
  })
  
  
  output$process_matrix_plot <- renderPlot({
    
    event_data() %>% processmapR::precedence_matrix() %>% plot()
    
  })
  
  output$resource_map_plot <- DiagrammeR::renderGrViz({
    
    event_data() %>% processmapR::resource_map()
    
  })
  
  
}