
# Packages
devtools::load_all()
#library(shintoforms)



# Set - see 1Pass
mcekey <- Sys.getenv("TINYMCE_API_KEY")
options(mce_api_key = mcekey)

source("preload/load_packages.R")
shintoshiny::check_for_version("softui","0.4")
shintoshiny::check_for_version("shintousers","0.4-2")
shintoshiny::check_for_version("shintocatman","0.1-5")

GLOBAL_AUDIT <- TRUE


reactable_click_handler <- function(id = NULL, what = "data", session = shiny::getDefaultReactiveDomain()){
  
  what <- match.arg(what)
  
  id <- session$ns(id)
  fun_ <- glue::glue("function(rowInfo, column) {
              let values = rowInfo.values;
              let ranval = Math.random();
              let vallis = Object.assign(values,ranval);
              Shiny.setInputValue('<<<id>>>', vallis, { priority: 'event' })
              }", .open = "<<<", .close = ">>>")
  htmlwidgets::JS(fun_)
}

# custom module NON-relation
fenomeenSelectUI <- function(id, data = NULL, columns = NULL, ...){
  
  ns <- NS(id)
  
  # misschien niet de beste plek
  cfg <- list("A"= "z", "B"= "x")
  
  val1 <- shintoforms::make_default_value(columns[1], data, default = "")
  val2 <- shintoforms::make_default_value(columns[2], data, default = "")
  
  tagList(
    selectInput(ns("sel_fenomeen"), "Fenomeen", 
                selected = val1,
                choices = c("", sort(names(cfg)))),
    selectInput(ns("sel_sub_fenomeen"), "Sub-fenomeen", choices = val2, selected = val2)
  )
  
}


fenomeenSelect <- function(input, output, session, columns, ...){
  
  cfg <- list("A"= "z", "B"= "x")
  
  observeEvent(input$sel_fenomeen, {
    chc <- cfg[[input$sel_fenomeen]]
    updateSelectInput(session, "sel_sub_fenomeen", 
                      choices = chc, 
                      selected = input$sel_sub_fenomeen)
  })
  
  
  return( reactive(NULL)
  )
  
  
}

## Custom module Relation
 
relationSelectUI <- function(id,  relation_title="", ...){
  
  ns <- NS(id) 
  
  fluid_page(
    sub_box(title = relation_title, collapsible = F,
            fluid_row(
              column(4,
                     selectInput( ns("sel_id1"), "Selecteer", choices=list("Adres1"="AdresId1", "Adres2"="AdresId2", "Adres3"="AdresId3") )
              ),
              column(4,
                     textInput( ns("txt_id1"), "Opmerking")
              ),
              column(4,
                     action_button( ns("btn_id1"), "Voeg toe", status = "success", icon = bsicon("plus-circle"))
              )
            ),
            fluid_row(
              reactableOutput( ns('dt_basket') )
            )
    ) 
    
  )
  
  
}

relationSelectModule <- function(input, output, session ,reg_id, relation_type='',  ...){ 
  
  data_signal <- reactiveVal() 
    
  # inititalise data from new id
  observeEvent(reg_id(),{   
    hoofdadressen <- .reg$get_objects_for_collector(collector_id=reg_id(),
                                                    relation_type = relation_type) 
    data_signal(hoofdadressen)
  })
  
  
  # add data
  observeEvent(input$btn_id1, ignoreInit = T,{   
   
    new_row <-  data.frame(
                id=uuid::UUIDgenerate(),
                collector_id=reg_id(),
                 collector_type=.reg$class_type, 
                 object_id=input$sel_id1,
                 object_type="adres",
                 relation_id=as.character(session$ns("")),
                 relation_type=relation_type,
                 comment=input$txt_id1,
                 status=1,
                 username="",
                 timestamp=as.integer(Sys.time()) )
 
    new_signal_data<- rbind(data_signal(), new_row)
    data_signal(new_signal_data)
  
  })
  output$dt_basket <- renderReactable({
    req(data_signal() > 0)
    reactable(
      data_signal()  %>% 
        select(id, object_id, object_type, relation_type, comment)  %>%
        mutate(delete_row=NA),
      columns = list( 
        id=colDef(show=F),
        object_id=colDef(name='Object'),
        relation_type=colDef(name='Relatie'),
        comment=colDef(name='Opmerking'),
        object_type=colDef(show=F),
        delete_row = colDef(
          name = "",
          sortable = FALSE,
          cell = function() action_button( session$ns("btn_id3"), "", status = "danger", icon = bsicon("trash3-fill")) 
        ) 
      ),
      onClick = reactable_click_handler("delete_row") 
    )
  })
  
  observeEvent(input$delete_row,{   
    data_signal(filter(data_signal(), id !=  input$delete_row$id) )
  }) 
  return(data_signal)
}



# Database connection
.reg <- formClass$new(sqlite = "data/registraties.sqlite",
                              def_table = "formulier_velden",
                              audit=GLOBAL_AUDIT,
                              class_type='registration',
                              audit_table = "registraties_audit",
                              def_columns = list(
                                id_form = "id_formulierveld",
                                column_field = "kolomnaam_veld",
                                label_field = "label_veld",
                                type_field = "type_veld",
                                order_field = "volgorde_veld",
                                form_section = "formulier_sectie",
                                visible = "zichtbaar",
                                date_deleted = "datum_uitgeschakeld",
                                options = "opties",
                                order_options = "volgorde_opties",
                                colors = "kleuren",
                                removable = "kan_worden_verwijderd"
                                # make_filter = "make_filter",
                                # tooltip = "tooltip"
                              ),
                              data_table = "registraties",
                              data_columns = list(
                                id = "id_registratie",
                                name = "naam_registratie",
                                time_created = "invoerdatum",
                                time_modified = "wijzigdatum",
                                user = "user_id",
                                status = "status"
                              ), 
                      relation_table = "object_relations", 
                      relation_columns = list(
                        id = "id",
                        collector_id = "collector_id",
                        collector_type = "collector_type",
                        object_id = "object_id",
                        object_type = "object_type",
                        relation_id = "relation_id",
                        relation_type = "relation_type",   
                        comment = "comment", 
                        status = "status",
                        username = "username", 
                        timestamp = "timestamp"
                      ), 
                      relation_audit_table = "object_relations_audit",
                      inject = list(
                        list(position = 1,
                             section = 1, 
                             ui_module = fenomeenSelectUI,
                             server_module = fenomeenSelect,
                             columns = c("fenomeen","subfenomeen") 
                        ),
                        list(position = 1,
                             section = 2,
                             relation = TRUE,
                             ui_module = relationSelectUI,
                             server_module = relationSelectModule,
                             module_ui_pars=list("relation_title" = "Hoofdadres"),
                             module_server_pars=list("relation_type" = "hoofdadres")
                        ),
                        list(position = 2,
                             section = 2,
                             relation = TRUE,
                             ui_module = relationSelectUI,
                             server_module = relationSelectModule,
                             module_ui_pars=list("relation_title" = "Nevenadressen"),
                             module_server_pars=list("relation_type" = "nevenadres")
                        )
                        ))
 


#dbExecute(.reg$con, "alter table registraties add column serial_id INTEGER AUTOINCREMENT NOT NULL")


# End.
onStop(function() {
  .reg$close()
})




