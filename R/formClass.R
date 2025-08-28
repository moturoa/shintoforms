#' R6 Class voor Registratie formulier
#' @importFrom R6 R6Class
#' @importFrom shintodb connect
#' @importFrom shintodb databaseClass
#' @importFrom DBI dbGetQuery
#' @importFrom janitor make_clean_names
#' @importFrom glue glue
#' @importFrom cli cli_alert_success cli_alert_danger
#' @importFrom uuid UUIDgenerate
#' @export
formClass <- R6::R6Class(
  inherit = shintodb::databaseClass,
  lock_objects = FALSE,
  
  public = list(
    
    #' @description Make new form object
    #' @param config_file Path to DB config
    #' @param what Entry in config for DB connection
    #' @param schema DB schema
    #' @param class_type Name for the class of the objects to be managed with shintoforms
    #' @param db_connection Optional, an existing DB connection
    #' @param audit If TRUE, writes edits to audit log
    #' @param audit_table Name of the table for the audit logs
    #' @param def_table Table in DB (in schema) that holds form config
    #' @param def_columns List with internal names of the config fields, and names of the corresponding columns in the table
    #' @param data_table Name of the table to hold the data saved by shintoforms
    #' @param data_columns List with internal names of required data columns, and corresponding names in the data table
    #' @param relation_table Name of the 'relations' table
    #' @param relation_columns List with internal names of the relations table and corresponding names in the actual postgres table
    #' @param relation_audit_table If TRUE, audits the relation table
    #' @param inject Custom fields to be injected; see Apollo for examples
    #' @param pool If TRUE, connects with dbPool
    #' @param connect_on_init If TRUE (default), connects to DB on initialization. Otherwise, call `$connect_to_database` at a later stage.
    #' @param sqlite If path to an SQLite file, uses SQLite.
    #' @param shintousers_object Optional, an instance of shintoUser (".user") passed during init.
    #' @param default_color Default color
    initialize = function(config_file = "conf/config.yml", 
                          what, 
                          schema = NULL,
                          class_type="",
                          db_connection = NULL,
                          audit = FALSE,
                          audit_table = "registrations_audit",
                          def_table = "formulier_velden",
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
                            option_active = "optie_actief",
                            removable = "kan_worden_verwijderd",
                          ),
                          data_table = NULL,
                          data_columns = list(
                            id = "id_registratie",
                            serial_id = "serial_id",
                            name = "naam_registratie",
                            casenr = "zaak_nummer",
                            time_created = "invoerdatum",
                            time_modified = "wijzigdatum",
                            user = "user_id",
                            deleted = "deleted",
                            status = "status",
                            priority = "priority"
                          ), 
                          casenr_code = NULL,
                          relation_table = NULL,
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
                            timestamp = "timestamp",
                            verwijderd = "verwijderd"
                          ), 
                          relation_audit_table = NULL, #"object_relations_audit",
                          inject = NULL,
                          pool = TRUE,
                          sqlite = NULL,
                          connect_on_init = TRUE,
                          
                          shintousers_object = NULL,
                          
                          default_color = "#3333cc"){
      
      self$class_type <- class_type
      
      self$audit <- audit
      self$audit_table <- audit_table
      
      self$inject <- inject
      
      
      self$relation_table <- relation_table
      
      self$relation_columns <- relation_columns
      self$relation_audit_table <- relation_audit_table
      
      self$default_color <- default_color
      
      self$relation_table <- relation_table
      self$relation_columns <- relation_columns
      self$relation_audit_table <- relation_audit_table
      
      super$initialize(what = what, config_file = config_file, schema = schema,
                       pool = pool, sqlite = sqlite, 
                       db_connection = db_connection,
                       connect_on_init = connect_on_init)
      
      
      self$def_table <- def_table
      self$def <- def_columns
      self$data_table <- data_table
      self$data_columns <- data_columns
      
      self$casenr_code <- casenr_code
      
      
      # Integrity checks if connected to DB
      if(connect_on_init){
        # check if audit table is present 
        if(is.null(self$schema)){
          if(self$audit & !DBI::dbExistsTable(self$con, self$audit_table)){ 
            stop(glue::glue("Audit feature is on but there is no table named {self$audit_table}")) 
          }
        } else {
          if(self$audit & !DBI::dbExistsTable(self$con, DBI::Id(schema = self$schema, table = self$audit_table))){
            #if(self$audit & !DBI::dbExistsTable(self$con, self$audit_table, self$schema)){ 
            stop(glue::glue("Audit feature is on but there is no table named {self$audit_table} in schema {self$schema}")) 
          }
        }
        
        
        if(!is.null(self$data_table)){
          
          # Name of deleted column may be missing ...
          if(is.null(self$data_columns$deleted)){
            self$data_columns$deleted <- "deleted"
          }
          
          # check new 'deleted' field
          datacols <- self$table_columns(self$data_table)
          
          
          if(!self$data_columns$deleted %in% datacols){
            stop(paste("Must have 'deleted' column in registrations data (`alter table <<table>> add column deleted integer default 0`),",
                       " or set data_columns$deleted to the name of the boolean 'deleted' flag column"))
          }  
        }
        # Check
        defcols <- self$table_columns(self$def_table)
        di1 <- unlist(self$def) %in% defcols
        if(any(!di1)){
          stop(paste("Columns in def_columns not found:", paste(defcols[!di1], collapse=",")))
        }
        
      }
      
      # 'schema' string for query building
      self$store_schema_str()
      
    },
    
    
    #--- logger
    #' @description Log something
    log =  function(...){
      futile.logger::flog.info(...)
    },
    
    #' @description (Re-)write schema_str 
    store_schema_str = function(){
      
      self$schema_str <- ifelse(is.null(self$schema), "", paste0(self$schema,"."))
      
    },
    
    #----- Generic database methods
    
    #' @description Multi-replace value where (sql update wrapper)
    #' @param table Table name (no schema)
    #' @param replace_list Named list of values to replace; name of list element is column name
    #' @param col_compare Which column to compare (e.g. 'id')
    #' @param val_compare Value to compare (e.g. 101, then id = 101 will be updated according to replace_list)
    #' @param query_only If TRUE, returns only the qyuery
    #' @param quiet If TRUE, prints nothing
    #' @param username If passed; updates 'username' in table for audit purposes
    replace_value_where_multi = function(table, replace_list, col_compare, val_compare,
                                         query_only = FALSE, quiet = FALSE,  username=""){
      
      # nodig voor wanneer men op 'opslaan' klikt zonder wijziging
      if(length(replace_list) > 0){
        
        # interpolate KOLOM = ?value  voor alle records in de replace_list behalve bool type
        set_values <- lapply(names(replace_list),  function(x){
          
          replace_val <- replace_list[[x]]
          
          if(is.null(replace_val)){
            return("")
          }
          
          # DB method gebruikt sqlInterpolate zodat integers/chars correct worden vervangen.
          # maar werkt niet voor boolean vandaar deze eerste stap.
          # de stap !is.na(new_val) is nodig omdat is.logical(NA) == TRUE!
          if(is.logical(replace_val) & !is.na(replace_val)){
            query <- glue("{x} = {ifelse(replace_val, 'true', 'false')}") %>% as.character() 
          } else { 
            sub_query <- glue("{x} = ?val_replace") %>% as.character()
            query <- sqlInterpolate(DBI::ANSI(), 
                                    sub_query, 
                                    val_replace = replace_val )
          }
          return(query)
        })
        
        update_str <- paste(set_values[set_values != ""],  collapse = ", ")
        
        if(!is.null(self$schema)){ #? geen else?
          query <- glue("update {self$schema_str}{table} set {update_str}, {self$data_columns$user} = '{username}', {self$data_columns$time_modified} = NOW()::timestamp where ",
                        "{col_compare} = ?val_compare") %>% as.character() 
          
          if(self$audit){
            
            data_row_query_1 <- glue("select * from {self$schema_str}{table} where {col_compare}=?val_compare;")
            data_row_query <- sqlInterpolate(DBI::ANSI(), data_row_query_1,  val_compare = val_compare) 
            data_row <- self$get_query(data_row_query)
            
            self$append_data(self$audit_table, data_row)
            
          }
        }
        
        query <- sqlInterpolate(DBI::ANSI(), 
                                query, 
                                val_compare = val_compare)
        
        if(query_only)return(query)
        
        if(!quiet){
          self$log(query)
        }
        
        self$execute_query(query)   
      }
    }, 
    
    #----- Form registration methods
    #' @description Get a row from the form definition by the form id.
    #' @param id_form Id of form field
    get_by_id = function(id_form){
      self$read_table(self$def_table, lazy = TRUE) %>%
        dplyr::filter(!!sym(self$def[["id_form"]]) == !!id_form) %>%
        collect
      
    },
    
    #' @description Get column name for a form field by ID
    #' @param id_form Id of form field
    column_name_from_id = function(id_form){
      row <- self$get_by_id(id_form)
      row[[self$def[["column_field"]]]]
    },
    
    #' @description Get form field ID for a column name
    #' @param formfield_name Formfield name
    id_from_column_name = function(formfield_name){
      out <- self$read_table(self$def_table, lazy = TRUE) %>%
        dplyr::filter(!!sym(self$def[["column_field"]]) == !!formfield_name) %>%
        collect
      
      out[[self$def[["id_form"]]]]
    },
    
    #' @description Get all fields of a certain type
    #' @param field_type Type of field, e.g. 'singleselect'
    get_fields_by_type = function(field_type){
      
      self$read_table(self$def_table, lazy = TRUE) %>%
        dplyr::filter(!!sym(self$def[["type_field"]]) %in% !!field_type, 
                      !!sym(self$def[["visible"]])) %>% 
        collect
      
    },
    
    
    #' @description Get (recoded) choices for a select field
    #' @param column_field Name of field (i.e. column name)
    get_field_choices = function(column_field){
      
      self$read_definition(lazy = TRUE) %>%
        filter(!!sym(self$def$column_field) == !!column_field) %>%
        pull(!!sym(self$def$options)) %>%
        self$from_json()
      
    },
    
    
    #' @description Check if we have a field with a name
    form_has_field = function(column_field){
      
      out <- self$read_definition(lazy = TRUE) %>%
        filter(!!sym(self$def$column_field) == !!column_field) %>%
        collect
      
      nrow(out) > 0
      
    },
    
    #' @description Get order of the choices for a field
    #' @details Safe implementation; if the order length does not match the options,
    #' or it is otherwise corrupted, a simple vector of length(n choices) is returned.
    #' @param column_field Name of field (i.e. column name)
    get_field_choices_in_order = function(column_field){
      
      data <- self$read_definition(lazy = TRUE) %>%
        filter(!!sym(self$def$column_field) == !!column_field) %>%
        select(all_of(c(self$def$order_options, self$def$options))) %>%
        collect
      
      chc <- unname(unlist(self$from_json(data[[self$def$options]])))
      ord <- self$from_json(data[[self$def$order_options]]) 
      
      if(length(ord) == length(chc)){
        return(chc[ord])
      } else {
        cli::cli_alert_danger("Order options for field {column_field} are corrupted; check!")
        return(chc)
      }
      
      
    },
    
    #' @description Get the label for a field
    #' @param column_field Name of field (i.e. column name)
    get_label_field = function(column_field){
      self$read_definition(lazy = TRUE) %>%
        filter(!!sym(self$def$column_field) == !!column_field) %>%
        select(!!sym(self$def$label_field)) %>%
        pull(!!sym(self$def$label_field))
      
    },
    
    #' @description Make choices (for selectInput) based on values and names
    #' @param values_from Name of column to take values from
    #' @param names_from Name of column to take names from
    #' @param data Dataframe
    #' @param sort If TRUE, sorts choices alphabetically
    make_choices = function(values_from, names_from = values_from, data = NULL, sort = TRUE){
      
      data <- data %>%
        distinct(!!sym(values_from), !!sym(names_from))
      
      out <- data[[values_from]] %>% 
        setNames(data[[names_from]])
      
      # Sorteer op labels, niet op waardes
      if(sort){
        out <- out[order(names(out))]
      }
      
      return(out)
      
    },
    
    #' @description Unpack a JSON field to make a named vector
    #' @param x A JSON string
    choices_from_json = function(x){
      
      val <- self$from_json(x)
      nms <- unlist(unname(val))
      
      out <- setNames(names(val), nms)
      
      out2 <- suppressWarnings({
        setNames(as.integer(out), nms)
      })
      
      if(!any(is.na(out2))){
        out <- out2
      }
      
      # remove empty choices
      out <- out[val != ""]
      
      out
    },
    
    #' @description Get distinct values of a column in the registrations table
    #' @param column_name Name of column in data_table
    distinct_registration_field = function(column_name){
      
      self$read_table(self$data_table, lazy = TRUE) %>%
        distinct(!!sym(column_name)) %>%
        collect %>%
        pull(!!sym(column_name))
      
    },
    
    
    
    #'@description Rename database table to correct internal column names
    #'@param data Dataframe
    rename_definition_table = function(data, from_where = ""){
      # Rename to standard colnames
      key <- data.frame(
        old = unname(unlist(self$def)),
        new = names(self$def)
      )
      
      nm_not_def <- setdiff(names(data), key$old)
      
      if(length(nm_not_def) > 0){
        msg <- glue::glue("shintoforms (from ${from_where}): names in data not in definition list: {paste(nm_not_def, collapse=', ')}")
        cli::cli_alert_warning(msg)
      }
      
      dplyr::rename_with(data, .fn = function(x){
        nw <- key$new[match(x, key$old)]
        nw[is.na(nw)] <- x[is.na(nw)]
        nw
      })
      
    },
    
    #' @description Filter fields by visibility (or not)
    #' @param data Dataframe
    #' @param visibility TRUE or FALSE. When NULL, returns all data.
    filter_by_visibility = function(data, visibility = TRUE){
      
      vis <- self$def[["visible"]]
      
      if(is.null(vis)){
        return(data)
      } else {
        dplyr::filter(data, !!sym(vis) == !!visibility)
      }
      
    },
    
    
    
    #' @description Get all non-deleted form input fields
    #' @param zichtbaarheid Visibility
    get_input_fields = function(zichtbaarheid = TRUE){
      
      out <- self$read_table(self$def_table, lazy = TRUE)
      
      if(!is.null(self$def[["visible"]])){
        out <- out %>%
          dplyr::filter(!!sym(self$def[["visible"]]) == !!zichtbaarheid)
      }
      
      out <- collect(out)
      
      self$rename_definition_table(out, from_where = "get_input_fields")
      
    },
    
    #' @description Get form fields for left or right column of the form
    #' @param form_section Either 1 (left) or 2 (right)
    get_form_fields = function(form_section){
      
      qu <- glue::glue("SELECT * FROM {self$schema_str}{self$def_table} WHERE {self$def$form_section} = {form_section} AND {self$def$visible} = TRUE")
      
      out <- DBI::dbGetQuery(self$con, qu)
      out <- out[order(out[[self$def$order_field]]),]
      
      self$rename_definition_table(out, from_where = "get_form_fields")
      
    },
    
    # Alleen zichtbare velden hoeven een volgorde nummer te hebben en moeten worden meegenomen.
    #' @description Get next available form order number
    #' @param form_section Either 1 (left) or 2 (right)
    get_next_formorder_number = function(form_section){
      
      qu <- glue::glue("SELECT COUNT(DISTINCT {self$def$id_form}) FROM {self$schema_str}{self$def_table} WHERE {self$def$form_section} = {form_section} AND {self$def$visible} = TRUE")
      
      count <- DBI::dbGetQuery(self$con, qu)
      return(count[[1]]+1)
      
    },
    
    #' @description Adds an input field to a form
    #' @param label_field Label for the field
    #' @param type_field Type of field (options : TODO)
    #' @param form_section Either 1 (left) or 2 (right)
    #' @param column_name Optional; name of column in output data (otherwise made from label_field)
    #' @param column_2_name Only used for nested select
    add_input_field_to_form = function(label_field, 
                                       type_field, 
                                       form_section = NULL,
                                       column_name = NULL, 
                                       column_2_name = NULL
    ){
      
      
      # Make an ID for the field. Integer or UUID depending on column type.
      id_column <- self$def$id_form
      idcoltype <- self$table_info(self$def_table) %>%
        filter(column_name == !!id_column) %>% pull(data_type)
      
      if(idcoltype == "integer"){
        # do nothing; assume id is a serial
        add_id <- FALSE
      } else {
        add_id <- TRUE
        id <- uuid::UUIDgenerate()  
      }
      
      # Sanitize column names
      if(is.null(column_name)){
        column_name <- janitor::make_clean_names(tolower(label_field), parsing_option = 1)
      }
      
      if(!self$check_uniqueness_column_name(column_name))return(-1)
      if(!is.null(column_2_name) && !self$check_uniqueness_column_name(column_2_name))return(-1)
      
      if(!is.null(form_section) && length(form_section) > 0){
        field_order_nr <- self$get_next_formorder_number(form_section)
      } else {
        field_order_nr <- 0
        form_section <- 0
      }
      
      
      if(type_field == "boolean"){
        choice_values <- '{"1":"Ja","2":"Nee"}'
      } else {
        choice_values <- "[]"
      }
      
      if(type_field == "nestedselect"){
        if(is.null(column_2_name)){
          message("column_2_name must be provided for type field: nestedselect")
          return(-1)
        }
        
        choice_values <- list(key = setNames(list(NULL),column_name),
                              value = setNames(list(NULL),janitor::make_clean_names(tolower(column_2_name), parsing_option = 1)),
                              label = column_2_name) %>% 
          self$to_json()
      }
      
      data <- data.frame(
        column_field = column_name,
        label_field = label_field,
        type_field = type_field,
        order_field = field_order_nr,
        form_section = form_section,
        visible = TRUE,
        options = as.character(choice_values),
        order_options = "[]",
        colors = '[]',
        option_active = '[]',
        removable = TRUE
      )
      
      if(add_id){
        data$id_form <- id
      }
      
      # ugly rename because robust
      m_i <- match(names(data), names(unlist(self$def)))
      data <- data[,!is.na(m_i)]
      m_i <- m_i[!is.na(m_i)]
      names(data) <- unname(unlist(self$def))[m_i]
      
      
      self$append_data(self$def_table, data)
      
      if(type_field == "boolean"){
        self$amend_options_order(id, opties)
        self$amend_options_colors(id, opties)
      }
      
      return(0)
      
      
    },
    
    #' @description Check if a column name can be used (returns TRUE if the column does not exist in the definition table)
    #' @param column Name of the column
    check_uniqueness_column_name =  function(column){
      
      qu <- glue::glue("SELECT * FROM {self$schema_str}{self$def_table} WHERE {self$def$column_field} = '{column}'")
      
      res <- DBI::dbGetQuery(self$con, qu)
      return(nrow(res)==0)
      
    },
    
    
    #' @description Edit the label for an input field
    #' @param id_form ID of form field
    #' @param new_label New label text
    edit_label_field = function(id_form, new_label){
      
      self$replace_value_where(self$def_table, col_compare = self$def$id_form, val_compare = id_form,
                               col_replace = self$def$label_field, 
                               val_replace = new_label)
    },
    
    #' @description Change the field type for a form field
    #' @param id_form ID of form field
    #' @param new_type New type
    edit_field_type = function(id_form, new_type){
      
      self$replace_value_where(self$def_table, col_compare = self$def$id_form, val_compare = id_form,
                               col_replace = self$def$type_field, 
                               val_replace = new_type)
      
    },
    
    #' @description from_json wrapper
    #' @param x Text
    #' @param ... Other args to [shintocatman::from_json()]
    from_json = function(x, ...){
      
      shintocatman::from_json(x, ...)
      
    },
    
    #' @description to_json wrapper
    #' @param x Text
    #' @param ... Other args to [shintocatman::to_json()]
    to_json = function(x, ...){
      
      shintocatman::to_json(x, ...)
      
    },
    
    #' @description Edit options (choices) for a form field
    edit_options_field = function(id_form, new_options){
      if(length(id_form) == 0){
        cli::cli_alert_danger("$edit_options_field called with empty or null ID - aborting!")
        return(invisible(NULL))
      }
      
      if(!is.character(new_options)){
        new_options <- self$to_json(new_options)  
      }
      
      
      if(is.null(names(self$from_json(new_options)))){
        stop("JSON new_options MUST have names (1:n) (edit_options_field)")
      }
      
      self$replace_value_where(self$def_table, 
                               col_compare = self$def$id_form, 
                               val_compare = id_form,
                               col_replace = self$def$options, 
                               val_replace = new_options)
      
    },
    
    #' @description Label for nested column is kept in options list
    #' @param id_form ID of form field
    edit_nested_column_label = function(id_form, opts, new_label){
      
      opts <- self$from_json(opts)
      opts$label <- new_label
      
      self$replace_value_where(self$def_table, 
                               col_compare = self$def$id_form, 
                               val_compare = id_form,
                               col_replace = self$def$options, 
                               val_replace = as.character(self$to_json(opts)))
      
    },
    
    
    #' @description Is the provided color(s) valid?
    is_color = function(colors){
      
      sapply(colors, function(x) {
        tryCatch(!is.na(x) && is.matrix(col2rgb(x)), 
                 error = function(e) FALSE)
      }, USE.NAMES = FALSE)
      
    },
    
    #' @description Edit colors for a form field
    #' @param id_form ID of form field
    edit_options_colors = function(id_form, new_colors){
      
      if(any(!self$is_color(new_colors))){
        stop("Invalid colors!")
      }
      
      new_colors <- self$to_json(as.list(new_colors) %>% setNames(1:length(new_colors)))
      
      self$replace_value_where(self$def_table, 
                               col_compare = self$def$id_form, 
                               val_compare = id_form,
                               col_replace = self$def$colors, 
                               val_replace = new_colors)
      
    },
    
    #' @description If new options added, make sure the color vector is amended
    #' @param id_form ID of form field
    amend_options_colors = function(id_form, options){
      
      cur_color <- self$get_by_id(id_form) %>%
        dplyr::pull(!!sym(self$def[["colors"]]))
      
      cur_color <- self$from_json(cur_color)
      options <- self$from_json(options)
      
      if(length(cur_color) != length(options)){
        
        nc <- length(cur_color)
        n <- length(options) - nc
        
        if(n > 0){ # new colors
          new_cols <- as.list(rep(self$default_color,n))
          
          self$edit_options_colors(id_form, c(cur_color, new_cols))  
        } else { # remove colors
          
          new_cols <- cur_color[1:(length(cur_color) + n)]
          self$edit_options_colors(id_form, new_cols)
        }
        
      }
      
      
    },
    
    #' @description Set the order for a single/multi select field
    #' @param id_form ID of form field
    set_options_order = function(id_form, new_order){
      
      if(!is.null(names(new_order))){
        warning("Dropping names : set_options_order")
        names(new_order) <- NULL
      }
      
      if(!is.character(new_order) && length(new_order) > 0){
        new_order <- self$to_json(new_order)
      }
      
      self$replace_value_where(self$def_table, 
                               col_compare = self$def$id_form, 
                               val_compare = id_form,
                               col_replace = self$def$order_options, 
                               val_replace = new_order)
      
    },
    
    
    #' @description Prepare a nested choice column
    #' @param id_form ID of form field
    prepare_nested_choice_column = function(id_form, name, options){
      
      key <- setNames(list(options),name)
      
      value <- vector("list", length = length(options))
      names(value) <- names(key[[1]])
      
      lis <- list(key = key, value = value)
      
      self$edit_options_field(id_form, lis)
    },
    
    
    #' @description If new categories added or removed, make sure the order vector is amended
    #' @param id_form ID of form field
    amend_options_order = function(id_form, options){
      
      cur_order <- self$get_by_id(id_form) %>%
        dplyr::pull(!!sym(self$def[["order_options"]]))
      
      cur_order <- self$from_json(cur_order)
      options <- self$from_json(options)
      new_order <- cur_order
      
      if(length(options) < length(cur_order)){
        
        new_order <- cur_order[1:length(options)]  
        self$set_options_order(id_form, new_order)
        
      } else if(length(options) > length(cur_order)){
        
        nc <- length(cur_order)
        n <- length(options) - nc
        
        new_order <- c(cur_order, (nc+1):(nc+n))
        self$set_options_order(id_form, new_order)
        
      }
      
      return(invisible(new_order))
    },
    
    
    #' @description Like amend_options_order but takes field name
    fix_options_order = function(column_field){
      
      id_form <- self$read_definition(lazy = TRUE) %>%
        filter(!!sym(self$def$column_field) == !!column_field) %>%
        select(all_of(c(self$def[["id_form"]],self$def[["options"]]))) %>%
        collect
      
      id <- id_form[[1]]
      opts <- id_form[[2]]
      
      ord <- self$amend_options_order(id, opts)
      
      return(ord)
    },
    
    #' @description Edit the form layout
    edit_formulier_setup = function(new_setup){
      
      lapply(1:nrow(new_setup), function(x){
        
        id <- new_setup$id[x]
        side <- new_setup$side[x]
        order <- new_setup$order[x]
        
        qu <- glue::glue("UPDATE {self$schema_str}{self$def_table} SET {self$def$form_section} = '{side}', \"{self$def$order_field}\" = '{order}'  WHERE {self$def$id_form} = '{id}'")
        
        self$execute_query(qu)
        
      })
      
    },
    
    #' @description Change the visibility
    #' @param id_formfield ID of form field
    edit_zichtbaarheid_invoerveld = function(id_formfield, new_zichtbaarheid){
      
      qu <- glue::glue("UPDATE {self$schema_str}{self$def_table} SET {self$def$visible} = {new_zichtbaarheid} WHERE {self$def$id_form} = '{id_formfield}'")
      
      self$execute_query(qu)
      
    },
    
    #' @description Change the removal date
    #' @param id_formfield ID of form field
    edit_verwijder_datum = function(id_formfield, new_date){
      
      qu <- glue::glue("UPDATE {self$schema_str}{self$def_table} SET {self$def$date_deleted} = '{new_date}' WHERE {self$def$id_form} = '{id_formfield}'")
      
      self$execute_query(qu)
      
    },
    
    # hoeft alleen voor zichtbare velden
    #' @description Change the formfield order
    amend_formfield_order = function(formside, deleted_number){
      
      qu <- glue::glue("UPDATE {self$schema_str}{self$def_table} SET \"{self$def$order_field}\" = \"{self$def$order_field}\" - 1 WHERE {self$def$form_section} = '{formside}' AND \"{self$def$order_field}\" > {deleted_number} AND {self$def$visible} = TRUE")
      
      self$execute_query(qu)
      
    },
    
    #' @description Reset the form field order
    #' @param id_formfield ID of form field
    reset_volgorde_invoerveld = function(id_formfield, new_volgorde_nummer){
      
      qu <- glue::glue("UPDATE {self$schema_str}{self$def_table} SET \"{self$def$order_field}\" = '{new_volgorde_nummer}' WHERE {self$def$id_form} = '{id_formfield}'")
      
      self$execute_query(qu)
      
    },
    
    #' @description Edit states for options for a form field
    #' @param id_form ID of form field
    edit_options_states = function(id_form, new_states){
      #Check if column exists
      if(is.null(self$def$option_active)){
        cli::cli_alert_warning(glue("No status column exists in {self$def_table} table"))
      } else {
        if(length(id_form) == 0){
          cli::cli_alert_danger("$edit_options_field called with empty or null ID - aborting!")
          return(invisible(NULL))
        }
        
        if(!is.character(new_states)){
          new_states <- self$to_json(new_states)  
        }
        
        
        if(is.null(names(self$from_json(new_states)))){
          stop("JSON new_states MUST have names (1:n) (edit_options_field)")
        }
        
        self$replace_value_where(self$def_table, 
                                 col_compare = self$def$id_form, 
                                 val_compare = id_form,
                                 col_replace = self$def$option_active, 
                                 val_replace = new_states)
      }
      
      
    },
    
    #' @description Actually really delete a field
    #' @param id ID of form field
    really_delete_formfield = function(id){
      
      tab <- glue::glue("{self$schema_str}{self$def_table}")
      
      qu <- glue::glue("delete from {tab} where {self$def$id_form} = '{id}'")
      self$execute_query(qu)
    },
    
    
    #' @description Database column type based on field input type
    #' @param type One of 'text', 'numeric', 'boolean', 'singleselect', 'multiselect', 'date', 'singlecheck'
    column_type_from_field_type = function(type){
      
      db <- self$dbtype
      
      if(db == "sqlite"){
        switch(type,
               freetext = "text",
               numeric = "real",
               boolean = "integer",
               singleselect = "text",
               multiselect = "text",
               date = "text",
               singlecheck = "boolean",
               "text"   # if not in list
        )
      } else if(db == "postgres"){
        switch(type,
               freetext = "text",
               numeric = "double precision",
               boolean = "boolean",
               singleselect = "text",
               multiselect = "text",
               date = "date",
               singlecheck = "boolean",
               "text"   # if not in list
        )
      } else {
        stop("DB type not supported ($column_type_from_field_type)")
      }
      
    },
    
    #' @description Make sure the necessary columns are present in the data_table
    prepare_data_table = function(){
      
      tab <- self$get_input_fields()
      
      cols_output <- self$table_columns(self$data_table)
      
      for(i in seq_len(nrow(tab))){
        
        if(tab$type_field[i] == "nestedselect"){
          column_2 <- names(self$from_json(tab$options[i])$value)
          column_2 <- janitor::make_clean_names(tolower(column_2), parsing_option = 1)
          
          if(!tolower(column_2) %in% cols_output){
            self$make_column(self$data_table, column_2, "text")
            
            # wanneer audit -> kolom ook aan audit table toevoegen
            if(self$audit){
              self$make_column(self$audit_table, column_2, "text")
            }
          }
        }
        
        
        if(!tab$column_field[i] %in% cols_output){
          
          new_col_type <- self$column_type_from_field_type(tab$type_field[i])
          
          flog.info(glue("Adding column: {tab$column_field[i]}, type : {new_col_type}"))
          self$make_column(self$data_table, tab$column_field[i], new_col_type)
          
          # wanneer audit -> kolom ook aan audit table toevoegen
          if(self$audit){
            self$make_column(self$audit_table, tab$column_field[i], new_col_type)
            
          }
          
        } else {
          
          # do nothing.
          # TODO maybe check that even though the column exists it has the right type
          
        }
        
      }
      
    },
    
    
    #' @description Rename registrations data (as read with $read_registrations) to 
    #' internal column names as defined in `data_columns` argument
    #' @param data Dataframe
    rename_registrations_intern = function(data){
      
      cols <- self$data_columns
      ii <- match(names(data), unlist(cols))
      mtch <- which(!is.na(ii))
      
      names(data)[mtch] <- names(cols)[ii[mtch]]
      
      data
    },
    
    
    #' @description Writes a registration to the data table
    #' @param data data
    #' @param user_id User ID
    #' @param current_reg_id ID for the registration
    #' @param data_only Return only the dataset before appending to an actual table (for testing)
    write_new_registration = function(data, user_id, current_reg_id, data_only = FALSE){ 
      # add missing columns to output etc.
      self$prepare_data_table()
      
      data_pre <- data.frame( 
        id = current_reg_id, 
        time_created = format(self$postgres_now()),
        time_modified = format(self$postgres_now()),
        user = user_id
      )
      
      
      # TODO generic renaming method (to/from, data/def tables)
      data_pre <- dplyr::rename_with(data_pre, 
                                     .fn = function(x){
                                       unname(unlist(self$data_columns[x]))
                                     })  
      
      data[sapply(data,length) == 0] <- NULL
      
      
      data <- as.data.frame(
        lapply(data, function(x){
          # jsonify arrays
          if(length(x) > 1){
            x <- as.character(self$to_json(x))
          }
          
          # needed for empty json strings; not sure why
          if(class(x) == "json"){
            x <- as.character(x)
          }
          
          x
        })
      )
      
      data_all <- cbind(data_pre, data)
      
      if(data_only)return(data_all)
      
      # Append to db table
      res <- self$append_data(self$data_table, data_all)
      
      # If no error, create casenr based on serial_id and possible casenr-code
      if(!inherits(res, "try-error")){
        new_serial <- self$get_registration_by_id(current_reg_id)[[self$data_columns$serial_id]]
        
        if(is.null(self$casenr_code)){
          self$replace_value_where(self$data_table, col_replace = self$data_columns$casenr, val_replace = new_serial, col_compare = self$data_columns$id, val_compare = current_reg_id)
        } else {
          new_serial_code <- glue::glue("{self$casenr_code}-{new_serial}")
          self$replace_value_where(self$data_table, col_replace = self$data_columns$casenr, val_replace = new_serial_code, col_compare = self$data_columns$id, val_compare = current_reg_id)
        }
      }
      
      
      # TRUE if success (append_data has a try()) 
      return(!inherits(res, "try-error"))
    },
    
    #' @description Edits an existing registration
    #' @param old_data Dataframe with previous entry of registration
    #' @param new_data Dataframe with new data
    #' @param user_id User ID, for audit logging
    #' @param current_reg_id Registration ID
    edit_registration = function(old_data, new_data, user_id, current_reg_id){
      # prepare output columns
      self$prepare_data_table()
      
      # id of the registration
      id <- current_reg_id 
      
      replace_list <- list()
      
      
      # valideer of de kolom echt gewijzigd is
      for(col in names(new_data)){
        new_value <- new_data[[col]]
        old_value <- old_data[[col]]
        
        # When a field is not filled in, even when NULL is given as input, the system sometimes retreives it as NA,
        # # So this extra check is necessary; comparison with NULL is fine. TODO: Consult with Remko
        # if(is.na(old_value)){
        #   old_value <- NULL
        # }
        
        data_has_changed <- try(!isTRUE(new_value == old_value) && !(is.na(old_value) & is.na(new_value)))
        
        if(inherits(data_has_changed, "try-error")){
          warning(glue("Problem with column: {col}"))
        }
        
        # data has changed; append to list
        if(isTRUE(data_has_changed)){ 
          replace_list[col] <- new_value 
        }
      } 
      
      
      self$replace_value_where_multi(self$data_table,
                                     replace_list=replace_list, 
                                     col_compare=self$data_columns$id,
                                     val_compare=id,  
                                     query_only = FALSE, 
                                     quiet = FALSE,  
                                     username=user_id)
      
      
      
      return(TRUE)
    },
    
    
    #' @description Soft/hard/(un)delete a registration. `deleted` field will be set to 1.
    #' @param registration_id ID of the registration
    #' @param method Either 'soft', 'hard' (really delete), 'undelete'
    delete_registration = function(registration_id, method = c("soft","hard","undelete")){
      
      if(length(registration_id) > 1){
        registration_id <- registration_id[1]
        message("Warning: $delete_registration is not vectorized, send one registration_id!")
      }
      method <- match.arg(method)
      
      if(self$audit){
        
        data_row_query_1 <- glue("select * from {self$schema_str}{self$data_table} WHERE {self$data_columns$id} =?val_compare;")
        data_row_query <- sqlInterpolate(DBI::ANSI(), data_row_query_1,  val_compare = registration_id) 
        data_row <- self$get_query(data_row_query)
        
        self$append_data(self$audit_table, data_row)
        
      }
      
      
      # time_modified
      tabl <- ifelse(is.null(self$schema),self$data_table,glue("{self$schema}.{self$data_table}"))
      mod_query <- glue("update {tabl} set {self$data_columns$time_modified} = NOW()::timestamp where ",
                        "{self$data_columns$id} = '{registration_id}'") %>% as.character() 
      
      if(!is.null(mod_query)){
        self$execute_query(mod_query)
      }
      
      if(method %in% c("soft","undelete")){
        flag <- ifelse(method == "soft", 1, 0)
        self$replace_value_where(self$data_table, col_replace = self$data_columns$deleted, 
                                 val_replace = flag,
                                 col_compare = self$data_columns$id, val_compare = registration_id)  
        
      } else if(method == "hard"){
        
        self$delete_rows_where(self$data_table, col_compare = self$data_columns$id, val_compare = registration_id)
        
        # TODO
        # audit en relations ook kunnen verwijderen voor AVG
      }
      
    },
    
    
    #' @description Read registrations, recode select values where needed.
    #' @param recode If TRUE (default), replaces codes with labels (for select fields)
    #' @param include_deleted If TRUE, include deleted registrations
    #' @param lazy If TRUE, returns a lazy tbl
    read_registrations = function(recode = TRUE, 
                                  include_deleted = TRUE,
                                  deleted_only = FALSE,
                                  lazy = FALSE
    ){
      
      data <- self$read_table(self$data_table, lazy = TRUE)
      
      if(deleted_only){
        data <- filter(data, !!sym(self$data_columns$deleted) == 1)
      } else if(!include_deleted){
        data <- filter(data, !!sym(self$data_columns$deleted) == 0)
      }
      
      if(!lazy){
        data <- dplyr::collect(data)
        
        if(recode){
          data <- self$recode_registrations(data)
        }
      } 
      
      data
      
    },
    
    #' @description Replace column names of registrations with their labels
    #' @param data Read in with `read_registrations` or `get_registration_by_id`
    label_registrations_columns = function(data){
      
      labc <- self$def$label_field
      colc <- self$def$column_field
      
      key <- self$read_definition(lazy=TRUE) %>% 
        select(all_of(c(labc,colc))) %>% 
        collect
      
      ii <- match(names(data), key[[colc]])
      jj <- which(!is.na(ii))
      ii <- ii[!is.na(ii)]
      
      names(data)[jj] <- key[[labc]][ii]
      data
      
    },
    
    #' @description Recode select values in registrations data
    #' @param data Dataframe (registrations data)
    recode_registrations = function(data){
      # Single select, can use a direct `dplyr::recode`
      def <- self$read_definition(lazy = TRUE) %>% 
        self$filter_by_visibility() %>%
        filter(!!sym(self$def[["type_field"]]) %in% c("singleselect","nestedselect"),
               !!sym(self$def[["column_field"]]) %in% !!names(data)) %>%
        collect
      
      
      
      # for every select field, replace values
      for(i in seq_len(nrow(def))){
        opt <- def[[self$def$options]][i]
        key <- self$from_json(opt)
        
        if(is.null(self$def$option_active)){
          active_key <- setNames(as.list(rep(TRUE, length(key))), names(key))
        } else {
          active <- def[[self$def$option_active]][i]
          active_key <- self$from_json(active)
        }
        
        if(length(key) == length(active_key)){
          key <- mapply(function(str, flag) {
            if (!flag) paste0(str, "*") else str
          }, key, active_key, SIMPLIFY = FALSE)
        } else {
          flog.info(glue("Option statuses for field {self$def[['column_field']]} are not used!"))
        }
        
        if(def[[self$def[["type_field"]]]][i] == "nestedselect"){
          key <- key$key[[1]]
        }
        
        col <- def[[self$def$column_field]][i]
        
        if(length(key)){
          data[[col]] <- dplyr::recode(data[[col]], !!!key)   
        }
        
      }
      
      # Multi-select, make JSON values
      def_multi <- self$read_definition(lazy = TRUE) %>% 
        filter(!!sym(self$def[["type_field"]]) == "multiselect",
               !!sym(self$def[["column_field"]]) %in% !!names(data)) %>%
        collect
      
      
      for(i in seq_len(nrow(def_multi))){
        opt <- def_multi[[self$def$options]][i]
        key <- self$from_json(opt)
        col <- def_multi[[self$def$column_field]][i]
        
        if(is.null(self$def$option_active)){
          active_key <- setNames(as.list(rep(TRUE, length(key))), names(key))
        } else {
          active <- def_multi[[self$def$option_active]][i]
          active_key <- self$from_json(active)
        }
        
        if(length(key) == length(active_key)){
          key <- mapply(function(str, flag) {
            if (!flag) paste0(str, "*") else str
          }, key, active_key, SIMPLIFY = FALSE)
        } else {
          flog.info(glue("Option statuses for field {self$def[['column_field']]} are not used!"))
        }
        
        if(length(key)){
          
          val <- lapply(data[[col]], function(x)if(x %in% c("[]","{}"))NA_character_ else self$from_json(x))
          i_val <- vapply(val, function(x)!all(is.na(x)), FUN.VALUE = logical(1))
          val[i_val]  <- sapply(val[i_val], function(x)self$to_json(unname(unlist(key[x]))))
          
          val[val == "NA"] <- NA
          
          data[[col]] <- unlist(val)
        }
        
      }
      
      data
    },
    
    #' @description Get a registration with a uuid
    #' @param id ID of the registration (uuid)
    #' @param recode If TRUE, recodes integer values to their labels
    get_registration_by_id = function(id, recode = TRUE){
      
      out <- self$read_table(self$data_table, lazy = TRUE) %>%
        dplyr::filter(!!sym(self$data_columns$id) == !!id) %>%
        collect
      
      if(recode){
        out <- self$recode_registrations(out)
      }
      
      out
      
    },
    
    #' @description Get rows with registrations by the the registration ids.
    #' @param id_forms Ids of registrations
    get_registrations_by_multiple_ids = function(id_forms){
      
      self$read_table(self$data_table, lazy = TRUE) %>%
        dplyr::filter(!!sym(self$data_columns$id) %in% !!id_forms) %>%
        collect
      
    },
    
    #' @description Filter data by time_created
    #' @param data Read with `read_registrations`
    #' @param date_start Date start
    #' @param date_end Date end
    filter_period = function(data, 
                             date_start = Sys.Date()-7, 
                             date_end = Sys.Date()){
      
      timecol <- self$data_columns$time_created
      
      if(!timecol %in% names(data)){
        stop("$filter_period works only on data returned with $read_registrations")
      }
      dplyr::filter(data, between(as.Date(!!sym(timecol)), date_start, date_end))
      
    },
    
    #' @description Reads the definition table
    read_definition = function(...){
      
      self$read_table(self$def_table, ...)
      
    },
    
    
    get_occurences = function(table, column, record){
      
      qu <- glue::glue("SELECT * FROM {self$schema_str}{table} WHERE {column} = '{record}'")
      
      res <- DBI::dbGetQuery(self$con, qu)
      
      return(res)
      
    },
    
    get_registration_by_json = function(table, column, record){
      
      qu <- glue::glue("SELECT * FROM {self$schema_str}{table} WHERE {column}::jsonb ? '{record}'")
      
      res <- DBI::dbGetQuery(self$con, qu)
      
      return(res)
      
    },
    
    
    
    ####### AUDIT #####
    
    # maak punt mutaties; let op dit is een zware operatie!
    # table: is de huidige registratietabel; evt gefiltered 
    # columns: eventueel een selectie van kolommen waardoor de operatie minder heftig is.
    # VOORBEELD 1: alleen events voor de kolom aantal_brommers (+ creaties)
    #timeseries <- .reg$create_timeseries(columns=c("aantal_brommers"),table=NULL) 
    # VOORBEELD 2: hergebruik van reeds ingeladen data object 
    #timeseries <- .reg$create_timeseries(columns=NULL,table=signal_data()) 
    
    #' @description Creates a timeseries for the audit
    create_timeseries = function(columns = NULL, date_range = NULL){
      
      selected_columns <- unique(c(columns,
                                   self$data_columns$id,  
                                   self$data_columns$name, 
                                   self$data_columns$user, 
                                   self$data_columns$time_modified))
      
      
      data_current <- self$read_table(self$data_table, lazy=TRUE) %>%
        select(any_of(selected_columns))
      
      data_history <- self$read_table(self$audit_table, lazy=TRUE) %>%
        select(any_of(selected_columns))
      
      if(!is.null(date_range)){
        
        if(length(date_range) != 2 || sum(is.na(date_range)) > 0){
          message("Malformed date_range passed to $create_timeseries (shintoforms), ignoring!")
        } else {
          data_current <- dplyr::filter(data_current, between(time_modified, !!date_range[1], !!date_range[2]))
          data_history <- dplyr::filter(data_history, between(time_modified, !!date_range[1], !!date_range[2]))
        }
        
      }
      
      data_current <- collect(data_current)
      data_history <- collect(data_history)
      
      return(dplyr::bind_rows(data_current, data_history))
    },
    
    # functie om een audit table om te zetten in een event log
    # creation only filterd alleen de aanmaak events en dus geen updates
    #' @description Creates 'events' based on the timeseries
    create_events = function(timeseries, date_range = NULL, creation_only=NULL){
      
      if(!is.null(creation_only))message("argument creation_only to $create_events is ignored")
      
      create_partial_mutations_new(timeseries, 
                                   lowerdate = date_range[1],
                                   upperdate = date_range[2],
                                   auditstamp_column = self$data_columns$time_modified,
                                   id_column = self$data_columns$id,
                                   name_column = self$data_columns$name,
                                   username_column = self$data_columns$user)
      
    }, 
    
    ## Relations
    #' @description Reads the relation table
    get_all_relations = function(){
      
      if(is.null(self$relation_table)){
        stop("Set 'relation_table' in shintoforms formcall (has no default value anymore)")
      }
      
      qu <- glue::glue("SELECT * FROM {self$schema_str}{self$relation_table} WHERE {self$relation_columns$verwijderd} = false;")
      
      self$query(qu)
      
    },
    
    #' @description get_objects_for_collector 
    #' @param collector_type Type of collector
    #' @param collector_id ID of collector
    #' @param relation_type Description of collector -> object relation
    #' @param relation_id ID of relation (often session ID of module)
    #' @param filter_verwijderd when status is zero
    # examples: 
    # get_objects_for_collector('signaal')
    # get_objects_for_collector('signaal', collector_id="12")
    # get_objects_for_collector('signaal', relation_type="hoofdadres") 
    get_objects_for_collector = function(collector_type=NA,
                                         collector_id=NA,
                                         relation_type=NA,
                                         relation_id=NA,
                                         object_type=NA, 
                                         filter_verwijderd=TRUE){
      
      collector_type = ifelse(is.na(collector_type), self$class_type, collector_type) 
      
      # Build query parts
      V <- ifelse(filter_verwijderd, glue(" AND {self$relation_columns$verwijderd} = false"), "")
      A <- ifelse(is.na(collector_id), "", glue(" AND {self$relation_columns$collector_id} = '{collector_id}'"))
      B <- ifelse(is.na(relation_id),"", glue(" AND {self$relation_columns$relation_id} = '{relation_id}'"))
      C <- ifelse(is.na(relation_type), "", glue(" AND {self$relation_columns$relation_type} = '{relation_type}'"))
      D <- ifelse(is.na(object_type), "", glue(" AND {self$relation_columns$object_type} = '{object_type}'"))
      
      qu <- glue::glue("SELECT * FROM {self$schema_str}{self$relation_table} WHERE {self$relation_columns$collector_type} = '{collector_type}'{V}{A}{B}{C}{D};")
      
      self$query(qu)
      
    },  
    
    #' @description get_collector_for_object 
    #' @param collector_type Type of collector
    #' @param collector_id ID of collector
    #' @param relation_type Description of collector -> object relation
    #' @param relation_id ID of relation (often session ID of module)
    #' @param filter_verwijderd when status is zero
    # examples: 
    # get_objects_for_collector('signaal')
    # get_objects_for_collector('signaal', collector_id="12")
    # get_objects_for_collector('signaal', relation_type="hoofdadres") 
    get_collector_for_object = function( object_id,
                                         collector_type=NA,
                                         object_type=NA,
                                         relation_type=NA,
                                         relation_id=NA,
                                         filter_verwijderd=T){
      
      # Removed this statement so we are not limited to only one collector_type. 
      # This collector_type has been added as arguments to function-calls which were already made in apollo_ondermijning.
      #collector_type = ifelse(is.na(collector_type), self$class_type, collector_type)
      
      # Build query parts
      V <- ifelse(filter_verwijderd, glue(" AND {self$relation_columns$verwijderd} = false"), "")
      A <- ifelse(is.na(object_type), "", glue(" AND {self$relation_columns$object_type} = '{object_type}'"))
      B <- ifelse(is.na(collector_type), "", glue(" AND {self$relation_columns$collector_type} = '{collector_type}'"))
      C <- ifelse(is.na(relation_id),"", glue(" AND {self$relation_columns$relation_id} = '{relation_id}'"))
      D <- ifelse(is.na(relation_type), "", glue(" AND {self$relation_columns$relation_type} = '{relation_type}'"))
      
      qu <- glue::glue("SELECT * FROM {self$schema_str}{self$relation_table} WHERE {self$relation_columns$object_id} = '{object_id}'{V}{A}{B}{C}{D};")
      
      self$query(qu)
      
    },
    
    #' @description Adds a relation to the relation table
    add_relation = function(id = uuid::UUIDgenerate(),
                            collector_id,
                            collector_type, 
                            object_id,
                            object_type,
                            relation_id,
                            relation_type,
                            comment=NA,
                            status="1",
                            verwijderd=FALSE,
                            username,
                            timestamp){ 
      
      data <- data.frame(id,
                         collector_id,
                         collector_type, 
                         object_id,
                         object_type,
                         relation_id,
                         relation_type,
                         comment,
                         status,
                         username,
                         timestamp)
      
      names(data) <- self$relation_columns
      
      self$append_data(self$relation_table, data) 
      
    },
    
    #' @description timestamp in postgres timezone 
    postgres_now = function(){
      
      # overwrites shintodb::databaseClass / postgres_now because it fails on sqlite,
      # so we need a fallback here
      
      tm <- try({
        self$query("select now()", quiet = TRUE)$now  
      }, silent = TRUE)
      
      if(inherits(tm, "try-error")){
        return(Sys.time())
      }
      
      tm
      
    },
    
    #' @description Select rows in registration audit table since some timestamp
    #' @param time_since POSIXct timestamp
    get_rows_since_auditstamp = function(time_since){  
      
      time_since <- format(time_since)  
      
      modcol <- self$data_columns$time_modified
      #creacol <- self$data_columns$time_created
      
      collect_cols <- c(self$data_columns$id, modcol)#, creacol)
      
      modif <- self$read_table(self$data_table, lazy = TRUE) %>%
        filter(!!sym(modcol) > !!time_since) %>%
        select(all_of(collect_cols)) %>%
        collect
      
      #creat <- self$read_table(self$data_table, lazy = TRUE) %>%
      #  filter(!!sym(creacol) > !!time_since) %>%
      #  select(all_of(collect_cols)) %>%
      #  collect 
      dplyr::distinct(modif) #rbind(modif, creat))
      
    },
    
    
    #' @description Soft delete relations
    #' @param ids ID of rows to delete
    soft_delete_relations = function(ids){
      
      if(length(ids) > 0){
        
        if(self$audit) {
          
          data_row <- self$filter(self$relation_table, !!sym(self$relation_columns$id) %in% ids)
          self$append_data(self$relation_audit_table, data_row)
        }
        
        # delete all relations that are ALTERED or DELETED
        qu_delete <- glue::glue("DELETE FROM {self$schema_str}{self$relation_table} WHERE {self$relation_columns$id} in ('{paste(ids, collapse=\"','\")}');")
        
        self$execute_query(qu_delete)
      }
    },  
    
    #' @description Append relations
    #' @param data Data to append 
    #' @param registration_id ID for the registration
    write_new_relations = function(data, registration_id){ 
      postgres_time <- self$postgres_now()
      
      # Voor postgres GEBRUIK now()::timestamp
      data <- data %>%
        mutate(!!sym(self$relation_columns$collector_id) := registration_id,
               !!sym(self$relation_columns$timestamp) := format(postgres_time),
               !!sym(self$relation_columns$collector_type) := ifelse(is.na(!!sym(self$relation_columns$collector_type)), self$class_type, !!sym(self$relation_columns$collector_type)))
      
      
      self$append_data(self$relation_table,
                       data)
      
    },
    
    #' @description update current relation table for registration ID
    #' @param new_relations The current state that should end up in the relations table 
    #' @param registration_id ID for the registration
    update_relations = function(new_relations, registration_id){ 
      old_relations <- self$filter(self$relation_table, !!sym(self$relation_columns$collector_id) == !!registration_id)
      
      if(nrow(new_relations) == 0 & nrow(old_relations) == 0) return(TRUE)
      
      # Explanation:
      # There are four groups of data:
      # DELETED rows   (in old_relations,     not in new_relations)
      # NEW rows       (not in old_relations, in new_relations)
      # UNALTERED rows (in old_relations,     in new_relations, no differences)
      # ALTERED rows   (in old_relations,     in new_relations, with differences) 
      #
      # actions:
      # DELETED rows are archived, deleted, then the new version is added
      # ALTERED rows are archived, deleted, then the new version is added
      # NEW rows are added
      # UNALTERED rows are left as they are
      
      # get all (old) relations that are ALTERED or DELETED
      altered_or_deleted <- old_relations %>% 
        left_join(new_relations, by=self$relation_columns$id, suffix=c("", "_new"))  %>% 
        filter((!!sym(glue("{self$relation_columns$comment}_new")) != !!sym(self$relation_columns$comment) & !!sym(glue("{self$relation_columns$status}_new")) != !!sym(self$relation_columns$status)) | is.na(!!sym(glue("{self$relation_columns$collector_id}_new")))) %>%
        mutate(!!sym(self$relation_columns$verwijderd) := ifelse(is.na(!!sym(glue("{self$relation_columns$collector_id}_new"))), TRUE, !!sym(self$relation_columns$verwijderd))) %>%
        select(!ends_with('_new'))
      
      # archive all relations that are ALTERED or DELETED   
      self$soft_delete_relations(altered_or_deleted[[self$relation_columns$id]]) 
      
      # append all relations that are NEW or ALTERED
      new  <- new_relations %>% 
        left_join(old_relations, by=self$relation_columns$id, suffix=c("", "_old"))%>% 
        filter(is.na(!!sym(glue("{self$relation_columns$status}_old")))) %>%
        select(!ends_with('_old'))
      
      new_data <- dplyr::bind_rows(new, altered_or_deleted)
      if(nrow(new_data) > 0){
        self$write_new_relations(new_data, registration_id = registration_id)
      }
      
      return(TRUE)
      
    } 
  )
  
  
)

