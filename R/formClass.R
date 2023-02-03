#' R6 Class voor Registratie formulier
#' @importFrom R6 R6Class
#' @export
formClass <- R6::R6Class(
  lock_objects = FALSE,
  
  public = list(
    
    #' @description Make new form object
    #' @param config_file Path to DB config
    #' @param wat Entry in config for DB connection
    #' @param schema DB schema
    #' @param filterable Boolean, TRUE if the user wants to control which registrations to filter. Requires make_filter and tooltip in the def_columns list.
    #' @param table Table in DB (in schema) that holds form config
    #' @param pool If TRUE, connects with dbPool
    #' @param audit If TRUE, writes edits to audit log
    #' @param sqlite If path to an SQLite file, uses SQLite.
    #' @param default_color Default color
    initialize = function(config_file = "conf/config.yml", 
                          what, 
                          schema = NULL,
                          class_type="",
                          db_connection = NULL,
                          filterable = FALSE,
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
                            removable = "kan_worden_verwijderd",
                            make_filter = "make_filter",
                            tooltip = "tooltip"
                          ),
                          data_table = "registrations",
                          data_columns = list(
                            id = "id_registratie",
                            name = "naam_registratie",
                            time_created = "invoerdatum",
                            time_modified = "wijzigdatum",
                            user = "user_id",
                            status = "status",
                            priority = "priority" 
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
                            timestamp = "timestamp",
                            verwijderd = "verwijderd"
                          ), 
                          relation_audit_table = "object_relations_audit",
                          inject = NULL,
                          pool = TRUE,
                          sqlite = NULL,
                          default_color = "#3333cc"){
      
      self$class_type <- class_type
      
      self$audit <- audit
      self$audit_table <- audit_table
      
      self$relation_table <- relation_table
      self$relation_columns <- relation_columns
      self$relation_audit_table <- relation_audit_table
      
      self$default_color <- default_color
      
      self$relation_table <- relation_table
      self$relation_columns <- relation_columns
      self$relation_audit_table <- relation_audit_table
      
      if(is.null(db_connection)){
        self$connect_to_database(config_file, schema, what, pool, sqlite)  
      } else {
        
        if(!DBI::dbIsValid(db_connection)){
          stop("Please pass a valid dbConnection object")
        }
        
        self$con <- db_connection
        self$schema <- schema
        self$pool <- pool  #unused with passed db_connection?
        self$dbtype <- "postgres"
        
      }
      
      # 'schema' string for query building
      self$schema_str <- ifelse(is.null(self$schema), "", paste0(self$schema,"."))
      
      
      self$def_table <- def_table
      self$def <- def_columns
         
      
      self$data_table <- data_table
      self$data_columns <- data_columns
      
      # check new 'deleted' field
      datacols <- self$table_columns(self$data_table)
      if(!"deleted" %in% datacols){
        stop("Must have 'deleted' column in registrations data (`alter table <<table>> add column deleted integer default 0.")
      }
      
      self$filterable <- filterable
      
      # Check
      defcols <- self$table_columns(self$def_table)
      di1 <- unlist(self$def) %in% defcols
      if(any(!di1)){
        stop(paste("Columns in def_columns not found:", paste(defcols[!di1], collapse=",")))
      }
      
      
      
      
      # Extra custom fields (modules, can be anything even static HTML)
      self$inject <- inject
    },
    
    #----- Generic database methods
    
    #' @description Connect to a database
    connect_to_database = function(config_file = NULL, 
                                   schema = NULL, 
                                   what = NULL, 
                                   pool = TRUE, 
                                   sqlite = NULL){
      
      if(!is.null(sqlite)){
        
        if(!file.exists(sqlite)){
          stop("SQlite not found, check path")
        }
        
        self$schema <- NULL
        self$dbname <- sqlite
        self$pool <- pool
        
        if(pool){
          self$con <- dbPool(RSQLite::SQLite(), dbname = sqlite)  
        } else {
          self$con <- dbConnect(RSQLite::SQLite(), dbname = sqlite)  
        }
        
        self$dbtype <- "sqlite"
        
      } else {
        
        self$schema <- schema
        self$dbname <- what
        self$pool <- pool
        self$dbuser <- cf$dbuser
        self$dbtype <- "postgres"
        
        response <- try({
          shintodb::connect(what, pool = pool)
        })
        
        if(!inherits(response, "try-error")){
          self$con <- response
        } else {
          stop("Error when connecting to DB : check your configs!")
        }
        
      }
      
      # check if audit table is present 
      if(self$audit & !DBI::dbExistsTable(self$con, self$audit_table, schema=self$schema)){ 
        stop(glue("Audit feature is on but there is no table named {self$audit_table}")) 
      } 
      
    },
    
    #' @description Close database connection
    close = function(){
      
      if(!is.null(self$con) && dbIsValid(self$con)){
        
        if(self$pool){
          flog.info("poolClose", name = "DBR6")
          
          poolClose(self$con)
        } else {
          flog.info("dbDisconnect", name = "DBR6")
          
          dbDisconnect(self$con)
        }
        
      } else {
        flog.info("Not closing an invalid or null connection", name = "DBR6")
      }
    },
    
    
    make_column = function(table, column, type = "varchar"){
      
      qu <- glue::glue("alter table {self$schema_str}{table} add column {column} {type}")
      
      self$execute_query(qu)
      
    },
    
    
    read_table = function(table, lazy = FALSE){
      
      #tictoc::tic(glue("tbl({table})"))
      
      if(!is.null(self$schema)){
        out <- tbl(self$con, in_schema(self$schema, table))  
      } else {
        out <- tbl(self$con, table)
      }
      
      
      if(!lazy){
        out <- collect(out)
      }
      
      #tictoc::toc()
      
      out
      
    },
    
    append_data = function(table, data){
      
      #flog.info(glue("dbWriteTable({table})"), append = TRUE, name = "DBR6")
      
      if(!is.null(self$schema)){
        
        try(
          dbWriteTable(self$con,
                       name = DBI::Id(schema = self$schema, table = table),
                       value = data,
                       append = TRUE)
        )  
        
      } else {
        
        try(
          dbWriteTable(self$con,
                       name = table,
                       value = data,
                       append = TRUE)
        )
        
      }
      
      
    },
    
    #' @description Delete rows from a table given a single logical condition
    #' @param table Table name
    #' @param col_compare Column where to make logical comparison
    #' @param val_compare Values in `col_compare` where rows will be deleted
    #' @details Do not use inside shiny apps or otherwise, only as a tool to clean up tables.
    delete_rows_where = function(table, col_compare, val_compare){
      
      query <- glue("delete from {self$schema_str}{table} where {col_compare}= ?val")
      query <- DBI::sqlInterpolate(DBI::ANSI(), query, val = val_compare)
      
      self$execute_query(query)
      
    },
    
    
    table_columns = function(table){
      
       names(self$query(glue("select * from {self$schema_str}{table} where false")))
      
    },
    
    query = function(txt, glue = TRUE, quiet = FALSE){
      
      if(glue)txt <- glue::glue(txt)
      # if(!quiet){
      #   flog.info(glue("query({txt})"), name = "DBR6")  
      # }
      # 
      
      try(
        dbGetQuery(self$con, txt)
      )
      
    },
    
    
    execute_query = function(txt,...){
      
      try(
        dbExecute(self$con, txt, ...)
      )
      
    },
    
    has_value = function(table, column, value){
      
      out <- self$query(glue("select {column} from {self$schema_str}{table} where {column} = '{value}' limit 1"))  
      
      nrow(out) > 0
    },
    
    # meerdere tegelijk
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
        
        if(!is.null(self$schema)){
          query <- glue("update {self$schema_str}{table} set {update_str}, {self$data_columns$user} = '{username}', {self$data_columns$time_modified} = NOW()::timestamp where ",
                        "{col_compare} = ?val_compare") %>% as.character() 
          
          if(self$audit){
            audit_query <- glue("insert into {self$schema_str}{self$audit_table} select * from ",
                                "{self$schema_str}{table} where {col_compare}=?val_compare;") %>% as.character() 
            audit_query <- sqlInterpolate(DBI::ANSI(), audit_query,  val_compare = val_compare) 
          }
        }
        
        query <- sqlInterpolate(DBI::ANSI(), 
                                query, 
                                val_compare = val_compare)
        
        if(query_only)return(query)
        
        if(!quiet){
          flog.info(query, name = "DBR6")  
          if(self$audit){ 
            flog.info(audit_query, name = "DBR6")  
          }
        }
        
        if(self$audit){ 
          self$execute_query(audit_query)
        }
        
        self$execute_query(query)   
      }
    }, 
    
    # set verwijderd=1 where naam=gekozennaam.
    # replace_value_where("table", 'verwijderd', 'true', 'naam', 'gekozennaam')
    replace_value_where = function(table, col_replace, val_replace, col_compare, val_compare,
                                   query_only = FALSE, quiet = FALSE){
      
      if(is.logical(val_replace) & !is.na(val_replace)){
        query <- glue("update {self$schema_str}{table} set {col_replace} = ?val_replace::boolean where ",
                      "{col_compare} = ?val_compare") %>% as.character() 
      } else {
        query <- glue("update {self$schema_str}{table} set {col_replace} = ?val_replace where ",
                      "{col_compare} = ?val_compare") %>% as.character() 
      }
        
      
      query <- sqlInterpolate(DBI::ANSI(), 
                              query, 
                              val_replace = val_replace, val_compare = val_compare)
      
      if(query_only)return(query)
      
      self$execute_query(query)
      
    },
     
    #----- Form registration methods
    #' @description Get a row from the form definition by the form id.
    get_by_id = function(id_form){
      
      self$read_table(self$def_table, lazy = TRUE) %>%
        dplyr::filter(!!sym(self$def[["id_form"]]) == !!id_form) %>%
        collect
      
    },
    
    column_name_from_id = function(id_form){
      row <- self$get_by_id(id_form)
      row[[self$def[["column_field"]]]]
    },
    
    get_fields_by_type = function(field_type){
      
      self$read_table(self$def_table, lazy = TRUE) %>%
        dplyr::filter(!!sym(self$def[["type_field"]]) %in% !!field_type,
                      !!sym(self$def[["visible"]])) %>%
        collect
      
    },
    
    
    #' @description Get (recoded) choices for a select field
    get_field_choices = function(column_field){
      
      self$read_definition(lazy = TRUE) %>%
        filter(!!sym(self$def$column_field) == !!column_field) %>%
        pull(!!sym(self$def$options)) %>%
        self$from_json()
      
    },
    
    
    #' @description Make choices (for selectInput) based on values and names
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
    distinct_registration_field = function(column_name){
      
      self$read_table(self$data_table, lazy = TRUE) %>%
        distinct(!!sym(column_name)) %>%
        collect %>%
        pull(!!sym(column_name))
      
    },
    
    
    
    #'@description Rename database table to correct internal column names
    rename_definition_table = function(data){
      
      # Rename to standard colnames
      key <- data.frame(
        old = unname(unlist(self$def)),
        new = names(self$def)
      )
      
      if(!all(names(data) %in% key$old)){
        stop("Not all definition table names configured : check def_columns argument)")
      }
      
      dplyr::rename_with(data, .fn = function(x){
        key$new[match(x, key$old)]
      })
      
    },
    
    
    #' @description Get all non-deleted form input fields
    get_input_fields = function(zichtbaarheid = TRUE){
      
      out <- self$read_table(self$def_table, lazy = TRUE) %>%
        dplyr::filter(!!sym(self$def[["visible"]]) == !!zichtbaarheid) %>% 
        collect
      
      self$rename_definition_table(out)
      
    },
    
    #' @description Get form fields for left or right column of the form
    get_form_fields = function(form_section){
      
      qu <- glue::glue("SELECT * FROM {self$schema_str}{self$def_table} WHERE {self$def$form_section} = {form_section} AND {self$def$visible} = TRUE")
      
      out <- dbGetQuery(self$con, qu)
      out <- out[order(out[[self$def$order_field]]),]
      
      self$rename_definition_table(out)
      
    },
    
    # Alleen zichtbare velden hoeven een volgorde nummer te hebben en moeten worden meegenomen.
    get_next_formorder_number = function(form_section){
      
      qu <- glue::glue("SELECT COUNT(DISTINCT {self$def$id_form}) FROM {self$schema_str}{self$def_table} WHERE {self$def$form_section} = {form_section} AND {self$def$visible} = TRUE")
      
      count <- dbGetQuery(self$con, qu)
      return(count[[1]]+1)
      
    },
    
    #' @description Adds an input field to a form
    #' @param label_field Label for the field
    #' @param type_field Type of field (options : TODO)
    #' @param form_section Left or right
    add_input_field_to_form = function(label_field, type_field, form_section, filterable = NULL, 
                                       tooltip = NULL, column_name = NULL, column_2_name = NULL
                                       ){
      
      
      #assert_input_field_type(type_field)
      
      id <- uuid::UUIDgenerate()
      
      # Sanitize column name
      if(is.null(column_name)){
        column_name <- janitor::make_clean_names(tolower(label_field), parsing_option = 1)
      }
      
      if(!self$check_uniqueness_column_name(column_name))return(-1)
      if(!is.null(column_2_name) && !self$check_uniqueness_column_name(column_2_name))return(-1)
        
      field_order_nr <- self$get_next_formorder_number(form_section)
      
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
                              value = setNames(list(NULL),column_2_name)) %>% 
          self$to_json()
      }
      
      data <- data.frame(
        id_form = id,
        column_field = column_name,
        label_field = label_field,
        type_field = type_field,
        order_field = field_order_nr,
        form_section = form_section,
        visible = TRUE,
        options = as.character(choice_values),
        order_options = "[]",
        colors = '[]',
        removable = TRUE
      )
      
      if(self$filterable){
        data <- data %>%
          mutate(make_filter = filterable,
                 tooltip = tooltip)
      }
      
      
      data <- dplyr::rename_with(data, 
                                 .fn = function(x){
                                   unname(unlist(self$def[x]))
                                 })
      
      self$append_data(self$def_table, data)
      
      if(type_field == "boolean"){
        self$amend_options_order(id, opties)
        self$amend_options_colors(id, opties)
      }
      
      return(0)
        
      
    },
    
    check_uniqueness_column_name =  function(column){
      
      qu <- glue::glue("SELECT * FROM {self$schema_str}{self$def_table} WHERE {self$def$column_field} = '{column}'")
      
      res <- dbGetQuery(self$con, qu)
      return(nrow(res)==0)
      
    },
    
    
    #' @description Edit the label for an input field
    edit_label_field = function(id_form, new_label){
      
      self$replace_value_where(self$def_table, col_compare = self$def$id_form, val_compare = id_form,
                               col_replace = self$def$label_field, 
                               val_replace = new_label)
    },
    
    edit_field_type = function(id_form, new_type){
      
      self$replace_value_where(self$def_table, col_compare = self$def$id_form, val_compare = id_form,
                               col_replace = self$def$type_field, 
                               val_replace = new_type)
      
    },
    
    edit_filterable_column = function(id_form, new_filter_boolean, new_tooltip){
      self$replace_value_where(self$def_table, col_compare = self$def$id_form, val_compare = id_form,
                               col_replace = self$def$make_filter, 
                               val_replace = new_filter_boolean)
      
      self$replace_value_where(self$def_table, col_compare = self$def$id_form, val_compare = id_form,
                               col_replace = self$def$tooltip, 
                               val_replace = new_tooltip)
    },
    
    from_json = function(x, ...){
      
      shintocatman::from_json(x, ...)
      
    },
    
    to_json = function(x, ...){
      
      shintocatman::to_json(x, ...)
      
    },
    
    edit_options_field = function(id_form, new_options){
      
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
    edit_nested_column_label = function(id_form, opts, new_label){
      
      opts <- self$from_json(opts)
      opts$label <- new_label
      
      self$replace_value_where(self$def_table, 
                               col_compare = self$def$id_form, 
                               val_compare = id_form,
                               col_replace = self$def$options, 
                               val_replace = as.character(self$to_json(opts)))
      
    },
    
    
    # amend_nested_options_key = function(id_form, new_options){
    #   
    #   # check if we have to amend nested select options
    #   nested_fields <- self$get_fields_by_type("nestedselect")
    #   
    #   if(nrow(nested_fields) > 0){
    #     colname <- self$column_name_from_id(id_form)
    #     
    #     for(i in seq_len(nrow(nested_fields))){
    #       dat <- slice(nested_fields,i)
    #       o <- self$from_json(dat[[self$def$options]])
    #       if(names(o$key) == colname){
    #         o$key[[1]] <- self$from_json(new_options)
    #         self$edit_options_field(dat[[self$def$id_form]], o)
    #       }
    #     }
    #     
    #   }
    #   
    # },
    
    #' @description Is the provided color(s) valid?
    is_color = function(colors){
      
      sapply(colors, function(x) {
        tryCatch(!is.na(x) && is.matrix(col2rgb(x)), 
                 error = function(e) FALSE)
      }, USE.NAMES = FALSE)
      
    },
    
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
    amend_options_colors = function(id_form, options){
      
      cur_color <- self$get_by_id(id_form) %>%
        dplyr::pull(!!sym(self$def[["colors"]]))
      
      cur_color <- self$from_json(cur_color)
      options <- self$from_json(options)
      
      if(length(cur_color) != length(options)){
        
        nc <- length(cur_color)
        n <- length(options) - nc
        
        new_cols <- as.list(rep(self$default_color,n))
        
        self$edit_options_colors(id_form, c(cur_color, new_cols))
        
      }
      
      
    },
    
    set_options_order = function(id_form, new_order){
      
      if(!is.null(names(new_order))){
        warning("Dropping names : set_options_order")
        names(new_order) <- NULL
      }
      
      if(length(new_order) > 0){
        new_order <- self$to_json(new_order)
      }
      
      self$replace_value_where(self$def_table, 
                               col_compare = self$def$id_form, 
                               val_compare = id_form,
                               col_replace = self$def$order_options, 
                               val_replace = new_order)
      
    },
    
    
    prepare_nested_choice_column = function(id_form, name, options){
      
      key <- setNames(list(options),name)
      
      value <- vector("list", length = length(options))
      names(value) <- names(key[[1]])
      
      lis <- list(key = key, value = value)
      
      self$edit_options_field(id_form, lis)
    },

    
    #' @description If new categories added, make sure the order vector is amended
    amend_options_order = function(id_form, options){
      
      cur_order <- self$get_by_id(id_form) %>%
        dplyr::pull(!!sym(self$def[["order_options"]]))
      
      cur_order <- self$from_json(cur_order)
      options <- self$from_json(options)
      
      if(length(cur_order) != length(options)){
        
        nc <- length(cur_order)
        n <- length(options) - nc
        
        new_order <- c(cur_order, (nc+1):(nc+n))
        self$set_options_order(id_form, new_order)
        
      }
      
    },
    
    edit_formulier_setup = function(new_setup){
      
      lapply(1:nrow(new_setup), function(x){
        
        id <- new_setup$id[x]
        side <- new_setup$side[x]
        order <- new_setup$order[x]
        
        qu <- glue::glue("UPDATE {self$schema_str}{self$def_table} SET {self$def$form_section} = '{side}', \"{self$def$order_field}\" = '{order}'  WHERE {self$def$id_form} = '{id}'")
        
        self$execute_query(qu)
        
      })
      
    },
    
    edit_zichtbaarheid_invoerveld = function(id_formfield, new_zichtbaarheid){
      
      qu <- glue::glue("UPDATE {self$schema_str}{self$def_table} SET {self$def$visible} = {new_zichtbaarheid} WHERE {self$def$id_form} = '{id_formfield}'")
      
      self$execute_query(qu)
      
    },
    
    edit_verwijder_datum = function(id_formfield, new_date){
      
      qu <- glue::glue("UPDATE {self$schema_str}{self$def_table} SET {self$def$date_deleted} = '{new_date}' WHERE {self$def$id_form} = '{id_formfield}'")
      
      self$execute_query(qu)
      
    },
    
    # hoeft alleen voor zichtbare velden
    amend_formfield_order = function(formside, deleted_number){
      
      qu <- glue::glue("UPDATE {self$schema_str}{self$def_table} SET \"{self$def$order_field}\" = \"{self$def$order_field}\" - 1 WHERE {self$def$form_section} = '{formside}' AND \"{self$def$order_field}\" > {deleted_number} AND {self$def$visible} = TRUE")
      
      self$execute_query(qu)
      
    },
    
    reset_volgorde_invoerveld = function(id_formfield, new_volgorde_nummer){

      qu <- glue::glue("UPDATE {self$schema_str}{self$def_table} SET \"{self$def$order_field}\" = '{new_volgorde_nummer}' WHERE {self$def$id_form} = '{id_formfield}'")

      self$execute_query(qu)
      
    },
    
    
    really_delete_formfield = function(id){
      
      tab <- glue::glue("{self$schema_str}{self$def_table}")
      
      qu <- glue::glue("delete from {tab} where {self$def$id_form} = '{id}'")
      self$execute_query(qu)
    },
    
    
    #' @description Database column type based on field input type
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
          
          if(!column_2 %in% cols_output){
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
    rename_registrations_intern = function(data){
      
      cols <- self$data_columns
      ii <- match(names(data), unlist(cols))
      mtch <- which(!is.na(ii))
      
      names(data)[mtch] <- names(cols)[ii[mtch]]
      
      data
    },
    
    
    
    write_new_registration = function(data, user_id, current_reg_id){ 
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
      
      res <- self$append_data(self$data_table, data_all)
      
      # TRUE if success (append_data has a try()) 
      return(!inherits(res, "try-error"))
    },
    
     
    edit_registration = function(old_data, new_data, user_id, current_reg_id){
      
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
      
      method <- match.arg(method)
      
      if(self$audit) {
        
        qu_audit <- glue::glue("insert into {self$schema_str}{self$audit_table} select * from ",
                               "{self$schema_str}{self$data_table} WHERE {self$data_columns$id} =?val_compare;")  
 
        audit_query <- sqlInterpolate(DBI::ANSI(), qu_audit,  val_compare = registration_id) 
        
        self$execute_query(audit_query)
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
        self$replace_value_where(self$data_table, col_replace = "deleted", val_replace = flag,
                                 col_compare = self$data_columns$id, val_compare = registration_id)  
        
      } else if(method == "hard"){
        
        self$delete_rows_where(self$data_table, col_compare = self$data_columns$id, val_compare = registration_id)
        
        # TODO
        # audit en relations ook kunnen verwijderen voor AVG
      }
      
    },
    

    #' @description Read registrations, recode select values where needed.
    #' @param recode If TRUE (default), replaces codes with labels (for select fields)
    read_registrations = function(recode = TRUE, 
                                  include_deleted = TRUE,
                                  deleted_only = FALSE,
                                  lazy = FALSE
                                  ){
      
      data <- self$read_table(self$data_table, lazy = TRUE)
      
      if(deleted_only){
        data <- filter(data, deleted == 1)
      } else if(!include_deleted){
        data <- filter(data, deleted == 0)
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
        filter(!!sym(self$def[["type_field"]]) %in% c("singleselect","nestedselect"),
               !!sym(self$def[["visible"]]),
               !!sym(self$def[["column_field"]]) %in% !!names(data)) %>%
        collect
      
      # for every select field, replace values
      for(i in seq_len(nrow(def))){
        
        opt <- def[[self$def$options]][i]
        
        key <- self$from_json(opt)
        
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
        
        if(length(key)){
          
          val <- lapply(data[[col]], function(x)if(x %in% c("[]","{}"))NA_character_ else self$from_json(x))
          
          new_val <- sapply(val, function(x)self$to_json(dplyr::recode(x, !!!key)), USE.NAMES = FALSE)
          new_val[new_val == "NA"] <- NA
          
          data[[col]] <- new_val
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
    
    
    filter_period = function(data, 
                             date_start = Sys.Date()-7, 
                             date_end = Sys.Date()){
      
      timecol <- self$data_columns$time_created
      
      if(!timecol %in% names(data)){
        stop("$filter_period works only on data returned with $read_registrations")
      }
      dplyr::filter(data, between(as.Date(!!sym(timecol)), date_start, date_end))
      
    },
    
    read_definition = function(...){
      
      self$read_table(self$def_table, ...)
      
    },
    
    get_occurences = function(table, column, record){
      
      qu <- glue::glue("SELECT * FROM {self$schema_str}{table} WHERE {column} = '{record}'")  

      res <- dbGetQuery(self$con, qu)
      
      return(res)
      
    },
    
    get_registration_by_json = function(table, column, record){

      qu <- glue::glue("SELECT * FROM {self$schema_str}{table} WHERE {column}::jsonb ? '{record}'")

      res <- dbGetQuery(self$con, qu)
      
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
    
    create_timeseries = function(columns=NULL, table=NULL){
 
      selected_columns <- unique( c(columns,
                                    self$data_columns$id,  
                                    self$data_columns$name, 
                                    self$data_columns$user, 
                                    self$data_columns$time_modified))
      
      if(is.null(table)){
        if(!is.null(columns)) {
          huidige_data <-  self$read_table(self$data_table, lazy=TRUE) %>%
            select(selected_columns) %>%
              collect
        } else {
          huidige_data <-  self$read_table(self$data_table)
        }
       
    } else if(!is.null(columns)){
      huidige_data <-  table %>%
        select(selected_columns)  
    } else { 
      huidige_data <- table
    }
    
    if(!is.null(columns)){
      
      historische_data <- self$read_table(self$audit_table, lazy=TRUE) %>%
        select(selected_columns) %>%
        collect
    } else {  
      historische_data <- self$read_table(self$audit_table)
    }
    
        return(rbind(huidige_data, historische_data))
  },

  # functie om een audit table om te zetten in een event log
  # creation only filterd alleen de aanmaak events en dus geen updates
  create_events = function(timeseries, creation_only=FALSE){
       
 
    timeseries$auditstamp <- timeseries[[self$data_columns$time_modified]]
    timeseries$auditstamp <- as.POSIXct( timeseries$auditstamp, tz = "UTC" ) 
    # outer loop over all unique ID's
    all_mutations <- lapply(unique(timeseries[[self$data_columns$id]]), function(i){
      
        # i is nu een uniek ID voor een registratie 
        timeseries_for_id <- timeseries %>% 
                              filter(!!as.symbol(self$data_columns$id)==i) %>% 
                              arrange(auditstamp)
         
        
        
        # alle creates voor ID 
        aanmaak_regel <- timeseries_for_id[1,] %>% 
          mutate( type='C', 
                  variable=NA,
                  old_val=NA, 
                  new_val=NA) %>% 
          select(self$data_columns$id,  
                 self$data_columns$name, 
                 self$data_columns$user, 
                 self$data_columns$time_modified,
                 type,  
                 variable,
                 old_val,
                 new_val)
        
        # er is maar 1 rij en dat is dan de aanmaak regel ...
        if(nrow(timeseries_for_id) ==1 | creation_only){
          return(aanmaak_regel) 
        }  
        
        
        # alle Updates voor IDb
        # inner loop over all mutations for an ID 
        mutations_for_id <- lapply(1:(nrow(timeseries_for_id)-1), function(time_index){
          # time_index is nu de index waarop de old row is gemuteerd naar de new_row
          
          # get (old)row at time_index and (new)row at time_index+1
          old_row <- timeseries_for_id[time_index,] 
          new_row <- timeseries_for_id[time_index+1,] 
          
          
          # calculate changes
          # note -> we dont care if only time_modified or user changes.
          suppressWarnings({ 
            differences <- diffdf::diffdf(new_row %>% select(-self$data_columns$user, 
                                                             -self$data_columns$time_modified,
                                                             -auditstamp),
                                          old_row %>% select(-self$data_columns$user, 
                                                             -self$data_columns$time_modified,
                                                             -auditstamp)) 
          })
          # loop over changes (minus NumDiff want die is niet relevant)
          point_mutations <- lapply(attr(differences, "names")[attr(differences, "names") != 'NumDiff'] , function(changed_attri){ 
            return(differences[[changed_attri]]) 
          })
          
          # samenvoegen en checken of de data nog klopt.
          df <- as.data.frame(do.call(rbind, point_mutations) )
          if(nrow(df) <= 0){
            return(NA)
          }  
          # formatteren
          # let op voor een wijziging gebruiken we expres het label van de oude rij, 
          # maar user+time_modified van de nieuwe rij. 
          df_formatted <- df %>% mutate(old_val = as.character(COMPARE),
                                        new_val=as.character(BASE),
                                        !!self$data_columns$id := old_row[[self$data_columns$id]],
                                        #type=case_when(VARIABLE == 'verwijderd' ~ 'D', DOORONTWIKKELING? 
                                        #                TRUE ~ 'U'),                   DOORONTWIKKELING? 
                                        type='U',
                                        variable=VARIABLE,
                                        !!self$data_columns$name := old_row[[self$data_columns$name]],
                                        !!self$data_columns$user := new_row[[self$data_columns$user]],
                                        !!self$data_columns$time_modified := new_row[[self$data_columns$time_modified]]) %>% 
            
            select(self$data_columns$id,  
                   self$data_columns$name, 
                   self$data_columns$user, 
                   self$data_columns$time_modified,
                   type,  
                   variable,
                   old_val,
                   new_val) 
          
          return(df_formatted)
          
          
        })    
        # return one dataframe with all mutations for id
        
        wijzigingen <- mutations_for_id[!is.na(mutations_for_id)]
        wijziging_regels <- as.data.frame(do.call(rbind, wijzigingen))
        
        return(rbind(wijziging_regels, aanmaak_regel)) 
     
            
      })  
      # return one dataframe with all mutations for all p_ids
      return(as.data.frame(do.call(rbind, all_mutations)))
      
  }, 
  
  ## Relations
  get_all_relations = function(){

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
    collector_type = ifelse(is.na(collector_type), self$class_type, collector_type)

    # Build query parts
    V <- ifelse(filter_verwijderd, glue(" AND {self$relation_columns$verwijderd} = false"), "")
    A <- ifelse(is.na(object_type), "", glue(" AND {self$relation_columns$object_type} = '{object_type}'"))
    B <- ifelse(is.na(collector_type), "", glue(" AND {self$relation_columns$collector_type} = '{collector_type}'"))
    C <- ifelse(is.na(relation_id),"", glue(" AND {self$relation_columns$relation_id} = '{relation_id}'"))
    D <- ifelse(is.na(relation_type), "", glue(" AND {self$relation_columns$relation_type} = '{relation_type}'"))
    
    qu <- glue::glue("SELECT * FROM {self$schema_str}{self$relation_table} WHERE {self$relation_columns$object_id} = '{object_id}'{V}{A}{B}{C}{D};")

    self$query(qu)
    
  },
  
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
    
    tm <- try({
      self$query("select now()", quiet = TRUE)$now  
    }, silent = TRUE)
    
    if(inherits(tm, "try-error")){
      return(Sys.time())
    }
    
    tm
    
  },

  #' @description Select rows in registration audit table since some timestamp
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
        qu_audit <- glue::glue("insert into {self$schema_str}{self$relation_audit_table} select * from ",
                                 "{self$schema_str}{self$relation_table} WHERE {self$relation_columns$id} in ('{paste(ids, collapse=\"','\")}');") %>% as.character() 

        self$execute_query(qu_audit)
      }
      
      # delete all relations that are ALTERED or DELETED
      qu_delete <- glue::glue("DELETE FROM {self$schema_str}{self$relation_table} WHERE {self$relation_columns$id} in ('{paste(ids, collapse=\"','\")}');")

      self$execute_query(qu_delete)
    }
  },  

  #' @description Append relations
  #' @param data Data to append 
  write_new_relations = function(data, registration_id){ 
    
    postgres_time <- self$postgres_now()

    # Voor postgres GEBRUIK now()::timestamp
    data <- data %>%
      mutate(collector_id = registration_id,
             timestamp = format(postgres_time),
             collector_type = ifelse(is.na(collector_type), self$class_type, collector_type))


    self$append_data(self$relation_table,
                     data)
    
  },
  
  #' @description update current relation table for registration ID
  #' @param new_relations The current state that should end up in the relations table 
  update_relations = function(new_relations, registration_id){ 
 
    qu <- glue::glue("SELECT * FROM {self$schema_str}{self$relation_table} WHERE {self$relation_columns$collector_id} = '{registration_id}';")

    old_relations <- dbGetQuery(self$con, qu)
     
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
      left_join(new_relations, by='id', suffix=c("", "_new"))  %>% 
      filter((comment_new != comment & status_new != status) | is.na(collector_id_new)) %>%
      mutate(verwijderd = ifelse(is.na(collector_id_new), TRUE, verwijderd)) %>%
      select(!ends_with('_new'))
 
    # archive all relations that are ALTERED or DELETED   
    self$soft_delete_relations(altered_or_deleted$id) 
    
    # append all relations that are NEW or ALTERED
    new  <- new_relations %>% 
      left_join(old_relations, by='id', suffix=c("", "_old"))%>% 
      filter(is.na(status_old)) %>%
      select(!ends_with('_old'))
 
    new_data <- dplyr::bind_rows(new, altered_or_deleted)
    if(nrow(new_data) > 0){
      self$write_new_relations(new_data, registration_id=registration_id)
    }
     
    return(TRUE)
  
  } 
  )
   
  
)
 
