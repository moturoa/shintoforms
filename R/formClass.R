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
    #' @param table Table in DB (in schema) that holds form config
    #' @param pool If TRUE, connects with dbPool
    #' @param sqlite If path to an SQLite file, uses SQLite.
    #' @param default_color Default color
    initialize = function(config_file = "conf/config.yml", 
                          what,
                          schema = NULL,
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
                            removable = "kan_worden_verwijderd"
                          ),
                          data_table = "registrations",
                          data_columns = list(
                            id = "id_registratie",
                            name = "naam_registratie",
                            time_created = "invoerdatum",
                            time_modified = "wijzigdatum",
                            user = "user_id"
                          ),
                          pool = TRUE,
                          sqlite = NULL,
                          default_color = "#3333cc"){
      
      self$default_color <- default_color
      self$connect_to_database(config_file, schema, what, pool, sqlite)
      
      self$def_table <- def_table
      self$def <- def_columns

      self$data_table <- data_table
      self$data_columns <- data_columns
      
      # Check
      defcols <- self$table_columns(self$def_table)
      di1 <- unlist(self$def) %in% defcols
      if(any(!di1)){
        stop(paste("Columns in def_columns not found:", paste(defcols[!di1], collapse=",")))
      }
      
      datacols <- self$table_columns(self$data_table)
      di2 <- unlist(self$data_columns) %in% datacols
      
      if(any(!di2)){
        stop(paste("Columns in data_columns not found:", paste(datacols[!di2], collapse=",")))
      }
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
        
        cf <- config::get(what, file = config_file)
        
        print("----CONNECTING TO----")
        print(cf$dbhost)
        
        self$dbuser <- cf$dbuser
        
        if(pool){
          flog.info("pool::dbPool", name = "DBR6")
          response <- try(pool::dbPool(RPostgres::Postgres(),
                                       dbname = cf$dbname,
                                       host = cf$dbhost,
                                       port = cf$dbport,
                                       user = cf$dbuser,
                                       password = cf$dbpassword,
                                       minSize = 1,
                                       maxSize = 25,
                                       idleTimeout = 60*60*1000))
        } else {
          flog.info("DBI::dbConnect", name = "DBR6")
          response <- try(DBI::dbConnect(RPostgres::Postgres(),
                                         dbname = cf$dbname,
                                         host = cf$dbhost,
                                         port = cf$dbport,
                                         user = cf$dbuser,
                                         password = cf$dbpassword))
        }
        
        if(!inherits(response, "try-error")){
          self$con <- response
        }
        
        
        self$dbtype <- "postgres"
        
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
      
      if(is.null(self$schema)){
        qu <- glue::glue("alter table {table} add column {column} {type}")
      } else {
        qu <- glue::glue("alter table {self$schema}.{table} add column {column} {type}")  
      }
      
      dbExecute(self$con, qu)
      
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
    
    table_columns = function(table){
      
      if(is.null(self$schema)){
        names(self$query(glue("select * from {table} where false")))
      } else {
        names(self$query(glue("select * from {self$schema}.{table} where false")))
      }
      
      
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
    
    has_value = function(table, column, value){
      
      if(!is.null(self$schema)){
        out <- self$query(glue("select {column} from {self$schema}.{table} where {column} = '{value}' limit 1"))  
      } else {
        out <- self$query(glue("select {column} from {table} where {column} = '{value}' limit 1"))  
      }
      
      nrow(out) > 0
    },
    
    
    # set verwijderd=1 where naam=gekozennaam.
    # replace_value_where("table", 'verwijderd', 'true', 'naam', 'gekozennaam')
    replace_value_where = function(table, col_replace, val_replace, col_compare, val_compare,
                                   query_only = FALSE, quiet = FALSE){
      
      if(!is.null(self$schema)){
        query <- glue("update {self$schema}.{table} set {col_replace} = ?val_replace where ",
                      "{col_compare} = ?val_compare") %>% as.character()  
      } else {
        query <- glue("update {table} set {col_replace} = ?val_replace where ",
                      "{col_compare} = ?val_compare") %>% as.character()
      }
      
      query <- sqlInterpolate(DBI::ANSI(), 
                              query, 
                              val_replace = val_replace, val_compare = val_compare)
      
      if(query_only)return(query)
      
      # if(!quiet){
      #   flog.info(query, name = "DBR6")  
      # }
      
      dbExecute(self$con, query)
      
    },
    
    
    #----- Form registration methods
    
    
    #' @description Get a row from the form definition by the form id.
    get_by_id = function(id_form){
      
      self$read_table(self$def_table, lazy = TRUE) %>%
        dplyr::filter(!!sym(self$def[["id_form"]]) == !!id_form) %>%
        collect
      
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
      
      out
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
      
      if(!is.null(self$schema)){
        qu <- glue::glue("SELECT * FROM {self$schema}.{self$def_table} WHERE {self$def$form_section} = {form_section} AND {self$def$visible} = TRUE")
      } else {
        qu <- glue::glue("SELECT * FROM {self$def_table} WHERE {self$def$form_section} = {form_section} AND {self$def$visible} = TRUE")
      }
      
      out <- dbGetQuery(self$con, qu)
      out <- out[order(out[[self$def$order_field]]),]
      
      self$rename_definition_table(out)
      
    },
    
    # Alleen zichtbare velden hoeven een volgorde nummer te hebben en moeten worden meegenomen.
    get_next_formorder_number = function(form_section){
      
      if(!is.null(self$schema)){
        qu <- glue::glue("SELECT COUNT(DISTINCT {self$def$id_form}) FROM {self$schema}.{self$def_table} WHERE {self$def$form_section} = {form_section} AND {self$def$visible} = TRUE")
      } else {
        qu <- glue::glue("SELECT COUNT(DISTINCT {self$def$id_form}) FROM {self$def_table} WHERE {self$def$form_section} = {form_section} AND {self$def$visible} = TRUE")
      }
      
      count <- dbGetQuery(self$con, qu)
      return(count[[1]]+1)
      
    },
    
    #' @description Adds an input field to a form
    #' @param label_field Label for the field
    #' @param type_field Type of field (options : TODO)
    #' @param form_section Left or right
    add_input_field_to_form = function(label_field, type_field, form_section){
      

      assert_input_field_type(type_field)
      
      id <- uuid::UUIDgenerate()
      
      # Sanitize column name
      kol_nm_veld <- janitor::make_clean_names(tolower(label_field), parsing_option = 1)
      
      if(self$check_uniqueness_column_name(kol_nm_veld)){
        
        volg_veld <- self$get_next_formorder_number(form_section)
        
        if(type_field == "boolean"){
          opties <- '{"1":"Ja","2":"Nee"}'
          #volgorde <- self$to_json('[1,2]')
        } else {
          opties <- "[]"
          #volgorde <- "[]"
        }
        
        data <- data.frame(
          id_form = id,
          column_field = kol_nm_veld,
          label_field = label_field,
          type_field = type_field,
          order_field = volg_veld,
          form_section = form_section,
          visible = TRUE,
          options = opties,
          order_options = "[]",
          colors = '[]',
          removable = TRUE
        )
        
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
        
      } else {
        
        
        return(-1)
        
        # dit mag niet
        # goed voorbeeld van onion model idee fout.
        # deze methode weet niet dat toastr beschikbaar is 
        #toastr_error("Dit label zorgt voor een kolomnaam die al bestaat. Voer een ander label in.")
      }
      
      
    },
    
    check_uniqueness_column_name =  function(column){
      
      if(!is.null(self$schema)){
        qu <- glue::glue("SELECT * FROM {self$schema}.{self$def_table} WHERE {self$def$column_field} = '{column}'")
      } else {
        qu <- glue::glue("SELECT * FROM {self$def_table} WHERE {self$def$column_field} = '{column}'")
      }
      
      res <- dbGetQuery(self$con, qu)
      return(nrow(res)==0)
      
    },
    
    
    #' @description Edit the label for an input field
    edit_label_field = function(id_form, new_label){
      
      self$replace_value_where(self$def_table, col_compare = self$def$id_form, val_compare = id_form,
                               col_replace = self$def$label_field, 
                               val_replace = new_label)
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
      
      if(length(new_order) > 1){
        new_order <- self$to_json(new_order)
      }
      
      self$replace_value_where(self$def_table, 
                               col_compare = self$def$id_form, 
                               val_compare = id_form,
                               col_replace = self$def$order_options, 
                               val_replace = new_order)
      
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
        
        if(!is.null(self$schema)){
          qu <- glue::glue("UPDATE {self$schema}.{self$def_table} SET {self$def$form_section} = '{side}', \"{self$def$order_field}\" = '{order}'  WHERE {self$def$id_form} = '{id}'")
        } else {
          qu <- glue::glue("UPDATE {self$def_table} SET {self$def$form_section} = '{side}', \"{self$def$order_field}\" = '{order}'  WHERE {self$def$id_form} = '{id}'")
        }
        
        dbExecute(self$con, qu)
        
      })
      
    },
    
    edit_zichtbaarheid_invoerveld = function(id_formfield, new_zichtbaarheid){
      
      if(!is.null(self$schema)){
        qu <- glue::glue("UPDATE {self$schema}.{self$def_table} SET {self$def$visible} = {new_zichtbaarheid} WHERE {self$def$id_form} = '{id_formfield}'")
      } else {
        qu <- glue::glue("UPDATE {self$def_table} SET {self$def$visible} = {new_zichtbaarheid} WHERE {self$def$id_form} = '{id_formfield}'")
      }
      
      dbExecute(self$con, qu)
      
    },
    
    edit_verwijder_datum = function(id_formfield, new_date){
      
      if(!is.null(self$schema)){
        qu <- glue::glue("UPDATE {self$schema}.{self$def_table} SET {self$def$date_deleted} = '{new_date}' WHERE {self$def$id_form} = '{id_formfield}'")
      } else {
        qu <- glue::glue("UPDATE {self$def_table} SET {self$def$date_deleted} = '{new_date}' WHERE {self$def$id_form} = '{id_formfield}'")
      }
      
      dbExecute(self$con, qu)
      
    },
    
    # hoeft alleen voor zichtbare velden
    amend_formfield_order = function(formside, deleted_number){
      
      if(!is.null(self$schema)){
        qu <- glue::glue("UPDATE {self$schema}.{self$def_table} SET \"{self$def$order_field}\" = \"{self$def$order_field}\" - 1 WHERE {self$def$form_section} = '{formside}' AND \"{self$def$order_field}\" > {deleted_number} AND {self$def$visible} = TRUE")
      } else {
        qu <- glue::glue("UPDATE {self$def_table} SET \"{self$def$order_field}\" = \"{self$def$order_field}\" - 1 WHERE {self$def$form_section} = '{formside}' AND \"{self$def$order_field}\" > {deleted_number} AND {self$def$visible} = TRUE")
      }
      
      dbExecute(self$con, qu)
      
    },
    
    reset_volgorde_invoerveld = function(id_formfield, new_volgorde_nummer){
      if(!is.null(self$schema)){
        qu <- glue::glue("UPDATE {self$schema}.{self$def_table} SET \"{self$def$order_field}\" = '{new_volgorde_nummer}' WHERE {self$def$id_form} = '{id_formfield}'")
      } else {
        qu <- glue::glue("UPDATE {self$def_table} SET \"{self$def$order_field}\" = '{new_volgorde_nummer}' WHERE {self$def$id_form} = '{id_formfield}'")
      }
      
      dbExecute(self$con, qu)
      
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
               "text"   # if not in list
        )
      } else {
        stop("DB type not supported (column_type_from_field_type)")
      }
      
    },
    
    #' @description Make sure the necessary columns are present in the data_table
    prepare_data_table = function(){
      
      tab <- self$get_input_fields()
      
      cols_output <- self$table_columns(self$data_table)
      
      for(i in seq_len(nrow(tab))){
        
        if(!tab$column_field[i] %in% cols_output){
          
          new_col_type <- self$column_type_from_field_type(tab$type_field[i])
          
          flog.info(glue("Adding column: {tab$column_field[i]}, type : {new_col_type}"))
          self$make_column(self$data_table, tab$column_field[i], new_col_type)
          
        } else {
          
          # do nothing.
          # TODO maybe check that even though the column exists it has the right type
          
        }
        
      }
      
    },
    
    write_new_registration = function(data, user_id){
      
      self$prepare_data_table()
      
      data_pre <- data.frame(
        id = uuid::UUIDgenerate(),
        time_created = format(Sys.time()),
        time_modified = format(Sys.time()),
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
    
    read_registrations = function(recode = TRUE){
      
      data <- self$read_table(self$data_table)
      
      if(recode){
        
        # find select fields. they will be recoded with the actual values
        def <- self$read_definition() %>% filter(
          !!sym(self$def[["type_field"]]) %in% c("multiselect","singleselect"),
          !!sym(self$def[["column_field"]]) %in% names(data))
        
        # for every select field, replace values
        for(i in seq_len(nrow(def))){
          opt <- def[[self$def$options]][i]
          key <- self$from_json(opt)
          col <- def[[self$def$column_field]][i]
          data[[col]] <- dplyr::recode(data[[col]], !!!key)
        }
        
      }
      
      data
      
    },
    
    read_definition = function(...){
      
      self$read_table(self$def_table, ...)
      
    }
    
  )
  
)
