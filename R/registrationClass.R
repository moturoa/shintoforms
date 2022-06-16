#' R6 Class voor Registratie formulier
#' @importFrom R6 R6Class
#' @export
registrationClass <- R6::R6Class(
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
                          table = "formulier_velden",
                          pool = TRUE,
                          sqlite = NULL,
                          default_color = "#3333cc"){
      
      self$default_color <- default_color
      self$connect_to_database(config_file, schema, what, pool, sqlite)
      self$table <- table
      
    },
    
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
    
    #' @description Get form input fields
    get_input_fields = function(zichtbaarheid = TRUE){
      
      if(!is.null(self$schema)){
        qu <- glue::glue("SELECT * FROM {self$schema}.{self$table} WHERE zichtbaar = {zichtbaarheid}")
      } else {
        qu <- glue::glue("SELECT * FROM {self$table} WHERE zichtbaar = {zichtbaarheid}")
      }
      DBI::dbGetQuery(self$con, qu)
    },
    
    
    # Alleen zichtbare velden hoeven een volgorde nummer te hebben en moeten worden meegenomen.
    get_next_formorder_number = function(kant_formulier = c("links", "rechts")){
      if(!is.null(self$schema)){
        qu <- glue::glue("SELECT COUNT(DISTINCT id_formulierveld) FROM {self$schema}.{self$table} WHERE formulier_kant = '{kant_formulier}' AND zichtbaar = TRUE")
      } else {
        qu <- glue::glue("SELECT COUNT(DISTINCT id_formulierveld) FROM {self$table} WHERE formulier_kant = '{kant_formulier}' AND zichtbaar = TRUE")
      }
      
      count <- dbGetQuery(self$con, qu)
      return(count+1)
      
    },
    
    add_input_field_to_form = function(label_veld, type_veld, formulier_kant){
      
      id <- uuid::UUIDgenerate()
      
      kol_nm_veld <- janitor::make_clean_names(tolower(label_veld), parsing_option = 1)
      
      # ?
      if(!is.na(as.numeric(substr(kol_nm_veld, 1,1)))){
        kol_nm_veld <- paste0("x",kol_nm_veld)
      }
      
      if(self$check_uniqueness_column_name(kol_nm_veld)){
        lbl_veld <- label_veld
        tp_veld <- type_veld
        volg_veld <- self$get_next_formorder_number(formulier_kant)
        form_kant <- formulier_kant
        
        if(type_veld == "boolean"){
          opties <- '{"1":"Ja","2":"Nee"}'
          #volgorde <- self$to_json('[1,2]')
        } else {
          opties <- "[]"
          #volgorde <- "[]"
        }
        
        if(!is.null(self$schema)){
          qu <- glue::glue("INSERT INTO {self$schema}.{self$table}(id_formulierveld, kolomnaam_veld, label_veld, type_veld, volgorde_veld, formulier_kant, zichtbaar, opties, volgorde_opties, kleuren, kan_worden_verwijderd) VALUES('{id}', '{kol_nm_veld}', '{lbl_veld}', '{tp_veld}', '{volg_veld}', '{form_kant}', TRUE, '{opties}', '[]', '[]', TRUE) ")
        } else {
          qu <- glue::glue("INSERT INTO {self$table}(id_formulierveld, kolomnaam_veld, label_veld, type_veld, volgorde_veld, formulier_kant, zichtbaar, opties, volgorde_opties, kleuren, kan_worden_verwijderd) VALUES('{id}', '{kol_nm_veld}', '{lbl_veld}', '{tp_veld}', '{volg_veld}', '{form_kant}', TRUE, '{opties}', '[]', '[]', TRUE) ")
        }
        
        DBI::dbExecute(self$con, qu)
        
        if(type_veld == "boolean"){
          self$amend_optie_order(id, opties)
          self$amend_optie_colors(id, opties)
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
    
    check_uniqueness_column_name =  function(kolomnaam){
      
      if(!is.null(self$schema)){
        qu <- glue::glue("SELECT * FROM {self$schema}.{self$table} WHERE kolomnaam_veld = '{kolomnaam}'")
      } else {
        qu <- glue::glue("SELECT * FROM {self$table} WHERE kolomnaam_veld = '{kolomnaam}'")
      }
      
      res <- dbGetQuery(self$con, qu)
      return(nrow(res)==0)
      
    },
    
    
    edit_label_field = function(id_formveld, new_label){
      if(!is.null(self$schema)){
        qu <- glue::glue("UPDATE {self$schema}.{self$table} SET label_veld = '{new_label}' WHERE id_formulierveld = '{id_formveld}'")
      } else {
        qu <- glue::glue("UPDATE {self$table} SET label_veld = '{new_label}' WHERE id_formulierveld = '{id_formveld}'")
      }
      dbExecute(self$con, qu)
      
    },
    
    from_json = function(x, ...){
      
      shintocatman::from_json(x, ...)
      
    },
    
    to_json = function(x, ...){
      
      shintocatman::to_json(x, ...)
      
    },
    
    edit_opties_invulveld = function(id_formfield, new_opties){
      
      if(!is.character(new_opties)){
        new_opties <- self$to_json(new_opties)  
      }
      
      if(is.null(names(self$from_json(new_opties)))){
        stop("JSON new_opties MUST have names (1:n) (edit_opties_invulveld)")
      }
      
      if(!is.null(self$schema)){
        qu <- glue::glue("UPDATE {self$schema}.{self$table} SET opties = '{new_opties}' WHERE id_formulierveld = '{id_formfield}'")
      } else {
        qu <- glue::glue("UPDATE {self$table} SET opties = '{new_opties}' WHERE id_formulierveld = '{id_formfield}'")
      }
      
      dbExecute(self$con, qu)
      
      
    },
    
    is_color = function(colors){
      
      sapply(colors, function(x) {
        tryCatch(!is.na(x) && is.matrix(col2rgb(x)), 
                 error = function(e) FALSE)
      }, USE.NAMES = FALSE)
      
    },
    
    set_optie_colors = function(id_formfield, new_colors){
      
      if(any(!self$is_color(new_colors))){
        stop("Invalid colors!")
      }
      
      new_colors <- self$to_json(as.list(new_colors) %>% setNames(1:length(new_colors)))
      
      if(!is.null(self$schema)){
        qu <- glue::glue("UPDATE {self$schema}.{self$table} SET kleuren = '{new_colors}' WHERE id_formulierveld = '{id_formfield}'")
      } else {
        qu <- glue::glue("UPDATE {self$table} SET kleuren = '{new_colors}' WHERE id_formulierveld = '{id_formfield}'")
      }
      
      dbExecute(self$con, qu)
      
    },
    
    #' @description If new options added, make sure the color vector is amended
    amend_optie_colors = function(id_formfield, opties){
      
      if(!is.null(self$schema)){
        qu <- glue::glue("SELECT kleuren FROM {self$schema}.{self$table} WHERE id_formulierveld = '{id_formfield}'")
      } else {
        qu <- glue::glue("SELECT kleuren FROM {self$table} WHERE id_formulierveld = '{id_formfield}'")
      }
      
      cur_color <- dbGetQuery(self$con, qu) %>%
        pull(kleuren)
      
      cur_color <- self$from_json(cur_color)
      opties <- self$from_json(opties)
      
      if(length(cur_color) != length(opties)){
        
        nc <- length(cur_color)
        n <- length(opties) - nc
        
        new_cols <- as.list(rep(self$default_color,n))
        
        self$set_optie_colors(id_formfield, c(cur_color, new_cols))
        
      }
      
      
    },
    
    set_optie_order = function(id_formfield, new_order){
      
      if(!is.null(names(new_order))){
        warning("Dropping names : set_optie_order")
        names(new_order) <- NULL
      }
      
      if(length(new_order) > 1){
        new_order <- self$to_json(new_order)
      }
      
      if(!is.null(self$schema)){
        qu <- glue::glue("UPDATE {self$schema}.{self$table} SET volgorde_opties = '{new_order}' WHERE id_formulierveld = '{id_formfield}'")
      } else {
        qu <- glue::glue("UPDATE {self$table} SET volgorde_opties = '{new_order}' WHERE id_formulierveld = '{id_formfield}'")
      }
      
      dbExecute(self$con, qu)
      
    },
    
    #' @description If new categories added, make sure the order vector is amended
    amend_optie_order = function(id_formfield, opties){
      
      if(!is.null(self$schema)){
        qu <- glue::glue("SELECT volgorde_opties FROM {self$schema}.{self$table} WHERE id_formulierveld = '{id_formfield}'")
      } else {
        qu <- glue::glue("SELECT volgorde_opties FROM {self$table} WHERE id_formulierveld = '{id_formfield}'")
      }
      
      cur_order <- dbGetQuery(self$con, qu) %>%
        pull(volgorde_opties)
      
      cur_order <- self$from_json(cur_order)
      opties <- self$from_json(opties)
      
      if(length(cur_order) != length(opties)){
        
        nc <- length(cur_order)
        n <- length(opties) - nc
        
        new_order <- c(cur_order, (nc+1):(nc+n))
        .reg$set_optie_order(id_formfield, new_order)
        
      }
      
      
    },
    
    edit_formulier_setup = function(new_setup){
      
      lapply(1:nrow(new_setup), function(x){
        
        id <- new_setup$id[x]
        side <- new_setup$side[x]
        order <- new_setup$order[x]
        
        if(!is.null(self$schema)){
          qu <- glue::glue("UPDATE {self$schema}.{self$table} SET formulier_kant = '{side}', volgorde_veld = '{order}'  WHERE id_formulierveld = '{id}'")
        } else {
          qu <- glue::glue("UPDATE {self$table} SET formulier_kant = '{side}', volgorde_veld = '{order}'  WHERE id_formulierveld = '{id}'")
        }
        
        dbExecute(self$con, qu)
        
      })
      
    },
    
    edit_zichtbaarheid_invoerveld = function(id_formfield, new_zichtbaarheid){
      
      if(!is.null(self$schema)){
        qu <- glue::glue("UPDATE {self$schema}.{self$table} SET zichtbaar = {new_zichtbaarheid} WHERE id_formulierveld = '{id_formfield}'")
      } else {
        qu <- glue::glue("UPDATE {self$table} SET zichtbaar = {new_zichtbaarheid} WHERE id_formulierveld = '{id_formfield}'")
      }
      
      dbExecute(self$con, qu)
      
    },
    
    edit_verwijder_datum = function(id_formfield, new_date){
      
      if(!is.null(self$schema)){
        qu <- glue::glue("UPDATE {self$schema}.{self$table} SET datum_uitgeschakeld = '{new_date}' WHERE id_formulierveld = '{id_formfield}'")
      } else {
        qu <- glue::glue("UPDATE {self$table} SET datum_uitgeschakeld = '{new_date}' WHERE id_formulierveld = '{id_formfield}'")
      }
      
      dbExecute(self$con, qu)
      
    },
    
    # hoeft alleen voor zichtbare velden
    amend_formfield_order = function(formside, deleted_number){
      
      if(!is.null(self$schema)){
        qu <- glue::glue("UPDATE {self$schema}.{self$table} SET volgorde_veld = volgorde_veld - 1 WHERE formulier_kant = '{formside}' AND volgorde_veld > {deleted_number} AND zichtbaar = TRUE")
      } else {
        qu <- glue::glue("UPDATE {self$table} SET volgorde_veld = volgorde_veld - 1 WHERE formulier_kant = '{formside}' AND volgorde_veld > {deleted_number} AND zichtbaar = TRUE")
      }
      
      dbExecute(self$con, qu)
      
    },
    
    reset_volgorde_invoerveld = function(id_formfield, new_volgorde_nummer){
      if(!is.null(self$schema)){
        qu <- glue::glue("UPDATE {self$schema}.{self$table} SET volgorde_veld = '{new_volgorde_nummer}' WHERE id_formulierveld = '{id_formfield}'")
      } else {
        qu <- glue::glue("UPDATE {self$table} SET volgorde_veld = '{new_volgorde_nummer}' WHERE id_formulierveld = '{id_formfield}'")
      }
      
      dbExecute(self$con, qu)
      
    },
    
    get_velden_form = function(kant_formulier = c("links", "rechts")){
      
      if(!is.null(self$schema)){
        qu <- glue::glue("SELECT * FROM {self$schema}.{self$table} WHERE formulier_kant = '{kant_formulier}' AND zichtbaar = TRUE")
      } else {
        qu <- glue::glue("SELECT * FROM {self$table} WHERE formulier_kant = '{kant_formulier}' AND zichtbaar = TRUE")
      }
      
      result <- dbGetQuery(self$con, qu)
      result <- result[order(result$volgorde_veld),]
      return(result)
      
    }
    
    
  )
  
)
