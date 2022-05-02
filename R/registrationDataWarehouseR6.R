registrationDataWarehouseR6 <- R6::R6Class(
  
  public = list(
    
    con = NULL,
    schema = NULL,
    dbname = NULL,
    dbuser = NULL,
    pool = NULL,
    dbtype = NULL,
    
    initialize = function(config_file = "conf/config.yml", 
                          what,
                          schema = NULL,
                          pool = TRUE,
                          sqlite = NULL){

      
      self$connect_to_database(config_file, schema, what, pool, sqlite)
      
    },
    
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
    
    get_invulvelden = function(zichtbaarheid = TRUE){
      
      if(!is.null(self$schema)){
        qu <- glue::glue("SELECT * FROM {self.schema}.formulier_velden WHERE zichtbaar = {zichtbaarheid}")
      } else {
        qu <- glue::glue("SELECT * FROM formulier_velden WHERE zichtbaar = {zichtbaarheid}")
      }
      dbGetQuery(self$con, qu)
    },
    
    get_next_formorder_number = function(kant_formulier = c("links", "rechts")){
      if(!is.null(self$schema)){
        qu <- glue::glue("SELECT COUNT(DISTINCT id_formulierveld) FROM {self.schema}.formulier_velden WHERE formulier_kant = '{kant_formulier}'")
      } else {
        qu <- glue::glue("SELECT COUNT(DISTINCT id_formulierveld) FROM formulier_velden WHERE formulier_kant = '{kant_formulier}'")
      }
      
      count <- dbGetQuery(self$con, qu)
      return(count+1)
      
    },
    
    add_invoerveld_formulier = function(label_veld, type_veld, formulier_kant){
      
      id <- uuid::UUIDgenerate()
      kol_nm_veld <- janitor::make_clean_names(tolower(label_veld), parsing_option = 1)
      lbl_veld <- label_veld
      tp_veld <- type_veld
      volg_veld <- self$get_next_formorder_number(formulier_kant)
      form_kant <- formulier_kant
      
      if(!is.null(self$schema)){
        qu <- glue::glue("INSERT INTO {self.schema}.formulier_velden(id_formulierveld, kolomnaam_veld, label_veld, type_veld, volgorde_veld, formulier_kant, zichtbaar, kan_worden_verwijderd) VALUES('{id}', '{kol_nm_veld}', '{lbl_veld}', '{tp_veld}', '{volg_veld}', '{form_kant}', TRUE, TRUE) ")
      } else {
        qu <- glue::glue("INSERT INTO formulier_velden(id_formulierveld, kolomnaam_veld, label_veld, type_veld, volgorde_veld, formulier_kant, zichtbaar, kan_worden_verwijderd) VALUES('{id}', '{kol_nm_veld}', '{lbl_veld}', '{tp_veld}', '{volg_veld}', '{form_kant}', TRUE, TRUE) ")
      }
      dbExecute(self$con, qu)
    }
    
  )
  
)