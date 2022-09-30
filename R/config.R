
configured_field_types <- c("Tekstinvoer" = "freetext",
                           "Numerieke invoer" = "numeric",
                           "Ja/Nee" = "boolean",
                           "Keuzemenu (enkele optie)" = "singleselect",
                           "Keuzemenu (meerdere opties)" = "multiselect",
                           "Datum" = "date",
                           "Checkbox" = "singlecheck")
                           #"Tekst met opmaak" = "html")


assert_input_field_type <- function(type){
  if(!type %in% configured_field_types){
    stop(paste("type_field must be one of:",paste(configured_field_types,collapse=",")))
  }  
}



