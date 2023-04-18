
# audit functies, apart gezet
# komen uit WBM.
# Ooit moet hele audit systeem onder de loep worden gelegd mbt performance....
#' @param x A vector of any type
get_column_mutations <- function(x){
  
  n_r <- length(x)
  if(n_r == 1)return(NULL)
  
  if(length(unique(x)) == 1)return(NULL)
  
  x <- as.character(x) # safer comparison (?)
  x[is.na(x)] <- "Onbekend"
  x[x == "[]"] <- "Onbekend"
  
  # successive comparison
  ii <- x[2:n_r] == x[1:(n_r-1)]
  
  if(all(ii))return(NULL)
  
  # which elements of x are changed from the previous one
  i_change <- which(!ii) + 1
  
  tibble(
    index = i_change,
    old_val = x[i_change - 1],
    new_val = x[i_change]
  )
  
}

#' @description Partial mutations for a single ID (dataframe)
create_partial_mutations_id <- function(data, 
                                        ignore_cols = c("user_id","auditstamp")){
  
  # nodig?
  data$auditstamp <- as.POSIXct(data$auditstamp, tz = "UTC" ) 
  
  data <- arrange(data, auditstamp)
  
  event_out <- slice(data, 1) %>%
    transmute(id, type = "Aanmaak", name = naam,
              variable = NA_character_, old_val = NA_character_,
              new_val = NA_character_, username, auditstamp)
  
  if(nrow(data) >1){
    
    # find mutations for each column
    # except columns we ignore
    events <- lapply(select(data,-any_of(ignore_cols)), 
                     get_column_mutations)
    
    for(i in seq_along(events)){
      if(is.null(events[[i]]))next
      events[[i]]$variable <- names(events)[i]
    }
    
    events <- bind_rows(events)
    
    if(nrow(events) > 0){
      
      # add other columns
      events <- cbind(events, 
                      slice(select(data, id, name = naam, username, auditstamp), events$index))
      
      # type
      events$type <- ifelse(events$variable == "verwijderd", "Verwijderd", "Wijziging")
      
      event_out <- bind_rows(event_out, select(events, -index))
      
    }
    
  }
  
  event_out
}


create_partial_mutations_new <- function(data, 
                                         lowerdate = as.Date("1970-1-1"), 
                                         upperdate = Sys.Date(),
                                         auditstamp_column = "time_modified",
                                         id_column = "registration_id",
                                         name_column = "registration_name",
                                         username_column = "user_id"
){
  
  lowerdate <- as.Date(lowerdate)
  upperdate <- as.Date(upperdate)
  
  # truc om zo weinig mogelijk te veranderen in de functies hierboven, de komen uit WBM
  data <- data %>% 
    mutate(auditstamp = !!sym(auditstamp_column),
           id = !!sym(id_column),
           naam = !!sym(name_column),
           username = !!sym(username_column) )
  
  data_in_range <- data %>%
    dplyr::filter(between(as.Date(auditstamp), lowerdate, upperdate))
  
  last_before_range <- data %>%
    dplyr::filter(as.Date(auditstamp) < lowerdate, 
                  id %in% !!data_in_range$id) %>%
    group_by(id) %>%
    filter(auditstamp == max(auditstamp)) %>%
    ungroup
  
  data_mut <- rbind(last_before_range, data_in_range)
  
  total_changes <- split(data_mut, data_mut$id) %>% 
    lapply(create_partial_mutations_id) %>% 
    bind_rows  
  
  if(nrow(total_changes) > 0){
    out <- total_changes %>% 
      dplyr::filter(auditstamp >= lowerdate)
  } else {
    out <- total_changes  
  }
  
  names(out) <- c(id_column, "type", name_column, "variable", "old_val", "new_val", 
                  username_column, auditstamp_column)
  
  out
}