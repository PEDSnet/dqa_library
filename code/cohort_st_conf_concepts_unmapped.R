
#' Function to check for value set conformance to a defined set
#' @param concept_list a list that contains the following elements:
#' - element name: description of the object that will be summarized
#' - first element of sublist: the object that contains the element 
#' that will be summarized (e.g., `site_cdm_tbl('drug_exposuer')`)
#' - second element of sublist: the element that will be summarized
#' - third element of sublist: if `produce_mapped_list = TRUE` the name of the 
#' source values of unmapped elements
#' @param produce_mapped_list if `TRUE` then will show the value of the top unmapped values; 
#' values > 10
#' @param check_string the name of the check that will be named in the output. Defaults to 
#' `st_conf_concepts_unmapped`
#' 
#' @return 
#' main table output with the table name `ct_conf_concepts_unmapped_grpd`:
#'   tbl with the following columns:
#'   - site
#'   - measure: from element name describing measuer
#'   - total_rows: the denominator 
#'   - unmapped_rows: numerator
#'   - check_name: from `check_string`
#'   - database_version
#'   - unmapped_prop
#'   
#' if `produce_mapped_list` is `TRUE`, then the following table will be automatically output
#' to the database with the table name `st_conf_concepts_grpd`:
#'   - site
#'   - unmapped_description
#'   - database_version
#'   - check_name
#'   - src_value
#'   - src_value_name
#'   - src_value_ct
#'   
#' 
#' @export
#'  
#'                                                              

check_st_conf_concepts_unmapped <- function(concept_list,
                                               produce_mapped_list=TRUE,
                                               check_string = 'st_conf_concepts_unmapped') {
  
  
  check_concepts <- list()
  
  for(i in 1:length(concept_list)) {
    
    total_rows <- 
      concept_list[[i]][[1]] %>%
      summarise(
        total_rows = n()
      ) %>% collect()
    
    
    colname <- concept_list[[i]][[2]]
    
    unmapped_values <- 
      concept_list[[i]][[1]] %>%
      filter(
        .data[[colname]]  %in% c(44814650L,0L,
                                 44814653L, 44814649L)
        ) 
    
    if(produce_mapped_list) {
      
      meta_desc = names(concept_list[i])
      
     unmapped_db <- 
       unmapped_values %>% 
        group_by(!! sym(concept_list[[i]][[3]])) %>% 
        summarise(src_value_ct = n()) %>%
       ungroup() %>% filter(src_value_ct > 10) %>%
       pivot_longer(cols=!!sym(concept_list[[i]][[3]]), 
                    names_to = 'src_value_name',
                    values_to = 'src_value') %>%
       add_meta(check_lib = check_string) %>%
       mutate(
         unmapped_description = meta_desc
       ) %>% collect()
     
      output_tbl_append(data=unmapped_db,
                        name=paste0(check_string, '_grpd')) 
      
    }
    
    total_unmapped <- 
      unmapped_values %>% 
      summarise(
        unmapped_rows = n()
      ) %>% collect()
    
    unmapped_cts <- 
      bind_cols(total_rows, total_unmapped) %>%
      add_meta(check_lib = check_string) %>%
      relocate(site, .before = total_rows) %>%
      mutate(measure = names(concept_list[i])) %>%
      relocate(measure, .after = site) %>%
      mutate(
        unmapped_prop = round(unmapped_rows / total_rows, 2)
      )
    
    check_concepts[[names(concept_list[i])]] <- unmapped_cts
    
  }
  
  check_concepts
  
}

#' 
#' Function that produces output that contains unmapped values by year
#' 
#' @param concept_list same list as function `check_st_conf_concepts_unmapped`
#' @param check_string the name of the check that will be named in the output. Defaults to 
#' `st_conf_concepts_unmapped`
#' 
#' @return the following table, called `st_conf_concepts_unmapped_by_year`
#' 
#' - site
#' - year_date
#' - total_unmapped_row_ct
#' - total_row_ct
#' - unmapped_description 
#' - check_name
#' - database_version
#' 
#' 

check_st_conf_concepts_by_year <- function(concept_list,
                                           check_string = 'st_conf_concepts_unmapped') {
  
  check_concepts <- list()
  
  for(i in 1:length(concept_list)) {
    
    colname <- concept_list[[i]][[2]]
    
    
    unmapped_values <- 
      concept_list[[i]][[1]] %>%
      filter(
        .data[[colname]]  %in% c(44814650L,0L,
                                 44814653L, 44814649L)
      ) 
    
    date_cols <- 
      unmapped_values %>%
      select(ends_with('_date')) %>% select(- contains('end'))
    
    order_cols <- ncol(date_cols)
    
    date_cols_unmapped <- 
      date_cols %>% 
      select(order_cols)
    
    date_col_final <- 
      colnames(date_cols_unmapped)
    
    sql_string <- paste0("extract(year from ", date_col_final, ")")
    
    total_rows <- 
      concept_list[[i]][[1]] %>%
      mutate(year_date = as.integer(sql(sql_string))) %>%
      group_by(
        year_date
      ) %>% summarise(
        total_row_ct = n()
      )%>% collect() 
    
    
    date_col_grpd <- 
      date_cols_unmapped %>%
      mutate(year_date = as.integer(sql(sql_string))) %>%
      group_by(
        year_date
      ) %>% summarise(
        total_unmapped_row_ct = n()
      ) %>% collect() %>% #mutate(year_date=as.Date(as.character(year_date),format='%Y')) %>%
     # mutate(
       # year_date=format(year_date, format='%Y')
     # ) %>%
      inner_join(total_rows) %>% 
      mutate(unmapped_description=names(concept_list[i])) %>%
        add_meta(check_lib = check_string) %>%
      relocate(
        site, .before = year_date
      )
      
    check_concepts[[names(concept_list[i])]] <- date_col_grpd
      
  }
  
  check_concepts
  
} 


