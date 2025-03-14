
#' Unmapped Concepts
#' 
#' @param concept_list a list of lists that contains the following elements:
#'   - name: check description
#'      1. the table to look for unmapped concepts
#'      2. the field in which unmapped concepts should be identified
#'      3. the check name identifier
#'      4. the source value field to identify source values associated with
#'         unmapped concepts (used when produce_mapped_list = TRUE)
#' @param produce_mapped_list if `TRUE` then will produce an additional table, automatically
#' output to `results_schema`, identifying the source value of the top unmapped values 
#' (limited to source values with > 10 appearances)
#' @param check_string the name of the check that will be named in the output. Defaults to `uc`
#' 
#' @return a dataframe summarizing the amount of unmapped concepts per table/field. contains:
#'   - site
#'   - measure: from element name describing measuer
#'   - total_rows: the denominator 
#'   - unmapped_rows: numerator
#'   - check_name: from `check_string`
#'   - database_version
#'   - unmapped_prop
#'   
#' if `produce_mapped_list` is `TRUE`, then the following table will be automatically output
#' to the database with the table name `uc_grpd`:
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
check_uc <- function(concept_list,
                     produce_mapped_list=TRUE,
                     check_string = 'uc') {
  
  
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
        group_by(!! sym(concept_list[[i]][[4]])) %>% 
        summarise(src_value_ct = n()) %>%
       ungroup() %>% filter(src_value_ct > 10) %>% collect() %>%
       pivot_longer(cols=!!sym(concept_list[[i]][[4]]), 
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
      mutate(check_name = concept_list[[i]][[3]]) %>%
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

#' Unmapped Concepts per Year
#' 
#' @param concept_list same list as function `check_uc`
#' @param check_string the name of the check that will be named in the output. Defaults to `uc`
#' 
#' @return a dataframe summarizing unmapped concepts throughout time. contains:
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
check_uc_by_year <- function(concept_list,
                             check_string = 'uc') {
  
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
    
    if(class(config('db_src')) %in% c('PostgreSQLConnection', 'PqConnection')){
      sql_string <- paste0("extract(year from ", date_col_final, ")")
    }else{
      sql_string <- paste0('YEAR(', date_col_final, ')')
    }
    
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
      ) %>% mutate(check_name = concept_list[[i]][[3]])
      
    check_concepts[[names(concept_list[i])]] <- date_col_grpd
      
  }
  
  check_concepts
  
} 


