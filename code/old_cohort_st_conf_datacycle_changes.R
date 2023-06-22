
#' @md
#' compare cycles meta
#' 
#' @param meta_tbls a list of descriptions for the tables that are being computed
#' @param versions_tbl_list a list of the computed tables
#' @param check_string the string for the check name
#' 
#' @return A four column table that has table name, description, number of columns,
#' number of rows; this is the meta table for checking between versions
#' 

compute_meta_tbl <- function(meta_tbls, 
                             versions_tbl_list,
                             check_string='st_conf_dc') {
  
  
  meta_tibble <- 
    tibble(
      table_name = character(),
      table_description = character(),
      table_col_num = numeric(),
      table_row_num = numeric()
    )
  
  for(i in 1:length(meta_tbls)) {
    
    
    full_tbl_name = paste0(check_string, '_', names(prev_v_cts[i]))
    
    table_type_name = full_tbl_name
    table_description_thisround = meta_tbls[[i]]
    
    matched_tbl <- 
      intersect(names(versions_tbl_list),
                full_tbl_name)
    
    matched_tbl_data <- versions_tbl_list[[matched_tbl]]
    
    
    number_of_columns = ncol(matched_tbl_data)
    number_of_rows = nrow(matched_tbl_data)
    
    meta_tibble_thisround <- 
      meta_tibble %>%
      add_row(
        table_name=table_type_name,
        table_description = table_description_thisround,
        table_col_num = number_of_columns,
        table_row_num = number_of_rows
      )
    
    meta_tibble <- meta_tibble_thisround
    
  }
  meta_tibble
}

#' @md
#' compare both total counts in current db vs prev db
#' as well as count of patients in current db vs prev db
#' 
#' @param prev_v_tbls 
#' @param current_v_tbls
#' @param prev_v string of previous version of db
#' @param current_v string of current version
#' 
#' @return tibble that has grouped variables, and total 
#' row and patient counts of previous vs current versions 
#' 
#' 

check_st_conf_dc <- function(prev_v_tbls,
                               current_v_tbls,
                               meta_tbls,
                               prev_v=config('previous_version'),
                               current_v=config('current_version'),
                               check_string='st_conf_dc') {
  
  prev_v_cts <- list()
  
  for(i in 1:length(prev_v_tbls)) {
      
   this_round <- 
     prev_v_tbls[[i]] %>%
      summarise(total_ct=n(),
                total_pt_ct=n_distinct(person_id)) %>%
     collect()
  
   prev_v_cts[[names(prev_v_tbls[i])]] <- this_round %>% 
          mutate(database_version=prev_v)
     
   prev_v_cts
    
  }
    
  current_v_cts <- list()
  
  for(i in 1:length(current_v_tbls)) {
    
    this_round <- 
      current_v_tbls[[i]] %>%
      summarise(total_ct=n(),
                total_pt_ct=n_distinct(person_id)) %>%
      collect()
    
    current_v_cts[[names(current_v_tbls[i])]] <- this_round %>% 
      mutate(database_version=current_v)
    
    current_v_cts
    
  }
  
  combined_list <- list()
  
  for(i in 1:length(prev_v_cts)) {
    
    same_name <- intersect(names(prev_v_cts[i]),
                           names(current_v_cts))
    
    combined <- 
      dplyr::union(prev_v_cts[[i]],
                 current_v_cts[[same_name]]) 
    
    pre_mutated <- 
      combined %>% 
      mutate(domain = names(prev_v_cts[i])) %>%
      mutate(check_name = check_string)
    
    tbl_name <- paste0(check_string, '_', names(prev_v_cts[i]))
    
    mutated <- 
      pre_mutated %>%
      mutate(table_name = tbl_name)
      
    
    combined_list[[tbl_name]] <- mutated
  
  }
  
 combined_list
 
 meta <- compute_meta_tbl(meta_tbls=meta_tbls,
                          versions_tbl_list=combined_list,
                          check_string=check_string)
 
 combined_list[['st_conf_dc_meta']] <- meta
 
 combined_list
  
}













