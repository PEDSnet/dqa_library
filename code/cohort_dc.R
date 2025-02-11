


#' @md
#' compare cycles meta; the list should have both the table name  
#' as well as 
#' 
#' @param meta_tbls a list of descriptions for the tables that are being computed;
#' should contain both the table name as well as a description of the table
#' @param versions_tbl_list a list of the computed tables comparing data cycles
#' @param check_string the string for the check name
#' 
#' @return A four column table that has table name, description, number of columns,
#' number of rows; this is the meta table for checking between versions
#' 

compute_dc_meta_tbl <- function(meta_tbls, 
                                        #           versions_tbl_list,
                                        check_string='dc') {
  
  
  meta_tibble <- 
    tibble(
      check_name = character(),
      check_description = character(),
      check_type = character(),
      version_previous = character(),
      version_current = character()
    )
  
  for(i in 1:length(meta_tbls)) {
    
    
    full_tbl_name = paste0(check_string, '_', meta_tbls[[i]][[2]])
    
    table_type_name = full_tbl_name
    table_description_thisround = meta_tbls[[i]][[1]]
    
    meta_tibble_thisround <- 
      meta_tibble %>%
      add_row(
        check_name=table_type_name,
        check_description = table_description_thisround,
        check_type = check_string,
        version_previous = config('previous_version'),
        version_current = config('current_version')
      )
    
    meta_tibble <- meta_tibble_thisround
    
  }
  meta_tibble
}


#' compare both total counts in current db vs prev db
#' as well as count of patients in current db vs prev db
#' 
#' @param prev_v_results the data cycle changes check output that includes
#' the precomputed counts from the most recent version of the data model
#' @param current_v_tbls A list that contains the tables from the current
#' version of the db; 
#' the name of the list will be the name of the table
#' @param meta_tbls A list that contains: 
#'  1. List name containing the same name as the `prev_v_tbls` and `current_v_tbls` list
#'  2. Description of the 
#' @param prev_v string of previous version of db
#' @param current_v string of current version
#' 
#' @return tibble that has grouped variables, and total 
#' row and patient counts of previous vs current versions 
#' 
#' 
check_dc <- function(prev_v_results,
                     current_v_tbls,
                     meta_tbls,
                     prev_v,
                     current_v,
                     site_nm,
                     check_string='dc') {
  
  cts <- list()
  
  for(i in 1:length(current_v_tbls)) {
    
    t=names(current_v_tbls[i])
    
    if(any(str_detect(colnames(current_v_tbls[[i]]),'person_id'))){
      
    message(paste0('Computing ',names(current_v_tbls[i])))
      
    this_round_prev <- 
      prev_v_results %>%
      filter(site == site_nm,
             database_version == prev_v,
             domain == t) %>%
      collect()
    
    this_round_current <- 
      current_v_tbls[[i]] %>%
      summarise(total_ct=n(),
                total_pt_ct=n_distinct(person_id)) %>%
      collect() %>% 
      mutate(database_version=current_v)
    
    }else{
      
      message(paste0('Computing ',names(current_v_tbls[i])))
      
      this_round_prev <- 
        prev_v_results %>%
        filter(site == site_nm,
               database_version == prev_v,
               domain == t) %>%
        collect()
      
      this_round_current <- 
        current_v_tbls[[i]] %>%
        summarise(total_ct=n(),
                  total_pt_ct=0) %>%
        collect() %>% 
        mutate(database_version=current_v)
    }
    
    
    this_round <- this_round_current
  
    q=meta_tbls[[t]][[2]]
    
    cts[[names(current_v_tbls[i])]] <- 
      this_round  %>% 
      mutate(site = site_nm) %>%
     # add_meta(check_lib=check_string) %>%
      mutate(domain=t) %>%
      mutate(check_name=paste0(check_string,'_',q)) %>%
     # mutate(table_name = check_name_full) %>%
      relocate(site, .before=total_ct)  %>% mutate(check_type=check_string) %>%
      union(this_round_prev)
    
    cts
    
  }
  
  
  meta <- compute_dc_meta_tbl(meta_tbls=meta_tbls,
                                      #versions_tbl_list=combined_list,
                                      check_string=check_string)
  
  cts[[paste0(check_string,'_meta')]] <- meta
  
  cts 
  
}


#' compare both total counts in current db vs prev db
#' as well as count of patients in current db vs prev db
#' 
#' @param prev_v_tbls A list that contains the tables from the previous 
#' version of the db;
#' the name of the list will be the name of the table;
#' e.g., `('procedure_occurrence' = cdm_tbl_prev('procedure_occurrence'))`
#' @param current_v_tbls A list that contains the tables from the current
#' version of the db; 
#' the name of the list will be the name of the table
#' @param meta_tbls A list that contains: 
#'  1. List name containing the same name as the `prev_v_tbls` and `current_v_tbls` list
#'  2. Description of the 
#' @param prev_v string of previous version of db
#' @param current_v string of current version
#' 
#' @return tibble that has grouped variables, and total 
#' row and patient counts of previous vs current versions 
#' 
#' 
check_dc_init <- function(prev_v_tbls,
                          current_v_tbls,
                          meta_tbls,
                          prev_v,
                          current_v,
                          site_nm,
                          check_string='dc') {
  
  cts <- list()
  
  for(i in 1:length(prev_v_tbls)) {
    
    if(any(str_detect(colnames(prev_v_tbls[[i]]),'person_id'))){
      
      message(paste0('Computing ',names(prev_v_tbls[i])))
      
      this_round_prev <- 
        prev_v_tbls[[i]] %>%
        summarise(total_ct=n(),
                  total_pt_ct=n_distinct(person_id)) %>%
        collect() %>% 
        mutate(database_version=prev_v)
      
      this_round_current <- 
        current_v_tbls[[i]] %>%
        summarise(total_ct=n(),
                  total_pt_ct=n_distinct(person_id)) %>%
        collect() %>% 
        mutate(database_version=current_v)
      
    }else{
      
      message(paste0('Computing ',names(prev_v_tbls[i])))
      
      this_round_prev <- 
        prev_v_tbls[[i]] %>%
        summarise(total_ct=n(),
                  total_pt_ct=0) %>%
        collect() %>% 
        mutate(database_version=prev_v)
      
      this_round_current <- 
        current_v_tbls[[i]] %>%
        summarise(total_ct=n(),
                  total_pt_ct=0) %>%
        collect() %>% 
        mutate(database_version=current_v)
    }
    
    
    this_round <- 
      dplyr::union(this_round_prev,
                   this_round_current)
    
    t=names(prev_v_tbls[i])
    q=meta_tbls[[t]][[2]]
    
    cts[[names(prev_v_tbls[i])]] <- 
      this_round  %>% 
      mutate(site = site_nm) %>%
      # add_meta(check_lib=check_string) %>%
      mutate(domain=t) %>%
      mutate(check_name=paste0(check_string,'_',q)) %>%
      # mutate(table_name = check_name_full) %>%
      relocate(site, .before=total_ct)  %>% mutate(check_type=check_string)
    
    cts
    
  }
  
  
  meta <- compute_dc_meta_tbl(meta_tbls=meta_tbls,
                              #versions_tbl_list=combined_list,
                              check_string=check_string)
  
  cts[[paste0(check_string,'_meta')]] <- meta
  
  cts 
  
}
