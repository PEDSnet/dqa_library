


#' pull dqa table names
#' 
#' @return a two-column tibble: `schema`, `table`
#' will return with results schema removed
#' 

pull_dqa_table_names <- function() {
  
  
  
  #' pulls table names from database
  tbl_names <- 
    config('db_src') %>%
    DBI::dbListObjects(DBI::Id(schema= config('results_schema'))) %>%
    dplyr::pull(table) %>%
    purrr::map(~slot(.x, 'name')) %>%
    dplyr::bind_rows() %>%
    # select(! matches('*_pp_*')) 
    filter(! str_detect(table,'pp')) %>% 
    filter(str_detect(table,'output|violations')) %>% 
    filter(! str_detect(table, 'fot')) %>% 
    filter(! str_detect(table, 'thrshld')) %>% 
    filter(! str_detect(table, 'old'))
  
  
  #' will remove the results name tag from table names
  tbl_names_short <- 
    tbl_names %>% 
    mutate_all(~gsub(paste0(config('results_name_tag')),'',.))
  
  
  
}


#' pull dqa tables
#' 
#' @param tbl_names output from `pull_dqa_table_names`
#' @return list of tables with dqa results for each element
#' 

pull_dqa_tables <- function(tbl_names) {
  

  #' pulls all table results out
  tbls_all <- list()

  
  for(i in 1:nrow(tbl_names)) {
    
    string_name <- 
      tbl_names_short[i,] %>%
      select(table) %>%
      pull()
    
    tbl_dq <- 
      results_tbl(string_name) %>% collect()
    
    tbls_all[[paste0(string_name)]] <- tbl_dq
    
  }
  
  tbls_all
  
}

#' appends check_name and check_type and site to table names
#' 
#' @param tbl_names_df output from `pull_dqa_table_names`
#' @return A tibble with 3 columns: `site`,`check_type`,`check_name`
#' 

get_check_names <- function(tbl_names) {
  
  
  final <- list()
  
  for(i in 1:nrow(tbl_names)) {
    
    string_name <- 
      tbl_names_short[i,] %>%
      select(table) %>%
      pull()
    
    
    if(any(colnames(results_tbl(paste0(string_name))) == 'check_type')) {
      tbl_current <- results_tbl(paste0(string_name))
    } else {tbl_current <- results_tbl(paste0(string_name)) %>%
      mutate(check_type = 'unknown')}
    
    if(any(colnames(results_tbl(paste0(string_name))) == 'check_name')) {
      tbl_current <- tbl_current
    } else {tbl_current <- tbl_current %>%
      mutate(check_name = 'unknown')}
    
    if(any(colnames(results_tbl(paste0(string_name))) == 'site')) {
      tbl_current <- tbl_current
    } else {tbl_current <- tbl_current %>%
      mutate(site = 'unknown')}
    
    
    check_name_tbl <- 
      tbl_current %>%
      select(site,
             check_type,
             check_name) %>%
      distinct() %>% collect() %>% filter(! check_type == 'unknown',
                                          ! check_name == 'unknown',
                                          ! site == 'unknown')
    
    final[[i]] <- check_name_tbl
    
  }
  
  final_reduce <- 
    reduce(.x=final,
           .f=dplyr::union)
  
  final_reduce
}


#' reads in thresholds from the specs folder
#' 
#' @param check_tbl output from `get_check_names`
#' @param threshold_tbl a csv file from the specs folder that
#' has all thresholds
#' 
#' @return tbl with 5 columns: `site`, `check_type`, `check_name`, `threshold`, `threshold_operator`
#' 
#' 

set_broad_thresholds <- function(check_tbl,
                                 threshold_tbl=read_codeset('threshold_limits',
                                                            col_types='ccdc')) {
  
  checks <- 
    check_tbl %>% 
    inner_join(threshold_tbl,
               by=c('check_type','check_name')) %>% 
    filter(! site == 'unknown',
           ! site == 'pedsnet_total')
  
  
}


#' attaches thresholds to tables
#' 
#' @param tbls_all output of `pull_dqa_tbls`
#' @return this function will output tables to the database:
#' 1) All tables with thresholds implemented (called *_thrshld)
#' 2) Current tables without thresholds renamed (called *_thrshldno)
#' 3) Tables with thresholds impelemtned (not suffixed with anything)
#' 

attach_thresholds <- function(tbls_all) {
  
  #' attaches thresholds to tables
  
  tbls_with_threshold <- list()
  
  for(i in 1:length(tbls_all)) {
    
    check_for_check <- 
      tbls_all[[i]] %>% 
      inner_join(thresholds,
                 by=c('site','check_type','check_name'),
                 copy=TRUE) 
    
    tbls_with_threshold[[paste0(names(tbls_all[i]),'_thrshld')]] <- check_for_check
    
  }
  
  output_list_to_db(tbls_with_threshold,
                    append=FALSE)
  
  
  
  tbls_with_threshold_replace <- list()
  
  for(i in 1:length(tbls_all)) {
    
    tbl_renamed <- 
      tbls_all[[i]] %>% 
      collect() %>% 
      output_tbl(paste0(names(tbls_all[i]),'thrshldno'))
    
    check_for_check <- 
      tbls_all[[i]] %>% 
      left_join(thresholds,
                 by=c('site','check_type','check_name'),
                 copy=TRUE) 
    
    tbls_with_threshold_replace[[paste0(names(tbls_all[i]))]] <- check_for_check
    
  }
  
  output_list_to_db(tbls_with_threshold_replace,
                    append=FALSE)
  
  
}


