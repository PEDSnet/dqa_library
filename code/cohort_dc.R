
#' Data Cycle Changes
#'
#' @param dc_tbl_list list of tables that should be tested in both the current and
#'                    previous data model versions
#'                    
#'                    should be a named list where each element contains the name of
#'                    the table as a string and any filter logic that should be applied
#'                    to the table; if no filter logic is needed, the second element should
#'                    be left as NA
#' @param prev_ct_src a string indicating where the counts from the previous data model
#'                    should be extracted: either `cdm` (to pull from the previous CDM instance)
#'                    or `result` (to pull from a previous instance of check_dc output)
#' @param prev_rslt_tbl if prev_ct_src = 'result', the name of the table where previous results
#'                      are stored. automatically will pull this from the previously defined
#'                      results_schema_prev config
#' @param prev_rslt_db if prev_ct_src = 'result', the database connection to be used to 
#'                     access the previous results; defaults to `config('db_src')`
#' @param dc_meta_list list of metadata associated with each entry in `dc_tbl_list`
#'                     
#'                     should be structured as a nested list where each entry is named
#'                     the same as the entry in `dc_tbl_list` and contains two elements:
#'                     a longer description of the check and an abbreviated identifier to 
#'                     represent the check (i.e. 'dr' for 'drug_exposure')
#' @param check_string an abbreviated identifier to identify all output from this module
#'                     defaults to `dc`
#'
#' @returns a list with two dataframes:
#'          - `dc_cts`: dataframe containing the row and (where applicable) person counts for each table
#'          - `dc_meta`: the metadata associated with each input table that appears in `dc_cts`
#' @export
#'
#' @examples
check_dc <- function(dc_tbl_list,
                     prev_ct_src = 'cdm',
                     prev_rslt_tbl = 'dc_output',
                     prev_rslt_db = config('db_src'),
                     dc_meta_list, 
                     check_string = 'dc'){
  
  site_nm <- config('site')
  prev_v <- config('previous_version')
  current_v <- config('current_version')
  
  cts <- list()
  
  for(i in 1:length(dc_tbl_list)){
    
    cli::cli_inform(paste0('Computing ', names(dc_tbl_list[i])))
    
    pid_check <- any(str_detect(colnames(dc_tbl_list[[i]][[1]]),'person_id'))
      
    ## Current Tables
    if(!is.na(dc_tbl_list[[i]][[2]])){
      this_round_current <- dc_tbl_list[[i]][[1]] %>%
        filter(!! rlang::parse_expr(dc_tbl_list[[i]][[2]]))
    }else{
      this_round_current <- dc_tbl_list[[i]][[1]]
      }
      
    if(pid_check){
      this_round_current <- this_round_current %>% 
        summarise(total_ct=n(),
                  total_pt_ct=n_distinct(person_id)) %>%
        collect() %>% 
        mutate(database_version=current_v)
    }else{
      this_round_current <- this_round_current %>% 
        summarise(total_ct=n(),
                  total_pt_ct=0) %>%
        collect() %>% 
        mutate(database_version=current_v)
    }
      
    ## Previous Tables
    if(prev_ct_src == 'cdm'){
      
      if(!is.na(dc_tbl_list[[i]][[2]])){
        this_round_prev <- dc_tbl_list[[i]][[3]] %>% 
          filter(!! rlang::parse_expr(dc_tbl_list[[i]][[2]]))
      }else{
        this_round_prev <- dc_tbl_list[[i]][[3]]
      }
      
      if(pid_check){
        this_round_prev <- this_round_prev %>%
          summarise(total_ct=n(),
                    total_pt_ct=n_distinct(person_id)) %>%
          collect() %>% 
          mutate(database_version=prev_v)
      }else{
        this_round_prev <- this_round_prev %>%
          summarise(total_ct=n(),
                    total_pt_ct=0) %>%
          collect() %>% 
          mutate(database_version=prev_v)
      }
      
    }else{
      t <- names(dc_tbl_list[i])
      
      this_round_prev <- 
        get_argos_default()$qual_tbl(name = prev_rslt_tbl, schema_tag = 'results_schema_prev', 
                                     db = prev_rslt_db) %>%
        filter(site == site_nm,
               database_version == prev_v,
               domain == t) %>%
        collect()
    }
    
    q <- dc_meta_list[[i]][[2]]
    t <- names(dc_tbl_list[i])
    
    if(prev_ct_src == 'cdm'){
      cts[[names(dc_tbl_list[i])]] <- 
        this_round_current %>% 
        union(this_round_prev) %>%
        mutate(site = site_nm) %>%
        mutate(domain=t) %>%
        mutate(check_name=paste0(check_string,'_',q)) %>%
        relocate(site, .before=total_ct)  %>% mutate(check_type=check_string)
    }else{
      cts[[names(dc_tbl_list[i])]] <- 
        this_round_current %>% 
        mutate(site = site_nm) %>%
        mutate(domain=t) %>%
        mutate(check_name=paste0(check_string,'_',q)) %>%
        relocate(site, .before=total_ct)  %>% mutate(check_type=check_string) %>%
        union(this_round_prev)
      }
    
    cts
      
  }
  
  meta <- compute_dc_meta_tbl(meta_tbls=dc_meta_list,
                              check_string=check_string)
  
  collapse_cts <- purrr::reduce(.x = cts,
                                .f = union)
  
  final_list <- list('dc_cts' = collapse_cts,
                     'dc_meta' = meta)
  
  return(final_list)
}