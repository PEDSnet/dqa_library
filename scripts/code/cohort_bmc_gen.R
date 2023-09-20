
#' @md
#' takes cohort of drug exposures, 
#' joins to vocabulary table, 
#' creates xwalk between drug and level 
#' 
#' @param drug_tbl defaults to `cdm_tbl('drug_exposure')`
#' @param concept_tbl defaults to `vocabulary_tbl('concept')`
#' 
#' @return the original drug_tbl with an extra 
#' column called `rxnorm_level`
#' 

find_concept_names <- function(fact_tbl,
                               fact_concept_id,
                               concept_field,
                               concept_tbl=vocabulary_tbl('concept')) {
  
  

  fact_tbl %>%
    rename(concept_id = !!sym(fact_concept_id)) %>%
    inner_join(
      select(concept_tbl,
             concept_id,
             !!sym(concept_field)),
      by=c('concept_id')
    ) %>% rename(concept_type=!!sym(concept_field))
  
}



#' compute numbers proportion of rows and patients at
#' each level of rxnorm drug level
#' 
#' @param drug_tbl_list_args A list of lists where list name is a description of
#' what the table contains; 
#'  1. The first element of the nested list is the table that will be computed; 
#'  2. The second element of the nested list is a description of the table
#' @param check_string string that contains a description of the table
#' 

check_bmc_gen <- function(fact_tbl_list_args,
                          check_string='bmc') {
  
  results <- list()
  
  for(i in 1:length(fact_tbl_list_args)) {
    
    fact_tbl_name <- paste0(names(fact_tbl_list_args[i]))
    
    xwalk <-
      find_concept_names(fact_tbl = fact_tbl_list_args[[i]][[1]],
                         fact_concept_id = fact_tbl_list_args[[i]][[2]],
                         concept_field = fact_tbl_list_args[[i]][[5]])
    
    total_cts <- 
      xwalk %>% 
      summarise(total_rows=n(),
                total_pts=n_distinct(person_id)) %>% collect_new()
    
    grps <- dplyr::group_vars(xwalk)
    
    concept_grpd <- c(grps, 'concept_type')
    
    concept_cts <- 
      xwalk %>%
      group_by(!!! syms(concept_grpd)) %>%
      summarise(concept_rows=n(),
                concept_pts=n_distinct(person_id)) %>% 
      rename('concept' = !!sym(concept_grpd)) %>%
      collect_new()
    
    if(length(concept_grpd) > 1) {
      
      props <- 
        concept_cts %>%
        left_join(total_cts) %>% ungroup() %>%
        mutate(row_proportions=round(concept_rows/total_rows,2)) %>%
        mutate(person_proportions=round(concept_pts/total_pts,2)) %>%
        add_meta(check_lib = check_string) %>%
        mutate(check_name = fact_tbl_list_args[[i]][[4]]) %>%
        mutate(check_desc = fact_tbl_list_args[[i]][[3]]) %>%
        mutate(check_desc_short =fact_tbl_name)
      
    } else {
      
      props <- 
        concept_cts %>%
        mutate(total_rows=total_cts$total_rows,
               total_pts=total_cts$total_pts) %>%
        #left_join(total_cts) %>% ungroup() %>%
        mutate(row_proportions=round(concept_rows/total_rows,2)) %>%
        mutate(person_proportions=round(concept_pts/total_pts,2)) %>%
        add_meta(check_lib = check_string) %>%
        mutate(check_name = fact_tbl_list_args[[i]][[4]]) %>%
        mutate(check_desc = fact_tbl_list_args[[i]][[3]]) %>%
        mutate(check_desc_short=fact_tbl_name)
      
    }
    
    
    results[[paste0(check_string,'_',names(fact_tbl_list[i]))]] <- props
    
  }
  
  results
  
}