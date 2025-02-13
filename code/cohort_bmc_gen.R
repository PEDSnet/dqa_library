
#' Find concept identifiers for BMC facts
#' 
#' @param fact_tbl the CDM table associated with the check
#' @param fact_concept_id the concept_id column in the `fact_tbl` that should be used
#' to join to the concept table
#' @param concept_field the field in the concept table used to identify the concept
#' (usually concept_name or concept_class_id)
#' @param concept_tbl defaults to `vocabulary_tbl('concept')`
#' 
#' @return the original fact_tbl that also contains a `concept_type` field that reflects
#' the contents of the `concept_field` associated with each concept of interest
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



#' Best Mapped Concepts
#' 
#' @param fact_tbl_list_args list of lists where each element is named with the check identifier 
#' and contains the following information:
#'    1. The table where the concept is located
#'    2. The `*_concept_id` column with the relevant concept
#'    3. A plain language string label for the check
#'    4. The check name (formatted as `bmc_*`)
#'    5. The column in the `concept` table that is needed to identify the concept 
#'       (either concept_name or concept_class_id)
#' @param check_string string that contains a description of the table
#' 
#' @return a dataframe summarizing the distribution of each concept type in the table/field
#' of interest; best/not best designations will be applied to this table in the processing step
#' 
check_bmc_gen <- function(fact_tbl_list_args,
                          check_string='bmc') {
  
  results <- list()
  
  for(i in 1:length(fact_tbl_list_args)) {
    
    fact_tbl_name <- paste0(names(fact_tbl_list_args[i]))
    
    if(grepl('fips_', names(fact_tbl_list_args[i]))){
      xwalk <- fact_tbl_list_args[[i]][[1]] %>%
        rename(concept_type=!!sym(fact_tbl_list_args[[i]][[2]]))
    }else{
      xwalk <-
        find_concept_names(fact_tbl = fact_tbl_list_args[[i]][[1]],
                           fact_concept_id = fact_tbl_list_args[[i]][[2]],
                           concept_field = fact_tbl_list_args[[i]][[5]])
    }
    
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
      mutate(concept = as.character(concept)) %>%
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