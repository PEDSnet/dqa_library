

#' Check Expected Concept Presence
#' 
#' This function will loop through the list elements provided in `ecp_execute` to identify
#' the count of patients who have the concept identified in the list element and the proportion
#' of patients who have the concept based on the user-provided denominator table.
#'
#' @param ecp_list a list with one list element per concept, found in `ecp_execute`.
#'                 This list includes:
#' 
#'                 - the fact table where the concept of interest is located
#'                   (i.e. measurement_labs)
#'                 - the table with the desired population of patients to be
#'                   used as the denominator (i.e. patients with a drug, 
#'                   patients in the person table)
#'                 - the column within the fact table with the concept_ids of
#'                   interest (i.e. measurement_concept_id)
#'                 - a codeset containing concept_ids for the concept of interest
#'                   (i.e. hemoglobin codes)
#'                 - a string with a name for the check application (i.e. ecp_hemoglobin)
#'
#' @return a table with the total patient count, the count of patients with a particular concept,
#'         the proportion of total patients with the concept, and relevant metadata
#'

check_ecp <- function(ecp_list){
  
  result <- list()
  
  for(i in 1:length(ecp_list)){
    
    concept_group <- ecp_list[[i]][[4]] %>% select(concept_group) %>% distinct() %>% pull()
    
    message(paste0('Starting ', concept_group))
    
    total_pts <- ecp_list[[i]][[2]] %>%
      summarise(total_pt_ct = n_distinct(person_id)) %>%
      collect()
    
    join_cols <- set_names('concept_id', ecp_list[[i]][[3]])
    
    fact_pts <- ecp_list[[i]][[1]] %>%
      inner_join(ecp_list[[i]][[2]]) %>%
      inner_join(ecp_list[[i]][[4]], by = join_cols) %>%
      summarise(concept_pt_ct = n_distinct(person_id)) %>%collect()
    
    pt_cohort <- ecp_list[[i]][[2]] %>% ungroup() %>% distinct(cohort_def) %>% collect() %>% pull()
    
    final_tbl <- total_pts %>%
      mutate(concept_pt_ct = fact_pts$concept_pt_ct,
             concept_group = concept_group,
             prop_with_concept = as.numeric(concept_pt_ct/total_pt_ct),
             check_name = ecp_list[[i]][[5]],
             cohort_denominator = pt_cohort) %>%
      add_meta(check_lib = 'ecp') 
    
    
    result[[paste0(ecp_list[[i]][[5]])]] <- final_tbl
  }
  
  compress <- reduce(.x = result,
                     .f = dplyr::union)
  
  return(compress)
  
}