#' Function to find visit_occurrences without facts in another set of tables
#' Function is utilized for the following checks: ST-COMP-Facts and ST-COMP-PatientRecords 
#' 
#' @param fact_tbls list of tbls in which to look at visit_occurrences
#' 
#' @return tbl with visit_occurrences, and all columns in the original visit_occurrence table,
#'        for which there are no facts in the `fact_tbls`
#' example function call: check_st_comp_facts(list('condition_occurrence',
#'                                                 'procedure_occurrence', ...)

check_st_comp_facts_records <- function(fact_tbls) {
  
  all_tbls <- NA
  for (i in 1:length(fact_tbls)) {
    
    tbl_comp <- cdm_tbl(fact_tbls[[i]]) %>% select(visit_occurrence_id) %>% distinct()
    
    if(all(is.na(all_tbls))) all_tbls <- tbl_comp else all_tbls <- dplyr::union(all_tbls, tbl_comp) %>%
        compute_new(indexes = list('visit_occurrence_id'),
                    temporary = TRUE)
  }
  
  
  cdm_tbl('visit_occurrence') %>%
    anti_join(all_tbls, by = 'visit_occurrence_id')

  
}
