#' Function to find patients that do not satisfy the PEDSnet inclusion criteria 
#' @param inclusion_check_tbls list of tables to check for inclusion 
#' @param acceptable_codes inclusion standardized codes  
#' @param inclusion_date start of inclusion of cohort  
#' 
#' @return tbl with patients that do not satisfy the visit, condition, and year inclusion criteria in the format of the person CDM tbl
#' example function call: check_st_conf_inclusion(c('visit_occurrence', 'condition_occurrence'), list of codes, "2009-01-01")


check_st_conf_inclusion <- function(inclusion_check_tbls,
                                    acceptable_codes,
                                    inclusion_date,
                                    check_string='st_conf_inclusion'){
  
  # TODO Exclude covid patients -- compute outside of the function (create a list of covid patients -- make that a parameter, then pass those list of patients into the function)
  
    all_tbls <- list()
    all_patients <- cdm_tbl('person') %>% select(person_id)
    
    for (i in 1:length(inclusion_check_tbls)) {
      
      # Getting domain from tables (example: visit_occurrence --> visit)
      domain <- stringr::str_extract(inclusion_check_tbls[[i]], "^[^_]+(?=_)")
      
      tbl_comp <- cdm_tbl(inclusion_check_tbls[[i]]) %>% 
        select(ends_with("_concept_id"), ends_with("_start_date"), person_id) %>% 
        rename(start_date = ends_with("_start_date")) %>% # temporarily renaming column for filter
        # Getting records before the inclusion date
        filter(start_date < inclusion_date) %>%
        select(person_id, ends_with("_concept_id"), start_date)
      
      # tmp_comp <-
        # Add other domains if needed
        if(domain == "visit"){
          tbl_comp<- # removing for now, but this was a change from Hanieh
            tbl_comp %>%
            # Filtering visit with acceptable codes
            # filter(visit_concept_id %in% acceptable_codes)
            filter(!visit_concept_id %in% acceptable_codes)
        }
      
      # Getting the person_id's of all invalid patients  
      all_tbls[[i]] <- tbl_comp %>% distinct(person_id) %>% mutate(domain_violation = domain)
      
    } 
    
    # Getting all invalid patients that do not satisfy the inclusion criteria
    all_invalid_patients <- reduce(.x=all_tbls,
                                 .f=dplyr::full_join, by='person_id')
    
    # Getting all the patients that do not satisfy the inclusion criteria 
    dplyr::inner_join(all_patients, all_invalid_patients, by='person_id') %>% 
      inner_join(cdm_tbl('person'), by='person_id') 
    
    }