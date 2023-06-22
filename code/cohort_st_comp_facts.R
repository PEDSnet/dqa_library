#' Function to find visit_occurrences without facts in another set of tables
#' @param fact_tbls list of tbls in which to look at visit_occurrences
#' @return tbl with visit_occurrences, and all columns in the original visit_occurrence table,
#'        for which there are no facts in the `fact_tbls`
#' example function call: check_st_comp_facts(list('condition_occurrence',
#'                                                 'procedure_occurrence',
#'                                                 ...))
check_st_comp_facts <- function(fact_tbls,
                                visit_tbl=site_cdm_tbl('visit_occurrence'),
                                check_string='st_comp_facts') {
  
  visit_tbl_all <-
    visit_tbl %>%
    summarise(
      total_visits = n(),
      total_pts = n_distinct(person_id)
    ) %>% ungroup() %>% collect()
  
  
  all_tbls <- list()
  
  for (i in 1:length(fact_tbls)) {
    
    tbl_comp <- fact_tbls[[i]]
    
    measure_tbl_name <- names(fact_tbls[i])
    
    message(paste0('Starting ',measure_tbl_name))
    
    visit_tbl_all_name <- 
      visit_tbl_all %>%
      mutate(measure_tbl = measure_tbl_name)
    
    missed_visits <- 
      visit_tbl %>% select(visit_occurrence_id) %>%
      anti_join(tbl_comp,
                  by='visit_occurrence_id') %>%
      mutate(measure_tbl = measure_tbl_name) %>%
      group_by(
        measure_tbl
      ) %>%
      summarise(
        no_fact_visits = n()
        ) %>% ungroup() %>% collect()
    
    missed_pts <- 
      visit_tbl %>% select(person_id) %>%
      anti_join(tbl_comp,
                by='person_id') %>%
      mutate(measure_tbl = measure_tbl_name) %>%
      group_by(
        measure_tbl
      ) %>% 
      summarise(
        no_fact_pts = n_distinct(person_id)
      ) %>% ungroup() %>% collect()
    
   # total_ct <- 
    #  missed %>% 
    #  mutate(measure_tbl = measure_tbl_name) %>%
    #  group_by(
    #    measure_tbl
   #   ) %>%
    #  summarise(
    #    no_fact_visits = n(),
    #    no_fact_pts = n_distinct(person_id)
    #  ) %>% ungroup() %>% collect() 
    
    cts_combined <- 
      missed_visits %>%
      left_join(missed_pts) %>%
      left_join(visit_tbl_all_name) %>%
      mutate(
        no_fact_visits_prop = round(
          no_fact_visits / total_visits, 2
        ),
        no_fact_pts_prop = round(
          no_fact_pts / total_pts, 2
        )
      )  %>% 
      add_meta(check_lib=check_string) %>%
      mutate_if(., is.numeric, ~replace(., is.na(.), 0)) %>%
      mutate(
        fact_visits = total_visits - no_fact_visits,
        fact_pts = total_pts - no_fact_pts,
        fact_visits_prop = round(1.00 - no_fact_visits_prop, 2),
        fact_pts_prop = round(1.00 - no_fact_pts_prop, 2)
      )
    
   # if(all(is.na(all_tbls))) all_tbls <- tbl_comp else all_tbls <- dplyr::union(all_tbls, tbl_comp) %>%
      #  compute_new(indexes = list('visit_occurrence_id'),
                  #  temporary = TRUE)
    
    all_tbls[[names(fact_tbls[i])]] <- cts_combined
  }
  
  
  all_tbls
  
}


