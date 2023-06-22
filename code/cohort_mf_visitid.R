



#' Function to check for visits in a table having an associated visit_occurrence_id
#' 
#' @param check_visit_list A list where each element is structured as:
#' element name: string describing the table checking visits for
#' 1st subelement: the table containing the `visit_occurrence_id`
#' @param string_tbl_name defaults to name of check
#' 
#' @return table with:
#' measure | total_visits | missing_visits_total | missing_visits_distinct | 
#' visit_na | total_id | check_name | database_version | site
#' 



check_mf_visitid <- function(check_visit_list,
                             string_tbl_name='mf_visitid') {
  
  
  tbl_visits <- list()
  
  for(i in 1:length(check_visit_list)) {
    
    total_visit_ids <- 
      check_visit_list[[i]][[1]] %>%
      summarise(
        total_visits=n_distinct(visit_occurrence_id)
      ) %>% collect()
    
    tbl_visit_ids <- 
      check_visit_list[[i]][[1]] %>%
      anti_join(
        cdm_tbl('visit_occurrence'),
        by='visit_occurrence_id'
      ) 
    
    tbl_visit_flags <- 
      tbl_visit_ids %>%
      mutate(missing_flag = 
               case_when(
                 is.na(visit_occurrence_id) ~ 'visit_na',
                 TRUE ~ 'visit_id'
               )) 
    
    visit_summaries <-
      tbl_visit_ids %>%
      summarise(
        missing_visits_total = n(),
        missing_visits_distinct = n_distinct(visit_occurrence_id)
      ) %>% collect() 
    
    visit_summaries_nas <- 
      tbl_visit_flags %>%
      group_by(
        missing_flag
      ) %>% summarise(
        total_ct = n()
      ) %>% collect() 
    
    if(dim(visit_summaries_nas)[1] == 0) {
      
      visit_summaries_nas_all <- 
        visit_summaries_nas %>%
        add_row(
          missing_flag = 'visit_na',
          total_ct = 0
        ) %>%
        add_row(
          missing_flag = 'visit_id',
          total_ct = 0
        )
      
    } else if(dim(visit_summaries_nas)[1] == 1) {
      
      if(visit_summaries_nas$missing_flag == 'visit_na') {
        visit_summaries_nas_all <- 
          visit_summaries_nas %>%
          add_row(
            missing_flag = 'visit_id',
            total_ct = 0
          ) } else {
            visit_summaries_nas_all <- 
              visit_summaries_nas %>%
              add_row(
                missing_flag = 'visit_na',
                total_ct = 0
              )
          }
      visit_summaries_nas_all    
      
      }  else {visit_summaries_nas_all <- visit_summaries_nas }
      
    visit_summaries_nas_all <- 
      visit_summaries_nas_all %>%
      pivot_wider(
        names_from = 'missing_flag',
        values_from = 'total_ct'
      )
    
    all_tbl <- tibble(
      measure = names(check_visit_list[i]),
      total_visits = total_visit_ids$total_visits,
      missing_visits_total = visit_summaries$missing_visits_total,
      missing_visits_distinct = visit_summaries$missing_visits_distinct,
      visit_na = visit_summaries_nas_all$visit_na,
      total_id = visit_summaries_nas_all$visit_id
    ) %>% distinct()
    
    tbl_visits[[i]] = all_tbl %>% add_meta(check_lib = string_tbl_name) %>%
                      mutate(check_name=check_visit_list[[i]][[2]])
    
  }
  
  tbl_visits
  
}

