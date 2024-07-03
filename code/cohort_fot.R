

#' Function that iterates through a list, computing monthly visits
#' 
#' @param time_tbls a list with the following requirements:
#' `element name`: a short description of the monthly computed visits being measured
#' `first element`: the expression being evaluated (e.g., `cdm_tbl('visit_occurrernce') %>% group_by(visit_concept_id`)
#' @param meta_tbls: a list containing a description of each output from time_tbls. Element names *must* match.
#' 
#' @param time_fame a list that contains the end date of every month to iterate through
#' @param lookback_weeks if lookback is in weeks instead of months, this should be set to a non-zero integer. 
#' Defaults to 0, for lookback to be in months.
#' @param lookback_months the number of monoths to look back
#' @param check_string the name of the check, used in metadata table 
#' @param visits_only if TRUE, counts ONLY distinct visits and not patients
#' @param distinct_visits if TRUE, counts distinct visits as well as total counts and total patients
#' 
#' @return table with the following rows (if `distinct_visits` = `TRUE`:
#'  `month_end` | `check_name` | `database_version` | `site` | `time_desc` | `row_cts` | `row_pts` | `row_visits`
#'  
#'  ** if `time_tbls` contains fields that are grouped, the output will contain the grouped variables
#' @return a metadata table summarizing all the table names produced
#' 
#' *** table names are derived from `time_tbls` element names.
#' 

check_fot <- function(time_tbls,
                      meta_tbls,
                      time_frame = time_span,
                      lookback_weeks=0,
                      lookback_months=1,
                      check_string = 'fot',
                      visits_only = TRUE,
                      distinct_visits = TRUE) {
  
  
  final_results <- list()
  
  for(i in 1:length(time_tbls)) {
    
    message(paste0('Starting ',i))
    
    total_cts <- time_tbls_list[[k]][[1]] %>%
      group_by(site) %>%
      summarise(total_pt = n_distinct(person_id),
                total_visit = n_distinct(visit_occurrence_id),
                total_row = n()) %>%
      collect()
    
    temp_results <- list()
    
    for(k in time_frame) {
      
      message(paste0('Starting ',k))
      
      target <- ymd(k)
      
      baseline_end_date <- target
      if(lookback_weeks == 0) {
        baseline_start_date <- target %m-% months(lookback_months)
      } else {baseline_start_date <- target - weeks(x=lookback_weeks)}
      
      
      date_cols <- 
        time_tbls[[i]][[1]] %>% ungroup() %>%
        select(ends_with('_date')) %>% select(- contains('end')) %>% 
        select(-contains(c('result','order'))) 
      
      order_cols <- ncol(date_cols)
      
      date_cols_unmapped <- 
        date_cols %>% 
        select(all_of(order_cols))
      
      colname_string <- as.character(colnames(date_cols_unmapped)[1])
      
      visits_narrowed <-
        time_tbls[[i]][[1]] %>%
        filter(!! sym(colname_string) <= baseline_end_date &
                 !! sym(colname_string) > baseline_start_date)  
      
      n <- names(time_tbls[i])
      d <- time_tbls[[i]][[2]]
      
      if(visits_only) {
        visit_cts <-
          visits_narrowed %>%
          summarise(row_visits = n_distinct(visit_occurrence_id)) %>%
          collect() %>%  
          ungroup()  %>% 
          add_meta(check_lib=check_string) %>%
          mutate(check_name = n) %>% 
          mutate(check_desc = d)
      } else if(distinct_visits & !visits_only) {
        visit_cts <-
          visits_narrowed %>%
          summarise(row_cts = n(),
                    row_visits = n_distinct(visit_occurrence_id),
                    row_pts = n_distinct(person_id)) %>%
          collect() %>%  
          ungroup()  %>% 
          add_meta(check_lib=check_string) %>%
          mutate(check_name = n) %>% 
          mutate(check_desc = d)
          #mutate(check_name = time_tbls[[i]][[2]]) %>%
          #mutate(check_desc = names(time_tbls[i]))
      } else if(!distinct_visits & !visits_only) {
        visit_cts <-
          visits_narrowed %>%
          summarise(row_cts = n(),
                    row_pts = n_distinct(person_id)) %>%
          collect() %>%  
          ungroup()  %>% 
          add_meta(check_lib=check_string) %>%
          mutate(check_name = n) %>% 
          mutate(check_desc = d)
          #mutate(check_name = time_tbls[[i]][[2]]) %>%
          #mutate(check_desc = names(time_tbls[i]))
      }
      
      
      
      if(! lookback_weeks) {
        this_round <- visit_cts %>%
          mutate(month_end = date(k)) 
      } else {this_round <- this_round_pre %>%
        mutate(week_end = date(i)) }
      
      temp_results[[k]] <- this_round
      
    }
    
    final_results[[paste0(check_string, '_', n)]] = reduce(.x=temp_results, .f=union) %>% 
      left_join(total_cts)
    
  }
  
 # meta <- compute_meta_tbl(meta_tbls=meta_tbls,
                        #   versions_tbl_list=final_results,
                         #  check_string=check_string)
  
 # final_results[[paste0(check_string,'_meta')]] <- meta
  
  final_results
  
}
