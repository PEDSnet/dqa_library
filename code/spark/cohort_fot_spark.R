
#' Spark application of FOT check
#'
#' @param time_tbls a list with the following requirements:
#' `element name`: a short description of the monthly computed visits being measured
#' `first element`: the expression being evaluated (e.g., `cdm_tbl('visit_occurrernce') %>% group_by(visit_concept_id`)
#' @param meta_tbls: a list containing a description of each output from time_tbls. Element names *must* match.
#' @param time_frame: a list that contains the end date of every month to iterate through
#' @param time_period: the time increment over which facts should be counted (i.e. months or years) 
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
check_fot_spark <- function(time_tbls,
                            meta_tbls,
                            time_frame = time_span,
                            time_period = 'month',
                            # lookback_weeks=0,
                            # lookback_months=1,
                            check_string = 'fot',
                            visits_only = TRUE,
                            distinct_visits = TRUE) {
  
  
  final_results <- list()
  
  for(i in 1:length(time_tbls)) {
    
    message(paste0('Starting ',i))
    
    start_date <- as.Date(time_frame[1])
    end_num <- length(time_frame)
    end_date <- as.Date(time_frame[end_num])
    
    nm <- names(time_tbls[i])
    d <- time_tbls[[i]][[2]]
    
    all_mnths <- tibble(month_end = time_frame[2:end_num],
                        check_name = nm,
                        check_desc = d) %>%
      add_meta(check_lib=check_string) %>%
      mutate(month_end = as.Date(month_end))
    
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
      filter(!! sym(colname_string) <= end_date &
               !! sym(colname_string) > start_date) 
    
    date_clean <- visits_narrowed %>%
      mutate(month_end = last_day(!! sym(colname_string)))
    #mutate(month_end = ceiling_date(!! sym(colname_string), time_period)-1)
    
    if(visits_only) {
      visit_cts <-
        date_clean %>%
        group_by(month_end) %>%
        summarise(row_visits = n_distinct(visit_occurrence_id)) %>%
        collect() %>%  
        ungroup()  #%>% 
      # add_meta(check_lib=check_string) %>%
      # mutate(check_name = nm) %>% 
      # mutate(check_desc = d)
    } else if(distinct_visits & !visits_only) {
      visit_cts <-
        date_clean %>%
        group_by(month_end) %>%
        summarise(row_cts = n(),
                  row_visits = n_distinct(visit_occurrence_id),
                  row_pts = n_distinct(person_id)) %>%
        collect() %>%  
        ungroup() #%>% 
      # add_meta(check_lib=check_string) %>%
      # mutate(check_name = nm) %>% 
      # mutate(check_desc = d)
      #mutate(check_name = time_tbls[[i]][[2]]) %>%
      #mutate(check_desc = names(time_tbls[i]))
    } else if(!distinct_visits & !visits_only) {
      visit_cts <-
        date_clean %>%
        group_by(month_end) %>%
        summarise(row_cts = n(),
                  row_pts = n_distinct(person_id)) %>%
        collect() %>%  
        ungroup()  #%>% 
      # add_meta(check_lib=check_string) %>%
      # mutate(check_name = nm) %>% 
      # mutate(check_desc = d)
      #mutate(check_name = time_tbls[[i]][[2]]) %>%
      #mutate(check_desc = names(time_tbls[i]))
    }
    
    if(nrow(visit_cts) == 0){
      final <- all_mnths %>%
        mutate(row_cts = 0,
               row_pts = 0,
               row_visits = 0)
    }else{
      final <- all_mnths %>%
        left_join(visit_cts) %>%
        mutate_if(is.numeric, ~replace(., is.na(.), 0))
    }
    
    final_results[[paste0(check_string, '_', names(time_tbls[i]))]] = final
    
  }
  
  # meta <- compute_meta_tbl(meta_tbls=meta_tbls,
  #   versions_tbl_list=final_results,
  #  check_string=check_string)
  
  # final_results[[paste0(check_string,'_meta')]] <- meta
  
  final_results
  
}