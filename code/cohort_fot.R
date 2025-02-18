
#' Facts Over Time
#' 
#' @param time_tbls a list of input arguments with the following requirements:
#'  - element name: the check name identifier
#'  - first element: the table being evaluated (e.g., cdm_tbl('visit_occurrernce') %>% group_by(visit_concept_id))
#'  - second element: a short description of the monthly computed visits being measured
#' @param time_frame a list that contains the end date of every month to iterate through
#' @param lookback_weeks if lookback is in weeks instead of months, this should be set to a non-zero integer. 
#' Defaults to 0, for lookback to be in months.
#' @param lookback_months the number of months to look back; defaults to 1
#' @param check_string the abbreviated name of the check; defaults to `fot`
#' @param visits_only if TRUE, counts ONLY distinct visits and not patients
#' @param distinct_visits if TRUE, counts distinct visits as well as total counts and total patients
#' 
#' @return table with the following rows (if `distinct_visits` = `TRUE`:
#'  `month_end` | `check_name` | `database_version` | `site` | `time_desc` | `row_cts` | `row_pts` | `row_visits`
#'  
#'  ** if `time_tbls` contains fields that are grouped, the output will contain the grouped variables
#' 
check_fot <- function(time_tbls,
                      time_frame = time_span,
                      lookback_weeks=0,
                      lookback_months=1,
                      check_string = 'fot',
                      visits_only = FALSE,
                      distinct_visits = TRUE) {
  
  
  if(any(class(config('db_src')) %in% 'PrestoConnection')){
    
    fot_rslt <- check_fot_trino(time_tbls = time_tbls,
                                time_frame = time_frame,
                                lookback_weeks = lookback_weeks,
                                lookback_months = lookback_months,
                                check_string = check_string,
                                visits_only = visits_only,
                                distinct_visits = distinct_visits)
    
  }else{
    
    fot_rslt <- check_fot_orig(time_tbls = time_tbls,
                               time_frame = time_frame,
                               lookback_weeks = lookback_weeks,
                               lookback_months = lookback_months,
                               check_string = check_string,
                               visits_only = visits_only,
                               distinct_visits = distinct_visits)
    
  }
  
  return(fot_rslt)
  
}

#' Facts Over Time (Original Postgres Implementation)
#' 
#' @param time_tbls a list of input arguments with the following requirements:
#'  - element name: the check name identifier
#'  - first element: the table being evaluated (e.g., cdm_tbl('visit_occurrernce') %>% group_by(visit_concept_id))
#'  - second element: a short description of the monthly computed visits being measured
#' @param time_frame a list that contains the end date of every month to iterate through
#' @param lookback_weeks if lookback is in weeks instead of months, this should be set to a non-zero integer. 
#' Defaults to 0, for lookback to be in months.
#' @param lookback_months the number of months to look back; defaults to 1
#' @param check_string the abbreviated name of the check; defaults to `fot`
#' @param visits_only if TRUE, counts ONLY distinct visits and not patients
#' @param distinct_visits if TRUE, counts distinct visits as well as total counts and total patients
#' 
#' @return table with the following rows (if `distinct_visits` = `TRUE`:
#'  `month_end` | `check_name` | `database_version` | `site` | `time_desc` | `row_cts` | `row_pts` | `row_visits`
#'  
#'  ** if `time_tbls` contains fields that are grouped, the output will contain the grouped variables
#' 
check_fot_orig <- function(time_tbls,
                           time_frame = time_span,
                           lookback_weeks=0,
                           lookback_months=1,
                           check_string = 'fot',
                           visits_only = FALSE,
                           distinct_visits = TRUE) {
  
  
  final_results <- list()
  
  for(i in 1:length(time_tbls)) {
    
    cli::cli_inform(paste0('Starting ',i))
    
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
      }
      
      
      
      if(! lookback_weeks) {
        this_round <- visit_cts %>%
          mutate(month_end = date(k)) 
      } else {this_round <- this_round_pre %>%
        mutate(week_end = date(i)) }
      
      temp_results[[k]] <- this_round
      
    }
    
    final_results[[paste0(check_string, '_', n)]] = reduce(.x=temp_results, .f=union) 
  }
  
  final_results
  
}



#' Facts Over Time (Optimized for Trino)
#' 
#' @param time_tbls a list of input arguments with the following requirements:
#'  - element name: the check name identifier
#'  - first element: the table being evaluated (e.g., cdm_tbl('visit_occurrernce') %>% group_by(visit_concept_id))
#'  - second element: a short description of the monthly computed visits being measured
#' @param time_frame a list that contains at least end date of the first and last month of the desired time span
#' @param lookback_weeks if lookback is in weeks instead of months, this should be set to a non-zero integer. 
#' Defaults to 0, for lookback to be in months.
#' @param lookback_months the number of months to look back; defaults to 1
#' @param check_string the abbreviated name of the check; defaults to `fot`
#' @param visits_only if TRUE, counts ONLY distinct visits and not patients
#' @param distinct_visits if TRUE, counts distinct visits as well as total counts and total patients
#' 
#' @return table with the following rows (if `distinct_visits` = `TRUE`:
#'  `month_end` | `check_name` | `database_version` | `site` | `time_desc` | `row_cts` | `row_pts` | `row_visits`
#'  
#'  ** if `time_tbls` contains fields that are grouped, the output will contain the grouped variables
#'  
check_fot_trino <- function(time_tbls,
                            time_frame = time_span,
                            lookback_weeks=0,
                            lookback_months=1,
                            check_string = 'fot',
                            visits_only = TRUE,
                            distinct_visits = TRUE) {
  
  
  final_results <- list()
  
  for(i in 1:length(time_tbls)) {
    
    cli::cli_inform(paste0('Starting ',i))
    
    start_date <- time_span[1]
    end_int <- length(time_span)
    end_date <- time_span[end_int]
    
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
      filter(!! sym(colname_string) <= as.Date(end_date) &
               !! sym(colname_string) > as.Date(start_date)) %>%
      mutate(month_end = last_day_of_month(!!sym(colname_string)))
    
    n <- names(time_tbls[i])
    d <- time_tbls[[i]][[2]]
    
    if(visits_only) {
      visit_cts <-
        visits_narrowed %>%
        group_by(month_end) %>%
        summarise(row_visits = n_distinct(visit_occurrence_id)) %>%
        collect() %>%  
        ungroup()  %>% 
        add_meta(check_lib=check_string) %>%
        mutate(check_name = n) %>% 
        mutate(check_desc = d)
    } else if(distinct_visits & !visits_only) {
      visit_cts <-
        visits_narrowed %>%
        group_by(month_end) %>%
        summarise(row_cts = n(),
                  row_visits = n_distinct(visit_occurrence_id),
                  row_pts = n_distinct(person_id)) %>%
        collect() %>%  
        ungroup()  %>% 
        add_meta(check_lib=check_string) %>%
        mutate(check_name = n) %>% 
        mutate(check_desc = d)
    } else if(!distinct_visits & !visits_only) {
      visit_cts <-
        visits_narrowed %>%
        group_by(month_end) %>%
        summarise(row_cts = n(),
                  row_pts = n_distinct(person_id)) %>%
        collect() %>%  
        ungroup()  %>% 
        add_meta(check_lib=check_string) %>%
        mutate(check_name = n) %>% 
        mutate(check_desc = d)
    }
    
    final_results[[paste0(check_string, '_', n)]] = visit_cts 
    
  }
  
  final_results
  
}
