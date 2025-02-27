
#' computes concordance of patients
#' for a site; computes total and across sites
#'
#' @param conc_tbls list with each element containing the following:
#' list name: the description of the concordant domains
#' first element: the first domain
#' second element: the second domain
#' third element: the check name/application
#' @param patients2 cohort of patients; needs to have
#' a column called `yr` that extracts date field
#' @param check_string the check string to be added to the function
#' call `add_meta`
#'
#' @return a tbl with the following columns:
#' `site`;
#' `yr` calendar year to look at concordance; defaults to `9999`
#' if a total combined count
#' `cohort_ct` takes values `patients_ct` and `overlap_ct_byyr`
#' `value` the total count of patients for cohort_ct and cohort
#' `cohort` takes on values `cohort_1`, `cohort_2`, and `combined`
#'

check_dcon_pts <- function(conc_tbls,
                           check_string='dcon_pts'){



  final <- list()

  for(k in 1:length(conc_tbls)) {
  combined_list_yr <- list()
  combined_list_total <- list()

  pt_list_final <- list()

  pt_list <- list(conc_tbls[[k]][[1]],
                  conc_tbls[[k]][[2]])

  for(i in 1:length(pt_list)) {

    date_yr_thisrnd <-  pt_list[[i]] %>% select(ends_with('_date')) %>%
      select(-c(contains('end'), contains('order'))) %>% colnames()

    pts <- pt_list[[i]] %>% mutate(date_yr = !! sym(date_yr_thisrnd)) %>%
      mutate(yr=sql("extract(year from date_yr)")) %>%
      group_by(yr) %>% distinct(person_id,
                                yr) %>% compute_new(indexes=list('person_id'))

    pt_list_final[[i]] <- pts

  }


  for(i in 1:length(pt_list_final)){


    combined_list_byyr <-
      pt_list_final[[i]] %>%
      summarise(patients_ct=
                  n_distinct(person_id)) %>%
      collect() %>% pivot_longer(cols=patients_ct,names_to='cohort_ct') %>%
      mutate(cohort=paste0('cohort_',i))

    combined_list_ct <-
      pt_list_final[[i]] %>%
      ungroup() %>%#group_by(site) %>%
      summarise(patients_ct=
                  n_distinct(person_id)) %>%
      collect() %>% pivot_longer(cols=patients_ct,names_to='cohort_ct') %>%
      mutate(cohort=paste0('cohort_',i))  %>%
      mutate(yr=9999)

    combined_list_yr[[i]] <- combined_list_byyr
    combined_list_total[[i]] <- combined_list_ct

  }

  pt1_pt2_yrs <- reduce(.x=combined_list_yr,
                        .f=dplyr::union)
  pt1_pt2_total <- reduce(.x=combined_list_total,
                          .f=dplyr::union)

  pats1_pats2_overlap <-
    pt_list_final[[1]] %>%
    inner_join(pt_list_final[[2]])

  pats1_pats2_overlap_byyear <- pats1_pats2_overlap %>%
    summarise(overlap_ct_byyr=n_distinct(person_id)) %>%
    ungroup() %>% collect() %>%
    pivot_longer(cols=overlap_ct_byyr,names_to='cohort_ct') %>%
    mutate(cohort='combined')
  pats1_pats2_overlap_overall <- pats1_pats2_overlap %>%
    ungroup()%>%
    summarise(value=n_distinct(person_id))%>%
    collect()%>%
    mutate(cohort='combined',
           yr=9999,
           cohort_ct='overlap_ct_byyr')


  combined <-
    dplyr::union(pt1_pt2_yrs,
                 pt1_pt2_total) %>%
    dplyr::union(pats1_pats2_overlap_byyear) %>%
    dplyr::union(pats1_pats2_overlap_overall)%>%
    add_meta(check_lib = check_string) %>%
    mutate(check_name=conc_tbls[[k]][[3]]) %>%
    mutate(check_desc=names(conc_tbls[k]))

  final[[k]] <- combined

  }

  final
}

#' metadata for dcon check
#'
#' @param conc_tbls_meta a list of lists that must contain the
#' following elements:
#' 1) each name of the list of lists must have the same name as the `check_dcon_pts` function
#' 2) within each list, a second list with the following format:
#'    --- `cohort label (default can be `cohort_1`)` = `cohort description (written description of the cohort)`
#' @param check_string the check type in a string
#'
#' @return a metadata table with the following columns:
#' `check_type`, `check_name`, `check_label`, `cohort`
#'
#' Note that EACH dcon check will automatically output two rows for each check (assuming 2 domains)
#'

check_dcon_pts_meta <- function(conc_tbls_meta,
                                check_string='dcon_pts') {

  final <- list()

  for(i in 1:length(conc_tbls_meta)) {

    cohorts <- names(conc_tbls_meta[[i]])
    cohort_desc <- unlist(conc_tbls_meta[[i]], use.names = FALSE)

    meta_tbl <- tibble(check_type=check_string,
                       check_name=names(conc_tbls_meta[i]),
                       cohort_label=cohorts,
                       cohort=cohort_desc)
    final[[i]] <- meta_tbl
  }
  reduce(.x=final,
         .f=dplyr::union)
}



#' Compute domain concordance between 2 cohorts
#'
#' @param conc_tbls list of inputs from `dcon_execute.R` with each element containing 
#'                  the following:
#'                  list name: the description of the concordant domains
#'                  first element: table with at least person_id OR visit_occurrence_id
#'                                 that represents all members of the first cohort
#'                  second element: table with at least person_id OR visit_occurrence_id
#'                                 that represents all members of the second cohort
#'                  third element: the check name/application
#' @param check_string a string that denotes the level at which the analysis should
#'                     take place 
#'                     
#'                     if it is `dcon_visits`, the analysis will take place
#'                     at the visit level; otherwise it will take place at the
#'                     person level
#'
#' @return one dataframe with counts for the patients/visits in the first cohort, 
#'         the patients/visits in the second cohort, and the patients/visits in both
#'         
#'         contains the columns: value, cohort, yr (set to 9999), check_type, 
#'                               database_version, site, check_name, check_desc 
#'         
#' 
check_dcon<- function(conc_tbls,
                      check_string='dcon_visits'){
  
  
  
  final <- list()
  
  for(k in 1:length(conc_tbls)) {
    
    c1_date <- colnames(conc_tbls[[k]][[1]]) %>% str_subset(pattern = 'date') %>% first() #%>% pull()
    c2_date <- colnames(conc_tbls[[k]][[2]]) %>% str_subset(pattern = 'date') %>% first() #%>% pull()
    
    cohort_1 <- conc_tbls[[k]][[1]] %>% mutate(date1 = !!sym(c1_date))
    cohort_2 <- conc_tbls[[k]][[2]] %>% mutate(date2 = !!sym(c2_date))
    
    if(check_string=='dcon_visits'){
      col_nm <- sym('visit_occurrence_id')
    } else{col_nm <- sym('person_id')}
    
    if(check_string != 'dcon_visits'){
      
      days_diff_integer <- conc_tbls[[k]][[4]]
      
      combined <- 
        cohort_1 %>% select(site, all_of(col_nm), date1) %>% 
        inner_join(
          select(cohort_2, site, all_of(col_nm), date2)
        ) %>%
        mutate(date_diff = sql(calc_days_between_dates(date_col_1 = 'date2', 
                                                       date_col_2 = 'date1')),
               date_diff = abs(as.numeric(date_diff))) %>%
        filter(date_diff <= days_diff_integer)
      
    }else{
      
      combined <- 
        cohort_1 %>% select(all_of(col_nm)) %>% 
        inner_join(
          select(cohort_2, all_of(col_nm))
        )
      
    }
    
    cohort_list <- list('cohort_1' = cohort_1,
                        'cohort_2' = cohort_2,
                        'combined' = combined)
    cohort_list_cts <- list()
    
    for(i in 1:length(cohort_list)) {
      
      string_nm <- names(cohort_list[i])
      
      final_cts <- cohort_list[[i]] %>% 
        summarise(value=n_distinct(col_nm)) %>% 
        collect() %>% 
        mutate(cohort = string_nm)
      
      cohort_list_cts[[i]] <- final_cts
      
    }
    
    nm <- conc_tbls[[k]][[3]]
    d <- names(conc_tbls[k])
    
    final_tbls <- 
      reduce(.x=cohort_list_cts,
             .f=dplyr::union) %>% 
      #mutate(yr=9999) %>% 
      add_meta(check_lib = 'dcon') %>%
      mutate(check_name=nm,
             check_desc=d) %>% collect()
    
    final[[k]] <- final_tbls
    
  }
  
  final
  
}


#' Check domain concordance over time for 2 cohorts
#'
#' @param conc_tbls conc_tbls list of inputs from `dcon_execute.R` with each element containing 
#'                  the following:
#'                  list name: the description of the concordant domains
#'                  first element: table with at least person_id OR visit_occurrence_id
#'                                 that represents all members of the first cohort
#'                  second element: table with at least person_id OR visit_occurrence_id
#'                                 that represents all members of the second cohort
#'                  third element: the check name/application
#' @param check_string a string that denotes the level at which the analysis should
#'                     take place 
#'                     
#'                     if it is `dcon_visits`, the analysis will take place
#'                     at the visit level ONLY; otherwise it will take place at both
#'                     the visit & person level
#'
#' @return one dataframe with counts for the patients/visits in the first cohort and 
#'         the patients/visits in the second cohort for each year in `time_span_yr`
#'         
#'         contains the columns: value, cohort, yr, check_type, database_version, 
#'                               site, check_name, check_desc 
#' 
check_dcon_overtime <- function(conc_tbls,
                                check_string='dcon_visits') {
  
  final <- list()
  
  for(k in 1:length(conc_tbls)) {
    
    
    cohort_1 <- conc_tbls[[k]][[1]]
    cohort_2 <- conc_tbls[[k]][[2]]
    
    test_fot <- 
      check_fot(time_tbls = list('cohort_1'= list(cohort_1,'cohort_1'),
                                 'cohort_2'= list(cohort_2,'cohort_2')),
                time_frame = time_span_yr,
                visits_only = ifelse(check_string=='dcon_visits',TRUE,FALSE),
                lookback_months = 12) 
    
    if(check_string == 'dcon_visits'){
      
    fot_reduce <- reduce(.x=test_fot,
                         .f=dplyr::union) %>% 
      collect() %>% 
      mutate(yr=year(month_end),
             value_pts=as.numeric(NA)) %>% 
      rename(cohort=check_desc,
             value_visits=row_visits) %>% 
      select(value_pts,value_visits,cohort,yr) %>% 
      add_meta(check_lib = check_string) %>%
      mutate(check_name=conc_tbls[[k]][[3]]) %>%
      mutate(check_desc=names(conc_tbls[k]))
    
    }else{
      
      fot_reduce <- reduce(.x=test_fot,
                           .f=dplyr::union) %>% 
        collect() %>% 
        mutate(yr=year(month_end)) %>% 
        rename(cohort=check_desc,
               value_visits=row_visits,
               value_pts=row_pts) %>% 
        select(value_pts,value_visits,cohort,yr) %>% 
        add_meta(check_lib = check_string) %>%
        mutate(check_name=conc_tbls[[k]][[3]]) %>%
        mutate(check_desc=names(conc_tbls[k]))
      
    }
    
    final[[k]] <- fot_reduce
    
  }
  
  final
  
  
}

#' Identify specialties for relevant couplets
#'
#' @param visits CDM visit occurrence table
#' @param specialty_conceptset concept set with relevant specialties
#'
#' @return table of visits with their associated specialties, prioritizing provider
#'         specialty and using care site specialty where provider specialty is not available
#'         or not informative
#' 
find_specialty <- function(visits,
                           specialty_conceptset) {
  prov_informative <- cdm_tbl('provider') %>% 
    inner_join(specialty_conceptset, by = c('specialty_concept_id' = 'concept_id')) %>% 
    select(provider_id, prov_specialty = specialty_concept_id) %>% compute_new()
  cs_informative <- cdm_tbl('care_site') %>% 
    inner_join(specialty_conceptset, by = c('specialty_concept_id' = 'concept_id')) %>%
    select(care_site_id, cs_specialty = specialty_concept_id) %>% compute_new()
  
  new_tbl <- visits %>% 
    left_join(prov_informative, by = 'provider_id') %>% 
    left_join(cs_informative, by = 'care_site_id') %>% 
    filter(!is.na(prov_specialty) | !is.na(cs_specialty)) %>%
    mutate(visit_specialty_concept_id =
             case_when(prov_specialty != 38004477L ~ prov_specialty,
                       cs_specialty != 38004477L ~ cs_specialty,
                       prov_specialty == 38004477L ~ 38004477L,
                       cs_specialty == 38004477L ~ 38004477L,
                       TRUE ~ 0L)) %>% 
    select(-prov_specialty, -cs_specialty) %>% compute_new()
  
  return(new_tbl)
}
