

#' Finds specialties from the provier and care site tables; 
#' If no specialties are found, returns negative integers
#' @md
#' 
#' @param specialty_types a field in a database table (from load_codeset()) that 
#' specifies the concept_id of the specialties of interest.
#' 
#' **** The codeset MUST contain a column called `specialty_flag` which corresponds
#'        to the specialty for a given concept_id (concept_id is the specialty-cocnept_id here)
#' 
#' @return A list composed of two tables. 
#' The first is the provider tbl filtered by provider specialties
#' and the second is the care_site tbl filtered by care_site specialties.
#' The structure of the table is primary_key (provider_id or care_site_id) | specialty_cocnept_id | specialty_flag 

find_specialties <- function(specialty_codeset) {
  
  
  specialty_list <- 
    list(provider = select(cdm_tbl('provider'), 
                           provider_id, specialty_concept_id) %>%
           inner_join(select(specialty_codeset,concept_id,specialty_flag),
                      by=c('specialty_concept_id'='concept_id')) %>% #collect() %>%
           mutate(specialty_type = 'provider')
         ,
         care_site = select(cdm_tbl('care_site'), 
                            care_site_id, specialty_concept_id) %>%
           inner_join(select(specialty_codeset,concept_id,specialty_flag),
                      by=c('specialty_concept_id'='concept_id')) %>% #collect() %>%
           mutate(specialty_type = 'care_site'))
  
  
 # if(dim(specialty_list$provider)[1] == 0) {specialty_list$provider[1, ] =
  #  tibble(provider_id = as.integer(-123), specialty_concept_id = as.integer(-456))}
 # if(dim(specialty_list$care_site)[1] == 0) {specialty_list$care_site[1, ] = 
 #   tibble(care_site_id = as.integer(-123), specialty_concept_id = as.integer(-456))}
  
  specialty_list
  
}



#' Finds all visits where a specialist of interest is identified (either care_site or provider)
#' @md
#' 
#' @param visit_types is a vector limiting the visits of interest
#' @param specialty_codeset is a vector of concept_id's that will be used in the '.find_specialties()'
#' function defined elsewhere. It represents the specialties of interest.
#' 
#' **** The codeset MUST contain a column called `specialty_flag` which corresponds
#'        to the specialty for a given concept_id (concept_id is the specialty-cocnept_id here)
#' @param included_cohort_flag a logical parameter indicating that the function should look 
#' for specialties for just a specific cohort. Defaults to `FALSE`.
#' @param included_cohort the cohort for which to find specialties. Must include `person_id`
#' 
#' @param visit_tbl is the cdm_tbl where visits are identified.
#' 
#' @return all visits that are specialist visits
#' 

find_specialty_visits <- function(included_cohort,
                                  visit_types = c(9202L),
                                  specialty_codeset = load_codeset('rheum_test',
                                                                   col_types = 'icccc'),
                                  visit_tbl = cdm_tbl('visit_occurrence'),
                                  included_cohort_flag = FALSE) {
  
  all_specs <-
    find_specialties(specialty_codeset = 
                        specialty_codeset)
  
  
  visit_tbl_narrow <-
    select(visit_tbl,
           person_id,
           visit_occurrence_id,
           provider_id,
           care_site_id)
  
  if(! included_cohort_flag) {
    
    visit_tbl_narrow <- visit_tbl_narrow 
  }
  
    else {
      
      included_cohort_distinct <-
        select(included_cohort,person_id) %>% distinct()
      
      visit_tbl_narrow <- 
        visit_tbl_narrow %>%
        inner_join(included_cohort_distinct) 
    }
  
  
  specs_list <- 
    list(provider = visit_tbl_narrow %>%
                      inner_join(all_specs[[1]],by='provider_id') %>%
                      mutate(provider = 1L) %>% 
                      compute_new(temporary=TRUE,
                                  indexes=list('visit_occurrence_id')),
         care_site = visit_tbl_narrow %>%
                      inner_join(all_specs[[2]],by='care_site_id') %>%
                      mutate(care_site = 1L) %>%
                      compute_new(temporary=TRUE,
                                  indexes=list('visit_occurrence_id')))
  
  combined_visits <- 
    dplyr::union(select(specs_list[[1]],person_id,visit_occurrence_id),
                select(specs_list[[2]],person_id,visit_occurrence_id))
  
  visits_with_flags <- 
    combined_visits %>%
    left_join(select(specs_list[[1]],person_id,visit_occurrence_id,specialty_flag,provider)) %>%
    left_join(select(specs_list[[2]],person_id,visit_occurrence_id,specialty_flag,care_site))
  
}

#' Finds all specialties for a given cohort of patients
#' @md
#' 
#' @param included_cohort the cohort for which to find specialties
#' @param visit_types vector containing visit types for specialties. Defaluts to 9202.
#' @param visit_tbl the table containing the visit information. Defaults to `cdm_tbl('visit_occurrence')`
#' @return database tbl with the following columns: visit_occurrence_id | person_id | visit_concept_id |
#' provider_id | care_site_id | provider_specialty | care_site_specialty 

find_all_specialties <- function(included_cohort,
                                 visit_types = c(9202L),
                                 visit_tbl = cdm_tbl('visit_occurrence')) {
  
  visits_narrowed <-  
    visit_tbl %>%
    inner_join(included_cohort) %>%
    filter(visit_concept_id %in% visit_types) %>%
    select(visit_occurrence_id,
           person_id,
           visit_concept_id,
           provider_id,
           care_site_id) %>%
    compute_new(temporary=TRUE,
                indexes=list('provider_id',
                             'care_site_id'))
  
  visits_narrowed_specialties <-
    visits_narrowed %>%
    left_join(select(cdm_tbl('provider'),
                     provider_id,specialty_concept_id),
              by='provider_id') %>%
    rename(provider_specialty = specialty_concept_id) %>%
    left_join(select(cdm_tbl('care_site'),
                     care_site_id,specialty_concept_id),
              by='care_site_id') %>%
    rename(care_site_specialty = specialty_concept_id)
  
}


#' 
#' 
#' 
#' 
#' 
#' 
#' 

find_specialty_visits_alternative <- function(visit_types = c(9202L),
                                              specialty_codeset = load_codeset('rheum_test',
                                                                   col_types = 'icccc'),
                                              visit_tbl = cdm_tbl('visit_occurrence')) {
  
  all_specs <-
    .find_specialties(specialty_codeset = 
                        specialty_codeset)
  
  results <- list()
  ct <- 0
  
  for(i in all_specs[1:length(all_specs)]) {
    
    index_var <- i[1][,1] 
    
    #index_var <- i[,1] 
    index_name <- as_string(names(index_var))
    
    ct <- ct + 1
    
    i_compute <- 
      i %>%
      copy_to_new(df=.,
                  name = paste0('ct_',ct),
                  indexes=list(index_name)) 
    
    result <- 
      select(visit_tbl,visit_occurrence_id,
             visit_concept_id,provider_id,care_site_id)%>%
      inner_join(i_compute) %>%
      filter(visit_concept_id %in% visit_types) 
    
    
    results[[ct]] <- result
    
  }
  
  
  
  all_visits <- results[[1]] %>%
    select(visit_occurrence_id,specialty_flag)
  
  for (dfs in results[2:length(results)]) {
    dfs <- select(dfs,visit_occurrence_id,specialty_flag)
    all_visits <- dplyr::union(all_visits,
                               dfs)
  }
  
  for (k in results[1:length(results)]) {
    
    new_col <-
      k %>% 
      select(specialty_type) %>% 
      distinct() %>%
      pull()
    
    k_select <- 
      k %>%
      select(visit_occurrence_id,
             specialty_flag)
    
    all_visits <- 
      all_visits %>% 
      left_join(k_select) %>%
      mutate(!! sym(new_col) := 1L)
  }
  
  all_visits
  
}


