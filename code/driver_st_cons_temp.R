

# Top-level code for execution of data request

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(rlang))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(readr))

# Required for execution using Rscript
suppressPackageStartupMessages(library(methods))

#' Set up the execution environment
#'
#' The .load() function sources the R files needed to execute the query
#' and sets up the execution environment.  In particular, all of the base
#' framework files, as well as files inthe code_dir with names matching
#' `cohort_*.R` or `analyze_*.R` will be sourced.
#'
#' This function is usually run automatically when the `run.R` file is sourced
#' to execute the request.  It may also be executed manually during an
#' interactive session to re-source changed code or to re-establish a connection
#' to the database.
#'
#' **N.B.** You will almost never have to edit this function.
#'
#' @param here The name of the top-level directory for the request.  The default
#'   is `config('base_dir')` if the config function has been set up, or the
#'   global variable `base_dir` if not.
#'
#' @return The value of `here`.
#' @md
.load <- function(here = ifelse(typeof(get('config')) == 'closure',
                                config('base_dir'), base_dir)) {
  source(file.path(here, 'code', 'config.R'))
  source(file.path(here, 'code', 'req_info.R'))
  source(config('site_info'))
  source(file.path(here, config('subdirs')$code_dir, 'setup.R'))
  source(file.path(here, config('subdirs')$code_dir, 'codesets.R'))
  for (fn in list.files(file.path(here, config('subdirs')$code_dir),
                        'util_.+\\.R', full.names = TRUE))
    source(fn)
  for (fn in list.files(file.path(here, config('subdirs')$code_dir),
                        'cohort_.+\\.R', full.names = TRUE))
    source(fn)
  for (fn in list.files(file.path(here, config('subdirs')$code_dir),
                        'analyze_.+\\.R', full.names = TRUE))
    source(fn)
  source(file.path(here, config('subdirs')$code_dir, 'cohorts.R'))
  
  .env_setup()
  
  for (def in c('retain_intermediates', 'results_schema')) {
    if (is.na(config(def)))
      config(def, config(paste0('default_', def)))
  }
  
  here
}

#' Execute the request
#'
#' This function presumes the environment has been set up, and executes the
#' steps of the request.
#'
#' In addition to performing queries and analyses, the execution path in this
#' function should include periodic progress messages to the user, and logging
#' of intermediate totals and timing data through [append_sum()].
#'
#' This function is also typically executed automatically, but is separated from
#' the setup done in [.load()] to facilitate direct invocation during
#' development and debugging.
#'
#' @param base_dir The name of the top-level directory for the request.  The default
#'   is `config('base_dir')`, which should always be valid after execution of
#'   [.load()].
#'
#' @return The return value is dependent on the content of the request, but is
#'   typically a structure pointing to some or all of the retrieved data or
#'   analysis results.  The value is not used by the framework itself.
#' @md
.run  <- function(base_dir = config('base_dir')) {
  
  message('Starting execution with framework version ',
          config('framework_version'))
  
  
  time_tbls_list = list(
    
    'visits_grouped' = site_cdm_tbl('visit_occurrence') %>% group_by(visit_concept_id),
    'procs' = site_cdm_tbl('procedure_occurrence') %>% 
      inner_join(select(
        site_cdm_tbl('visit_occurrence'),
        visit_occurrence_id, visit_concept_id
      )) %>% filter(visit_concept_id == 9202L),
    'icu_visits' = site_cdm_tbl('adt_occurrence'),
    'drugs_grouped' = site_cdm_tbl('drug_exposure') %>% group_by(drug_type_concept_id),
    'conditions_grouped' = site_cdm_tbl('condition_occurrence') %>% group_by(condition_type_concept_id),
    'outpatient_labs' = site_cdm_tbl('measurement_labs') %>% 
      inner_join(select(
        site_cdm_tbl('visit_occurrence'),
        visit_occurrence_id, visit_concept_id
      )) %>% filter(visit_concept_id == 9202L)
    
  )
  
  time_tbls_list_mini = list(
    
    'visits_grouped' = site_cdm_tbl('visit_occurrence') %>% group_by(visit_concept_id),
    'procs' = site_cdm_tbl('procedure_occurrence') %>% 
      inner_join(select(
        site_cdm_tbl('visit_occurrence'),
        visit_occurrence_id, visit_concept_id
      )) %>% filter(visit_concept_id == 9202L)
    
  )
  
  time_tbls_meta <- list(
    
    'visits_grouped' = 'All visits grouped by visit_concept_id',
    'procs' = 'All procedurers joined to visit_concept_id == 9202',
    'icu_visits' = 'All ICU',
    'drugs_grouped' = 'Drugs grouped by drug_type_concept_id',
    'conditions_grouped' = 'Conditions grouped by condition_type_concept_id',
    'outpatient_labs' = 'All labs joined to visit_concept_id == 9202'
    
  )
  
  time_tbls_meta_mini <- list(
    
    'visits_grouped' = 'All visits grouped by visit_concept_id',
    'procs' = 'All procedurers joined to visit_concept_id == 9202'
    
  )
  
  
time_span_list_output <- 
  as.character(seq(as.Date('2009-02-01'), length = 155, by='months')-1)

time_tbl_list_output_mini <- 
  as.character(seq(as.Date('2020-02-01'), length = 3, by='months')-1)
  
time_span <- c(time_span_list_output)
time_span_mini <- c(time_tbl_list_output_mini)
  
  
  ## Stanford
  
  config('cdm_schema', 'stanford_pedsnet')
  config('site', 'stanford')
  config('site_filter', NA)
  config('site_filter_previous', 'stanford')
  
  stanford_st_cons_temp <- 
    check_st_cons_temp(time_tbls = time_tbls_list,
                       meta_tbls = time_tbls_meta,
                       time_frame = time_span)
  
  output_list_to_db(stanford_st_cons_temp)
  
 
  
  
  ## Seattle
  
  config('cdm_schema', 'seattle_pedsnet')
  config('site', 'seattle')
  config('site_filter', NA)
  config('site_filter_previous', 'seattle')
  
  seattle_st_cons_temp <- 
    check_st_cons_temp(time_tbls = time_tbls_list,
                       meta_tbls = time_tbls_meta,
                       time_frame = time_span)
  
  output_list_to_db(seattle_st_cons_temp)
  
  ## chop
  
  config('cdm_schema', 'chop_pedsnet')
  config('site', 'chop')
  config('site_filter', NA)
  config('site_filter_previous', 'chop')
  
  chop_st_cons_temp <- 
    check_st_cons_temp(time_tbls = time_tbls_list,
                       meta_tbls = time_tbls_meta,
                       time_frame = time_span)
  
  output_list_to_db(chop_st_cons_temp)
  
  ## nemours
  
  config('cdm_schema', 'nemours_pedsnet')
  config('site', 'nemours')
  config('site_filter', NA)
  config('site_filter_previous', 'nemours')
  
  nemours_st_cons_temp <- 
    check_st_cons_temp(time_tbls = time_tbls_list,
                       meta_tbls = time_tbls_meta,
                       time_frame = time_span)
  
  output_list_to_db(nemours_st_cons_temp)
  
  
  ## nationwide
  
  config('cdm_schema', 'nationwide_pedsnet')
  config('site', 'nationwide')
  config('site_filter', NA)
  config('site_filter_previous', 'nationwide')
  
  nationwide_st_cons_temp <- 
    check_st_cons_temp(time_tbls = time_tbls_list,
                       meta_tbls = time_tbls_meta,
                       time_frame = time_span)
  
  output_list_to_db(nationwide_st_cons_temp)
  
  
  ## colorado
  
  config('cdm_schema', 'colorado_pedsnet')
  config('site', 'colorado')
  config('site_filter', NA)
  config('site_filter_previous', 'colorado')
  
  colorado_st_cons_temp <- 
    check_st_cons_temp(time_tbls = time_tbls_list,
                       meta_tbls = time_tbls_meta,
                       time_frame = time_span)
  
  output_list_to_db(colorado_st_cons_temp)
  
  
  ## lurie
  
  config('cdm_schema', 'lurie_pedsnet')
  config('site', 'lurie')
  config('site_filter', NA)
  config('site_filter_previous', 'lurie')
  
  lurie_st_cons_temp <- 
    check_st_cons_temp(time_tbls = time_tbls_list,
                       meta_tbls = time_tbls_meta,
                       time_frame = time_span)
  
  output_list_to_db(lurie_st_cons_temp)
  
  ## cchmc
  
  config('cdm_schema', 'cchmc_pedsnet')
  config('site', 'cchmc')
  config('site_filter', NA)
  config('site_filter_previous', 'cchmc')
  
  cchmc_st_cons_temp <- 
    check_st_cons_temp(time_tbls = time_tbls_list,
                       meta_tbls = time_tbls_meta,
                       time_frame = time_span)
  
  output_list_to_db(cchmc_st_cons_temp)
  
  
  ##### META TABLE 
  
  versions_tbls_list = list(
    
    'st_cons_temp_visits_grouped' = results_tbl('st_cons_temp_visits_grouped') %>% collect(),
    'st_cons_temp_procs' = results_tbl('st_cons_temp_procs') %>% collect(),
    'st_cons_temp_icu_visits' = results_tbl('st_cons_temp_icu_visits') %>% collect(),
    'st_cons_temp_drugs_grouped' = results_tbl('st_cons_temp_drugs_grouped') %>% collect(),
    'st_cons_temp_conditions_grouped' = results_tbl('st_cons_temp_conditions_grouped') %>% collect(),
    'st_cons_temp_outpatient_labs' = results_tbl('st_cons_temp_outpatient_labs') %>% collect()
    
  )
  
  time_tbls_meta <- list(
    
    'visits_grouped' = 'All visits grouped by visit_concept_id',
    'procs' = 'All procedurers joined to visit_concept_id == 9202',
    'icu_visits' = 'All ICU',
    'drugs_grouped' = 'Drugs grouped by drug_type_concept_id',
    'conditions_grouped' = 'Conditions grouped by condition_type_concept_id',
    'outpatient_labs' = 'All labs joined to visit_concept_id == 9202'
    
  )
  
  
  meta_tbls_st_cons_temp <- 
    compute_meta_tbl(meta_tbls = time_tbls_meta,
                     versions_tbl_list = versions_tbls_list,
                     check_string = 'st_cons_temp')
  
  output_tbl(
    meta_tbls_st_cons_temp,
    'st_cons_temp_meta'
  )
  
  

  
  # Getting all unique visits in the visit_occurrence tbl 
  #total_visit_occurrences <- 
  # cdm_tbl('visit_occurrence') %>% 
  # First version of function
  # group_vars(cohort = .,
  #           group_log=TRUE, 
  #           grouped_vars=list('site')) %>% 
  # summarize(all_visits = n_distinct(visit_occurrence_id))
  
  # ST-COMP-Facts: Checking completeness in facts for visits 
  # Visits are considered complete when there is a visit_occurrence in procedure_occurrence OR condition_occurrence OR measurement (should this be AND?)
  #check_st_comp_facts_records(
  # list('procedure_occurrence',
  #      'condition_occurrence',
  #     'measurement')) %>% 
  # First version of function
  # group_vars(cohort = .,
  #          group_log=TRUE, 
  #          grouped_vars=list('site')) %>% 
  # summarize(visits_checks = n_distinct(visit_occurrence_id)) %>% # Count of visit_occurrence_id's that are not in any of the three tbls
  #  inner_join(total_visit_occurrences, by='site') %>% 
  #  ungroup() %>% 
  #  mutate(percent = round(((visits_checks / as.numeric(all_visits)) *100), digits = 2)) %>% # Percentage of visit_occurrence_id's that are not present in any of the tbls above
  #  output_tbl('st_comp_facts',
  #   db=TRUE,
  #   file=FALSE)
  
  # ST-COMP-PatientRecords: Checking completeness in patient records 
  # Patients records are considered complete when there is a visit_occurrence in drug_exposure OR procedure_occurrence OR measurement (should this be AND?) 
  # check_st_comp_facts_records(
  #  list('drug_exposure',
  #      'procedure_occurrence',
  #      'measurement')) %>%
  # First version of function
  # group_vars(cohort = .,
  #            group_log=TRUE, 
  #            grouped_vars=list('site')) %>% 
  #  summarize(visits_checks = n_distinct(visit_occurrence_id)) %>% # count of visit_occurrence_id's that are not in any of the three tbls 
  #  inner_join(total_visit_occurrences, by='site') %>% 
  #  ungroup() %>% 
  #  mutate(percent = round(((visits_checks / as.numeric(all_visits)) *100), digits = 2)) %>% # Percentage of visit_occurrence_id's that are not present in any of the tbls above 
  #  output_tbl('st_comp_patientrecords',
  #          db=TRUE,
  #          file=FALSE)
  
  
}

#' Set up and execute a data request
#'
#' This function encapsulates a "production" run of the data request.  It sets
#' up the environment, executes the request, and cleans up the environment.
#'
#' Typically, the `run.R` file calls run_request() when in a production mode.
#'
#' @param base_dir Path to the top of the data request files.  This is
#'   typically specified in `run.R`.
#'
#' @return The result of [.run()].
#' @md
run_request <- function(base_dir) {
  base_dir <- .load(base_dir)
  on.exit(.env_cleanup())
  .run(base_dir)
}
