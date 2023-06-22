

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
  
  concept_list_full <- 
    list(
      'all drugs' = list(site_cdm_tbl('drug_exposure'), 'drug_concept_id', 'drug_source_value'),
      'inpatient administrations' = list(site_cdm_tbl('drug_exposure') %>% 
                                           filter(drug_type_concept_id == 38000180L),
                                         'drug_concept_id', 'drug_source_value'),
      'prescription drugs' = list(site_cdm_tbl('drug_exposure') %>% 
                                    filter(drug_type_concept_id == 38000177L),
                                  'drug_concept_id', 'drug_source_value'),
      'conditions' = list(site_cdm_tbl('condition_occurrence'),
                          'condition_concept_id', 'condition_source_value'),
      'all labs' = list(site_cdm_tbl('measurement_labs'),
                                'measurement_concept_id', 'measurement_source_value'),
      'immunizations' = list(site_cdm_tbl('immunization'),
                              'immunization_concept_id', 'immunization_source_value')
    )
  
  concept_list_procedures <- 
    list(
      'all procedures' = list(site_cdm_tbl('procedure_occurrence'), 'procedure_concept_id','procedure_source_value')
    )
  
  
  ## Stanford
  
  config('cdm_schema', 'stanford_pedsnet')
  config('site', 'stanford')
  config('site_filter', 'stanford')
  config('site_filter_previous', 'stanford')
  
  stanford_st_conf_concepts_unmapped <- 
    check_st_conf_concepts_unmapped(concept_list_full)
  
  stanford_st_conf_concepts <- 
    reduce(.x=stanford_st_conf_concepts_unmapped,
           .f=dplyr::union)
  
  stanford_st_conf_concepts_by_year <- 
    check_st_conf_concepts_by_year(concept_list_full)
  
  stanford_st_conf_concepts_by_year <- 
    reduce(.x=stanford_st_conf_concepts_by_year,
           .f=dplyr::union)
  
  output_tbl_append(stanford_st_conf_concepts,
                    'st_conf_concepts_unmapped')
  
  output_tbl_append(stanford_st_conf_concepts_by_year,
                    'st_conf_concepts_unmapped_by_year')
  ###
  
  stanford_st_conf_procs <- 
    check_st_conf_concepts_unmapped(concept_list_procedures)
  
  stanford_st_conf_procs_concepts <- 
    reduce(.x=stanford_st_conf_procs,
           .f=dplyr::union)
  
  stanford_st_conf_procs_concepts_by_year <- 
    check_st_conf_concepts_by_year(concept_list_procedures)
  
  stanford_st_conf_procs_concepts_by_year <- 
    reduce(.x=stanford_st_conf_procs_concepts_by_year,
           .f=dplyr::union)
  
  output_tbl_append(stanford_st_conf_procs_concepts,
                    'st_conf_concepts_unmapped')
  
  output_tbl_append(stanford_st_conf_procs_concepts_by_year,
                    'st_conf_concepts_unmapped_by_year')
  
  
  

  
  
  
  ## Seattle
  
  config('cdm_schema', 'seattle_pedsnet')
  config('site', 'seattle')
  config('site_filter', 'seattle')
  config('site_filter_previous', 'seattle')
  
  seattle_st_conf_concepts_unmapped <- 
    check_st_conf_concepts_unmapped(concept_list_full)
  
  seattle_st_conf_concepts_by_year <- 
    check_st_conf_concepts_by_year(concept_list_full)
  
  seattle_st_conf_concepts <- 
    reduce(.x=seattle_st_conf_concepts_unmapped,
           .f=dplyr::union)
  
  seattle_st_conf_concepts_by_year <- 
    reduce(.x=seattle_st_conf_concepts_by_year,
           .f=dplyr::union)
  
  output_tbl_append(seattle_st_conf_concepts,
                    'st_conf_concepts_unmapped')
  
  output_tbl_append(seattle_st_conf_concepts_by_year,
                    'st_conf_concepts_unmapped_by_year')
  
  ###
  
  seattle_st_conf_procs <- 
    check_st_conf_concepts_unmapped(concept_list_procedures)
  
  seattle_st_conf_procs_concepts <- 
    reduce(.x=seattle_st_conf_procs,
           .f=dplyr::union)
  
  seattle_st_conf_procs_concepts_by_year <- 
    check_st_conf_concepts_by_year(concept_list_procedures)
  
  seattle_st_conf_procs_concepts_by_year <- 
    reduce(.x=seattle_st_conf_procs_concepts_by_year,
           .f=dplyr::union)
  
  output_tbl_append(seattle_st_conf_procs_concepts,
                    'st_conf_concepts_unmapped')
  
  output_tbl_append(seattle_st_conf_procs_concepts_by_year,
                    'st_conf_concepts_unmapped_by_year')
  
  
  ## chop
  
  config('cdm_schema', 'chop_pedsnet')
  config('site', 'chop')
  config('site_filter', 'chop')
  config('site_filter_previous', 'chop')
  
  chop_st_conf_concepts_unmapped <- 
    check_st_conf_concepts_unmapped(concept_list_full)
  
  chop_st_conf_concepts_by_year <- 
    check_st_conf_concepts_by_year(concept_list_full)
  
  chop_st_conf_concepts <- 
    reduce(.x=chop_st_conf_concepts_unmapped,
           .f=dplyr::union)
  
  chop_st_conf_concepts_by_year <- 
    reduce(.x=chop_st_conf_concepts_by_year,
           .f=dplyr::union)
  
  output_tbl_append(chop_st_conf_concepts,
                    'st_conf_concepts_unmapped')
  
  output_tbl_append(chop_st_conf_concepts_by_year,
                    'st_conf_concepts_unmapped_by_year')
  
  ###
  
  chop_st_conf_procs <- 
    check_st_conf_concepts_unmapped(concept_list_procedures)
  
  chop_st_conf_procs_concepts <- 
    reduce(.x=chop_st_conf_procs,
           .f=dplyr::union)
  
  chop_st_conf_procs_concepts_by_year <- 
    check_st_conf_concepts_by_year(concept_list_procedures)
  
  chop_st_conf_procs_concepts_by_year <- 
    reduce(.x=chop_st_conf_procs_concepts_by_year,
           .f=dplyr::union)
  
  output_tbl_append(chop_st_conf_procs_concepts,
                    'st_conf_concepts_unmapped')
  
  output_tbl_append(chop_st_conf_procs_concepts_by_year,
                    'st_conf_concepts_unmapped_by_year')
  
  
  ## nemours
  
  config('cdm_schema', 'nemours_pedsnet')
  config('site', 'nemours')
  config('site_filter', 'nemours')
  config('site_filter_previous', 'nemours')
  
  nemours_st_conf_concepts_unmapped <- 
    check_st_conf_concepts_unmapped(concept_list_full)
  
  nemours_st_conf_concepts_by_year <- 
    check_st_conf_concepts_by_year(concept_list_full)
  
  nemours_st_conf_concepts <- 
    reduce(.x=nemours_st_conf_concepts_unmapped,
           .f=dplyr::union)
  
  nemours_st_conf_concepts_by_year <- 
    reduce(.x=nemours_st_conf_concepts_by_year,
           .f=dplyr::union)
  
  output_tbl_append(nemours_st_conf_concepts,
                    'st_conf_concepts_unmapped')
  
  output_tbl_append(nemours_st_conf_concepts_by_year,
                    'st_conf_concepts_unmapped_by_year')
  
  ###
  
  nemours_st_conf_procs <- 
    check_st_conf_concepts_unmapped(concept_list_procedures)
  
  nemours_st_conf_procs_concepts <- 
    reduce(.x=nemours_st_conf_procs,
           .f=dplyr::union)
  
  nemours_st_conf_procs_concepts_by_year <- 
    check_st_conf_concepts_by_year(concept_list_procedures)
  
  nemours_st_conf_procs_concepts_by_year <- 
    reduce(.x=nemours_st_conf_procs_concepts_by_year,
           .f=dplyr::union)
  
  output_tbl_append(nemours_st_conf_procs_concepts,
                    'st_conf_concepts_unmapped')
  
  output_tbl_append(nemours_st_conf_procs_concepts_by_year,
                    'st_conf_concepts_unmapped_by_year')
  
  
  ## nationwide
  
  config('cdm_schema', 'nationwide_pedsnet')
  config('site', 'nationwide')
  config('site_filter', 'nationwide')
  config('site_filter_previous', 'nationwide')
  
  
  nationwide_st_conf_concepts_unmapped <- 
    check_st_conf_concepts_unmapped(concept_list_full)
  
  nationwide_st_conf_concepts_by_year <- 
    check_st_conf_concepts_by_year(concept_list_full)
  
  nationwide_st_conf_concepts <- 
    reduce(.x=nationwide_st_conf_concepts_unmapped,
           .f=dplyr::union)
  
  nationwide_st_conf_concepts_by_year <- 
    reduce(.x=nationwide_st_conf_concepts_by_year,
           .f=dplyr::union)
  
  output_tbl_append(nationwide_st_conf_concepts,
                    'st_conf_concepts_unmapped')
  
  output_tbl_append(nationwide_st_conf_concepts_by_year,
                    'st_conf_concepts_unmapped_by_year')
  
  ###
  
  nationwide_st_conf_procs <- 
    check_st_conf_concepts_unmapped(concept_list_procedures)
  
  nationwide_st_conf_procs_concepts <- 
    reduce(.x=nationwide_st_conf_procs,
           .f=dplyr::union)
  
  nationwide_st_conf_procs_concepts_by_year <- 
    check_st_conf_concepts_by_year(concept_list_procedures)
  
  nationwide_st_conf_procs_concepts_by_year <- 
    reduce(.x=nationwide_st_conf_procs_concepts_by_year,
           .f=dplyr::union)
  
  output_tbl_append(nationwide_st_conf_procs_concepts,
                    'st_conf_concepts_unmapped')
  
  output_tbl_append(nationwide_st_conf_procs_concepts_by_year,
                    'st_conf_concepts_unmapped_by_year')
  
  ## colorado
  
  config('cdm_schema', 'colorado_pedsnet')
  config('site', 'colorado')
  config('site_filter', 'colorado')
  config('site_filter_previous', 'colorado')
  
  
  colorado_st_conf_concepts_unmapped <- 
    check_st_conf_concepts_unmapped(concept_list_full)
  
  colorado_st_conf_concepts_by_year <- 
    check_st_conf_concepts_by_year(concept_list_full)
  
  colorado_st_conf_concepts <- 
    reduce(.x=colorado_st_conf_concepts_unmapped,
           .f=dplyr::union)
  
  colorado_st_conf_concepts_by_year <- 
    reduce(.x=colorado_st_conf_concepts_by_year,
           .f=dplyr::union)
  
  output_tbl_append(colorado_st_conf_concepts,
                    'st_conf_concepts_unmapped')
  
  output_tbl_append(colorado_st_conf_concepts_by_year,
                    'st_conf_concepts_unmapped_by_year')
  
  ###
  
  colorado_st_conf_procs <- 
    check_st_conf_concepts_unmapped(concept_list_procedures)
  
  colorado_st_conf_procs_concepts <- 
    reduce(.x=colorado_st_conf_procs,
           .f=dplyr::union)
  
  colorado_st_conf_procs_concepts_by_year <- 
    check_st_conf_concepts_by_year(concept_list_procedures)
  
  colorado_st_conf_procs_concepts_by_year <- 
    reduce(.x=colorado_st_conf_procs_concepts_by_year,
           .f=dplyr::union)
  
  output_tbl_append(colorado_st_conf_procs_concepts,
                    'st_conf_concepts_unmapped')
  
  output_tbl_append(colorado_st_conf_procs_concepts_by_year,
                    'st_conf_concepts_unmapped_by_year')
  
  
  
  ## lurie
  
  config('cdm_schema', 'lurie_pedsnet')
  config('site', 'lurie')
  config('site_filter', 'lurie')
  config('site_filter_previous', 'lurie')
  
  lurie_st_conf_concepts_unmapped <- 
    check_st_conf_concepts_unmapped(concept_list_full)
  
  lurie_st_conf_concepts_by_year <- 
    check_st_conf_concepts_by_year(concept_list_full)
  
  lurie_st_conf_concepts <- 
    reduce(.x=lurie_st_conf_concepts_unmapped,
           .f=dplyr::union)
  
  lurie_st_conf_concepts_by_year <- 
    reduce(.x=lurie_st_conf_concepts_by_year,
           .f=dplyr::union)
  
  output_tbl_append(lurie_st_conf_concepts,
                    'st_conf_concepts_unmapped')
  
  output_tbl_append(lurie_st_conf_concepts_by_year,
                    'st_conf_concepts_unmapped_by_year')
  
  ###
  
  lurie_st_conf_procs <- 
    check_st_conf_concepts_unmapped(concept_list_procedures)
  
  lurie_st_conf_procs_concepts <- 
    reduce(.x=lurie_st_conf_procs,
           .f=dplyr::union)
  
  lurie_st_conf_procs_concepts_by_year <- 
    check_st_conf_concepts_by_year(concept_list_procedures)
  
  lurie_st_conf_procs_concepts_by_year <- 
    reduce(.x=lurie_st_conf_procs_concepts_by_year,
           .f=dplyr::union)
  
  output_tbl_append(lurie_st_conf_procs_concepts,
                    'st_conf_concepts_unmapped')
  
  output_tbl_append(lurie_st_conf_procs_concepts_by_year,
                    'st_conf_concepts_unmapped_by_year')
  
  
  
  ## stanford
  
  config('cdm_schema', 'stanford_pedsnet')
  config('site', 'stanford')
  config('site_filter', 'stanford')
  config('site_filter_previous', 'stanford')
  
  stanord_st_comp_facts_all <- 
    check_st_comp_facts(all_comp_facts_list)
  stanord_st_comp_facts_op <- 
    check_st_comp_facts(op_comp_facts_list,
                        visit_tbl=site_cdm_tbl('visit_occurrence') %>% 
                          filter(visit_concept_id %in% c(9202L,2000000469L,581399L)))
  stanord_st_comp_facts_ip <- 
    check_st_comp_facts(ip_comp_facts_list,
                        visit_tbl=site_cdm_tbl('visit_occurrence') %>% 
                          filter(visit_concept_id %in% c(9201L,2000000048L)))
  stanord_all_list <- 
    c(stanord_st_comp_facts_all,
      stanord_st_comp_facts_op,
      stanord_st_comp_facts_ip)
  
  stanord_st_comp_facts <- 
    reduce(.x = stanord_all_list,
           .f = dplyr::union)
  
  output_tbl_append(stanord_st_comp_facts,
                    'st_comp_facts')
  
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
