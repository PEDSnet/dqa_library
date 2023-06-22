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
  
  procs_drugs <- 
    dplyr::union(site_cdm_tbl('procedure_occurrence') %>% select(person_id,visit_occurrence_id),
                 site_cdm_tbl('drug_exposure') %>% select(person_id,visit_occurrence_id)) #%>%
  #  dplyr::union(site_cdm_tbl('measurement_labs_view') %>% 
                   # select(person_id,visit_occurrence_id))
  
  procs_drugs_labs <- 
    dplyr::union(site_cdm_tbl('procedure_occurrence') %>% select(person_id,visit_occurrence_id),
                 site_cdm_tbl('drug_exposure') %>% select(person_id,visit_occurrence_id)) %>%
    dplyr::union(site_cdm_tbl('measurement_labs') %>% select(person_id,visit_occurrence_id))
                 
  
  all_comp_facts_list <- 
    list(
      'all_visits_with_procedures' = site_cdm_tbl('procedure_occurrence'),
      'all_visits_with_conditions' = site_cdm_tbl('condition_occurrence'),
     # 'all_visits_without_labs' = site_cdm_tbl('measurement_labs_view'),
      'all_visits_with_drugs' = site_cdm_tbl('drug_exposure'),
      'all_visits_with_procs_drugs' = procs_drugs
    )
  
  all_comp_labs_list <- 
    list(
      'all_lab_visits' = site_cdm_tbl('measurement_labs'),
      'all_visits_with_procs_drugs_labs' = procs_drugs_labs
    )
  
  op_comp_facts_list <- 
    list(
      'op_visits_with_procedures' = site_cdm_tbl('procedure_occurrence'),
      'op_visits_with_conditions' = site_cdm_tbl('condition_occurrence'),
    #  'op_visits_without_labs' = site_cdm_tbl('measurement_labs_view'),
      'op_visits_with_drugs' = site_cdm_tbl('drug_exposure'),
      'op_visits_with_procs_drugs' = procs_drugs
    )
  
  op_comp_labs_list <- 
    list(
      'op_lab_visits' = site_cdm_tbl('measurement_labs'),
      'op_all_visits_with_procs_drugs_labs' = procs_drugs_labs
    )
  
  ip_comp_facts_list <- 
    list(
      'ip_visits_with_procedures' = site_cdm_tbl('procedure_occurrence'),
      'ip_visits_with_conditions' = site_cdm_tbl('condition_occurrence'),
   #   'ip_visits_without_labs' = site_cdm_tbl('measurement_labs_view'),
      'ip_visits_with_drugs' = site_cdm_tbl('drug_exposure'),
      'ip_visits_with_procs_drugs' = procs_drugs
    )
  
  ip_comp_labs_list <- 
    list(
      'ip_lab_visits' = site_cdm_tbl('measurement_labs'),
      'ip_all_visits_with_procs_drugs_labs' = procs_drugs_labs
    )
  
  ## Stanford
  
  config('cdm_schema', 'stanford_pedsnet')
  config('site', 'stanford')
  config('site_filter', NA)
  config('site_filter_previous', 'stanford')
  
  stanford_st_comp_labs_all <- 
    check_st_comp_facts(all_comp_labs_list)
  stanford_st_comp_labs_op <- 
    check_st_comp_facts(op_comp_labs_list,
                        visit_tbl=site_cdm_tbl('visit_occurrence') %>% 
                          filter(visit_concept_id %in% c(9202L,2000000469L,581399L)))
  stanford_st_comp_labs_ip <- 
    check_st_comp_facts(ip_comp_labs_list,
                        visit_tbl=site_cdm_tbl('visit_occurrence') %>% 
                          filter(visit_concept_id %in% c(9201L,2000000048L)))
  stanford_all_labs_list <- 
    c(stanford_st_comp_labs_all,
      stanford_st_comp_labs_op,
      stanford_st_comp_labs_ip)
  
  stanford_st_comp_facts_labs <- 
    reduce(.x = stanford_all_labs_list,
           .f = dplyr::union)
  
  
  stanford_all_comp_facts <-
    check_st_comp_facts(all_comp_facts_list)
  stanford_op_comp_facts <- 
    check_st_comp_facts(op_comp_facts_list,
                        visit_tbl=site_cdm_tbl('visit_occurrence') %>% 
                          filter(visit_concept_id %in% c(9202L,2000000469L,581399L)))
  stanford_ip_comp_facts <- 
    check_st_comp_facts(ip_comp_facts_list,
                        visit_tbl=site_cdm_tbl('visit_occurrence') %>% 
                          filter(visit_concept_id %in% c(9201L,2000000048L)))
  
  stanford_all_list <- 
    c(stanford_all_comp_facts,
      stanford_op_comp_facts,
      stanford_ip_comp_facts)
  
  stanford_all_tbl <- 
    reduce(.x=stanford_all_list,
           .f=dplyr::union)
  
  output_tbl_append(stanford_all_tbl,
                    'st_comp_facts')
  
  ## Seattle
  
  config('cdm_schema', 'seattle_pedsnet')
  config('site', 'seattle')
  config('site_filter', NA)
  config('site_filter_previous', 'seattle')
  
  seattle_st_comp_labs_all <- 
    check_st_comp_facts(all_comp_labs_list)
  seattle_st_comp_labs_op <- 
    check_st_comp_facts(op_comp_labs_list,
                        visit_tbl=site_cdm_tbl('visit_occurrence') %>% 
                          filter(visit_concept_id %in% c(9202L,2000000469L,581399L)))
  seattle_st_comp_labs_ip <- 
    check_st_comp_facts(ip_comp_labs_list,
                        visit_tbl=site_cdm_tbl('visit_occurrence') %>% 
                          filter(visit_concept_id %in% c(9201L,2000000048L)))
  seattle_all_labs_list <- 
    c(seattle_st_comp_labs_all,
      seattle_st_comp_labs_op,
      seattle_st_comp_labs_ip)
  
  seattle_st_comp_facts_labs <- 
    reduce(.x = seattle_all_labs_list,
           .f = dplyr::union)
  
  
  seattle_all_comp_facts <-
    check_st_comp_facts(all_comp_facts_list)
  seattle_op_comp_facts <- 
    check_st_comp_facts(op_comp_facts_list,
                        visit_tbl=site_cdm_tbl('visit_occurrence') %>% 
                          filter(visit_concept_id %in% c(9202L,2000000469L,581399L)))
  seattle_ip_comp_facts <- 
    check_st_comp_facts(ip_comp_facts_list,
                        visit_tbl=site_cdm_tbl('visit_occurrence') %>% 
                          filter(visit_concept_id %in% c(9201L,2000000048L)))
  
  seattle_all_list <- 
    c(seattle_all_comp_facts,
      seattle_op_comp_facts,
      seattle_ip_comp_facts)
  
  seattle_all_tbl <- 
    reduce(.x=seattle_all_list,
           .f=dplyr::union)
  
  output_tbl_append(seattle_all_tbl,
                    'st_comp_facts')
  ## chop
  
  config('cdm_schema', 'chop_pedsnet')
  config('site', 'chop')
  config('site_filter', NA)
  config('site_filter_previous', 'chop')
  
  chop_st_comp_labs_all <- 
    check_st_comp_facts(all_comp_labs_list)
  chop_st_comp_labs_op <- 
    check_st_comp_facts(op_comp_labs_list,
                        visit_tbl=site_cdm_tbl('visit_occurrence') %>% 
                          filter(visit_concept_id %in% c(9202L,2000000469L,581399L)))
  chop_st_comp_labs_ip <- 
    check_st_comp_facts(ip_comp_labs_list,
                        visit_tbl=site_cdm_tbl('visit_occurrence') %>% 
                          filter(visit_concept_id %in% c(9201L,2000000048L)))
  chop_all_labs_list <- 
    c(chop_st_comp_labs_all,
      chop_st_comp_labs_op,
      chop_st_comp_labs_ip)
  
  chop_st_comp_facts_labs <- 
    reduce(.x = chop_all_labs_list,
           .f = dplyr::union)
  
  
  chop_all_comp_facts <-
    check_st_comp_facts(all_comp_facts_list)
  chop_op_comp_facts <- 
    check_st_comp_facts(op_comp_facts_list,
                        visit_tbl=site_cdm_tbl('visit_occurrence') %>% 
                          filter(visit_concept_id %in% c(9202L,2000000469L,581399L)))
  chop_ip_comp_facts <- 
    check_st_comp_facts(ip_comp_facts_list,
                        visit_tbl=site_cdm_tbl('visit_occurrence') %>% 
                          filter(visit_concept_id %in% c(9201L,2000000048L)))
  
  chop_all_list <- 
    c(chop_all_comp_facts,
      chop_op_comp_facts,
      chop_ip_comp_facts)
  
  chop_all_tbl <- 
    reduce(.x=chop_all_list,
           .f=dplyr::union)
  
  output_tbl_append(chop_all_tbl,
                    'st_comp_facts')
  ## nemours
  
  config('cdm_schema', 'nemours_pedsnet')
  config('site', 'nemours')
  config('site_filter', NA)
  config('site_filter_previous', 'nemours')
  
  nemours_st_comp_labs_all <- 
    check_st_comp_facts(all_comp_labs_list)
  nemours_st_comp_labs_op <- 
    check_st_comp_facts(op_comp_labs_list,
                        visit_tbl=site_cdm_tbl('visit_occurrence') %>% 
                          filter(visit_concept_id %in% c(9202L,2000000469L,581399L)))
  nemours_st_comp_labs_ip <- 
    check_st_comp_facts(ip_comp_labs_list,
                        visit_tbl=site_cdm_tbl('visit_occurrence') %>% 
                          filter(visit_concept_id %in% c(9201L,2000000048L)))
  nemours_all_labs_list <- 
    c(nemours_st_comp_labs_all,
      nemours_st_comp_labs_op,
      nemours_st_comp_labs_ip)
  
  nemours_st_comp_facts_labs <- 
    reduce(.x = nemours_all_labs_list,
           .f = dplyr::union)
  
  
  nemours_all_comp_facts <-
    check_st_comp_facts(all_comp_facts_list)
  nemours_op_comp_facts <- 
    check_st_comp_facts(op_comp_facts_list,
                        visit_tbl=site_cdm_tbl('visit_occurrence') %>% 
                          filter(visit_concept_id %in% c(9202L,2000000469L,581399L)))
  nemours_ip_comp_facts <- 
    check_st_comp_facts(ip_comp_facts_list,
                        visit_tbl=site_cdm_tbl('visit_occurrence') %>% 
                          filter(visit_concept_id %in% c(9201L,2000000048L)))
  
  nemours_all_list <- 
    c(nemours_all_comp_facts,
      nemours_op_comp_facts,
      nemours_ip_comp_facts)
  
  nemours_all_tbl <- 
    reduce(.x=nemours_all_list,
           .f=dplyr::union)
  
  output_tbl_append(nemours_all_tbl,
                    'st_comp_facts')
  
  ## nationwide
  
  config('cdm_schema', 'nationwide_pedsnet')
  config('site', 'nationwide')
  config('site_filter', NA)
  config('site_filter_previous', 'nationwide')
  
  
  nationwide_st_comp_labs_all <- 
    check_st_comp_facts(all_comp_labs_list)
  nationwide_st_comp_labs_op <- 
    check_st_comp_facts(op_comp_labs_list,
                        visit_tbl=site_cdm_tbl('visit_occurrence') %>% 
                          filter(visit_concept_id %in% c(9202L,2000000469L,581399L)))
  nationwide_st_comp_labs_ip <- 
    check_st_comp_facts(ip_comp_labs_list,
                        visit_tbl=site_cdm_tbl('visit_occurrence') %>% 
                          filter(visit_concept_id %in% c(9201L,2000000048L)))
  nationwide_all_labs_list <- 
    c(nationwide_st_comp_labs_all,
      nationwide_st_comp_labs_op,
      nationwide_st_comp_labs_ip)
  
  nationwide_st_comp_facts_labs <- 
    reduce(.x = nationwide_all_labs_list,
           .f = dplyr::union)
  
  
  nationwide_all_comp_facts <-
    check_st_comp_facts(all_comp_facts_list)
  nationwide_op_comp_facts <- 
    check_st_comp_facts(op_comp_facts_list,
                        visit_tbl=site_cdm_tbl('visit_occurrence') %>% 
                          filter(visit_concept_id %in% c(9202L,2000000469L,581399L)))
  nationwide_ip_comp_facts <- 
    check_st_comp_facts(ip_comp_facts_list,
                        visit_tbl=site_cdm_tbl('visit_occurrence') %>% 
                          filter(visit_concept_id %in% c(9201L,2000000048L)))
  
  nationwide_all_list <- 
    c(nationwide_all_comp_facts,
      nationwide_op_comp_facts,
      nationwide_ip_comp_facts)
  
  nationwide_all_tbl <- 
    reduce(.x=nationwide_all_list,
           .f=dplyr::union)
  
  output_tbl_append(nationwide_all_tbl,
                    'st_comp_facts')
  
  
  ## colorado
  
  config('cdm_schema', 'colorado_pedsnet')
  config('site', 'colorado')
  config('site_filter', NA)
  config('site_filter_previous', 'colorado')
  
  
  colorado_st_comp_labs_all <- 
    check_st_comp_facts(all_comp_labs_list)
  colorado_st_comp_labs_op <- 
    check_st_comp_facts(op_comp_labs_list,
                        visit_tbl=site_cdm_tbl('visit_occurrence') %>% 
                          filter(visit_concept_id %in% c(9202L,2000000469L,581399L)))
  colorado_st_comp_labs_ip <- 
    check_st_comp_facts(ip_comp_labs_list,
                        visit_tbl=site_cdm_tbl('visit_occurrence') %>% 
                          filter(visit_concept_id %in% c(9201L,2000000048L)))
  colorado_all_labs_list <- 
    c(colorado_st_comp_labs_all,
      colorado_st_comp_labs_op,
      colorado_st_comp_labs_ip)
  
  colorado_st_comp_facts_labs <- 
    reduce(.x = colorado_all_labs_list,
           .f = dplyr::union)
  
  
  colorado_all_comp_facts <-
    check_st_comp_facts(all_comp_facts_list)
  colorado_op_comp_facts <- 
    check_st_comp_facts(op_comp_facts_list,
                        visit_tbl=site_cdm_tbl('visit_occurrence') %>% 
                          filter(visit_concept_id %in% c(9202L,2000000469L,581399L)))
  colorado_ip_comp_facts <- 
    check_st_comp_facts(ip_comp_facts_list,
                        visit_tbl=site_cdm_tbl('visit_occurrence') %>% 
                          filter(visit_concept_id %in% c(9201L,2000000048L)))
  
  colorado_all_list <- 
    c(colorado_all_comp_facts,
      colorado_op_comp_facts,
      colorado_ip_comp_facts)
  
  colorado_all_tbl <- 
    reduce(.x=colorado_all_list,
           .f=dplyr::union)
  
  output_tbl_append(colorado_all_tbl,
                    'st_comp_facts')
  
  ## lurie
  
  config('cdm_schema', 'lurie_pedsnet')
  config('site', 'lurie')
  config('site_filter', NA)
  config('site_filter_previous', 'lurie')
  
  lurie_st_comp_labs_all <- 
    check_st_comp_facts(all_comp_labs_list)
  lurie_st_comp_labs_op <- 
    check_st_comp_facts(op_comp_labs_list,
                        visit_tbl=site_cdm_tbl('visit_occurrence') %>% 
                          filter(visit_concept_id %in% c(9202L,2000000469L,581399L)))
  lurie_st_comp_labs_ip <- 
    check_st_comp_facts(ip_comp_labs_list,
                        visit_tbl=site_cdm_tbl('visit_occurrence') %>% 
                          filter(visit_concept_id %in% c(9201L,2000000048L)))
  lurie_all_labs_list <- 
    c(lurie_st_comp_labs_all,
      lurie_st_comp_labs_op,
      lurie_st_comp_labs_ip)
  
  lurie_st_comp_facts_labs <- 
    reduce(.x = lurie_all_labs_list,
           .f = dplyr::union)
  
  
  lurie_all_comp_facts <-
    check_st_comp_facts(all_comp_facts_list)
  lurie_op_comp_facts <- 
    check_st_comp_facts(op_comp_facts_list,
                        visit_tbl=site_cdm_tbl('visit_occurrence') %>% 
                          filter(visit_concept_id %in% c(9202L,2000000469L,581399L)))
  lurie_ip_comp_facts <- 
    check_st_comp_facts(ip_comp_facts_list,
                        visit_tbl=site_cdm_tbl('visit_occurrence') %>% 
                          filter(visit_concept_id %in% c(9201L,2000000048L)))
  
  lurie_all_list <- 
    c(lurie_all_comp_facts,
      lurie_op_comp_facts,
      lurie_ip_comp_facts)
  
  lurie_all_tbl <- 
    reduce(.x=lurie_all_list,
           .f=dplyr::union)
  
  output_tbl_append(lurie_all_tbl,
                    'st_comp_facts')
  
  ## cchmc
  config('cdm_schema', 'cchmc_pedsnet')
  config('site', 'cchmc')
  config('site_filter',NA)
  config('site_filter_previous', 'cchmc')
  
  cchmc_st_comp_labs_all <- 
    check_st_comp_facts(all_comp_labs_list)
  cchmc_st_comp_labs_op <- 
    check_st_comp_facts(op_comp_labs_list,
                        visit_tbl=site_cdm_tbl('visit_occurrence') %>% 
                          filter(visit_concept_id %in% c(9202L,2000000469L,581399L)))
  cchmc_st_comp_labs_ip <- 
    check_st_comp_facts(ip_comp_labs_list,
                        visit_tbl=site_cdm_tbl('visit_occurrence') %>% 
                          filter(visit_concept_id %in% c(9201L,2000000048L)))
  cchmc_all_labs_list <- 
    c(cchmc_st_comp_labs_all,
      cchmc_st_comp_labs_op,
      cchmc_st_comp_labs_ip)
  
  cchmc_st_comp_facts_labs <- 
    reduce(.x = cchmc_all_labs_list,
           .f = dplyr::union)
  
  
  cchmc_all_comp_facts <-
    check_st_comp_facts(all_comp_facts_list)
  cchmc_op_comp_facts <- 
    check_st_comp_facts(op_comp_facts_list,
                        visit_tbl=site_cdm_tbl('visit_occurrence') %>% 
                          filter(visit_concept_id %in% c(9202L,2000000469L,581399L)))
  cchmc_ip_comp_facts <- 
    check_st_comp_facts(ip_comp_facts_list,
                        visit_tbl=site_cdm_tbl('visit_occurrence') %>% 
                          filter(visit_concept_id %in% c(9201L,2000000048L)))
  
  cchmc_all_list <- 
    c(cchmc_all_comp_facts,
      cchmc_op_comp_facts,
      cchmc_ip_comp_facts)
  
  cchmc_all_tbl <- 
    reduce(.x=cchmc_all_list,
           .f=dplyr::union)
  
  output_tbl_append(cchmc_all_tbl,
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
