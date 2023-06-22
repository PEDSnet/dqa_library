# Top-level code for execution of data request

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(rlang))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(tidyr))

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
  
  ## Person and Drugs
  prev_person_drug <- 
    list(
      'person' = site_cdm_tbl_prev('person'),
      'drug_exposure' = site_cdm_tbl_prev('drug_exposure')
    )
  
  current_person_drug <- 
    list(
      'person' = site_cdm_tbl('person'),
      'drug_exposure' = site_cdm_tbl('drug_exposure')
    )
  
  meta_person_drug <- 
    list(
      'person' = 'full person table',
      'drug_exposure' = 'full drug exposure table'
    )
  
  ## Condition and ADT
  prev_condition_adt <- 
    list(
      'condition_occurrence' = site_cdm_tbl_prev('condition_occurrence'),
      'adt_occurrence' = site_cdm_tbl_prev('adt_occurrence')
    )
  
  current_condition_adt <- 
    list(
      'condition_occurrence' = site_cdm_tbl('condition_occurrence'),
      'adt_occurrence' = site_cdm_tbl('adt_occurrence')
    )
  
  meta_condition_adt <- 
    list(
      'condition_occurrence' = 'full_condition occurrence table',
      'adt_occurrence' = 'full adt occurrence table'
    )
  
  ## Device, immunization, and visits
  prev_device_imm_visit <- 
    list(
      'device_exposure' = site_cdm_tbl_prev('device_exposure'),
      'immunization' = site_cdm_tbl_prev('immunization'),
      'visit_occurrence' = site_cdm_tbl_prev('visit_occurrence')
    )
  
  
  current_device_imm_visit <- 
    list(
      'device_exposure' = site_cdm_tbl('device_exposure'),
      'immunization' = site_cdm_tbl('immunization'),
      'visit_occurrence' = site_cdm_tbl('visit_occurrence')
    )
  
  meta_device_imm_visit <- 
    list(
      'device_exposure' = 'full device exposure table',
      'immunization' = 'full immunization table',
      'visit_occurrence' = 'full visit occurrence table'
    )

  
  ## Vitals
  prev_vitals <- 
    list(
      'measurement_vitals' = site_cdm_tbl_prev('measurement_vitals')
    )
  
  current_vitals <- 
    list(
      'measurement_vitals' = site_cdm_tbl('measurement_vitals')
    )
  
  meta_vitals <- 
    list(
      'measurement_vitals' = 'vitals measurements'
    )
  
  ## Labs
  prev_labs <- 
    list(
      'measurement_labs' = site_cdm_tbl_prev('measurement_labs')
    )
  
  current_labs <- 
    list(
      'measurement_labs' = site_cdm_tbl('measurement_labs')
    )
  
  meta_labs <- 
    list(
      'measurement_labs' = 'labs: non-anthro and non-vitals measurements'
    )
  ## Condition types and adt type
  prev_cond_adt_type <- 
    list(
      'condition_type' = site_cdm_tbl_prev('condition_occurrence') %>% group_by(condition_type_concept_id),
      'adt_type' = site_cdm_tbl_prev('adt_occurrence') %>% group_by(service_concept_id)
    )
  
  current_cond_adt_type <- 
    list(
      'condition_type' = site_cdm_tbl('condition_occurrence') %>% group_by(condition_type_concept_id),
      'adt_type' = site_cdm_tbl('adt_occurrence') %>% group_by(service_concept_id)
    )
  
  meta_cond_adt_type <- 
    list(
      'condition_type' = 'condition occurrence grouped by condition_type_concept_id',
      'adt_type' = 'adt occurrence grouped by service_concept_id'
    )
  
  ### Visit and drug type 
  prev_visit_drug_type <- 
    list(
      'visit_type' = site_cdm_tbl_prev('visit_occurrence') %>% group_by(visit_concept_id),
      'drug_type' = site_cdm_tbl_prev('drug_exposure') %>% group_by(drug_type_concept_id)
    )

  current_visit_drug_type <-
    list(
      'visit_type' = site_cdm_tbl('visit_occurrence') %>% group_by(visit_concept_id),
      'drug_type' = site_cdm_tbl('drug_exposure') %>% group_by(drug_type_concept_id)
    )
    
   meta_visit_drug_type <- 
      list(
        'visit_type' = 'visit occurrence grouped by visit_concept_id',
        'drug_type' = 'drug exposure grouped by drug_type_concept_id'
      )
  
  ### Proc and anthro
  prev_proc_anthro <- 
    list(
      'procedures' = site_cdm_tbl_prev('procedure_occurrence'),
      'measurement_anthro' = site_cdm_tbl_prev('measurement_anthro')
    )

  
  current_proc_anthro <- 
    list(
      'procedures' = site_cdm_tbl('procedure_occurrence'),
      'measurement_anthro' = site_cdm_tbl('measurement_anthro')
    )
  
  meta_proc_anthro <- 
    list(
      'procedures' = 'full procedures',
      'measurement_anthro' = 'measurement anthros with derivations in place'
    )
  
  ###
  
  st_cons_short_list <- 
    list(
      list(prev_person_drug,
           current_person_drug,
           meta_person_drug),
      list(prev_condition_adt,
           current_condition_adt,
           meta_condition_adt)
    )
  
  st_cons_full_list <- 
    list(
      first = list(prev_person_drug,
                     current_person_drug,
                     meta_person_drug),
      second = list(prev_condition_adt,
                      current_condition_adt,
                      meta_condition_adt),
      third = list(prev_device_imm_visit,
                     current_device_imm_visit,
                     meta_device_imm_visit),
      fourth = list(prev_labs,
                      current_labs,
                      meta_labs),
      fifth = list(prev_vitals,
                     current_vitals,
                     meta_vitals),
      sixth = list(prev_cond_adt_type,
                     current_cond_adt_type,
                     meta_cond_adt_type),
      seventh = list(prev_visit_drug_type,
                       current_visit_drug_type,
                       meta_visit_drug_type),
      eigth = list(prev_proc_anthro,
                     current_proc_anthro,
                     meta_proc_anthro)
    )
 
  go_st_cons_dc <- function(st_cons_iter) {
    
    output <- check_st_cons_dc(prev_v_tbls=st_cons_iter[[1]],
                               current_v_tbls=st_cons_iter[[2]],
                               meta_tbls=st_cons_iter[[3]])
    output_list_to_db(output)
  }

  
  
  ### SEATTLE
  config('cdm_schema', 'seattle_pedsnet')
  config('site', 'seattle')
  config('site_filter', NA)
  config('site_filter_previous', 'seattle')
  st_cons_seattle <- 
    map(.x=st_cons_full_list,
        .f=go_st_cons_dc)
  
  
  ## COLORADO
  config('cdm_schema', 'colorado_pedsnet')
  config('site', 'colorado')
  config('site_filter', NA)
  config('site_filter_previous', 'colorado')
  st_cons_colorado <- 
    map(.x=st_cons_full_list,
        .f=go_st_cons_dc)
  
  ## CHOP
  config('cdm_schema', 'chop_pedsnet')
  config('site', 'chop')
  config('site_filter',NA)
  #config('site_filter', 'chop')
  config('site_filter_previous', 'chop')
  st_cons_chop <-
    map(.x=st_cons_full_list,
        .f=go_st_cons_dc)
  
  ## NEMOURS
  config('cdm_schema', 'nemours_pedsnet')
  config('site', 'nemours')
  config('site_filter', NA)
  config('site_filter_previous', 'nemours')
  st_cons_nemours <- 
    map(.x=st_cons_full_list,
        .f=go_st_cons_dc)
  
  ## NATIONWIDE
  config('cdm_schema', 'nationwide_pedsnet')
  config('site', 'nationwide')
  config('site_filter', NA)
  config('site_filter_previous', 'nationwide')
  st_cons_nationwide <- 
    map(.x=st_cons_full_list,
        .f=go_st_cons_dc)
  
  ## STANFORD
  config('cdm_schema', 'stanford_pedsnet')
  config('site', 'stanford')
  config('site_filter', NA)
  config('site_filter_previous', 'stanford')
  st_cons_stanford <- 
    map(.x=st_cons_full_list,
        .f=go_st_cons_dc)
  
  ## LURIE
  config('cdm_schema', 'lurie_pedsnet')
  config('site', 'lurie')
  config('site_filter', NA)
  config('site_filter_previous', 'lurie')
  st_cons_lurie <- 
    map(.x=st_cons_full_list,
        .f=go_st_cons_dc)
  
  ## CCHMC
  config('cdm_schema', 'cchmc_pedsnet')
  config('site', 'cchmc')
  config('site_filter', NA)
  config('site_filter_previous', 'cchmc')
  st_cons_cchmc <- 
    map(.x=st_cons_full_list,
        .f=go_st_cons_dc)
  
  ## adding on to compute proportions in output
  
  test_st_cons_dc_person <- 
    results_tbl('st_cons_dc_person') %>% collect() %>%
    pivot_longer(
      cols = starts_with('total_ct'),
      names_to = 'total_ct',
      values_to = 
    )
  
  
  convert_to_wide_fun <- function(db_tbl_string,
                                  previous_version=config('previous_version'),
                                  current_version=config('current_version')) {
    
    #tbl_string <- 
     # paste0(db_tbl_string,'_long')
    
    out <- 
      results_tbl(db_tbl_string) %>%
      collect() %>%
      pivot_wider(
        names_from = database_version,
        values_from = c(total_ct,total_pt_ct)
      ) %>% mutate(
        prop_total_change = round(
          (!! sym(paste0('total_ct_',current_version)) - !! sym(paste0('total_ct_',previous_version)))/!! sym(paste0('total_ct_',previous_version)), 3)
      ) %>% mutate(
        prop_pt_change = round(
          (!! sym(paste0('total_ct_',current_version)) - !! sym(paste0('total_ct_',previous_version)))/!! sym(paste0('total_ct_',previous_version)), 3)
      )
        
      
    
    output_tbl(
      out,
      db_tbl_string
    )
    
  }
  
  convert_list <- 
    list(
      'st_cons_dc_adt_occurrence',
      'st_cons_dc_adt_type',
      'st_cons_dc_condition_occurrence',
      'st_cons_dc_condition_type',
      'st_cons_dc_device_exposure',
      'st_cons_dc_drug_exposure',
      'st_cons_dc_drug_type',
      'st_cons_dc_immunization',
      'st_cons_dc_measurement_anthro',
      'st_cons_dc_measurement_labs',
      'st_cons_dc_measurement_vitals',
      'st_cons_dc_person',
     # 'st_cons_dc_person_long',
      'st_cons_dc_visit_occurrence',
      'st_cons_dc_visit_type'
    )
  
  map(.x=convert_list,
      .f=convert_to_wide_fun)
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
