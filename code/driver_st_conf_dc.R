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
  
  test_tbl_list_prev <- 
    list(
      'person' = site_cdm_tbl_prev('person'),
      'co_filtered' = site_cdm_tbl_prev('condition_occurrence') %>% 
        filter(condition_type_concept_id %in% c(2000000089L,
                                                2000000090L,
                                                2000000091L)),
      'de_filtered_grp' = site_cdm_tbl_prev('drug_exposure') %>%
        filter(drug_type_concept_id %in% c(38000180L, 38000177L)) %>%
        group_by(drug_type_concept_id),
      'adt_filtered' = site_cdm_tbl_prev('adt_occurrence') %>%
        filter(service_concept_id == 2000000080L),
      'vo_grp' = site_cdm_tbl_prev('visit_occurrence') %>%
        group_by(visit_concept_id)
      
      
    )
  
  test_tbl_list_current <- 
    list(
      'person' = cdm_tbl('person'),
      'co_filtered' = cdm_tbl('condition_occurrence') %>% 
        filter(condition_type_concept_id %in% c(2000000089L,
                                                2000000090L,
                                                2000000091L)),
      'de_filtered_grp' = cdm_tbl('drug_exposure') %>%
        filter(drug_type_concept_id %in% c(38000180L, 38000177L)) %>%
        group_by(drug_type_concept_id),
      'adt_filtered' = cdm_tbl('adt_occurrence') %>%
        filter(service_concept_id == 2000000080L),
      'vo_grp' = cdm_tbl('visit_occurrence') %>%
        group_by(visit_concept_id)
      
    )
  
  
  test_tbls_meta_list <- 
    list(
      'person' = 'full person table',
      'co_filtered' = 'condition_occurrence with no problem list dx and grouped by type_concept_id',
      'de_filtered_grp' = 'drug_exposure filtered to inpatient admin and prescription and grouped by type_concept_id',
      'adt_filtered' = 'adt_occurrence filtered by NICU',
      'vo_grp' = 'visit_occurrence grouped by type_concept_id'
    )
  
  test_datacycles <- 
    check_st_conf_dc(prev_v_tbls = test_tbl_list_prev,
                     current_v_tbls = test_tbl_list_current,
                     meta_tbls = test_tbls_meta_list)
  
  output_list_to_db(test_datacycles)
  
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
