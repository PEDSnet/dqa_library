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

  
  # st_conf_specificity_rxnorm
    ## drug list
    drug_tbl_list <- list(
    
    'admin_rx' = list(site_cdm_tbl('drug_exposure') %>% 
                                filter(drug_type_concept_id %in% c(38000180L,
                                                                   38000177L)) %>%
                                group_by(drug_type_concept_id),
                              'grouped by drug administrations and prescriptions')
    )
    #'inpatient_admin' = list(cdm_tbl('drug_exposure') %>% 
                          #     filter(drug_type_concept_id %in% c(38000180L)),
                          #   'all inpatient administrations')
    
    #  )
    
    ## STANFORD
    
    config('cdm_schema', 'stanford_pedsnet')
    config('site', 'stanford')
    config('site_filter', NA)
    config('site_filter_previous', 'stanford')
    
    
    ## function
    check_st_conf_specificity_rxnorm <- 
      compute_st_conf_specificity_rxnorm(drug_tbl_list_args = 
                                           drug_tbl_list) 
      output_list_to_db(check_st_conf_specificity_rxnorm)
    
    ## metadata
    check_st_conf_specificity_metadata <- 
      compute_st_conf_specificity_meta(drug_tbl_list_args = 
                                         drug_tbl_list) %>%
      output_tbl_append('st_conf_rxnorm_meta')
    
    
    
  ## Seattle
    
    config('cdm_schema', 'seattle_pedsnet')
    config('site', 'seattle')
    config('site_filter', NA)
    config('site_filter_previous', 'seattle')
    
    ## function
    check_st_conf_specificity_rxnorm <- 
      compute_st_conf_specificity_rxnorm(drug_tbl_list_args = 
                                           drug_tbl_list) 
    output_list_to_db(check_st_conf_specificity_rxnorm)
    
    ## metadata
    check_st_conf_specificity_metadata <- 
      compute_st_conf_specificity_meta(drug_tbl_list_args = 
                                         drug_tbl_list) %>%
      output_tbl_append('st_conf_rxnorm_meta')
    
  ## CHOP
    
    config('cdm_schema', 'chop_pedsnet')
    config('site', 'chop')
    config('site_filter', NA)
    config('site_filter_previous', 'chop')
    
    ## function
    check_st_conf_specificity_rxnorm <- 
      compute_st_conf_specificity_rxnorm(drug_tbl_list_args = 
                                           drug_tbl_list) 
    output_list_to_db(check_st_conf_specificity_rxnorm)
    
    ## metadata
    check_st_conf_specificity_metadata <- 
      compute_st_conf_specificity_meta(drug_tbl_list_args = 
                                         drug_tbl_list) %>%
      output_tbl_append('st_conf_rxnorm_meta')
    
  ## NEMOURS
    
    config('cdm_schema', 'nemours_pedsnet')
    config('site', 'nemours')
    config('site_filter', NA)
    config('site_filter_previous', 'nemours')
    ## function
    check_st_conf_specificity_rxnorm <- 
      compute_st_conf_specificity_rxnorm(drug_tbl_list_args = 
                                           drug_tbl_list) 
    output_list_to_db(check_st_conf_specificity_rxnorm)
    
    ## metadata
    check_st_conf_specificity_metadata <- 
      compute_st_conf_specificity_meta(drug_tbl_list_args = 
                                         drug_tbl_list) %>%
      output_tbl_append('st_conf_rxnorm_meta')
    
  ## NATIONWIDE
    
    config('cdm_schema', 'nationwide_pedsnet')
    config('site', 'nationwide')
    config('site_filter',NA)
    config('site_filter_previous', 'nationwide')
    
    ## function
    check_st_conf_specificity_rxnorm <- 
      compute_st_conf_specificity_rxnorm(drug_tbl_list_args = 
                                           drug_tbl_list) 
    output_list_to_db(check_st_conf_specificity_rxnorm)
    
    ## metadata
    check_st_conf_specificity_metadata <- 
      compute_st_conf_specificity_meta(drug_tbl_list_args = 
                                         drug_tbl_list) %>%
      output_tbl_append('st_conf_rxnorm_meta')
    
  ## COLORADO
    
    config('cdm_schema', 'colorado_pedsnet')
    config('site', 'colorado')
    config('site_filter', NA)
    config('site_filter_previous', 'colorado')
    
    ## function
    check_st_conf_specificity_rxnorm <- 
      compute_st_conf_specificity_rxnorm(drug_tbl_list_args = 
                                           drug_tbl_list) 
    output_list_to_db(check_st_conf_specificity_rxnorm)
    
    ## metadata
    check_st_conf_specificity_metadata <- 
      compute_st_conf_specificity_meta(drug_tbl_list_args = 
                                         drug_tbl_list) %>%
      output_tbl_append('st_conf_rxnorm_meta')
    
  ## LURIE
    
    config('cdm_schema', 'lurie_pedsnet')
    config('site', 'lurie')
    config('site_filter', NA)
    config('site_filter_previous', 'lurie')
    ## function
    check_st_conf_specificity_rxnorm <- 
      compute_st_conf_specificity_rxnorm(drug_tbl_list_args = 
                                           drug_tbl_list) 
    output_list_to_db(check_st_conf_specificity_rxnorm)
    
    ## metadata
    check_st_conf_specificity_metadata <- 
      compute_st_conf_specificity_meta(drug_tbl_list_args = 
                                         drug_tbl_list) %>%
      output_tbl_append('st_conf_rxnorm_meta')
    
  ## CCHMC
    
    config('cdm_schema', 'cchmc_pedsnet')
    config('site', 'cchmc')
    config('site_filter', NA)
    config('site_filter_previous', 'cchmc')
    ## function
    check_st_conf_specificity_rxnorm <- 
      compute_st_conf_specificity_rxnorm(drug_tbl_list_args = 
                                           drug_tbl_list) 
    output_list_to_db(check_st_conf_specificity_rxnorm)
    
    ## metadata
    check_st_conf_specificity_metadata <- 
      compute_st_conf_specificity_meta(drug_tbl_list_args = 
                                         drug_tbl_list) %>%
      output_tbl_append('st_conf_rxnorm_meta')
    
    
    
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
