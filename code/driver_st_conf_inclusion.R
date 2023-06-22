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
  
  # ST-CONF-Inclusion: Satisfying inclusion criteria
  check_st_conf_inclusion_complete <- 
    check_st_conf_inclusion(inclusion_check_tbls = c('visit_occurrence', 'condition_occurrence'),
                            acceptable_codes = c(9201,          # Inpatient Visit
                                                 9202,          # Outpatient Visit 
                                                 9203,          # Emergency Room Visit 
                                                 2000000469,    # Outpatient Non Physician 
                                                 42898160,      # Long Term Care Visit 
                                                 44814710,      # Non-Acute Institutional Stay
                                                 2000000048,    # Emergency Department Admit to Inpatient Hospital Stay 
                                                 2000000088),   # Observation Stay
                            inclusion_date = "2009-01-01") 
  
  # Getting count of overall violations (visit and condition)
  total_violations <- check_st_conf_inclusion_complete %>% filter(!is.na(person_id)) %>% 
    tally(name = 'total_violations') %>% add_meta(., 'st_conf_inclusion')
  
  # Getting count of visit violations
  visit_violations <- check_st_conf_inclusion_complete %>% filter(!is.na(domain_violation.x)) %>% 
    tally(name = 'visit_violations') %>% add_meta(., 'st_conf_inclusion')
  
  # Getting count of condition violations
  condition_violations <- check_st_conf_inclusion_complete %>% filter(!is.na(domain_violation.y)) %>% 
    tally(name = 'condition_violations') %>% add_meta(., 'st_conf_inclusion')
  
  # Joining all violations and metadata 
  total_violations %>% 
    inner_join(visit_violations) %>% 
    inner_join(condition_violations) %>% 
    output_tbl('st_conf_inclusion',
               db=TRUE,
               file=FALSE)
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
