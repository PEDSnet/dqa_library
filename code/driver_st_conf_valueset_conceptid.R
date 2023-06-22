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
  
  
  vs_list <- 
    list(
      'adt_occurrence' = list('valueset_service',
                              'service_concept_id'),
      'adt_occurrence' = list('valueset_adt_event_type',
                              'adt_type_concept_id'),
      'observation' = list('valueset_observation'),
      'immunization' = list('valueset_imm_route',
                            'imm_route_concept_id'),
      'person' = list('valueset_race',
                      'race_concept_id'),
      'person' = list('valueset_ethnicity',
                      'ethnicity_concept_id')
    )

## STANFORD
config('cdm_schema', 'stanford_pedsnet')
config('site', 'stanford')
config('site_filter', NA)
config('site_filter_previous', 'stanford')  
  stanford_vs <- 
    check_st_conf_vs_conceptid(vs_list)

## SEATTLE
config('cdm_schema', 'seattle_pedsnet')
config('site', 'seattle')
config('site_filter', NA)
config('site_filter_previous', 'seattle')
  seattle_vs <- 
    check_st_conf_vs_conceptid(vs_list)

## CHOP
config('cdm_schema', 'chop_pedsnet')
config('site', 'chop')
config('site_filter', NA)
config('site_filter_previous', 'chop')
  chop_vs <- 
    check_st_conf_vs_conceptid(vs_list)

## COLORADO
config('cdm_schema', 'colorado_pedsnet')
config('site', 'colorado')
config('site_filter', NA)
config('site_filter_previous', 'colorado')
  colorado_vs <- 
    check_st_conf_vs_conceptid(vs_list)
  
## NATIONWIDE
config('cdm_schema', 'nationwide_pedsnet')
config('site', 'nationwide')
config('site_filter', NA)
config('site_filter_previous', 'nationwide')
  nationwide_vs <- 
    check_st_conf_vs_conceptid(vs_list)

## NEMOURS
config('cdm_schema', 'nemours_pedsnet')
config('site', 'nemours')
config('site_filter', NA)
config('site_filter_previous', 'nemours')  
  nemours_vs <- 
    check_st_conf_vs_conceptid(vs_list)
  

## LURIE
config('cdm_schema', 'lurie_pedsnet')
config('site', 'lurie')
config('site_filter', NA)
config('site_filter_previous', 'lurie')  
  lurie_vs <- 
    check_st_conf_vs_conceptid(vs_list)
  
## CCHMC
config('cdm_schema', 'cchmc_pedsnet')
config('site', 'cchmc')
config('site_filter', NA)
config('site_filter_previous', 'cchmc')  
  cchmc_vs <- 
    check_st_conf_vs_conceptid(vs_list)


  
  
  combined_vs <- 
    c(seattle_vs,
      chop_vs,
      colorado_vs,
      nationwide_vs,
      nemours_vs,
      lurie_vs,
      stanford_vs)
  
  #######
  
  
  all_tbls <- 
    c(combined_vs,
      combined_vocab)
  
 output <- 
    create_vs_output(all_tbls)
 
  
  output_df <- 
    reduce(.x=output,
          .f=dplyr::union) 
  
  output_df %>% 
    output_tbl_append('st_conf_vs_violations')
  
  
  combined_list <- 
    c(vs_list,
      vocab_list)
  
  metadata_vs_concepts <- 
    compute_metadata_conf_vs(valuesets = vs_list,
                             valuesets_output_tbl_name = 'st_conf_vs_violations',
                             check_type = 'valueset_concepts')
  metadata_vs_vocab <- 
    compute_metadata_conf_vs(valuesets = vocab_list,
                             valuesets_output_tbl_name = 'st_conf_vs_violations',
                             check_type = 'vocabulary')
  
  metadata_vs_all <- 
      metadata_vs_concepts  %>% 
    dplyr::union(
      metadata_vs_vocab
    )
  
  output_tbl(metadata_vs_all,
             'st_conf_vs_metadata')
  
  #output_tbl_append(metadata_vs_concepts,
                   # 'st_conf_vs_metadata')

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
