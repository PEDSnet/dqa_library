# Top-level code for execution of data request

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(rlang))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(purrr))
#suppressPackageStartupMessages(library(stringr))
#suppressPackageStartupMessages(library(tidyr))
#suppressPackageStartupMessages(library(lubridate))

# Need to do this for assignInNamespace to work
suppressPackageStartupMessages(library(dbplyr))

# Required for execution using Rscript
suppressPackageStartupMessages(library(methods))

#' Set up the execution environment
#'
#' The .load() function sources the R files needed to execute the query and sets
#' up the execution environment.  In particular, all of the framework files, as
#' well as request-specific files in the code_dir with names matching
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
#' @param driver The name of the driver file to source.  This allows you to set
#'   up alternate execution paths (.run() functions) for your project and
#'   specify which one to follow.
#'
#' @return The value of `here`.
#' @md
.load <- function(here = ifelse(typeof(get('config')) == 'closure',
                                config('base_dir'), base_dir),
                  driver = 'driver.R') {
    source(file.path(here, 'code', 'config.R'))
    #source(file.path(here, 'code', 'req_info.R'))
    source(config('site_info'))
    source(file.path(here, "code", 'shims.R'))
    source(file.path(here, "code", 'codesets.R'))
    for (fn in list.files(file.path(here, "code"),
                        'util_.+\\.R', full.names = TRUE))
      source(fn)
     for (fn in list.files(file.path(here, "code"),
                        'addon_.+\\.R', full.names = TRUE))
      source(fn)
    for (fn in list.files(file.path(here, "code"),
                          'cohort_.+\\.R', full.names = TRUE))
      source(fn)
    for (fn in list.files(file.path(here, "code"),
                        'analyze_.+\\.R', full.names = TRUE))
      source(fn)
    source(file.path(here, "code", 'cohorts.R'))
    #source(file.path(here, config('subdirs')$code_dir, driver))

    if (config_exists('extra_packages')) {
      for (pkg in config('extra_packages'))
        suppressPackageStartupMessages(library(pkg, character.only = TRUE))
    }

    .env_setup()

    for (def in c('retain_intermediates', 'results_schema')) {
      if (! config_exists(def) || is.na(config(def)))
        config(def, config(paste0('default_', def)))
    }

    here
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
#' @param driver The name of the driver file to source (cf. [.load()]).
#'
#' @return The result of [.run()].
#' @md
run_request <- function(base_dir, driver = 'driver.R') {
    base_dir <- .load(base_dir)
    on.exit(.env_cleanup())
    .run(base_dir)
}
