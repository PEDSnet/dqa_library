
#' Make sure the `dqa_library.Rproj` file is opened prior to executing this script
#' This will ensure the working directory is populated appropriately

# Load required packages
library(argos)
library(srcr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(DBI)
library(dbplyr)
library(lubridate)

# Source file with wrapper function
source(file.path('setup', 'argos_wrapper.R'))

###' `Set site name` ###
site <- 'seattle'

# Establish connection to database
initialize_session(session_name = 'ndq_assessment',
                   db_conn = Sys.getenv('PEDSNET_BASE_CONFIG'),
                   is_json = TRUE,
                   cdm_schema = paste0(site, '_pedsnet'),
                   results_schema = 'dqa_test',
                   retain_intermediates = FALSE,
                   results_tag = '')

###' `Set additional configs` ###

config('site', site)

# needed if executing check_dc_init and pointing to previous CDM instance
config('cdm_schema_prev', 'dcc_pedsnet')
config('db_src_prev', srcr(Sys.getenv('PEDSNET_PREV_CONFIG')))
# needed if executing check_dc and pointing to previous results
config('results_schema_prev', 'dqa_rox')

config('previous_version','v55')
config('current_version','v56')

# Build time windows for FOT
num_mnths <- (interval(mdy(01012009), today()) %/% months(1)) + 1

time_span_list_output <-
  as.character(seq(as.Date('2009-01-01'), length = num_mnths, by='months')-1)
time_span <- c(time_span_list_output)

num_yrs <- (interval(mdy(01012009), today()) %/% years(1)) + 1

time_span_yr_output <-
  as.character(seq(as.Date('2010-01-01'), length = num_yrs, by='years')-1)
time_span_yr <- c(time_span_yr_output)

# Source cohort_* files
for (fn in list.files('code', 'cohort_.+\\.R', full.names = TRUE)){
  source(fn)
  }
rm(fn)
