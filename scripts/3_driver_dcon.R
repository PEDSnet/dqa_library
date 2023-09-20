
library('RPostgres')
library('srcr')
library('tidyr')
library('lubridate')
library('stringr')
library('assertthat')

Sys.setenv(PEDSNET_DATA_REQUEST_ROOT = "/data/airflow/dags/dqa_library/scripts")

source('site/run.R')
source(file.path(base_dir, 'code/cohort_dcon.R'))
source(file.path(base_dir, 'code/dcon_execute.R'))

message('Domain Concordance Check')
dcon_pt <- check_dcon(conc_tbls = conc_pts_list,
                      check_string = 'dcon_pts') %>%
  reduce(dplyr::union)
dcon_visit <- check_dcon(conc_tbls = conc_visits_list,
                         check_string = 'dcon_visits') %>%
  reduce(dplyr::union)
dcon_all <- dplyr::union(dcon_pt, dcon_visit)

output_tbl_append(dcon_all,
                  'dcon_output')