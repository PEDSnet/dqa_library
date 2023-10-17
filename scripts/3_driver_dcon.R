
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

dcon_meta <- check_dcon_pts_meta(conc_tbls_meta = conc_metadata)
output_tbl_append(dcon_meta,
                  'dcon_meta')

DBI::dbExecute(conn = config('db_src'), paste0("ALTER TABLE ", config('results_schema'), 
                                               ".dcon_output_op_1510 OWNER TO dcc_analytics;"))
DBI::dbExecute(conn = config('db_src'), paste0("ALTER TABLE ", config('results_schema'), 
                                               ".dcon_meta_op_1510 OWNER TO dcc_analytics;"))