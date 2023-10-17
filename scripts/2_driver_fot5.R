library('RPostgres')
library('srcr')
library('tidyr')
library('lubridate')
library('stringr')
library('assertthat')

Sys.setenv(PEDSNET_DATA_REQUEST_ROOT = "/data/airflow/dags/dqa_library/scripts")

source('site/run.R')
source(file.path(base_dir, 'code/cohort_fot.R'))
source(file.path(base_dir, 'code/fot_execute.R'))

message('FOT (Facts Over Time) Check')
fot_all <- check_fot(time_tbls = time_tbls_list[c(31,32)],
                     visits_only = FALSE)
fot_all_reduce <- reduce(.x=fot_all,
                         .f=dplyr::union)
output_tbl_append(fot_all_reduce,
                  'fot_output')

DBI::dbExecute(conn = config('db_src'), paste0("ALTER TABLE ", config('results_schema'), 
                                               ".fot_output_op_1510 OWNER TO dcc_analytics;"))