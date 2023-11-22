
library('RPostgres')
library('srcr')
library('tidyr')
library('lubridate')
library('stringr')
library('assertthat')

Sys.setenv(PEDSNET_DATA_REQUEST_ROOT = "/data/airflow/dags/dqa_library/scripts")

source('site/run.R')
source(file.path(base_dir, 'code/cohort_ecp.R'))
source(file.path(base_dir, 'code/ecp_execute.R'))


message('Expected Concepts Present')
ecp <- check_ecp(ecp_codeset_list)

output_tbl_append(ecp,
                  'ecp_output')

DBI::dbExecute(conn = config('db_src'), paste0("ALTER TABLE ", config('results_schema'), 
                                               ".ecp_output_op_1510 OWNER TO dcc_analytics;"))