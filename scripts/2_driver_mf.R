
library('RPostgres')
library('srcr')
library('tidyr')
library('lubridate')
library('stringr')
library('assertthat')

Sys.setenv(PEDSNET_DATA_REQUEST_ROOT = "/data/airflow/dags/dqa_library/scripts")

source('site/run.R')
source(file.path(base_dir, 'code/cohort_mf_visitid.R'))
source(file.path(base_dir, 'code/mf_visitid_execute.R'))

message('MF VisitID (Missing Field: Visit Occurrence ID) Check')
mf_visitid <- check_mf_visitid(mf_visitid_list)
mf_visitid_reduce <- reduce(.x=mf_visitid,
                            .f=dplyr::union)

output_tbl_append(mf_visitid_reduce,
                  'mf_visitid_output')

DBI::dbExecute(conn = config('db_src'), paste0("ALTER TABLE ", config('results_schema'), 
                                               ".mf_visitid_output_op_1510 OWNER TO dcc_analytics;"))