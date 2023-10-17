
library('RPostgres')
library('srcr')
library('tidyr')
library('lubridate')
library('stringr')
library('assertthat')

Sys.setenv(PEDSNET_DATA_REQUEST_ROOT = "/data/airflow/dags/dqa_library/scripts")

source('site/run.R')
source(file.path(base_dir, 'code/cohort_vc_vs.R'))
source(file.path(base_dir, 'code/vc_vs_execute.R'))

message('VC (Vocabulary Conformance) and VS (Valueset Conformance) Check')
vc <- check_vc(vocabvals=vc_list)
vs <- check_vs(valuesets=vs_list)
vc_standard <- create_vc_vs_output(vc)
vs_standard <- create_vc_vs_output(vs)
vc_vs_joined <- c(vc_standard,
                  vs_standard)
vc_vs_final <- vc_vs_joined %>% reduce(.f=dplyr::union)


output_tbl_append(vc_vs_final,
                  'vc_vs_violations')
DBI::dbExecute(conn = config('db_src'), paste0("ALTER TABLE ", config('results_schema'), 
                                               ".vc_vs_violations_op_1510 OWNER TO dcc_analytics;"))