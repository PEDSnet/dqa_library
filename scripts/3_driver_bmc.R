
library('RPostgres')
library('srcr')
library('tidyr')
library('lubridate')
library('stringr')
library('assertthat')

Sys.setenv(PEDSNET_DATA_REQUEST_ROOT = "/data/airflow/dags/dqa_library/scripts")

source('site/run.R')
source(file.path(base_dir, 'code/cohort_bmc_gen.R'))
source(file.path(base_dir, 'code/bmc_gen_execute.R'))

message('BMC (Best Mapped Concepts) Check')
bmc_gen <- check_bmc_gen(fact_tbl_list)
bmc_gen_reduce <- reduce(.x=bmc_gen,
                         .f=dplyr::union)

output_tbl_append(bmc_gen_reduce,
                  'bmc_gen_output')

DBI::dbExecute(conn = config('db_src'), paste0("ALTER TABLE ", config('results_schema'), 
                                               ".bmc_gen_output_op_1510 OWNER TO dcc_analytics;"))