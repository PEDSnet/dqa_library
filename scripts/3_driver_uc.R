
library('RPostgres')
library('srcr')
library('tidyr')
library('lubridate')
library('stringr')
library('assertthat')

Sys.setenv(PEDSNET_DATA_REQUEST_ROOT = "/data/airflow/dags/dqa_library/scripts")

source('site/run.R')
source(file.path(base_dir, 'code/cohort_uc.R'))
source(file.path(base_dir, 'code/uc_execute.R'))

message('UC (Unmapped Concepts) Check')
uc <- check_uc(concept_list=uc_args_list)
uc_by_year <- check_uc_by_year(uc_args_list)
uc_reduce <- reduce(.x=uc,
                    .f=dplyr::union)
uc_by_year_reduce <- reduce(.x=uc_by_year,
                            .f=dplyr::union)

output_tbl_append(uc_reduce,
                  'uc_output')
output_tbl_append(uc_by_year_reduce,
                  'uc_by_year')