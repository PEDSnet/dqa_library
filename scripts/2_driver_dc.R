library('RPostgres')
library('srcr')
library('tidyr')
library('lubridate')
library('stringr')
library('assertthat')

Sys.setenv(PEDSNET_DATA_REQUEST_ROOT = "/data/airflow/dags/dqa_library/scripts")

source('site/run.R')
source(file.path(base_dir, 'code/cohort_dc.R'))
source(file.path(base_dir, 'code/dc_execute.R'))

message('DC (Data Cycle) Check')
dc_output <- check_dc(prev_v_tbls = dc_args_prev,
                      current_v_tbls = dc_args_current,
                      meta_tbls = dc_args_meta)
dc_output_new <- dc_output[1:36]
dc_output_meta <- dc_output[[37]] 

dc_output_df <-
  reduce(.x=dc_output_new,
         .f=dplyr::union)

output_tbl_append(dc_output_df,
                  'dc_output')
output_tbl_append(dc_output_meta,
                  'dc_meta')