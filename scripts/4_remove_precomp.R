Sys.setenv(PEDSNET_DATA_REQUEST_ROOT = "/data/airflow/dags/dqa_library/scripts")

library('RPostgres')
library('srcr')
library('tidyr')
library('lubridate')
library('stringr')
library('assertthat')

source('site/run.R')
source(file.path(base_dir, 'code/cohorts.R'))

remove_precompute()