library('RPostgres')
library('srcr')
library('tidyr')
library('lubridate')
library('stringr')
library('assertthat')

Sys.setenv(PEDSNET_DATA_REQUEST_ROOT = "/data/airflow/dags/dqa_library/scripts")

source('site/run.R')
source(file.path(base_dir, 'code/cohort_pf_visits.R'))
source(file.path(base_dir, 'code/pf_visits_execute.R'))

message('PF Visits (Patient Facts for Visits) Check')
pf_edvisits <- check_pf_visits(ed_list,
                               visit_tbl = site_cdm_tbl('visit_occurrence') %>%
                                 filter(visit_concept_id %in% c(9203L,2000000048L)))

pf_reduce <-
  reduce(.x=pf_edvisits,
         .f=dplyr::union)

output_tbl_append(pf_reduce,
                  'pf_output')

DBI::dbExecute(conn = config('db_src'), paste0("ALTER TABLE ", config('results_schema'), 
                                               ".pf_output_op_1510 OWNER TO dcc_analytics;"))
