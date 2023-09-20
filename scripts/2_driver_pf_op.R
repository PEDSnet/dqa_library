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
pf_opvisits <- check_pf_visits(op_list,
                               visit_tbl = site_cdm_tbl('visit_occurrence') %>%
                                 filter(visit_concept_id %in% c(9202L,581399L)))

pf_reduce <-
  reduce(.x=pf_opvisits,
         .f=dplyr::union)

output_tbl_append(pf_reduce,
                  'pf_output')
