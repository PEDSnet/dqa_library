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
pf_ipvisits <- check_pf_visits(ip_list,
                               visit_tbl = site_cdm_tbl('visit_occurrence') %>%
                                 filter(visit_concept_id %in% c(9201L,2000000048L)))

pf_reduce <-
  reduce(.x=pf_ipvisits,
         .f=dplyr::union)

output_tbl_append(pf_reduce,
                  'pf_output')
