
############################## TABLE PREP #######################################

procs_drugs <- 
  dplyr::union(site_cdm_tbl('procedure_occurrence') %>% select(person_id,visit_occurrence_id),
               site_cdm_tbl('drug_exposure') %>% select(person_id,visit_occurrence_id)) 
procs_drugs_labs <- 
  dplyr::union(site_cdm_tbl('procedure_occurrence') %>% select(person_id,visit_occurrence_id),
               site_cdm_tbl('drug_exposure') %>% select(person_id,visit_occurrence_id)) %>%
  dplyr::union(site_cdm_tbl('measurement_labs') %>% select(person_id,visit_occurrence_id))

icu_transfer <- 
  site_cdm_tbl('adt_occurrence') %>% 
  filter(service_concept_id %in% c(2000000079L,2000000080L,2000000078L)) %>% 
           select(person_id,visit_occurrence_id) %>% distinct()

visit_payer <- (select(site_cdm_tbl('visit_payer'), visit_occurrence_id, visit_payer_id)) %>%
  inner_join(select(site_cdm_tbl('visit_occurrence'), person_id, visit_occurrence_id)) %>%
  select(person_id, visit_occurrence_id) 


########################## TABLE ARGS ########################################

#' patient facts element definitions
#' 
#' broken into all visits, outpatient visits, inpatient, ed visit lists by default,
#' but the same list input can also be used for multiple executions
#' 
#' for each list, should be structured as a list of lists with the following
#' elements:
#' 
#' name: the check description
#'    1. The CDM table with the fact type of interest
#'    2. the check name identifier
#' 

all_list <- 
  list(
    'all_visits_with_procedures' = list(site_cdm_tbl('procedure_occurrence'), 'pf_visits_pr'),
    'all_visits_with_conditions' = list(site_cdm_tbl('condition_occurrence'), 'pf_visits_co'),
    'all_visits_with_drugs' = list(site_cdm_tbl('drug_exposure'), 'pf_visits_dr'),
    'all_visits_with_procs_drugs' = list(procs_drugs, 'pf_visits_prdr'),
    'all_lab_visits' = list(site_cdm_tbl('measurement_labs'), 'pf_visits_ml'),
    'all_visits_with_procs_drugs_labs' = list(procs_drugs_labs, 'pf_visits_prdrml'),
    'all_visits_with_payer' = list(visit_payer, 'pf_visits_vp'),
    'all_visits_with_immunizations' = list(site_cdm_tbl('immunization'), 'pf_visits_im')
  )

op_list <-
  list(
    'op_visits_with_procedures' = list(site_cdm_tbl('procedure_occurrence'),'pf_opvisits_pr'),
    'op_visits_with_conditions' = list(site_cdm_tbl('condition_occurrence'),'pf_opvisits_co'),
    'op_visits_with_drugs' = list(site_cdm_tbl('drug_exposure'), 'pf_opvisits_dr'),
    'op_visits_with_procs_drugs' = list(procs_drugs, 'pf_opvisits_prdr'),
    'op_lab_visits' = list(site_cdm_tbl('measurement_labs'),'pf_opvisits_ml'),
    'op_all_visits_with_procs_drugs_labs' = list(procs_drugs_labs, 'pf_opvisits_prdrml'),
    'op_visits_with_payer' = list(visit_payer, 'pf_opvisits_vp'),
    'op_visits_with_immunizations' = list(site_cdm_tbl('immunization'), 'pf_opvisits_im')
  )

ip_list <-
  list(
    'ip_visits_with_procedures' = list(site_cdm_tbl('procedure_occurrence'), 'pf_ipvisits_pr'),
    'ip_visits_with_conditions' = list(site_cdm_tbl('condition_occurrence'), 'pf_ipvisits_co'),
    'ip_visits_with_drugs' = list(site_cdm_tbl('drug_exposure'), 'pf_ipvisits_dr'),
    'ip_visits_with_procs_drugs' = list(procs_drugs, 'pf_ipvisits_prdr'),
    'ip_lab_visits' = list(site_cdm_tbl('measurement_labs'), 'pf_ipvisits_ml'),
    'ip_all_visits_with_procs_drugs_labs' = list(procs_drugs_labs, 'pf_ipvisits_prdrml'),
    'ip_icu'=list(icu_transfer,'pf_ipvisits_icu'),
    'ip_visits_with_payer' = list(visit_payer, 'pf_ipvisits_vp'),
    'ip_visits_with_immunizations' = list(site_cdm_tbl('immunization'), 'pf_ipvisits_im')
  )


ed_list <-
  list(
    'ed_visits_with_procedures' = list(site_cdm_tbl('procedure_occurrence'), 'pf_edvisits_pr'),
    'ed_visits_with_conditions' = list(site_cdm_tbl('condition_occurrence'), 'pf_edvisits_co'),
    'ed_visits_with_drugs' = list(site_cdm_tbl('drug_exposure'), 'pf_edvisits_dr'),
    'ed_visits_with_procs_drugs' = list(procs_drugs, 'pf_edvisits_prdr'),
    'ed_lab_visits' = list(site_cdm_tbl('measurement_labs'), 'pf_edvisits_ml'),
    'ed_all_visits_with_procs_drugs_labs' = list(procs_drugs_labs, 'pf_edvisits_prdrml'),
    'ed_visits_with_payer' = list(visit_payer, 'pf_edvisits_vp'),
    'ed_visits_with_immunizations' = list(site_cdm_tbl('immunization'), 'pf_edvisits_im')
  )

long_ip_list <- 
  list(
    'long_ip_visits_with_procedures' = list(site_cdm_tbl('procedure_occurrence'), 'pf_lipvisits_pr'),
    'long_ip_visits_with_conditions' = list(site_cdm_tbl('condition_occurrence'), 'pf_lipvisits_co'),
    'long_ip_visits_with_drugs' = list(site_cdm_tbl('drug_exposure'), 'pf_lipvisits_dr'),
    'long_ip_visits_with_procs_drugs' = list(procs_drugs, 'pf_lipvisits_prdr'),
    'long_ip_lab_visits' = list(site_cdm_tbl('measurement_labs'), 'pf_lipvisits_ml'),
    'long_ip_visits_with_procs_drugs_labs' = list(procs_drugs_labs, 'pf_lipvisits_prdrml'),
    'long_ip_icu'=list(icu_transfer,'pf_lipvisits_icu'),
    'long_ip_visits_with_payer' = list(visit_payer, 'pf_lipvisits_vp'),
    'long_ip_visits_with_immunizations' = list(site_cdm_tbl('immunization'), 'pf_lipvisits_im')
  )
