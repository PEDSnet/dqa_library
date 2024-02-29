


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

# pv_spec <- select(site_cdm_tbl('provider'), provider_id, specialty_concept_id) %>%
#   inner_join(select(site_cdm_tbl('visit_occurrence'), person_id, visit_occurrence_id, 
#                     provider_id)) %>%
#   filter(!is.na(specialty_concept_id))
# 
# cs_spec <- select(site_cdm_tbl('care_site'), care_site_id, specialty_concept_id) %>%
#   inner_join(select(site_cdm_tbl('visit_occurrence'), person_id, visit_occurrence_id, 
#                     care_site_id)) %>%
#   filter(!is.na(specialty_concept_id))

#' check element definitions for the `pf` check type
#' 
#' broken into all visits, outpatient visits, inpatient, ed visit lists
#' 
#' all have the same structure
#' 
#' all are a list of lists
#' 
#' list name: description
#' list first element: table to look for visit facts
#' second element: check name
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
    # 'all_visits_with_pv_spec' = list(pv_spec, 'pf_visits_pv_spec'),
    # 'all_visits_with_cs_spec' = list(cs_spec, 'pf_visits_cs_spec')
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
    # 'op_visits_with_pv_spec' = list(pv_spec, 'pf_opvisits_pv_spec'),
    # 'op_visits_with_cs_spec' = list(cs_spec, 'pf_opvisits_cs_spec')
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
    # 'ip_visits_with_pv_spec' = list(pv_spec, 'pf_ipvisits_pv_spec'),
    # 'ip_visits_with_cs_spec' = list(cs_spec, 'pf_ipvisits_cs_spec')
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
    # 'ed_visits_with_pv_spec' = list(pv_spec, 'pf_edvisits_pv_spec'),
    # 'ed_visits_with_cs_spec' = list(cs_spec, 'pf_edvisits_cs_spec')
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
    # 'all_visits_with_pv_spec' = list(pv_spec, 'pf_visits_pv_spec'),
    # 'all_visits_with_cs_spec' = list(cs_spec, 'pf_visits_cs_spec')
  )
