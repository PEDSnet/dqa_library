
# Outpatient Labs
site_voml <- site_cdm_tbl('measurement_labs') %>%
  inner_join(select(site_cdm_tbl('visit_occurrence'),
    visit_occurrence_id, visit_concept_id)) %>%
  filter(visit_concept_id == 9202L)

output_tbl(site_voml, paste0(config('site'), '_voml'),
           indexes = c('person_id', 'visit_occurrence_id'))

## Outpatient Med Admin
site_vodi <- site_cdm_tbl('drug_exposure') %>%
  filter(drug_type_concept_id == 38000180L) %>%
  inner_join(select(site_cdm_tbl('visit_occurrence'),
    visit_occurrence_id, visit_concept_id)) %>% 
  filter(visit_concept_id == 9202L)

output_tbl(site_vodi, paste0(config('site'), '_vodi'), 
           indexes = c('person_id', 'visit_occurrence_id'))

## Inpatient Prescriptions
site_vipdp <- site_cdm_tbl('drug_exposure') %>%
  filter(drug_type_concept_id == 38000177L) %>%
  inner_join(select(site_cdm_tbl('visit_occurrence'),
    visit_occurrence_id, visit_concept_id)) %>% 
  filter(visit_concept_id %in% c(9201L, 2000000048L))

output_tbl(site_vipdp, paste0(config('site'), '_vipdp'),
           indexes = c('person_id', 'visit_occurrence_id'))

## Outpatient Procedures
site_prvo <- site_cdm_tbl('procedure_occurrence') %>%
  inner_join(select(site_cdm_tbl('visit_occurrence'),
    visit_occurrence_id, visit_concept_id)) %>% 
  filter(visit_concept_id == 9202L)

output_tbl(site_prvo, paste0(config('site'), '_prvo'),
           indexes = c('person_id', 'visit_occurrence_id'))

## CKD Conditions
site_ckddx <- site_cdm_tbl('condition_occurrence') %>% 
  inner_join(load_codeset('dx_ckd','iccccc'),
             by=c('condition_concept_id'='concept_id'))

output_tbl(site_ckddx, paste0(config('site'), '_ckddx'),
           indexes = c('person_id', 'visit_occurrence_id'))

## HTN Prescription
site_htnrx <- site_cdm_tbl('drug_exposure') %>% 
  inner_join(
    load_codeset('rx_htn'),
    by=c('drug_concept_id'='concept_id'))

output_tbl(site_htnrx, paste0(config('site'), '_htnrx'),
           indexes = c('person_id', 'visit_occurrence_id'))

## Procedures, Drugs, AND Labs (ECP)

pdl_pts <- 
  dplyr::inner_join(site_cdm_tbl('procedure_occurrence') %>% select(person_id),
               site_cdm_tbl('drug_exposure') %>% select(person_id)) %>%
  dplyr::inner_join(site_cdm_tbl('measurement_labs') %>% select(person_id))

output_tbl(pdl_pts, paste0(config('site'), '_pdl_pts'),
           indexes = c('person_id'))