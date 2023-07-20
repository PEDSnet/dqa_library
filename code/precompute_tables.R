
# Outpatient Labs
site_voml <- site_cdm_tbl('measurement_labs') %>%
  inner_join(select(site_cdm_tbl('visit_occurrence'),
    visit_occurrence_id, visit_concept_id)) %>%
  filter(visit_concept_id == 9202L)

output_tbl(site_voml, 'site_voml',
           indexes = c('person_id', 'visit_occurrence_id'))

## Outpatient Med Admin
site_vodi <- site_cdm_tbl('drug_exposure') %>%
  filter(drug_type_concept_id == 38000180L) %>%
  inner_join(select(site_cdm_tbl('visit_occurrence'),
    visit_occurrence_id, visit_concept_id)) %>% 
  filter(visit_concept_id == 9202L)

output_tbl(site_vodi, 'site_vodi', 
           indexes = c('person_id', 'visit_occurrence_id'))

## Inpatient Prescriptions
site_vipdp <- site_cdm_tbl('drug_exposure') %>%
  filter(drug_type_concept_id == 38000177L) %>%
  inner_join(select(site_cdm_tbl('visit_occurrence'),
    visit_occurrence_id, visit_concept_id)) %>% 
  filter(visit_concept_id %in% c(9201L, 2000000048L))

output_tbl(site_vipdp, 'site_vipdp',
           indexes = c('person_id', 'visit_occurrence_id'))

## Outpatient Procedures
site_prvo <- site_cdm_tbl('procedure_occurrence') %>%
  inner_join(select(site_cdm_tbl('visit_occurrence'),
    visit_occurrence_id, visit_concept_id)) %>% 
  filter(visit_concept_id == 9202L)

output_tbl(site_prvo, 'site_prvo',
           indexes = c('person_id', 'visit_occurrence_id'))

## CKD Conditions
site_ckddx <- site_cdm_tbl('condition_occurrence') %>% 
  inner_join(load_codeset('dx_ckd','iccccc'),
             by=c('condition_concept_id'='concept_id'))

output_tbl(site_ckddx, 'site_ckddx',
           indexes = c('person_id', 'visit_occurrence_id'))

## HTN Prescription
site_htnrx <- site_cdm_tbl('drug_exposure') %>% 
  inner_join(
    load_codeset('rx_htn'),
    by=c('drug_concept_id'='concept_id'))

output_tbl(site_htnrx, 'site_htnrx',
           indexes = c('person_id', 'visit_occurrence_id'))

