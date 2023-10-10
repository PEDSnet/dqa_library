
## Precompute outpatient specialty tables

op_prov_spec <- site_cdm_tbl('visit_occurrence') %>% 
  filter(visit_concept_id %in% c(9202L, 581399L)) %>%
  inner_join(select(site_cdm_tbl('provider'), provider_id, specialty_concept_id))

op_cs_spec <- site_cdm_tbl('visit_occurrence') %>% 
  filter(visit_concept_id %in% c(9202L, 581399L)) %>%
  inner_join(select(site_cdm_tbl('care_site'), care_site_id, specialty_concept_id))

op_spec <- select(site_cdm_tbl('visit_occurrence'), person_id, visit_concept_id,
                  provider_id, care_site_id) %>%
  filter(visit_concept_id %in% c(9202L, 581399L)) %>%
  pivot_longer(cols = c(provider_id, care_site_id),
               names_to = 'domain_id',
               values_to = 'entity_id') %>%
  mutate(domain_id = case_when(domain_id == 'provider_id' ~ 'PROVIDER',
                               domain_id == 'care_site_id' ~ 'CARE_SITE')) %>%
  mutate(entity_id = as.character(entity_id)) %>%
  inner_join(select(site_cdm_tbl('specialty'), domain_id, entity_id, 
                    specialty_concept_id))

#' 
#' `fact_tbl_list` - a list of arguments that feed into the `check_bmc_gen` function
#' 
#' There are 5 elements required in each list entry:
#'    1. The table where the concept is located
#'    2. The `*_concept_id` column with the relevant concept
#'    3. A plain language string label for the check
#'    4. The check name (formatted as `bmc_*`)
#'    5. The column in the `concept` table that is needed (either concept_name or concept_class_id)

fact_tbl_list <- list(
  
  'admin' = list(site_cdm_tbl('drug_exposure') %>%
                   filter(drug_type_concept_id %in% c(38000180L)),
                 'drug_concept_id',
                 'inpatient admin',
                 'bmc_rxnorm_di',
                 'concept_class_id'),

  'rx' =  list(site_cdm_tbl('drug_exposure') %>%
                 filter(drug_type_concept_id %in% c(38000177L)),
               'drug_concept_id',
               'prescriptions',
               'bmc_rxnorm_dp',
               'concept_class_id'),

  'race' = list(site_cdm_tbl('person'),
                'race_concept_id',
                'race',
                'bmc_race',
                'concept_name'),

  'ethnicity' = list(site_cdm_tbl('person'),
                     'ethnicity_concept_id',
                     'ethnicity',
                     'bmc_ethnicity',
                     'concept_name'),

  'op_pv_spec' = list(op_prov_spec,
                      'specialty_concept_id',
                      'outpatient provider specialty',
                      'bmc_pvvo_spec',
                      'concept_name'),

  'op_cs_spec' = list(op_cs_spec,
                      'specialty_concept_id',
                      'outpatient care site specialty',
                      'bmc_csvo_spec',
                      'concept_name'),
  
  'op_spec' = list(op_spec,
                   'specialty_concept_id',
                   'outpatient specialty (from specialty tbl)',
                   'bmc_vo_spec',
                   'concept_name')
  
)