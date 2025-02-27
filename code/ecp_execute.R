
## Patients with procedure, drug, AND lab
pcd <- site_cdm_tbl('procedure_occurrence') %>% select(person_id) %>% distinct() %>% compute_new()
drg <- site_cdm_tbl('drug_exposure') %>% select(person_id) %>% distinct() %>% compute_new()
ml <- site_cdm_tbl('measurement_labs') %>% select(person_id) %>% distinct() %>% compute_new()

pdl_pts <- pcd %>%
  inner_join(drg) %>%
  inner_join(ml) %>% 
  mutate(cohort_def = 'Patients with evidence of a procedure, drug, AND lab') %>%
  compute_new()

## Patients with face-to-face visit with a diagnosis, valid birth date, and valid sex
valid_ftf_dx <- cdm_tbl('visit_occurrence') %>%
  select(person_id, visit_occurrence_id, visit_concept_id) %>%
  filter(visit_concept_id %in% c(9201, 9202, 9203, 581399, 2000000048)) %>%
  inner_join(select(cdm_tbl('condition_occurrence'), person_id, 
                    visit_occurrence_id)) %>%
  select(person_id) %>%
  compute_new()

valid_demo <- cdm_tbl('person') %>%
  add_site() %>%
  inner_join(valid_ftf_dx) %>%
  filter(!is.na(birth_date) & 
           !gender_concept_id %in% c(44814650, 44814653, 44814649)) %>%
  distinct(site, person_id, location_id) %>% 
  mutate(cohort_def = 'Patients with a valid sex, valid DOB, and at least one face to face visit associated with a diagnosis') %>%
  compute_new()

geocode_tbls <- prep_geocodes(person_tbl = valid_demo)
geocode_tract <- copy_to_new(df = geocode_tbls$tract_level)
geocode_cbg <- copy_to_new(df = geocode_tbls$block_group_level)
geocode_lohis_tract <- copy_to_new(df = geocode_tbls$lohis_tract)
geocode_lohis_bg <- copy_to_new(df = geocode_tbls$lohis_bg)

## Patients with inpatient admission
ip_admit <- cdm_tbl('visit_occurrence') %>%
  filter(visit_concept_id %in% c(9201, 2000000048)) %>%
  distinct(person_id) %>%
  mutate(cohort_def = 'Patients with an inpatient admission (9201 or 2000000048)')

#' List of inputs for check_ecp
#'
#' Each element of the parent list should be named after the concept and domain of interest
#' 
#' Within each of those list elements, a second list should be constructed with the 
#' following elements in this exact order:
#' 
#'    1. the fact table that contains the concept of interest. 
#'    
#'       this can be filtered as needed within the list to tailor the fact table 
#'       (i.e. selecting only outpatient labs)
#'       
#'    2. the table with the desired population of patients to be used as the denominator
#'    
#'       this can be filtered as needed within the list to tailor the denominator
#'       (i.e. selecting only patients with a drug) and can differ between list elements
#'       
#'    3. the name of the column within the fact table that contains the relevant
#'       concept_ids (i.e. measurement_concept_id, condition_source_concept_id, etc.)
#'       
#'    4. a codeset that contains or is filtered down to codes for the concept of interest
#'    
#'    5. a string identifier for the check, prefixed with the check name like `ecp_*` 
#'    

ecp_codeset_list <- list(
  
  'hemoglobin_labs' = list(site_cdm_tbl('measurement_labs'),
                           pdl_pts,
                           'measurement_concept_id',
                           load_codeset('ecp_concepts', 'ciccc') %>% filter(concept_group == 'hemoglobin') %>%
                             compute_new(),
                           'ecp_hemoglobin'),

  'platelet_labs' = list(site_cdm_tbl('measurement_labs'),
                         pdl_pts,
                               'measurement_concept_id',
                               load_codeset('ecp_concepts', 'ciccc') %>%
                                 filter(concept_group == 'platelets'),
                               'ecp_platelet_count'),

  'anc_labs' = list(site_cdm_tbl('measurement_labs'),
                    pdl_pts,
                    'measurement_concept_id',
                    load_codeset('ecp_concepts', 'ciccc') %>% filter(concept_group == 'anc') %>%
                      compute_new(),
                    'ecp_anc'),

  'scr_labs' = list(site_cdm_tbl('measurement_labs'),
                    pdl_pts,
                    'measurement_concept_id',
                    load_codeset('ecp_concepts', 'ciccc') %>% filter(concept_group == 'creatinine_serum') %>%
                      compute_new(),
                    'ecp_scr'),

  'sodium_labs' = list(site_cdm_tbl('measurement_labs'),
                       pdl_pts,
                       'measurement_concept_id',
                       load_codeset('ecp_concepts', 'ciccc') %>% filter(concept_group == 'sodium') %>%
                         compute_new(),
                       'ecp_sodium'),

  'alanine_transaminase_labs' = list(site_cdm_tbl('measurement_labs'),
                                     pdl_pts,
                                     'measurement_concept_id',
                                     load_codeset('ecp_concepts', 'ciccc') %>%
                                       filter(concept_group == 'alanine_transaminase') %>%
                                       compute_new(),
                                     'ecp_alanine_transaminase'),

  'urine_protein_qual_labs' = list(site_cdm_tbl('measurement_labs'),
                                   pdl_pts,
                              'measurement_concept_id',
                              load_codeset('ecp_concepts', 'ciccc') %>%
                                filter(concept_group == 'urine_protein_qual') %>%
                                compute_new(),
                              'ecp_urine_protein_qual'),

  # 'cholesterol_labs' = list(site_cdm_tbl('measurement_labs'),
  #                           pdl_pts,
  #                           'measurement_concept_id',
  #                           load_codeset('ecp_concepts', 'ciccc') %>% filter(concept_group == 'cholesterol_all'),
  #                           'ecp_cholesterol'),

  'rapid_strep_labs' = list(site_cdm_tbl('measurement_labs'),
                            pdl_pts,
                            'measurement_concept_id',
                            load_codeset('ecp_concepts', 'ciccc') %>% filter(concept_group == 'rapid_strep') %>%
                              compute_new(),
                            'ecp_rapid_strep'),

  'flu_labs' = list(site_cdm_tbl('measurement_labs'),
                    pdl_pts,
                    'measurement_concept_id',
                    load_codeset('ecp_concepts', 'ciccc') %>% filter(concept_group == 'influenza') %>%
                      compute_new(),
                    'ecp_flu'),

  'rsv_labs' = list(site_cdm_tbl('measurement_labs'),
                    pdl_pts,
                    'measurement_concept_id',
                    load_codeset('ecp_concepts', 'ciccc') %>% filter(concept_group == 'rsv') %>%
                      compute_new(),
                    'ecp_rsv'),

  'head_circumference' = list(site_cdm_tbl('measurement_anthro'),
                              pdl_pts,
                              'measurement_concept_id',
                              load_codeset('ecp_concepts', 'ciccc') %>%
                                filter(concept_group == 'head_circumference') %>%
                                compute_new(),
                              'ecp_head_circumference'),

  'smoking_tobacco' = list(site_cdm_tbl('observation'),
                           pdl_pts,
                           'observation_concept_id',
                           load_codeset('ecp_concepts', 'ciccc') %>%
                             filter(concept_group == 'smoking_tobacco') %>%
                             compute_new(),
                           'ecp_smoking_tobacco'),

  'height' = list(site_cdm_tbl('measurement_anthro'),
                  pdl_pts,
                  'measurement_concept_id',
                  load_codeset('ecp_concepts', 'ciccc') %>%
                    filter(concept_group == 'height') %>%
                    compute_new(),
                  'ecp_height'),

  'weight' = list(site_cdm_tbl('measurement_anthro'),
                  pdl_pts,
                  'measurement_concept_id',
                  load_codeset('ecp_concepts', 'ciccc') %>%
                    filter(concept_group == 'weight') %>%
                    compute_new(),
                  'ecp_weight'),
  
  'tract_2010' = list(geocode_tract %>%
                        filter(ndigit_fips == 11 & geocode_year == 2010),
                      valid_demo %>% select(-location_id),
                      'geocode_year',
                      load_codeset('ecp_concepts', 'ciccc') %>% 
                        filter(concept_group == '2010_tract') %>%
                        compute_new(),
                      'ecp_tract_2010'),
  
  'tract_2020' = list(geocode_tract %>%
                        filter(ndigit_fips == 11 & geocode_year == 2020),
                      valid_demo %>% select(-location_id),
                      'geocode_year',
                      load_codeset('ecp_concepts', 'ciccc') %>% 
                        filter(concept_group == '2020_tract') %>%
                        compute_new(),
                      'ecp_tract_2020'),
  
  'block_group_2010' = list(geocode_cbg %>%
                              filter(ndigit_fips == 12 & geocode_year == 2010),
                            valid_demo %>% select(-location_id),
                            'geocode_year',
                            load_codeset('ecp_concepts', 'ciccc') %>% 
                              filter(concept_group == '2010_cbg') %>%
                              compute_new(),
                            'ecp_block_group_2010'),
  
  'block_group_2020' = list(geocode_cbg %>%
                              filter(ndigit_fips == 12 & geocode_year == 2020),
                            valid_demo %>% select(-location_id),
                            'geocode_year',
                            load_codeset('ecp_concepts', 'ciccc') %>% 
                              filter(concept_group == '2020_cbg') %>%
                              compute_new(),
                            'ecp_block_group_2020'),
  
  'twoplus_lohis_tract_2010' = list(geocode_lohis_tract %>%
                                filter(geocode_year == 2010, ngeo_lohis > 1),
                              valid_demo %>% select(-location_id),
                              'geocode_year',
                              load_codeset('ecp_concepts', 'ciccc') %>% 
                                filter(concept_group == '2010_lohis_tract') %>%
                                compute_new(),
                              'ecp_twoplus_lohis_tract_2010'),
  
  'twoplus_lohis_cbg_2010' = list(geocode_lohis_bg %>%
                                      filter(geocode_year == 2010, ngeo_lohis > 1),
                                    valid_demo %>% select(-location_id),
                                    'geocode_year',
                                    load_codeset('ecp_concepts', 'ciccc') %>% 
                                      filter(concept_group == '2010_lohis_cbg') %>%
                                    compute_new(),
                                    'ecp_twoplus_lohis_cbg_2010'),
  
  'twoplus_lohis_tract_2020' = list(geocode_lohis_tract %>%
                                filter(geocode_year == 2020, ngeo_lohis > 1),
                              valid_demo %>% select(-location_id),
                              'geocode_year',
                              load_codeset('ecp_concepts', 'ciccc') %>% 
                                filter(concept_group == '2020_lohis_tract') %>%
                                compute_new(),
                              'ecp_twoplus_lohis_tract_2020'),
  
  'twoplus_lohis_cbg_2020' = list(geocode_lohis_bg %>%
                                      filter(geocode_year == 2020, ngeo_lohis > 1),
                                    valid_demo %>% select(-location_id),
                                    'geocode_year',
                                    load_codeset('ecp_concepts', 'ciccc') %>% 
                                      filter(concept_group == '2020_lohis_cbg') %>%
                                    compute_new(),
                                    'ecp_twoplus_lohis_cbg_2020'),
  
  'blood_culture_labs' = list(site_cdm_tbl('measurement_labs'),
                              ip_admit,
                              'measurement_concept_id',
                              load_codeset('ecp_concepts', 'ciccc') %>% 
                                filter(concept_group == 'blood_culture_labs') %>%
                                compute_new(),
                              'ecp_blood_culture_labs'),
  
  'blood_culture_px' = list(site_cdm_tbl('procedure_occurrence'),
                            ip_admit,
                            'procedure_concept_id',
                            load_codeset('ecp_concepts', 'ciccc') %>% 
                              filter(concept_group == 'blood_culture_px') %>%
                              compute_new(),
                            'ecp_blood_culture_px')
  
)
