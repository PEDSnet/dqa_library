

pcd <- site_cdm_tbl('procedure_occurrence') %>% select(person_id) %>% distinct()
drg <- site_cdm_tbl('drug_exposure') %>% select(person_id) %>% distinct()
ml <- site_cdm_tbl('measurement_labs') %>% select(person_id) %>% distinct()

pdl_pts <- pcd %>%
  inner_join(drg) %>% 
  inner_join(ml) %>% compute_new()

geocode_tbls <- prep_geocodes()
geocode_tract <- copy_to_new(df = geocode_tbls$tract_level)
geocode_cbg <- copy_to_new(df = geocode_tbls$block_group_level)

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
                           load_codeset('ecp_concepts', 'ciccc') %>% filter(concept_group == 'hemoglobin'),
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
                    load_codeset('ecp_concepts', 'ciccc') %>% filter(concept_group == 'anc'),
                    'ecp_anc'),
  
  'scr_labs' = list(site_cdm_tbl('measurement_labs'),
                    pdl_pts,
                    'measurement_concept_id',
                    load_codeset('ecp_concepts', 'ciccc') %>% filter(concept_group == 'creatinine_serum'),
                    'ecp_scr'),
  
  'sodium_labs' = list(site_cdm_tbl('measurement_labs'),
                       pdl_pts,
                       'measurement_concept_id',
                       load_codeset('ecp_concepts', 'ciccc') %>% filter(concept_group == 'sodium'),
                       'ecp_sodium'),
  
  'alanine_transaminase_labs' = list(site_cdm_tbl('measurement_labs'),
                                     pdl_pts,
                                     'measurement_concept_id',
                                     load_codeset('ecp_concepts', 'ciccc') %>% 
                                       filter(concept_group == 'alanine_transaminase'),
                                     'ecp_alanine_transaminase'),
  
  'urine_protein_qual_labs' = list(site_cdm_tbl('measurement_labs'),
                                   pdl_pts,
                              'measurement_concept_id',
                              load_codeset('ecp_concepts', 'ciccc') %>% 
                                filter(concept_group == 'urine_protein_qual'),
                              'ecp_urine_protein_qual'),
  
  # 'cholesterol_labs' = list(site_cdm_tbl('measurement_labs'),
  #                           pdl_pts,
  #                           'measurement_concept_id',
  #                           load_codeset('ecp_concepts', 'ciccc') %>% filter(concept_group == 'cholesterol_all'),
  #                           'ecp_cholesterol'),
   
  'rapid_strep_labs' = list(site_cdm_tbl('measurement_labs'),
                            pdl_pts,
                            'measurement_concept_id',
                            load_codeset('ecp_concepts', 'ciccc') %>% filter(concept_group == 'rapid_strep'),
                            'ecp_rapid_strep'),
  
  'flu_labs' = list(site_cdm_tbl('measurement_labs'),
                    pdl_pts,
                    'measurement_concept_id',
                    load_codeset('ecp_concepts', 'ciccc') %>% filter(concept_group == 'influenza'),
                    'ecp_flu'),
  
  'rsv_labs' = list(site_cdm_tbl('measurement_labs'),
                    pdl_pts,
                    'measurement_concept_id',
                    load_codeset('ecp_concepts', 'ciccc') %>% filter(concept_group == 'rsv'),
                    'ecp_rsv'),
  
  'head_circumference' = list(site_cdm_tbl('measurement_anthro'),
                              pdl_pts,
                              'measurement_concept_id',
                              load_codeset('ecp_concepts', 'ciccc') %>% 
                                filter(concept_group == 'head_circumference'),
                              'ecp_head_circumference'),
  
  'smoking_tobacco' = list(site_cdm_tbl('observation'),
                           pdl_pts,
                           'observation_concept_id',
                           load_codeset('ecp_concepts', 'ciccc') %>% 
                             filter(concept_group == 'smoking_tobacco'),
                           'ecp_smoking_tobacco'),
  
  'height' = list(site_cdm_tbl('measurement_anthro'),
                  pdl_pts,
                  'measurement_concept_id',
                  load_codeset('ecp_concepts', 'ciccc') %>% 
                    filter(concept_group == 'height'),
                  'ecp_height'),
  
  'weight' = list(site_cdm_tbl('measurement_anthro'),
                  pdl_pts,
                  'measurement_concept_id',
                  load_codeset('ecp_concepts', 'ciccc') %>% 
                    filter(concept_group == 'weight'),
                  'ecp_weight'),
  
  'tract_2010' = list(geocode_tract %>%
                        filter(ndigit_fips == 11 & geocode_year == 2010),
                      site_cdm_tbl('person') %>% select(site, person_id),
                      'geocode_year',
                      load_codeset('ecp_concepts', 'ciccc') %>% 
                        filter(concept_group == '2010_tract'),
                      'ecp_tract_2010'),
  
  'tract_2020' = list(geocode_tract %>%
                        filter(ndigit_fips == 11 & geocode_year == 2020),
                      site_cdm_tbl('person') %>% select(site, person_id),
                      'geocode_year',
                      load_codeset('ecp_concepts', 'ciccc') %>% 
                        filter(concept_group == '2020_tract'),
                      'ecp_tract_2020'),
  
  'block_group_2010' = list(geocode_cbg %>%
                              filter(ndigit_fips == 12 & geocode_year == 2010),
                            site_cdm_tbl('person') %>% select(site, person_id),
                            'geocode_year',
                            load_codeset('ecp_concepts', 'ciccc') %>% 
                              filter(concept_group == '2010_cbg'),
                            'ecp_block_group_2010'),
  
  'block_group_2020' = list(geocode_cbg %>%
                              filter(ndigit_fips == 12 & geocode_year == 2020),
                            site_cdm_tbl('person') %>% select(site, person_id),
                            'geocode_year',
                            load_codeset('ecp_concepts', 'ciccc') %>% 
                              filter(concept_group == '2020_cbg'),
                            'ecp_block_group_2020')
  
)
