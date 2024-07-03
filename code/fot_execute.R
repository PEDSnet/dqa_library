
#' list element definitions for `fot` check type
#' 
#' List of lists
#' 
#' List Name: Check Name (output as check name)
#' List First Element: The object for which the `fot` check is run against
#' List Second Element: Check Description
#' 

time_tbls_list = list(
  'fot_im' = list(site_cdm_tbl('immunization'), 'all immunizations'),
  'fot_im_covid19' = list(site_cdm_tbl('immunization') %>% inner_join(load_codeset('c19_immunizations'),
                                                                      by=c('immunization_concept_id'='concept_id')), 'covid19 immunizations'),
  #'fot_pr_appendectomy' = list(site_cdm_tbl('procedure_occurrence') %>% inner_join(load_codeset('appendectomy'),
                                                                                   #by=c('procedure_concept_id'='concept_id')), 'appendectomy procedures'),
  'fot_vi' = list(site_cdm_tbl('visit_occurrence'), 'all visits'),
  'fot_vo_office' = list(site_cdm_tbl('visit_occurrence') %>% filter(visit_concept_id == 9202L), 'outpatient visits'),
  'fot_vo_labs' = list(site_cdm_tbl('visit_occurrence') %>% filter(visit_concept_id == 2000000469L), 'outpatient lab visits'),
  'fot_vo_th' = list(site_cdm_tbl('visit_occurrence') %>% filter(visit_concept_id == 581399L), 'telehealth visits'),
  'fot_vo_oa' = list(site_cdm_tbl('visit_occurrence') %>% filter(visit_concept_id == 44814711L), 'other ambulatory visits'),
  'fot_vip' = list(site_cdm_tbl('visit_occurrence') %>% filter(visit_concept_id == 9201L), 'inpatient visits, only 9201'),
  'fot_vip_ipcombined' = list(site_cdm_tbl('visit_occurrence') %>% filter(visit_concept_id == 2000000048L), 'ED to IP combined visits'),
  'fot_vob' = list(site_cdm_tbl('visit_occurrence') %>% filter(visit_concept_id == 2000000088L), 'Observation visits'),
  'fot_co' = list(site_cdm_tbl('condition_occurrence'), 'all conditions'),
  'fot_prvo' = list(results_tbl(paste0(config('site'),'_prvo')), 'outpatient procedures'),
  'fot_adt_picu' = list(site_cdm_tbl('adt_occurrence') %>% filter(service_concept_id == 2000000078L), 'picu'),
  'fot_adt_nicu' = list(site_cdm_tbl('adt_occurrence') %>% filter(service_concept_id == 2000000080L), 'nicu'),
  'fot_adt_cicu' = list(site_cdm_tbl('adt_occurrence') %>% filter(service_concept_id == 2000000079L), 'cicu'),
  'fot_dp' = list(site_cdm_tbl('drug_exposure') %>% filter(drug_type_concept_id ==  38000177L), 'prescription drugs'),
  'fot_ip' = list(site_cdm_tbl('drug_exposure') %>% filter(drug_type_concept_id == 38000180L), 'inpatient administration drugs'),
  'fot_cop' = list(site_cdm_tbl('condition_occurrence') %>% filter(condition_type_concept_id %in%
                                                                                  c(2000000095L,
                                                                                    2000000096L,
                                                                                    2000000097L,
                                                                                    2000000101L,
                                                                                    2000000102L,
                                                                                    2000000103L)), 'outpatient conditions primary and secondary'),
  'fot_ci' = list(site_cdm_tbl('condition_occurrence') %>% filter(condition_type_concept_id %in%
                                                                                 c(2000000092L,
                                                                                   2000000093L,
                                                                                   2000000094L,
                                                                                   2000000098L,
                                                                                   2000000099L,
                                                                                   2000000100L)), 'inpatient conditions primary and secondary'),
  'fot_ced' = list(site_cdm_tbl('condition_occurrence') %>% filter(condition_type_concept_id %in%
                                                                          c(2000001280L,
                                                                            2000001281L,
                                                                            2000001282L,
                                                                            2000001283L,
                                                                            2000001284L,
                                                                            2000001285L)), 'ED conditions primary and secondary'),
  'fot_cop_cb' = list(site_cdm_tbl('condition_occurrence') %>% filter(condition_type_concept_id %in%
                                                                                  c(2000000096L,
                                                                                    2000000097L,
                                                                                    2000000102L,
                                                                                    2000000103L)), 'Outpatient billing'),
  'fot_cop_cc' = list(site_cdm_tbl('condition_occurrence') %>% filter(condition_type_concept_id %in%
                                                                                c(2000000095L, 2000000101)), 'Outpatient order'),
  'fot_ci_cc' = list(site_cdm_tbl('condition_occurrence') %>% filter(condition_type_concept_id %in%
                                                                                c(2000000092L, 2000000098L)), 'Inpatient condition order'),
  'fot_ci_cb' = list(site_cdm_tbl('condition_occurrence') %>% filter(condition_type_concept_id %in%
                                                                                  c(2000000099L,
                                                                                    2000000100L,
                                                                                    2000000093L,
                                                                                    2000000094L)), 'Inpatient condition billing'),
  'fot_voml' = list(results_tbl(paste0(config('site'),'_voml')), 'outpatient labs (9202)'),
  
  'fot_vodi' = list(results_tbl(paste0(config('site'),'_vodi')), 'outpatient med administration'),
  
  
  'fot_vipdp' = list(results_tbl(paste0(config('site'),'_vipdp')), 'inpatient prescriptions'),

  'fot_ma_ht' = list(site_cdm_tbl('measurement_anthro') %>%
                       filter(measurement_concept_id == 3023540L), 'Height'),

  'fot_ma_wt' = list(site_cdm_tbl('measurement_anthro') %>%
                       filter(measurement_concept_id == 3013762L), 'Weight'),

  'fot_mv_sbp' = list(site_cdm_tbl('measurement_vitals') %>% filter(measurement_concept_id %in%
                                                                      c(3018586L,
                                                                        3035856L,
                                                                        3009395L,
                                                                        3004249L)), 'Systolic Blood Pressure'),

  'fot_mv_dbp' = list(site_cdm_tbl('measurement_vitals') %>% filter(measurement_concept_id %in%
                                                                      c(3034703L,
                                                                        3019962L,
                                                                        3013940L,
                                                                        3012888L)), 'Diastolic Blood Pressure')

)


