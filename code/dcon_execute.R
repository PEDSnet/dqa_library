


#' list element definitions for the `dcon_pts` check type
#' 
#' List of lists
#' 
#' Name of list: check description
#' First Element: First Domain
#' Second Element: Second Domain
#' Third Element: Check Name
#' Fourth Element: chronic vs acute identifier (controls the distance between events required by the function)
#' 
#' The order of first and second element must match the description order
#' 

conc_pts_list <-
  list(
    'pts_with_ckd_dx_and_htn_rx' = list(results_tbl(paste0(config('site'),'_ckddx')),
                                        results_tbl(paste0(config('site'),'_htnrx')),
                                        'dcon_pts_ckd-dx_htn-rx',
                                        'chronic'),
    'asthma_dx_broncho_rx' = list(site_cdm_tbl('condition_occurrence') %>%
                                    select(site, person_id, condition_concept_id, condition_start_date) %>%
                                    inner_join(load_codeset('dx_asthma'), by = c('condition_concept_id' = 'concept_id')) ,
                                  site_cdm_tbl('drug_exposure') %>%
                                    select(site, person_id, drug_concept_id, drug_exposure_start_date) %>%
                                    inner_join(load_codeset('rx_albuterol'), by = c('drug_concept_id' = 'concept_id')) ,
                                  'dcon_asthma_dx_broncho_rx',
                                  'acute'),
    'leukemia_dx_onco_spec' = list(site_cdm_tbl('condition_occurrence') %>% select(site, person_id, condition_concept_id, condition_start_date) %>%
                                     inner_join(load_codeset('dx_leukemia_comb'), by = c('condition_concept_id' = 'concept_id')),
                                   site_cdm_tbl('visit_occurrence') %>% inner_join(site_cdm_tbl('provider'), by = c('site', 'provider_id')) %>%
                                     select(site, person_id, provider_id, specialty_concept_id, visit_start_date) %>%
                                     inner_join(load_codeset('oncology_edit'), by = c('specialty_concept_id' = 'concept_id')),
                                   'dcon_leukemia_dx_onco_spec',
                                   'acute'),
    'nephsyn_dx_neph_spec' = list(site_cdm_tbl('condition_occurrence') %>% select(site, person_id, condition_concept_id, condition_start_date) %>%
                                    inner_join(load_codeset('dx_nephrotic_syndrome'), by = c('condition_concept_id' = 'concept_id')),
                                  site_cdm_tbl('visit_occurrence') %>% inner_join(site_cdm_tbl('provider'), by = c('site', 'provider_id')) %>%
                                    select(site, person_id, provider_id, specialty_concept_id, visit_start_date) %>%
                                    inner_join(load_codeset('nephrology'), by = c('specialty_concept_id' = 'concept_id')),
                                  'dcon_nephsyn_dx_neph_spec',
                                  'chronic'),
    'frac_dx_img_px' = list(cdm_tbl('condition_occurrence') %>% select(site, person_id, condition_concept_id, condition_start_date) %>%
                              inner_join(load_codeset('dx_fracture'), by = c('condition_concept_id' = 'concept_id')),
                            cdm_tbl('procedure_occurrence') %>% select(site, person_id, procedure_concept_id, procedure_date) %>%
                              inner_join(load_codeset('px_radiologic'), by = c('procedure_concept_id' = 'concept_id')),
                            'dcon_frac_dx_img_px',
                            'acute'),
    't1d_dx_insulin_rx' = list(cdm_tbl('condition_occurrence') %>% select(site, person_id, condition_concept_id, condition_start_date) %>%
                                 inner_join(load_codeset('T1D_SNOMED_codes'), by = c('condition_concept_id' = 'concept_id')),
                               cdm_tbl('drug_exposure') %>% select(site, person_id, drug_concept_id, drug_exposure_start_date) %>%
                                 inner_join(load_codeset('insulin'), by = c('drug_concept_id' = 'concept_id')),
                               'dcon_t1d_dx_insulin_rx',
                               'acute')
  )

conc_visits_list <-
  list('ED_visits_ED_conds' = list(site_cdm_tbl('visit_occurrence') %>% filter(visit_concept_id==9203L) %>%
                                     select(site, person_id, visit_occurrence_id, visit_start_date),
                                   site_cdm_tbl('condition_occurrence') %>% filter(
                                     condition_type_concept_id %in% c(2000001280L,2000001281L,2000001282L,
                                                                      2000001283L,2000001284L,2000001285L)) %>%
                                     select(site, person_id, visit_occurrence_id, condition_concept_id, condition_start_date), 
                                   'dcon_ed_visits_conds',
                                   'visit'),
       'IP_visits_IP_conds' = list(site_cdm_tbl('visit_occurrence') %>% filter(visit_concept_id %in% c(9201)) %>%
                                     select(site, person_id, visit_occurrence_id, visit_start_date),
                                   site_cdm_tbl('condition_occurrence') %>% filter(condition_type_concept_id %in%
                                                                                     c(2000000092L,2000000093L,
                                                                                       2000000094L,2000000098L,
                                                                                       2000000099L,2000000100L)) %>%
                                     select(site, person_id, visit_occurrence_id, condition_concept_id, condition_start_date),
                                   'dcon_ip_visits_conds',
                                   'visit'),
       'OP_visits_op_conds' = list(site_cdm_tbl('visit_occurrence') %>% filter(visit_concept_id %in% c(9202L)) %>%
                                     select(site, person_id, visit_occurrence_id, visit_start_date),
                                   site_cdm_tbl('condition_occurrence') %>% filter(condition_type_concept_id %in% 
                                                                                     c(2000000095L, 2000000096L,
                                                                                       2000000097L, 2000000101L,
                                                                                       2000000102L, 2000000103L)) %>%
                                     select(site, person_id, visit_occurrence_id, condition_concept_id, condition_start_date),
                                   'dcon_op_visits_conds',
                                   'visit')
  )



conc_metadata <- 
  list(
    'dcon_pts_ckd-dx_htn-rx' = list('cohort_1' = 'Patients with CKD diagnosis code',
                                    'cohort_2' = 'Patients with anti-hypertensive medication'),
    'dcon_ed_visits_conds' = list('cohort_1' = 'ED visit type',
                                  'cohort_2' = 'ED condition headers'),
    'dcon_ip_visits_conds' = list('cohort_1' = 'Inpatient visit type',
                                  'cohort_2' = 'Inpatient condition headers'),
    'dcon_op_visits_conds' = list('cohort_1' = 'Outpatient visit type',
                                  'cohort_2' = 'Outpatient condition headers'),
    'dcon_asthma_dx_broncho_rx' = list('cohort_1' = 'Patients with asthma diagnosis code',
                                       'cohort_2' = 'Patients with bronchodilator medication'),
    'dcon_leukemia_dx_onco_spec' = list('cohort_1' = 'Patients with a leukemia diagnosis code',
                                        'cohort_2' = 'Patients who have seen an oncology provider'),
    'dcon_nephsyn_dx_neph_spec' = list('cohort_1' = 'Patients with a nephrotic syndrome diagnosis code',
                                       'cohort_2' = 'Patients who have seen a nephrology provider'),
    'dcon_frac_dx_img_px' = list('cohort_1' = 'Patients with a fracture diagnosis code',
                                 'cohort_2' = 'Patients with an imaging procedure code'),
    'dcon_t1d_dx_insulin_rx' = list('cohort_1' = 'Patients with a Type 1 diabetes diagnosis code',
                                    'cohort_2' = 'Patients with an insulin medication')
  )