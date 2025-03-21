


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

neph_spec_prep <- find_specialty(visits = site_cdm_tbl('visit_occurrence'),
                                 specialty_conceptset = load_codeset('nephrology'))

onco_spec_prep <- find_specialty(visits = cdm_tbl('visit_occurrence'),
                                 specialty_conceptset = load_codeset('oncology'))

conc_pts_list <-
  list(
    'pts_with_ckd_dx_and_htn_rx' = list(results_tbl(paste0(config('site'),'_ckddx')) %>% add_site(),
                                        results_tbl(paste0(config('site'),'_htnrx')) %>% add_site(),
                                        'dcon_pts_ckd-dx_htn-rx',
                                        730.5),
    'asthma_dx_broncho_rx' = list(site_cdm_tbl('condition_occurrence') %>% add_site() %>%
                                    select(site, person_id, condition_concept_id, condition_start_date) %>%
                                    inner_join(load_codeset('dx_asthma'), by = c('condition_concept_id' = 'concept_id')) %>% compute_new(),
                                  site_cdm_tbl('drug_exposure') %>% add_site() %>%
                                    select(site, person_id, drug_concept_id, drug_exposure_start_date) %>%
                                    inner_join(load_codeset('rx_albuterol'), by = c('drug_concept_id' = 'concept_id')) %>% compute_new(),
                                  'dcon_asthma_dx_broncho_rx',
                                  90),
    'leukemia_dx_onco_spec' = list(site_cdm_tbl('condition_occurrence') %>% add_site() %>% select(site, person_id, condition_concept_id, condition_start_date) %>%
                                     inner_join(load_codeset('dx_leukemia_lymphoma'), by = c('condition_concept_id' = 'concept_id')) %>% compute_new(),
                                   onco_spec_prep %>% add_site(),
                                   'dcon_leukemia_dx_onco_spec',
                                   90),
    'nephsyn_dx_neph_spec' = list(site_cdm_tbl('condition_occurrence') %>% add_site() %>% select(site, person_id, condition_concept_id, condition_start_date) %>%
                                    inner_join(load_codeset('dx_nephrotic_syndrome'), by = c('condition_concept_id' = 'concept_id')) %>% compute_new(),
                                  neph_spec_prep %>% add_site(),
                                  'dcon_nephsyn_dx_neph_spec',
                                  730.5),
    'frac_dx_img_px' = list(cdm_tbl('condition_occurrence') %>% add_site() %>% select(site, person_id, condition_concept_id, condition_start_date) %>%
                              inner_join(load_codeset('dx_fracture'), by = c('condition_concept_id' = 'concept_id')) %>% compute_new(),
                            cdm_tbl('procedure_occurrence') %>% add_site() %>% select(site, person_id, procedure_concept_id, procedure_date) %>%
                              inner_join(load_codeset('px_radiologic'), by = c('procedure_concept_id' = 'concept_id')) %>% compute_new(),
                            'dcon_frac_dx_img_px',
                            90),
    't1d_dx_insulin_rx' = list(cdm_tbl('condition_occurrence') %>% add_site() %>% select(site, person_id, condition_concept_id, condition_start_date) %>%
                                 inner_join(load_codeset('T1D_SNOMED_codes'), by = c('condition_concept_id' = 'concept_id')) %>% compute_new(),
                               cdm_tbl('drug_exposure') %>% add_site() %>% select(site, person_id, drug_concept_id, drug_exposure_start_date) %>%
                                 inner_join(load_codeset('insulin'), by = c('drug_concept_id' = 'concept_id')) %>% compute_new(),
                               'dcon_t1d_dx_insulin_rx',
                               90),
    'flu_dx_flu_neg_lab' = list(site_cdm_tbl('condition_occurrence') %>% add_site() %>% select(site, person_id, condition_concept_id, condition_start_date) %>%
                              inner_join(load_codeset('dx_influenza'), by = c('condition_concept_id' = 'concept_id')) %>% compute_new(),
                            site_cdm_tbl('measurement_labs') %>% add_site() %>% select(site, person_id, measurement_concept_id, measurement_date,
                                                                        value_as_concept_id) %>%
                              inner_join(load_codeset('lab_influenza'), by = c('measurement_concept_id' = 'concept_id')) %>%
                              filter(value_as_concept_id %in% c(9189L,9190L,45878583L,45884153L)) %>% compute_new(),
                            'dcon_flu_dx_flu_neg_lab',
                            14),
    'flu_dx_flu_pos_lab' = list(site_cdm_tbl('condition_occurrence') %>% add_site() %>% select(site, person_id, condition_concept_id, condition_start_date) %>%
                                  inner_join(load_codeset('dx_influenza'), by = c('condition_concept_id' = 'concept_id')) %>% compute_new(),
                                site_cdm_tbl('measurement_labs') %>% add_site() %>% select(site, person_id, measurement_concept_id, measurement_date,
                                                                            value_as_concept_id) %>%
                                  inner_join(load_codeset('lab_influenza'), by = c('measurement_concept_id' = 'concept_id')) %>%
                                  filter(value_as_concept_id %in% c(9191L,4126681L,45884084L,45878745L,4328749L,45876384L,45881666L)) %>% compute_new(),
                                'dcon_flu_dx_flu_pos_lab',
                                14),
    'rsv_dx_rsv_neg_lab' = list(site_cdm_tbl('condition_occurrence') %>% add_site() %>% select(site, person_id, condition_concept_id, condition_start_date) %>%
                              inner_join(load_codeset('dx_rsv'), by = c('condition_concept_id' = 'concept_id')) %>% compute_new(),
                            site_cdm_tbl('measurement_labs') %>% add_site() %>% select(site, person_id, measurement_concept_id, measurement_date,
                                                                        value_as_concept_id) %>%
                              inner_join(load_codeset('lab_rsv'), by = c('measurement_concept_id' = 'concept_id')) %>%
                              filter(value_as_concept_id %in% c(9189L,9190L,45878583L,45884153L)) %>% compute_new(),
                            'dcon_rsv_dx_rsv_neg_lab',
                            14),
    'rsv_dx_rsv_pos_lab' = list(site_cdm_tbl('condition_occurrence') %>% add_site() %>% select(site, person_id, condition_concept_id, condition_start_date) %>%
                                  inner_join(load_codeset('dx_rsv'), by = c('condition_concept_id' = 'concept_id')) %>% compute_new(),
                                site_cdm_tbl('measurement_labs') %>% add_site() %>% select(site, person_id, measurement_concept_id, measurement_date,
                                                                            value_as_concept_id) %>%
                                  inner_join(load_codeset('lab_rsv'), by = c('measurement_concept_id' = 'concept_id')) %>%
                                  filter(value_as_concept_id %in% c(9191L,4126681L,45884084L,45878745L,4328749L,45876384L,45881666L)) %>% compute_new(),
                                'dcon_rsv_dx_rsv_pos_lab',
                                14)
  )

conc_visits_list <-
  list('ED_visits_ED_conds' = list(site_cdm_tbl('visit_occurrence') %>% filter(visit_concept_id==9203L) %>%
                                     add_site() %>% select(site, person_id, visit_occurrence_id, visit_start_date) %>% compute_new(),
                                   site_cdm_tbl('condition_occurrence') %>% filter(
                                     condition_type_concept_id %in% c(2000001280L,2000001281L,2000001282L,
                                                                      2000001283L,2000001284L,2000001285L)) %>% add_site() %>%
                                     select(site, person_id, visit_occurrence_id, condition_concept_id, condition_start_date) %>% compute_new(), 
                                   'dcon_ed_visits_conds',
                                   'visit'),
       'IP_visits_IP_conds' = list(site_cdm_tbl('visit_occurrence') %>% filter(visit_concept_id %in% c(9201)) %>% add_site() %>%
                                     select(site, person_id, visit_occurrence_id, visit_start_date) %>% compute_new(),
                                   site_cdm_tbl('condition_occurrence') %>% filter(condition_type_concept_id %in%
                                                                                     c(2000000092L,2000000093L,
                                                                                       2000000094L,2000000098L,
                                                                                       2000000099L,2000000100L)) %>% add_site() %>%
                                     select(site, person_id, visit_occurrence_id, condition_concept_id, condition_start_date) %>% compute_new(),
                                   'dcon_ip_visits_conds',
                                   'visit'),
       'OP_visits_op_conds' = list(site_cdm_tbl('visit_occurrence') %>% filter(visit_concept_id %in% c(9202L)) %>% add_site() %>%
                                     select(site, person_id, visit_occurrence_id, visit_start_date) %>% compute_new(),
                                   site_cdm_tbl('condition_occurrence') %>% filter(condition_type_concept_id %in% 
                                                                                     c(2000000095L, 2000000096L,
                                                                                       2000000097L, 2000000101L,
                                                                                       2000000102L, 2000000103L)) %>% add_site() %>%
                                     select(site, person_id, visit_occurrence_id, condition_concept_id, condition_start_date) %>% compute_new(),
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
                                    'cohort_2' = 'Patients with an insulin medication'),
    'dcon_flu_dx_flu_pos_lab' = list('cohort_1' = 'Patients with a flu diagnosis code',
                                     'cohort_2' = 'Patients with a positive flu lab test'),
    'dcon_flu_dx_flu_neg_lab' = list('cohort_1' = 'Patients with a flu diagnosis code',
                                     'cohort_2' = 'Patients with a negative flu lab test'),
    'dcon_rsv_dx_rsv_pos_lab' = list('cohort_1' = 'Patients with a RSV diagnosis code',
                                     'cohort_2' = 'Patients with a positive RSV lab test'),
    'dcon_rsv_dx_rsv_neg_lab' = list('cohort_1' = 'Patients with a RSV diagnosis code',
                                     'cohort_2' = 'Patients with a negative RSV lab test')
    
  )