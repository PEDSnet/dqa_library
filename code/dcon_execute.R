


#' list element definitions for the `dcon_pts` check type
#' 
#' List of lists
#' 
#' Name of list: check description
#' First Element: First Domain
#' Second Element: Second Domain
#' Third Element: Check Name
#' 
#' The order of first and second element must match the description order
#' 

conc_pts_list <- 
  list(
    'pts_with_ckd_dx_and_htn_rx' = list(results_tbl('site_ckddx'),
                                        results_tbl('site_htnrx'),
                                        'dcon_pts_ckd-dx_htn-rx')
  )

conc_visits_list <-
  list('ED_visits_ED_conds' = list(site_cdm_tbl('visit_occurrence') %>% filter(visit_concept_id==9203L),
                                   site_cdm_tbl('condition_occurrence') %>% filter(
                                         condition_type_concept_id %in% c(2000001280L,2000001281L,2000001282L,
                                                                          2000001283L,2000001284L,2000001285L)
                                       ), 'dcon_ed_visits_conds'),
       'IP_visits_IP_conds' = list(site_cdm_tbl('visit_occurrence') %>% filter(visit_concept_id %in% c(9201)),
                                   site_cdm_tbl('condition_occurrence') %>% filter(condition_type_concept_id %in%
                                                                                         c(2000000092L,2000000093L,
                                                                                           2000000094L,2000000098L,
                                                                                           2000000099L,2000000100L)),
                                       'dcon_ip_visits_conds'),
       'OP_visits_op_conds' = list(site_cdm_tbl('visit_occurrence') %>% filter(visit_concept_id %in% c(9202L)),
                                   site_cdm_tbl('condition_occurrence') %>% filter(condition_type_concept_id %in% 
                                                                                     c(2000000095L, 2000000096L,
                                                                                       2000000097L, 2000000101L,
                                                                                       2000000102L, 2000000103L)),
                                   'dcon_op_visits_conds'),
       'Nephrology_dx_specialty' = list()
           )
  
  
  
conc_metadata <- 
  list(
    'dcon_pts_ckd-dx_htn-rx' = list('cohort_1' = 'patients with CKD diagnosis code',
                                        'cohort_2' = 'patients with antihypertensive medication'),
    'dcon_ed_visits_conds' = list('cohort_1' = 'ED visit type',
                                  'cohort_2' = 'ED condition headers'),
    'dcon_ip_visits_conds' = list('cohort_1' = 'Inpatient visit type',
                                  'cohort_2' = 'Inpatient condition headers'),
    'dcon_op_visits_conds' = list('cohort_1' = 'Outpatient visit type',
                                  'cohort_2' = 'Outpatient condition headers')
  )