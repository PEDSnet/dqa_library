

# c19_codes_dx_prev <- 
#   load_codeset_spark('c19_dx','icccc') %>%
#   copy_to_new(dest=config('db_src_prev'),
#               df=.,
#               name='c19_dx',
#               temporary=TRUE,
#               indexes=list('concept_id'))

# c19_codes_labs_prev <- 
#   load_codeset_spark('c19_viral_labs') %>%
#   copy_to_new(dest=config('db_src_prev'),
#               df=.,
#               temporary=TRUE,
#               indexes=list('concept_id'))
# 
# c19_dx_lab_prev <- 
#   site_cdm_tbl_prev('condition_occurrence') %>%
#   inner_join(c19_codes_dx_prev,
#              by=c('condition_concept_id'='concept_id')) %>%
#   select(person_id) %>%
#   inner_join(site_cdm_tbl_prev('measurement_labs'),
#              by='person_id') %>%
#   inner_join(c19_codes_labs_prev,
#              by=c('measurement_concept_id'='concept_id')) %>%
#   distinct(person_id) %>% compute_new(temporary=TRUE,
#                                       indexes=list('person_id'))

c19_dx_lab_current <- 
  site_cdm_tbl('condition_occurrence') %>%
  inner_join(load_codeset('c19_dx'),
             by=c('condition_concept_id'='concept_id')) %>%
  select(person_id) %>%
  inner_join(site_cdm_tbl('measurement_labs'),
             by='person_id') %>%
  inner_join(load_codeset('c19_viral_labs'),
             by=c('measurement_concept_id'='concept_id')) %>%
  distinct(person_id) # %>% compute_new(temporary=TRUE, indexes=list('person_id'))

#' `dc_args_prev`
#' list element definition for the `dc` check: PREVIOUS cycle
#' 
#' List of lists
#' 
#' List name: Check Domain (must match `dc_args_current`)
#' List First Element: Table from Previous Cycle
#' 
#' `dc_args_current`
#' list element definition for the `dc` check: CURRENT cycle
#' 
#' List of lists 
#' 
#' List name: Check Domain (must match `dc_args_prev`)
#' List First Element: Table from Current Cycle
#' 
#' `dc_args_meta`
#' 
#' List name: Check Domain (must match `dc_args_prev` and `dc_args_current`)
#' First Element: Check Description
#' Second Element: Check Name
#' 
 
  

# dc_args_prev <- 
#   list(
#     'person' = site_cdm_tbl_prev('person'),
#     'drug_exposure' = site_cdm_tbl_prev('drug_exposure'),
#     'condition_occurrence' = site_cdm_tbl_prev('condition_occurrence'),
#     'adt_occurrence' = site_cdm_tbl_prev('adt_occurrence'),
#     'device_exposure' = site_cdm_tbl_prev('device_exposure'),
#     'immunization' = site_cdm_tbl_prev('immunization'),
#     'visit_occurrence' = site_cdm_tbl_prev('visit_occurrence'),
#     'measurement_vitals' = site_cdm_tbl_prev('measurement_vitals'),
#     'measurement_labs' = site_cdm_tbl_prev('measurement_labs'),
#     'condition_outpatient' = site_cdm_tbl_prev('condition_occurrence') %>% filter(condition_type_concept_id %in%
#                                                                                     c(2000000095L,
#                                                                                       2000000096L,
#                                                                                       2000000097L,
#                                                                                       2000000101L,
#                                                                                       2000000102L,
#                                                                                       2000000103L)),
#     'condition_inpatient' = site_cdm_tbl_prev('condition_occurrence') %>% filter(condition_type_concept_id %in%
#                                                                                    c(2000000092L,
#                                                                                      2000000093L,
#                                                                                      2000000094L,
#                                                                                      2000000098L,
#                                                                                      2000000099L,
#                                                                                      2000000100L)),
#     'condition_ed' = site_cdm_tbl_prev('condition_occurrence') %>% filter(condition_type_concept_id %in%
#                                                                             c(2000001280L,
#                                                                               2000001281L,
#                                                                               2000001282L,
#                                                                               2000001283L,
#                                                                               2000001284L,
#                                                                               2000001285L)),
#     'condition_op_billing' = site_cdm_tbl_prev('condition_occurrence') %>% filter(condition_type_concept_id %in%
#                                                                                     c(2000000096L,
#                                                                                       2000000097L,
#                                                                                       2000000102L,
#                                                                                       2000000103L)),
#     'condition_op_order' = site_cdm_tbl_prev('condition_occurrence') %>% filter(condition_type_concept_id %in%
#                                                                                   c(2000000095L, 2000000101)),
#     'condition_ip_order' = site_cdm_tbl_prev('condition_occurrence') %>% filter(condition_type_concept_id %in%
#                                                                                   c(2000000092L, 2000000098L)),
#     'condition_ip_billing' = site_cdm_tbl_prev('condition_occurrence') %>% filter(condition_type_concept_id %in%
#                                                                                     c(2000000099L,
#                                                                                       2000000100L,
#                                                                                       2000000093L,
#                                                                                       2000000094L)),
#     'adt_picu' = site_cdm_tbl_prev('adt_occurrence') %>% filter(service_concept_id == 2000000078L),
#     'adt_nicu' = site_cdm_tbl_prev('adt_occurrence') %>% filter(service_concept_id == 2000000080L),
#     'adt_cicu' = site_cdm_tbl_prev('adt_occurrence') %>% filter(service_concept_id == 2000000079L),
#     'visit_op_office' = site_cdm_tbl_prev('visit_occurrence') %>% filter(visit_concept_id == 9202L),
#     'visit_op_labs' = site_cdm_tbl_prev('visit_occurrence') %>% filter(visit_concept_id == 2000000469L),
#     'visit_op_telehealth' = site_cdm_tbl_prev('visit_occurrence') %>% filter(visit_concept_id == 581399L),
#     'visit_op_oa' = site_cdm_tbl_prev('visit_occurrence') %>% filter(visit_concept_id == 44814711L),
#     'visit_ip' = site_cdm_tbl_prev('visit_occurrence') %>% filter(visit_concept_id == 9201L),
#     'visit_edip' = site_cdm_tbl_prev('visit_occurrence') %>% filter(visit_concept_id == 2000000048L),
#     'visit_ov' = site_cdm_tbl_prev('visit_occurrence') %>% filter(visit_concept_id == 2000000088L),
#     'drug_rx' = site_cdm_tbl_prev('drug_exposure') %>% filter(drug_type_concept_id ==  38000177L),
#     'drug_ip' = site_cdm_tbl_prev('drug_exposure') %>% filter(drug_type_concept_id == 38000180L),
#     'procedures_billed' = site_cdm_tbl_prev('procedure_occurrence') %>% filter(procedure_type_concept_id %in%
#                                                                                  c(44786630L,44786631L)),
#     'procedures_ordered' = site_cdm_tbl_prev('procedure_occurrence') %>% filter(procedure_type_concept_id %in%
#                                                                                   c(2000001494L,38000275L)),
#     'procedures' = site_cdm_tbl_prev('procedure_occurrence'),
#     'measurement_anthro' = site_cdm_tbl_prev('measurement_anthro'),
#     'covid19_dx_labs' = c19_dx_lab_prev,
#     'care_site' = site_cdm_tbl_prev('care_site'),
#     'provider' = site_cdm_tbl_prev('provider'),
#     'specialty' = site_cdm_tbl_prev('specialty')
#   )

################################################################        

dc_args_current <- 
  list(
    'person' = site_cdm_tbl('person'),
    'drug_exposure' = site_cdm_tbl('drug_exposure'),
    'condition_occurrence' = site_cdm_tbl('condition_occurrence'),
    'adt_occurrence' = site_cdm_tbl('adt_occurrence'),
    'device_exposure' = site_cdm_tbl('device_exposure'),
    'immunization' = site_cdm_tbl('immunization'),
    'visit_occurrence' = site_cdm_tbl('visit_occurrence'),
    'measurement_vitals' = site_cdm_tbl('measurement_vitals'),
    'measurement_labs' = site_cdm_tbl('measurement_labs'),
    'condition_outpatient' = site_cdm_tbl('condition_occurrence') %>% filter(condition_type_concept_id %in%
                                                                                    c(2000000095L,
                                                                                      2000000096L,
                                                                                      2000000097L,
                                                                                      2000000101L,
                                                                                      2000000102L,
                                                                                      2000000103L)),
    'condition_inpatient' = site_cdm_tbl('condition_occurrence') %>% filter(condition_type_concept_id %in%
                                                                                   c(2000000092L,
                                                                                     2000000093L,
                                                                                     2000000094L,
                                                                                     2000000098L,
                                                                                     2000000099L,
                                                                                     2000000100L)),
    'condition_ed' = site_cdm_tbl('condition_occurrence') %>% filter(condition_type_concept_id %in%
                                                                            c(2000001280L,
                                                                              2000001281L,
                                                                              2000001282L,
                                                                              2000001283L,
                                                                              2000001284L,
                                                                              2000001285L)),
    'condition_op_billing' = site_cdm_tbl('condition_occurrence') %>% filter(condition_type_concept_id %in%
                                                                                    c(2000000096L,
                                                                                      2000000097L,
                                                                                      2000000102L,
                                                                                      2000000103L)),
    'condition_op_order' = site_cdm_tbl('condition_occurrence') %>% filter(condition_type_concept_id %in%
                                                                                  c(2000000095L, 2000000101)),
    'condition_ip_order' = site_cdm_tbl('condition_occurrence') %>% filter(condition_type_concept_id %in%
                                                                                  c(2000000092L, 2000000098L)),
    'condition_ip_billing' = site_cdm_tbl('condition_occurrence') %>% filter(condition_type_concept_id %in%
                                                                                    c(2000000099L,
                                                                                      2000000100L,
                                                                                      2000000093L,
                                                                                      2000000094L)),
    'adt_picu' = site_cdm_tbl('adt_occurrence') %>% filter(service_concept_id == 2000000078L),
    'adt_nicu' = site_cdm_tbl('adt_occurrence') %>% filter(service_concept_id == 2000000080L),
    'adt_cicu' = site_cdm_tbl('adt_occurrence') %>% filter(service_concept_id == 2000000079L),
    'visit_op_office' = site_cdm_tbl('visit_occurrence') %>% filter(visit_concept_id == 9202L),
    'visit_op_labs' = site_cdm_tbl('visit_occurrence') %>% filter(visit_concept_id == 2000000469L),
    'visit_op_telehealth' = site_cdm_tbl('visit_occurrence') %>% filter(visit_concept_id == 581399L),
    'visit_op_oa' = site_cdm_tbl('visit_occurrence') %>% filter(visit_concept_id == 44814711L),
    'visit_ip' = site_cdm_tbl('visit_occurrence') %>% filter(visit_concept_id == 9201L),
    'visit_edip' = site_cdm_tbl('visit_occurrence') %>% filter(visit_concept_id == 2000000048L),
    'visit_ov' = site_cdm_tbl('visit_occurrence') %>% filter(visit_concept_id == 2000000088L),
    'drug_rx' = site_cdm_tbl('drug_exposure') %>% filter(drug_type_concept_id ==  38000177L),
    'drug_ip' = site_cdm_tbl('drug_exposure') %>% filter(drug_type_concept_id == 38000180L),
    'procedures_billed' = site_cdm_tbl('procedure_occurrence') %>% filter(procedure_type_concept_id %in%
                                                                                 c(44786630L,44786631L)),
    'procedures_ordered' = site_cdm_tbl('procedure_occurrence') %>% filter(procedure_type_concept_id %in%
                                                                                  c(2000001494L,38000275L)),
    'procedures' = site_cdm_tbl('procedure_occurrence'),
    'measurement_anthro' = site_cdm_tbl('measurement_anthro'),
    'covid19_dx_labs' = c19_dx_lab_current,
    'care_site' = site_cdm_tbl('care_site'),
    'provider' = site_cdm_tbl('provider'),
    'specialty' = site_cdm_tbl('specialty')
  )

################################################################

dc_args_meta <- 
  list(
    'person' = list('full person table', 'pd'),
    'drug_exposure' = list('full drug exposure table','dr'),
    'condition_occurrence' = list('full condition occurrence table', 'co'),
    'adt_occurrence' = list('full adt occurrence table', 'adt'),
    'device_exposure' = list('full device exposure', 'de'),
    'immunization' = list('full immunizations','im'),
    'visit_occurrence' = list('full visit table', 'vi'),
    'measurement_vitals' = list('full vitals', 'mv'),
    'measurement_labs' = list('full labs', 'ml'),
    'condition_outpatient' = list('outpatient condition headers', 'cop'),
    'condition_inpatient' = list('inpatient condition headers', 'ci'),
    'condition_ed' = list('ed condition headers', 'ced'),
    'condition_op_billing' = list('condition op billing diagnoses', 'cop_cb'),
    'condition_op_order' = list('condition op order diagnoses', 'cop_cc'),
    'condition_ip_order' = list('condition ip order diagnoses', 'ci_cc'),
    'condition_ip_billing' = list('condition ip billing diagnoses', 'ci_cb'),
    'adt_picu' = list('picu from adt table', 'adt_picu'),
    'adt_nicu' = list('nicu from adt table', 'adt_nicu'),
    'adt_cicu' = list('cicu from adt table', 'adt_cicu'),
    'visit_op_office' = list('outpatient office visits', 'vo_office'),
    'visit_op_labs' = list('outpatient lab visits', 'vo_labs'),
    'visit_op_telehealth' = list('outpatient telehealth visits', 'vo_th'),
    'visit_op_oa' = list('outpatient other ambulatory visits', 'vo_oa'),
    'visit_ip' = list('inpatient 9201 visits', 'vip'),
    'visit_edip' = list('inpatient or ed to inpatient', 'vip_ipcombined'),
    'visit_ov' = list('observation visit types', 'vob'),
    'drug_rx' = list('drug prescriptions', 'dp'),
    'drug_ip' = list('inpatient drug administrations', 'di'),
    'procedures' = list('all procedures', 'pr'),
    'procedures_billed' = list('billed procedures', 'pb'),
    'procedures_ordered' = list('ordered procedures', 'po'),
    'measurement_anthro' = list('measured anthropometrics', 'ma'),
    'covid19_dx_labs' = list('c19 diagnosed patients with labs','co_ml_covid'),
    'care_site' = list('full care site table', 'cs'),
    'provider' = list('full provider table', 'pv'),
    'specialty' = list('full specialty table', 'sp')
    
  )


