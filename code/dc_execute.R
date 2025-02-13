
######################### PREP TABLES #####################################

#' used to pre-compute tables to be used in the arguments for check_dc
#' this is mostly used when more complex filter logic or joins are needed
#' to achieve the table of interest for the count computation
#' 
#' i.e. looking at patients with diagnoses AND labs and how that changes over time
#' 

site_nm <- config('site')

c19_dx_lab_current <- 
  site_cdm_tbl('condition_occurrence') %>%
  inner_join(load_codeset('c19_dx'),
             by=c('condition_concept_id'='concept_id')) %>%
  select(person_id) %>%
  inner_join(site_cdm_tbl('measurement_labs'),
             by='person_id') %>%
  inner_join(load_codeset('c19_viral_labs'),
             by=c('measurement_concept_id'='concept_id')) %>%
  distinct(person_id) %>% compute_new()

# c19_dx_lab_prev <- 
#   site_cdm_tbl_prev('condition_occurrence') %>%
#   inner_join(load_codeset('c19_dx', db = config('db_src_prev)),
#              by=c('condition_concept_id'='concept_id')) %>%
#   select(person_id) %>%
#   inner_join(site_cdm_tbl_prev('measurement_labs') ,
#              by='person_id') %>%
#   inner_join(load_codeset('c19_viral_labs', db = config('db_src_prev')),
#              by=c('measurement_concept_id'='concept_id')) %>%
#   distinct(person_id) %>% compute_new()

########################## TABLE ARGS ######################################        

#' The primary list containing the arguments for the check_dc function
#' Should be structured as a named list, where the name of each element is a brief
#' descriptor of the check. Each element should contain the following:
#'   1. The table where current counts can be computed
#'   2. A string with any filter logic that should be applied to filter down the table to
#'      achieve the check of interest. If no filter logic is needed, set to NA
#'   3. If you wish to retrieve counts from a previous data model instance, the table
#'      where previous counts can be computed. The same filter logic will be applied to this
#'      table as the table with current counts.
#'      
#'      If you are retrieving counts from a previous version of check_dc results, either
#'      exclude this element or set to NA.

dc_args_list <- 
  
  list(
    'person' = list(site_cdm_tbl('person'), NA, NA),
    'drug_exposure' = list(site_cdm_tbl('drug_exposure'), NA, NA),
    'condition_occurrence' = list(site_cdm_tbl('condition_occurrence'), NA, NA),
    'adt_occurrence' = list(site_cdm_tbl('adt_occurrence'), NA, NA),
    'device_exposure' = list(site_cdm_tbl('device_exposure'), NA, NA),
    'immunization' = list(site_cdm_tbl('immunization'), NA, NA),
    'visit_occurrence' = list(site_cdm_tbl('visit_occurrence'), NA, NA),
    'measurement_vitals' = list(site_cdm_tbl('measurement_vitals'), NA, NA),
    'measurement_labs' = list(site_cdm_tbl('measurement_labs'), NA, NA),
    'condition_outpatient' = list(site_cdm_tbl('condition_occurrence'), "condition_type_concept_id %in% c(2000000095, 2000000096,
                                                        2000000097, 2000000101, 2000000102, 2000000103)", NA),
    'condition_inpatient' = list(site_cdm_tbl('condition_occurrence'), "condition_type_concept_id %in% c(2000000092, 2000000093,
                                                       2000000094, 2000000098, 2000000099, 2000000100)", NA),
    'condition_ed' = list(site_cdm_tbl('condition_occurrence'), "condition_type_concept_id %in% c(2000001280, 2000001281,
                                                2000001282, 2000001283, 2000001284, 2000001285)", NA),
    'condition_op_billing' = list(site_cdm_tbl('condition_occurrence'), "condition_type_concept_id %in% c(2000000096,
                                                        2000000097, 2000000102, 2000000103)", NA),
    'condition_op_order' = list(site_cdm_tbl('condition_occurrence'), "condition_type_concept_id %in% c(2000000095, 2000000101)", NA),
    'condition_ip_order' = list(site_cdm_tbl('condition_occurrence'), "condition_type_concept_id %in% c(2000000092, 2000000098)", NA),
    'condition_ip_billing' = list(site_cdm_tbl('condition_occurrence'), "condition_type_concept_id %in% c(2000000099, 2000000100,
                                                        2000000093, 2000000094)", NA),
    'adt_picu' = list(site_cdm_tbl('adt_occurrence'), "service_concept_id == 2000000078", NA),
    'adt_nicu' = list(site_cdm_tbl('adt_occurrence'), "service_concept_id == 2000000080", NA),
    'adt_cicu' = list(site_cdm_tbl('adt_occurrence'), "service_concept_id == 2000000079", NA),
    'visit_op_office' = list(site_cdm_tbl('visit_occurrence'), "visit_concept_id == 9202", NA),
    'visit_op_labs' = list(site_cdm_tbl('visit_occurrence'), "visit_concept_id == 2000000469", NA),
    'visit_op_telehealth' = list(site_cdm_tbl('visit_occurrence'), "visit_concept_id == 581399", NA),
    'visit_op_oa' = list(site_cdm_tbl('visit_occurrence'), "visit_concept_id == 44814711", NA),
    'visit_ip' = list(site_cdm_tbl('visit_occurrence'), "visit_concept_id == 9201", NA),
    'visit_edip' = list(site_cdm_tbl('visit_occurrence'), "visit_concept_id == 2000000048", NA),
    'visit_ov' = list(site_cdm_tbl('visit_occurrence'), "visit_concept_id == 2000000088", NA),
    'drug_rx' = list(site_cdm_tbl('drug_exposure'), "drug_type_concept_id ==  38000177", NA),
    'drug_ip' = list(site_cdm_tbl('drug_exposure'), "drug_type_concept_id == 38000180", NA),
    'procedures_billed' = list(site_cdm_tbl('procedure_occurrence'), "procedure_type_concept_id %in% c(44786630,44786631)", NA),
    'procedures_ordered' = list(site_cdm_tbl('procedure_occurrence'), "procedure_type_concept_id %in% c(2000001494,38000275)", NA),
    'procedures' = list(site_cdm_tbl('procedure_occurrence'), NA, NA),
    'measurement_anthro' = list(site_cdm_tbl('measurement_anthro'), NA, NA),
    'covid19_dx_labs' = list(c19_dx_lab_current, NA, NA),
    'care_site' = list(site_cdm_tbl('care_site'), NA, NA),
    'provider' = list(site_cdm_tbl('provider'), NA, NA),
    'specialty' = list(site_cdm_tbl('specialty'), NA, NA)
  )

############################# METADATA ########################################

#' A secondary list containing metadata information for each element in the
#' primary table argument list.
#' 
#' Should be structured as a named list where each element has the same name as
#' the associated element in the primary list. Each element should contain the 
#' following:
#'   1. A longer description of the check to describe what is being computed
#'   2. A brief ID to more easily identify the check within the table. For example,
#'      "dr" might be used to represent "drug_exposure"

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