

#' missing Field: visit_occurrence_id element definitions
#' 
#' formatted as a named list of lists where each element is named with the check description 
#' and contains the following information:
#'      1. the table in which missing visit ids should be identified
#'      2. the check name identifier
#'      
#' 

mf_visitid_list <- 
  list(
    'conditions excluding problem list' = list(site_cdm_tbl('condition_occurrence') %>% 
                                               filter(! condition_type_concept_id %in% c(2000000089L,
                                                        2000000090L,
                                                        2000000091L)),
                                               'mf_visitid_coied'),
    'all drugs' = list(site_cdm_tbl('drug_exposure'),
                       'mf_visitid_dr'),
    'prescription or inpatient drugs' = list(site_cdm_tbl('drug_exposure') %>% filter(! drug_type_concept_id == 38000175L),
                                             'mf_visitid_dp'),
    'all procedures' = list(site_cdm_tbl('procedure_occurrence'),
                            'mf_visitid_pr'),
    'all labs' = list(site_cdm_tbl('measurement_labs'),
                      'mf_visitid_ml'),
    'all immunizations' = list(site_cdm_tbl('immunization'),
                               'mf_visitid_im')
  )