

#' list element definitions for `uc` check type
#' 
#' List of lists
#' 
#' list name: check description 
#' list first element: table to look for unmapped concepts
#' list second element: field to look for unmapped concepts
#' list third element: check name
#' list fourth element: source value to look for potential
#' source values that are unmapped
#' 

uc_args_list <- 
  list(
     'all drugs' = list(site_cdm_tbl('drug_exposure'), 'drug_concept_id', 'uc_dr', 'drug_source_value'),
    
     'inpatient administrations' = list(site_cdm_tbl('drug_exposure') %>%
                                        filter(drug_type_concept_id == 38000180L),
                                        'drug_concept_id', 'uc_di', 'drug_source_value'),
    
     'prescription drugs' = list(site_cdm_tbl('drug_exposure') %>%
                                 filter(drug_type_concept_id == 38000177L),
                                 'drug_concept_id', 'uc_dp', 'drug_source_value'),

     'drug dose unit' = list(site_cdm_tbl('drug_exposure'),
                             'dose_unit_concept_id', 'uc_du', 'dose_unit_source_value'),
    
     'drug route' = list(site_cdm_tbl('drug_exposure'),
                             'route_concept_id', 'uc_drt', 'route_source_value'),

     'conditions' = list(site_cdm_tbl('condition_occurrence'),
                         'condition_concept_id', 'uc_co', 'condition_source_value'),
     
     'condition source' = list(site_cdm_tbl('condition_occurrence'),
                               'condition_source_concept_id', 'uc_co_scid', 'condition_source_value'),
    
     'all labs' = list(site_cdm_tbl('measurement_labs'),
                       'measurement_concept_id', 'uc_ml', 'measurement_source_value'),
    
     'lab units' = list(site_cdm_tbl('measurement_labs') %>% filter(value_as_number != 9999,
                                                                    !is.na(value_as_number)),
                             'unit_concept_id', 'uc_mlu', 'unit_source_value'),
    
     'immunizations' = list(site_cdm_tbl('immunization'),
                            'immunization_concept_id', 'uc_im', 'immunization_source_value'),
     
     'immunization dose unit' = list(site_cdm_tbl('immunization'),
                                     'imm_dose_unit_concept_id', 'uc_imu', 'imm_dose_unit_source_value'),

     'immunization route' = list(site_cdm_tbl('immunization'),
                                 'imm_route_concept_id', 'uc_imrt', 'imm_route_source_value'),
     
     'all procedures' = list(site_cdm_tbl('procedure_occurrence'), 
                             'procedure_concept_id','uc_pr', 'procedure_source_value'),
     
     'ordered procedures' = list(site_cdm_tbl('procedure_occurrence') %>% 
                                  filter(procedure_type_concept_id %in% c(2000001494L,38000275L)),
                                 'procedure_concept_id','uc_po', 'procedure_source_value'),
     
     'billed procedure' = list(site_cdm_tbl('procedure_occurrence') %>% 
                                filter(procedure_type_concept_id %in% c(44786630L,44786631)),
                               'procedure_concept_id','uc_pb', 'procedure_source_value'),
     
     'payer plan class' = list(site_cdm_tbl('visit_payer') %>% mutate(payer_class = ifelse(plan_class == 'Other/Unknown', 0, 1)) %>%
                                 inner_join(site_cdm_tbl('visit_occurrence') %>% select(visit_occurrence_id, visit_start_date)),
                               'payer_class', 'uc_vpc', 'plan_name'),
     
     'payer plan type' = list(site_cdm_tbl('visit_payer') %>% mutate(payer_type = ifelse(plan_type == 'Other/Unknown', 0, 1)) %>%
                                inner_join(site_cdm_tbl('visit_occurrence') %>% select(visit_occurrence_id, visit_start_date)),
                              'payer_type', 'uc_vpt', 'plan_name')
  )