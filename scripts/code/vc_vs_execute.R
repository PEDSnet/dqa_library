

#' list element definitions for both the `vs` and `vc` check types
#' 
#' List of lists
#' 
#' list name: table name to look for violations
#' `vc` first element: acceptable vocabularies
#' `vc` second element: field to look for vocabulary violations
#' `vc` third element: check name
#' 
#' `vs` first element: check description
#' `vs` second element: field to look for valueset violations
#' `vs` third element: check name


vc_list <- 
  list(
    'vc_pr_cid' = list(c('ICD10CM','CPT4','ICD9CM','ICD10','ICD9','ICD10PCS','ICD9Proc','HCPCS'),
                                  'procedure_concept_id',
                                  'procedure_occurrence'),
    'vc_co_cscid' = list(c('ICD9','ICD9CM','ICD10','ICD10CM'),
                                  'condition_source_concept_id',
                                  'condition_occurrence'),
    'vc_im_dose' = list(c('UCUM'),
                          'imm_dose_unit_concept_id',
                          'immunization'),
    'vc_dt_cause_cid' = list(c('SNOMED', 'OMOP Extension'),
                   'cause_concept_id',
                   'death'),
    'vc_co_cid' = list(c('SNOMED','OMOP Extension'),
                                  'condition_concept_id',
                                  'condition_occurrence'),
    'vc_dr_cid' = list(c('RxNorm', 'RxNorm Extension'),
                           'drug_concept_id',
                           'drug_exposure'),
    'vc_im_cid' = list(c('CVX'),
                          'immunization_concept_id',
                          'immunization'),
    'vc_dr_dose' = list(c('UCUM'),
                           'dose_unit_concept_id',
                           'drug_exposure')
  )

vs_list <- 
  list(
    'vs_adt_cid' = list('valueset_service',
                            'service_concept_id',
                            'adt_occurrence'),
    'vs_adt_tcid' = list('valueset_adt_event_type',
                            'adt_type_concept_id',
                            'adt_occurrence'),
    'vs_ob_cid' = list('valueset_observation',
                         'observation_concept_id',
                         'observation'),
    'vc_im_route_cid' = list('valueset_imm_route',
                          'imm_route_concept_id',
                          'immunization'),
    'vs_pd_race_cid' = list('valueset_race',
                    'race_concept_id',
                    'person'),
    'vs_pd_eth_cid' = list('valueset_ethnicity',
                    'ethnicity_concept_id',
                    'person')
  )


