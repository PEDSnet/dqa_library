
#' vocabulary conformance element definitions
#' 
#' formatted as a named list of lists where each element is named with the check name 
#' identifier and contains the following information:
#'      1. the acceptable vocabulary for the field of interest, as they appear in the OMOP concept table
#'      2. the field in which vocabulary violations should be identified
#'      3. the name of the CDM table where the field is located

vc_list <- 
  list(
    'vc_pr_cid' = list(c('ICD10CM','CPT4','ICD9CM','ICD10','ICD9','ICD10PCS','ICD9Proc','HCPCS', 'SNOMED'),
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
    'vc_dr_cid' = list(c('RxNorm', 'RxNorm Extension', 'NDC'),
                         'drug_concept_id',
                         'drug_exposure'),
    'vc_im_cid' = list(c('CVX'),
                         'immunization_concept_id',
                         'immunization'),
    'vc_dr_dose' = list(c('UCUM'),
                          'dose_unit_concept_id',
                          'drug_exposure'),
    'vc_pr_pscid' = list(c('ICD10CM','CPT4','ICD9CM','ICD10','ICD9','ICD10PCS','ICD9Proc','HCPCS'),
                           'procedure_source_concept_id',
                           'procedure_occurrence'),
    'vc_ml_cid' = list(c('LOINC', 'SNOMED', 'PEDSnet'),
                         'measurement_concept_id',
                         'measurement_labs')
  )


#' valueset conformance element definitions
#' 
#' formatted as a named list of lists where each element is named with the check name 
#' identifier and contains the following information:
#'      1. the name of the relevant valueset that appears in the specs directory
#'      2. the field in which valueset violations should be identified
#'      3. the name of the CDM table where the field is located

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
    'vs_im_route_cid' = list('valueset_imm_route',
                             'imm_route_concept_id',
                             'immunization'),
    'vs_pd_race_cid' = list('valueset_race',
                            'race_concept_id',
                            'person'),
    'vs_pd_eth_cid' = list('valueset_ethnicity',
                           'ethnicity_concept_id',
                           'person')
  )


