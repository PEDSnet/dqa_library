

drug_tbl_list <- list(
  
  'admin' = list(site_cdm_tbl('drug_exposure') %>% 
                   filter(drug_type_concept_id %in% c(38000180L)),
                 'inpatient admin',
                 'bmc_rxnorm_di'),
  'rx' =  list(site_cdm_tbl('drug_exposure') %>% 
                 filter(drug_type_concept_id %in% c(38000177L)),
               'prescriptions',
               'bmc_rxnorm_dp')
)