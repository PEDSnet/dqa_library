
.run  <- function() {

  ###'`Precompute Tables` ###
  #' This step will precompute relevant tables for the rest of the data quality
  #' analysis.
  cli::cli_inform(cli::col_br_green('Precompute Tables'))
  
  source(file.path('code', 'precompute_tables.R'))


  ###' `Data Cycle Changes` ###
  #' This check intakes the list defined in `dc_execute.R` to check how counts
  #' have changed in CDM tables between the current and previous cycles.
  #' 
  #' If this is your first time executing this analysis or you have added a new
  #' check type since the last execution, we recommend changing `prev_ct_src` to 
  #' `cdm` to pull previous counts directly from a CDM instance.
  #' 
  cli::cli_inform(cli::col_br_green('Data Cycle Changes'))
  
  source(file.path('code', 'dc_execute.R'))
  
  dc_output <- check_dc(dc_tbl_list = dc_args_list,
                        dc_meta_list = dc_args_meta,
                        prev_ct_src = 'result', ## if first run or new check was added, change to cdm
                        prev_rslt_tbl = 'dc_output',
                        prev_rslt_db = config('db_src'))

  output_tbl_append(dc_output[1],'dc_output')
  output_tbl_append(dc_output[2],'dc_meta')

  ###' `Vocabulary and Valueset Conformance` ###
  cli::cli_inform(cli::col_br_green('Vocabulary and Valueset Conformance'))
  
  source(file.path('code', 'vc_vs_execute.R'))
  
  vc <- check_vc(vocabvals=vc_list)
  vc_standard <- create_vc_vs_output(vc, vs_list, string_tbl_name = 'vc')
  
  vc_reduce <- vc_standard %>% reduce(.f = dplyr::union)
  output_tbl_append(vc_reduce, 'vc_output')
    
  vs <- check_vs(valuesets=vs_list)
  vs_standard <- create_vc_vs_output(vs, vs_list, string_tbl_name = 'vs')

  vs_reduce <- vs_standard %>% reduce(.f = dplyr::union)
  output_tbl_append(vs_reduce, 'vs_output')

  ###' `Unmapped Concepts` ###
  cli::cli_inform(cli::col_br_green('Unmapped Concepts'))
  
  source(file.path('code', 'uc_execute.R'))
  
  uc <- check_uc(concept_list=uc_args_list)
  uc_reduce <- reduce(.x=uc,
                      .f=dplyr::union)
  output_tbl_append(uc_reduce,
                    'uc_output')
  
  uc_by_year <- check_uc_by_year(uc_args_list)
  uc_by_year_reduce <- reduce(.x=uc_by_year,
                              .f=dplyr::union)
  output_tbl_append(uc_by_year_reduce,
                    'uc_by_year')

  ###' `Missing Field: visit_occurrence_id` ###
  cli::cli_inform(cli::col_br_green('Missing Field: visit_occurrence_id'))
  
  source(file.path('code', 'mf_visitid_execute.R'))
  
  mf_visitid <- check_mf_visitid(mf_visitid_list)
  mf_visitid_reduce <- reduce(.x=mf_visitid,
                              .f=dplyr::union)
  output_tbl_append(mf_visitid_reduce,
                    'mf_visitid_output')

  ###' `Best Mapped Concepts` ###
  cli::cli_inform(cli::col_br_green('Best Mapped Concepts'))
  
  source(file.path('code', 'bmc_gen_execute.R'))
  
  bmc_gen <- check_bmc_gen(fact_tbl_list)
  bmc_gen_reduce <- reduce(.x=bmc_gen,
                              .f=dplyr::union)
  output_tbl_append(bmc_gen_reduce,
                    'bmc_gen_output')

  
  
  
  
  message('Expected Concepts Present')
    source(file.path(base_dir, 'code', 'ecp_execute.R'))
    ecp <- check_ecp(ecp_codeset_list)

    output_tbl_append(ecp,
                      'ecp_output')

  message('PF Visits (Patient Facts for Visits) Check')
    source(file.path(base_dir, 'code', 'pf_visits_execute.R'))
    pf_allvisits <- check_pf_visits(all_list)
    pf_opvisits <- check_pf_visits(op_list,
                                   visit_tbl = site_cdm_tbl('visit_occurrence') %>%
                                     filter(visit_concept_id %in% c(9202L,581399L)))
    pf_ipvisits <- check_pf_visits(ip_list,
                                   visit_tbl = site_cdm_tbl('visit_occurrence') %>%
                                     filter(visit_concept_id %in% c(9201L,2000000048L)))
    pf_edvisits <- check_pf_visits(ed_list,
                                   visit_tbl = site_cdm_tbl('visit_occurrence') %>%
                                    filter(visit_concept_id %in% c(9203L,2000000048L)))

    pf_long_ip <- check_pf_visits(long_ip_list,
                                  visit_tbl = results_tbl(paste0(config('site'), '_iptwo')))

    pf_combined <-
      c(pf_allvisits,
        pf_opvisits,
        pf_ipvisits,
        pf_edvisits,
        pf_long_ip)

    pf_combined_reduce <-
      reduce(.x=pf_combined,
             .f=dplyr::union)

    output_tbl_append(pf_combined_reduce,
                      'pf_output')

  message('FOT (Facts Over Time) Check')
    source(file.path(base_dir, 'code', 'fot_execute.R'))
    fot_all <- check_fot(time_tbls = time_tbls_list,
                         visits_only = FALSE)
    fot_all_reduce <- reduce(.x=fot_all,
                             .f=dplyr::union)

    fot_visit_denom <- fot_all_reduce %>% filter(check_name == 'fot_vi') %>%
      select(site, month_end, row_cts, row_visits, row_pts) %>%
      rename('total_pt' = row_pts,
             'total_visit' = row_visits,
             'total_row' = row_cts)

    fot_w_denom <- fot_all_reduce %>% left_join(fot_visit_denom)

    output_tbl_append(fot_w_denom,
                      'fot_output')

  message('Domain Concordance Check')
    source(file.path(base_dir, 'code', 'dcon_execute.R'))
    # dcon_all <- check_dcon_pts(conc_tbls = conc_pts_list)
    # dcon_all_reduce <- reduce(.x=dcon_all,
    #                           .f=dplyr::union)
    dcon_pt <- check_dcon(conc_tbls = conc_pts_list,
                          check_string = 'dcon_pts') %>%
      reduce(dplyr::union)
    dcon_visit <- check_dcon(conc_tbls = conc_visits_list,
                             check_string = 'dcon_visits') %>%
      reduce(dplyr::union)
    dcon_all <- dplyr::union(dcon_pt, dcon_visit)

    output_tbl_append(dcon_all,
                      'dcon_output')

  # message('Domain Concordance Over Time')
  #   dcon_pt_yr <- check_dcon_overtime(conc_tbls = conc_pts_list,
  #                                       check_string = 'dcon_pts') %>%
  #     reduce(dplyr::union)
  #   dcon_visit_yr <- check_dcon_overtime(conc_tbls = conc_visits_list,
  #                                          check_string = 'dcon_visits') %>%
  #     reduce(dplyr::union)
  #   dcon_all_yr <- dplyr::union(dcon_pt_yr, dcon_visit_yr)
  #
  #   output_tbl_append(dcon_all_yr,
  #                     'dcon_by_yr')


    dcon_meta <- check_dcon_pts_meta(conc_tbls_meta = conc_metadata)
    output_tbl_append(dcon_meta,
                      'dcon_meta')

    message('Remove Precomputed Tables')
    remove_precompute()


    message('Uploading Crosswalks')
    pf_mappings <- read_codeset('pf_mappings','cc')
    output_tbl(pf_mappings,
               'pf_mappings')

    dc_mappings <- read_codeset('dc_mappings','cc')
    output_tbl(dc_mappings,
               'dc_mappings')

    check_meta <- read.csv('specs/dqa_check_metadata.csv')
    output_tbl(check_meta,
               'all_check_metadata')




}
