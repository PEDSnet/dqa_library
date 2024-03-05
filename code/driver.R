# Vector of additional packages to load before executing the request
config_append('extra_packages', c('tidyr','lubridate','stringr', 'dplyr', 'sparklyr'))


#' Execute the request
#'
#' This function presumes the environment has been set up, and executes the
#' steps of the request.
#'
#' In addition to performing queries and analyses, the execution path in this
#' function should include periodic progress messages to the user, and logging
#' of intermediate totals and timing data through [append_sum()].
#'
#' This function is also typically executed automatically, but is separated from
#' the setup done in [.load()] to facilitate direct invocation during
#' development and debugging.
#'
#' @param base_dir The name of the top-level directory for the request.  The default
#'   is `config('base_dir')`, which should always be valid after execution of
#'   [.load()].
#'
#' @return The return value is dependent on the content of the request, but is
#'   typically a structure pointing to some or all of the retrieved data or
#'   analysis results.  The value is not used by the framework itself.
#' @md
.run  <- function(base_dir = config('base_dir')) {

  message('Starting execution with framework version ',
          config('framework_version'))
  
  message('Precompute Tables')
  source(file.path(base_dir, 'code', 'precompute_tables.R'))
  
  prev <- as.character(config('previous_version'))
  curr <- as.character(config('current_version'))
  site_nm <- as.character(config('site'))

  message('DC (Data Cycle) Check')
    source(file.path(base_dir, 'code', 'dc_execute.R'))
    dc_output <- check_dc(prev_v_tbls = dc_args_prev,
                          current_v_tbls = dc_args_current,
                          meta_tbls = dc_args_meta,
                          prev_v = prev,
                          current_v = curr,
                          site_nm = site_nm)
    dc_output_new <- dc_output[1:36]
    dc_output_meta <- dc_output[[37]] %>%
      copy_to(dest = config('db_src'))

    dc_output_df <-
      reduce(.x=dc_output_new,
             .f=dplyr::union)
    
    output_tbl_spark(dc_output_df,
                     'dc_output',
                     append = TRUE)
    
    ## need a copy_to for this or need to rework meta_tbl function*
    output_tbl_spark(dplyr::copy_to(dest = config('db_src'), dc_output_meta),
                     'dc_meta',
                     append = TRUE)

    # output_tbl_append(dc_output_df,
    #                 'dc_output')
    # output_tbl_append(dc_output_meta,
    #                 'dc_meta')

  message('VC (Vocabulary Conformance) and VS (Valueset Conformance) Check')
    source(file.path(base_dir, 'code', 'vc_vs_execute.R'))
    vc <- check_vc(vocabvals=vc_list)
    vs <- check_vs(valuesets=vs_list)
    vc_standard <- create_vc_vs_output(vc, string_tbl_name = 'vc')
    vs_standard <- create_vc_vs_output(vs, string_tbl_name = 'vs')
    vc_vs_joined <- c(vc_standard,
                      vs_standard)
    vc_vs_final <- vc_vs_joined %>% reduce(.f=dplyr::union)
    output_tbl_append(vc_vs_final,
                      'vc_vs_violations')

  message('UC (Unmapped Concepts) Check')
    source(file.path(base_dir, 'code', 'uc_execute.R'))
    uc <- check_uc(concept_list=uc_args_list)
    uc_by_year <- check_uc_by_year(uc_args_list)
    uc_reduce <- reduce(.x=uc,
                        .f=dplyr::union)
    uc_by_year_reduce <- reduce(.x=uc_by_year,
                                .f=dplyr::union)
    output_tbl_append(uc_reduce,
                      'uc_output')
    output_tbl_append(uc_by_year_reduce,
                      'uc_by_year')

  message('MF VisitID (Missing Field: Visit Occurrence ID) Check')
    source(file.path(base_dir, 'code', 'mf_visitid_execute.R'))
    mf_visitid <- check_mf_visitid(mf_visitid_list)
    mf_visitid_reduce <- reduce(.x=mf_visitid,
                                .f=dplyr::union)
    output_tbl_append(mf_visitid_reduce,
                      'mf_visitid_output')

  message('BMC (Best Mapped Concepts) Check')
    source(file.path(base_dir, 'code', 'bmc_gen_execute.R'))
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
    
    site_iptwo <- site_cdm_tbl('visit_occurrence') %>%
      filter(visit_concept_id %in% c(9201L, 2000000048L)) %>%
      mutate(los = datediff(visit_end_date, visit_start_date)) %>%
      filter(los > 2)
    
    pf_long_ip <- check_pf_visits(long_ip_list,
                                  visit_tbl = site_iptwo)
    
    pf_combined <-
      c(pf_allvisits,
        pf_opvisits,
        pf_ipvisits,
        pf_edvisits,
        pf_long_ip)

    pf_combined_reduce <-
      reduce(.x=pf_combined,
             .f=dplyr::union)
    
    output_tbl_spark(dplyr::copy_to(dest = config('db_src'), pf_combined_reduce),
                     'pf_output',
                     append = TRUE)

    # output_tbl_append(pf_combined_reduce,
    #                   'pf_output')

  message('FOT (Facts Over Time) Check')
    source(file.path(base_dir, 'code', 'fot_execute.R'))
    # fot_all <- check_fot(time_tbls = time_tbls_list,
    #                      visits_only = FALSE)
    fot_all <- check_fot_spark(time_tbls = time_tbls_list,
                               visits_only = FALSE)
    fot_all_reduce <- reduce(.x=fot_all,
                             .f=dplyr::union)
    
    output_tbl_spark(dplyr::copy_to(dest = config('db_src'), fot_all_reduce),
                     'fot_output',
                     append = TRUE)
    
    # output_tbl_append(fot_all_reduce,
    #                   'fot_output')
    
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
    
    output_tbl_spark(dplyr::copy_to(dest = config('db_src'), dcon_all),
                     'dcon_output',
                     append = TRUE)
    
    # output_tbl_append(dcon_all,
    #                   'dcon_output')
    
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
