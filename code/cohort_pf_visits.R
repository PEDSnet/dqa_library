#' Function to find visit_occurrences without facts in another set of tables
#' Function is utilized for the following checks: ST-COMP-Facts and ST-COMP-PatientRecords
#'
#' @param fact_tbls list of tbls in which to look at visit_occurrences
#'
#' @return tbl with visit_occurrences, and all columns in the original visit_occurrence table,
#'        for which there are no facts in the `fact_tbls`
#' example function call: check_st_comp_facts(list('condition_occurrence',
#'                                                 'procedure_occurrence', ...)

check_st_comp_facts_records <- function(fact_tbls) {

  all_tbls <- NA
  for (i in 1:length(fact_tbls)) {

    tbl_comp <- cdm_tbl(fact_tbls[[i]]) %>% select(visit_occurrence_id) %>% distinct()

    if(all(is.na(all_tbls))) all_tbls <- tbl_comp else all_tbls <- dplyr::union(all_tbls, tbl_comp) %>%
        compute_new(indexes = list('visit_occurrence_id'),
                    temporary = TRUE)
  }


  cdm_tbl('visit_occurrence') %>%
    anti_join(all_tbls, by = 'visit_occurrence_id')


}

#' Function to find visit_occurrences without facts in another set of tables
#' @param fact_tbls list of tbls in which to look at visit_occurrences
#' @return tbl with visit_occurrences, and all columns in the original visit_occurrence table,
#'        for which there are no facts in the `fact_tbls`
#' example function call: check_st_comp_facts(list('condition_occurrence',
#'                                                 'procedure_occurrence',
#'                                                 ...))
check_pf_visits <- function(fact_tbls,
                            visit_tbl=site_cdm_tbl('visit_occurrence'),
                            check_string='pf') {

  visit_tbl_all <-
    visit_tbl %>%
    summarise(
      total_visits = n(),
      total_pts = n_distinct(person_id)
    ) %>% ungroup() %>% collect()


  all_tbls <- list()

  for (i in 1:length(fact_tbls)) {

    tbl_comp <- fact_tbls[[i]][[1]]

    check_description_name <- names(fact_tbls[i])
    
    chk_nm <- fact_tbls[[i]][[2]]

    message(paste0('Starting ',check_description_name))

    visit_tbl_all_name <-
      visit_tbl_all %>%
      mutate(check_description = check_description_name) 

    missed_visits <-
      visit_tbl %>% select(person_id,
                           visit_occurrence_id) %>%
      anti_join(tbl_comp,
                by='visit_occurrence_id') %>%
      mutate(check_description = check_description_name) %>%
      group_by(
        check_description
      ) %>%
      summarise(
        no_fact_visits = n(),
        no_fact_pts = n_distinct(person_id)
      ) %>% ungroup() %>% collect()

    missed_pts <-
      visit_tbl %>% select(person_id) %>%
      anti_join(tbl_comp,
                by='person_id') %>%
      mutate(check_description = check_description_name) %>%
      group_by(
        check_description
      ) %>%
      summarise(
        no_fact_pts = n_distinct(person_id)
      ) %>% ungroup() %>% collect()

    # total_ct <-
    #  missed %>%
    #  mutate(check_description = check_description_name) %>%
    #  group_by(
    #    check_description
    #   ) %>%
    #  summarise(
    #    no_fact_visits = n(),
    #    no_fact_pts = n_distinct(person_id)
    #  ) %>% ungroup() %>% collect()

    cts_combined <-
      visit_tbl_all_name %>%
      left_join(missed_visits) %>%
      left_join(missed_pts) %>%
      mutate(
        no_fact_visits_prop = round(
          no_fact_visits / total_visits, 2
        ),
        no_fact_pts_prop = round(
          no_fact_pts / total_pts, 2
        )
      )  %>%
      add_meta(check_lib=check_string) %>%
      mutate_if(., is.numeric, ~replace(., is.na(.), 0)) %>%
      mutate(
        fact_visits = total_visits - no_fact_visits,
        fact_pts = total_pts - no_fact_pts,
        fact_visits_prop = round(1.00 - no_fact_visits_prop, 2),
        fact_pts_prop = round(1.00 - no_fact_pts_prop, 2)
      ) %>% mutate(check_name=chk_nm) #%>% collect()

    # if(all(is.na(all_tbls))) all_tbls <- tbl_comp else all_tbls <- dplyr::union(all_tbls, tbl_comp) %>%
    #  compute_new(indexes = list('visit_occurrence_id'),
    #  temporary = TRUE)

    all_tbls[[names(fact_tbls[i])]] <- cts_combined
  }


  all_tbls

}


