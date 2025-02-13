
#' Valueset Conformance
#' 
#' @param valuesets a list that contains the following values:
#'  - name of list element: the check name identifier
#'      1. the name of the relevant valueset that appears in the specs directory
#'      2. the field in which valueset violations should be identified
#'      3. the name of the CDM table where the field is located
#' @param from_specs logical that determines if valueset exists in the specs directory
#' @param string_tbl_name the table name prefixed to the output
#' 
#' @return a dataframe with summary information about each value that does not 
#'         appear in the valueset
#' 
#' @export
#'  
#'                                                              
check_vs <- function(valuesets,
                     from_specs=TRUE,
                     string_tbl_name='vs') {
  
  
  check_valueset <- list()
  
  for(i in 1:length(valuesets)) {
      
   if(from_specs) {
     codeset_round <- load_codeset(valuesets[[i]][[1]], col_types = 'icccc')
    } else {codeset_round <- valuesets[[i]][[1]]}
    
    if(length(valuesets[[i]]) > 1) {
      concept_id_fn <- paste0(valuesets[[i]][[2]])
    } else {concept_id_fn = paste0(names(valuesets[i]), '_concept_id')}
    
    
    join_cols <- set_names('concept_id', paste0(concept_id_fn))
    
    total_rows <- 
      site_cdm_tbl(valuesets[[i]][[3]]) %>%
      summarise(total_denom_ct=n(),
                total_pt_ct=n_distinct(person_id)) %>% collect_new() %>%
      add_meta(check_lib = string_tbl_name)
    
      illegal_values <- 
        site_cdm_tbl(valuesets[[i]][[3]]) %>%
        anti_join(codeset_round,
                  by = join_cols) %>%
        filter(! .data[[concept_id_fn]] %in% c(44814650L,0L,44814653L,44814649L)) %>%
        group_by(!!! rlang::syms(concept_id_fn)) %>%
        summarise(total_viol_ct = n(),
                  total_viol_pt_ct = n_distinct(person_id)) %>%
        ungroup() %>%
        inner_join(select(
          vocabulary_tbl('concept'),
          concept_id, concept_name, vocabulary_id
        ), by = join_cols) %>% collect_new() 
      
      
      if(nrow(illegal_values) > 0){
      
        illegal_final <- illegal_values %>%
          add_meta(check_lib = string_tbl_name) %>%
          mutate(check_name = names(valuesets[i]),
                 table_application = valuesets[[i]][[3]],
                 accepted_value = FALSE) %>%
          left_join(total_rows)
      
      }else{
        
        illegal_final <- total_rows %>%
          add_meta(check_lib = string_tbl_name) %>%
          mutate(check_name = names(valuesets[i]),
                 table_application = valuesets[[i]][[3]],
                 accepted_value = TRUE,
                 measurement_column = -999,
                 total_viol_ct = 0,
                 total_viol_pt_ct = 0,
                 concept_name = 'No violations',
                 vocabulary_id = 'PEDSnet',
                 accepted_value = TRUE) %>%
          relocate(measurement_column) %>%
          rename_with(~valuesets[[i]][[2]], measurement_column) 
        
      }

    check_valueset[[names(valuesets[i])]] <- illegal_final
    
  }
  
  check_valueset
  
  check_valueset[!sapply(check_valueset, function(x) all(is.na(x)))]
  
}




#' Vocabulary Conformance
#' 
#' @param vocabvals a list that contains the following values:
#'  - name of list element: the check name identifier
#'      1. the acceptable vocabulary for the field of interest, as they appear in the OMOP concept table
#'      2. the field in which vocabulary violations should be identified
#'      3. the name of the CDM table where the field is located
#' @param string_tbl_name the table name prefixed to the output
#' 
#' @return a dataframe with summary information about each vocabulary that appears in the field, 
#'         with violations marked in a T/F field
#'         
#'         vocabularies associated with "null" concepts are ignored, so proportions may not add
#'         up to 1 as a result
#' 
#' @export
#'  
#' 
check_vc <- function(vocabvals,
                     string_tbl_name='vc') {
  
  vocab_illegals <- list()
  
  for(i in 1:length(vocabvals)) {
  
    input_list <- list()
    values <- c(vocabvals[[i]][[1]])
      
    concept_id_fn <- vocabvals[[i]][[2]]
      
      
    join_cols <- set_names('concept_id', paste0(concept_id_fn))
      
    total_rows <- 
      site_cdm_tbl(vocabvals[[i]][[3]]) %>%
      summarise(total_denom_ct=n(),
                total_pt_ct=n_distinct(person_id),
                total_concept_ct=n_distinct(!!sym(concept_id_fn))) %>% collect_new() %>%
      add_meta(check_lib = string_tbl_name)
      
    illegal_values <- 
      site_cdm_tbl(vocabvals[[i]][[3]]) %>%
      filter(! .data[[concept_id_fn]] %in% c(44814650L,0L,44814653L,44814649L)) %>%
      inner_join(select(
        vocabulary_tbl('concept'),
        concept_id, concept_name, vocabulary_id
      ), by = join_cols) %>% 
      group_by(vocabulary_id) %>%
      summarise(total_viol_ct = n(),
                total_viol_pt_ct = n_distinct(person_id),
                total_viol_concept_ct = n_distinct(!!sym(concept_id_fn))) %>%
      ungroup() %>%
      collect_new() %>%
      add_meta(check_lib = string_tbl_name) %>%
      mutate(check_name = names(vocabvals[i]),
             table_application = vocabvals[[i]][[3]],
             accepted_value = ifelse(vocabulary_id %in% values, TRUE, FALSE),
             concept_name = paste0('Vocabulary Identifier - ', vocabulary_id),
             temp = 0) %>%
      relocate(temp) %>%
      rename_with(~concept_id_fn, temp) %>%
      left_join(total_rows)
      
      vocab_illegals[[names(vocabvals[i])]] <- illegal_values
      
    }
  
  vocab_illegals
  
  vocab_illegals[!sapply(vocab_illegals, function(x) all(is.na(x)))]

}

#' Additional processing for VS & VC checks
#' 
#' @param tbl_list a list that contains all the vc or vs violations
#' @param string_tbl_name a string that contains the table name for the check output
#' 
#' @return a pivoted version of the input table with dummy rows added for checks
#'         that did not return any violations
#' 
create_vc_vs_output <- function(tbl_list,
                                vs_list,
                                string_tbl_name='vc') {
  
  meta_tbl <- list()
  
  if(length(tbl_list) == 0L) {
    
    for(i in 1:length(vs_list)){
    
    final <- 
      tibble(
        table_application = vs_list[[i]][[3]],
        measurement_column = vs_list[[i]][[2]],
        concepts = -999,
        total_viol_ct = 0,
        total_viol_pt_ct = 0,
        concept_name = 'No violations',
        vocabulary_id = 'PEDSnet',
        check_type = string_tbl_name,
        database_version = config('current_version'),
        site = config('site'),
        check_name = names(vs_list[i]),
        total_denom_ct = 0,
        total_pt_ct = 0,
        accepted_value = TRUE
      )
    
    meta_tbl[[i]] <- final
    
    }
    
  }  else {
    
    for(i in 1:length(tbl_list)) {
      
      
      pivot_col <- sym(colnames(tbl_list[[i]][,1]))
      current_tbl <- tbl_list[[i]]
      
      final <-pivot_longer(current_tbl,
                           cols=all_of(pivot_col),
                           names_to='measurement_column',
                           values_to='concepts') %>%
        relocate(c(measurement_column,concepts), .before = total_viol_ct) %>%
        #mutate(table_application = names(tbl_list[i])) %>%
        relocate(table_application, .before = measurement_column)
      
      meta_tbl[[i]] <- final
    }
    
  }
  
  meta_tbl
}