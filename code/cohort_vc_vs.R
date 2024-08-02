
#' Function to check for value set conformance to a defined set
#' @param valuesets a list that contains the following values:
#'  - name of list element: name of cdm_tbl as a string
#'  - first element: name of file from either the specs directory or the actual codeset;
#'  if from specifications, must follow the format `icccc`
#'  - second element: the field in the table name; *** if the field name is the
#'  name of the table_concept_id, then the second element does not need to exist.
#' @param from_specs logical that determines if valueset exists in 
#' @param string_tbl_name the table name prefixed to the output
#' @return tbl with violations to concept_id check, with all cols in original table + columns in codeset
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
    
    #codeset_round <- load_codeset(valuesets[[i]][[1]], col_types = 'icccc')
    
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




#' Function to search for all valid concept_ids in a certain vocabulary
#' @param vocabvals a list with the following elements
#'   - name of element: table name
#'   - first element: a vector containing strings of acceptable voaabularies
#'   - second element: the name of the field to check
#' @param string_tbl_name the string table name; defaults to st_conf_voc
#'   

check_vc <- function(vocabvals,
                     string_tbl_name='vc') {
  
  vocab_illegals <- list()
  
  for(i in 1:length(vocabvals)) {
  
    input_list <- list()
    values <- c(vocabvals[[i]][[1]])
    
    # valid_concepts <- vocabulary_tbl('concept') %>%
    #   filter(vocabulary_id %in% values) 
    # 
    # input_list[[names(vocabvals[i])]] <- list(values,
    #                                           vocabvals[[i]][[2]],
    #                                           vocabvals[[i]][[3]])
    
    # this_round <- check_vs(valuesets = input_list,
    #                        from_specs = FALSE,
    #                        string_tbl_name = string_tbl_name)
      
    # codeset_round <- load_codeset(valuesets[[i]][[1]], col_types = 'icccc')
      
    concept_id_fn <- vocabvals[[i]][[2]]
      
      
    join_cols <- set_names('concept_id', paste0(concept_id_fn))
      
    total_rows <- 
      site_cdm_tbl(vocabvals[[i]][[3]]) %>%
      summarise(total_denom_ct=n(),
                total_pt_ct=n_distinct(person_id)) %>% collect_new() %>%
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
                total_viol_pt_ct = n_distinct(person_id)) %>%
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
      
      # if(length(this_round) > 0) {
      #   vocab_illegals[[paste0(names(this_round[1]))]] <- this_round[[1]]
      # } else {vocab_illegals[[paste0(names(this_round[1]))]] <- NA}
      
    }
  
  vocab_illegals
  
  vocab_illegals[!sapply(vocab_illegals, function(x) all(is.na(x)))]

}

#' combined vs checks into one table output
#' 
#' @param tbl_list a list that contains all the vs violations
#' @param string_tbl_name a string that contains the table name for the check output
#' @return a table with concepts and their counts that violate 
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


#' metadata for the `st_conf_voc` check 
#' 
#' @param valuesets the list used in `check_st_conf_valueset_conceptid`
#' @param valuesets_tbl_name the name of the table output in `create_vs_output`
#' 
#' 

compute_metadata_conf_vs <- function(valuesets,
                                     valuesets_output_tbl_name,
                                     check_type,
                                     db_version_arg = config('current_version'),
                                     check_string='st_conf_vs') {
  
  
  metavs <- tibble(
    table_name = character(),
    table_application = character(),
    field_name = character(),
    db_version = character(),
    check_name = character(),
    type = character()
  )
  
  for(i in 1:length(valuesets)) {
    
    desc <- valuesets[[i]][[1]]
    
    if(length(valuesets[[i]]) > 1) {
      concept_id_fn <- paste0(valuesets[[i]][[2]])
    } else {concept_id_fn = paste0(names(valuesets[i]), '_concept_id')}
    
    desc_name <- valuesets_output_tbl_name
    
    final_thisround <- 
      metavs %>%
      add_row(
        table_name = desc_name,
        table_application = names(valuesets[i]),
        field_name = concept_id_fn,
        db_version = config('current_version'),
        check_name = check_string,
        type = check_type
      )
    
    metavs <- final_thisround
  }
  
  metavs
  
}