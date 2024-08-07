
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
                                                                                                                                                                                
check_st_conf_vs_conceptid <- function(valuesets,
                                             from_specs=TRUE,
                                             string_tbl_name='st_conf_vs') {
  
  
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
    
    illegal_values <- 
      site_cdm_tbl(names(valuesets[i])) %>%
      anti_join(codeset_round,
                by = join_cols) %>%
      filter(! .data[[concept_id_fn]] %in% c(44814650L,0L)) %>%
      group_by(!!! rlang::syms(concept_id_fn)) %>%
      summarise(total_ct = n(),
                total_pt_ct = n_distinct(person_id)) %>%
      ungroup() %>%
      inner_join(select(
        vocabulary_tbl('concept'),
        concept_id, concept_name, vocabulary_id
      ), by = join_cols) %>% collect_new() %>%
      add_meta(check_lib = string_tbl_name)
    
    check_valueset[[names(valuesets[i])]] <- illegal_values
    
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

check_st_conf_vs_vocab <- function(vocabvals,
                                         string_tbl_name='st_conf_voc') {
  
  vocab_illegals <- list()
  
  for(i in 1:length(vocabvals)) {
  
    input_list <- list()
    values <- c(vocabvals[[i]][[1]])
    
    valid_concepts <- vocabulary_tbl('concept') %>%
      filter(vocabulary_id %in% values) 
    
    input_list[[names(vocabvals[i])]] <- list(valid_concepts,
                                              vocabvals[[i]][[2]])
    
    this_round <- check_st_conf_vs_conceptid(valuesets = input_list,
                                                   from_specs = FALSE,
                                                   string_tbl_name = string_tbl_name)
    
    if(length(this_round) > 0) {
      vocab_illegals[[paste0(names(this_round[1]))]] <- this_round[[1]]
      } else {vocab_illegals[[paste0(names(this_round[1]))]] <- NA}
    
    
    
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

create_vs_output <- function(tbl_list,
                             string_tbl_name='st_conf_voc') {
  
  meta_tbl <- list()
  
  if(length(tbl_list) == 0L) {
    
    final <- 
      tibble(
        table_application = 'none',
        measurement_column = 'none',
        concepts = -999,
        total_pt_ct = 0,
        concept_name = 'No violations',
        vocabulary_id = 'PEDSnet',
        check_name = string_tbl_name,
        database_version = config('current_version')
      )
  }  else {
    
    for(i in 1:length(tbl_list)) {
      
      
      pivot_col <- sym(colnames(tbl_list[[i]][,1]))
      current_tbl <- tbl_list[[i]]
      
      final <-pivot_longer(current_tbl,
                           cols=all_of(pivot_col),
                           names_to='measurement_column',
                           values_to='concepts') %>%
        relocate(c(measurement_column,concepts), .before = total_ct) %>%
        mutate(table_application = names(tbl_list[i])) %>%
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

