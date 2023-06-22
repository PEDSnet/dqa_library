

#' @md
#' takes cohort of drug exposures, 
#' joins to vocabulary table, 
#' creates xwalk between drug and level 
#' 
#' @param drug_tbl defaults to `cdm_tbl('drug_exposure')`
#' @param concept_tbl defaults to `vocabulary_tbl('concept')`
#' 
#' @return the original drug_tbl with an extra 
#' column called `rxnorm_level`
#' 

find_rxnorm_level <- function(drug_tbl = cdm_tbl('drug_exposure'),
                              concept_tbl=vocabulary_tbl('concept')) {
  
  drug_tbl %>%
    inner_join(
      select(concept_tbl,
             concept_id,
             concept_class_id),
      by=c('drug_concept_id'='concept_id')
    ) %>% rename(rxnorm_level=concept_class_id)
  
}



#' compute numbers proportion of rows and patients at
#' each level of rxnorm drug level
#' 
#' @param drug_tbl_list_args A list of lists where list name is a description of
#' what the table contains; 
#'  1. The first element of the nested list is the table that will be computed; 
#'  2. The second element of the nested list is a description of the table
#' @param check_string string that contains a description of the table
#' 

compute_st_conf_specificity_rxnorm <- function(drug_tbl_list_args,
                                               check_string='st_conf_rxnorm') {
  
  drugs <- list()
  
  for(i in 1:length(drug_tbl_list_args)) {
    
    drug_tbl_name <- paste0(names(drug_tbl_list_args[i]))
    
    xwalk <-
      find_rxnorm_level(drug_tbl=
                          drug_tbl_list_args[[i]][[1]])
    
    total_cts <- 
      xwalk %>% 
      summarise(total_rows=n(),
                total_pts=n_distinct(person_id)) %>% collect_new()
    
    grps <- dplyr::group_vars(xwalk)
    
    rxnorm_grpd <- c(grps, 'rxnorm_level')
    
    rxnorm_cts <- 
      xwalk %>%
      group_by(!!! syms(rxnorm_grpd)) %>%
      summarise(rxnorm_rows=n(),
                rxnorm_pts=n_distinct(person_id)) %>% collect_new()
    
    if(length(rxnorm_grpd) > 1) {
      
      props <- 
        rxnorm_cts %>%
        left_join(total_cts) %>% ungroup() %>%
        mutate(row_proportions=round(rxnorm_rows/total_rows,2)) %>%
        mutate(person_proportions=round(rxnorm_pts/total_pts,2)) %>%
        add_meta(check_lib = check_string) %>%
        mutate(tbl_name=drug_tbl_name)
      
    } else {
     
       props <- 
        rxnorm_cts %>%
        mutate(total_rows=total_cts$total_rows,
               total_pts=total_cts$total_pts) %>%
        #left_join(total_cts) %>% ungroup() %>%
        mutate(row_proportions=round(rxnorm_rows/total_rows,2)) %>%
        mutate(person_proportions=round(rxnorm_pts/total_pts,2)) %>%
        add_meta(check_lib = check_string) %>%
        mutate(tbl_name=drug_tbl_name)
      
    }
    
    
    drugs[[paste0(check_string,'_',names(drug_tbl_list[i]))]] <- props
    
  }
  
 drugs
  
}


#' compute metadata for drug specificity 
#' 
#' @param drug_tbl_list_args list of args for drug specificity
#' Requirements:
#' 1) List with name - names will be used to name the tables
#' 2) First element: query to specificy which drugs should be evaluated
#' 3) Second element: description of the table output
#' 
#' @return table with the following columns:
#' table_name | table_description | db_version | check_name
#' 
#' 

compute_st_conf_specificity_meta <- function(drug_tbl_list_args,
                                          db_version_arg = config('current_version'),
                                          check_string='st_conf_rxnorm') {
  
  metadrug <- tibble(
    table_name = character(),
    table_description = character(),
    db_version = character(),
    check_name = character()
  )
  
  for(i in 1:length(drug_tbl_list_args)) {
    
    desc <- drug_tbl_list_args[[i]][[2]]
    
    desc_name <- names(drug_tbl_list_args[i])
    
    final_thisround <- 
      metadrug %>%
      add_row(
        table_name = desc_name,
        table_description = paste0(check_string,'_',desc),
        db_version = config('current_version'),
        check_name = check_string
      )
      
    metadrug <- final_thisround
  }
  
  metadrug
}