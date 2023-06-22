
##Summary Functions

#Connect to a table outside the CDM
connect_table <- function(table_name){
  return(tbl(config('db_src'), table_name))
}

#Get Count

get_patient_count <- function(table_one){
  return(table_one %>% select(person_id) %>% distinct() %>% tally())
}

#Make percentage instead of count
make_percentage <- function(data, field = "Patient_Count"){
  data %>% mutate(Percent_Total = round(!!rlang::sym(field)*100/sum(!!rlang::sym(field)),2))
}

#Get by Gender
gender_breakdown <- function(table_one, concept_name = T){
  if(!concept_name){ 
    table_one %>% 
    left_join(vocabulary_tbl('concept') %>% select(concept_id, gender_concept_name = concept_name),
              by = c('gender_concept_id' = 'concept_id'))}
  
  table_one %>% 
    group_by(gender_concept_id, gender_concept_name) %>%
    summarize(Patient_Count = n_distinct(person_id)) %>%
    select(gender_concept_id, gender_concept_name, Patient_Count)
}

#Get by Race
race_breakdown <- function(table_one, concept_name = T){
  if(!concept_name){ 
    table_one %>% 
      left_join(vocabulary_tbl('concept') %>% select(concept_id, race_concept_name = concept_name),
                by = c('race_concept_id' = 'concept_id'))}
  
  table_one %>% 
    group_by(race_concept_id, race_concept_name) %>%
    summarize(Patient_Count = n_distinct(person_id)) %>%
    select(race_concept_id, race_concept_name, Patient_Count)
}

#Get by Ethnicity
ethnicity_breakdown <- function(table_one, concept_name = T){
  if(!concept_name){ 
    table_one %>% 
      left_join(vocabulary_tbl('concept') %>% select(concept_id, ethnicity_concept_name = concept_name),
                by = c('ethnicity_concept_id' = 'concept_id'))}
  
  table_one %>% 
    group_by(ethnicity_concept_id, ethnicity_concept_name) %>%
    summarize(Patient_Count = n_distinct(person_id)) %>%
    select(ethnicity_concept_id, ethnicity_concept_name, Patient_Count)
}

#Get by Site 
site_breakdown <- function(table_one){
  if('site' %in% colnames(table_one)){
   result <- table_one %>% 
      group_by(site) %>%
      summarize(Patient_Count = n_distinct(person_id)) %>%
      select(site, Patient_Count)
  }
  else{
  result <- table_one %>% 
    inner_join(cdm_tbl('person') %>% select(person_id, site), by = 'person_id') %>%
    group_by(site) %>%
    summarize(Patient_Count = n_distinct(person_id)) %>%
    select(site, Patient_Count)
  }
  return(result)
}



