

#' Process geocodes for analysis
#'
#' @param fips_tbl the CDM location_fips table
#' @param person_tbl the CDM person table
#'
#' @return a list of two dataframes: one assembling the full geocode up to the tract
#' level with NAs and improper characters removed, and a second doing the same up to
#' the block group level
#' 
prep_geocodes <- function(fips_tbl = cdm_tbl('location_fips'),
                          person_tbl = cdm_tbl('person')){
    
  # Current Locations
  current_locations <- person_tbl %>%
    left_join(fips_tbl)
  
  add_nas <- current_locations %>%
    # filter(!is.na(geocode_state) & !is.na(geocode_county) &
    #          !is.na(geocode_tract)) %>%
    collect_new() %>%
    mutate(across(where(is.character), ~ na_if(.,""))) %>%
    mutate(across(where(is.character), ~ na_if(.," ")))
  
  ## Tract
  fips_code_tct <- add_nas %>%
    mutate(fips_code = paste0(geocode_state, geocode_county, geocode_tract),
           fips_code = str_remove_all(fips_code, '[A-Za-z]'),
           ndigit_fips = nchar(fips_code))
  
  ## Block Group
  fips_code_bg<- add_nas %>%
    mutate(fips_code = paste0(geocode_state, geocode_county, geocode_tract, geocode_group),
           fips_code = str_remove_all(fips_code, '[A-Za-z]'),
           ndigit_fips = nchar(fips_code))
  
  # Location History
  lohis_fips <- cdm_tbl('location_history') %>%
    filter(tolower(domain_id) == 'person') %>%
    rename(person_id = entity_id) %>%
    select(site, person_id, location_id, start_date, end_date) %>%
    left_join(fips_tbl) %>%
    inner_join(person_tbl %>% select(site, person_id))
  
  add_nas_lohis <- lohis_fips %>%
    # filter(!is.na(geocode_state) & !is.na(geocode_county) &
    #          !is.na(geocode_tract)) %>%
    collect_new() %>%
    mutate(across(where(is.character), ~ na_if(.,""))) %>%
    mutate(across(where(is.character), ~ na_if(.," ")))
  
  ## Build code
  lohis_code_tct<- add_nas_lohis %>%
    mutate(fips_code = paste0(geocode_state, geocode_county, geocode_tract),
           fips_code = str_remove_all(fips_code, '[A-Za-z]'),
           ndigit_fips = nchar(fips_code))
  
  lohis_code_bg<- add_nas_lohis %>%
    mutate(fips_code = paste0(geocode_state, geocode_county, geocode_tract, geocode_group),
           fips_code = str_remove_all(fips_code, '[A-Za-z]'),
           ndigit_fips = nchar(fips_code))
  
  ## Count per patient
  lohis_summ_tract <- lohis_code_tct %>%
    filter(ndigit_fips == 11) %>%
    group_by(site, person_id, geocode_year) %>% 
    summarise(ngeo_lohis = n_distinct(location_id))
  
  lohis_summ_bg <- lohis_code_bg %>%
    filter(ndigit_fips == 12) %>%
    group_by(site, person_id, geocode_year) %>% 
    summarise(ngeo_lohis = n_distinct(location_id))
  
  opt <- list('tract_level' = fips_code_tct,
              'block_group_level' = fips_code_bg,
              'lohis_tract' = lohis_summ_tract,
              'lohis_bg' = lohis_summ_bg)
  
  return(opt)
  
}