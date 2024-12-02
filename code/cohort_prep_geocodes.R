

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
  
  current_locations <- cdm_tbl('person') %>%
    select(site, person_id, location_id) %>%
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
  
  opt <- list('tract_level' = fips_code_tct,
              'block_group_level' = fips_code_bg)
  
  return(opt)
  
}