#'
#' This file contains functions to identify cohorts in a study.  Its
#' contents, as well as the contents of any R files whose name begins
#' with "cohort_", will be automatically sourced when the request is
#' executed.
#'
#' For simpler requests, it will often be possible to do all cohort
#' manipulation in functions within this file.  For more complex
#' requests, it may be more readable to separate different cohorts or
#' operations on cohorts into logical groups in different files.
#'

## Remove Tables Function
remove_precompute <- function() {
  
  db_remove_table(name = in_schema(config('results_schema'), paste0(config('site'),'_voml')))
  db_remove_table(name = in_schema(config('results_schema'), paste0(config('site'),'_vodi')))
  db_remove_table(name = in_schema(config('results_schema'), paste0(config('site'),'_prvo')))
  db_remove_table(name = in_schema(config('results_schema'), paste0(config('site'),'_vipdp')))
  db_remove_table(name = in_schema(config('results_schema'), paste0(config('site'),'_ckddx')))
  db_remove_table(name = in_schema(config('results_schema'), paste0(config('site'),'_htnrx')))
  db_remove_table(name = in_schema(config('results_schema'), paste0(config('site'),'_pdl_pts')))
  db_remove_table(name = in_schema(config('results_schema'), paste0(config('site'),'_iptwo')))
  
}


