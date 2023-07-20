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
  
  db_remove_table(name = in_schema(config('results_schema'), 'site_voml_pasc_109'))
  db_remove_table(name = in_schema(config('results_schema'), 'site_vodi_pasc_109'))
  db_remove_table(name = in_schema(config('results_schema'), 'site_prvo_pasc_109'))
  db_remove_table(name = in_schema(config('results_schema'), 'site_vipdp_pasc_109'))
  db_remove_table(name = in_schema(config('results_schema'), 'site_ckddx_pasc_109'))
  db_remove_table(name = in_schema(config('results_schema'), 'site_htnrx_pasc_109'))
  
}


