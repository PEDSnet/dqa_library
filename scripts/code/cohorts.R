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
  
  db_remove_table(name = in_schema(config('results_schema'), paste0(config('site'),'_voml_op_1510')))
  db_remove_table(name = in_schema(config('results_schema'), paste0(config('site'),'_vodi_op_1510')))
  db_remove_table(name = in_schema(config('results_schema'), paste0(config('site'),'_prvo_op_1510')))
  db_remove_table(name = in_schema(config('results_schema'), paste0(config('site'),'_vipdp_op_1510')))
  db_remove_table(name = in_schema(config('results_schema'), paste0(config('site'),'_ckddx_op_1510')))
  db_remove_table(name = in_schema(config('results_schema'), paste0(config('site'),'_htnrx_op_1510')))
  db_remove_table(name = in_schema(config('results_schema'), paste0(config('site'),'_pdl_pts_op_1510')))
  
}