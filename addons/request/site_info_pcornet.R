### Addon
# For queries targeting the PCORnet CDM, this file should replace the
# site/site_info.R file in the standard package.
### Addon


#' Site-specific information for data request execution.
#'
#' Please edit information as noted below.
#'
#' The information in this file describes site-specific practices for managing
#' data requests in general, such as connection information and organiation of
#' databases, and defaults for output handling.  These settings are not
#' typically request-specific, and will likely remain unchanged across multiple
#' requests targeting the same version of data.  As a result, you can often copy
#' this file from one request to another, or point multiple requests to a single
#' site_info.R file.
#'
#' @md
"site_info.R"

#' Your site's name.
#' @md
config('site', 'site_name')

#' Code to establish a database connection at your site.
#'
#' The connection must be able to reach CDM data and any result schemata needed.
#' The connection may be either a dplyr-style src_foo() object or a DBI-style
#' dbConnect() object.
#'
#' A few notes:
#'
#' * You may find it convenient to use the Argos package at
#'   [https://github.com/baileych/ohdsi-argos] to abstract database connection
#'   information such as credentials and server names out of this file.
#' * If using Oracle, the following are required before loading ROracle, if
#'   these are not set in the global environment:
#'     * `Sys.setenv(TZ=Sys.timezone())`
#'     * `Sys.setenv(ORA_SDTZ=Sys.timezone())`
#'
#' @md
config('db_src', {
  # require(Argos);
  # src_argos('argos_pcornet')
})

#' Name of the schema, if any, to be prepended to CDM fact table names.
#'
#' @details
#' If `NA`, no schema qualifier is added.
#' @md
config('cdm_schema', 'pedsnet')

#' Name of the schema in which to create intermediate and results tables
#'
#' This value determines whether a schema name is added to names of tables
#' holding intermediate or final results.  If it is `NA`, no explicit schema is
#' used, and tables are created wherever the DBMS places them.  It can be
#' overridden by the request-specific setting in `run.R`.
#' @md
config('default_results_schema', NA)

#' Whether or not to keep intermediate tables
#'
#' This Boolean value determines whether tables holding codesets or intermediate
#' steps are retained after execution completes.  If `FALSE`, they are created
#' as temporary tables.  It can be overridden by the request-specific setting in
#' `run.R`.
#' @md
config('default_retain_intermediates', FALSE)


#' Names of standard tables used in queries.
#' @md
#'
#' Please edit only the right-hand side of each assignment.
#' Table names on the left must be lower-case; those on the right
#' must reflect naming conventions in the database.
config('table_names',
       list(condition = 'condition',
            death = 'death',
            death_cause = 'death_cause',
            demographic = 'demographic',
            diagnosis = 'diagnosis',
            dispensing = 'dispensing',
            encounter = 'encounter',
            enrollment = 'enrollment',
            harvest = 'harvest',
            hash_token = 'hash_token',
            immunizaiotn = 'immunization',
            lab_result_cm = 'lab_result_cm',
            lds_address_history = 'lds_address_history',
            med_admin = 'med_admin',
            obs_clin = 'obs_clin',
            obs_gen = 'obs_gen',
            pcornet_trial = 'pcornet_trial',
            prescribing = 'prescribing',
            pro_cm = 'pro_cm',
            procedures = 'procedures',
            provider = 'provider',
            vital = 'vital'))

#> ##################### End of site-specific configuration
