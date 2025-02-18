
#' Initialize argos session
#'
#' @param session_name an arbitrary string label to identify your session
#' @param db_conn the database connection information; can either be a connection
#' object like those created by DBI::dbConnect OR the path to a JSON file containing
#' your connection information
#' @param is_json a boolean indicating whether db_conn is a file path pointing to a JSON
#' file or not
#' @param base_directory the base or working directory; in a project-oriented workflow, this 
#' will be the working directory established when opening the project.
#' @param specs_subdirectory the subdirectory within the base directory where any files to be used in the analysis
#' (i.e. concept sets) will be stored; defaults to `specs`
#' @param results_subdirectory the subdirectory within the base directory where results should be output; 
#' defaults to `results`
#' @param default_file_output a boolean indicating whether output_tbl should output a file by default or
#' if it should just output the results to the database; defaults to FALSE (i.e. no file output)
#' @param cdm_schema the schema on the database where the data in a CDM format is stored
#' @param results_schema the schema on the database where any results should be output
#' @param vocabulary_schema the schema on the database where vocabulary reference tables 
#' (i.e. the OHDSI vocabulary concept tables) are stored
#' @param results_tag if desired, a suffix to be appended onto results tables to help organize
#' project-specific output
#' @param cache_enabled a boolean value indicating whether repeated attempts to load the same
#' codeset (via load_codeset) should use a cached value rather than reloading; defaults to TRUE
#' @param retain_intermediates a boolean indicating whether intermediate/temporary tables should be
#' manifested and retained; defaults to FALSE
#' @param db_trace a boolean indicating whether the query log should include
#' detailed information about execution of SQL queries in the database 
#' (essentially a "verbose" argument); defaults to TRUE
#'
#' @returns will quietly load all exported argos functions into the environment and establish
#' the necessary configurations to allow them to operate; note that the argos session itself
#' will NOT appear in the global environment pane in the RStudio IDE
#' 
#' the connection information will print after this function is run to confirm connection to the database of choice
#' 
initialize_session <- function(session_name,
                               db_conn,
                               is_json = FALSE,
                               base_directory = getwd(),
                               specs_subdirectory = 'specs',
                               results_subdirectory = 'results',
                               default_file_output = FALSE,
                               cdm_schema = 'dcc_pedsnet',
                               results_schema,
                               vocabulary_schema = 'vocabulary',
                               results_tag = NULL,
                               cache_enabled = FALSE,
                               retain_intermediates = FALSE,
                               db_trace = TRUE){
  
  # Establish session
  argos_session <- argos$new(session_name)
  
  set_argos_default(argos_session)
  
  # Set db_src
  if(!is_json){
    get_argos_default()$config('db_src', db_conn)
  }else{
    get_argos_default()$config('db_src', srcr(db_conn))
  }
  
  # Set misc configs
  get_argos_default()$config('cdm_schema', cdm_schema)
  get_argos_default()$config('results_schema', results_schema)
  get_argos_default()$config('vocabulary_schema', vocabulary_schema)
  get_argos_default()$config('cache_enabled', cache_enabled)
  get_argos_default()$config('retain_intermediates', retain_intermediates)
  get_argos_default()$config('db_trace', db_trace)
  get_argos_default()$config('can_explain', !is.na(tryCatch(db_explain(config('db_src'), 'select 1 = 1'),
                                                            error = function(e) NA)))
  get_argos_default()$config('results_target', ifelse(default_file_output, 'file', TRUE))
  
  if(is.null(results_tag)){
    get_argos_default()$config('results_name_tag', '')
  }else{
    get_argos_default()$config('results_name_tag', results_tag)
  }
  
  # Set working directory
  get_argos_default()$config('base_dir', base_directory)
  
  # Set specs & results directories
  ## Drop path to base directory if present
  specs_drop_wd <- str_remove(specs_subdirectory, base_directory)
  results_drop_wd <- str_remove(results_subdirectory, base_directory)
  get_argos_default()$config('subdirs', list(spec_dir = specs_drop_wd,
                                             result_dir = results_drop_wd))
  
  # Print session information
  db_str <- DBI::dbGetInfo(config('db_src'))
  cli::cli_div(theme = list(span.code = list(color = 'blue')))
  
  cli::cli_inform(paste0('Connected to: ', db_str$dbname, '@', db_str$host))
  cli::cli_inform('To see environment settings, run {.code get_argos_default()}')
}