
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