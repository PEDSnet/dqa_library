
#' Connect to an existing CDM vocabulary table
#'
#' @param name The name of the table
#' @param db The database connection; you will rarely need to specify this.
#'
#' @return A [dplyr::tbl()]] pointing to the table
#' @md
vocabulary_tbl_spark <- function(name, 
                                 conn = config('db_src'),
                                 db = config('db')){
  
  path_build <- paste0('hdfs:////data/', db, '/', 
                       config('vocabulary_schema'), '/', name)
  
  tbl <- spark_read_parquet(sc = db,
                            name = name,
                            path = path_build,
                            memory = FALSE)
  
}

#' Add site to the cdm_tbl
#' 
#' @param name the name of the table, as a string
#' @param site_filter the name of the site to filter by, if 
#' filtering by a site; defaults to `config('site_filter')`.
#' If pointing to a site_specific schema and no filter is needed,
#' `config('site_filter')` should be set to NA
#' 
#' @return the cdm_tbl name with site as a grouper
#' 

cdm_tbl_spark <- function(name,
                         conn = config('db_src'),
                         db = config('db'),
                         site = TRUE) {
  
  path_build <- paste0('hdfs:////data/', db, '/', 
                       config('cdm_schema'), '/', name)
  
  if(site){
    
    filt_col <- config('site_filter')
    
    tbl <- spark_read_parquet(sc = conn,
                              name = name,
                              path = path_build,
                              memory = FALSE) %>%
      filter(site == filt_col)
  }else{
    tbl <- spark_read_parquet(sc = conn,
                              name = name,
                              path = path_build,
                              memory = FALSE)
  }
  
  return(tbl)
  
}

#' Add site to the cdm_tbl_prev
#' 
#' @param name the name of the table, as a string
#' @param site_filter the name of the site to filter by, if 
#' filtering by a site; defaults to `config('site_filter_previous')`.
#' If pointing to a site_specific schema and no filter is needed,
#' `config('site_filter_previous')` should be set to NA
#' 
#' @return the cdm_tbl_previous name with site as a grouper
#' 
cdm_tbl_prev_spark <- function(name,
                         conn = config('db_src'),
                         db = config('db_prev'),
                         site = TRUE) {
  
  path_build <- paste0('hdfs:////data/', db, '/',
                       config('cdm_schema_prev'), '/', name)
  
  if(site){
    
    filt_col <- config('site_filter')
    
    tbl <- spark_read_parquet(sc = conn,
                              name = name,
                              path = path_build,
                              memory = FALSE) %>%
      filter(site == filt_col)
  }else{
    tbl <- spark_read_parquet(sc = conn,
                              name = name,
                              path = path_build,
                              memory = FALSE)
  }
  
  return(tbl)
  
}


#' output table to database if it does not exist, or
#' append it to an existing table with the same name if it does
#' 
#' @param data the data to output
#' @param name the name of the table to output 
#' 
#' Parameters are the same as `output_tbl`
#' 
#' @return The table as it exists on the databse, with the new data
#' appended, if the table already existts.
#' 

output_tbl_spark <- function(data, 
                       name = NA, 
                       append = FALSE,
                       local = FALSE,
                       file = ifelse(config('execution_mode') !=
                                       'development', TRUE, FALSE),
                       conn = config('db_src'),
                       db = config('db'),
                       results_tag = TRUE,
                       indexes = NULL,
                       ...) {
  
  if (is.na(name)) name <- quo_name(enquo(data))
  
  if (file) {
    rslt <- .output_csv(data, name, local)
  }
  
  if(results_tag){
    name <- paste0(name, config('results_tag'))
  }
  
  path_build <- paste0('hdfs:////data/', db, '/', 
                       config('results_schema'), '/', name) 
  
  name <- data
  
  if(append){
    
    tbl <- spark_write_parquet(x = name,
                               path = path_build,
                               mode = 'append',
                               partition_by = indexes,
                               ...)
    
  }else{
    
    tbl <- spark_write_parquet(x = name,
                               path = path_build,
                               mode = 'overwrite',
                               partition_by = indexes,
                               ...)
    
  }
  
  return(tbl)
  
}


#' @param name The name of the codeset.  Typically just the file name
#'   without `.csv` suffix, but if for some reason you've expanded the
#'   directory tree under `specs`, you may prefix it with
#'   subdirectories.
#' @param col_types A column specification compatible with
#'   [readr::read_csv()].
#' @param full_path A Boolean value indicating whether `name` represents a full
#'   path or a relative file name that should be expanded.
#'
#' @return A tbl contanining the codeset
#' @md
read_codeset <- function(name,
                         col_types = 'iccc',
                         full_path = FALSE) {
  path <-
    if_else(full_path, name,
            file.path(config('base_dir'), config('subdirs')$spec_dir,
                      paste0(name, '.csv')))
  read_csv(path, col_names = TRUE, col_types = col_types)
}

# Invalidate cache when run starts
config_rm('_codesets')

#' @inheritParams read_codeset
#' @param table_name An optional name for the table, if you want it to
#'   differ from `name`.
#' @param indexes A list of columns on which indexes should be created.
#' @param db A connection to the database into which to load the codeset.
#'
#' @return A tbl pointing to the table in the database
#' @md
load_codeset_spark <- function(name,
                         col_types = 'iccc',
                         table_name = name,
                         indexes = list('concept_id'),
                         full_path = FALSE,
                         conn = config('db_src')) {
  
  if (config('cache_enabled')) {
    if (is.null(config('_codesets'))) config('_codesets', list())
    cache <- config('_codesets')
    if (! is.null(cache[[name]])) return(cache[[name]])
  }
  
  codes <- sdf_broadcast(copy_to(conn, 
                                 df = read_codeset(name, col_types),
                                 name = name,
                                 overwrite=TRUE,
                                 memory = FALSE,
                                 indexes = indexes))
  
  if (config('cache_enabled')) {
    cache[[name]] <- codes
    config('_codesets', cache)
  }
  
  codes
}