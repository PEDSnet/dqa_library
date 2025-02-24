

#' Connect to an existing CDM data table from previous cycle
#'
#' @param name The name of the table
#' @param db The database connection; you will rarely need to specify this. 
#' Defaults to `config('db_src_prev')`
#'
#' @return A [dplyr::tbl()]] pointing to the table
#' @md

results_tbl <- function(name, db = config('db_src'),
                        results_tag =  TRUE, local_tag = FALSE) {
  .qual_tbl(intermed_name(name, temporary = FALSE,
                          results_tag = results_tag,
                          local_tag = local_tag),
            'results_schema', db)
}


#' Connect to an existing CDM data table from previous cycle
#'
#' @param name The name of the table
#' @param db The database connection; you will rarely need to specify this. 
#' Defaults to `config('db_src_prev')`
#'
#' @return A [dplyr::tbl()]] pointing to the table
#' @md

results_tbl_prev <- function(name, db = config('db_src_prev'),
                        results_tag =  TRUE, local_tag = FALSE) {
  .qual_tbl(intermed_name(name, temporary = FALSE,
                          results_tag = results_tag,
                          local_tag = local_tag,
                          schema = 'results_schema_prev'),
            'results_schema_prev', db)
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

site_cdm_tbl <- function(name,
                       site_filter = config('site_filter'),
                       ...) {
  
  my_tbl <- cdm_tbl(name, ...)
  
  if (!is.na(site_filter)) {
    my_tbl_new <- filter(my_tbl,site == site_filter)
  } else {my_tbl_new <- my_tbl}
  
  my_tbl_new
  
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
site_cdm_tbl_prev <- function(name,
                              site_filter = config('site_filter_previous'),
                              ...) {
  
  my_tbl <- cdm_tbl_prev(name)
  
  if (!is.na(site_filter)) {
    my_tbl_new <- filter(my_tbl,site == site_filter)
  } else {my_tbl_new <- my_tbl}
  
  my_tbl_new
  
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

output_tbl_append <- function(data, name = NA, local = FALSE,
                              file = ifelse(config('execution_mode') !=
                                              'development', TRUE, FALSE),
                              db = ifelse(config('execution_mode') !=
                                            'distribution', TRUE, FALSE),
                              results_tag = TRUE, ...) {
  
  if (is.na(name)) name <- quo_name(enquo(data))
  
  if(db_exists_table(config('db_src'),intermed_name(name,temporary=FALSE))) {
    
    tmp <- results_tbl(name) %>% collect_new 
    new_tbl <- 
      dplyr::union(tmp,
                   data)
    output_tbl(data=new_tbl,
               name=name,
               local=local,
               file=file,
               db=db,
               results_tag = TRUE, ...)
  } else {
    output_tbl(data=data,
               name=name,
               local=local,
               file=file,
               db=db,
               results_tag = TRUE, ...)
  }
  
  
}

#' Return `cdm_tbl()` from the previous data cycle
#' 
#' @param name the name of the table from the previous data cycle
#' @param db the name of the database; defaults to `config('db_src_prev')`
#' 
#' The cdm_tbl() from the previous database
#' 

cdm_tbl_prev <- 
  function(name, db = config('db_src_prev'))
    .qual_tbl(name, 'cdm_schema_prev', db)



#' Group by any variable needed
#' 
#' @param tbl_name table name to group
#' @param group_by_vars vector of variables to group by 
#' @param current_cycle logical that flags whether table is in current cycle
#' 
#' @return the table grouped by the variables specified


group_by_opt <- function(tbl_name,
                         group_by_vars,
                         current_cycle=TRUE) {
  
  if(is.na(group_by_vars) | length(group_by_vars) == 0) {cdm_tbl(tbl_name)}
    else {
      group_by_syms <- rlang::syms(group_by_vars)
      if (current_cycle) {grpd_tbl <- 
        cdm_tbl(tbl_name) %>%
          group_by(!!! group_by_syms)
      } else {grpd_tbl <- 
        cdm_tbl_prev(tbl_name) %>%
            group_by(!!! group_by_syms)}
    }
  
  grpd_tbl
  
}



#' list of tables to filter
#' 
#' @param filter_list
#'  a list with the following structure:
#'   - list element name should be the name of the db table in quotes (e.g., `'visit_occurrence'``)
#'   - name of variables to filter by, enclosed by a quosure 
#'       ---- e.g., `quos(visit_concept_id %in% c(9203L, 9201L, 9202L))` 
#' @param current_cycle logical that flags whether table is in current cycle
#' 
#' @return a list of tables, with each element 
#' filtered by the specifications
#' 
#' 


filter_values <- function(filter_list,
                          current_cycle=TRUE) {
  
  tbl_list <- list()
  
  for(i in 1:length(filter_list)) {
    
    tbl_name <- names(filter_list[i])
    
    if(is.na(filter_list[[i]])) {this_iteration <- cdm_tbl(tbl_name)}
      else {
        
        if(current_cycle) {this_iteration <- 
          cdm_tbl(tbl_name) %>% filter(!!!(filter_list[[i]]))
        } else {this_iteration <- 
          cdm_tbl_prev(tbl_name) %>% filter(!!!(filter_list[[i]]))}
        
        
      }
    
    tbl_list[[paste0(names(filter_list[i]))]] <- this_iteration
    
    }
    
  tbl_list
  
}


#' list of tables to group_by
#' 
#' @param filter_list
#'  a list with the following structure:
#'   - list element name should be the name of the db table in quotes (e.g., `'visit_occurrence'``)
#'   - vector of variables to group by, with each element a string
#'       ---- e.g., `c('visit_concept_id')` 
#' @param current_cycle logical that flags whether table is in current cycle
#' 
#' @return a list of tables, with each element 
#' filtered by the specifications
#' 
#' 


group_by_values <- function(group_by_list,
                            current_cycle=TRUE) {
  
  tbl_list <- list()
  
  if(length(group_by_list) == 0) {tbl_list = tbl_list
  } else {
    
    for(i in 1:length(group_by_list)) {
      
      this_iteration <-
        group_by_opt(tbl_name=names(group_by_list[i]),
                     group_by_vars=group_by_list[[i]][[1]],
                     current_cycle=current_cycle)
      
      tbl_list[[paste0(names(group_by_list[i]))]] <- this_iteration
      
    }
    
  }
  
  
  tbl_list
  
}


#' full list of tables
#' 
#' @param full_tbl_list list of tables where further manipulation is not necessary
#' @param current_cycle logical that flags whether table is in current cycle
#' 
#' @return a list with each element being the table specified
#' 


full_tbl_values <- function(full_tbl_list,
                            current_cycle=TRUE) {
  tbl_list <- list()
  
  for(i in 1:length(full_tbl_list)) {
    tbl_name <- full_tbl_list[[i]]
    if(current_cycle) {this_iteration <- cdm_tbl(tbl_name)
    } else {this_iteration <- cdm_tbl_prev(tbl_name)}
        
    tbl_list[[paste0(tbl_name)]] <- this_iteration
  }
  
  tbl_list
  
}


#' output a list of tables to the database 
#' 
#' @param output_list list of tables to output
#' @param append logical to determine if you want to append if the table exists
#' 
#' @return tables output to the database; if 
#' table already exists, it will be appended
#' 

output_list_to_db <- function(output_list,
                              append=TRUE) {
  
  
  if(append) {
    
    for(i in 1:length(output_list)) {
      
      output_tbl_append(data=output_list[[i]],
                        name=names(output_list[i]))
      
    }
    
  } else {
    
    for(i in 1:length(output_list)) {
      
      output_tbl(data=output_list[[i]],
                 name=names(output_list[i]))
      
    }
    
  }
 
}

#' add check name, db version, and site name to a given table
#' 
#' @param tbl_meta the table to add meta information to 
#' @param check_lib the name of the check
#' @param version the version of the database; defaults to 
#' `config('current_version')`;
#' @param site_nm the name of the site; defaults to
#' `config('site')`
#' 

add_meta <- function(tbl_meta,
                     check_lib,
                     version=config('current_version'),
                     site_nm=config('site')) {
  
  tbl_meta %>%
    mutate(check_type = check_lib,
           database_version=version,
           site=site_nm) 
    
  
}


#' @md
#' compare cycles meta; the list should have both the table name  
#' as well as 
#' 
#' @param meta_tbls a list of descriptions for the tables that are being computed;
#' should contain both the table name as well as a description of the table
#' @param versions_tbl_list a list of the computed tables
#' @param check_string the string for the check name
#' 
#' @return A four column table that has table name, description, number of columns,
#' number of rows; this is the meta table for checking between versions
#' 

compute_meta_tbl <- function(meta_tbls, 
                             versions_tbl_list,
                             check_string='st_cons_dc') {
  
  
  meta_tibble <- 
    tibble(
      table_name = character(),
      table_description = character(),
      table_col_num = numeric(),
      table_row_num = numeric()
    )
  
  for(i in 1:length(meta_tbls)) {
    
    
    full_tbl_name = paste0(check_string, '_', names(meta_tbls[i]))
    
    table_type_name = full_tbl_name
    table_description_thisround = meta_tbls[[i]]
    
    matched_tbl <- 
      intersect(names(versions_tbl_list),
                full_tbl_name)
    
    matched_tbl_data <- versions_tbl_list[[matched_tbl]]
    
    
    number_of_columns = ncol(matched_tbl_data)
    number_of_rows = nrow(matched_tbl_data)
    
    meta_tibble_thisround <- 
      meta_tibble %>%
      add_row(
        table_name=table_type_name,
        table_description = table_description_thisround,
        table_col_num = number_of_columns,
        table_row_num = number_of_rows
      )
    
    meta_tibble <- meta_tibble_thisround
    
  }
  meta_tibble
}

#' Connect to a result table in another schema. The `schema_name` should be 
#' set in the config with `results_schema_other`. The output is `results_tbl_other`.
#'
#' This function sets up the connection to a database table that was presumably
#' created by during execution of this or a prior data request, and is found
#' in the schema designated for results, whether the table itself is intended to
#' be permanent or just an intermediate for this run.
#'
#' @param name The name of the table
#' @param db The database connection; you will rarely need to specify this.
#' @param results_tag The request tag to add to the table name (see [intermed_name()]).
#' @param local_tag The local tag to add to the table name (see [intermed_name()]).
#'
#' @return A [dplyr::tbl()]] pointing to the table
#' @seealso [intermed_name()], for more information on how the table
#'   specification is determined.
#' @md
results_tbl_other <- function(name, db = config('db_src'),
                        results_tag =  FALSE, local_tag = TRUE) {
  .qual_tbl(intermed_name(name, temporary = FALSE,
                          results_tag = results_tag,
                          local_tag = local_tag,
                          schema='results_schema_other'),
            schema_tag='results_schema_other', db)
}


#' Calculate Date Differences in Multiple SQL Backends
#'
#' Function to get sql code for number of days between date1 and date2.
#' Adapted for sql dialects for Postgres and MS SQL.
#'
#' Should always be wrapped by sql()
#' @param date_col_1 Date col 1
#' @param date_col_2 Date col 2
#' @param db connection type object. Defaulted to config('db_src') for standard framework
#' Functionality added for Postgres, MS SQL and Snowflake
#'
#' @return an integer representing the difference (in days) between the two provided
#' dates
#'
calc_days_between_dates <-
  function(date_col_1, date_col_2, db = config("db_src")) {
    if (class(db) %in% "Microsoft SQL Server") {
      sql_code <-
        paste0("DATEDIFF(day, ", date_col_1, ", ", date_col_2, ")")
    } else if (class(db) %in% "PqConnection") {
      sql_code <-
        paste0(date_col_2, " - ", date_col_1)
    } else if (class(db) %in% "Snowflake") {
      sql_code <-
        paste0(
          "DATEDIFF(day, ",
          '"',
          date_col_1,
          '"',
          ",",
          '"',
          date_col_2,
          '"',
          ")"
        )
    }else if(class(db) %in% 'SQLiteConnection'){
      sql_code <-
        paste0("julianday(", date_col_2, ") - julianday(", date_col_1, ")")
    }else if(class(db) %in% 'PrestoConnection'){
      sql_code <-
        paste0("date_diff(day, ", date_col_1, ", ", date_col_2, ")")
    }
    return(sql_code)
  }
