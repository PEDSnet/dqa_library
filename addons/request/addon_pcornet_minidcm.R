########
#
# This file is an addon to the PEDSnet standard framework.  In order to make use
# of it, copy this file to the `code` directory of your request-specific copy
# of the framework.  The functions defined here will be automatically included
# when the request is run.
#
########
suppressPackageStartupMessages(require(stringr))

#' Extract a PCORnet mini-CDM for a cohort
#'
#' In order to have a stable substrate on which to develop cohort-specific
#' analyses, it is common practice to identify patients in a screening cohort
#' and then retrieve all CDM data for those patients into a schema separate
#' from the main PCORnet databases.  This may help you develop more rapidly,
#' it will preserve a substrate for study after the main database has been
#' archived, or it can be used to generate a study-specific CDM that can be
#' made available to a study team after appropriate postprocessing.
#'
#' The mini-CDM extraction, as encapsulated in [build_mini_cdm()], proceeds in
#' three stages:
#'
#' 1. For CDM tables that have a `patid`, relevant records are extracted.
#'    These are referred to in this addon as "personal tables".
#' 2. For CDM tables that don't have a `patid`, the records from step 1 are
#'    used to construct a set of other IDs included (e.g. `encounterid`),
#'    and those are in turn used to extract records relevant to the cohort.
#'    These are referred to as "impersonal tables" in this addon.
#'
#' By default, as a mini-CDM is built, the tables are materialized in the
#' results schema, with the result tag appended.  It's possible to disable this,
#' in which case the process will return a set of unmaterialized tbls.
#' However, this can make the creation of impersonal tables extremely slow,
#' since the unmaterialized queries may need to be executed several times to
#' track down foreign keys.
#'
#' When extracting a mini-CDM, you have the option of generating tables as
#' used in the PEDSnet DCC, which include additional precomputed columns
#' intended to facilitate exploratory analyses.  Alternatively, you can
#' generate a more straightforward PCORnet CDM, without these added columns.
#' This option is controlled by the `clean` parameters to the table extraction
#' functions.
#'
#' Finally, it is important to note that extracted mini-CDM contains all of the
#' data in the main CDM, and is intended for use by PEDSnet certified data
#' scientists. If it is to be made available to a study team or released
#' outside of the PEDSnet DCC, then the standard processes of desensitization
#' (cf. the `desensitize` addon) and lessidentification (cf. the
#' [PEDSnet::Lessidentify](https://github.com/PEDSnet/PEDSnet-Lessidentify)
#' utility).
#'
#' @return This function exists only for its documentation
#' @md
minicdm.pcornet.vignette <- function() {
  'See help on this topic for a summary of extracting a PCORnet mini-cdm'
}


# Given a database tbl containing PCORnet CDM data, remove content common to
# all tables that we don't forward to study teams.
# N.B This is intended to clean up internal derivations and non-source-value
# columns that are not disclosed.  It does not replace lessidentification or
# desensitization.
.clean_table_base <- function(table) {
    table %>%
    select(-any_of('site'))
}
.trial_cleanup <- function(tr_tbl) {
  tr_tbl %>%
    mutate(trial_invite_code = as.character(''))
}
.provider_cleanup <- function(pr_tbl) {
  pr_tbl %>% mutate(provider_npi = NA_character_)
}
.obs_clin_cleanup <- function(oc_tbl) {
  oc_tbl %>% mutate(obsclin_result_text = NA_character_)
}
.obs_gen_cleanup <- function(og_tbl) {
  og_tbl %>% mutate(obsgen_result_text = NA_character_)
}
.lds_address_history_cleanup <- function(ah_tbl) {
  ah_tbl %>% mutate(address_city = NA_character_, address_zip9 = NA_character_)
}
.imm_cleanup <- function(imm_tbl) {
  imm_tbl %>% mutate(vx_lot_num = NA_character_)
}

#' Metadata about personal tables
#'
#' Return a tbl whose rows are tables containing a `patid`, with
#' corresponding values providing a function that cleans up rows in that table
#' for release to study teams and a set of columns to index when the table is
#' instantiated.  Note that the table-specific cleanups are in addition to the
#' general changes made in [.clean_table_base()].
#'
#' If you wish to include additional personal tables in your extraction, you
#' may add rows to the returned tbl.  Each must have three columns:
#'
#' * `tbl_expr` - a dplyr tbl specifying the CDM table to extract
#' * `cleanup_name` - the name of a table-specific cleanup function. If it is
#'   `NA`, no table-specific cleanup is done
#' * `indexes` - a list specifying the indices to construct, as described for
#'    [dbplyr::compute()].
#' * `output_name` - the name to give the resulting mini-CDM tbl
#'
#' For example, if you have a table named `my_cohort`, you might say something
#' like
#'
#' > my_meta = personal_tables() %>%
#' >   bind_rows(tibble_row(tbl_expr = cdm_tbl('my_cohort'),
#' >                        output_name = 'my_cohort',
#' >                        indexes = list('patid', 'cohort_id')))
#'
#' It is also frequent practice to use a common prefix or other part of the
#' table name to make it easy to group mini-cdm tables.  You can do this by
#' making appropriate modifications to the `output_name` column in the metadata,
#' such as
#'
#' > my_meta = personal_tables() %>%
#' >   mutate(output_name = str_replace(output_name, '(.+)', 'cdm_\\1')
#'
#' Finally, it will be useful in many cases to constrain the data collected
#' into the mini-cdm, for instance to limit to a defined study period.  This is
#' done by modifying the `tbl_expr` for each row appropriately. As the name
#' implies, these are dplyr expressions, and can be constrained using verbs
#' such as [dplyr::filter()].
#'
#' @return The tbl of tables
#' @md
personal_tables <- function() {
  tribble( ~tbl_expr, ~cleanup_name, ~indexes, ~output_name,
          cdm_tbl('demographic'), NA, list('patid'), 'demographic',
          cdm_tbl('enrollment'), NA, list('patid'), 'enrollment',
          cdm_tbl('encounter'), NA, list('patid', 'encounterid', 'enc_type'), 'encounter',
          cdm_tbl('diagnosis'), NA,
          list('patid', 'diagnosisid', 'encounterid', c('dx', 'dx_type')),
          'diagnosis',
          cdm_tbl('procedures'), NA,
          list('patid', 'proceduresid', 'encounterid', c('px', 'px_type')), 'procedures',
          cdm_tbl('vital'), NA,  list('patid', 'vitalid', 'encounterid'), 'vital',
          cdm_tbl('dispensing'), NA,
          list('patid', 'dispensingid', 'ndc'), 'dispensing',
          cdm_tbl('lab_result_cm'), NA,
          list('patid', 'lab_result_cm_id', 'encounterid', 'lab_loinc'),
          'lab_result_cm',
          cdm_tbl('condition'), NA,
          list('patid', 'encounterid', 'conditionid', c('condition', 'condition_type')),
          'condition',
          cdm_tbl('pro_cm'), NA,
          list('patid', 'encounterid', 'pro_cm_id'), 'pro_cm',
          cdm_tbl('prescribing'), NA, list('patid', 'prescribingid', 'rxnorm_cui'),
          'prescribing',
          cdm_tbl('pcornet_trial'), '.trial_cleanup', list('patid', 'trialid', 'participantid'),
          'pcornet_trial',
          cdm_tbl('death'), NA, list('patid'), 'death',
          cdm_tbl('death_cause'), NA,
          list('patid', c('death_cause_code', 'death_cause_type')), 'death_cause',
          cdm_tbl('med_admin'), NA,
          list('patid', 'encounterid', 'medadminid', c('medadmin_code', 'medadmin_type')),
          'med_admin',
          cdm_tbl('obs_clin'), '.obs_clin_cleanup',
          list('patid', 'encounterid', 'obsclinid', c('obsclin_code', 'obsclin_type')),
          'obs_clin',
          cdm_tbl('obs_gen'), '.obs_gen_cleanup',
          list('patid', 'encounterid', 'obsgenid', c('obsgen_code', 'obsgen_type')),
          'obs_gen',
          cdm_tbl('lds_address_history'), '.lds_address_history_cleanup',
          list('patid'), 'lds_address_history',
          cdm_tbl('immunization'), '.imm_cleanup',
          list('patid', 'immunizationid', c('vx_code', 'vx_code_type')),
          'immunization'
          )
}

#' List of impersonal tables with cleanups
#'
#' Return a tbl whose rows are tables not containing a `patid`, with
#' corresponding values providing a function that cleans up rows in that table
#' for release to study teams, a list of columns to index when the table is
#' instantiated, and column names used to join to clinical facts.  Note that the
#' table-specific cleanups are in addition to the general changes made in
#' [.clean_table_base()].
#'
#' If you wish to include additional personal tables in your extraction, you
#' may add rows to the returned tbl.  Each must have four columns:
#'
#' * `tbl_expr` - a dplyr tbl specifying the CDM table to extract
#' * `cleanup_name` - the name of a table-specific cleanup function. If it is
#'   `NA`, no table-specific cleanup is done
#' * `indexes` - a list specifying the indices to construct, as described for
#'    [dbplyr::compute()].
#' * `output_name` - the name to give the resulting mini-CDM tbl
#' * `join_on` - the name of the column in this table by which records are
#'   linked to other CDM tables
#'
#' Cf. documentation for [personal_tables()] for further discussion of
#' customising metadata to meet your needs.
#'
#' @return The list of tables
#' @md
impersonal_tables <- function() {
   tribble( ~tbl_expr, ~cleanup_name, ~indexes, ~output_name, ~join_on,
           cdm_tbl('provider'), '.provider_cleanup', list('providerid'),
           'provider', 'providerid',
           cdm_tbl('lab_history'), NA, list('labhistoryid', 'lab_loinc'),
           'lab_history', 'lab_loinc')
}


#' Generate cohort-specific personal tables
#'
#' For each of the PEDSnet CDM tables named in `metadata`, generate a
#' cohort-specific version in the results schema
#'
#' @param cohort A tbl containing the `patid`s to include in the output
#' @param metadata A tbl of tables to process (cf. [personal_tables()]).
#' @param clean Whether or not to remove DCC extensions.
#'
#' @return A list of output table names and tbl handles.
#' @md
get_personal_tables <- function(cohort, metadata = personal_tables(),
                                clean = TRUE) {
  rslt <- list()

  for (tname in pull(metadata, output_name)) {
    message('Extracting ', tname)
    tdata <- metadata %>% filter(output_name == local(tname))
    rslt[[tname]] <- pull(tdata, tbl_expr)[[1]] %>%
      semi_join(cohort, by = 'patid')

    if (clean) {
      cohort_data <- rslt[[tname]]
      cleanup <- pull(tdata, cleanup_name)
      if (!is.na(cleanup)) cohort_data <- do.call(cleanup, list(cohort_data))
      rslt[[tname]] <- .clean_table_base(cohort_data)
    }
  }
  rslt
}


#' Generate cohort-specific output tables not containing person IDs
#'
#' For each of the PEDSnet tables not containing a `patid`, gather records
#' referred to in any of the supplied personal tables via foreign keys such as a
#' visit or provider identifier.
#'
#' Note that this function only looks for foreign keys in the personal tables;
#' if the returned impersonal tables themselves have foreign keys into other
#' impersonal tables, you may need to accumulate complete results via a series
#' of calls to [get_impersonal_tables()].
#'
#' @param personal_tables A list of personal tables from which to collect the
#'   impersonal table IDs to extract.
#' @param metadata A tbl of tables to process (cf. [impersonal_tables()]).
#' @param clean Whether or not to remove DCC extensions
#' @param .cycle_limit The number of source columns to scan before
#'   [dbplyr::compute()]ing an intermediate result, to avoid creating a deeply
#'   nested SQL expression
#'
#' @return A list of output table names and tbl handles.
#' @md
get_impersonal_tables <- function(personal_tables,
                                  metadata = impersonal_tables(),
                                  clean = TRUE,
                                  .cycle_limit = 2) {
  rslt <- list()
  accum <- NA
  toggle <- 0L
  cycle <- 1L
  join_cols <- pull(metadata, join_on)
  get_output <- function(tname) {
    message('Extracting ', tname)
    tdata <- metadata %>% filter(output_name == local(tname))
    join_col <- pull(tdata, join_on)
    probe <- select(accum, any_of(join_col)) %>% distinct()
    rslt[[tname]] <- inner_join(pull(tdata, tbl_expr)[[1]], probe, by = join_col)
    if (clean) {
      impdata <- rslt[[tname]]
      cleanup <- pull(tdata, cleanup_name)
      if (!is.na(cleanup)) impdata <- do.call(cleanup, list(impdata))
      rslt[[tname]] <- .clean_table_base(impdata)
    }
  }

  for (tabl in personal_tables) {
    cols <- tbl_vars(tabl)
    links <- intersect(cols, join_cols)
    if (length(links) == 0) next;
    fills <- setdiff(join_cols, cols)
    this <- select(tabl, any_of(links)) %>% distinct()
    for (f in fills) this <- mutate(this, '{f}' := NA_character_)
    if (any(is.na(accum))) accum <- this else accum <- dplyr::union(accum, this)
    if (cycle %% .cycle_limit == 0) {
      accum <- compute_new(accum,
                           name = paste0('prepip_',
                                         letters[toggle %% 2 + 1],
                                         Sys.getpid()),
                           indexes = as.list(join_cols))
      toggle <- toggle + 1
    }
    cycle <- cycle + 1
  }
  if (cycle %% .cycle_limit == 0)
    accum <- compute_new(accum,
                         name =  paste0('prepip_',
                                        letters[toggle %% 2 + 1],
                                        Sys.getpid()),
                         indexes = as.list(join_cols))

  for (tname in pull(metadata, output_name)) {
    rslt[[tname]] <- get_output(tname)
  }
  rslt
}


#' Materialize tbls as result tables
#'
#' Given a named list of tbl expressions, attempt to instantiate each via
#' [output_tbl()] (meaning the table will be placed in the results schema and
#' may have a results tag appended).
#'
#' @param tables A list of tbls to materialize, where the name of each element
#'   is the name of the output table to create.
#' @param metadata A tbl containing a row for each table to be materialized,
#'   with the name of the table in `output_name`, and a list of columns to be
#'   indexed in `indexes`.
#'
#' @return A list of result tables
#' @md
materialize_tbls <- function(tables,
                             metadata = dplyr::union(personal_tables(),
                                                     impersonal_tables())) {

  rslt <- list()

  for (tname in names(tables)) {
    tmeta <- filter(metadata, output_name == local(tname))
    indx <- tmeta %>% pull(indexes)
    if (length(indx) > 0) indx <- indx[[1]]
    outname <- tmeta %>% pull(output_name)
    message('Instantiating ', tname)
    rslt[[tname]] <- output_tbl(tables[[tname]], name = outname, indexes = indx)
  }
  rslt
}


#' Generate "mini-cdm" from PCORnet data for a specified cohort
#'
#' Retrieve all PCORnet data for a specified cohort of patients.  For CDM tables
#' that have a `patid` ("personal tables"), all records pertaining to a person
#' in the cohort are retrieved. For CDM tables that do not have a `patid`
#' ("impersonal tables"), fact identifiers (e.g. `providerid`) from the personal
#' tables are used to retrieve relevant records.  Finally, the harvest table is
#' retrieved.
#'
#' The results are typically returned as materialized result tables.  You can
#' prevent this with `materialize = FALSE`, which may be useful if you are only
#' extracting a few tables' worth of data temporarily.  However, be aware that
#' skipping materialization may slow execution severely, since ascertaining
#' what to gather from impersonal tables will require re-evaluation
#' of queries against the main database tables.
#'
#' @param cohort The cohort tbl with `patid`.
#' @param clean Whether or not to remove DCC extensions and fields at high risk
#'   for PHI
#' @param personal A list of personal tables to extract, with metadata
#' @param impersonal A list of impersonal tables to extract, with metadata
#' @param materialize A Boolean value indicating whether the results should
#'   be materialized as database tables
#' @param .fix_names A function that takes a vector of output names and
#'   returns a vector of altered names to use for output tables
#'
#' @return A list of mini-cdm tbls with their names.
#' @md
build_mini_cdm <-
  function (cohort, clean = TRUE,
            personal = personal_tables(),
            impersonal = impersonal_tables(),
            materialize = TRUE,
            .fix_names = function(x) str_replace(x, '(.+)', 'cdm_\\1')) {

  harvest_out <- 'harvest'
  if (is.function(.fix_names)) {
    harvest_out <- .fix_names(harvest_out)
    personal <- personal %>%
      mutate(output_name = .fix_names(output_name))
    impersonal <- impersonal %>%
      mutate(output_name = .fix_names(output_name))
  }

  t_personal <- get_personal_tables(cohort = cohort,
                                    metadata = personal,
                                    clean = clean)
  if (materialize) t_personal <- materialize_tbls(t_personal, personal)


  t_impersonal <- get_impersonal_tables(personal_tables = t_personal,
                                        metadata = impersonal,
                                        clean = clean)
    if (materialize) t_impersonal <- materialize_tbls(t_impersonal, impersonal)
  maybe_more <-
    reduce(map(names(t_impersonal),
               .f = function (x) {
                 intersect(tbl_vars(t_impersonal[[x]]),
                           pull(impersonal %>% filter(output_name != x),
                                'join_on'))
               }), c)
  while (length(maybe_more) > 0) {
    thismeta <- impersonal %>% filter(join_on %in% local(maybe_more))
    trial <- get_impersonal_tables(personal_tables = t_impersonal,
                                   metadata = thismeta, clean = clean)
    pre_cts <- map(t_impersonal, .f = function (x) pull(tally(ungroup(x))))
    walk(names(t_impersonal),
         .f = function (tname) {
           if (exists(tname, where = trial))
             trial[[tname]] <- union(t_impersonal[[tname]],
                                            trial[[tname]])
         })
    if (materialize) trial <- materialize_tbls(trial, thismeta)
    post_cts <- map(trial, .f = function (x) pull(tally(ungroup(x))))
    t_impersonal[names(trial)] <- trial[names(trial)]
    maybe_more <- names(pre_cts[unlist(pre_cts) != unlist(post_cts)])
  }

  rslt <- c(t_personal, t_impersonal)
  harvest <- list(cdm_tbl('harvest'))
  if (materialize)
    harvest <- materialize_tbls(setNames(harvest, harvest_out),
                                 tribble(~output_name, ~indexes,
                                         harvest_out, NULL))

  c(rslt, harvest)
}
