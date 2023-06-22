suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(stringr))
suppressPackageStartupMessages(require(purrr))

#' Removing sensitive information from datasets
#'
#' A number of analytic methods, such as propensity score computation or mutiple
#' imputation, may make use of variables not central to a study's independent or
#' dependent variable definitions. When a PEDSnet dataset contains covariate
#' data to support analysis, it is routine practice to redact data that deals
#' with topics that receive special consideration due to potential social
#' impact.  The removal of these data is called _desensitization_.  By policy,
#' the PEDSnet DCC maintains a set of codes used for standard desensitization,
#' that is, the process applied to all datasets where the sensitive codes are
#' not required for the planned analyses (and approved in the PEDSnet study
#' designation process).
#'
#' Sensitive codes are grouped together into codesets using two categories.  The
#' codeset _group_ reflects a clinical topic area; the [std_sens_groups()]
#' function provides the list of current standard groups.  For example, a
#' codeset whose group designation is `hiv` will relate to care for HIV
#' infection.  The codeset _domain_ reflects the type of healthcare utilization
#' to which the codeset typically applies; the [std_sens_domains()] function
#' provides a list of current standard domains.  For example, a codeset with a
#' domain of 'drug' would be expected to apply to medication codes.  By
#' convention, the name of a codeset contains both its group and domain labels,
#' though there is not a required ordering within the name.
#'
#' The desensitization process typically involves several steps:
#'
#' 1. The necessary codesets are retrieved - see [sens_files()] for retrieving
#'    codesets from files in the standard frameworks, and [sens_tables()] for
#'    retrieving codesets from the standard location in DCC databases.
#' 2. Any necessary edits to the codesets are made to reflect the analytic
#'    requirements of the study.
#' 3. Rows containing the codes to be redacted are removed from data.  See
#'    [pedsnet_std_desensitization()] for information about the sensitive
#'    codeset domains and the corresponding columns in fact tables that are part
#'    of the standard desnesitization process for PEDSnet.
#' 4. If redacted records may be referenced from out data, those data or the
#'    foreign keys pointing to redacted records may need to be modified as
#'    well.  This is not the case for any of the standard PEDSnet
#'    desensitization codesets.
#'
#' For data in the PEDSmet or OMOP/OHDSI CDMs, you will tpyically call the
#' convenience functions [desensitize_pedsnet_tbl()] for a single table or
#' [desensitize_pedsnet_tbls()] for a complete dataset, which spare you the need
#' to replicate overhsad that doesn't vary often.
#'
#' @return This function exists for its documentation.
#' @md
desensitize.vignette <- function() {
  'See help on this topic for a summary of the desensitization process'
}

#' List the known exclusion codeset groups
#'
#' The group attribute of an exclusion codeset, designated by a word
#' in its name, indicates the clinical topic area that the codeset
#' covers.
#'
#' Note that the returned list is of groups that have defined meaning
#' in the current desensitization scheme.  This does not mean that a
#' given set of codesets will cover each group, only that if a
#' codeset's name contains a group name, the codeset will be
#' considered to apply to that group.
#'
#' @return A list whose names are the known group names and whose
#'   values are brief descriptions of each group.
#' @md
std_sens_groups <- function() {
  list('hiv' = 'Codes related to HIV diagnosis and care',
       'preg' = 'Codes related to pregnancy and reproductive health',
       'bh' = 'Codes related to behavioral health')
}


#' List the known exclusion codeset domains
#'
#' The domain attribute of an exclusion codeset, designated by a word
#' in its name, indicates the type of data that the codeset covers.
#'
#' Note that the returned list is of domains that have defined meaning
#' in the current desensitization scheme.  This does not mean that a
#' given set of codesets will cover each domain, only that if a
#' codeset's name contains a domain name, the codeset will be
#' considered to apply to that domain.
#'
#' @return A list whose names are the known domain names and whose
#'   values are brief descriptions of each domain.
#' @md
std_sens_domains <- function() {
  list('cond' = 'Diagnoses (including problem list entries)',
       'drugs' = 'Medications',
       'testproc' = 'Procedures and diagnostic tests (including results)')
}


#' Return a list of exclusion codesets from CSV files
#'
#' Returns a list of tbls that hold codesets to be excluded from result data,
#' taken from flatfile codesets in the specification files directory.
#'
#' These codesets are identified by two metadata tags, group and domain, which
#' must appear in the name of the file.  See [std_sens_groups()] and
#' [std_sens_domains()] for lists of standard values.
#'
#' The concept IDs to be redacted must be in the `concept_id` column.
#'
#' @param groups A character vector containing the names of groups to
#'   be retrieved.  By default, all groups found will be used.
#' @param domains A character vector containing the names of domains
#'   of interest. By default, all domains found will be used.
#' @param name_filter A regular expression used to filter table names
#'   (e.g. '^exclude_'), separate from `group` and `domain`.  Defaults to NA.
#' @param spec_dir The location in which to look for codesets.  Defaults to the
#'   specifications directory for this request.
#' @param ... Additional arguments to be passed to [load_codeset()] when loading
#'   the codeset files.  These are typically used if the codeset file is not
#'   laid out in the default structure for [load_codeset()].
#'
#' @return A list containing the tbls, with the table names as the names of the
#'   list elements
#' @md
sens_files <- function(groups = NA, domains = NA,
                       name_filter = NA,
                       spec_dir = file.path(config('base_dir'),
                                            config('subdirs')$spec_dir),
                       ...) {
  filenames <-
    list.files(spec_dir, pattern = '\\.csv$', full.names = TRUE, ignore.case = TRUE)
  if (!is.na(name_filter)) filenames <- str_subset(filenames, name_filter)
  if (!any(is.na(groups)))
    filenames <-
      unique(unlist(lapply(groups,
                           function (x) str_subset(filenames, x))))
  if (!any(is.na(domains)))
    filenames <-
      unique(unlist(lapply(domains,
                           function (x) str_subset(filenames, x))))
  tabnames <- str_remove(basename(filenames),
                            regex('\\.csv$', ignore_case = TRUE))
  rslt <- map2(filenames, tabnames,
               function(f, t) load_codeset(f, table_name = t,
                                           full_path = TRUE, ...))
  names(rslt) <- tabnames
  rslt
}

#' Return a list of exclusion codesets from database tables
#'
#' Returns a list of tbls that hold codesets to be excluded from result data,
#' taken from tables in the specified database.
#'
#' These codesets are identified by two metadata tags, group and domain, which
#' must appear in the name of the table.  See [std_sens_groups()] and
#' [std_sens_domains()] for lists of standard values.
#'
#' The concept IDs to be redacted must be in the `concept_id` column.
#'
#' @param db The db in which to search.
#' @param groups A character vector containing the names of groups to
#'   be retrieved.  By default, all groups found will be used.
#' @param domains A character vector containing the names of domains
#'   of interest. By default, all domains found will be used.
#' @param schema The name of the schema in which to look for exclusion
#'   tables.  Defaults to `sensitive_codesets`.
#' @param name_filter A regular expression used to filter table names
#'   (e.g. '^exclude_'), separate from `group` and `domain`.  Defaults to NA.
#'
#' @return A list containing the tbls, with the table names as the names of the
#'   list elements
#' @md
sens_tables <- function(db, groups = NA, domains = NA,
                        schema = 'sensitive_codesets',
                        name_filter = NA) {
  tabnames <-
    tbl(db, dbplyr::in_schema('information_schema', 'tables'))
  if (!is.na(schema))
    tabnames <- filter(tabnames, table_schema == schema)
  tabnames <- pull(tabnames, table_name)
  if (!is.na(name_filter)) tabnames <- str_subset(tabnames, name_filter)
  if (!any(is.na(groups)))
    tabnames <-
      unique(unlist(lapply(groups,
                           function (x) str_subset(tabnames, x))))
  if (!any(is.na(domains)))
    tabnames <-
      unique(unlist(lapply(domains,
                           function (x) str_subset(tabnames, x))))
  if (!is.na(schema)) {
    rslt <- lapply(tabnames, function (x) dbplyr::in_schema(schema, x))
  }
  else { rslt <- tabnames }
  rslt <- lapply(rslt, function (x) tbl(db, x))
  names(rslt) <- tabnames
  rslt
}

#' Remove sensitive codes from a tbl
#'
#' Removes all rows from a tbl where the value in a column of interest
#' matches one of the codes in a sensitive codeset
#'
#' @param data The tbl containing clinical data to be screened
#' @param codes A list of tbls containing sensitive codesets; the values in the
#'   `concept_id` column are the codes that will trigger removal. If a codeset
#'   is large, consider indexing on this column.
#' @param cols A vector containing the names of columns to scan for sensitive
#'   codes.  The default, all columns with names ending in `concept_id`, will
#'   generally catch all records of interest, but may be inefficient if the data
#'   tbl is large and contains many metadata columns using "safe" concepts.
#' @param sens_code_col The name of the column in codes that contains the
#'   sensitive codes to be redacted.
#' @md
desensitize_tbl <- function(data, codes,
                            cols = str_subset(tbl_vars(data),
                                              '_concept_id$'),
                            sens_code_col = 'concept_id') {
  if (length(codes) > 1) {
    targ <- reduce(codes, dplyr::union) %>%
      compute(temporary = TRUE, indexes = list('concept_id'))
  }
  else { targ <- codes[[1]] }
  cid <- sens_code_col
  names(cid)  <- cols[1]
  rslt <- anti_join(data, targ, by = cid)
  if (length(cols) > 1) {
    for (i in 2:length(cols)) {
      names(cid) <- cols[i]
      rslt <- anti_join(rslt, targ, by = cid)
    }
  }
  rslt
}

#' Standard desensitization steps for PEDSnet CDM
#' @md
#'
#' Returns a data structure describing the standard desensitization
#' steps for PEDSnet CDM tables.  This is a nested list, in which the
#' names of the top-level elements are the PEDSnet table names.  The
#' values are themselves lists, with the element names specifying
#' data domains, and the values being a list of columns to which
#' sensitive coeesets in that domain should be applied.
#'
#' Note that source_concept_id fields are included in the standard
#' mappings for completeness, but in pratice, since the sensitive
#' codesets contain standard concepts, screening using the standard
#' concept_id fields may be adequate in many cases.
#'
#' @param version The PEDSnet CDM version for which to retrieve the
#'   specifications, as a character vector.  The string `current` returns the
#'   specifications for the currnt version of the CDM (v3.6 as of this writing).
#' @return The list of desensitization steps
pedsnet_std_desensitization <- function(version = 'current') {
  list('condition_occurrence' = list('cond' =
                                       list('condition_concept_id',
                                            'condition_source_concept_id')),
       'death' = list('cond' = list('cause_concept_id')),
       'device_exposure' = list('testproc' = list('device_concept_id',
                                                  'device_source_concept_id'),
                                'drugs' = list('device_concept_id',
                                               'device_source_concept_id')),
       'drug_exposure' = list('drugs' = list('drug_concept_id',
                                             'drug_source_concept_id'),
                              'testproc' = list('drug_concept_id',
                                                'drug_source_concept_id')),
       'measurement' = list('testproc' =
                              list('measurement_concept_id',
                                   'measurement_source_concept_id')),
       'measurement_organism' = list('testproc' = list('organism_concept_id')),
       'procedure_occurrence' = list('testproc' =
                                       list('procedure_concept_id',
                                            'procedure_source_concept_id')),
       'observation' = list('cond' = list('observation_concept_id',
                                          'observation_source_concept_id'),
                            'drugs' = list('observation_concept_id',
                                           'obnservation_source_concept_id'),
                            'testproc' = list('observation_concept_id',
                                              'observation_source_concept_id'))
       )
}

#' Desensitize a PEDSnet CDM table
#'
#' Given a tbl following the structure of a standard PEDSnet CDM
#' table, remove records with sensitive codes.
#'
#' While you can provide your own codesets (useful in the case that you need to
#' modify standard codesets to account for study requirements), the defaults
#' save you the need to collect codesets.
#'
#' The desensitization is controlled by a number of parameters.  The `groups`
#' and `domains` settings specify which codesets to use, so you can pass a
#' multi-puropose list (or take the standard default) across multiple tables.
#' Similarly, the `rule` parameter specifies where in the target tbl to look for
#' the sensitive codes; see [pedsnet_std_desensitization()] for the PEDSnet CDM
#' standard rules, and the structure of a rule, if you want to create your own.
#' Finally, the `stringency` parameter defines how the rules are applied, so you
#' can adapt to different levels of redaction without having to redefine rules
#' based on table structure.
#'
#' @param data The tbl containing the data to be desensitized
#' @param groups A character vector containing the names of groups to
#'   be retrieved.
#' @param domains A character vector containing the names of domains
#'   of interest.
#' @param stringency A string specifying how aggressively to
#'   desensitize.  Three values are understood:
#'   * `full` - All sensitive codesets are used to screen every
#'     concept_id column. This is intended to limit potential misses,
#'     but can be very time-consuming.
#'   * 'std' - Perform screening based on `rules`.
#'   * 'rapid' - Like `std`, but skip source_concept_id columns, since
#'     if the ETL mappings were accurate, these will not remove any
#'     additional data when using the standard sensitive codesets.
#' @param name The name of the CDM table whose rules should be followed.  Note
#'   that tha default of `dbplyr::remote_name(data)`, is a best guess but often
#'   not useful: it will work only if `data` points to a named database table
#'   rather than a lazy query, and only if that table matches the (usually
#'   standard CDM) table name in `rule`.
#' @param rule The set of PEDSnet CDM rules to use.
#' @param sense_codesets A list of sensitive codesets. Provided codesets must be
#'   in the same data source as `data`.  Note that if this list is provided
#'   explicitly, then `groups` and `domains` are not used to filter the
#'   codesets, though `domaains` is still used to decide what rules apply.
#' @param sens_code_col The name of the column in each sensitive codeset that
#'   contains the sensitive codes to be redacted.
#'
#' @return The desensitized tbl
#' @md
desensitize_pedsnet_tbl <- function(data,
                                    groups = names(std_sens_groups()),
                                    domains = names(std_sens_domains()),
                                    stringency = 'rapid',
                                    name = dbplyr::remote_name(data),
                                    rule = pedsnet_std_desensitization()[[name]],
                                    sens_codesets =
                                       sens_tables(db = data$src,
                                                  groups = groups,
                                                  domains = domains),
                                    sens_code_col = 'concept_id') {
  rslt <- data
  if (stringency == 'full') {
    d_cols <- str_subset(tbl_vars(data), '_concept_id$')
    d_codes <- map(sens_codesets, function(x) select(x, concept_id))
    rslt <- desensitize_tbl(data = rslt, codes = d_codes, cols = d_cols)
  }
  else {
    for (d in domains) {
      if (any(names(rule) == d)) {
        d_cols <- unlist(rule[[d]])
        if (stringency == 'rapid')
          d_cols <- str_subset(d_cols, '_source_concept_id$', negate = TRUE)
        d_codes <- sens_codesets[str_which(names(sens_codesets), d)]
        # We do this separately per rule to allow for the potential that a given
        # code may be redactable for only one domain, though it's hard to think
        # that would occur with any frequency
        rslt <- desensitize_tbl(data = rslt, codes = d_codes, cols = d_cols)
      }
    }
  }
  rslt
}

#' Desensitize known PEDSnet tables
#'
#' Given a list of PEDSnet CDM tables, desensitize each using the
#' standard rules for that table.
#'
#' @param tables A list containing the tables; the names of the list
#'   elements should be the canonical CDM table names to use in rule
#'   selection.
#' @param groups A character vector containing the names of groups to
#'   be retrieved.
#' @param stringency A string specifying how aggressively to
#'   desensitize; see [desensitize_pedsnet_tbl()].
#' @param rules The set of PEDSnet CDM rules to use.  Defaults to the
#'   current output of [pedsnet_std_desensitization()]
#' @param sense_codesets A list of sensitive codesets.  Defaults to
#'   [sens_tables()], using the database containing `data` and the
#'   value of `groups`.  Conversely, if a value for this parameter is
#'   provided, then the value of `groups`is ignored for codeset
#'   selection.  Provided codesets must be in the same data source as
#'   `data`.
#' @param sens_code_col The name of the column in each sensitive codeset that
#'   contains the sensitive codes to be redacted.
#'
#' @return A list of the desensitized tbls
#' @md
desensitize_pedsnet_tbls <- function(tables,
                                    groups = NA,
                                    stringency = 'rapid',
                                    rules = pedsnet_std_desensitization(),
                                    sens_codesets =
                                      sens_tables(db = tables[[1]]$src,
                                                  groups = groups),
                                    sens_code_col = 'concept_id') {
  # First, precompute codesets for domains we'll actually use
  domains_present <- std_sens_domains()
  domains_present <-
    domains_present[vapply(names(domains_present),
                           function (x) any(grepl(x, names(sens_codesets))),
                           logical(1))]
  for (dn in names(domains_present)) {
    cs <- sens_codesets[str_detect(names(sens_codesets), dn)]
    if (length(cs) > 1) {
      domains_present[[dn]] <- reduce(cs, dplyr::union) %>%
        compute(temporary = TRUE, indexes = list('concept_id'))

    }
    else { domains_present[[dn]] <- cs[[1]] }
  }

  # Now, iterate over the tables
  rslt <- list()
  for (tn in names(tables)) {
    if (exists(tn, rules)) {
      rslt[[tn]] <-
        desensitize_pedsnet_tbl(tables[[tn]],
                                groups = groups,
                                domains = names(domains_present),
                                stringency = stringency,
                                name = tn,
                                rule = rules[[tn]],
                                sens_codesets = domains_present)
    }
    else { rslt[[tn]] <- tables[[tn]] }
  }
  rslt
}
