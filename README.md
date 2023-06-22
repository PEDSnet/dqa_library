# PEDSnet Network Data Quality Checks

This repository contains the code that is used to generate the raw output of the data quality analyses that are displayed on the PEDSnet Data Quality Dashboard. For each of the 8 checks, there is a `cohort_*.R` and `*_execute.R` file that make up the check. The `cohort` file contains the function, while the `execute` file contains the inputs used in the function. The functions are all executed in `driver.R`.

## Data Cycle Changes

#### Files:

-   `cohort_dc.R`
-   `dc_execute.R`

#### How the function works:

This function loops through a list of tables from the previous data cycle and compares the count of rows and patients to the same table in the current data cycle. There are 3 tables (care_site specialty, provider specialty, and specialty) that do not have a person_id field, so specialty_concept_id is used instead to count the number of distinct specialties in place of the patient count. The complete list of tables can be found in `dc_execute.R`.

## Vocabulary/Valueset Conformance

#### Files

-   `cohort_vc_vs.R`
-   `vc_vs_execute.R`

#### How the functions work:

The Valueset Conformance function `check_vs.R` compares the values in the specified field against a standard valueset (found in the `specs` folder). The resulting table will have a list of every time the valueset was violated for each field.

The Vocabulary Conformance function `check_vc.R` joins the specified field with the vocabulary table and checks that all values comply with a list of acceptable vocabularies. The resulting table will have a list of each unique occurrence of a vocabulary violation.

The list of fields this check examines, the names of the valueset files, and the lists of acceptable vocabularies used for each field are found in `vc_vs_execute.R`.

## Unmapped Concepts

#### Files

-   `cohort_uc.R`
-   `uc_execute.R`

#### How the function works:

This function loops through a list of concept fields and looks for rows where the concept is mapped to `44814650`, `0`, `44814653`, or `44814649`. The resulting table will contain a list of the source_values and counts associated with those concept_ids to demonstrate where there are significant gaps in mapping. The fields examined as part of this check can be found in `uc_execute.R`

## Person Facts/Records

#### Files

-   `cohort_pf_visits.R`
-   `pf_visits_execute.R`

#### How the function works:

This function takes all of the distinct patients and visits in the `visit_occurrence` table and measures how many of those patients & visits are associated with a certain fact type. The function is executed 4 times, once for each visit type: all visits, inpatient visits (9201, 2000000048), outpatient visits (9202, 581399), and emergency visits (9203, 2000000048). The facts that are examined as part of this check can be found in `pf_visits_execute.R`

## Best Mapped Concepts

#### Files

- `cohort_bmc_gen.R`
- `bmc_gen_execute.R`

#### How the function works:

This function looks at the distribution of mappings for a specified field, and as part of a later processing step, the proportion of preferred/ideal values are identified and compared to the proportion of non-ideal values. The resulting table of this check will have the proportions of all existing mapped values, and the ideal values that we are using on the processing end can be viewed on the dashboard. The list of fields examined is found in `bmc_gen_execute.R`

## Facts Over Time

#### Files

- `cohort_fot.R`
- `fot_execute.R`

#### How the function works:

This function loops through a list of specified tables and counts the number of rows, distinct visits, and distinct patients in the specified table for each month from 01-01-2009 to the month of data submission. The list of tables used in this check is in `fot_execute.R`

## Domain Concordance

#### Files

- `cohort_dcon.R`
- `dcon_execute.R`

#### How the functions work:

There are two Domain Concordance functions. `check_dcon` can look at the concordance between two cohorts at both the patient and visit level, depending on the check_string argument. If the check_string is `dcon_visits`, the resulting table will show the amount of visits in cohort A, cohort B, and both cohorts. Otherwise, it will show the same results at the patient level.

`check_dcon_overtime` looks at how each cohort changes in size throughout each year in a list (2009-2023). The combined cohort is not applied in this portion of the function.

The cohorts used for each check can be found in `dcon_execute.R`. There is one list of inputs for patient level checks, and a separate list of inputs for visit level checks.

## Facts with Associated Visit ID

#### Files

- `cohort_mf_visitid.R`
- `mf_visitid_execute.R`

#### How the function works:

This function looks at several tables (found in `mf_visitid_execute.R`) to see if all the visit_occurrence_ids exist both in that table and the visit_occurrence table. 
