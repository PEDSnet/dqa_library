# PEDSnet Network Data Quality Checks

This repository contains the code that is used to generate the raw output of the data quality analyses that are displayed on the PEDSnet Data Quality Dashboard. For each of the 8 checks, there is a `cohort_*.R` and `*_execute.R` file that make up the check. The `cohort` file contains the function, while the `execute` file contains the inputs used in the function. The functions are all executed in [driver.R](code/driver.R).

For information about each individual execution of the 8 checks, please see [dqa_check_descriptions.xlsx](dqa_check_descriptions.xlsx). This file has plain language descriptions of each check execution, organized based on the check type, the check domain, and the check application. This is similar to how the information in the REDCap DQA Issues form is formatted, so it should hopefully simplify the process of connecting a check in the form to its description in the file.

## Setup
In order to prepare your environment to execute this step, open [setup/setup.R](setup/setup.R). This file is where the connection to your local database is established and where internal configurations are set that are used throughout the process.

You will first need to set the name of the institution for which you are executing these checks. Then, you will set your database configuration information in the parameters of the `initialize_session` function. Documentation for those parameters can be found [here](setup/argos_wrapper.R). Your connection information can be read in from an external JSON file or the connection can be established within the R session using a package like DBI.

The next configurations are primarily related to previous data, which is needed to execute the Data Cycle Changes check. If you plan to execute this check, you will need to set an additional database connection and set EITHER `cdm_schema_prev` or `results_schema_prev`. Then, you should set both `previous_version` and `current_version` to indicate each database version. If you do NOT plan to execute this check, just set `current_version`.

## Data Cycle Changes

#### Files:

-   [cohort_dc.R](code/cohort_dc.R)
-   [dc_execute.R](code/dc_execute.R)

#### How the function works:

This function loops through a list of tables from the previous data cycle and compares the count of rows and patients to the same table in the current data cycle. For any table without a person_id field, only row count is computed while patient count is defaulted to 0.

## Vocabulary/Valueset Conformance

#### Files

-   [cohort_vc_vs.R](code/cohort_vc_vs.R)
-   [vc_vs_execute.R](code/vc_vs_execute.R)

#### How the functions work:

The Valueset Conformance function [check_vs](code/cohort_vc_vs.R#L2-L72) compares the values in the specified field against a standard valueset (found in the [specs](specs) folder). The resulting table will have a list of every time the valueset was violated for each field.

The Vocabulary Conformance function [check_vc](code/cohort_vc_vs.R#L77-L118) joins the specified field with the vocabulary table and checks that all values comply with a list of acceptable vocabularies. The resulting table will have a list of each unique occurrence of a vocabulary violation.

The list of fields this check examines, the names of the valueset files, and the lists of acceptable vocabularies used for each field are found in [vc_vs_execute.R](code/vc_vs_execute.R).

## Unmapped Concepts

#### Files

-   [cohort_uc.R](code/cohort_uc.R)
-   [uc_execute.R](code/uc_execute.R)

#### How the function works:

This function loops through a list of concept fields and looks for rows where the concept is mapped to `44814650`, `0`, `44814653`, or `44814649`. The resulting table will contain a list of the source_values and counts associated with those concept_ids to demonstrate where there are significant gaps in mapping. The fields examined as part of this check can be found in [uc_execute.R](code/uc_execute.R)

## Person Facts/Records

#### Files

-   [cohort_pf_visits.R](code/cohort_pf_visits.R)
-   [pf_visits_execute.R](code/pf_visits_execute.R)

#### How the function works:

This function takes all of the distinct patients and visits in the `visit_occurrence` table and measures how many of those patients & visits are associated with a certain fact type. The function is executed 4 times, once for each visit type: all visits, inpatient visits (9201, 2000000048), outpatient visits (9202, 581399), and emergency visits (9203, 2000000048). The facts that are examined as part of this check can be found in [pf_visits_execute.R](code/pf_visits_execute.R)

## Best Mapped Concepts

#### Files

- [cohort_bmc_gen.R](code/cohort_bmc_gen.R)
- [bmc_gen_execute.R](code/bmc_gen_execute.R)

#### How the function works:

This function looks at the distribution of mappings for a specified field, and as part of a later processing step, the proportion of preferred/ideal values are identified and compared to the proportion of non-ideal values. The resulting table of this check will have the proportions of all existing mapped values, and the ideal values that we are using on the processing end can be viewed on the dashboard. The list of fields examined is found in [bmc_gen_execute.R](code/bmc_gen_execute.R)

## Facts Over Time

#### Files

- [cohort_fot.R](code/cohort_fot.R)
- [fot_execute.R](code/fot_execute.R)

#### How the function works:

This function loops through a list of specified tables and counts the number of rows, distinct visits, and distinct patients in the specified table for each month from 01-01-2009 to the month of data submission. The list of tables used in this check is in [fot_execute.R](code/fot_execute.R)

## Domain Concordance

#### Files

- [cohort_dcon.R](code/cohort_dcon.R)
- [dcon_execute.R](code/dcon_execute.R)

#### How the functions work:

There are two Domain Concordance functions. [check_dcon](code/cohort_dcon.R#L155-L234) can look at the concordance between two cohorts at both the patient and visit level, depending on the check_string argument. If the check_string is `dcon_visits`, the resulting table will show the amount of visits in cohort A, cohort B, and both cohorts. Otherwise, it will show the same results at the patient level.

[check_dcon_overtime](code/cohort_dcon.R#L237-L315) looks at how each cohort changes in size throughout each year in a list (2009-2023). The combined cohort is not applied in this portion of the function.

The cohorts used for each check can be found in [dcon_execute.R](code/dcon_execute.R). There is one list of inputs for patient level checks, and a separate list of inputs for visit level checks.

## Facts with Associated Visit ID

#### Files

- [cohort_mf_visitid.R](code/cohort_mf_visitid.R)
- [mf_visitid_execute.R](code/mf_visitid_execute.R)

#### How the function works:

This function looks at several tables (found in [mf_visitid_execute.R](code/mf_visitid_execute.R)) to see if all the visit_occurrence_ids exist both in that table and the visit_occurrence table. 

## Expected Concepts Present

#### Files

- [cohort_ecp.R](code/cohort_ecp.R)
- [ecp_execute.R](code/ecp_execute.R)
- [ecp_concepts.csv](specs/ecp_concepts.csv)

#### How the function works:

This function looks at a population of patients with evidence of a drug, procedure, AND lab and checks to see if these patients have evidence of the clinical concepts listed in [ecp_concepts.csv](specs/ecp_concepts.csv). Each concept_group in the CSV file represents a check application, with one row per code in the concept_group. Currently, this check is only being applied to lab values, but it can be applied to other domains as long as the user provides the correct CDM table in the [ecp_execute.R](code/ecp_execute.R) file.
