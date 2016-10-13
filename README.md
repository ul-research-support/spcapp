# spcapp for Healthcare Epidemiology

## Overview
The spcapp is a shiny package dsigned to run the RStudio server. Like any shiny application, it can also be ran natively through RStudio.

## REDCap database
The REDCap database is structured so that variable names are used by the app.

#### Variables
R is case-sensitive so we recommend using the following variable names in the database (with matching case):

1. `date` for date
2. `hosp_id_xxxxxxx` with the hospital name as "xxxxxxx"
3. `xxxxxxx_num` and `xxxxxxx_denom` as your items of interest (num = numerator, ie. number of cases; denom = denominator, ie. number of patient/device days)

Other things to note are that variable labels are very strict--they are used to populate the user control widgets. For the `xxxxxxx_num` label, we recommend the following format:

`Number of xxxxxxx Cases`

It is important to note that "xxxxxxx" will be what is populated in the pull-down menu for item of interest--avoid using punctuation and keep capitalization in mind. For `xxxxxxx_denom` we reccomend the following format:

`Number of Patient Days`

For similar reasons.
