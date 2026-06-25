# Raw data — NOT INCLUDED

The script `R/data_processing/01-data_cleaning.R` originally reads from
`data/raw/UpdateSelimRiskIndex-sum_cat.csv`, which contains PII (names,
emails, phone numbers) and is not redistributable.

For this OSF package, the anonymised output of 01-data_cleaning.R
(equivalent to running it on the raw data and stripping PII columns) is
provided directly in `data/processed/`:

  - `data_full.csv`         all 97 respondents, anonymised
  - `data_nonzero.csv`      85 respondents with non-zero network degree
  - `long_format_data.csv`  long-format alter-report data
  - `prepared_data.csv`     85 respondents with RDS weights computed

Replication therefore SKIPS script 01 and starts from script 02, which
loads `cleaned_data.RData` — see the modified 02 script in the parallel
folder for the OSF-compatible loader.

The original raw data is retained on the authors' institutional storage
and is available to the editor on confidential request for verification.
