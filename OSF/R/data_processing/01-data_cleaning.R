# 01-data_cleaning.R (OSF replication version)
#
# In the original pipeline this script reads the raw survey CSV
# (data/raw/UpdateSelimRiskIndex-sum_cat.csv) and applies cleaning to
# produce data/processed/cleaned_data.RData and the CSV exports.
#
# The raw CSV is NOT included in the OSF replication package because it
# contains PII (respondent names, emails, phone numbers, RDS-referral
# phone numbers, free-text "other" responses with identifying detail).
# See data/raw/README.md for details.
#
# In this OSF-compatible version, the script loads the already-anonymised
# data/processed/data_full.csv (which is the equivalent of running the
# raw-data pipeline through the anonymisation step) and writes the
# cleaned_data.RData cache that 02-data_preparation.R expects.

library(here)
library(dplyr)

cat("01-data_cleaning.R (OSF version)\n")
cat("Loading anonymised data/processed/data_full.csv...\n")
data_final <- read.csv(here("data", "processed", "data_full.csv"),
                        stringsAsFactors = FALSE)
data_nonzero <- read.csv(here("data", "processed", "data_nonzero.csv"),
                          stringsAsFactors = FALSE)
df_long <- read.csv(here("data", "processed", "long_format_data.csv"),
                     stringsAsFactors = FALSE)

cat(sprintf("  data_final:   %d rows x %d cols\n", nrow(data_final), ncol(data_final)))
cat(sprintf("  data_nonzero: %d rows x %d cols\n", nrow(data_nonzero), ncol(data_nonzero)))
cat(sprintf("  df_long:      %d rows x %d cols\n", nrow(df_long), ncol(df_long)))

# Drop duplicate sum_categories column if present (it duplicated composite_risk
# in the raw export)
if ("composite_risk" %in% names(data_final) && "sum_categories" %in% names(data_final)) {
  if (isTRUE(all.equal(data_final$composite_risk, data_final$sum_categories,
                       check.attributes = FALSE))) {
    data_final$sum_categories <- NULL
    cat("Dropped duplicate column sum_categories (identical to composite_risk).\n")
  }
}

# Save the cleaned data cache that 02-data_preparation.R expects
out_path <- here("data", "processed", "cleaned_data.RData")
save(data_final, data_nonzero, df_long, file = out_path)
cat(sprintf("\nSaved: %s\n", out_path))
cat("\nRun 02-data_preparation.R next to rebuild the RDS-package rd.dd object.\n")
