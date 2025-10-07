# 02-data_preparation.R
# Data preparation pipeline: RDS formatting, comparable indicators, and weights
# Includes CE's comparable RDS/NSUM indicator specifications (2024-11-05)

# Load required libraries
library(tidyverse)
library(RDS)
library(car)
library(psych)
library(here)

# Load helper functions for nationality clustering
source(here("R", "utils", "helper_functions.R"))

# Load cleaned data
if (!exists("data_final") || !exists("data_nonzero")) {
  load(here("data", "processed", "cleaned_data.RData"))
}

# Create comparable RDS/NSUM indicators following CE's specifications
create_comparable_indicators <- function(data) {
  
  cat("Creating comparable RDS/NSUM indicators...\n")
  
  # CE's mappings from most confident to least confident:
  
  # 1. Document withholding (MOST CONFIDENT): Q70 5f2 = Q71 5f3
  # Q70: Has employer withheld travel/identity documents?
  data$zQ70 <- recode(data$q70, "4=NA")  # Set don't know to NA
  data$zQ70 <- recode(data$q70, "c(0,1,2) = 1; c(3,4)=0")  # Always/often/sometimes = 1, never = 0
  
  # Q71: Know others without access to documents? (already numeric count)
  data$document_withholding_rds <- data$zQ70
  data$document_withholding_nsum <- ifelse(data$q71 > 0, 1, 0)  # Any known others = 1
  data$document_withholding_nsum[is.na(data$q71)] <- NA
  
  # 2. Pay/debt issues (HIGH CONFIDENCE): Q39 5b4 and Q42 5b7 = Q43 5b8
  # Q39: Have to pay debt to someone who helped find work?
  data$zQ39 <- recode(data$q39, "2=NA")  # Don't know to NA
  data$zQ39 <- recode(data$q39, "0 = 1; c(1,2)=0")  # Yes = 1, No = 0
  
  # Q42: Has pay ever been withheld?
  data$zQ42 <- recode(data$q42, "4=NA")  # Don't know to NA
  data$zQ42 <- recode(data$q42, "c(0,1,2) = 1; c(3,4)=0")  # Always/often/sometimes = 1, never = 0
  
  # Q43: Know others with debt/pay problems? (already numeric count)
  data$pay_issues_rds <- ifelse(data$zQ39 == 1 | data$zQ42 == 1, 1, 0)  # Logical OR
  data$pay_issues_rds[is.na(data$zQ39) & is.na(data$zQ42)] <- NA  # NA if both missing
  data$pay_issues_nsum <- ifelse(data$q43 > 0, 1, 0)  # Any known others = 1
  data$pay_issues_nsum[is.na(data$q43)] <- NA
  
  # 3. Threats/abuse/force (HIGH CONFIDENCE): Q45 5c2 and Q47 5c4 and Q48 5c5 = Q49 5c6
  # Q45: Forced, deceived or threatened into poor conditions?
  data$zQ45 <- recode(data$q45, "2=NA")  # Prefer not to say to NA
  data$zQ45 <- recode(data$q45, "0 = 1; c(1,2)=0")  # Yes = 1, No = 0
  
  # Q47: Has employer threatened or intimidated you?
  data$zQ47 <- recode(data$q47, "4=NA")  # Don't know to NA
  data$zQ47 <- recode(data$q47, "c(0,1,2) = 1; c(3,4)=0")  # Yes/often/sometimes = 1, never = 0
  
  # Q48: Has employer verbally abused you?
  data$zQ48 <- recode(data$q48, "4=NA")  # Don't know to NA
  data$zQ48 <- recode(data$q48, "c(0,1,2) = 1; c(3,4)=0")  # Yes often/sometimes/maybe = 1, never = 0
  
  # Q49: Know others with threat/force experiences? (already numeric count)
  data$threats_abuse_rds <- ifelse(data$zQ45 == 1 | data$zQ47 == 1 | data$zQ48 == 1, 1, 0)  # Logical OR
  data$threats_abuse_rds[is.na(data$zQ45) & is.na(data$zQ47) & is.na(data$zQ48)] <- NA  # NA if all missing
  data$threats_abuse_nsum <- ifelse(data$q49 > 0, 1, 0)  # Any known others = 1
  data$threats_abuse_nsum[is.na(data$q49)] <- NA
  
  # 4. Excessive hours (LOWER CONFIDENCE): Q61 5d8 and Q62 5d9 = Q64 5d11
  # NOTE: Q64 includes annual leave which RDS questions don't cover
  # Q61: Weekly rest longer than 24 hours consecutively?
  data$zQ61 <- recode(data$q61, "4=NA")  # Don't know to NA  
  data$zQ61 <- recode(data$q61, "c(2,3) = 1; c(0,1,4)=0")  # Sometimes/never = 1 (inadequate rest), always/often = 0
  
  # Q62: Worked overtime that felt excessive?
  data$zQ62 <- recode(data$q62, "4=NA")  # Don't know to NA
  data$zQ62 <- recode(data$q62, "c(0,1,2) = 1; c(3,4)=0")  # Always/often/sometimes = 1, never = 0
  
  # Q64: Know others with labour rights issues? (includes excessive hours + annual leave)
  data$excessive_hours_rds <- ifelse(data$zQ61 == 1 | data$zQ62 == 1, 1, 0)  # Logical OR
  data$excessive_hours_rds[is.na(data$zQ61) & is.na(data$zQ62)] <- NA  # NA if both missing
  data$excessive_hours_nsum <- ifelse(data$q64 > 0, 1, 0)  # Any known others = 1
  data$excessive_hours_nsum[is.na(data$q64)] <- NA
  
  # 5. Access to help (LEAST CONFIDENT): Q78 5f10 (coded as No) = Q79 5f11
  # Q78: Do you know who might help if not properly paid/treated? (reverse coded)
  data$zQ78 <- recode(data$q78, "1 = 0; 0 = 1")  # No = 1 (vulnerable), Yes = 0
  data$Q78rev <- data$zQ78  # Reversed version to align with Q79
  
  # Q79: Know others who don't know where to go for help? (already numeric count)
  data$access_to_help_rds <- data$Q78rev  # Higher values = more vulnerable
  data$access_to_help_nsum <- ifelse(data$q79 > 0, 1, 0)  # Any known others = 1
  data$access_to_help_nsum[is.na(data$q79)] <- NA
  
  # Additional: Known network size variable (from CE's note)
  # Q13 2f: Number of domestic workers for whom you have contact details in phone
  data$known_network_size <- as.numeric(data$q13)
  
  return(data)
}

# Prepare legacy indicators for backward compatibility
prepare_legacy_indicators <- function(data) {
  
  cat("Preparing legacy indicators for backward compatibility...\n")
  
  # Legacy Q36 and Q80 indicators (remove D/K responses)
  data$zQ36 <- recode(data$q36, "5=NA")
  data$zQ36 <- recode(data$q36, "c(0,1,2,3) = 1; c(4,5)=0")
  
  data$zQ80 <- recode(data$q80, "4=NA")
  data$zQ80 <- recode(data$q80, "c(0,1) = 1; c(2,3,4,5)=0")
  
  # Risk categories
  data$sum_categories_factor <- as.factor(data$sum_categories)
  data$sum_categories_cut <- cut_interval(data$sum_categories, n = 10)
  
  return(data)
}

# Main data preparation function
prepare_data <- function() {
  
  # Start with non-zero degree data
  dd <- data_nonzero
  
  # Create comparable indicators using CE's specifications
  dd <- create_comparable_indicators(dd)
  
  # Add legacy indicators for backward compatibility
  dd <- prepare_legacy_indicators(dd)
  
  dd <- create_nationality_clusters(dd)

  # Fix recruiter.id for seeds (MA.estimates requirement)
  # All seeds should have the same recruiter.id (e.g., "0")
  cat("Standardizing seed recruiter IDs for MA.estimates compatibility...\n")
  dd <- dd %>%
    mutate(
      recruiter.id = ifelse(recruiter.id == "-1", "0", as.character(recruiter.id)),
      # Fix zero network sizes (minimum degree for recruited individuals is 1)
      known_network_size = ifelse(known_network_size == 0, 1, known_network_size)
    )

  # Remove list columns before creating RDS object (causes issues with weight functions)
  cat("Removing list columns for RDS compatibility...\n")
  list_cols <- sapply(dd, is.list)
  if (any(list_cols)) {
    cat("  Removing:", paste(names(dd)[list_cols], collapse = ", "), "\n")
    dd <- dd[, !list_cols]
  }

  # Create RDS data frame object
  cat("Creating RDS data frame...\n")
  rd.dd <- as.rds.data.frame(dd,
                             id="id",
                             recruiter.id="recruiter.id",
                             network.size="known_network_size",
                             max.coupons = 5,
                             check.valid = FALSE)

  # Calculate RDS weights using STANDARD RDS package functions
  cat("Calculating RDS and population weights using standard RDS package...\n")

  # RDS-I weights for comparable indicators (outcome-specific)
  cat("  Calculating RDS-I weights (outcome-specific)...\n")
  dd$wt.RDS1_document_withholding <- rds.I.weights(rd.dd, "document_withholding_rds")
  dd$wt.RDS1_pay_issues <- rds.I.weights(rd.dd, "pay_issues_rds")
  dd$wt.RDS1_threats_abuse <- rds.I.weights(rd.dd, "threats_abuse_rds")
  dd$wt.RDS1_excessive_hours <- rds.I.weights(rd.dd, "excessive_hours_rds")
  dd$wt.RDS1_access_to_help <- rds.I.weights(rd.dd, "access_to_help_rds")
  dd$wt.RDS1_zQ36 <- rds.I.weights(rd.dd, "zQ36")
  dd$wt.RDS1_zQ80 <- rds.I.weights(rd.dd, "zQ80")
  dd$wt.RDS1_sum_categories_factor <- rds.I.weights(rd.dd, "sum_categories_factor")

  # RDS-II (VH) weights using standard RDS package
  # Note: Using a reference outcome variable to get weights (weights are based on network structure, not outcome)
  cat("  Calculating RDS-II/VH weights (population-specific)...\n")
  reference_outcome <- "document_withholding_rds"  # Any binary outcome works

  rds_ii_980k <- RDS.II.estimates(rd.dd, reference_outcome, N = 980000)
  dd$wt.vh_980k <- rds_ii_980k$weights / sum(rds_ii_980k$weights)  # Normalize to sum=1

  rds_ii_100k <- RDS.II.estimates(rd.dd, reference_outcome, N = 100000)
  dd$wt.vh_100k <- rds_ii_100k$weights / sum(rds_ii_100k$weights)

  rds_ii_050k <- RDS.II.estimates(rd.dd, reference_outcome, N = 50000)
  dd$wt.vh_050k <- rds_ii_050k$weights / sum(rds_ii_050k$weights)

  rds_ii_1740k <- RDS.II.estimates(rd.dd, reference_outcome, N = 1740000)
  dd$wt.vh_1740k <- rds_ii_1740k$weights / sum(rds_ii_1740k$weights)

  # RDS-SS weights using standard RDS package
  cat("  Calculating RDS-SS weights (population-specific)...\n")

  rds_ss_980k <- RDS.SS.estimates(rd.dd, reference_outcome, N = 980000)
  dd$wt.SS_980k <- rds_ss_980k$weights / sum(rds_ss_980k$weights)  # Normalize to sum=1

  rds_ss_100k <- RDS.SS.estimates(rd.dd, reference_outcome, N = 100000)
  dd$wt.SS_100k <- rds_ss_100k$weights / sum(rds_ss_100k$weights)

  rds_ss_050k <- RDS.SS.estimates(rd.dd, reference_outcome, N = 50000)
  dd$wt.SS_050k <- rds_ss_050k$weights / sum(rds_ss_050k$weights)

  rds_ss_1740k <- RDS.SS.estimates(rd.dd, reference_outcome, N = 1740000)
  dd$wt.SS_1740k <- rds_ss_1740k$weights / sum(rds_ss_1740k$weights)

  cat("  Weight calculation complete. All weights normalized to sum=1 for NSUM use.\n")
  
  # Save prepared data
  cat("Saving prepared data...\n")
  save(dd, rd.dd, file = here("data", "processed", "prepared_data.RData"))

  # Export CSV for external use (list columns already removed)
  write.csv(dd, here("data", "processed", "prepared_data.csv"), row.names = FALSE)
  
  cat("Data preparation completed successfully!\n")
  cat("- Prepared data:", nrow(dd), "observations\n")
  cat("- RDS data object created\n")
  cat("- Comparable indicators created following CE's specifications\n")
  cat("- Multiple population size weights calculated\n")
  cat("- Files saved to data/processed/\n")
  
  return(list(
    dd = dd,
    rd.dd = rd.dd
  ))
}

# Execute data preparation if running this script directly
if (!exists("skip_execution")) {
  prepared_data <- prepare_data()
  # Make dd and rd.dd available in global environment
  dd <<- prepared_data$dd
  rd.dd <<- prepared_data$rd.dd
}
