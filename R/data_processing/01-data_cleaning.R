# 01-data_cleaning.R
# Data cleaning and import pipeline for DWeMSinUK project
# Domestic Worker Exploitation and Modern Slavery in UK

# Load required libraries
library(tidyverse)
library(haven)
library(janitor)
library(here)

# Main data cleaning function
clean_data <- function() {
  
  # Read raw data with proper path handling
  cat("Reading raw data...\n")
  data <- read.csv(here("data", "raw", "UpdateSelimRiskIndex-sum_cat.csv")) %>%
    select(-contains("column")) %>%
    clean_names()
  
  # Basic data preparation
  cat("Processing basic variables...\n")
  data <- data %>%
    mutate(
      # Convert key numeric variables
      q13 = as.numeric(q13),
      # Set up recruiter relationships
      recruiter.id = as.numeric(node_1_recruiter),
      recruiter.id = replace_na(recruiter.id, -1),
      # Create participant IDs
      id = node_2_id_respondent_recruit,
      # Handle recruiter character variable (ridc) properly
      ridc = as.character(node_1_recruiter),
      ridc = replace_na(ridc, "seed"),
      rowNum = row_number()
    ) %>%
    # Reorganize columns for clarity
    select(rowNum, ridc, recruiter.id, id, q13, starts_with("node_"), everything())
  
  # Process network size variables
  cat("Processing network size and referral data...\n")
  df_processed <- data %>%
    rowwise() %>%
    mutate(
      # Count non-empty network contacts (q105-q115)
      NonEmptyCount = sum(!is.na(c_across(q105:q115))),
      NonEmptyValues = list(na.omit(c_across(q105:q115)))
    ) %>%
    ungroup()
  
  # Calculate referral frequencies (how many people each participant recruited)
  count_df <- df_processed %>%
    group_by(node_1_recruiter) %>%
    summarise(referedFreq = n(), .groups = 'drop')
  
  # Join referral frequencies and calculate final network size
  data_final <- df_processed %>%
    left_join(count_df, by = c("id" = "node_1_recruiter")) %>%
    mutate(
      referedFreq = replace_na(referedFreq, 0),
      # Flag suspicious responses where network claims exceed evidence
      suspicious_variable = ifelse(NonEmptyCount > q13 | referedFreq > q13, 1, 0),
      # Use maximum of claimed size, contact count, and referral count
      numRef = pmax(NonEmptyCount, q13, referedFreq),
      # Set final network size variables
      network.size.variable = as.numeric(numRef),
      network.size = numRef
    ) %>%
    # Reorganize final column order
    select(numRef, NonEmptyCount, referedFreq, q13, suspicious_variable, 
           network.size.variable, network.size, everything())
  
  # Create filtered version removing zero-degree nodes
  data_nonzero <- data_final %>% 
    filter(numRef > 0)
  
  # Save both versions
  cat("Saving cleaned data...\n")
  save(data_final, data_nonzero, 
       file = here("data", "processed", "cleaned_data.RData"))
  
  # Create CSV exports for external use (remove list columns first)
  data_final_csv <- data_final %>% select(-NonEmptyValues)
  data_nonzero_csv <- data_nonzero %>% select(-NonEmptyValues)
  
  write.csv(data_final_csv, here("data", "processed", "data_full.csv"), row.names = FALSE)
  write.csv(data_nonzero_csv, here("data", "processed", "data_nonzero.csv"), row.names = FALSE)
  
  # Create long format data for network analysis
  cat("Creating long format network data...\n")
  df_long <- data_final %>%
    filter(!is.na(NonEmptyValues), map_lgl(NonEmptyValues, ~length(.x) > 0)) %>%
    unnest(NonEmptyValues) %>%
    select(id, NonEmptyCount, NonEmptyValues, everything())
  
  write.csv(df_long, here("data", "processed", "long_format_data.csv"), row.names = FALSE)
  
  cat("Data cleaning completed successfully!\n")
  cat("- Full data:", nrow(data_final), "observations\n")
  cat("- Non-zero degree data:", nrow(data_nonzero), "observations\n")
  cat("- Files saved to data/processed/\n")
  
  return(list(
    full = data_final,
    nonzero = data_nonzero,
    long = df_long
  ))
}

# Execute data cleaning if running this script directly
if (!exists("skip_execution")) {
  cleaned_data <- clean_data()
}
