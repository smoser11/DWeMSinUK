# helper_functions.R
# Utility functions for DWeMSinUK project
# Domestic Worker Exploitation and Modern Slavery in UK

# Load required libraries
library(tidyverse)
library(janitor)
library(here)

# Set up proper path handling
ensure_here_setup <- function() {
  if (!exists("here")) {
    library(here)
  }
  # Verify we're in the right directory
  if (!file.exists(here("CLAUDE.md"))) {
    warning("Project root directory may not be set correctly. Expected CLAUDE.md in root.")
  }
}

# Network size validation and calculation
calculateOutDegreeIncludingZeros <- function(rds.df) {
  unique_ids <- unique(rds.df$id)
  out_degree_df <- data.frame(id = unique_ids, out_degree = 0)
  valid_recruiter_ids <- rds.df$recruiter.id[rds.df$recruiter.id != -1]
  out_degree <- table(valid_recruiter_ids)
  
  for (i in seq_along(out_degree_df$id)) {
    id <- out_degree_df$id[i]
    if (id %in% names(out_degree)) {
      out_degree_df$out_degree[i] <- out_degree[[as.character(id)]]
    }
  }
  return(out_degree_df)
}

# Nationality clustering based on survey responses
create_nationality_clusters <- function(data) {
  data %>%
    mutate(
      nationality_cluster = case_when(
        str_detect(tolower(q8_a), "filipin|pilip|asia") ~ "Filipino",
        str_detect(tolower(q8_a), "latin|spain|spai|brazil|colombia|mexico|peru|ecuador") ~ "Latinx", 
        q8 == 0 | str_detect(tolower(q8_a), "british|uk|united kingdom") ~ "British",
        TRUE ~ "Other"
      )
    )
}

# Legacy exploitation indicators (for backward compatibility)
clean_exploitation_indicators <- function(data) {
  data %>%
    mutate(
      # Legacy Q36 indicator (below minimum wage)
      zQ36 = case_when(
        q36 %in% c(0,1,2,3) ~ 1,
        q36 == 4 ~ 0,
        q36 == 5 ~ NA_real_,
        TRUE ~ NA_real_
      ),
      # Legacy Q80 indicator (NRM experience)
      zQ80 = case_when(
        q80 %in% c(0,1) ~ 1,
        q80 %in% c(2,3) ~ 0,
        q80 == 4 ~ NA_real_,
        TRUE ~ NA_real_
      )
    )
}

# Validate data structure for RDS analysis
validate_rds_data <- function(data, required_vars = c("id", "recruiter.id", "network.size")) {
  
  missing_vars <- setdiff(required_vars, names(data))
  if (length(missing_vars) > 0) {
    stop("Missing required variables: ", paste(missing_vars, collapse = ", "))
  }
  
  # Check for duplicate IDs
  if (any(duplicated(data$id))) {
    warning("Duplicate participant IDs found in data")
  }
  
  # Check network size consistency
  if ("numRef" %in% names(data) && "network.size" %in% names(data)) {
    inconsistent <- which(data$numRef != data$network.size)
    if (length(inconsistent) > 0) {
      warning("Inconsistent network size variables found in ", length(inconsistent), " observations")
    }
  }
  
  return(invisible(TRUE))
}

# Create formatted output tables
format_estimation_table <- function(estimates_df, title = "Estimation Results") {
  
  if (nrow(estimates_df) == 0) {
    return(data.frame(message = "No estimates available"))
  }
  
  # Round numeric columns appropriately
  numeric_cols <- sapply(estimates_df, is.numeric)
  estimates_df[numeric_cols] <- lapply(estimates_df[numeric_cols], function(x) round(x, 3))
  
  # Add formatting
  estimates_df$estimate_formatted <- ifelse(
    !is.na(estimates_df$estimate),
    format(estimates_df$estimate, big.mark = ",", scientific = FALSE),
    "NA"
  )
  
  return(estimates_df)
}

# Save results with proper naming
save_results_with_timestamp <- function(results_object, base_filename, output_dir = "output") {
  
  ensure_here_setup()
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- paste0(base_filename, "_", timestamp, ".RData")
  filepath <- here(output_dir, filename)
  
  # Also save without timestamp (latest version)
  latest_filepath <- here(output_dir, paste0(base_filename, ".RData"))
  
  save(results_object, file = filepath)
  save(results_object, file = latest_filepath)
  
  cat("Results saved to:\n")
  cat("- Timestamped:", filepath, "\n") 
  cat("- Latest:", latest_filepath, "\n")
  
  return(invisible(filepath))
}

# Check package dependencies
check_required_packages <- function(packages = c("tidyverse", "RDS", "sspse", "here", "janitor")) {
  
  missing_packages <- setdiff(packages, rownames(installed.packages()))
  
  if (length(missing_packages) > 0) {
    cat("Missing required packages:", paste(missing_packages, collapse = ", "), "\n")
    cat("Install with: install.packages(c(", paste0("'", missing_packages, "'", collapse = ", "), "))\n")
    return(FALSE)
  }
  
  return(TRUE)
}

# Population size parameter lookup
get_population_parameters <- function() {
  return(list(
    sizes = c(50000, 100000, 980000, 1740000),
    descriptions = c(
      "50K" = "Very conservative estimate",
      "100K" = "Conservative estimate", 
      "980K" = "EU baseline estimate",
      "1.74M" = "Upper bound estimate"
    ),
    baseline = 980000,
    nrm_referrals = 44360
  ))
}

# Initialize project environment
setup_project_environment <- function() {
  
  ensure_here_setup()
  
  # Check required packages
  if (!check_required_packages()) {
    stop("Please install missing packages before proceeding")
  }
  
  # Create output directories if they don't exist
  dir.create(here("output"), showWarnings = FALSE, recursive = TRUE)
  dir.create(here("output", "figures"), showWarnings = FALSE, recursive = TRUE)
  dir.create(here("output", "tables"), showWarnings = FALSE, recursive = TRUE)
  dir.create(here("output", "reports"), showWarnings = FALSE, recursive = TRUE)
  dir.create(here("data", "processed"), showWarnings = FALSE, recursive = TRUE)
  
  cat("Project environment setup complete\n")
  cat("- Working directory:", here(), "\n")
  cat("- Output directories created\n")
  cat("- Required packages available\n")
  
  return(invisible(TRUE))
}