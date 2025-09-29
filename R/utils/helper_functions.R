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
  # Verify project root using RStudio project file or a marker
  has_rproj <- length(list.files(here(), pattern = "\\.Rproj$", ignore.case = TRUE)) > 0
  has_marker <- file.exists(here(".project-root"))
  if (!(has_rproj || has_marker)) {
    warning("Project root may not be set. Expected an .Rproj file or .project-root marker in repository root.")
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
        # Filipino cluster - includes Asian (Filipino) responses
        str_detect(tolower(q8_a), "filipin|pilip") ~ "Filipino",

        # Latinx cluster - comprehensive Spanish/Latin American countries
        str_detect(tolower(q8_a),
                  "latin|españa|espa|spanish|brazil|brasil|colombia|boliv|
                   mexico|mexica|peru|ecuador|venezolan|dominican|cuba|panama|
                   latín|latin american") ~ "Latinx",

        # British cluster - UK nationals
        q8 == 0 | str_detect(tolower(q8_a), "british|uk|united kingdom") ~ "British",

        # Other cluster - remaining categories including Asian (non-Filipino)
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

# ============================================================================
# RDS ANALYSIS SHARED FUNCTIONS
# ============================================================================

# RDS Results Database Management
load_rds_results_database <- function() {
  results_file <- here("output", "rds_results_database.RDS")
  
  if (file.exists(results_file)) {
    cat("Loading existing RDS results database...\n")
    return(readRDS(results_file))
  } else {
    cat("Creating new RDS results database...\n")
    return(list())
  }
}

save_to_rds_database <- function(results_db, new_results, result_type) {
  results_file <- here("output", "rds_results_database.RDS")
  
  # Add new results to database
  results_db[[result_type]] <- new_results
  results_db[[paste0(result_type, "_timestamp")]] <- Sys.time()
  
  # Save database
  saveRDS(results_db, results_file)
  cat("Results saved to database:", result_type, "\n")
  
  return(results_db)
}

# Parameter ID generation for caching
create_parameter_id <- function(method, outcome_var, pop_size, additional_params = NULL) {
  components <- c(method, outcome_var, pop_size)
  if (!is.null(additional_params)) {
    components <- c(components, additional_params)
  }
  return(paste(components, collapse = "_"))
}

# CE's comparable indicators specification
get_comparable_indicators <- function() {
  return(list(
    rds_vars = c("document_withholding_rds", "pay_issues_rds", "threats_abuse_rds", 
                 "excessive_hours_rds", "access_to_help_rds"),
    nsum_vars = c("document_withholding_nsum", "pay_issues_nsum", "threats_abuse_nsum", 
                  "excessive_hours_nsum", "access_to_help_nsum"),
    labels = c(
      "document_withholding" = "Document withholding",
      "pay_issues" = "Pay-related issues", 
      "threats_abuse" = "Threats and abuse",
      "excessive_hours" = "Excessive working hours",
      "access_to_help" = "Limited access to help",
      "composite_risk" = "Composite risk score",
      "whether_exploitation" = "Overall exploitation indicator",
      "sum_categories" = "Risk exposure scale (ordinal)"
    ),
    confidence_levels = c(
      "document_withholding" = "Highest",
      "pay_issues" = "High",
      "threats_abuse" = "High", 
      "excessive_hours" = "Medium",
      "access_to_help" = "Lowest",
      "composite_risk" = "High",
      "whether_exploitation" = "High",
      "sum_categories" = "High"
    )
  ))
}

# Publication theme and colors
theme_rds_publication <- function() {
  theme_minimal() +
    theme(
      text = element_text(size = 12, family = "Arial"),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "gray60"),
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.5, color = "gray90"),
      strip.text = element_text(size = 10, face = "bold")
    )
}

# Color palette for methods
get_method_colors <- function() {
  return(c(
    "RDS_I" = "#E31A1C", 
    "RDS_II" = "#1F78B4", 
    "RDS_SS" = "#33A02C",
    "Model_Assisted" = "#FF7F00",
    "NSUM" = "#6A3D9A"
  ))
}

# Format estimates for publication
format_estimate_with_ci <- function(estimate, ci_lower, ci_upper, as_percentage = TRUE) {
  multiplier <- ifelse(as_percentage, 100, 1)
  suffix <- ifelse(as_percentage, "%", "")
  
  if (is.na(ci_lower) || is.na(ci_upper)) {
    return(sprintf("%.1f%s", estimate * multiplier, suffix))
  } else {
    return(sprintf("%.1f%s (%.1f–%.1f)", 
                   estimate * multiplier, suffix,
                   ci_lower * multiplier, ci_upper * multiplier))
  }
}
