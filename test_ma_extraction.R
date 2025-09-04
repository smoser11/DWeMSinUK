# Test script for MA.estimates CI extraction
library(here)
library(RDS)

# Source helper functions and load data
source(here("R", "utils", "helper_functions.R"))
if (!exists("dd") || !exists("rd.dd")) {
  load(here("data", "processed", "prepared_data.RData"))
}

# Test MA.estimates with threats_abuse_rds
outcome_var <- "threats_abuse_rds"
cat("Testing MA.estimates CI extraction for:", outcome_var, "\n")

# Run MA.estimates with debugging parameters
ma_result <- MA.estimates(
  rd.dd, 
  trait.variable = outcome_var,
  N = 980000,
  number.of.iterations = 1,
  M1 = 2,
  M2 = 1,
  parallel = 1,
  verbose = TRUE,
  full.output = TRUE,
  seed = 42,
  MPLE.samplesize = 10,
  SAN.maxit = 2,
  SAN.nsteps = 100,
  sim.interval = 10
)

cat("\n=== TESTING CI EXTRACTION ===\n")

# Test the extraction logic
if (!is.null(ma_result$estimate) && inherits(ma_result$estimate, "rds.interval.estimate")) {
  cat("✓ ma_result$estimate is an rds.interval.estimate object\n")
  
  # Extract point estimate
  if (!is.null(ma_result$estimate$estimate) && "1" %in% names(ma_result$estimate$estimate)) {
    point_estimate <- ma_result$estimate$estimate["1"]
    cat("✓ Point estimate:", point_estimate, "\n")
  } else {
    cat("✗ Failed to extract point estimate\n")
  }
  
  # Extract CIs from print output
  print_output <- capture.output(print(ma_result$estimate))
  cat("Print output captured, lines:", length(print_output), "\n")
  
  # Look for the prevalence line
  prevalence_line <- grep("^1\\s+", print_output, value = TRUE)
  if (length(prevalence_line) > 0) {
    cat("✓ Found prevalence line:", prevalence_line, "\n")
    
    # Extract CI
    interval_match <- regmatches(prevalence_line, regexpr("\\(\\s*([0-9\\.]+),\\s*([0-9\\.]+)\\s*\\)", prevalence_line))
    if (length(interval_match) > 0) {
      numbers <- regmatches(interval_match, gregexpr("[0-9\\.]+", interval_match))[[1]]
      if (length(numbers) == 2) {
        ci_lower <- as.numeric(numbers[1])
        ci_upper <- as.numeric(numbers[2])
        cat("✓ Successfully extracted CI: [", ci_lower, ",", ci_upper, "]\n")
      } else {
        cat("✗ Failed to parse CI numbers\n")
      }
    } else {
      cat("✗ Failed to match CI pattern\n")
    }
  } else {
    cat("✗ Could not find prevalence line\n")
  }
} else {
  cat("✗ ma_result$estimate is not an rds.interval.estimate object\n")
}

cat("\n=== COMPARISON WITH PRINT OUTPUT ===\n")
print(ma_result$estimate)

cat("\nTest completed.\n")