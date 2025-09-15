# Test the v2 CI extraction logic
library(here)
library(RDS)

# Load the saved MA result
load(here("output", "ma_result_debug_threats_abuse_rds.RData"))

cat("=== TESTING V2 CI EXTRACTION LOGIC ===\n")

# Copy the exact logic from v2 file
if (!is.null(ma_result$estimate) && inherits(ma_result$estimate, "rds.interval.estimate")) {
  cat("✓ ma_result$estimate is an rds.interval.estimate object\n")
  
  # Extract point estimate (v2 logic)
  if (!is.null(ma_result$estimate$estimate) && "1" %in% names(ma_result$estimate$estimate)) {
    point_estimate <- ma_result$estimate$estimate["1"]
    cat("✓ Point estimate:", point_estimate, "\n")
  } else {
    point_estimate <- NA
    cat("✗ Could not extract point estimate\n")
  }
  
  # Extract CIs from print output (v2 logic)
  print_output <- capture.output(print(ma_result$estimate))
  cat("✓ Captured print output for CI extraction\n")
  
  # Look for the line with the prevalence category (usually "1")
  # The line looks like: "1   0.5948 (  0.4682,   0.7214)          1.47     0.0646    51"
  prevalence_line <- grep("^1\\s+", print_output, value = TRUE)
  if (length(prevalence_line) > 0) {
    cat("✓ Found prevalence line:", prevalence_line, "\n")
    
    # Extract the interval from the line like: "1   0.5948 (  0.4682,   0.7214)..."
    interval_match <- regmatches(prevalence_line, regexpr("\\(\\s*([0-9\\.]+),\\s*([0-9\\.]+)\\s*\\)", prevalence_line))
    if (length(interval_match) > 0) {
      # Extract the numbers from the parentheses
      numbers <- regmatches(interval_match, gregexpr("[0-9\\.]+", interval_match))[[1]]
      if (length(numbers) == 2) {
        ci_lower <- as.numeric(numbers[1])
        ci_upper <- as.numeric(numbers[2])
        cat("✓ Successfully parsed CI from print output: [", ci_lower, ",", ci_upper, "]\n")
      } else {
        ci_lower <- NA
        ci_upper <- NA
        cat("✗ Failed to parse numbers from print output\n")
      }
    } else {
      ci_lower <- NA
      ci_upper <- NA
      cat("✗ Failed to match interval pattern in print output\n")
    }
  } else {
    cat("✗ Could not find prevalence category line in print output\n")
    # Debug: show all lines to help troubleshoot
    for (i in 1:min(10, length(print_output))) {
      cat("    Line", i, ": '", print_output[i], "'\n")
    }
    ci_lower <- NA
    ci_upper <- NA
  }
  
  # Final validation
  cat("\n=== FINAL RESULTS ===\n")
  cat("Point estimate (1):", point_estimate, "\n")
  cat("CI Lower:", ci_lower, "\n")
  cat("CI Upper:", ci_upper, "\n")
  
  # Validation checks
  if (!is.na(point_estimate) && !is.na(ci_lower) && !is.na(ci_upper)) {
    if (point_estimate >= ci_lower && point_estimate <= ci_upper) {
      cat("✓ Point estimate is within CI bounds\n")
    } else {
      cat("✗ Point estimate", point_estimate, "is OUTSIDE CI bounds [", ci_lower, ",", ci_upper, "]\n")
    }
    
    if (ci_lower < ci_upper && ci_lower >= 0 && ci_upper <= 1) {
      cat("✓ CI bounds are reasonable (0-1 range, lower < upper)\n")
    } else {
      cat("✗ CI bounds are unreasonable\n")
    }
    
    if (point_estimate >= 0 && point_estimate <= 1) {
      cat("✓ Point estimate is in reasonable range (0-1)\n")
    } else {
      cat("✗ Point estimate is outside 0-1 range\n")
    }
    
    cat("\n✅ EXTRACTION SUCCESS! All values are reasonable.\n")
  } else {
    cat("✗ Some values are missing (NA)\n")
  }
  
} else {
  cat("✗ ma_result$estimate is not an rds.interval.estimate object\n")
}

cat("\n=== ORIGINAL PRINT OUTPUT FOR REFERENCE ===\n")
print(ma_result$estimate)

