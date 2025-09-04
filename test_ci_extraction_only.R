# Test CI extraction from saved MA result
library(here)

# Load the saved MA result
load(here("output", "ma_result_debug_threats_abuse_rds.RData"))

cat("=== TESTING CI EXTRACTION FROM SAVED RESULT ===\n")

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
      cat("✓ Matched interval pattern:", interval_match, "\n")
      numbers <- regmatches(interval_match, gregexpr("[0-9\\.]+", interval_match))[[1]]
      cat("✓ Extracted numbers:", numbers, "\n")
      if (length(numbers) == 2) {
        ci_lower <- as.numeric(numbers[1])
        ci_upper <- as.numeric(numbers[2])
        cat("✓ Successfully extracted CI: [", ci_lower, ",", ci_upper, "]\n")
        
        # Test if point estimate is within CI
        if (!is.na(point_estimate) && !is.na(ci_lower) && !is.na(ci_upper)) {
          if (point_estimate >= ci_lower && point_estimate <= ci_upper) {
            cat("✓ Point estimate is within CI bounds\n")
          } else {
            cat("✗ Point estimate", point_estimate, "is OUTSIDE CI bounds [", ci_lower, ",", ci_upper, "]\n")
          }
        }
        
      } else {
        cat("✗ Failed to parse CI numbers, got", length(numbers), "numbers\n")
      }
    } else {
      cat("✗ Failed to match CI pattern\n")
    }
  } else {
    cat("✗ Could not find prevalence line\n")
    cat("Available lines:\n")
    for (i in 1:length(print_output)) {
      cat("[", i, "] '", print_output[i], "'\n")
    }
  }
} else {
  cat("✗ ma_result$estimate is not an rds.interval.estimate object\n")
}

cat("\n=== ORIGINAL PRINT OUTPUT ===\n")
print(ma_result$estimate)

cat("\n=== Test completed ===\n")