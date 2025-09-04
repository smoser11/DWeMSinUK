# Test the correct print method for rds.interval.estimate
library(here)
library(RDS)

# Load the saved MA result
load(here("output", "ma_result_debug_threats_abuse_rds.RData"))

cat("=== TESTING DIFFERENT PRINT METHODS ===\n")

cat("1. Using print():\n")
print(ma_result$estimate)

cat("\n2. Using print.rds.interval.estimate():\n")
print.rds.interval.estimate(ma_result$estimate)

cat("\n3. Looking at interval structure directly:\n")
cat("ma_result$estimate$interval:\n")
print(ma_result$estimate$interval)
cat("Length:", length(ma_result$estimate$interval), "\n")

# Based on analysis: positions [4] and [5] seem to be CI bounds for category "1"
if (length(ma_result$estimate$interval) >= 6) {
  cat("Direct extraction from interval vector:\n")
  cat("Position [4] (likely ci_lower):", ma_result$estimate$interval[4], "\n")
  cat("Position [5] (likely ci_upper):", ma_result$estimate$interval[5], "\n")
  
  # Point estimate from position 2 (category "1")
  point_estimate <- ma_result$estimate$estimate["1"]
  ci_lower <- ma_result$estimate$interval[4]  
  ci_upper <- ma_result$estimate$interval[5]
  
  cat("\nFINAL RESULTS:\n")
  cat("Point estimate (1):", point_estimate, "\n")
  cat("CI Lower:", ci_lower, "\n")
  cat("CI Upper:", ci_upper, "\n")
  
  # Check if reasonable
  if (!is.na(point_estimate) && !is.na(ci_lower) && !is.na(ci_upper)) {
    if (point_estimate >= ci_lower && point_estimate <= ci_upper) {
      cat("✓ Point estimate is within CI bounds\n")
    } else {
      cat("✗ Point estimate is outside CI bounds\n")
    }
    
    if (ci_lower < ci_upper && ci_lower >= 0 && ci_upper <= 1) {
      cat("✓ CI bounds are reasonable (0-1 range, lower < upper)\n")
    } else {
      cat("✗ CI bounds are unreasonable\n")
    }
  }
}