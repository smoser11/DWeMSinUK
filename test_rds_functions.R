# Quick test to see what RDS estimation functions return
library(RDS)
load('data/processed/prepared_data.RData')

# Test with a small sample
test_data <- rd.dd[!is.na(rd.dd$document_withholding_rds), ]
cat("Test data size:", nrow(test_data), "\n")

# Test MA.estimates
cat("\n=== Testing MA.estimates ===\n")
tryCatch({
  ma_result <- MA.estimates(rds.data = test_data, outcome.variable = "document_withholding_rds")
  cat("MA result class:", class(ma_result), "\n")
  cat("MA result names:", names(ma_result), "\n")
  cat("MA result structure:\n")
  str(ma_result)
}, error = function(e) {
  cat("MA error:", e$message, "\n")
})

# Test RDS.SS.estimates
cat("\n=== Testing RDS.SS.estimates ===\n")
tryCatch({
  ss_result <- RDS.SS.estimates(rds.data = test_data,
                               outcome.variable = "document_withholding_rds",
                               N = 980000)
  cat("SS result class:", class(ss_result), "\n")
  cat("SS result names:", names(ss_result), "\n")
  cat("SS result structure:\n")
  str(ss_result)
}, error = function(e) {
  cat("SS error:", e$message, "\n")
})

# Test RDS.I.estimates
cat("\n=== Testing RDS.I.estimates ===\n")
tryCatch({
  rds1_result <- RDS.I.estimates(rds.data = test_data, outcome.variable = "document_withholding_rds")
  cat("RDS-I result class:", class(rds1_result), "\n")
  cat("RDS-I result names:", names(rds1_result), "\n")
  cat("RDS-I result structure:\n")
  str(rds1_result)
}, error = function(e) {
  cat("RDS-I error:", e$message, "\n")
})