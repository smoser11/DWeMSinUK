# Simple test runner: sources each test_*.R script in this directory
cat("=== Running tests in tests/ ===\n")
this_dir <- dirname(normalizePath(sys.frame(1)$ofile %||% "."))
test_files <- list.files(this_dir, pattern = "^test_.*\\.R$", full.names = TRUE)
if (length(test_files) == 0) {
  cat("No test_*.R files found in tests/\n")
} else {
  for (f in test_files) {
    cat("\n--- ", basename(f), " ---\n", sep = "")
    try(source(f), silent = FALSE)
  }
}
cat("\n=== Tests complete ===\n")

