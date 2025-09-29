# test_netclust_minimal.R
# Minimal test to diagnose netclust issues

cat("=== MINIMAL NETCLUST DIAGNOSTIC TEST ===\n")

library(netclust)
library(tidyverse)

# Load data
load('data/processed/prepared_data.RData')
rds_data <- if(exists('dd')) dd else rd.dd

cat("Data loaded:", nrow(rds_data), "observations\n")

# Create the simplest possible test case
test_data <- rds_data %>%
  filter(!is.na(nationality_cluster), !is.na(document_withholding_rds)) %>%
  mutate(
    # Create just Filipino vs Other (2 clusters)
    cluster_simple = ifelse(nationality_cluster == "Filipino", "Filipino", "Other"),
    degree = as.numeric(q13),
    degree = ifelse(is.na(degree) | degree < 1, 1, degree)
  ) %>%
  select(id, degree, cluster_simple, document_withholding_rds)

cat("Simplified data:", nrow(test_data), "observations\n")
cat("Cluster distribution:\n")
print(table(test_data$cluster_simple))

# Extract vectors for netclust
degree_seq <- test_data$degree
cluster_assignments <- as.numeric(as.factor(test_data$cluster_simple))
n_clusters <- length(unique(cluster_assignments))

cat("Degree range:", range(degree_seq), "\n")
cat("Cluster assignments range:", range(cluster_assignments), "\n")
cat("Number of clusters:", n_clusters, "\n")

# Test 1: Very basic call with minimal parameters
cat("\n=== TEST 1: Minimal parameter set ===\n")
tryCatch({
  fit1 <- posteriorsize.c(
    s = degree_seq,
    c = cluster_assignments,
    median.prior.size = 1000,  # Much smaller prior
    burnin = 100,
    samplesize = 100,
    interval = 1,
    verbose = FALSE
  )
  cat("✓ TEST 1 SUCCESS: Basic call worked!\n")
  cat("Population size estimate:", fit1$N[3], "\n")  # Median

}, error = function(e) {
  cat("✗ TEST 1 FAILED:", e$message, "\n")
})

# Test 2: Add prop.prior.params explicitly
cat("\n=== TEST 2: With explicit cluster priors ===\n")
tryCatch({
  fit2 <- posteriorsize.c(
    s = degree_seq,
    c = cluster_assignments,
    median.prior.size = 1000,
    prop.prior.params = rep(1, n_clusters),  # Explicit cluster priors
    burnin = 100,
    samplesize = 100,
    interval = 1,
    verbose = FALSE
  )
  cat("✓ TEST 2 SUCCESS: With cluster priors worked!\n")
  cat("Population size estimate:", fit2$N[3], "\n")  # Median

}, error = function(e) {
  cat("✗ TEST 2 FAILED:", e$message, "\n")
})

# Test 3: Try with the example data format from package
cat("\n=== TEST 3: Using package example format ===\n")
tryCatch({
  # Load built-in example data
  data(csamp)
  csamp <- csamp[order(csamp$samp.order), ]

  cat("Example data size:", nrow(csamp), "clusters:", length(unique(csamp$cluster)), "\n")

  fit3 <- posteriorsize.c(
    csamp$network.size.variable,
    csamp$cluster,
    median.prior.size = 3000,
    prop.prior.params = rep(1, 3),
    burnin = 100,
    samplesize = 500,
    interval = 1,
    verbose = FALSE
  )
  cat("✓ TEST 3 SUCCESS: Package example worked!\n")
  cat("Population size estimate:", fit3$N[3], "\n")  # Median

}, error = function(e) {
  cat("✗ TEST 3 FAILED:", e$message, "\n")
})

# Test 4: Compare our data structure to example
cat("\n=== TEST 4: Data structure comparison ===\n")
if (exists("csamp")) {
  cat("Example data structure:\n")
  cat("  network.size.variable class:", class(csamp$network.size.variable), "\n")
  cat("  cluster class:", class(csamp$cluster), "\n")
  cat("  network.size.variable range:", range(csamp$network.size.variable), "\n")
  cat("  cluster range:", range(csamp$cluster), "\n")
}

cat("Our data structure:\n")
cat("  degree class:", class(degree_seq), "\n")
cat("  cluster_assignments class:", class(cluster_assignments), "\n")
cat("  degree range:", range(degree_seq), "\n")
cat("  cluster_assignments range:", range(cluster_assignments), "\n")

cat("\n=== DIAGNOSTIC TEST COMPLETE ===\n")