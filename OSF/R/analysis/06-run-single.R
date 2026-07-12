# 06c-run-single.R
# Run ONE MA.estimates sensitivity combination in a fresh R session.
# Designed to be invoked from 06c-run-all.sh (or interactively).
#
# Usage:
#   Rscript R/analysis/06c-run-single.R <indicator> <population_size> <seed_selection>
#
# Example:
#   Rscript R/analysis/06c-run-single.R document_withholding_rds 980000 degree
#
# Why this exists: running the full 96-combination sensitivity loop in a single
# R session accumulates memory pressure that crashes even on 128GB RAM. This
# script runs ONE combination per process invocation - R exits between calls,
# memory is fully reclaimed by the OS. Use the bash wrapper 06c-run-all.sh
# to loop, or call it directly for a single combination.
# Created 2026-06-11.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3) {
  cat("Usage: Rscript R/analysis/06c-run-single.R <indicator> <population_size> <seed_selection>\n")
  cat("  e.g. Rscript R/analysis/06c-run-single.R document_withholding_rds 980000 degree\n")
  quit(status = 1)
}
indicator       <- args[1]
population_size <- as.numeric(args[2])
seed_selection  <- args[3]

cat(sprintf("06c-run-single.R: indicator=%s, N=%s, seed=%s\n",
            indicator, format(population_size, big.mark = ","), seed_selection))

# Skip if already done (resumability)
suppressMessages({
  library(here)
})
combination_id <- paste(indicator,
                        format(population_size, scientific = FALSE),
                        seed_selection, sep = "_")
result_file <- here("output", paste0("ma_sensitivity_", combination_id, ".RData"))
if (file.exists(result_file)) {
  cat("  Already exists, skipping:", result_file, "\n")
  quit(status = 0)
}

# Load packages and project utilities
suppressMessages({
  library(RDS)
  library(sspse)
  library(tidyverse)
})
source(here("R", "utils", "helper_functions.R"))

# Load prepared RDS data. The data-preparation pipeline creates a processed
# .RData containing the rd.dd object used by all 06* scripts
prepared_path <- here("data", "processed", "prepared_data.RData")
if (!file.exists(prepared_path)) {
  cat("ERROR: prepared_data.RData not found at", prepared_path, "\n")
  cat("  Run R/data_processing/02-data_preparation.R first.\n")
  quit(status = 2)
}
load(prepared_path)  # loads rd.dd (or equivalent)
if (!exists("rd.dd")) {
  # Some prior pipeline versions named the object differently - try fallbacks
  candidates <- c("prepared_data", "rds_data", "dat", "dd")
  for (cand in candidates) {
    if (exists(cand)) {
      rd.dd <- get(cand)
      break
    }
  }
  if (!exists("rd.dd")) stop("Could not locate the RDS data object after loading prepared_data.RData")
}

# Parameters - kept in sync with 06c-bayesian_sensitivity.R (updated 2026-06-11)
# sum_categories was dropped 2026-06-17 (duplicate of composite_risk in raw CSV).
numeric_indicators <- c("composite_risk")
if (indicator %in% numeric_indicators) {
  params <- list(
    number.of.iterations = 15,
    M1 = 1000,
    M2 = 400,
    MPLE.samplesize = 150000,
    SAN.maxit = 25,
    SAN.nsteps = 2^22,
    sim.interval = 25000,
    parallel = 4,
    verbose = FALSE,
    full.output = TRUE,
    seed = 42
  )
} else {
  params <- list(
    number.of.iterations = 10,
    M1 = 500,
    M2 = 200,
    MPLE.samplesize = 50000,
    SAN.maxit = 10,
    SAN.nsteps = 2^19,
    sim.interval = 10000,
    parallel = 4,
    verbose = FALSE,
    full.output = TRUE,
    seed = 42
  )
}

cat("  Parameters: iterations=", params$number.of.iterations,
    " M1=", params$M1, " M2=", params$M2, "\n", sep = "")

start_time <- Sys.time()
ma_result <- tryCatch(
  MA.estimates(
    rd.dd,
    trait.variable        = indicator,
    N                     = population_size,
    number.of.iterations  = params$number.of.iterations,
    M1                    = params$M1,
    M2                    = params$M2,
    MPLE.samplesize       = params$MPLE.samplesize,
    SAN.maxit             = params$SAN.maxit,
    SAN.nsteps            = params$SAN.nsteps,
    sim.interval          = params$sim.interval,
    seed.selection        = seed_selection,
    parallel              = params$parallel,
    verbose               = params$verbose,
    full.output           = params$full.output,
    seed                  = params$seed
  ),
  error = function(e) {
    cat("ERROR running MA.estimates:", conditionMessage(e), "\n")
    quit(status = 3)
  }
)
elapsed <- difftime(Sys.time(), start_time, units = "mins")
cat(sprintf("  MA.estimates finished in %.1f minutes\n", as.numeric(elapsed)))

dir.create(here("output"), showWarnings = FALSE, recursive = TRUE)
save(ma_result, file = result_file)
cat("  Saved:", result_file, "\n")
cat("  Done.\n")
