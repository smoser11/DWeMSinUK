# 00-main_pipeline.R
# End-to-end analysis pipeline for the DWeMSinUK project.
# Domestic Worker Exploitation and Modern Slavery in the UK.
# RDS + NSUM + Bayesian Model-Assisted estimation.
#
# Updated 2026-06-20 for the cleaned, coherent R/analysis/ structure.
#
# Pipeline steps:
#   1.  Data cleaning           (R/data_processing/01-data_cleaning.R)
#   2.  Data preparation        (R/data_processing/02-data_preparation.R)
#   3.  RDS estimation          (R/analysis/03-rds_estimation.R)
#   4.  Bootstrap CIs (RDS)     (R/analysis/04-bootstrap_analysis.R)
#   5.  NSUM estimation         (R/analysis/05-nsum_estimation.R)
#   5b. NSUM continuous-tau     (R/analysis/05b-nsum_tau_sensitivity.R)
#   6.  Bayesian appendix       (R/analysis/06-bayesian_appendix.R)
#       PREREQ: bash R/analysis/06-run-all.sh   (long-running; runs out-of-band)
#   7.  Paper figures           (R/analysis/07-paper_figures.R)
#   8.  Subgroup analysis       (R/analysis/08b-simple_subgroup_analysis.R)
#       (08-netclust_subgroup_analysis.R requires netclust; not on CRAN; skip by default)
#   9.  LaTeX tables            (R/analysis/09-create_latex_tables.R)

cat("=== DWeMSinUK Analysis Pipeline ===\n")
cat("Domestic Worker Exploitation and Modern Slavery in the UK\n")
cat("RDS + NSUM + Bayesian Model-Assisted estimation\n\n")

# --------------------------------------------------------------------------
# Bootstrap
# --------------------------------------------------------------------------

library(here)
source(here("R", "utils", "helper_functions.R"))
source(here("R", "config.R"))

global_config <- get_global_config()
setup_project_environment()

# Flag so that scripts which check `skip_execution` will run their main body
skip_execution <- FALSE

# --------------------------------------------------------------------------
# Pipeline configuration
# --------------------------------------------------------------------------

pipeline_config <- list(
  # Toggle individual steps
  run_data_cleaning              = TRUE,
  run_data_preparation           = TRUE,
  run_rds_estimation             = TRUE,
  run_bootstrap_analysis         = TRUE,
  run_nsum_estimation            = TRUE,
  run_nsum_tau_sensitivity       = TRUE,   # NEW (continuous-tau NSUM sensitivity)
  run_bayesian_appendix          = TRUE,   # consolidator only; see PREREQ note below
  run_paper_figures              = TRUE,
  run_subgroup_analysis          = TRUE,
  run_latex_tables               = TRUE,

  # Parameters
  preferred_rds_method   = global_config$preferred_rds_method,
  preferred_nsum_method  = global_config$preferred_nsum_method,
  include_bootstrap      = global_config$include_bootstrap,
  n_bootstrap            = global_config$n_bootstrap,
  population_sizes       = global_config$population_sizes,
  main_population_size   = global_config$main_population_size,

  # Output options
  save_intermediate_results  = TRUE,
  create_summary_report      = TRUE
)

cat("Pipeline steps enabled:\n")
for (k in grep("^run_", names(pipeline_config), value = TRUE)) {
  cat(sprintf("  %-32s %s\n", k, pipeline_config[[k]]))
}
cat(sprintf("\nMain population size: %s\nBootstrap samples:    %d\n\n",
            format(pipeline_config$main_population_size, big.mark = ","),
            pipeline_config$n_bootstrap))

# --------------------------------------------------------------------------
# Helper: source a step file with success/fail logging
# --------------------------------------------------------------------------

run_step <- function(label, script_relpath) {
  cat(sprintf("\n>>> %s\n", label))
  full <- here(script_relpath)
  if (!file.exists(full)) {
    cat(sprintf("    [SKIP] file not found: %s\n", script_relpath))
    return(invisible(FALSE))
  }
  result <- tryCatch({
    source(full)
    TRUE
  }, error = function(e) {
    cat(sprintf("    [FAIL] %s\n      %s\n", script_relpath, e$message))
    FALSE
  })
  if (result) cat(sprintf("    [ OK ] %s\n", script_relpath))
  invisible(result)
}

# --------------------------------------------------------------------------
# Pipeline execution
# --------------------------------------------------------------------------

# Step 1: Data cleaning
if (pipeline_config$run_data_cleaning) {
  run_step("Step 1: Data cleaning and import",
           file.path("R", "data_processing", "01-data_cleaning.R"))
}

# Step 2: Data preparation (comparable indicators, RDS weights)
if (pipeline_config$run_data_preparation) {
  run_step("Step 2: Data preparation + CE comparable indicators",
           file.path("R", "data_processing", "02-data_preparation.R"))
}

# Step 3: RDS estimation (RDS-I, RDS-II, RDS-SS)
if (pipeline_config$run_rds_estimation) {
  run_step("Step 3: RDS estimation (RDS-I / RDS-II / RDS-SS)",
           file.path("R", "analysis", "03-rds_estimation.R"))
}

# Step 4: Bootstrap CIs for RDS (per-method and pooled)
if (pipeline_config$run_bootstrap_analysis && pipeline_config$include_bootstrap) {
  run_step("Step 4: Bootstrap confidence intervals (per-method RDS-I/II/SS)",
           file.path("R", "analysis", "04-bootstrap_analysis.R"))
}

# Step 5: NSUM estimation (MBSU baseline)
if (pipeline_config$run_nsum_estimation) {
  run_step("Step 5: NSUM estimation (MBSU baseline)",
           file.path("R", "analysis", "05-nsum_estimation.R"))
}

# Step 5b: NSUM continuous-tau sensitivity (NEW, 2026-06-20)
# Addresses the IJOPM "wide range" concern by showing prevalence as a continuous
# function of the visibility / true-positive-rate adjustment tau.
if (pipeline_config$run_nsum_tau_sensitivity) {
  run_step("Step 5b: NSUM continuous-tau sensitivity",
           file.path("R", "analysis", "05b-nsum_tau_sensitivity.R"))
}

# Step 6: Bayesian Model-Assisted appendix consolidation
# IMPORTANT: this script CONSOLIDATES results from the individual
# ma_sensitivity_*.RData files in output/. Those files are produced by
#   bash R/analysis/06-run-all.sh
# which runs ~hours-to-overnight on a workstation and MUST be executed
# out-of-band (not inside this pipeline) because it spawns its own R sessions.
# If the .RData files are missing, this step will report what was skipped
# and the consolidator will produce empty CSVs.
if (pipeline_config$run_bayesian_appendix) {
  cat("\n>>> Step 6: Bayesian appendix (consolidation only)\n")
  ma_files <- list.files(here("output"), pattern = "^ma_sensitivity_.*\\.RData$")
  if (length(ma_files) == 0) {
    cat("    [SKIP] no ma_sensitivity_*.RData files found in output/.\n")
    cat("    Run `bash R/analysis/06-run-all.sh` first (overnight job).\n")
  } else {
    cat(sprintf("    Found %d cached MA result files.\n", length(ma_files)))
    run_step("Step 6: Consolidate Bayesian results into appendix tables",
             file.path("R", "analysis", "06-bayesian_appendix.R"))
  }
}

# Step 7: Paper figures (canonical producer of record for all 8 manuscript figures)
if (pipeline_config$run_paper_figures) {
  run_step("Step 7: Paper figures (8 figures -> output/figures/paper/)",
           file.path("R", "analysis", "07-paper_figures.R"))
}

# Step 8: Subgroup analysis (08b- = simple; 08- = netclust, optional)
if (pipeline_config$run_subgroup_analysis) {
  run_step("Step 8: Subgroup analysis by nationality (simple variant)",
           file.path("R", "analysis", "08b-simple_subgroup_analysis.R"))
  # Optional netclust version (requires netclust 0.1.0 with R 4.5+ compat patch):
  # run_step("Step 8 (alt): Clustered SS-PSE via netclust",
  #          file.path("R", "analysis", "08-netclust_subgroup_analysis.R"))
}

# Step 9: LaTeX tables for the manuscript
if (pipeline_config$run_latex_tables) {
  run_step("Step 9: Generate LaTeX tables",
           file.path("R", "analysis", "09-create_latex_tables.R"))
}

# --------------------------------------------------------------------------
# Summary report
# --------------------------------------------------------------------------

if (pipeline_config$create_summary_report) {
  cat("\nGenerating summary report...\n")

  reports_dir <- here("output", "reports")
  dir.create(reports_dir, showWarnings = FALSE, recursive = TRUE)
  report_file <- file.path(reports_dir, paste0("pipeline_summary_", Sys.Date(), ".txt"))

  artefacts <- list(
    "Cleaned data"              = here("data", "processed", "cleaned_data.RData"),
    "Prepared data"             = here("data", "processed", "prepared_data.RData"),
    "RDS estimation results"    = here("output", "rds_estimation_results.RData"),
    "Bootstrap CIs (per-method)" = here("output", "tables", "rds_per_method_bootstrap_ci.csv"),
    "NSUM estimation results"   = here("output", "nsum_estimation_results.RData"),
    "NSUM tau sensitivity"      = here("output", "tables", "nsum_tau_sensitivity.csv"),
    "Bayesian appendix summary" = here("output", "tables", "ESM_appendix_bayesian_summary.csv"),
    "Bayesian seed-check"       = here("output", "tables", "ESM_appendix_bayesian_seedcheck.csv"),
    "Paper figures (8)"         = here("output", "figures", "paper")
  )

  lines <- c(
    "DWeMSinUK Analysis Pipeline - summary",
    paste0("Generated: ", Sys.time()),
    "",
    "Artefacts produced:"
  )
  for (label in names(artefacts)) {
    present <- file.exists(artefacts[[label]])
    lines <- c(lines, sprintf("  [%s] %s", if (present) "x" else " ", label))
  }
  lines <- c(lines, "",
             "Pipeline steps run:",
             vapply(grep("^run_", names(pipeline_config), value = TRUE),
                    function(k) sprintf("  %s = %s", k, pipeline_config[[k]]),
                    character(1)),
             "",
             "Output locations:",
             "  data/processed/   - processed RDS data",
             "  output/           - top-level cached results (.RData, .rds)",
             "  output/tables/    - .csv summaries",
             "  output/figures/   - cached figures (including output/figures/paper/ canonical set)",
             "  output/reports/   - text summary reports",
             "",
             "For manual / out-of-band steps:",
             "  bash R/analysis/06-run-all.sh   - Bayesian MA sensitivity (~overnight)",
             "")

  writeLines(lines, report_file)
  cat(sprintf("Summary report saved to: %s\n", report_file))
}

rm(skip_execution)

cat("\n=== PIPELINE COMPLETE ===\n")
cat("Inspect output/reports/, output/tables/, output/figures/paper/.\n")
cat("For documentation, see CLAUDE.md and AGENTS.md at repo root.\n")
