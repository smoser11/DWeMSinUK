# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This repository contains a research project analyzing domestic worker exploitation and modern slavery in the UK using Network Scale-Up Method (NSUM) and Respondent-Driven Sampling (RDS). The project implements multiple statistical estimation methods to measure prevalence of labor exploitation among UK domestic workers.

**Active submission**: Social Indicators Research (Springer). Current draft: `paper/SIR/manuscript-v4e.qmd` (Quarto) and compiled outputs in `paper/SIR/`.

**Authors**: Caroline Emberson (lead, Nottingham University Business School) & Scott Moser (School of Politics and International Relations, University of Nottingham).

## Architecture and Code Structure

### Core Directory Structure
```
DWeMSinUK/
├── R/                            # All R analysis code
│   ├── 00-main_pipeline.R        # Master analysis pipeline
│   ├── config.R                  # Centralised global configuration
│   ├── data_processing/          # Data cleaning and preparation
│   │   ├── 01-data_cleaning.R
│   │   └── 02-data_preparation.R
│   ├── analysis/                 # Statistical estimation methods
│   │   ├── 03-rds_estimation.R   # RDS-I/II/SS estimation + bootstrap
│   │   ├── 04-bootstrap_analysis.R
│   │   ├── 05-nsum_estimation.R  # NSUM estimation (uses modules below)
│   │   ├── 05b-nsum_tau_sensitivity.R   # Continuous-tau NSUM sensitivity
│   │   ├── 06-bayesian_appendix.R       # Consolidates overnight MA runs
│   │   ├── 06-run-all.sh                # Overnight bash driver (see below)
│   │   ├── 06-run-single.R              # Single MA.estimates call
│   │   ├── 07-paper_figures.R           # All publication figures
│   │   ├── 08-netclust_subgroup_analysis.R   # Clustered SS-PSE (netclust required)
│   │   ├── 08b-simple_subgroup_analysis.R    # RDS by subgroup
│   │   ├── 09-create_latex_tables.R          # LaTeX table generation
│   │   ├── nsum_core_estimators.R       # NSUM estimation engine
│   │   ├── nsum_bootstrap.R             # NSUM bootstrap CIs
│   │   ├── nsum_robust_adjustment.R     # Feehan & Salganik adjustment
│   │   ├── nsum_visualization.R         # NSUM plots
│   │   └── NSUM/                        # Three-step NSUM bootstrap scripts
│   │       ├── Boot_Step1.r / Boot_Step2.R / Boot_Step3.R
│   │       └── REGENERATE_NSUM_*.R
│   ├── utils/
│   │   └── helper_functions.R
│   ├── tests/                    # R test scripts
│   └── archive/                  # Archived old/experimental scripts
├── data/
│   ├── raw/                      # Original survey data
│   ├── processed/                # Cleaned datasets (.RData/.csv)
│   └── survey/                   # Survey instruments, codebooks
├── output/                       # Analysis outputs
│   ├── figures/                  # Plots and visualisations (.png, tracked)
│   ├── tables/                   # Summary tables (.csv, tracked)
│   ├── reports/                  # Generated analysis reports
│   └── logs/                     # Bash run logs (gitignored)
├── paper/
│   ├── SIR/                      # CURRENT submission (manuscript-v4e.qmd + ESMs)
│   ├── archive/                  # Historical draft versions (IJOPM, BJMM, etc.)
│   ├── decisions/                # Desk-rejection PDFs
│   └── references/               # .bib files and cited papers
├── correspondence/               # Project email history, by year
├── CRAN/                         # Vendored R-package source tarballs (tracked)
└── _snapshots/                   # GITIGNORED safety-net tarballs (2026-05-22 reorg)
```

### Tracked vs untracked

- **Source code, data, and manuscripts are tracked.**
- **Derived analysis outputs are gitignored** (`output/*.RData`, `output/*.rds`, `output/nsum_*/`, `output/archive_*/`, `output/logs/`). They stay on local disk so the pipeline can use cached intermediates via `force_recompute = FALSE`.
- **`Meetings/` and `_snapshots/` are gitignored.**
- **`CRAN/` is tracked** as a vendored package backup (especially `netclust`, which has known incompatibilities with R 4.5+ and is not on CRAN).

## Running the Analysis

### Complete pipeline
```r
source("R/00-main_pipeline.R")
```
This sources `R/config.R`, sets `skip_execution <- FALSE`, then runs all nine steps in sequence. Toggle individual steps by editing `pipeline_config` flags at the top of `00-main_pipeline.R`.

### Individual pipeline steps
```r
source("R/data_processing/01-data_cleaning.R")
source("R/data_processing/02-data_preparation.R")
source("R/analysis/03-rds_estimation.R")
source("R/analysis/04-bootstrap_analysis.R")
source("R/analysis/05-nsum_estimation.R")
source("R/analysis/05b-nsum_tau_sensitivity.R")
source("R/analysis/06-bayesian_appendix.R")   # only after bash step below
source("R/analysis/07-paper_figures.R")
source("R/analysis/08b-simple_subgroup_analysis.R")
source("R/analysis/09-create_latex_tables.R")
```

### Bayesian MA estimation (step 6 — overnight)
Step 6 has a prerequisite bash driver that must run first. Each `MA.estimates` call at N=50,000 takes ~2 hours; the full job runs overnight:
```bash
bash R/analysis/06-run-all.sh   # resumable; skips already-completed .RData files
```
Then consolidate in R:
```r
source("R/analysis/06-bayesian_appendix.R")
```

### Running tests
```r
source("R/tests/test_rds_functions.R")
source("R/tests/test_nsum_estimation.R")
source("R/tests/test_ma_extraction.R")
# quick smoke-test
source("R/tests/QUICK_TEST_nsum.R")
```

### Rendering the manuscript
```r
# Current draft
quarto::quarto_render("paper/SIR/manuscript-v4e.qmd")
```
Compiled PDFs, docx, and TeX outputs for each draft version are tracked in `paper/SIR/`.

## `skip_execution` flag pattern

Every analysis script checks `exists("skip_execution") && !skip_execution` before running its main body. When you `source()` a script directly for interactive use, the flag is unset so the script runs. When the master pipeline sets `skip_execution <- FALSE`, it explicitly enables execution. This lets scripts be sourced for their function definitions without triggering side effects.

## Key Technical Details

### Population Parameters
- UK domestic worker population: ~980,000 (EU data baseline; main scenario)
- NRM adult referrals baseline: 44,360
- Population scenarios tested: 50K, 100K, 980K, 1.74M

### CE's Comparable RDS/NSUM Indicators (2024-11-05 specification)
`create_comparable_indicators()` in `02-data_preparation.R` matches egocentric (RDS) to alter-centric (NSUM) indicators:

| Indicator | Confidence | Survey questions |
|---|---|---|
| `document_withholding_rds/nsum` | Highest | Q70 5f2 = Q71 5f3 |
| `pay_issues_rds/nsum` | High | Q39+Q42 = Q43 5b8 |
| `threats_abuse_rds/nsum` | High | Q45+Q47+Q48 = Q49 5c6 |
| `excessive_hours_rds/nsum` | Medium | Q61+Q62 = Q64 5d11 |
| `access_to_help_rds/nsum` | Lowest | Q78 (reverse coded) = Q79 5f11 |

All indicators are binary (0/1); RDS composites use logical OR; missing/"don't know" handled explicitly.

### NSUM Weighting (critical fix, 2025)
An earlier `Boot_Step2.R` had **inverted** Volz-Heckathorn weights (weight ∝ degree instead of ∝ 1/degree), producing a VH/RDS-II correlation of −0.49. The current `nsum_core_estimators.R` and `05-nsum_estimation.R` use standard RDS package functions for all weight calculations. Use `REGENERATE_NSUM_COMPREHENSIVE.R` (2–4 h) for publication; `REGENERATE_NSUM_ESTIMATES.R` (10–30 min) for quick checks.

### Risk Index Weights
- NRM referral indicator: 0.35
- Forced labor indicators: 0.55 total
- Below minimum wage: 0.10

### Network Analysis
- Recruiter-recruit relationships from RDS sampling
- Missing recruiter IDs stored as −1; zero-degree nodes handled explicitly
- Nationality clustering: Filipino, Latinx, British, Other

### Project Configuration (`R/config.R`)
```r
get_global_config()  # returns:
#   preferred_rds_method  = "RDS_SS"
#   preferred_nsum_method = "weighted"
#   population_sizes      = c(50000, 100000, 980000, 1740000)
#   main_population_size  = 980000
#   n_bootstrap           = 1000
#   parallel_cores        = 4
```

### Environment Setup
```r
install.packages(c("tidyverse", "janitor", "here", "RDS", "igraph",
                   "boot", "parallel", "ggplot2", "scales", "viridis", "sspse"))

# netclust (not on CRAN; R 4.5+ compatibility issues)
devtools::install_github('LJGamble/netclust')
# or install from CRAN/netclust_*.tar.gz in the vendored CRAN/ directory
```

## Debugging

| Symptom | Fix |
|---|---|
| `Error in here()`: project root not found | Ensure `CLAUDE.md` exists at repo root |
| Bootstrap convergence warnings | Adjust `bootstrap_samples`; check network topology |
| Memory issues | Reduce `population_sizes` to `c(50000, 100000)` or increase `parallel_cores` |
| `netclust` errors on R 4.5+ | Use `08b-simple_subgroup_analysis.R` instead, or install from source |
| Step 6 missing `.RData` files | Run `bash R/analysis/06-run-all.sh` first |

```r
# Verify outputs exist
list.files(here("output"), pattern = "\\.RData$")
list.files(here("output", "figures"))

# Check processed data
load(here("data", "processed", "prepared_data.RData"))
summary(prepared_data)
```
