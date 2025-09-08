# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This repository contains a research project analyzing domestic worker exploitation and modern slavery in the UK using Network Scale-Up Method (NSUM) and Respondent-Driven Sampling (RDS). The project implements multiple statistical estimation methods to measure prevalence of labor exploitation among UK domestic workers.

## Architecture and Code Structure

### Core Directory Structure
```
DWeMSinUK/
├── R/                          # All R analysis scripts
│   ├── 00-main_pipeline.R     # Master analysis pipeline
│   ├── data_processing/        # Data cleaning and preparation
│   │   ├── 01-data_cleaning.R  # Raw data import and cleaning
│   │   └── 02-data_preparation.R # Comparable indicators and RDS formatting
│   ├── analysis/              # Statistical estimation methods
│   │   ├── 03-rds_estimation.R # RDS-I/II/SS estimation and bootstrap
│   │   ├── 04-nsum_estimation.R # Network scale-up methods
│   │   └── 05-comparison_analysis.R # RDS vs NSUM comparison
│   ├── utils/                 # Helper functions and utilities
│   │   └── helper_functions.R # Shared utility functions
│   └── archive/               # Archived old/experimental scripts
│       ├── old_versions/      # Previous script versions
│       ├── experiments/       # Experimental scripts
│       └── scratch_work/      # Daily work files
├── data/                      # Data files (raw and processed)
│   ├── raw/                   # Original survey data
│   └── processed/             # Cleaned datasets (.RData files)
├── output/                    # Analysis results and outputs
│   ├── figures/               # All plots and visualizations
│   ├── tables/                # Summary tables and estimates
│   └── reports/               # Generated analysis reports
├── paper/                     # Academic papers and manuscripts
├── GNSUM/                     # G-NSUM specific materials
├── Replication/               # External replication studies
└── Notes/                     # Research notes and documentation
```

### Key Analysis Files
- `R/00-main_pipeline.R` - Master script that runs the complete analysis pipeline
- `R/data_processing/01-data_cleaning.R` - Raw data import, cleaning, and network size calculation
- `R/data_processing/02-data_preparation.R` - CE's comparable indicators and RDS weight calculation
- `R/analysis/03-rds_estimation.R` - RDS-I/II/SS estimation with configurable bootstrap confidence intervals
- `R/analysis/04-bootstrap_analysis.R` - Standalone bootstrap analysis (can be integrated or separate)
- `R/analysis/05-nsum_estimation.R` - Network scale-up method population estimates
- `R/analysis/06-*.R` - Extended sensitivity analysis and method comparison scripts  
- `R/analysis/07-*.R` - Convergence diagnostics and visualization utilities
- `R/utils/helper_functions.R` - Shared utility functions with proper path handling

### Data Processing Pipeline
1. Raw survey data is imported from `data/raw/`
2. Data cleaning applies standardized column names and validation
3. Network variables are calculated (degree measures, recruiter relationships)
4. Processed data is saved as `.RData` files in `data/processed/`
5. Multiple estimation methods are applied with results saved to `output/`

### Statistical Methods Implemented
- **RDS Estimators**: RDS-I, RDS-II, RDS-SS for different population sizes
- **Bootstrap Methods**: Neighborhood bootstrap and tree bootstrap for confidence intervals
- **NSUM**: Network scale-up method for hidden population estimation
- **Risk Index**: 13-category risk assessment with weighted scoring

## Common Development Commands

### Running Complete Analysis Pipeline
```r
# Run entire analysis pipeline (recommended)
source("R/00-main_pipeline.R")

# Or run individual steps
source("R/data_processing/01-data_cleaning.R")
source("R/data_processing/02-data_preparation.R") 
source("R/analysis/03-rds_estimation.R")
source("R/analysis/04-bootstrap_analysis.R")
source("R/analysis/05-nsum_estimation.R")
```

### Running Specific Analysis Methods
```r
# Individual analysis functions (after data preparation)
rds_results <- run_rds_estimation()      # RDS estimates with bootstrap CIs
nsum_results <- run_nsum_estimation()    # NSUM population estimates
comparison <- run_comparison_analysis()  # RDS vs NSUM comparison
```

### Data Processing
```r
# Clean and prepare data
data <- read_clean_data()
processed_data <- process_network_data(data)

# Save processed data
save(processed_data, file = "data/processed/prepared_data.RData")
```

### Paper Generation
```r
# Render Quarto manuscripts
quarto::quarto_render("paper/IJOPM_paperDraft_202050817.qmd")
quarto::quarto_render("paper/BJMMcorner-draft.qmd")

# Render R Markdown
rmarkdown::render("paper/DWeMSinUK.Rmd")
```

## Important Technical Details

### Population Parameters
- UK domestic worker population estimate: ~980,000 (EU data baseline)
- NRM adult referrals baseline: 44,360
- Multiple population size scenarios tested: 50K, 100K, 980K, 1.74M

### Key Survey Questions Mapping
- Q70/Q71: Document withholding indicators
- Q39+Q42/Q43: Pay-related exploitation
- Q45+Q47+Q48/Q49: Abuse and threats
- Q61+Q62/Q64: Excessive working hours
- Q78/Q79: Access to help and support

### CE's Comparable RDS/NSUM Indicators (2024-11-05)
The `create_comparable_indicators()` function in `R/data_processing/02-data_preparation.R` implements CE's exact specifications for matched indicators between RDS (egocentric) and NSUM (alter-centric) estimation methods:

**Highest Confidence:**
- `document_withholding_rds/nsum`: Q70 5f2 = Q71 5f3 (document control)

**High Confidence:**
- `pay_issues_rds/nsum`: Q39 5b4 and Q42 5b7 = Q43 5b8 (debt/pay withholding)  
- `threats_abuse_rds/nsum`: Q45 5c2 and Q47 5c4 and Q48 5c5 = Q49 5c6 (force/threats/abuse)

**Medium Confidence:**
- `excessive_hours_rds/nsum`: Q61 5d8 and Q62 5d9 = Q64 5d11 (work hours/rest, missing annual leave in RDS)

**Lowest Confidence:**
- `access_to_help_rds/nsum`: Q78 5f10 (reverse coded) = Q79 5f11 (access to support)

**Additional Network Variable:**
- `known_network_size`: Q13 2f (number of domestic workers with contact details)

All indicators are binary (0/1) with logical OR for composite RDS measures and proper handling of missing/"don't know" responses.

### Risk Index Weights
- NRM referral indicator: 0.35
- Forced labor indicators: 0.55 total
- Below minimum wage: 0.10

### Network Analysis
- Uses recruiter-recruit relationships from RDS sampling
- Handles zero-degree nodes and missing network connections
- Implements nationality clustering (Filipino, Latinx, British, Other)
- Validates network size claims against survey responses

### Environment Setup
```r
# Install required packages if not available
install.packages(c("tidyverse", "janitor", "here", "RDS", "igraph", 
                   "boot", "parallel", "ggplot2", "scales", "viridis"))

# Load project with proper path setup
library(here)
source(here("R", "utils", "helper_functions.R"))
setup_project_environment()
```

### Project Configuration
The main pipeline uses a configuration-driven approach:
- Pipeline settings in `R/00-main_pipeline.R` control which analyses run:
  ```r
  pipeline_config <- list(
    run_data_cleaning = TRUE,
    run_data_preparation = TRUE, 
    run_rds_estimation = TRUE,
    run_bootstrap_analysis = FALSE,  # Integrated in RDS by default
    run_nsum_estimation = TRUE,
    preferred_rds_method = "RDS_SS",
    n_bootstrap = 1000,
    population_sizes = c(50000, 100000, 980000, 1740000),
    main_population_size = 980000
  )
  ```
- Individual analysis configuration in each `R/analysis/*.R` script
- Skip execution flags prevent scripts from running when sourced
- Results are cached to avoid recomputation via `force_recompute = FALSE` parameters

### Data Requirements
- Survey data must be in `data/raw/` or `data/survey/`
- Expected data files: CSV or Excel formats with RDS network structure
- Required variables: recruiter.id, id, and survey question columns (Q##)
- Network validation handles missing recruiter IDs (-1) and zero-degree nodes

## Development Notes

### Code Organization (August 2025 Reorganization)
- **Modular Design**: Each analysis script can run independently after data preparation
- **Path Management**: All file paths use `here()` package for reproducibility 
- **CE's Specifications**: Comparable indicators implemented exactly per 2024-11-05 correspondence
- **Sequential Focus**: RDS analysis first (03), then NSUM (04), then comparison (05)
- **Archived Scripts**: Old/experimental code preserved in `R/archive/` with documentation

#### Modular Architecture
The analysis structure uses a sequential, coordinated approach:
- `03-rds_estimation.R` - Main RDS estimation with comprehensive configuration system
- `04-bootstrap_analysis.R` - Bootstrap confidence intervals (can be integrated or standalone)
- `05-nsum_estimation.R` - Network scale-up method estimation
- `06-*.R` - Extended analysis scripts for sensitivity testing and comparisons
- `07-*.R` - Convergence diagnostics and visualization

Analysis modules can be selectively enabled/disabled via configuration flags in each coordinator script for faster development cycles. The pipeline supports both sequential execution and individual module runs.

#### Extended Analysis Capabilities
Recent extensions provide additional analysis options:
- `06-*.R` scripts: Multiple versions for sensitivity analysis, Bayesian approaches, and modular estimator comparisons
- `07-*.R` scripts: Convergence diagnostics and enhanced visualization capabilities
- Modular estimator analysis with version control (v1, v2, v3, v4) for iterative development
- Enhanced appendix materials generation for academic publications

### Technical Requirements
- Use `.RData` format for processed datasets to preserve R object structures
- Bootstrap methods require careful handling of network topology  
- Missing data ("don't know" responses) requires special treatment in estimation
- Consider separate analyses for Filipino subgroup due to distinct network patterns
- All estimation results should include confidence intervals and uncertainty measures
- Path handling with `here()` package ensures reproducibility across systems

### Output Structure
- Results: `output/` (`.RData` files)
- Tables: `output/tables/` (`.csv` files) 
- Figures: `output/figures/` (`.png` files)
- Reports: `output/reports/` (`.txt` files)

### Debugging and Troubleshooting

#### Common Issues
- **"Error in here()": Project root not detected** - Ensure `CLAUDE.md` exists in project root
- **Bootstrap convergence warnings** - Adjust `bootstrap_samples` parameter or check network topology
- **Memory issues with large populations** - Reduce population size scenarios or use parallel processing
- **Missing RDS network connections** - Validate recruiter.id column for -1 values and network structure

#### Checking Results
```r
# Verify pipeline completion
list.files(here("output"), pattern = "*.RData")

# Check processed data integrity  
load(here("data", "processed", "prepared_data.RData"))
summary(prepared_data)

# Review convergence diagnostics
readRDS(here("output", "rds_convergence_diagnostics.RDS"))
```

#### Performance Optimization
- Set `force_recompute = FALSE` to use cached results (available in most analysis scripts)
- Adjust `parallel_cores` parameter for bootstrap methods (default: 4)
- Use smaller population size scenarios for testing: `c(50000, 100000)`
- Disable expensive analyses in configuration: `run_model_assisted = FALSE`, `run_bootstrap_analysis = FALSE`
- Use pipeline configuration to skip completed steps during development