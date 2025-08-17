# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This repository contains a research project analyzing domestic worker exploitation and modern slavery in the UK using Network Scale-Up Method (NSUM) and Respondent-Driven Sampling (RDS). The project implements multiple statistical estimation methods to measure prevalence of labor exploitation among UK domestic workers.

## Architecture and Code Structure

### Core Directory Structure
```
DWeMSinUK/
├── R/                          # All R analysis scripts
│   ├── data_processing/        # Data cleaning and preparation
│   ├── analysis/              # Statistical estimation methods
│   ├── utils/                 # Helper functions and utilities
│   └── ScratchWork/           # Experimental/development scripts
├── data/                      # Data files (raw and processed)
│   ├── raw/                   # Original survey data
│   └── processed/             # Cleaned datasets (.RData files)
├── paper/                     # Academic papers and manuscripts
├── output/                    # Analysis results and estimates
├── GNSUM/                     # G-NSUM specific materials
├── Replication/               # External replication studies
└── Notes/                     # Research notes and documentation
```

### Key Analysis Files
- `R/data_processing/01-data_cleaning.R` - Data import and cleaning pipeline
- `R/data_processing/02-data_preparation.R` - Network variable creation and RDS formatting
- `R/analysis/03-rds_estimation.R` - RDS-I, RDS-II, and RDS-SS estimation methods
- `R/analysis/04-bootstrap_analysis.R` - Neighborhood and tree bootstrap implementations
- `R/analysis/05-nsum_estimation.R` - Network scale-up method estimates
- `R/utils/helper_functions.R` - Shared utility functions

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

### Running Analysis
```r
# Load and run complete analysis pipeline
source("R/data_processing/01-data_cleaning.R")
source("R/data_processing/02-data_preparation.R")
source("R/analysis/03-rds_estimation.R")

# Run specific estimation methods
run_rds_estimates()    # Multiple RDS estimators
run_bootstrap_analysis()  # Bootstrap confidence intervals
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

### Comparable RDS/NSUM Indicators
The `create_comparable_indicators()` function in `R/01-Data.R` creates matched indicators for fair comparison between RDS (egocentric) and NSUM (alter-centric) estimation methods:

**High Confidence Pairs:**
- `document_withholding_rds/nsum`: Q70 vs Q71 (document control)
- `pay_issues_rds/nsum`: Q39+Q42 vs Q43 (debt/pay withholding)
- `threats_abuse_rds/nsum`: Q45+Q47+Q48 vs Q49 (force/threats/abuse)

**Lower Confidence Pairs:**
- `excessive_hours_rds/nsum`: Q61+Q62 vs Q64 (work hours/rest)
- `access_to_help_rds/nsum`: Q78rev vs Q79 (access to support)

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

## Development Notes

- Use `.RData` format for processed datasets to preserve R object structures
- Bootstrap methods require careful handling of network topology
- Missing data ("don't know" responses) requires special treatment in estimation
- Consider separate analyses for Filipino subgroup due to distinct network patterns
- All estimation results should include confidence intervals and uncertainty measures