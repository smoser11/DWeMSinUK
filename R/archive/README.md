# Archive Directory

This directory contains old, experimental, and duplicate R scripts from the DWeMSinUK project reorganization.

## Directory Structure

### `old_versions/`
Contains previous versions of scripts that have been consolidated:
- `01-Data.R` - Original data processing script (now consolidated into `data_processing/01-data_cleaning.R` and `02-data_preparation.R`)
- `02-Estimation.R`, `02b-Estimation_NSUM.R` - Original estimation scripts (now in `analysis/03-rds_estimation.R` and `04-nsum_estimation.R`)
- `03-ConsolodatedRDS_estimation.R` - Earlier RDS consolidation attempt
- `*_v0.R` - Version 0 files superseded by newer versions
- `03a-*` - Intermediate analysis scripts

### `experiments/`
Contains experimental and untitled scripts:
- `*Unite.R` - Experimental consolidation attempts
- `*Untitled*.R` - Untitled analysis experiments
- `seqsamp.R` - Sequential sampling experiments

### `scratch_work/`
Contains dated scratch work and testing files:
- `work*.R` - Daily work files with dates
- `testingBoot*.R` - Bootstrap method testing scripts

## Notes

- These files are preserved for reference but are not part of the current analysis pipeline
- The current organized structure is in the parent directories:
  - `data_processing/` - Data cleaning and preparation
  - `analysis/` - Statistical estimation methods
  - `utils/` - Helper functions

## Migration History

Reorganized on: August 2025
- Consolidated multiple versions into coherent workflow
- Implemented CE's comparable indicator specifications (2024-11-05)
- Added proper path management with `here` package
- Created modular, independent analysis scripts