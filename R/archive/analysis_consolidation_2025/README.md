# Archive: Analysis Consolidation 2025

This directory contains the original analysis files that were consolidated into a cleaner 3-file structure on 2025-08-21.

## Original File Structure (14 files)

### Main Coordinators
- `03-rds_estimation.R` (219 lines) - Original RDS coordinator
- `05-comparison_analysis.R` (374 lines) - RDS vs NSUM comparison

### RDS Sub-modules 
- `03a-rds_basic_estimation.R` (225 lines) - Basic RDS-I/II/SS estimation
- `03b-rds_model_assisted.R` (261 lines) - Model-assisted estimation  
- `03c-rds_population_size.R` (357 lines) - Population size sensitivity
- `03d-rds_convergence.R` (291 lines) - Convergence diagnostics
- `03e-rds_bootstrap.R` (472 lines) - Bootstrap confidence intervals

### RDS Analysis & Output
- `03a_enhanced_analysis.R` (106 lines) - Method selection & comparison
- `03a_comparison_tables.R` (136 lines) - Publication tables
- `03a_visualizations.R` (225 lines) - Publication plots  
- `03a_sensitivity_analysis.R` (236 lines) - Robustness testing

### Bootstrap & NSUM
- `04-bootstrap_analysis.R` (71 lines) - Standalone bootstrap (neighborhood only)
- `04-nsum_estimation.R` (336 lines) - Comprehensive NSUM analysis
- `05-nsum_estimation.R` (200 lines) - Simpler NSUM implementation

## New Consolidated Structure (3 files)

### Current Analysis Files (in parent directory)
- `03-rds_estimation.R` - **Consolidated RDS analysis with preferred model focus**
  - Incorporates: Basic estimation, sensitivity analysis, convergence diagnostics  
  - Focus: RDS-SS as preferred method for main text
  - Appendices: Population size sensitivity, method comparison

- `04-bootstrap_analysis.R` - **Consolidated bootstrap analysis**
  - Incorporates: Neighborhood bootstrap + Tree bootstrap from RDS-specific bootstrap
  - Focus: Confidence intervals for preferred RDS-SS model
  - Methods: Both neighborhood and tree bootstrap with conservative CI approach

- `05-nsum_estimation.R` - **Consolidated NSUM analysis** 
  - Based on comprehensive version (original `04-nsum_estimation.R`)
  - Focus: Weighted NSUM as preferred method for main text
  - Comparison: Direct RDS vs NSUM using CE's comparable indicators

## Key Consolidation Changes

### Shared Functions
Moved to `R/utils/helper_functions.R`:
- `load_rds_results_database()` and `save_to_rds_database()`
- `create_parameter_id()` 
- `get_comparable_indicators()`
- `theme_rds_publication()` and `get_method_colors()`
- `format_estimate_with_ci()`

### Paper Structure Alignment
- **Main Text**: One preferred model each (RDS-SS, Weighted NSUM)
- **Appendix A**: RDS robustness (population size, method comparison) 
- **Appendix B**: Bootstrap confidence intervals and sensitivity analysis

### Benefits
1. **Cleaner structure**: 3 focused files vs 14 fragmented files
2. **Preferred model focus**: Clear hierarchy for main text vs appendices  
3. **Eliminated redundancy**: Shared functions centralized
4. **Consistent configuration**: Unified parameter management
5. **Better maintainability**: Single files per analysis type

## Migration Notes

- All functionality preserved
- Results caching maintained through shared database functions
- CE's comparable indicators specifications exactly preserved  
- Configuration-driven analysis maintained
- Bootstrap methods enhanced (combined neighborhood + tree)

## File Recovery

If specific functionality from the original files is needed:
1. Original files preserved exactly as they were
2. Function signatures and logic unchanged  
3. Can be referenced for implementation details
4. Results should be identical when using same parameters