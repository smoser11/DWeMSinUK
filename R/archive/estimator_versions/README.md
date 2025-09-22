# Archived Estimator Analysis Versions

This directory contains previous versions of MA estimator and sensitivity analysis files that were consolidated on 2025-09-22.

## Archived Files

### MODULAR Estimator Analysis Evolution
- `06-MODULAR_estimator_analysis.R` - Original modular implementation
- `06-MODULAR_estimator_analysis_v1.R` - First iteration with improvements
- `06-MODULAR_estimator_analysis_v2.R` - Second iteration with enhanced parameters
- `06-MODULAR_estimator_analysis_v3.R` - Third iteration with production parameters
- **→ v4 became production file: `06-modular_estimator_analysis.R`**

### Enhanced Appendix Sensitivity Analysis Evolution
- `06-appendix_sensitivity_analysis.R` - Basic sensitivity analysis
- `06-enhanced_appendix_sensitivity.R` - Enhanced with bootstrap CIs
- `06-enhanced_appendix_sensitivity_CORRECTED.R` - Corrected implementation
- **→ FINAL became production file: `06-bayesian_sensitivity_analysis.R`**

## Current Production Files (in R/analysis/)
1. **`06-modular_estimator_analysis.R`** - Main modular estimator with individual functions for each method
2. **`06-bayesian_sensitivity_analysis.R`** - Proper Bayesian implementation with convergence diagnostics
3. **`06-RESULTS_analysis_comparison.R`** - Results comparison utilities

## Key Improvements in Final Versions
- **MA.estimates()** with production parameters (50K MPLE, 2^19 MCMC steps)
- Proper Bayesian uncertainty quantification (no inappropriate bootstrap)
- Enhanced convergence diagnostics for MCMC methods
- Modular functions for each estimator method
- Clean execution control with skip_execution flags
- Comprehensive result recording and metadata

## Consolidation Rationale
- **v4** had the most recent improvements and production-ready parameters
- **FINAL_bayesian** had the best Bayesian implementation practices
- Multiple intermediate versions created redundancy and confusion
- Simplified naming scheme improves maintainability

**Date:** September 22, 2025  
**Action:** Consolidation of estimator analysis files for production use