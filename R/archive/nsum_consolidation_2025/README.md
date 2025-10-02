# NSUM Consolidation Archive (October 2025)

This directory contains the original NSUM analysis files that were consolidated and replaced with a modular, error-corrected structure on October 1, 2025.

## Archived Files

1. **nsum_adjustment_factors.r** (654 lines)
   - Original robust NSUM with Feehan & Salganik adjustment factors
   - Included survey bootstrap implementation
   - Mixed functionality: estimation + bootstrap + adjustment factors

2. **nsum_results_display.R** (602 lines)
   - Original visualization and display functions
   - Created GT tables and ggplot visualizations
   - Sourced nsum_adjustment_factors.r

## Issues Addressed

### Critical Errors Fixed:
1. ❌ **nsum_adjustment_factors.r**: Used `known_network_size` as degree variable but didn't properly use probe questions for d_F,F calculation
2. ❌ **nsum_results_display.R**: Line 462 called non-existent function `run_robust_nsum_adjustments()` (should be `run_robust_nsum_analysis()`)
3. ❌ **nsum_results_display.R**: Used `str_replace()` without loading stringr explicitly
4. ❌ **Both files**: No validation that required variables exist in data

### Design Issues Addressed:
1. ⚠️ **Mixed concerns**: Estimation, adjustment, bootstrap, and visualization all mixed together
2. ⚠️ **Computational expense**: ~10,000 combinations × 1,000 bootstrap = 10M calculations
3. ⚠️ **Verbose debugging**: First 3 bootstrap samples printed detailed output
4. ⚠️ **Unclear dependencies**: Files sourced each other in unclear ways

## New Structure (Option A: Hierarchical)

The functionality has been reorganized into four modular files:

### 1. **05-nsum_estimation.R** (716 lines)
**Purpose**: Core NSUM estimation using basic and weighted methods

**Key Features**:
- Clean, validated NSUM implementation
- Multiple RDS weighting schemes (RDS-I, VH, SS)
- Proper probe question handling for d_F,F calculation
- Input validation functions
- Warning system for placeholder probe sizes
- RDS vs NSUM comparison

**No longer includes**: Adjustment factors, bootstrap CIs (see other files)

### 2. **nsum_robust_adjustment.R** (443 lines)
**Purpose**: Extends core NSUM with Feehan & Salganik (2016) adjustment factors

**Key Features**:
- Adjustment factors: δ_F (degree ratio), τ_F (TPR), η_F (precision)
- Inclusion probability calculation for RDS weights
- Integrates with core NSUM via `estimate_nsum_population()`
- Reduced computational burden (key scenarios vs. exhaustive)
- Comprehensive results with RDS comparison

**Formula**: N_H = basic × (1/δ_F) × (1/τ_F) × η_F

### 3. **nsum_bootstrap.R** (501 lines)
**Purpose**: Bootstrap confidence intervals using survey bootstrap methods

**Key Features**:
- Simple bootstrap (with replacement)
- Survey bootstrap (Rust & Rao rescaled bootstrap)
- Automatic fallback when surveybootstrap package unavailable
- Works with both basic and adjusted NSUM
- Multiple indicators support
- Reduced debugging verbosity

**Fixed**: Bootstrap sample structure handling, error handling, package availability checks

### 4. **nsum_visualization.R** (566 lines)
**Purpose**: Publication-ready tables and visualizations

**Key Features**:
- Summary tables (with optional gt package)
- Sensitivity analysis tables
- RDS vs NSUM comparison tables
- Multiple plot types (estimates, comparison, sensitivity, CI plots)
- Master visualization function
- Automatic fallback to data frames when gt unavailable

**Fixed**: Function name errors, package dependencies, stringr usage

## Benefits of New Structure

1. ✅ **Modular**: Each file has single responsibility
2. ✅ **Debuggable**: Easier to identify and fix issues
3. ✅ **Testable**: Can test each component independently
4. ✅ **Maintainable**: Clear dependencies between files
5. ✅ **Efficient**: Reduced computational burden with key scenarios
6. ✅ **Validated**: Input validation prevents runtime errors
7. ✅ **Documented**: Clear comments and structure

## Usage

### Old way (archived):
```r
source("R/analysis/nsum_adjustment_factors.r")
source("R/analysis/nsum_results_display.R")
robust_results <- run_robust_nsum_analysis(dd)  # Computationally expensive
viz <- analyze_robust_nsum_results(dd)          # Called wrong function name
```

### New way (recommended):
```r
# Core NSUM
source("R/analysis/05-nsum_estimation.R")
nsum_results <- run_nsum_estimation()

# With adjustments (optional)
source("R/analysis/nsum_robust_adjustment.R")
robust_results <- run_robust_nsum_analysis(rd.dd)

# With bootstrap CIs (optional)
source("R/analysis/nsum_bootstrap.R")
bootstrap_results <- calculate_nsum_bootstrap_multiple(rd.dd)

# Visualization
source("R/analysis/nsum_visualization.R")
viz <- create_nsum_visualizations(
  robust_results = robust_results,
  bootstrap_results = bootstrap_results
)
```

## Migration Notes

If you have existing code that sources the old files:

1. Replace `source("nsum_adjustment_factors.r")` with:
   - `source("05-nsum_estimation.R")` for core NSUM
   - `source("nsum_robust_adjustment.R")` for adjustments

2. Replace `run_robust_nsum_adjustments()` with `run_robust_nsum_analysis()`

3. For bootstrap CIs: `source("nsum_bootstrap.R")`

4. For visualizations: `source("nsum_visualization.R")`

## Date Archived
October 1, 2025

## Reason for Consolidation
Claude Code analysis identified multiple critical errors and design issues that warranted a complete restructuring rather than incremental fixes.
