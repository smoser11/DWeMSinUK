# NSUM Recalculation Workflow - Quick Start Guide

## Background

The previous NSUM estimates used custom manual weight implementations from `Boot_Step2.R` that had **reversed weighting patterns** (giving higher weights to people with larger networks instead of lower weights). This led to incorrect population size estimates.

The corrected workflow now uses **standard RDS package functions** for all weight calculations.

## Two Workflow Options

### 1. **REGENERATE_NSUM_ESTIMATES.R** - Single Configuration
- Fast, focused analysis with one set of parameters
- Good for initial results or testing
- ~10-30 minutes runtime

### 2. **REGENERATE_NSUM_COMPREHENSIVE.R** - Multiple Configurations ⭐
- Tests multiple RDS weights, NSUM methods, population sizes
- Includes adjustment factors (Feehan & Salganik 2016)
- Creates comparative plots showing prevalence (%)
- ~2-4 hours runtime for full analysis
- **Recommended for publication**

## What Changed

### Before (Incorrect)
- **Manual VH weights**: `weight ∝ degree / (2 × n_edges)` ❌
- **Manual SS weights**: Custom successive sampling with outcome-based grouping ❌
- Correlation between manual VH and standard RDS-II: **-0.49** (inverted!)

### After (Correct)
- **Standard RDS-II weights**: From `RDS.II.estimates()` (normalized) ✓
- **Standard RDS-SS weights**: From `RDS.SS.estimates()` (normalized) ✓
- **Standard RDS-I weights**: From `RDS.I.estimates()` (normalized) ✓

## Quick Start: Comprehensive Analysis (Recommended)

### Run All Configurations

```r
# Set working directory to project root
setwd("/Users/sm38679/Documents/GitHub/DWeMSinUK")

# Run comprehensive analysis
source("R/analysis/NSUM/REGENERATE_NSUM_COMPREHENSIVE.R")
```

**This will test:**
- **RDS Weights**: VH (RDS-II) and SS (RDS-SS)
- **NSUM Methods**:
  - MBSU with no adjustment (δ=1, τ=1, ρ=1)
  - MBSU conservative (δ=0.8, τ=0.7, ρ=1.0)
  - MBSU moderate (δ=0.9, τ=0.85, ρ=1.0)
  - GNSUM Symmetric
- **Population Sizes**: 50K, 100K, 980K, 1.74M
- **Total**: 32 configurations × 5 outcomes = 160 estimates

**Output:**
- `nsum_comprehensive_summary.csv` - Complete results table
- `prevalence_forest_*.png` - Forest plots by outcome (prevalence %)
- `prevalence_all_outcomes.png` - All outcomes comparison
- `population_sensitivity.png` - Population size sensitivity

---

## Quick Start: Single Configuration (Faster)

### Run with Default Settings

```r
# Set working directory to project root
setwd("/Users/sm38679/Documents/GitHub/DWeMSinUK")

# Run single configuration
source("R/analysis/NSUM/REGENERATE_NSUM_ESTIMATES.R")
```

This will:
1. ✓ Regenerate prepared data with correct weights
2. ✓ Generate 1,000 bootstrap samples (tree bootstrap)
3. ✓ Apply standard RDS-II/VH weights to bootstrap samples
4. ✓ Compute NSUM estimates for 5 indicators
5. ✓ Calculate bootstrap 95% confidence intervals
6. ✓ Save results to `output/nsum_corrected/`
7. ✓ Create forest plot visualization

**Runtime:** ~10-30 minutes depending on CPU

### Option 2: Customize Configuration

Edit the `CONFIG` section in `REGENERATE_NSUM_ESTIMATES.R`:

```r
CONFIG <- list(
  n_bootstrap = 500,                # Reduce for faster testing
  bootstrap_method = "tree",        # or "neighboot", "chain"
  rds_weight_method = "VH",         # "VH" (RDS-II), "SS", or "RDSI"
  population_size = 980000,         # UK domestic workers
  nsum_method = "mbsu",             # Modified Basic Scale-Up
  use_parallel = TRUE,              # Use multiple cores
  n_cores = 4,                      # Adjust for your CPU
  force_regenerate_data = TRUE,     # Regenerate weights
  force_regenerate_bootstrap = FALSE, # Reuse bootstrap samples if available
  force_reweight = TRUE             # Recompute weights
)
```

## Step-by-Step Workflow (Manual)

If you prefer to run each step separately:

### Step 1: Regenerate Prepared Data
```r
source("R/data_processing/01-data_cleaning.R")
clean_data()

source("R/data_processing/02-data_preparation.R")
prepare_data()

# Verify correct weights
load("data/processed/prepared_data.RData")
summary(dd$wt.vh_980k)  # Should be normalized (sum = 1)
```

### Step 2: Generate Bootstrap Samples
```r
library(here)
library(RDS)
source("R/analysis/NSUM/Boot_Step1.r")

load("data/processed/prepared_data.RData")

boot_samples <- bootstrap_rds_sample(
  rds_sample = rd.dd,
  method = "tree",
  B = 1000,
  traits = c("document_withholding_nsum", "pay_issues_nsum"),
  degree_col = "known_network_size",
  return_rds_df = FALSE,
  verbose = TRUE
)

save(boot_samples, file = "output/nsum_corrected/bootstrap_samples_step1.RData")
```

### Step 3: Apply Correct RDS Weights
```r
source("R/analysis/NSUM/Boot_Step2_FIXED.R")

weighted_samples <- compute_weights_batch_standard(
  bootstrap_samples = boot_samples,
  weight_method = "VH",  # RDS-II weights
  population_size = 980000,
  outcome_variable = "document_withholding_nsum",
  parallel = TRUE,
  n_cores = 4,
  verbose = TRUE
)

save(weighted_samples, file = "output/nsum_corrected/bootstrap_samples_weighted_step2.RData")
```

### Step 4: Compute NSUM Estimates
```r
source("R/analysis/NSUM/Boot_Step3.R")

# Point estimate on original data
nsum_estimate <- estimate_nsum(
  data = dd,
  nsum_method = "mbsu",
  outcome_variable = "document_withholding_nsum",
  degree_variable = "known_network_size",
  frame_size = 980000,
  weight_column = "inclusion_prob",
  verbose = TRUE
)

# Bootstrap estimates
boot_estimates <- sapply(weighted_samples, function(boot_sample) {
  result <- estimate_nsum(
    data = boot_sample,
    nsum_method = "mbsu",
    outcome_variable = "document_withholding_nsum",
    degree_variable = "known_network_size",
    frame_size = 980000,
    weight_column = "inclusion_prob",
    verbose = FALSE
  )
  return(result$estimate)
})

# Calculate confidence intervals
ci_95 <- quantile(boot_estimates, probs = c(0.025, 0.975), na.rm = TRUE)
```

## Output Files

After running `REGENERATE_NSUM_ESTIMATES.R`, you'll find:

```
output/nsum_corrected/
├── nsum_config.RDS                    # Configuration used
├── bootstrap_samples_step1.RData      # Bootstrap samples (Step 1)
├── bootstrap_samples_weighted_step2.RData  # Weighted samples (Step 2)
├── nsum_results_full.RDS              # Full results object
├── bootstrap_distributions.RDS         # Bootstrap distributions for each outcome
├── enhanced_nsum_results.RData        # Combined results (for paper)
├── nsum_estimates_summary.csv         # Summary table (Excel-ready)
└── nsum_forest_plot.png               # Visualization
```

## Comparing Old vs New Estimates

To compare the impact of the weight correction:

```r
# Load new estimates
load("output/nsum_corrected/enhanced_nsum_results.RData")
new_estimates <- summary_df

# Load old estimates (if available)
load("output/enhanced_nsum_results.RData")  # Old results
old_estimates <- summary_df

# Compare
comparison <- data.frame(
  outcome = new_estimates$outcome_variable,
  old_estimate = old_estimates$point_estimate,
  new_estimate = new_estimates$point_estimate,
  difference = new_estimates$point_estimate - old_estimates$point_estimate,
  pct_change = 100 * (new_estimates$point_estimate - old_estimates$point_estimate) / old_estimates$point_estimate
)

print(comparison)
```

## Sensitivity Analysis

Test different weight methods:

```r
# Test RDS-II (VH) weights
CONFIG$rds_weight_method <- "VH"
source("R/analysis/NSUM/REGENERATE_NSUM_ESTIMATES.R")

# Test RDS-SS weights
CONFIG$rds_weight_method <- "SS"
source("R/analysis/NSUM/REGENERATE_NSUM_ESTIMATES.R")

# Compare results
```

## Troubleshooting

### Issue: "Cannot find original RDS data"
**Solution:** Run Step 1 to regenerate prepared_data.RData

### Issue: Bootstrap samples fail
**Solution:** Check that rd.dd is a valid rds.data.frame object and has proper recruitment structure

### Issue: "Missing weight columns"
**Solution:** Ensure Boot_Step2_FIXED.R successfully added weights. Check first sample:
```r
names(weighted_samples[[1]])  # Should include weight_vh, weight_rds_i, weight_rds_ss, inclusion_prob
```

### Issue: Memory errors with 1000 bootstrap samples
**Solution:** Reduce `n_bootstrap` to 500 or 250, or disable parallel processing

## Next Steps

1. **Review Results**: Check `nsum_estimates_summary.csv`
2. **Compare with Previous**: Assess impact of weight correction
3. **Update Paper**: Use new estimates from `enhanced_nsum_results.RData`
4. **Sensitivity Analysis**: Try different weight methods (VH vs SS vs RDSI)
5. **Update RDS Estimates**: Run `03-rds_estimation.R` with standard package functions (already using correct weights)

## Key Files

- **Workflow Script**: `R/analysis/NSUM/REGENERATE_NSUM_ESTIMATES.R`
- **Fixed Weight Functions**: `R/analysis/NSUM/Boot_Step2_FIXED.R`
- **Updated Data Preparation**: `R/data_processing/02-data_preparation.R`
- **Bootstrap Generation**: `R/analysis/NSUM/Boot_Step1.r`
- **NSUM Estimation**: `R/analysis/NSUM/Boot_Step3.R`

## Questions?

- Check the inline comments in `REGENERATE_NSUM_ESTIMATES.R`
- Review `Boot_Step2_FIXED.R` for weight calculation details
- Compare with old `Boot_Step2.R` to see what changed
