# NSUM Complete Workflow Guide

## Overview

This workflow provides **complete NSUM population size estimation** with **bootstrap confidence intervals** for all 5 exploitation indicators.

**Configuration:**
- **Resampling Method:** Neighborhood Bootstrap
- **Weighting Scheme:** Gile's Sequential Sampling (SS)
- **Bootstrap Samples:** 1,000 (configurable)
- **Confidence Level:** 95%
- **Indicators:** All 5 NSUM variables

---

## Quick Start

### Option 1: Full Analysis (B=1000, ~10-15 minutes)

```r
source("R/analysis/RUN_NSUM_ANALYSIS.R")
```

This will:
1. Calculate point estimates with SS weights
2. Generate 1,000 neighborhood bootstrap samples
3. Reweight each sample with SS weights
4. Estimate NSUM for each bootstrap sample
5. Calculate 95% bootstrap CIs
6. Create formatted results table
7. Generate diagnostic plots

**Output:** `output/nsum_complete/`

### Option 2: Quick Test (B=50, ~1-2 minutes)

```r
source("R/analysis/QUICK_TEST_nsum.R")
```

Same workflow with only 50 bootstrap samples for quick validation.

**Output:** `output/nsum_quick_test/`

### Option 3: Custom Configuration

```r
# Load and modify the workflow
source("R/analysis/NSUM_complete_workflow.R")

# Results are returned invisibly - capture them
results <- source("R/analysis/NSUM_complete_workflow.R")$value

# Access components
results$formatted_table
results$bootstrap_estimates
```

---

## Configuration Options

Edit `NSUM_complete_workflow.R` to customize:

```r
config <- list(
  # Bootstrap parameters
  bootstrap_method = "neighboot",     # "neighboot", "tree", "chain", "ss"
  bootstrap_samples = 1000,           # Number of bootstrap replicates

  # Weighting
  weight_method = "SS",               # "SS", "VH", "RDSII", "RDSI"

  # Population
  frame_population = 980000,          # UK domestic workers

  # NSUM adjustments (Feehan & Salganik 2016)
  adjustment_factors = list(
    delta = 1.0,                      # Transmission bias (0 < δ ≤ 1)
    tau = 1.0,                        # Barrier effect (0 < τ ≤ 1)
    rho = 1.0                         # Popularity bias (ρ > 0)
  ),

  # Computational
  use_parallel = TRUE,                # Parallel processing
  n_cores = 4,                        # Number of cores

  # Output
  save_bootstrap_samples = FALSE      # Save bootstrap samples (large file)
)
```

---

## Output Files

After running the workflow, the following files are created:

### Main Results

**`nsum_results_table.csv`** - Main results table with:
- Indicator name
- Point estimate (N)
- Prevalence (%)
- 95% CI [lower, upper]
- Bootstrap mean
- Bootstrap SD

**`nsum_results_table.txt`** - Formatted text table for reports

**`nsum_complete_results.RData`** - Complete R objects:
- `point_estimates` - Point estimate details for each indicator
- `bootstrap_estimates` - Bootstrap distributions (1000 estimates per indicator)
- `results_table` - Numeric results table
- `formatted_table` - Pretty formatted table
- `config` - Analysis configuration

### Diagnostic Plots

**`bootstrap_distributions.pdf`** - Histograms showing:
- Bootstrap distribution for each indicator
- Point estimate (red line)
- 95% CI bounds (green dashed lines)

---

## Expected Output Format

### Console Output

```
NSUM POPULATION SIZE ESTIMATES
===============================
Resampling: Neighborhood Bootstrap (B = 1000)
Weighting: Gile's Sequential Sampling (SS)
Frame Population: 980,000
Confidence Level: 95%

┌─────────────────────────┬───────────────┬──────────────┬──────────────────────┬────────────────┬───────────────┐
│ Indicator               │ N (Point Est.)│ Prevalence (%)│ 95% CI              │ Bootstrap Mean │ Bootstrap SD  │
├─────────────────────────┼───────────────┼──────────────┼──────────────────────┼────────────────┼───────────────┤
│ Excessive Working Hours │        26,244 │         2.68 │ [21,500, 31,800]    │         26,150 │         2,650 │
│ Pay Related Issues      │        24,457 │         2.50 │ [19,800, 29,500]    │         24,300 │         2,480 │
│ Threats And Abuse       │        23,914 │         2.44 │ [19,200, 28,900]    │         23,800 │         2,470 │
│ Limited Access To Help  │        17,417 │         1.78 │ [13,500, 21,700]    │         17,250 │         2,080 │
│ Document Withholding    │        10,249 │         1.05 │ [ 7,800, 13,100]    │         10,150 │         1,340 │
└─────────────────────────┴───────────────┴──────────────┴──────────────────────┴────────────────┴───────────────┘
```

---

## Understanding the Results

### Point Estimate
- **N (Point Est.):** NSUM population size estimate using SS weights on original sample
- **Prevalence (%):** N / Frame Population × 100

### Bootstrap Statistics
- **Bootstrap Mean:** Mean of 1,000 bootstrap estimates (should be close to point estimate)
- **Bootstrap SD:** Standard deviation of bootstrap distribution (measure of uncertainty)
- **95% CI:** Percentile confidence interval [2.5th percentile, 97.5th percentile]

### Interpretation

**Example:** Document Withholding
- Point Estimate: 10,249 workers
- 95% CI: [7,800, 13,100]
- **Interpretation:** We estimate 10,249 UK domestic workers experience document withholding (1.05% prevalence). We are 95% confident the true number is between 7,800 and 13,100.

---

## Advanced Usage

### Access Bootstrap Distributions

```r
# Run workflow
results <- source("R/analysis/NSUM_complete_workflow.R")$value

# Get bootstrap distribution for specific indicator
doc_with_boot <- results$bootstrap_estimates$document_withholding_nsum

# Summary statistics
mean(doc_with_boot)
sd(doc_with_boot)
quantile(doc_with_boot, c(0.025, 0.975))

# Plot
hist(doc_with_boot, breaks=50, main="Document Withholding Bootstrap Distribution")
```

### Custom CI Levels

```r
# 90% CI
quantile(doc_with_boot, c(0.05, 0.95))

# 99% CI
quantile(doc_with_boot, c(0.005, 0.995))
```

### Sensitivity Analysis (Adjustment Factors)

```r
# Test different adjustment factors
config$adjustment_factors <- list(
  delta = 0.75,  # 75% transmission (25% underreporting)
  tau = 0.80,    # 80% mixing (20% barriers)
  rho = 0.90     # Hidden pop 10% smaller networks
)

# Re-run with adjustments
source("R/analysis/NSUM_complete_workflow.R")
```

---

## Workflow Architecture

### Three-Step Bootstrap Procedure

```
Step 1: RESAMPLE (Boot_Step1.r)
  └─ bootstrap_rds_sample() → 1000 resampled datasets

Step 2: REWEIGHT (Boot_Step2.R)
  └─ compute_rds_weights() → Add SS weights & π_i to each sample

Step 3: ESTIMATE (Boot_Step3.R + nsum_core_estimators.R)
  └─ estimate_nsum() → NSUM estimate for each sample
     └─ estimate_mbsu() → Core Horvitz-Thompson estimation
```

### File Dependencies

```
NSUM_complete_workflow.R
  ├─ Requires: nsum_core_estimators.R (core estimation)
  ├─ Requires: Boot_Step1.r (resampling)
  ├─ Requires: Boot_Step2.R (reweighting)
  └─ Requires: Boot_Step3.R (NSUM on bootstrap samples)
```

---

## Troubleshooting

### Memory Issues

If you encounter memory issues with B=1000:

```r
# Reduce bootstrap samples
config$bootstrap_samples <- 500

# Disable saving bootstrap samples
config$save_bootstrap_samples <- FALSE
```

### Slow Performance

**Option 1:** Enable parallel processing (already enabled by default)
```r
config$use_parallel <- TRUE
config$n_cores <- 4  # Adjust based on your CPU
```

**Option 2:** If parallel causes issues, use sequential version
```r
source("R/analysis/NSUM_workflow_SEQUENTIAL.R")
```

**Option 3:** Reduce bootstrap samples for testing
```r
config$bootstrap_samples <- 100
```

**Note:** Parallel processing is now fixed to properly load all required functions on worker nodes.

### Missing Packages

```r
# Install required packages
install.packages(c("RDS", "RDStreeboot", "Neighboot", "surveybootstrap"))
```

### Convergence Warnings

If you see warnings about RDS weight calculation:
- This is normal for small bootstrap samples
- Weights fall back to alternative methods automatically
- Results are still valid

---

## Computing Time

**Approximate runtime (Intel i5, 4 cores):**

| Bootstrap Samples | Sequential | Parallel (4 cores) |
|-------------------|------------|--------------------|
| B = 50            | ~1 min     | ~30 sec            |
| B = 100           | ~2 min     | ~1 min             |
| B = 500           | ~8 min     | ~4 min             |
| B = 1000          | ~15 min    | ~8 min             |

**Steps:**
- Step 1 (Resampling): ~10% of total time
- Step 2 (Reweighting): ~40% of total time
- Step 3 (NSUM): ~50% of total time

---

## Citation

If you use this workflow, please cite:

**Methods:**
- Feehan, D. M., & Salganik, M. J. (2016). Generalizing the network scale-up method: a new estimator for the size of hidden populations. *Sociological Methodology*, 46(1), 153-186.
- Gile, K. J. (2011). Improved inference for respondent-driven sampling data with application to HIV prevalence estimation. *Journal of the American Statistical Association*, 106(493), 135-146.
- Baraff, A. J., McCormick, T. H., & Raftery, A. E. (2016). Estimating uncertainty in respondent‐driven sampling using a tree bootstrap method. *Proceedings of the National Academy of Sciences*, 113(51), 14668-14673.

**Software:**
- RDS package: Handcock, M. S., Fellows, I. E., & Gile, K. J. (2024). RDS: Respondent-Driven Sampling. R package.
- Neighboot package: Neighborhood bootstrap for RDS

---

## Contact

For questions about this workflow:
- Check `NSUM_REFACTORING_NOTES.md` for architecture details
- Review `nsum_core_estimators.R` for estimation methods
- See `Boot_Step1.r`, `Boot_Step2.R`, `Boot_Step3.R` for bootstrap procedures
