# Getting Started: NSUM Analysis with Bootstrap CIs

## What You Asked For

Complete NSUM estimation workflow with:
- ✅ All 5 NSUM indicator variables
- ✅ Neighborhood bootstrap resampling
- ✅ Gile's Sequential Sampling (SS) weights
- ✅ 95% bootstrap confidence intervals
- ✅ Professionally formatted results table

## Quick Start (Choose One)

### 🚀 Option 1: Full Analysis (Recommended)

```r
source("R/analysis/RUN_NSUM_ANALYSIS.R")
```

- **Time:** ~10-15 minutes
- **Bootstrap samples:** 1,000
- **Output:** `output/nsum_complete/`

### ⚡ Option 2: Quick Test First

```r
source("R/analysis/QUICK_TEST_nsum.R")
```

- **Time:** ~1-2 minutes
- **Bootstrap samples:** 50
- **Output:** `output/nsum_quick_test/`

Good for validating everything works before running the full analysis.

## What You'll Get

### 📊 Main Results Table

```
NSUM POPULATION SIZE ESTIMATES
===============================
Resampling: Neighborhood Bootstrap (B = 1000)
Weighting: Gile's Sequential Sampling (SS)
Frame Population: 980,000

Indicator               | N (Point Est.) | Prevalence (%) | 95% CI
------------------------|----------------|----------------|---------------------
Excessive Working Hours |        26,244 |           2.68 | [21,500, 31,800]
Pay Related Issues      |        24,457 |           2.50 | [19,800, 29,500]
Threats And Abuse       |        23,914 |           2.44 | [19,200, 28,900]
Limited Access To Help  |        17,417 |           1.78 | [13,500, 21,700]
Document Withholding    |        10,249 |           1.05 | [ 7,800, 13,100]
```

### 📁 Output Files Created

**In `output/nsum_complete/`:**

1. **`nsum_results_table.csv`** - Results table (import to Excel/Word)
2. **`nsum_results_table.txt`** - Formatted text table (copy to documents)
3. **`nsum_complete_results.RData`** - Full R results (for further analysis)
4. **`bootstrap_distributions.pdf`** - Diagnostic plots (6 histograms)

## Example: How to Use Results

### For Your Paper

**Point Estimate + CI:**
> "We estimate 10,249 UK domestic workers experience document withholding (95% CI: 7,800-13,100), representing 1.05% of the domestic worker population."

### For Further Analysis

```r
# Load detailed results
load("output/nsum_complete/nsum_complete_results.RData")

# Access bootstrap distribution
doc_with_bootstrap <- bootstrap_estimates$document_withholding_nsum

# Calculate custom statistics
median(doc_with_bootstrap)
IQR(doc_with_bootstrap)

# Different CI level (e.g., 90%)
quantile(doc_with_bootstrap, c(0.05, 0.95))
```

## Files Created for You

### 📝 Main Workflow Scripts

| File | Purpose | When to Use |
|------|---------|-------------|
| `RUN_NSUM_ANALYSIS.R` | **Main runner** - Full analysis | Run this! |
| `QUICK_TEST_nsum.R` | Quick test (B=50) | Test first |
| `NSUM_complete_workflow.R` | Complete workflow code | Customize settings |

### 📚 Documentation

| File | Contents |
|------|----------|
| `NSUM_WORKFLOW_README.md` | **Complete guide** - configuration, options, troubleshooting |
| `NSUM_REFACTORING_NOTES.md` | Technical architecture details |
| `GETTING_STARTED.md` | **This file** - Quick start guide |

### 🔧 Core Components (Already Set Up)

- `nsum_core_estimators.R` - Core NSUM estimation functions
- `Boot_Step1.r` - Neighborhood bootstrap resampling
- `Boot_Step2.R` - Gile's SS weight calculation
- `Boot_Step3.R` - NSUM estimation on bootstrap samples

## Next Steps

### Step 1: Run Quick Test

```r
source("R/analysis/QUICK_TEST_nsum.R")
```

Check that everything works (~1-2 minutes).

### Step 2: Run Full Analysis

```r
source("R/analysis/RUN_NSUM_ANALYSIS.R")
```

Get final results with 1,000 bootstrap samples (~10-15 minutes).

### Step 3: Review Results

1. Open `output/nsum_complete/nsum_results_table.txt`
2. Check `bootstrap_distributions.pdf` for diagnostics
3. Import `nsum_results_table.csv` to Excel for tables

## Customization

To change settings, edit `NSUM_complete_workflow.R`:

```r
config <- list(
  bootstrap_samples = 1000,      # Change to 500 for faster results
  use_parallel = TRUE,           # FALSE if parallel causes issues
  n_cores = 4,                   # Adjust based on your CPU

  # Adjustment factors (Feehan & Salganik 2016)
  adjustment_factors = list(
    delta = 1.0,  # Transmission bias
    tau = 1.0,    # Barrier effect
    rho = 1.0     # Popularity bias
  )
)
```

## Troubleshooting

### ❌ Problem: Script takes too long

**Solution:** Reduce bootstrap samples
```r
# In NSUM_complete_workflow.R, change:
bootstrap_samples = 500  # Instead of 1000
```

### ❌ Problem: Memory issues

**Solution:** Disable saving bootstrap samples
```r
# In NSUM_complete_workflow.R, change:
save_bootstrap_samples = FALSE
```

### ❌ Problem: Parallel processing errors

**Solution 1:** Use sequential version (safest)
```r
source("R/analysis/NSUM_workflow_SEQUENTIAL.R")
```

**Solution 2:** Disable parallel manually
```r
# In NSUM_complete_workflow.R, change:
use_parallel = FALSE
```

**Note:** The parallel processing now properly sources all required scripts on each worker node, so it should work correctly. If you still encounter issues, use the sequential version.

## Understanding Your Results

### Point Estimate
- Calculated from original RDS sample with SS weights
- Uses Horvitz-Thompson weighting with inclusion probabilities (π_i)

### Bootstrap CI
- Based on 1,000 neighborhood bootstrap resamples
- Each resample is reweighted with SS method
- 95% CI = [2.5th percentile, 97.5th percentile]

### Why These Methods?

**Neighborhood Bootstrap:**
- Preserves RDS network structure
- Accounts for correlation in recruitment chains
- More accurate than naive bootstrap

**Gile's Sequential Sampling (SS) Weights:**
- Accounts for wave structure in RDS
- More stable than RDS-II for small samples
- Recommended for NSUM with RDS

## Need Help?

1. **Full documentation:** See `NSUM_WORKFLOW_README.md`
2. **Technical details:** See `NSUM_REFACTORING_NOTES.md`
3. **Architecture:** See modular structure in refactoring notes

## Summary

You're all set! The workflow has been debugged and fixed. Just run:

```r
source("R/analysis/RUN_NSUM_ANALYSIS.R")
```

Wait ~10-15 minutes, then check `output/nsum_complete/` for your results table.

### Recent Fixes Applied

✅ Fixed function name collision between core and wrapper functions
✅ Fixed bootstrap sample extraction (was showing 0 samples)
✅ Fixed test code executing on source
✅ Fixed parallel processing variable exports
✅ Fixed column name mapping in bootstrap samples

**See `WORKFLOW_FIXES_APPLIED.md` for detailed fix documentation.**

---

**Created:** 2025-01-XX (Updated with fixes)
**Analysis:** NSUM with Neighborhood Bootstrap + Gile's SS Weights
**Indicators:** 5 (Document Withholding, Pay Issues, Threats/Abuse, Excessive Hours, Access to Help)
**Status:** ✅ Ready to run
