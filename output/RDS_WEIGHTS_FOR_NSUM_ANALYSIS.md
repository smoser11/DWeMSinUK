# RDS Weights for NSUM Estimation Analysis
**Date:** October 2, 2025
**Objective:** Review RDS weight calculation methods for use as inclusion probabilities (π_i) in NSUM estimation

---

## Answer to Your Questions

### 1. Which RDS Estimator is Used in the Comparison?

**RDS-SS (Gile's Sequential Sampling estimator)**

From `output/rds_estimation_results.RData`:
```
$metadata$preferred_method
[1] "RDS_SS"
```

The RDS vs NSUM comparison table uses RDS-SS estimates at 980,000 population size.

### 2. Large Magnitude Difference: Expected or Problematic?

**This is EXPECTED and CORRECT** — but needs proper interpretation:

| Indicator | RDS (Egocentric) | NSUM (Population) | Interpretation |
|-----------|------------------|-------------------|----------------|
| Document withholding | 34.2% | 2.9% | RDS: 34% of *surveyed workers* experienced this personally |
| | | | NSUM: 2.9% of *all UK domestic workers* experience this |
| Pay issues | 49.7% | 4.7% | 10.7× difference |
| Threats/abuse | 57.2% | 4.7% | 12.2× difference |
| Excessive hours | 83.7% | 5.1% | 16.4× difference |
| Access to help | 59.4% | 4.0% | 14.9× difference |

**Why the large differences?**

1. **Different populations**:
   - RDS: Hard-to-reach sample (n=85) of domestic workers who *personally experienced* exploitation
   - NSUM: Estimates for *entire UK domestic worker population* (N=980,000)

2. **Different measurement**:
   - RDS (egocentric): "Have YOU experienced document withholding?"
   - NSUM (alter-centric): "How many domestic workers do you KNOW who experienced this?"

3. **Selection bias** (intentional):
   - RDS sample deliberately over-represents those experiencing exploitation (it's a study of exploitation!)
   - NSUM adjusts for this via network scale-up methodology

**Bottom line**: The order-of-magnitude difference suggests the RDS sample is enriched for exploitation (as expected). NSUM provides population-level prevalence estimates.

---

## RDS Weight Calculation Methods in Codebase

### A. In Data Preparation (`02-data_preparation.R`)

**Current implementation** (lines 155-180):

```r
dd <- dd %>%
  mutate(
    # RDS-I weights (outcome-specific)
    wt.RDS1_document_withholding = rds.I.weights(rd.dd, "document_withholding_rds"),
    wt.RDS1_pay_issues = rds.I.weights(rd.dd, "pay_issues_rds"),
    # ... etc.

    # Volz-Heckathorn weights (population-specific)
    wt.vh_980k = vh.weights(numRef, N = 980000),
    wt.vh_100k = vh.weights(numRef, N = 100000),
    # ... etc.

    # Gile's SS weights (population-specific)
    wt.SS_980k = gile.ss.weights(numRef, N = 980000),
    wt.SS_100k = gile.ss.weights(numRef, N = 100000),
    # ... etc.
  )
```

**⚠️ CRITICAL BUG IDENTIFIED:**
- Line 170-179 use `numRef` which is undefined
- Should be `rd.dd` (the RDS data frame object)
- This likely causes `vh.weights()` and `gile.ss.weights()` to fail or use wrong data

**Available weight columns in prepared data:**
```
wt.RDS1_document_withholding, wt.RDS1_pay_issues, wt.RDS1_threats_abuse,
wt.RDS1_excessive_hours, wt.RDS1_access_to_help, wt.RDS1_zQ36, wt.RDS1_zQ80,
wt.RDS1_sum_categories_factor, wt.vh_980k, wt.vh_100k, wt.vh_050k, wt.vh_1740k,
wt.SS_980k, wt.SS_100k, wt.SS_050k, wt.SS_1740k
```

**Sample weight values (first 5 observations):**
```
wt.SS_980k: 40495.22, 6749.20, 20247.61, 40495.22, 40495.22
wt.vh_980k: 40495.22, 6749.20, 20247.61, 40495.22, 40495.22
```

**Note**: Weights are unnormalized (don't sum to 1). This is expected for RDS package output.

### B. In Boot_Step2.R (Manual Calculations)

Boot_Step2.R provides comprehensive manual implementations:

1. **`compute_vh_weights()`** (lines 261-294)
   - Formula: π_i = d_i / (2 × m)
   - Where: d_i = degree, m = number of edges
   - Normalized to sum to 1

2. **`compute_enhanced_vh_weights()`** (lines 687-755)
   - Enhanced VH with finite population correction
   - Optional homophily adjustment
   - More sophisticated than basic VH

3. **`compute_rds_i_weights()`** (lines 305-353)
   - Salganik-Heckathorn estimator
   - Formula: w_i = 1 / d_i (inverse degree)
   - Normalized to sum to 1

4. **`compute_rds_ii_weights()`** (lines 366-435)
   - Volz-Heckathorn with homophily
   - Uses transition matrix for group-specific recruitment
   - Accounts for in-group vs cross-group recruitment patterns

5. **`compute_rds_ss_weights()`** (lines 547-611)
   - Gile's Sequential Sampling approximation
   - Accounts for changing sample composition over recruitment waves
   - Uses finite population correction and group representation adjustment

6. **`compute_successive_sampling_weights()`** (lines 617-681)
   - Core RDS-SS algorithm
   - Calculates weights sequentially by recruitment wave
   - Adjusts for over/under-representation of groups

---

## Inclusion Probabilities (π_i) for NSUM per Feehan & Salganik 2016

### Conceptual Framework

**In standard NSUM** (without RDS):
- Assume simple random sampling (SRS)
- All individuals have equal probability: π_i = n/N
- No weighting needed

**In RDS+NSUM hybrid** (your study):
- Sampling is NOT random — uses chain-referral
- Individuals have differential inclusion probabilities based on network size
- Must account for this via RDS weights

### Mathematical Relationship

**RDS weights ARE inclusion probability weights:**

For RDS estimation:
```
w_i = 1 / π_i × (normalization constant)
```

For NSUM with inclusion probabilities:
```
π_i = sampling probability of individual i
```

**Conversion:**
```r
# RDS package output (unnormalized weights)
rds_weights <- dd$wt.SS_980k  # e.g., [40495, 6749, 20247, ...]

# Convert to normalized weights (sum to 1)
normalized_weights <- rds_weights / sum(rds_weights)

# Convert to inclusion probabilities (inverse relationship)
inclusion_probs <- 1 / normalized_weights
inclusion_probs <- inclusion_probs / sum(inclusion_probs)

# OR equivalently, use normalized RDS weights directly as sampling weights
```

### Feehan & Salganik (2016) Framework

From their paper, the adjusted NSUM estimator is:

```
N̂_H = (1/n) Σ_i (y_i / π_i) × (d_F,i / d_H,i) × (1/τ_i) × η_i
```

Where:
- **y_i** = alter reports (number of hidden pop members known by i)
- **π_i** = inclusion probability (probability i is sampled)
- **d_F,i** = frame degree (network size in frame population)
- **d_H,i** = hidden degree (network size in hidden population)
- **τ_i** = true positive rate (reporting accuracy)
- **η_i** = precision (1 - false positive rate)

**For RDS+NSUM:**
```
π_i comes from RDS weights (not assumed equal)
```

---

## Recommendations for Next Steps

### 1. Fix `numRef` Bug in Data Preparation

**File:** `R/data_processing/02-data_preparation.R` (lines 170-179)

**Current (BROKEN):**
```r
wt.vh_980k = vh.weights(numRef, N = 980000),
wt.SS_980k = gile.ss.weights(numRef, N = 980000),
```

**Fix to:**
```r
wt.vh_980k = vh.weights(rd.dd, N = 980000),
wt.SS_980k = gile.ss.weights(rd.dd, N = 980000),
```

**Impact:** This might explain why all weighting schemes currently produce identical NSUM results.

### 2. Modify NSUM Estimation to Use Inclusion Probabilities

**File:** `R/analysis/05-nsum_estimation.R`

**Current approach:**
- Uses RDS weights directly as analysis weights
- Treats weights as frequency weights

**Correct approach per Feehan & Salganik:**
```r
# In estimate_nsum_population() function:

# STEP 1: Convert RDS weights to inclusion probabilities
if (!is.null(weights)) {
  # Normalize RDS weights to sum to 1
  normalized_weights <- weights / sum(weights, na.rm = TRUE)

  # Calculate inclusion probabilities (inverse of weights)
  # Larger weight = more likely sampled = higher π_i
  inclusion_probs <- normalized_weights * nrow(data_c)

  # For NSUM: weight inverse by inclusion probability
  # Individuals with higher π_i get DOWN-weighted
  # (they're over-represented in sample)
  nsum_weights <- 1 / inclusion_probs
  nsum_weights <- nsum_weights / sum(nsum_weights, na.rm = TRUE)

  # Calculate weighted alter reports
  y_FH <- sum(y_conn * nsum_weights, na.rm = TRUE) / sum(nsum_weights, na.rm = TRUE)

  # Calculate weighted network size
  d_FF <- sum(net_sizes * nsum_weights, na.rm = TRUE) / sum(nsum_weights, na.rm = TRUE)
}
```

### 3. Test Different Weight Schemes Properly

After fixing steps 1-2, re-run NSUM with:

**A. Different RDS estimators:**
- RDS-I weights: `wt.RDS1_document_withholding` (outcome-specific)
- VH weights: `wt.vh_980k` (population-specific)
- RDS-SS weights: `wt.SS_980k` (population-specific)

**B. Compare results:**
```
Indicator: Document Withholding
- Unweighted: 2.91%
- RDS-I weighted: ?
- VH weighted: ?
- RDS-SS weighted: ?
```

**C. Expected differences:**
- RDS-I: Adjusts only for degree (network size)
- VH: Adjusts for degree + finite population
- RDS-SS: Adjusts for degree + recruitment order + group composition

### 4. Implement Feehan & Salganik Adjustment Factors

**File:** `R/analysis/nsum_robust_adjustment.R` (already created)

Add proper inclusion probability integration:

```r
# Modified Basic Scale-Up with inclusion probabilities:
estimate_nsum_with_inclusion_probs <- function(data,
                                               inclusion_probs,
                                               hidden_var,
                                               degree_var,
                                               N_F) {

  # Calculate π_i-weighted estimates
  y_i <- data[[hidden_var]]
  d_i <- data[[degree_var]]
  pi_i <- inclusion_probs

  # Horvitz-Thompson style estimator
  y_F_H <- sum(y_i / pi_i, na.rm = TRUE) / sum(1 / pi_i, na.rm = TRUE)
  d_F_F <- sum(d_i / pi_i, na.rm = TRUE) / sum(1 / pi_i, na.rm = TRUE)

  # Basic scale-up
  N_H <- (y_F_H / d_F_F) * N_F

  # Apply Feehan & Salganik adjustments
  # δ_F (degree ratio), τ_F (true positive rate), η_F (precision)
  N_H_adjusted <- N_H * (1/delta_F) * (1/tau_F) * eta_F

  return(N_H_adjusted)
}
```

### 5. Create Comparison Table

**Output:** `output/tables/nsum_weight_method_comparison.csv`

| Indicator | Unweighted | RDS-I | VH | RDS-SS | Feehan-Salganik Adjusted |
|-----------|------------|-------|----|----|------------------------|
| Document withholding | 2.91% | ? | ? | ? | ? |
| Pay issues | 4.65% | ? | ? | ? | ? |
| ... | ... | ... | ... | ... | ... |

---

## Technical Notes

### RDS Package Weight Functions

From `RDS` package documentation:

1. **`rds.I.weights(rds.data, outcome.variable)`**
   - Salganik-Heckathorn (2004) estimator
   - Weights: w_i ∝ 1/d_i
   - Outcome-specific (different weights for each variable)

2. **`vh.weights(rds.data, N)`**
   - Volz-Heckathorn (2008) estimator
   - Weights: w_i = d_i / (2m) × FPC
   - Population-specific (same weights across variables)

3. **`gile.ss.weights(rds.data, N)`**
   - Gile (2011) Sequential Sampling estimator
   - Most sophisticated: accounts for sample evolution
   - Population-specific

**All return unnormalized weights** (don't sum to 1 or n)

### Weight Normalization Best Practices

```r
# For estimation (Horvitz-Thompson style):
normalized_weights <- weights / sum(weights)

# For design effect calculation:
effective_n <- (sum(weights))^2 / sum(weights^2)

# For inclusion probabilities (inverse):
pi_i <- (weights / sum(weights)) * n
```

---

## Current Status

✅ **Working:**
- Core NSUM estimation mathematically correct
- RDS weights calculated in prepared data
- Multiple weight schemes available

⚠️ **Issues:**
- `numRef` bug in data preparation (likely causes VH/SS weights to fail)
- Current NSUM implementation doesn't use inclusion probabilities correctly
- No adjustment factors implemented (degree ratio, reporting accuracy)

❌ **Not Yet Implemented:**
- Proper π_i-based weighting per Feehan & Salganik 2016
- Comparison across different RDS weight methods
- Sensitivity analysis for adjustment factors

---

## References

1. **Feehan, D. M., & Salganik, M. J. (2016).** Generalizing the network scale-up method: a new estimator for the size of hidden populations. *Sociological Methodology*, 46(1), 153-186.

2. **Gile, K. J. (2011).** Improved inference for respondent-driven sampling data with application to HIV prevalence estimation. *Journal of the American Statistical Association*, 106(493), 135-146.

3. **Salganik, M. J., & Heckathorn, D. D. (2004).** Sampling and estimation in hidden populations using respondent-driven sampling. *Sociological Methodology*, 34(1), 193-240.

4. **Volz, E., & Heckathorn, D. D. (2008).** Probability based estimation theory for respondent driven sampling. *Journal of Official Statistics*, 24(1), 79.

---

**Generated:** October 2, 2025
**Next Action:** Fix `numRef` bug and re-implement NSUM with proper inclusion probabilities
