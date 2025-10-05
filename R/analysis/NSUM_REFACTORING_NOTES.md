# NSUM Refactoring Summary

## Date: 2025-01-XX (Session continued from context overflow)

## Objective
Refactor NSUM estimation code into a modular architecture that supports:
1. Point estimates (on original data)
2. Bootstrap estimates (on resampled data)
3. Multiple RDS weighting schemes
4. Multiple NSUM estimators (MBSU, GNSUM, etc.)
5. Proper Horvitz-Thompson weighting with inclusion probabilities (π_i)

## Files Created/Modified

### 1. **R/analysis/nsum_core_estimators.R** (NEW FILE)

**Purpose:** Core NSUM estimation functions that can be used by both:
- `05-nsum_estimation.R` for point estimates
- `Boot_Step3.R` for bootstrap confidence intervals

**Key Functions:**
- `estimate_mbsu()` - Modified Basic Scale-Up with π_i weighting and adjustment factors
- `estimate_basic_nsum()` - Traditional unweighted NSUM
- `estimate_symmetric_gnsum()` - Placeholder for symmetric GNSUM
- `validate_nsum_variables()` - Input validation
- `convert_weights_to_pi()` - Convert RDS weights to inclusion probabilities
- `test_nsum_estimators()` - Testing utility

**Key Features:**
```r
estimate_mbsu(
  data,
  hidden_var,                         # Outcome variable name
  degree_var = "known_network_size",
  N_F = 980000,
  weights = NULL,                     # Option 1: Pass RDS weights as vector
  pi_column = NULL,                   # Option 2: Read π_i from data column
  use_inclusion_probs = TRUE,
  adjustment_factors = list(          # Feehan & Salganik 2016 adjustments
    delta = 1.0,                      # Transmission bias
    tau = 1.0,                        # Barrier effect
    rho = 1.0                         # Popularity bias
  ),
  verbose = FALSE
)
```

**Inclusion Probability Methods:**
1. **Calculate from weights** (for point estimates):
   ```r
   w_norm <- weights / sum(weights)
   pi_i <- w_norm * n
   ```

2. **Read from data column** (for bootstrap samples):
   ```r
   pi_i <- data[[pi_column]]
   ```

3. **Unweighted** (simple random sampling):
   ```r
   pi_i <- rep(1, n)
   ```

**Horvitz-Thompson Estimation:**
```r
y_F_H = Σ(y_i / π_i) / Σ(1 / π_i)
d_F_F = Σ(d_i / π_i) / Σ(1 / π_i)
N_H_basic = (y_F_H / d_F_F) × N_F
N_H_adjusted = N_H_basic × (1/δ) × (1/τ) × ρ
```

### 2. **R/analysis/05-nsum_estimation.R** (MODIFIED)

**Changes:**
1. Added source line for core estimators:
   ```r
   source(here("R", "analysis", "nsum_core_estimators.R"))
   ```

2. Replaced `estimate_nsum_population()` with thin wrapper:
   ```r
   estimate_nsum_population <- function(data, weights = NULL,
                                        hidden_connections_var, ...) {
     result <- estimate_mbsu(
       data = data,
       hidden_var = hidden_connections_var,
       weights = weights,
       use_inclusion_probs = TRUE,
       ...
     )
     # Add backward compatibility fields
     result$weighting_scheme <- weighting_scheme
     return(result)
   }
   ```

3. Fixed data object bug:
   - Changed `rd.dd` to `dd` in multiple places
   - Reason: Weights are stored in `dd`, not `rd.dd`

### 3. **R/analysis/NSUM/Boot_Step3.R** (MODIFIED)

**Changes:**
1. Added source line and saved references to core functions:
   ```r
   source(here("R", "analysis", "nsum_core_estimators.R"))

   # Save references before shadowing with wrappers
   core_estimate_mbsu <- estimate_mbsu
   core_estimate_basic_nsum <- estimate_basic_nsum
   core_estimate_symmetric_gnsum <- estimate_symmetric_gnsum
   ```

2. Replaced old `estimate_mbsu()` with wrapper:
   ```r
   estimate_mbsu <- function(data, outcome_variable, degree_variable,
                            frame_size, weight_column = "inclusion_prob",
                            adjustment_factors, ...) {
     # Translate parameter names
     result <- core_estimate_mbsu(
       data = data,
       hidden_var = outcome_variable,      # Name translation
       degree_var = degree_variable,
       N_F = frame_size,
       pi_column = weight_column,          # Use pre-calculated π_i
       adjustment_factors = adjustment_factors,
       ...
     )
     return(result)
   }
   ```

3. Wrapped loose execution code in `if (FALSE)` blocks to prevent errors when sourcing

## Testing Results

### Test 1: Core Estimator with Different Weight Schemes
```r
Document Withholding (N_F = 980,000):

unweighted     : N_H =   28,324 (2.89%)
ss_980k        : N_H =   10,249 (1.05%)  # 2.8× lower than unweighted
vh_980k        : N_H =   68,884 (7.03%)  # 2.4× higher than unweighted
```

**Key Finding:** Different weight schemes now produce meaningfully different estimates, confirming proper π_i-based weighting is working.

### Test 2: Boot_Step3.R Integration
```
Sample size: 50
Using pre-calculated π_i from column: inclusion_prob
y_F,H: 0.2368
d_F,F: 31.02
Basic estimate: 7,483
Adjusted estimate: 7,483
Method: MBSU (weighted, pre-calculated π_i)

✓ Boot_Step3.R successfully integrated!
```

## Architecture Benefits

### 1. **Separation of Concerns**
- **Core estimators** (nsum_core_estimators.R): Pure estimation logic
- **Point estimation** (05-nsum_estimation.R): Configuration and reporting
- **Bootstrap estimation** (Boot_Step3.R): Resampling and CI calculation

### 2. **Flexibility**
- Supports both calculated π_i (from weights) and pre-calculated π_i (from columns)
- Easy to add new estimators (just add to nsum_core_estimators.R)
- Adjustment factors can be varied for sensitivity analysis

### 3. **Backward Compatibility**
- Existing code continues to work via wrapper functions
- Return structures include aliases for different naming conventions
- Parameter name translation handled transparently

### 4. **Testability**
- Core functions can be tested independently
- Clear separation makes debugging easier
- Test utility (`test_nsum_estimators()`) included

## Next Steps (Optional)

1. **Create Bootstrap Pipeline Orchestrator** (`nsum_bootstrap_pipeline.R`)
   - Coordinate Boot_Step1 (resampling) → Boot_Step2 (reweighting) → Boot_Step3 (NSUM estimation)
   - Configuration-driven analysis
   - Parallel processing support

2. **Implement Full Symmetric GNSUM**
   - Currently a placeholder in `estimate_symmetric_gnsum()`
   - Requires additional network data for reciprocal ties

3. **Add Variance Estimation**
   - Analytical variance formulas for MBSU
   - Delta method for complex estimators

4. **Sensitivity Analysis Framework**
   - Grid search over adjustment factors (δ, τ, ρ)
   - Visualization of estimate sensitivity to assumptions

## Parameter Reference

### Adjustment Factors (Feehan & Salganik 2016)

**delta (δ)** - Transmission bias (0 < δ ≤ 1)
- Accounts for differential reporting by frame population about hidden population
- Lower δ = more underreporting
- Example: δ = 0.75 means 75% of connections are reported

**tau (τ)** - Barrier effect (0 < τ ≤ 1)
- Accounts for social mixing barriers between frame and hidden populations
- Lower τ = more barriers to connection
- Example: τ = 0.8 means 80% of potential connections exist

**rho (ρ)** - Popularity bias (ρ > 0)
- Accounts for differential degree distributions (hidden pop vs frame pop)
- ρ = degree_ratio = d_H,H / d_F,F
- Example: ρ = 0.9 means hidden pop has 90% the network size of frame pop

**Default (no adjustment):** δ = 1.0, τ = 1.0, ρ = 1.0

## Files Summary

**Created:**
- `R/analysis/nsum_core_estimators.R` (440 lines)

**Modified:**
- `R/analysis/05-nsum_estimation.R` (updated wrapper, fixed data objects)
- `R/analysis/NSUM/Boot_Step3.R` (integrated core estimators, wrapped test code)

**Output (from previous testing):**
- `output/nsum_estimation_results.RData`
- `output/tables/nsum_main_results.csv`
- `output/tables/rds_nsum_comparison.csv`

## References

- Feehan, D. M., & Salganik, M. J. (2016). Generalizing the network scale-up method: a new estimator for the size of hidden populations. *Sociological Methodology*, 46(1), 153-186.
- Volz, E., & Heckathorn, D. D. (2008). Probability based estimation theory for respondent driven sampling. *Journal of Official Statistics*, 24(1), 79.
- Gile, K. J. (2011). Improved inference for respondent-driven sampling data with application to HIV prevalence estimation. *Journal of the American Statistical Association*, 106(493), 135-146.
