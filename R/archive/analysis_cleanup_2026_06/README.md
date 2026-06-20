# Analysis cleanup archive — June 2026

These scripts were superseded during the 2026-06 cleanup that flattened
the multi-generation `06*` and `07*` families into a single coherent
numbered pipeline. They are preserved here for historical reference and
are NOT maintained for executability.

## What was archived and why

### 06-RESULTS_analysis_comparison.R
Older "gen-1" comparison helper that depended on `06-modular_estimator_analysis.R`.
Superseded by `R/analysis/07-paper_figures.R` (the new canonical figure producer)
and by `R/analysis/06-bayesian_appendix.R` (the new Bayesian consolidator).

### 06-bayesian_sensitivity_analysis.R
Older "gen-1" all-in-one Bayesian sensitivity analysis. Superseded by the
06-run-all.sh + 06-run-single.R + 06-bayesian_appendix.R trio, which uses
session restarts to avoid the OOM issues the gen-1 script hit.

### 06-modular_estimator_analysis.R
Older "gen-1" modular RDS estimator harness. Subsumed by
`R/analysis/03-rds_estimation.R` and `R/analysis/04-bootstrap_analysis.R`
(the per-method bootstrap added 2026-06-17).

### 06a-frequentist_estimators.R
Gen-2 production frequentist RDS estimator wrapper. Subsumed by the
per-method bootstrap CIs now produced by `R/analysis/04-bootstrap_analysis.R`.

### 06b-bayesian_estimators.R
Gen-2 production Bayesian script. Ran MA.estimates at N=980,000 in a single
R session; this OOM-crashed on Scott's 128GB workstation when running the
sensitivity loop. Superseded by the 06-run-all.sh wrapper which runs each
combination in its own Rscript process (memory reclaimed between calls)
and by the appendix-only design that runs MA at N=50,000 (per-N invariance
of prevalence; see paper methods section).

### 06c-bayesian_sensitivity.R
Gen-2 96-combination sensitivity loop. Superseded by the 10-combination
appendix-only design in 06-run-all.sh (8 indicators at degree-seed plus 2
extra seed-method checks for one indicator). See the SIR positioning memo
for the rationale.

### 06c-run-all.sh
Bash wrapper for the 96-combination sensitivity. Superseded by
`R/analysis/06-run-all.sh` (the 10-combination appendix-only version).

### 06d-consolidate_existing_results.R
Older consolidator for the 96-combination sensitivity output. Superseded by
`R/analysis/06-bayesian_appendix.R`.

### 07-comprehensive_comparison.R
Gen-1 comparison-figure producer. Produced some of the figures that
became orphaned (audit Finding 3). Superseded by `R/analysis/07-paper_figures.R`.

### 07-convergence_diagnostics_and_visualization.R
Gen-1 convergence-diagnostics-plus-figures script. Mixed two concerns
(convergence diagnostics and forest plots). Forest-plot functionality
subsumed by `R/analysis/07-paper_figures.R`. Convergence diagnostics
remain available in this archived script if needed.

## Date archived
2026-06-20
