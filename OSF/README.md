# Replication materials for SIR submission

**Paper:** Quantifying Hidden Exploitation: Dual-Method Prevalence Estimates of Labour Exploitation among Domestic Workers in the United Kingdom

**OSF project:** <https://osf.io/vf5yb/>

**Repository contents:** all data, code, figures, tables, and supplementary materials needed to reproduce the tables, figures, estimates, and ESMs reported in the manuscript. Author-identifying material has been removed from data files; the manuscript file is included as a Quarto source.

---

## Folder structure

```
OSF/
├── README.md                            (this file)
├── data/                                anonymised survey data + codebook
│   ├── data_full_anon.csv               n = 97, raw analytical CSV (PII columns nulled)
│   ├── data_nonzero_anon.csv            n = 85, analytical sample (non-zero degree)
│   ├── prepared_data_anon.csv           n = 85, with RDS weights computed
│   ├── long_format_data_anon.csv        long-format alter-report data
│   ├── Codebook.pdf                     survey question wording
│   └── Risk_Index_Construction.docx     composite risk index specification
├── R/                                   R analysis pipeline
│   ├── 00-main_pipeline.R               master script (sources 01–09 in order)
│   ├── config.R                         global configuration
│   ├── data_processing/
│   │   ├── 01-data_cleaning.R           reads data/, applies cleaning
│   │   └── 02-data_preparation.R        creates ILO-aligned indicators + RDS weights
│   ├── analysis/
│   │   ├── 03-rds_estimation.R          RDS-I, RDS-II, RDS-SS
│   │   ├── 04-bootstrap_analysis.R      per-method bootstrap CIs
│   │   ├── 05-nsum_estimation.R         MBSU NSUM (within-population scale-up)
│   │   ├── 05b-nsum_tau_sensitivity.R   continuous-tau sweep (headline NSUM result)
│   │   ├── 06-bayesian_appendix.R       Bayesian model-assisted (MA-RDS)
│   │   ├── 07-paper_figures.R           regenerate manuscript figures
│   │   ├── 08-netclust_subgroup_analysis.R   nationality subgroup analysis
│   │   ├── 09-create_latex_tables.R     produce publication-ready tables
│   │   └── nsum_*.R                     NSUM helper modules
│   └── utils/helper_functions.R         shared utility functions
├── python/                              ad-hoc Python scripts
│   ├── rds_diagnostics.py               generates Figure 2 + Table 4 (RDS diagnostics)
│   └── composite_weight_sensitivity.py  generates the weight-perturbation analysis
│                                         reported in Section 5.5
├── output/                              regenerated outputs (included so reviewers can
│   │                                    verify without re-running the full pipeline)
│   ├── figures/
│   │   ├── figure1_recruitment_network.png
│   │   ├── figure2_rds_diagnostics.png
│   │   ├── figure3_composite_risk_estimators.png
│   │   ├── figure4_ma_binary_estimates.png
│   │   ├── figure_nsum_moderate.png
│   │   ├── esm_b1_rds_methods_comparison.png
│   │   └── convergence_*.png            (per-indicator MA convergence plots, ESM_4)
│   └── tables/
│       ├── rds_main_results.csv         headline RDS-SS prevalence estimates
│       ├── rds_per_method_bootstrap_ci.csv   per-method bootstrap CIs
│       ├── rds_nsum_comparison.csv      RDS vs NSUM side-by-side (Table 5)
│       ├── nsum_main_results.csv        MBSU NSUM by tier
│       ├── nsum_tau_sensitivity.csv     continuous-tau sweep results
│       ├── AppendixA1_…csv …            ESM_4 robustness tables
│       └── composite_risk_weight_sensitivity.json   weight-perturbation summary
└── manuscript/                          paper artifacts
    ├── manuscript.qmd                   Quarto source (renders to PDF/DOCX/HTML)
    ├── ESM_1.docx                       codebook + composite risk index construction
    ├── ESM_2.docx                       survey instrument
    ├── ESM_3.docx                       three-step NSUM bootstrap procedure
    ├── ESM_4.docx                       sensitivity analyses + relocated material
    └── figure2_rds_diagnostics.png      figure referenced from manuscript.qmd
```

---

## Reproduction

### Software requirements

- **R** version 4.0 or later, with packages: `tidyverse`, `janitor`, `here`, `RDS`, `igraph`, `boot`, `parallel`, `ggplot2`, `scales`, `viridis`, `sspse`. Optional: `netclust` (for the nationality-subgroup analysis in `08-netclust_subgroup_analysis.R`; note this package has compatibility issues with R 4.5+ and may need installation from source).
- **Python** 3.9 or later, with packages: `pandas`, `numpy`, `scipy`, `matplotlib`.
- **Quarto** 1.4 or later (to render the manuscript).

### Full pipeline reproduction

From the OSF folder root:

```r
# In R, with working directory set to OSF/R/
source("00-main_pipeline.R")
```

This runs the full pipeline (~15 minutes on a modern laptop, ~90 minutes if the MA-RDS Bayesian sensitivity is re-run from scratch). Outputs are written to `../output/`.

```bash
# Then in Python, from OSF/python/
python3 rds_diagnostics.py
python3 composite_weight_sensitivity.py
```

To render the manuscript:

```bash
# From OSF/manuscript/
quarto render manuscript.qmd --to docx
quarto render manuscript.qmd --to pdf
```

### Selective reproduction

Individual analyses can be run separately after the data preparation step (which produces `prepared_data.RData`):

| To reproduce | Run |
|---|---|
| Table 2 (sample composition) | `R/analysis/03-rds_estimation.R` |
| Table 3 (estimands) | (descriptive; no script) |
| Figure 2 + Table 4 (RDS diagnostics) | `python/rds_diagnostics.py` |
| Figure 3 (composite risk) | `R/analysis/03-rds_estimation.R` + `R/analysis/07-paper_figures.R` |
| Figure 4 (MA binary) | `R/analysis/06-bayesian_appendix.R` |
| Table 5 (RDS-SS + NSUM preferred) | `R/analysis/03-rds_estimation.R` + `R/analysis/05-nsum_estimation.R` |
| NSUM continuous-tau sweep (Section 4.5) | `R/analysis/05b-nsum_tau_sensitivity.R` |
| Section 5.5 composite weight sensitivity | `python/composite_weight_sensitivity.py` |
| ESM_4 Tables 1–7 (full sensitivity matrix) | `R/analysis/04-bootstrap_analysis.R`, `06-bayesian_appendix.R`, `05-nsum_estimation.R` |

---

## Notes on data anonymisation

The original survey data contained the following personally-identifying columns, which have been removed from all data files included in this package:

- `q2` (respondent name)
- `q3` and `q117` (respondent email — same value collected twice for verification)
- `q4` and `q116` (respondent phone — same value collected twice)
- `q105` (referrer phone)
- `q118` (referrer email; in `data_full_anon.csv` only)

All other survey-question columns (`q1`, `q5` onward) are retained verbatim. Network structure (`recruiter.id`, referral linkages) is preserved using the numeric `id` column, which is internal to the dataset and not linkable to external identifiers.

The original (non-anonymised) raw data, survey-instrument source Excel files, and email logs are not included in this OSF package. They are retained on the authors' institutional storage and are available to the editor on confidential request to verify the anonymisation procedure if needed.

## Notes on output files

The `output/` folder contains the most recent versions of the published tables and figures, regenerated from the included data and code. Re-running the pipeline will overwrite these files with bit-identical (or within-bootstrap-noise identical for Monte Carlo procedures) copies. The Bayesian model-assisted estimates use a random seed fixed in `06-bayesian_appendix.R` and the Python composite-weight sensitivity uses a seed fixed in `composite_weight_sensitivity.py`; both should reproduce exactly under those seeds.

If a reviewer's regenerated results differ from the included `output/` files by more than bootstrap noise (e.g., > 1 percentage point on a prevalence estimate), please contact the authors via the OSF project page.

## Manuscript-to-output mapping

For convenience, every table and figure in the main manuscript (and its ESMs) corresponds to a specific output file:

| Main paper | OSF output file |
|---|---|
| Table 1 (indicator mapping) | hand-constructed; see `data/Risk_Index_Construction.docx` |
| Table 2 (sample composition) | `output/tables/table1_publication_ready.csv` (renamed in manuscript) |
| Table 3 (estimands) | hand-constructed; see Section 3.4 of manuscript |
| Table 4 (RDS diagnostics) | `output/tables/` + `python/rds_diagnostics.py` output |
| Table 5 (preferred prevalence: RDS-SS + NSUM Moderate) | `output/tables/rds_nsum_comparison.csv` |
| Figure 1 (recruitment network) | `output/figures/figure1_recruitment_network.png` |
| Figure 2 (RDS diagnostics) | `output/figures/figure2_rds_diagnostics.png` |
| Figure 3 (composite risk estimators) | `output/figures/figure3_composite_risk_estimators.png` |
| Figure 4 (MA binary estimates) | `output/figures/figure4_ma_binary_estimates.png` |
| ESM_4 Tables 1–7 (sensitivity) | `output/tables/AppendixA{1..6}_*.csv`, `ESM_appendix_bayesian_*.csv` |
| ESM_4 Figure B.1 (RDS-I vs RDS-SS) | `output/figures/esm_b1_rds_methods_comparison.png` |
| ESM_4 Figure 1 (continuous-tau NSUM sweep) | `output/tables/nsum_tau_sensitivity.csv` + `R/analysis/05b-nsum_tau_sensitivity.R` |

---

## Licence and citation

Code: MIT licence. Data: CC-BY-4.0 (anonymised). Please cite the paper if you use these materials.
