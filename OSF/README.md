# Replication materials for SIR submission

**Paper:** Quantifying Hidden Exploitation: Dual-Method Prevalence Estimates of Labour Exploitation among Domestic Workers in the United Kingdom

**OSF project:** <https://osf.io/vf5yb/>

**Repository contents:** all data, code, figures, tables, and supplementary materials needed to reproduce the tables, figures, and estimates reported in the manuscript. This OSF folder is self-contained: R scripts use `here()` paths relative to this folder's root (the `.here` marker file at the top of `OSF/` makes the resolution work); Quarto renders the manuscript and appendix from local bibliographies.

---

## Reproduction

### Software requirements

- **R** ≥ 4.0 with packages: `tidyverse`, `janitor`, `here`, `RDS`, `igraph`, `boot`, `parallel`, `ggplot2`, `scales`, `viridis`, `sspse`. Optional: `netclust` (nationality subgroup analysis; requires source build on R ≥ 4.5).
- **Python** ≥ 3.9 with `pandas`, `numpy`, `scipy`, `matplotlib`.
- **Quarto** ≥ 1.4 (renders manuscript and appendix book).

### Full pipeline reproduction

From the `OSF/` folder root, with R working directory set to `OSF/R/`:

```r
# Sources the full pipeline (~15 minutes; ~90 minutes if MA-RDS Bayesian is re-run)
source("00-main_pipeline.R")
```

Or run scripts individually (each script uses `here()` to resolve paths back to the OSF root via the `.here` marker):

```r
source("data_processing/01-data_cleaning.R")     # builds cleaned_data.RData
source("data_processing/02-data_preparation.R")  # builds prepared_data.RData + rd.dd
source("analysis/03-rds_estimation.R")
source("analysis/04-bootstrap_analysis.R")
source("analysis/05-nsum_estimation.R")
source("analysis/05b-nsum_tau_sensitivity.R")
source("analysis/06-bayesian_appendix.R")
source("analysis/07-paper_figures.R")
source("analysis/08c-loo_chain_analysis.R")      # LOO chain sensitivity (Appendix D.8/D.9 + Figure D.5)
```

For the two Python analyses (from `OSF/python/`):

```bash
python3 rds_diagnostics.py              # writes ../output/figures/rds_diagnostics.png
python3 composite_weight_sensitivity.py # writes ../output/tables/composite_risk_weight_sensitivity.json
```

### Rendering the paper

Main manuscript (from `OSF/manuscript/`):

```bash
quarto render manuscript.qmd --to pdf
quarto render manuscript.qmd --to docx
```

Online appendix book (from `OSF/manuscript/appendix/`):

```bash
cd appendix
quarto render
```

The appendix book renders as a self-contained multi-format artefact (HTML book + PDF + DOCX) with appendix-scoped table and figure numbering (Table A.1, A.2, B.1, D.1–D.9; Figure D.1–D.5).

### About the cleaning step (01-data_cleaning.R)

The original 01 script reads the raw survey CSV (`data/raw/UpdateSelimRiskIndex-sum_cat.csv`) to produce the cleaned data files. **The raw CSV is not included in this OSF package** because it contains PII.

The OSF version of 01-data_cleaning.R loads the already-anonymised `data/processed/data_full.csv` instead and writes the `cleaned_data.RData` cache that `02-data_preparation.R` expects. Reviewers who want to verify the cleaning logic against the raw data can request access from the editor (see PII note below).

---

## PII anonymisation

The following columns were stripped (set to empty string) from all four CSVs in `data/processed/`:

**Direct identifiers (17 columns):** `q2` (respondent name); `q3`, `q117` (email); `q4`, `q116` (phone); `q105`–`q115` (RDS-referral phone numbers, up to five primary + five backup); `q118` (referrer email; `data_full.csv` only).

**Free-text "other-specify" responses (21 columns):** `q8_a` (nationality "other" specifier); `q18_a`, `q34_a`, `q44_a`, `q44_b`, `q48_a`, `q51_a`, `q58_a`, `q59_a`, `q67_a`, `q82_a`, `q85_90_93_96`, `q86`, `q87_95`, `q98`, `q99`, `q100`, `q101`, `q103_a`, `q26_a`, `q26_b`.

The free-text columns add qualitative texture but were not used in any quantitative analysis reported in the manuscript. They were removed because narrative content in an n = 85 hidden-population sample can enable re-identification by someone who knows the respondent.

The underlying coded responses (`q1`, `q5`–`q104`) are retained verbatim across all four CSVs, preserving every analytical column the R pipeline reads. Network structure (`recruiter.id`, referral linkages) is preserved through the numeric `id` column, which is internal to the dataset and not linkable to external identifiers.

---

## Manuscript-to-output mapping

Where each main-paper element is produced or documented.

| Main paper | Source |
|---|---|
| Table 1 (ILO indicator mapping) | hand-constructed; see `data/raw/Risk_Index_Construction.docx` and `manuscript/appendix/A-codebook.qmd` (§A.1) |
| Table 2 (sample demographics: gender / age / employment / UK tenure) | `manuscript/manuscript.qmd` (hand-constructed from `data/processed/prepared_data.csv`) |
| Table 3 (sample composition by nationality × wave) | `output/tables/table1_publication_ready.csv` |
| Table 4 (estimands) | hand-constructed; see manuscript Section 3.4 and `manuscript/appendix/A-codebook.qmd` |
| Table 5 (RDS diagnostics) | `python/rds_diagnostics.py` → `output/figures/figure2_rds_diagnostics.png` |
| Table 6 (preferred prevalence: RDS-SS + NSUM Moderate) | `output/tables/rds_nsum_comparison.csv` |
| Figure 1 (recruitment network) | `output/figures/figure1_recruitment_network.png` |
| Figure 2 (RDS diagnostics) | `output/figures/figure2_rds_diagnostics.png` |
| Figure 3 (composite risk estimators) | `output/figures/figure3_composite_risk_estimators.png` |
| Figure 4 (MA binary estimates) | `output/figures/figure4_ma_binary_estimates.png` |

## Appendix-to-output mapping

| Appendix element | Source |
|---|---|
| Section A (codebook + indicator definitions) | `manuscript/appendix/A-codebook.qmd` |
| Section B (survey instrument) | `manuscript/appendix/B-survey.qmd` |
| Section C (three-step NSUM bootstrap procedure) | `manuscript/appendix/C-bootstrap.qmd` |
| Section D (sensitivity analyses) | `manuscript/appendix/D-sensitivity.qmd` |
| Table A.1 (composite risk index construction) | `manuscript/appendix/A-codebook.qmd` (hand-constructed) |
| Table D.1 (Bayesian MA-RDS estimates) | `output/tables/ESM_appendix_bayesian_summary.csv` |
| Table D.2 (MA-RDS sensitivity across N and seed method) | `output/tables/ESM_appendix_bayesian_seedcheck.csv` |
| Table D.3 (comprehensive RDS-I vs RDS-SS estimates) | `output/tables/AppendixA5_complete_matrix.csv` |
| Table D.4 (NSUM under alternative adjustment-factor tiers) | `output/tables/AppendixA4_nsum_method_comparison.csv` |
| Table D.5 (bootstrap method comparison) | `output/tables/AppendixA1_bootstrap_comparison.csv` |
| Table D.6 (weighting scheme comparison) | `output/tables/AppendixA2_weight_comparison.csv` |
| Table D.7 (population size comparison) | `output/tables/AppendixA3_popsize_comparison.csv` |
| Table D.8 (excluded chain roster, seed #55) | `R/analysis/08c-loo_chain_analysis.R` (BFS on `prepared_data.csv`) |
| Table D.9 (full-sample vs LOO comparison) | `output/tables/loo_chain_sensitivity.csv` |
| Figure D.1 (MA seed-selection sensitivity) | `output/figures/figA4_ma_seedcheck.png` |
| Figure D.2 (RDS prevalence by method) | `output/figures/figA2_rds_comparison.png` |
| Figure D.3 (NSUM MBSU Moderate forest plot) | `output/figures/figA3_nsum_forest.png` |
| Figure D.4 (continuous-τ sweep) | `output/figures/figA1_tau_sweep.png` |
| Figure D.5 (LOO chain sensitivity) | `output/figures/figA5_loo_chain.png` |

---

## Licence and citation

Code: MIT licence. Data: CC-BY-4.0 (anonymised). Please cite the paper if you use these materials.
