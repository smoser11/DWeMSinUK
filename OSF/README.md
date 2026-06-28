# Replication materials for SIR submission

**Paper:** Quantifying Hidden Exploitation: Dual-Method Prevalence Estimates of Labour Exploitation among Domestic Workers in the United Kingdom

**OSF project:** <https://osf.io/vf5yb/>

**Repository contents:** all data, code, figures, tables, and supplementary materials needed to reproduce the tables, figures, estimates, and ESMs reported in the manuscript are compressed in the file `Archive.zip`. Author-identifying material has been removed from data files. This OSF folder is self-contained: scripts use `here()` paths relative to this folder's root (the `.here` marker file at the top of `OSF/` makes the resolution work).

---

## Folder structure

```
OSF/
├── README.md                            (this file)
├── .here                                marker for here::here() resolution
├── data/
│   ├── raw/
│   │   ├── README.md                    (explains why raw CSV not included)
│   │   ├── Codebook.pdf                 survey question wording
│   │   └── Risk_Index_Construction.docx composite risk index specification
│   └── processed/                       anonymised analytical data (4 CSVs)
│       ├── data_full.csv                n = 97, full cleaned sample
│       ├── data_nonzero.csv             n = 85, non-zero network degree (analytical)
│       ├── long_format_data.csv         long-format alter-report data
│       └── prepared_data.csv            n = 85 with RDS weights computed
├── R/                                   R analysis pipeline (use `here()` paths)
│   ├── 00-main_pipeline.R               master script (sources 01–09 in order)
│   ├── config.R                         global configuration
│   ├── data_processing/
│   │   ├── 01-data_cleaning.R           builds cleaned_data.RData from anon CSV
│   │   └── 02-data_preparation.R        creates ILO indicators + RDS weights
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
├── output/                              regenerated outputs
│   ├── figures/                         13 figures (6 main paper + 7 ESM_4 convergence)
│   └── tables/                          17 CSV tables for main paper + ESMs
├── manuscript/                          paper artifacts
│   ├── manuscript.qmd                   Quarto source (renders to PDF/DOCX/HTML)
│   ├── ce-library.bib                   CE's reference library (biblatex/biber)
│   ├── sm-cited.bib                     SM cited references (26-entry subset)
│   ├── apa.csl                          APA citation style sheet
│   └── figure2_rds_diagnostics.png      figure referenced from manuscript.qmd
└── supporting/                          online supplementary materials (ESMs)
    ├── ESM_1.docx                       codebook + composite risk index construction
    ├── ESM_2.docx                       survey instrument
    ├── ESM_3.docx                       three-step NSUM bootstrap procedure
    └── ESM_4.docx                       sensitivity analyses + supporting material
```

---

## Reproduction

### Software requirements

- **R** version 4.0 or later, with packages: `tidyverse`, `janitor`, `here`, `RDS`, `igraph`, `boot`, `parallel`, `ggplot2`, `scales`, `viridis`, `sspse`. Optional: `netclust` (for the nationality-subgroup analysis in `08-netclust_subgroup_analysis.R`; this package has compatibility issues with R 4.5+ and may need installation from source).
- **Python** 3.9 or later, with packages: `pandas`, `numpy`, `scipy`, `matplotlib`.
- **Quarto** 1.4 or later (to render the manuscript).

### Full pipeline reproduction

From the `OSF/` folder root, with R working directory set to `OSF/R/`:

```r
# Source the full pipeline (~15 minutes; ~90 minutes if MA-RDS Bayesian is re-run)
source("00-main_pipeline.R")
```

Or run scripts individually in order (each script uses `here()` to resolve paths
back to the OSF root via the `.here` marker):

```r
source("data_processing/01-data_cleaning.R")     # builds cleaned_data.RData
source("data_processing/02-data_preparation.R")  # builds prepared_data.RData + rd.dd
source("analysis/03-rds_estimation.R")
source("analysis/04-bootstrap_analysis.R")
source("analysis/05-nsum_estimation.R")
source("analysis/05b-nsum_tau_sensitivity.R")
source("analysis/06-bayesian_appendix.R")
source("analysis/07-paper_figures.R")
source("analysis/08b-simple_subgroup_analysis.R")
source("analysis/09-create_latex_tables.R")
```

For the two Python analyses (from `OSF/python/`):

```bash
python3 rds_diagnostics.py              # writes ../output/figures/rds_diagnostics.png
python3 composite_weight_sensitivity.py # writes ../output/tables/composite_risk_weight_sensitivity.json
```

To render the manuscript (from `OSF/manuscript/`):

```bash
quarto render manuscript.qmd --to docx
quarto render manuscript.qmd --to pdf
```

### About the cleaning step (01-data_cleaning.R)

The original 01 script reads the raw survey CSV (`data/raw/UpdateSelimRiskIndex-sum_cat.csv`) to produce the cleaned data files. **The raw CSV is not included in this OSF package** because it contains PII.

The OSF version of 01-data_cleaning.R loads the already-anonymised `data/processed/data_full.csv` instead and writes the `cleaned_data.RData` cache that `02-data_preparation.R` expects. Reviewers who want to verify the cleaning logic against the raw data can request access from the editor (see PII note below).

---

## PII anonymisation

The following columns were stripped (set to empty string) from all four CSVs in `data/processed/`:

**Direct identifiers (17 columns):**
- `q2` (respondent name)
- `q3`, `q117` (respondent email, collected twice for verification)
- `q4`, `q116` (respondent phone, collected twice)
- `q105`–`q115` (RDS-referral phone numbers, up to five primary + five backup)
- `q118` (referrer email; in `data_full.csv` only)

**Free-text "other-specify" responses (21 columns):**
- `q8_a` (nationality "other" specifier)
- `q18_a`, `q34_a`, `q44_a`, `q44_b`, `q48_a`, `q51_a`, `q58_a`, `q59_a`, `q67_a`,
  `q82_a`, `q85_90_93_96`, `q86`, `q87_95`, `q98`, `q99`, `q100`, `q101`,
  `q103_a`, `q26_a`, `q26_b`

The free-text columns add qualitative texture but were not used in any quantitative analysis reported in the manuscript. They were removed because narrative content in an n = 85 hidden-population sample can enable re-identification by someone who knows the respondent.

The underlying coded responses (`q1`, `q5`–`q104`) are retained verbatim across all four CSVs, preserving every analytical column the R pipeline reads. Network structure (`recruiter.id`, referral linkages) is preserved through the numeric `id` column, which is internal to the dataset and not linkable to external identifiers.


---

## Manuscript-to-output mapping

| Main paper | OSF output file |
|---|---|
| Table 1 (ILO indicator mapping) | hand-constructed; see `data/raw/Risk_Index_Construction.docx` and `supporting/ESM_1.docx` |
| Table 2 (sample composition by nationality × wave) | `output/tables/table1_publication_ready.csv` |
| Table 3 (estimands) | hand-constructed; see manuscript Section 3.4 and `supporting/ESM_1.docx` |
| Table 4 (RDS diagnostics) | `python/rds_diagnostics.py` → `output/figures/figure2_rds_diagnostics.png` |
| Table 5 (preferred prevalence: RDS-SS + NSUM Moderate) | `output/tables/rds_nsum_comparison.csv` |
| Figure 1 (recruitment network) | `output/figures/figure1_recruitment_network.png` |
| Figure 2 (RDS diagnostics) | `output/figures/figure2_rds_diagnostics.png` |
| Figure 3 (composite risk estimators) | `output/figures/figure3_composite_risk_estimators.png` |
| Figure 4 (MA binary estimates) | `output/figures/figure4_ma_binary_estimates.png` |
| ESM_1 | `supporting/ESM_1.docx` — codebook + composite risk index construction |
| ESM_2 | `supporting/ESM_2.docx` — survey instrument |
| ESM_3 | `supporting/ESM_3.docx` — three-step NSUM bootstrap procedure |
| ESM_4 | `supporting/ESM_4.docx` — sensitivity analyses; source tables in `output/tables/AppendixA{1..6}_*.csv`, `ESM_appendix_bayesian_*.csv`; figures in `output/figures/` |

---

## Licence and citation

Code: MIT licence. Data: CC-BY-4.0 (anonymised). Please cite the paper if you use these materials.
