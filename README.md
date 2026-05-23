# DWeMSinUK

**Domestic Worker Exploitation and Modern Slavery in the United Kingdom**

Statistical estimation of labour-exploitation prevalence among domestic workers in the UK, using Respondent-Driven Sampling (RDS), Network Scale-Up Method (NSUM), and a novel continuous risk index based on the ILO indicators of forced labour.

## Quick start

```r
# from the repo root
source("R/00-main_pipeline.R")
```

This sources `R/config.R` for global parameters, then runs cleaning → preparation → RDS estimation → bootstrap → NSUM estimation in sequence. See `CLAUDE.md` for the full pipeline description.

## Layout

| Folder | Contents |
|---|---|
| `R/` | Analysis code (00-main_pipeline.R, config.R, data_processing/, analysis/, utils/, tests/, archive/). Paths use `here()`. |
| `data/` | `raw/` (original survey CSV), `processed/` (cleaned `.RData`/`.csv`), `survey/` (instrument PDFs, codebooks). |
| `output/` | `figures/`, `tables/`, `reports/`. Large `.RData` and `.rds` files are gitignored but live on local disk for caching. |
| `paper/` | `OneDrive_1_22-05-2026/` is the current SIR submission package; `archive/` holds 200+ old drafts; `decisions/` has the four desk-rejection PDFs; `references/` and `supporting/` hold bibliography and supporting materials. |
| `correspondence/` | Project email history, organised by year (`2023/`–`2026/` plus `undated/`). |
| `Notes/`, `Literature/` | Markdown research notes; cited papers. |
| `Replication/`, `GNSUM/` | Reference implementations of the methods; G-NSUM-specific materials. |
| `CRAN/` | Vendored R-package source tarballs (defence against upstream churn). |
| `_snapshots/` | **Gitignored.** Safety-net tarballs from the 2026-05-22 reorganisation. |

## Active manuscript

The current manuscript is being prepared for **Social Indicators Research** (Springer). The latest draft lives at:

```
paper/OneDrive_1_22-05-2026/SIR/Main file submission 20-01-2026.docx
```

Editorial guidance for revising it is in:

```
paper/OneDrive_1_22-05-2026/SIR/Editorial memo - Pre-SIR submission diagnostic.docx
```

## Submission history

Before the SIR submission, this manuscript was desk-rejected from four other journals (see `paper/decisions/`):

- **IJOPM** (Int'l J. of Operations & Production Mgmt, Oct 2025) — one reviewer; rejected on contribution to OSCM theory and on sample-size concerns.
- **JBE** (J. Business Ethics, Dec 2025) — desk reject; contribution to business ethics judged too limited.
- **IRJ** (Industrial Relations Journal, Jan 2026) — desk reject; "too quantitative for IR."
- **BJIR** (British J. of Industrial Relations, Feb 2026) — desk reject; "theoretically underdeveloped."

The pattern across rejections — strong methodology, mismatched audience — motivated the repositioning for SIR.

## Statistical methods

- **RDS estimators**: RDS-I (Salganik & Heckathorn 2004), RDS-II / Volz-Heckathorn (Volz & Heckathorn 2008), and Gile's SS estimator (Gile 2011).
- **Model-Assisted RDS** (Gile & Handcock 2015) for the continuous risk index.
- **Bayesian SS-PSE** (Handcock, Gile & Mar 2014) for population-size estimation.
- **NSUM**: Modified Basic Scale-Up estimator (Feehan & Salganick 2016) with three-step bootstrap for uncertainty quantification.
- **Risk Index**: 13-category risk assessment with weighted scoring, derived from ILO indicators of forced labour.

## Authors

- **Caroline Emberson** (lead) — Nottingham University Business School
- **Scott Moser** — School of Politics and International Relations, University of Nottingham

See `AGENTS.md` for guidance on working with AI assistants on this project, and `CLAUDE.md` for the full project-architecture reference.
