# Repository Guidelines

This file is the short-form guide for any agent (Claude, Codex, or human collaborator) working in this repo. The authoritative long-form spec is `CLAUDE.md`; this file mirrors it in shorthand.

## Project Structure & Module Organization
- `R/` — all R code.
  - `R/00-main_pipeline.R` — master entry point.
  - `R/config.R` — global configuration.
  - `R/data_processing/` — data cleaning and preparation (`01-data_cleaning.R`, `02-data_preparation.R`).
  - `R/analysis/` — numbered pipeline scripts (`03-` through `08-`), active NSUM helpers (`nsum_core_estimators.R`, `nsum_bootstrap.R`, `nsum_robust_adjustment.R`, `nsum_visualization.R`), and the paper-output utility `create_latex_tables.R`. NSUM bootstrap workflow lives in the `NSUM/` subdirectory (`Boot_Step1.r`, `Boot_Step2.R`, `Boot_Step3.R`, `REGENERATE_*.R`).
  - `R/utils/` — shared helpers (`helper_functions.R`).
  - `R/tests/` — test scripts (`test_*.R`, `QUICK_TEST_*.R`, `run_all.R`).
  - `R/archive/` — historical / superseded scripts kept for reference (`old_versions/`, `analysis_consolidation_2025/`, `nsum_consolidation_2025/`, `estimator_versions/`, `experiments/`, `scratch_work/`). Not maintained for executability.
- `data/` — `raw/`, `processed/`, `survey/` inputs. Do not commit sensitive raw files.
- `output/` — generated `figures/`, `tables/`, `reports/`, and serialized results. Most binary outputs are gitignored (see `.gitignore`).
- `paper/` — manuscripts and submission materials. Current SIR draft is in `paper/OneDrive_1_22-05-2026/SIR/`.
- `correspondence/` — project email history, organised by year (`2023/`, `2024/`, `2025/`, `2026/`, `undated/`). Naming convention for new entries: `YYYY-MM-DD <from> to <to> - <subject>.<ext>`.
- `Notes/` — research notes in markdown.
- `Meetings/` — meeting transcripts (text, vtt, srt). Note: `Meetings/emails/` currently duplicates parts of `correspondence/` and is under review.
- `Literature/` — cited literature PDFs.
- `GNSUM/` — G-NSUM specific materials (transcripts, notes, bibs).
- `Replication/` — external replication studies.
- `CRAN/` — vendored R-package source tarballs.
- `_snapshots/` — gitignored safety-net tarballs from past reorganisations.

## Build, Test, and Development Commands
- Run full pipeline: `Rscript R/00-main_pipeline.R`.
- Run a specific stage: edit flags in `pipeline_config` at the top of `R/00-main_pipeline.R`.
- Run tests: `Rscript R/tests/run_all.R`, or any single test: `Rscript R/tests/test_rds_functions.R`.

## Coding Style & Naming Conventions
- Language: R. Indent with 2 spaces; no hard tabs.
- Filenames: step-prefixed kebab case for pipeline files, e.g. `05-nsum_estimation.R`; snake_case for helpers, e.g. `nsum_core_estimators.R`.
- Objects/functions: snake_case; constants UPPER_SNAKE_CASE.
- Line length: aim for ~100 chars.
- Use `here::here()` for all paths; never `setwd()`.

## Testing Guidelines
- Test scripts live in `R/tests/` (not repo root).
- Keep tests reproducible (set seeds) and fast (target <1–2 min).
- Prefer pure functions in `R/utils/` and validate with minimal fixtures from `data/processed/`.
- If adding a new estimator, add a matching `R/tests/test_<name>.R`.

## Commit & Pull Request Guidelines
- Commits: short, imperative subject lines; scope prefix when helpful (e.g. `R/analysis: refine NSUM CI`).
- PRs: include purpose, key changes, how to run (commands), and sample outputs/paths (e.g. `output/tables/...`). Link issues where applicable and add screenshots of figures when relevant.

## Security & Configuration Tips
- R version: 4.2+ recommended. Note: `netclust` 0.1.0 has compatibility issues with R 4.5+; alternatives in `R/analysis/08b-simple_subgroup_analysis.R`.
- Do not commit sensitive survey/raw data; prefer placeholders or synthetic samples.
- Large binary outputs belong in `output/` and are gitignored.
- Never commit credentials. A historical leak (now revoked) lives in commit history; rotate any tokens you handle and use environment variables, not files.
