# Repository Guidelines

## Project Structure & Module Organization
- `R/` — analysis scripts (`analysis/`), data prep (`data_processing/`), utilities (`utils/`), and the entrypoint `00-main_pipeline.R`.
- `data/` — `raw/`, `processed/`, and `survey/` inputs. Do not commit sensitive raw files.
- `output/` — generated `figures/`, `tables/`, `reports/`, and serialized results (`*.RData`, `*.RDS`).
- Supporting folders: `paper/`, `Notes/`, `Meetings/`, `GNSUM/`, `archive/`, `analysis/`.

## Build, Test, and Development Commands
- Run pipeline: `Rscript R/00-main_pipeline.R` (drives data cleaning, RDS, NSUM, and summary report under `output/reports/`).
- Run a specific stage: edit flags in `pipeline_config` inside `R/00-main_pipeline.R`.
- Ad‑hoc tests: `Rscript test_v2_extraction.R` (or any `test_*.R` in repo root).
- Batch test example (bash): `for f in test_*.R; do Rscript "$f"; done`.

## Coding Style & Naming Conventions
- Language: R. Indent with 2 spaces; no hard tabs.
- Filenames: step‑prefixed kebab case, e.g., `05-nsum_estimation.R`.
- Objects/functions: snake_case; constants UPPER_SNAKE_CASE.
- Line length: aim for ~100 chars; wrap long pipelines.
- Use `here::here()` for paths; avoid `setwd()`.

## Testing Guidelines
- Tests are lightweight R scripts in repo root (`test_*.R`). Keep them reproducible (set seeds) and fast (<1–2 min).
- Prefer pure functions in `R/utils/` and validate with minimal fixtures from `data/processed/`.
- If adding a new estimator, provide a matching `test_<name>.R` and save small example outputs under `output/` for manual inspection.

## Commit & Pull Request Guidelines
- Commits: short, imperative subject lines; scope prefix when helpful (e.g., `R/analysis: refine NSUM CI`).
- PRs: include purpose, key changes, how to run (commands), and sample outputs/paths (e.g., `output/tables/...`). Link issues where applicable and add screenshots of figures when relevant.

## Security & Configuration Tips
- R version: 4.2+ recommended. Install packages as prompted by scripts.
- Do not commit sensitive survey/raw data; prefer placeholders or synthetic samples.
- Large artifacts belong in `output/`; confirm `.gitignore` before committing.
