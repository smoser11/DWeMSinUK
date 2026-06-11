# Tasks

> Active task list for the DWeMSinUK project. Updated 25 May 2026.

## Active — DWeMSinUK SIR submission

### Track 1 — Analysis (Scott's machine, optionally via Claude Code)
- [ ] Fix MA estimator iteration parameters in `R/analysis/06b-bayesian_estimators.R` and `06c-bayesian_sensitivity.R` (raise `number.of.iterations` from 2/3 to ≥10, `M1` from 50/75 to ≥1000, `M2` from 25/50 to ≥500). Re-run. Audit findings 4, 5, 8.
- [ ] Regenerate the 8 orphaned manuscript figures (5 main + 3 in ESM_4) from the canonical pipeline. None currently trace to anything in `output/figures/`. Audit finding 3.
- [ ] Investigate the Excessive Working Hours CI bug (upper bound below point estimate at 100K population). Audit finding 7.
- [ ] Address documented reviewer concerns from IJOPM/JBE/IRJ/BJIR rejection letters. See `paper/decisions/`.
- [ ] Make pipeline end-to-end reproducible: `Rscript R/00-main_pipeline.R` should regenerate every number and figure from a clean state.
- [ ] Set up an anonymised OSF mirror of the analysis code (SIR submission requirement).
- [ ] Full instructions in `instructions/Track 1 - analysis (for Claude Code).md`.

### Track 2 — Manuscript revision (Cowork Claude, after Track 1)
- [ ] Re-audit after Track 1 completes; replace numbers in main paper Table 2 and ESM_4 Tables 2/3 with corrected values.
- [ ] Incorporate Track 1's reviewer-response language into the discussion section.
- [ ] Update references list with Mazziotta-Pareto, Diamantopoulos, Maggino, Bartram et al. 2024.
- [ ] Word-count audit (target <10,000 incl. references; current ~8,600).
- [ ] Full instructions in `instructions/Track 2 - manuscript (for Cowork Claude).md`.

### Done in this session (May 2026)
- [x] SIR positioning analysis memo (deliverable in `paper/OneDrive_1_22-05-2026/SIR/`)
- [x] Assembled complete in-text manuscript with figures and tables embedded
- [x] Statistical audit of ESM_4 and main paper (8 findings documented)
- [x] Surgical edits applied to manuscript: revised abstract, contribution paragraph, conceptualization paragraph, ESM_4 "64/2 NSUM methods" fix
- [x] Strategy email to Caroline sent 24 May 2026
- [x] Repo reorganisation (5 commits sitting waiting for `git push`)
- [x] Email correspondence consolidated and renamed to YYYY-MM-DD convention
- [x] R/analysis/ cleaned up (numbered pipeline + active helpers only; tests in R/tests/; obsolete workflows archived)

## Waiting on

- [ ] Caroline's reply to the 24 May 2026 strategy email (her gut-check on whether the proposed three methodological contributions are authentic, how novel they actually are against HIV-/sex-work-prevalence literature, and whether SIR is the right venue).
- [ ] `git push` from Scott's terminal — 5 commits ahead of origin/main on the reorganisation + audit work.
- [ ] Confirmation that the leaked GitHub PAT from earlier (`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` in `git.R` at older commits) has been revoked at https://github.com/settings/tokens.

## Someday

- [ ] June catch-up with Caroline (she suggested it, Scott proposed).
- [ ] Reconnect with Selim if/when contact resumes (he worked on the Risk Index but is out of touch).
- [ ] If SIR also rejects: identify backup venue.
