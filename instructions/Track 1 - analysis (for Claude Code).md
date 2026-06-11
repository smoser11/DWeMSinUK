# Track 1 — Analysis re-check, reviewer concerns, extension, reproducibility

**Audience:** Claude Code (CC), running locally on Scott's machine with R installed.
**Authored by:** Cowork Claude session, 2026-05-25.
**Source-of-truth for what is broken and why:** `paper/OneDrive_1_22-05-2026/SIR/Statistical audit report - 2026-05-25.docx`. **Read that report in full before starting.**

---

## What you ARE doing in this track

Re-checking the statistical analysis of the DWeMSinUK paper, addressing the reviewer concerns raised in the four prior submissions, extending the analysis where useful, and making the entire pipeline end-to-end reproducible. The goal is that anyone with this repo and R can run `Rscript R/00-main_pipeline.R` and reproduce every number and every figure in the manuscript, including the appendix (ESM_4).

## What you are NOT doing in this track

- **Do not touch the manuscript .docx files in `paper/OneDrive_1_22-05-2026/SIR/`** (other than the figure source files `Figure 1.docx` through `Figure 5.docx` and `ESM_4.docx` where you are explicitly asked to replace embedded images). Manuscript prose revision is Track 2 (Cowork Claude).
- **Do not draft email or correspondence.** That is handled separately.
- **Do not reorganise files.** The repo structure was reorganised on 2026-05-23 to 2026-05-25; respect it. New code goes in the existing `R/analysis/` location with the existing naming convention.
- **Do not delete cached output files** in `output/` until you have regenerated equivalents. The audit cross-checks the manuscript against these cached files.

## Preconditions

- Repo at commit `7d1d16a` or later (the 5-commit organisational cleanup must be on disk).
- R 4.2+ installed with the packages listed in `CLAUDE.md`. Note: `netclust` is not on CRAN; install from source if needed (see `CLAUDE.md` for guidance).
- The audit report in `paper/OneDrive_1_22-05-2026/SIR/Statistical audit report - 2026-05-25.docx` has been read.

## Reference documents

- `paper/OneDrive_1_22-05-2026/SIR/Statistical audit report - 2026-05-25.docx` — the eight audit findings.
- `paper/decisions/` — desk-rejection letters from IJOPM, JBE, IRJ, BJIR (read each before Task 5).
- `CLAUDE.md` — repo conventions.
- `R/00-main_pipeline.R` — pipeline entry point.

---

## Task 1: Fix the MA estimator iteration parameters (audit Findings 4, 5, 8)

**Files to modify:**

- `R/analysis/06b-bayesian_estimators.R`, lines 51–58 (enhanced parameter block) and lines 67–78 (standard parameter block).
- `R/analysis/06c-bayesian_sensitivity.R`, the analogous parameter blocks (around lines 76 and 91; verify the actual line numbers when you open the file — the file has been modified since the audit).
- `R/analysis/06d-consolidate_existing_results.R` (the `get_sensitivity_parameters()` function around line 130).

**The change:**

The current parameter values are too small for the Gile-Handcock MA algorithm to converge. Replace:

```r
# Standard (current)                    # Standard (replacement)
number.of.iterations = 2          ->    number.of.iterations = 10
M1 = 50                           ->    M1 = 1000
M2 = 25                           ->    M2 = 500

# Enhanced (current)                    # Enhanced (replacement)
number.of.iterations = 3          ->    number.of.iterations = 15
M1 = 75                           ->    M1 = 2000
M2 = 50                           ->    M2 = 1000
```

The replacement values are starting points based on the RDS package documentation. If runtime is prohibitive (each MA.estimates call may take 1–10 minutes at these settings, and there are 96 sensitivity combinations in 06c-), do a convergence study: run for a single indicator and population size with iteration counts {2, 5, 10, 20, 50} and plot the estimate and SE against iteration count. Find the point at which both stabilise. Use that as the production setting. Document the convergence study results in `R/analysis/_docs/ma_convergence_study.md`.

**Update the source-code comments** that say "2 iterations sufficient for binary" — they are wrong. Replace with an explanation referencing the convergence study results.

**Verification (acceptance criteria for Task 1):**

After re-running `06b-bayesian_estimators.R`:

1. Open `output/bayesian_estimators_results.RData` and check `whether_exploitation`: SE should be in the range ~0.05–0.20, not 0.000003.
2. Check `composite_risk`: point estimate should be near 0.37 (close to the frequentist 36.9%), not 0.07.
3. No indicator should have SE > 0.3.

After re-running `06c-bayesian_sensitivity.R`:

4. Open `output/bayesian_sensitivity_results.RData` and check `seed_sensitivity_table`: rows should NOT be byte-identical across the `degree` / `random` / `sample` columns. Document withholding at 980K, for example, should show three numerically different estimates across the three seed methods (possibly close, but not identical to the last decimal).

If verifications 1–4 fail, the fix is incomplete or the parameters are still too small. Iterate.

---

## Task 2: Regenerate all manuscript figures from the pipeline (audit Finding 3)

**Background:** All eight figures in the manuscript (5 in main paper Figure 1.docx through Figure 5.docx, 3 in ESM_4.docx) are orphaned. No PNG in `output/figures/` hash-matches any of them. The figure-generation scripts likely exist but their last run was not the run that produced the figures currently embedded in the manuscript.

**Step 1: Identify the producing scripts.**

For each of the 8 manuscript figures, find the R script that produces a figure of the same type. Likely candidates:

- `R/analysis/07-comprehensive_comparison.R` — comparison plots
- `R/analysis/07-convergence_diagnostics_and_visualization.R` — convergence plots, forest plots
- `R/analysis/nsum_visualization.R` — NSUM-specific plots
- `R/analysis/08-netclust_subgroup_analysis.R` and `08b-` — subgroup plots

Read each candidate script and identify which `ggplot()` / `png()` call produces which manuscript figure based on the caption text in the .docx files. The captions are:

- **Main Figure 1:** "RDS Recruitment Network Structure"
- **Main Figure 2:** "Model-Assisted Estimates of Composite Risk Index. Distribution comparison..."
- **Main Figure 3:** "Forest Plot of Model-Assisted Binary Exploitation Estimates"
- **Main Figure 4:** "Forest Plot of RDS Estimates: Comparison of RDS-I and RDS-SS"
- **Main Figure 5:** **HAS IDENTICAL CAPTION TO MAIN FIGURE 4** — likely a labelling error. Verify which image is actually in each .docx (their MD5s differ; the captions don't). Determine the intended distinct content of Figure 5 from context (the manuscript text mentions Figure 5 at paragraph 84, between Tables 3 and 4 in section 4.5 Frequentist RDS Estimates).
- **ESM_4 Fig1:** "RDS Sensitivity to Population Size: each panel one RDS method"
- **ESM_4 Fig2:** "Model-Assisted Sensitivity by Population Size and Seed Selection"
- **ESM_4 Fig3:** "Model-Assisted Point Estimate Comparison at 980K population"

**Step 2: Run the identified scripts after Task 1 is complete.**

The MA-related figures must be regenerated AFTER Task 1's fix, because the underlying numbers will have changed.

**Step 3: Replace the embedded figures.**

For each of the 8 figure source .docx files, replace the embedded image with the regenerated PNG. The .docx files are:

```
paper/OneDrive_1_22-05-2026/SIR/Figure 1.docx
paper/OneDrive_1_22-05-2026/SIR/Figure 2.docx
paper/OneDrive_1_22-05-2026/SIR/Figure 3.docx
paper/OneDrive_1_22-05-2026/SIR/Figure 4.docx
paper/OneDrive_1_22-05-2026/SIR/Figure 5.docx
paper/OneDrive_1_22-05-2026/SIR/ESM_4.docx  (3 images at paragraph positions 9, 22, 27)
```

If replacing in-place is tricky in .docx, an acceptable alternative is to save the regenerated PNGs to `output/figures/manuscript_figures/` with the names `Figure_1.png`, `Figure_2.png`, … and leave a note in the file `output/figures/manuscript_figures/README.md` indicating which PNG should be embedded in which .docx slot. Track 2 (Cowork Claude) will do the embedding.

**Verification (acceptance criteria for Task 2):**

1. Every regenerated figure has a PNG file in `output/figures/` (or a subdirectory thereof) whose existence is the result of running the canonical pipeline.
2. Either the .docx files have been updated to embed those PNGs, OR `output/figures/manuscript_figures/README.md` exists with a clear mapping for Track 2 to apply.
3. The Figure 4 / Figure 5 caption-duplication issue is resolved or explicitly documented.

---

## Task 3: Investigate and fix the Excessive Working Hours CI bug (audit Finding 7)

**Symptom:** ESM_4 Table 1, "Excessive working hours" row, RDS-I column at 100K population shows "92.8% (71.8–92.5)". The point estimate (92.8%) is above the upper CI bound (92.5%), which is impossible by construction.

**Hypothesis:** The indicator is near a ceiling (~93%), and the bootstrap percentile CI is being computed from a finite bootstrap sample whose distribution is asymmetric near the boundary. Quantile-based CIs near boundaries are unreliable without sufficient samples or transformation.

**Diagnostic steps:**

1. Load `output/bootstrap_chain_1000.rds` (or whichever bootstrap result file contains the Excessive Hours RDS-I results at 100K population).
2. Inspect the bootstrap distribution for Excessive Hours RDS-I at 100K. How many of the 1000 (or however many) bootstrap replicates exceed the point estimate? If the answer is small (e.g., <50 / 1000), the upper quantile CI is unreliable.
3. Recompute the CI using a logit-transformed bootstrap CI: compute `qlogis(prevalence)` for each bootstrap replicate, take the 2.5% and 97.5% quantiles of that, then `plogis()` to transform back. This handles the ceiling more gracefully.
4. Alternatively use a BCa (bias-corrected and accelerated) bootstrap CI.

**Fix:** Apply whichever CI method gives a valid CI (one that contains the point estimate) and report it across all five indicators consistently. If a different CI method is used only for Excessive Hours, document why (it is the only indicator near the prevalence ceiling).

**Verification:** After the fix, every CI in ESM_4 Table 1 contains its own point estimate. Add a unit test in `R/tests/test_ci_contains_point_estimate.R` that loads each saved estimates file and asserts `lower <= point <= upper` for all rows.

---

## Task 4: Address documented reviewer concerns from the four desk-rejections

**Read these letters first:** `paper/decisions/` contains the IJOPM, JBE, IRJ, BJIR rejection letters. Read each. Most are short and frame the rejection as "scope" or "fit", but some contain substantive critique.

**Known specific critique (highest priority):**

The IJOPM reviewer flagged the wide 34%–84% range in prevalence estimates across indicators as a concern. This range is cross-indicator heterogeneity (document withholding ~34%, excessive hours ~84%), not statistical uncertainty within an indicator. The current paper conflates the two in places.

**Fix:** Add to the appendix and to the discussion section a per-indicator confidence interval display that clearly separates:

1. The point estimate for each of the five indicators (this is the cross-indicator variation).
2. The 95% CI around each point estimate (this is the within-indicator statistical uncertainty).

Concretely, produce a single forest plot in which:

- Rows = the five indicators
- Each row shows the point estimate (dot) and 95% CI (horizontal line) for that indicator separately
- Optionally with two rows per indicator: one for RDS-SS and one for NSUM (MBSU Moderate)

Save as `output/figures/per_indicator_forest_plot.png`. This will replace the current main paper Figure 3 or 4 if appropriate; coordinate with Task 2 above.

**Other reviewer concerns to look for in the rejection letters:**

- Small sample size (n=85). Address by: (a) honestly reporting it in the abstract; (b) showing that the Bayesian SS-PSE (post Task 1 fix) gives properly-wide credible intervals; (c) running a sensitivity analysis where you exclude the smallest nationality cluster and check whether estimates remain stable.
- Visibility / transmission bias in NSUM. Currently the paper reports MBSU with binary δ and τ adjustments (No / Moderate / Conservative). Replace with a CONTINUOUS sensitivity: sweep τ across {0.5, 0.6, 0.7, 0.8, 0.9, 1.0} and δ across {0.5, 0.75, 1.0, 1.25, 1.5}, produce a heatmap of prevalence for each indicator. Save as `output/figures/nsum_continuous_sensitivity_<indicator>.png`.
- Network non-representativeness. Show subgroup stability (already implemented in `08b-simple_subgroup_analysis.R`); ensure the results are surfaced in the appendix.
- Any reviewer concern about "selection bias" or "self-selection". If present, document the response in `R/analysis/_docs/reviewer_responses.md`.

**Deliverable:** A new file `R/analysis/_docs/reviewer_responses.md` documenting, for each substantive concern in each rejection letter, what was done to address it (or why it cannot be addressed in this revision).

---

## Task 5: Extend the analysis where genuinely useful (open-ended)

This task is exploratory. Document everything you try in `R/analysis/_docs/extension_attempts.md`, including the things that didn't work.

**Suggested extensions:**

1. **Model-Assisted Bayesian with informative priors.** The current MA estimator uses default priors. If you can specify a weakly informative prior on prevalence (e.g. Beta(2, 5) centred at 28%) you may get tighter credible intervals without overstating confidence. Verify any prior choice is defensible against a reviewer asking "why those parameters?".

2. **NSUM with the generalised Feehan-Salganik estimator.** The paper currently uses MBSU. Compare against the Feehan-Salganik 2016 generalised NSUM if not already done. Code template: `R/analysis/05-nsum_estimation.R` may already implement this; verify and ensure it appears in the comparison table.

3. **Subgroup analyses by nationality cluster.** Already partially implemented in `08-` and `08b-`. Ensure the Filipina-specific results (n=64, the largest single cluster, dominant in the network) are surfaced as a "best case" estimate and the small clusters (Latinx, British) are shown with appropriately wide CIs.

4. **Network diagnostic plots.** Recruitment chain depth, seed effect, sample composition. These speak to RDS sample quality and may answer a reviewer's "why should we trust this sample" objection.

**Do not invent extensions for their own sake.** Each extension should answer a question the manuscript actually needs answered. If a proposed extension does not have a clear addressee (reviewer concern, gap in current analysis), skip it.

---

## Task 6: Make the pipeline end-to-end reproducible

**Target state:** A fresh user with the repo and R 4.2+ installed can run

```bash
Rscript R/00-main_pipeline.R
```

…and the script runs to completion, regenerating every number and every figure in the manuscript and ESM_4. No intermediate manual steps. No prompts. No "see Scott's machine for the actual file."

**Concrete checklist:**

- `R/00-main_pipeline.R` runs all stages (01 through 08, including the 06b/06c/06d Bayesian sub-analyses and the figure-generation scripts in 07-).
- No script `source()`s a file that doesn't exist.
- No script writes to a path that doesn't exist (create directories with `dir.create(..., showWarnings = FALSE, recursive = TRUE)` as needed).
- Every cached `.RData` and `.rds` file in `output/` can be regenerated by running the pipeline from a clean state (no cached intermediates).
- A `renv.lock` or `DESCRIPTION` / `sessionInfo()` snapshot exists somewhere in the repo so package versions can be re-pinned.
- Random seeds are set explicitly at the top of every script that uses RNG (`set.seed(42)` or similar). Document this in `CLAUDE.md`.

**Test:** Move `output/*.RData`, `output/*.rds`, and `output/figures/` to a temporary `output_backup/` directory. Run `Rscript R/00-main_pipeline.R`. Compare the regenerated outputs to `output_backup/` — values should match within bootstrap noise (point estimates should be byte-identical given fixed seeds; CIs should be within bootstrap variation if seeds are set).

**Additionally:**

- Set up an OSF project mirror (manual step; document in `R/analysis/_docs/osf_setup.md` with the OSF URL). SIR requires authors to provide an anonymised reviewer-facing link to the analysis code at submission time.

---

## Reporting back

After completing each task, write a brief status entry to `R/analysis/_docs/track1_log.md` with:

- Date
- Task number and short description
- What was done
- What was verified (acceptance criteria met / not met)
- Open questions for Scott or Cowork Claude

If a task cannot be completed (technical blocker, unclear specification, conflicting requirement), log it under "open questions" and move to the next task rather than blocking.

When all tasks are complete, write a single-page summary at `R/analysis/_docs/track1_summary.md` covering: what changed, what was verified, what reviewer concerns are now addressed, what extensions were added, and what (if anything) is still outstanding.

## Hand-off to Track 2

Track 2 (Cowork Claude) needs the following deliverables to begin manuscript revision:

1. A fully reproducible pipeline (Task 6) — verifiable by running `Rscript R/00-main_pipeline.R`.
2. Regenerated figures in identified locations (Task 2) — either embedded in the .docx files or with a clear mapping in `output/figures/manuscript_figures/README.md`.
3. Corrected numerical results — `output/tables/AppendixA*.csv`, `output/bayesian_estimators_results.RData`, `output/bayesian_sensitivity_results.RData`, etc., all reflecting the post-fix analysis.
4. `R/analysis/_docs/reviewer_responses.md` — so Track 2 can incorporate the response language into the manuscript discussion.
5. `R/analysis/_docs/track1_summary.md` — the headline summary Track 2 will use to understand what changed.

Do not start any manuscript editing yourself. Hand off cleanly.
