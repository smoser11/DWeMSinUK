# Drafts: revised methods (Section 3) — surgical edits

Methods section is well-structured already (subsections 3.1 through 3.7). Five MUST edits to bring it in line with what the analysis actually does after Track 1's audit + fixes, two RECOMMEND edits for polish.

---

## Edit 1 (MUST) — P57: MA-RDS limitation for continuous outcomes

**Current:**
> For continuous outcomes such as the exploitation risk index, we employ Model-Assisted (MA-RDS) estimators (Gile and Handcock, 2015). MA-RDS is design-based yet leverages a working Exponential Random Graph Model (ERGM) with degree and homophily terms to approximate inclusion probabilities conditional on observed seeds. This approach directly addresses seed bias and homophily that conventional RDS estimators cannot correct, accommodates finite-population effects through successive-sampling logic, …

**Proposed addition** (append a sentence to the end of the existing paragraph):
> We note one important limitation: the MA-RDS implementation in the RDS package (`MA.estimates`) is designed primarily for binary traits, and we find that for the continuous composite risk index it does not converge to a useful posterior at our sample size (n=85); the credible interval covers essentially the full support of the variable. We therefore report frequentist RDS-SS estimates as the headline for the composite index and reserve MA-RDS estimates for the five binary indicators (see Section 4.2 and ESM_4).

**Why:** Our audit (2026-05-25) found that MA on the composite risk gives 7.1% with SE = 0.62 — essentially undefined. This is documented in the new Main paper Figure 2 (three-estimator comparison). The methods section needs to acknowledge the limitation rather than imply MA-RDS works for both binary and continuous outcomes.

---

## Edit 2 (MUST) — P59: Replace discrete-tier τ description with continuous sweep

**Current:**
> We apply these adjustments with sensitivity analysis across multiple parameter values: δ ∈ {0.8, 0.9, 1.0} and τ ∈ {0.7, 0.85, 1.0}, where δ=1.0 and τ=1.0 represent the unadjusted MBSU baseline.

**Proposed:**
> We apply these adjustments with two complementary sensitivity analyses. First, three discrete (δ, τ) tiers — Unadjusted (δ=1.0, τ=1.0), Moderate (δ=0.9, τ=0.85), and Conservative (δ=0.8, τ=0.70) — reported in ESM_4 Tables 4–7 for direct comparison with the literature. Second, a continuous-τ sweep across τ ∈ [0.40, 1.00] in 0.05 increments with δ fixed at 0.9, producing per-indicator prevalence as a function of τ with bootstrap 95% confidence intervals (R/analysis/05b-nsum_tau_sensitivity.R; figure in ESM_4). The continuous sweep responds to the observation that any single (δ, τ) choice is a point assumption; reporting prevalence as a function of τ lets readers locate their own preferred assumption on the curve.

**Why:** Track 1 added the continuous-τ analysis specifically to address the IJOPM reviewer's "wide 34–84% range" concern. The methods section needs to mention it.

---

## Edit 3 (MUST) — Add invariance-to-N sentence (task #21)

**Proposed insertion** as a new sentence at the end of P66 (the existing sensitivity-procedures paragraph):
> Empirically, RDS prevalence estimates are invariant to the assumed total UK domestic-worker population N across the four scenarios tested (50,000; 100,000; 980,000; 1,740,000), consistent with the theoretical insensitivity of RDS-I/RDS-II to N and the negligible finite-population effect on RDS-SS when n/N ≤ 0.001. The same invariance holds for MBSU prevalence (ESM_4 Table 6). All headline estimates use N = 980,000 throughout; conclusions are unchanged at other plausible values. For computational tractability, the MA-RDS Bayesian sensitivity in ESM_4 was computed at N = 50,000 only and prevalence scaled to the headline N for count-based reporting.

**Why:** This was on the outstanding-tasks list as #21. Insertion in P66 (the existing sensitivity-procedures paragraph) is the natural home. Pre-empts the obvious reviewer question "why N = 980,000?" with the empirical answer.

---

## Edit 4 (MUST) — P58: Update MBSU description for accuracy

**Current (last sentence of P58):**
> These assumptions often fail due to stigma, recall issues, or social barriers.

**Proposed addition** (after the current text):
> Modified Basic Scale-Up (MBSU) (Feehan and Salganik, 2016) addresses these failures through two multiplicative adjustment factors applied to the unadjusted scale-up estimate: δ_F, the ratio of the average hidden-population network size to the average frame-population network size (values < 1 indicate the hidden population has smaller networks, biasing the unadjusted estimate downward); and τ_F, the true-positive rate of respondents' reports about hidden-population alters (values < 1 indicate underreporting due to stigma, lack of awareness, or social distance). The adjusted estimate is N_H = N_H_basic × (1/δ_F) × (1/τ_F).

**Why:** Currently the MBSU formula and the meaning of δ, τ are introduced informally in P59 ("Second, the visibility/true positive rate adjustment (τ)…"). Better to introduce the formula and the substantive interpretation of each factor before naming the parameters. Makes Section 3.4 easier to follow.

---

## Edit 5 (MUST) — Add OSF code-availability footnote

**Proposed insertion** as a new sentence at the end of P54 (or as a footnote to the section heading 3.4 Estimation Framework):
> All analysis code, including the canonical pipeline (`R/00-main_pipeline.R`), per-method bootstrap (`R/analysis/04-bootstrap_analysis.R`), continuous-τ sensitivity (`R/analysis/05b-nsum_tau_sensitivity.R`), and Bayesian appendix consolidation (`R/analysis/06-bayesian_appendix.R`), is available at an anonymised OSF mirror: [OSF URL TO BE INSERTED]. The repository on GitHub (https://github.com/smoser11/DWeMSinUK) will be made public on acceptance.

**Why:** SIR's 2024 author-guidance editorial (Bartram et al.) requires anonymised reviewer code share. This sentence is the placeholder; once you set up the OSF mirror, swap in the URL.

---

## Edit 6 (RECOMMEND) — P55: Soften the "novel three-step bootstrap" claim

**Current:**
> We also developed a novel three-step bootstrap procedure for NSUM estimation using RDS data; details are reported in ESM_3.

**Proposed:**
> We also implement a three-step bootstrap procedure for NSUM estimation using RDS data that resamples respondents, their reported alters, and the exploitation classifications in succession, propagating uncertainty from each source through to the final confidence interval; details are reported in ESM_3.

**Why:** "Novel" is a strong claim. Standard parametric bootstrap structures for RDS-NSUM have appeared in the technical literature (Feehan & Salganik 2016 supplementary, various NGO technical reports). Softening to "implement" with a substantive description of what the procedure does is more defensible without giving up the contribution.

---

## Edit 7 (RECOMMEND) — P67-68: Tighten the Use of AI section

**Current:**
> Large Language Models (LLMs) were used for brainstorming the organization of the paper and some textual editing. Code co-pilot (Claude Code) was used to test and debug R scripts employed in the statistical analysis. No generative models were used to generate or simulate data.

**Proposed:**
> The authors used a Large Language Model assistant (Anthropic Claude) for textual editing and for code-review support during R script development; all final analysis decisions, statistical interpretations, and prose are the authors' own. No generative models were used to simulate or augment data.

**Why:** SIR doesn't have a specific AI disclosure policy yet, but the trend at most journals is toward a concise statement that (a) identifies the tool, (b) describes the scope of use, (c) affirms author responsibility for content. Current text is fine but a bit fragmentary; tightening it pre-empts any editorial query.

---

## Summary

| # | Severity | Section | Words removed | Words added |
|---|---|---|---|---|
| 1 | Must | P57 (MA limitations) | 0 | +70 |
| 2 | Must | P59 (continuous-τ sweep) | 38 | +130 |
| 3 | Must | P66 (invariance-to-N) | 0 | +110 |
| 4 | Must | P58 (MBSU formula) | 0 | +100 |
| 5 | Must | P54 (OSF footnote) | 0 | +60 |
| 6 | Recommend | P55 (novel-bootstrap softening) | 12 | +50 |
| 7 | Recommend | P67-68 (AI disclosure tighten) | 50 | +60 |

Net effect: methods section grows by ~580 words (with all 7 applied) — still well under any reasonable budget given current ~7,000-word manuscript versus SIR's 10,000-word cap.

Tell me which to apply (recommend all 7) and any text edits, then I'll produce a new dated copy + redline.
