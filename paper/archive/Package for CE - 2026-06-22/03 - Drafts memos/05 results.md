# Drafts: revised Results (Section 4) — surgical edits

Results section has more issues than Methods. The current structure presents all methods evenhandedly, has redundant subsections (4.4/4.5/4.6 each treats one method when 4.3 already promised a comparison), and still carries the now-discredited MA Bayesian composite-risk number (0.0124) that the May audit identified as artefactual.

Per Scott's framing decisions: (a) strong preferred-model framing with parenthetical pointers to robustness/sensitivity in ESM_4; (b) prior reviewer concerns addressed implicitly in Results (no "we address the concern that..." language; just present the corrected analyses cleanly), explicitly in Discussion later.

Eight edits below, ordered by severity. All MUST unless flagged.

---

## Edit R1 (MUST) — Add preferred-estimator opener at start of Section 4

**Proposed insertion** as a new paragraph after the "4. Results" heading and before "4.1 Descriptive Overview":

> We present results in five subsections. Section 4.1 describes the analytical sample. Sections 4.2 and 4.3 report the preferred prevalence estimates for the continuous composite risk index and the five binary ILO-aligned indicators respectively. For binary indicators, the preferred estimator is RDS-SS (Gile's Successive Sampling) with neighbourhood-bootstrap 95% confidence intervals; for the composite risk index, also RDS-SS, because the Bayesian Model-Assisted estimator does not converge to a useful posterior on continuous outcomes at our sample size (Section 3.4). The preferred NSUM estimator is the Modified Basic Scale-Up with Moderate adjustment (δ=0.9, τ=0.85). Section 4.4 reports supporting estimates — Bayesian Model-Assisted estimates for the five binary indicators, alternative RDS estimators (RDS-I, RDS-II), and NSUM under alternative adjustment-factor scenarios — included for comparison and to support the robustness claims in Section 4.5. Section 4.5 summarises the sensitivity analyses, with full details in ESM_4.

**Why:** This is the "strong preferred-model" framing Scott asked for. Reader knows from the first paragraph of Results what the headline estimators are, where to find them, and that everything else is supporting. The parenthetical pointer to Section 3.4 (where the MA-continuous limitation was explained) keeps the justification close at hand.

---

## Edit R2 (MUST) — Rewrite the composite-risk discussion (P78–84)

**Current state of these paragraphs (summarised):**
P77 (heading): "4.2 Model-Assisted Estimates of the Continuous Risk Index"
P78: introduces the continuous risk index as a central contribution
P79: justifies MA-RDS for continuous outcomes (now factually wrong — MA fails)
P84: reports "The population-weighted average risk is 0.0124" (the broken MA Bayesian estimate)

**Proposed replacement of the entire subsection (P77 heading + P78, P79, P84 bodies):**

> **4.2  Continuous Risk Index: RDS-SS Estimates**
>
> One of the central contributions of this study is the introduction of a continuous risk index measuring degree of exposure to labour exploitation. The index is constructed from the five binary ILO-aligned indicators (Section 2), weighted equally with an additional 0.35 weight for NRM referral reflecting its diagnostic priority in UK policy practice. Rather than treating exploitation as a dichotomy, the risk index conceptualises every domestic worker as facing some degree of potential exploitation, varying continuously in intensity from low to high.
>
> The preferred population-adjusted estimate uses Gile's Successive Sampling (RDS-SS) Horvitz-Thompson estimator with neighbourhood bootstrap. The estimated mean risk index in the UK domestic-worker population is 36.9% of the theoretical maximum (95% CI 30.3–43.5%), very close to the observed sample mean of 37.5% — indicating that the RDS-SS finite-population adjustment makes only a small correction at our sampling fraction. We report RDS-SS rather than the Bayesian Model-Assisted estimator for the composite index because, as noted in Section 3.4, MA-RDS does not converge to a useful posterior on continuous traits at this sample size; for completeness Figure 2 shows the MA estimate alongside the RDS-SS and sample estimates, with the wide MA credible interval making the limitation visible.

**Why:** Three things this fixes. First, removes the broken 0.0124 number that the audit flagged. Second, reports the actual headline number (36.9%, CI 30.3–43.5%) from the corrected analysis. Third, justifies the preferred-estimator choice in-line rather than burying it. Figure 2 (the three-estimator comparison) is already in place to support this prose.

---

## Edit R3 (MUST) — Restructure Section 4.3 as the headline binary-indicator results

**Current state:** P85–90 set up a comparison, but the actual comparison is then spread across 4.4 (Bayesian), 4.5 (Frequentist RDS), 4.6 (NSUM). Reader has to skip around.

**Proposed:** Consolidate 4.3 as the headline binary-indicator results with one figure and one table:
- Headline figure: Figure 4 (RDS-I and RDS-SS forest plot — the per-method bootstrap CIs)
- Headline table: a NEW Table 2 showing for each of the 5 indicators the RDS-SS prevalence with 95% CI, the MBSU (Moderate) NSUM prevalence with 95% CI side by side.

**Proposed replacement of P85–87:**

> **4.3  Binary Exploitation Indicators: Preferred Prevalence Estimates**
>
> Table 2 reports the preferred prevalence estimates for each of the five binary ILO-aligned indicators in the UK domestic-worker population, presenting both RDS-SS (ego-based; Salganik-Heckathorn and Gile's Successive Sampling lineage) and MBSU NSUM (alter-based; Feehan and Salganik 2016) estimates side by side. Figure 4 shows the RDS-SS estimates with per-method bootstrap confidence intervals for visual comparison with the RDS-I estimator.
>
> Prevalence is high for most indicators in this hidden population. RDS-SS estimates range from 34.2% for document withholding to 83.7% for excessive working hours; MBSU Moderate NSUM estimates range from 1.08% (document withholding) to 2.98% (excessive hours). The order of magnitude difference between RDS and NSUM is itself a substantive finding (Section 4.4), reflecting the difference between ego-based reports (the respondent's own experience) and alter-based reports (what the respondent observes in their network). Within methods, the ordering of indicators by prevalence is remarkably stable: excessive working hours consistently the highest, followed by threats and abuse and pay-related issues, with document withholding the lowest. This consistency across methods, even where absolute levels differ, is a robustness signal for the indicator hierarchy itself.

**Why:** Gives the reader the headline numbers up front in a single subsection rather than scattered across three. The "order of magnitude difference between RDS and NSUM is itself a substantive finding" sentence is the implicit address of the prior-reviewer "why do they differ?" concern, without naming it as such.

---

## Edit R4 (MUST) — Compress 4.4/4.5/4.6 into a single 4.4 "Supporting estimates and method comparison"

**Current state:** 4.4 Model-Assisted Bayesian Estimates (Table 2), 4.5 Frequentist RDS Estimates (Table 3), 4.6 NSUM Estimates (Table 4). Each subsection has its own table; the section feels repetitive.

**Proposed replacement (combine into a single 4.4 subsection):**

> **4.4  Supporting Estimates and Method Comparison**
>
> For each of the five binary indicators, three additional families of estimates are reported in the appendices for transparency and robustness:
>
> *Bayesian Model-Assisted (MA-RDS).* Table 2 (in ESM_4) reports the Gile-Handcock (2015) MA-RDS Bayesian estimates at the headline N = 50,000 with 95% credible intervals. Posterior means range from 34.1% (document withholding) to 84.0% (excessive working hours), broadly tracking the RDS-SS preferred estimates with slightly wider credible intervals — appropriate uncertainty quantification at n = 85. Figure 3 plots these as a forest.
>
> *Frequentist RDS-I and RDS-II.* Table 3 (in ESM_4) compares RDS-I (Salganik-Heckathorn) and RDS-SS estimates for each indicator with per-method bootstrap 95% CIs. RDS-I estimates run systematically higher than RDS-SS — for document withholding, 49.4% versus 34.2% — reflecting the known downward bias of RDS-I relative to the Successive Sampling estimator in this regime. Figure 4 visualises the comparison; we treat RDS-SS as the preferred binary estimator on standard methodological grounds (Gile 2011; Goel and Salganik 2010).
>
> *NSUM under alternative adjustment-factor tiers.* Table 4 reports MBSU NSUM estimates under three (δ, τ) tiers — Unadjusted, Moderate, Conservative. Conservative adjustments produce estimates approximately 1.8 times higher than Unadjusted; Moderate adjustments produce estimates approximately 1.3 times higher. Figure 5 shows the Moderate-tier NSUM forest plot. The continuous-τ sensitivity analysis (Section 4.5) supersedes this discrete-tier comparison for the substantive inference, but the tier comparison is retained here for direct comparability with the existing NSUM literature.

**Why:** Removes redundancy. Reader sees the supporting estimates in one place with explicit comparison logic. Each estimator is named, its preferred-or-supporting status is clear, and the pointer to the next subsection (continuous-τ) is in place.

---

## Edit R5 (MUST) — Replace Section 4.7 with 4.5 "Sensitivity and Robustness"

**Current state:** P117 heading "4.7 Robustness Checks", P118 body talks about "ESM3" and "comprehensive analysis comparing all prevalence estimates" but doesn't go into specifics.

**Proposed (renumber from 4.7 to 4.5 since 4.4/4.5/4.6 are now consolidated into a single 4.4):**

> **4.5  Sensitivity and Robustness**
>
> Three sensitivity analyses are reported in full in ESM_4. First, the *continuous-τ NSUM sweep* (Section 3.5; ESM_4 Figure 1) shows MBSU prevalence as a continuous function of the visibility/true-positive-rate adjustment τ for each of the five indicators, with δ fixed at 0.9. Prevalence estimates increase monotonically as τ decreases (as expected algebraically); the increase is modest in absolute terms across the plausible range τ ∈ [0.7, 1.0] and substantial only at low τ ≤ 0.5. This continuous picture supersedes the discrete-tier comparison (Section 4.4) for substantive inference about how visibility assumptions affect estimates.
>
> Second, the *MA-RDS seed-selection sensitivity* (ESM_4 Figure 2) shows that the Bayesian MA posterior estimates are insensitive to the choice of seed-selection method (degree, random, sample) at our sample size — different chains converge to the same posterior, confirming model robustness to initialisation.
>
> Third, *cross-indicator and cross-method consistency*: across the five indicators and the four main estimators (RDS-I, RDS-SS, MA-RDS, MBSU NSUM Moderate), the rank-ordering of indicators by prevalence is stable. Excessive working hours is consistently highest, followed by threats and abuse and pay-related issues, with document withholding the lowest. Absolute prevalence levels vary by method but the substantive picture — which exploitation behaviours are most common, which least — does not.

**Why:** Brings the new sensitivity work (continuous-τ, MA seed-selection) into Results explicitly. Implicitly addresses the IJOPM "wide 34–84% range" concern by separating cross-indicator heterogeneity (real, stable) from method-driven variation (also stable in its ordering). No mention of prior submissions; the work speaks for itself.

---

## Edit R6 (MUST) — Strike redundant/superseded passages

Three small deletions to support the restructuring above:

- **P84 last clause** ("The population-weighted average risk is 0.0124..."): delete entirely — superseded by Edit R2.
- **P112** (current NSUM characterisation: "Unadjusted estimates range from 0.83% to 2.28%... approximately 1.8 times higher... approximately 1.3 times..."): rewrite is folded into Edit R4 above; delete the original P112.
- **P118** (the current 4.7 "Robustness Checks" body): superseded by Edit R5; delete.

---

## Edit R7 (RECOMMEND) — Fix the "Fig1" typo and align other small typos

**P76** says "Sample characteristics are presented in Table 1 and Fig1." Should be "Figure 1". Trivial fix.
**P88** has a stray "[Insert about here]"-style placeholder that should be removed in the rendered version (likely already gone after our earlier edits, but worth verifying).

---

## Edit R8 (RECOMMEND) — Caption updates for Figures 3 and 5

Now that the manuscript treats Fig 3 (MA estimates) and Fig 5 (NSUM estimates) as supporting (rather than headline) figures, their captions can usefully note this:

- **Fig 3 caption:** add "Reported as a supporting Bayesian comparison; the preferred binary-indicator estimator is RDS-SS (Figure 4)."
- **Fig 5 caption:** add "Reported here at the Moderate (δ=0.9, τ=0.85) adjustment tier; for the continuous-τ sensitivity see ESM_4 Figure 1."

---

## Summary

| # | Severity | Section | Words removed | Words added |
|---|---|---|---|---|
| R1 | Must | New opener after 4. heading | 0 | +180 |
| R2 | Must | Rewrite 4.2 (composite risk) | ~140 | +180 |
| R3 | Must | Restructure 4.3 (binary headline) | ~135 | +210 |
| R4 | Must | Consolidate 4.4/4.5/4.6 → 4.4 | ~280 | +340 |
| R5 | Must | Replace 4.7 → 4.5 (sensitivity) | ~100 | +250 |
| R6 | Must | Strike superseded passages | ~150 | 0 |
| R7 | Recommend | Typo fix | 0 | 0 |
| R8 | Recommend | Caption updates for Figs 3 & 5 | 0 | +40 |

Net effect: results section grows by ~395 words (with all 8 applied). Manuscript total stays under SIR's 10K cap.

**Important note on table renumbering.** Edits R3 and R4 imply Table 2 (currently "MA estimates of binary indicators") gets repurposed or moved. Concretely: the proposed new headline table for Section 4.3 needs to be created and inserted as a new Table 2. The existing Tables 2 (MA), 3 (RDS), 4 (NSUM) become ESM_4 Tables. I'll produce the new Table 2 separately (it's just a 5-row × 4-column table: indicator, RDS-SS estimate, RDS-SS CI, NSUM Moderate estimate, NSUM Moderate CI) when you sign off on the edits.

Tell me which to apply (recommend all 8) and any text edits. Once you sign off, I'll apply edits and produce the new dated copy + redline + new Table 2.
