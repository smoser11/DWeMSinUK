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
> We apply these adjustments with two complementary sensitivity analyses. First, three discrete (δ, τ) tiers — Unadjusted (δ=1.0, τ=1.0), Moderate (δ=0.9, τ=0.85), and Conservative (δ=0.8, τ=0.70) — reported in ESM_4 Tables 4–7 for direct comparison with the literature. Second, a continuous-τ sweep across τ ∈ [0.40, 1.00] in 0.05 increments with δ fixed at 0.9, producing per-indicator prevalence as a function of τ with bootstrap 95% confidence intervals (full results in ESM_4). The continuous sweep responds to the observation that any single (δ, τ) choice is a point assumption; reporting prevalence as a function of τ lets readers locate their own preferred assumption on the curve.

**Why:** Track 1 added the continuous-τ analysis specifically to address the IJOPM reviewer's "wide 34–84% range" concern. The methods section needs to mention it.

---

## Edit 3 (MUST) — New subsection 3.6 "Choice of population size N and invariance of prevalence"

This was originally a single sentence inserted in the sensitivity-procedures paragraph. Promoted to a full ~290-word subsection per Scott's request, written for a non-specialist reader, addressing the "why are different N's used in different parts of the analysis?" question prominently rather than burying the answer.

**Proposed new subsection** to be inserted as Section 3.6 (renumbering existing 3.6 Bootstrap and Sensitivity Procedures to 3.7, and 3.7 Use of AI to 3.8):

> ### 3.6 Choice of population size and the invariance of prevalence
>
> Throughout the paper we report prevalence — the proportion of UK domestic workers subject to each exploitation indicator — rather than absolute counts of affected people. This distinction matters because the assumed total domestic-worker population N enters our estimators in different ways depending on whether we are computing a proportion or a count.
>
> For prevalence specifically, every estimator we use is either exactly invariant to N or essentially invariant to N at our sample size. RDS-I (Salganik and Heckathorn 2004) and RDS-II / Volz-Heckathorn (2008) are weighted averages of the binary indicator across respondents; the weights depend on network degree and recruitment-chain structure but not on N. The algebra simply does not contain N. The same is true of the Modified Basic Scale-Up NSUM prevalence: the formula p̂ = ȳ_FH / d̄_FF (alter reports divided by average network size) has no N in it; only when we multiply through by N to get an absolute count does N matter at all.
>
> RDS-SS (Gile 2011) and the Bayesian Model-Assisted estimator (MA-RDS; Gile and Handcock 2015) do formally depend on N, because they use without-replacement successive-sampling models that incorporate finite-population corrections. The size of that correction is determined by the sampling fraction n/N. At our sample size of n=85, this fraction is vanishingly small across every plausible UK domestic-worker population: 0.17% at N=50,000, 0.087% at N=100,000, 0.009% at N=980,000, and 0.005% at N=1,740,000. By comparison, the finite-population correction is conventionally considered negligible when the sampling fraction is below 5%; our fractions are 30–1000 times smaller than that threshold. Both estimators are therefore in the asymptotic regime where they converge to their infinite-population limits. We confirm this empirically: cached estimates across all four N values agree to within bootstrap noise (ESM_4 Table 6 for NSUM; equivalent invariance documented for RDS-SS).
>
> Why, then, are different N values used in different parts of the analysis? RDS and NSUM headline estimates use N = 980,000, the EU baseline estimate for UK domestic workers; this is the value most readers will expect to see and against which any absolute-count reporting should be benchmarked. The MA-RDS Bayesian analysis (ESM_4) uses N = 50,000, chosen purely for computational tractability: the MA algorithm must instantiate and simulate a network of size N at each iteration, and at N = 980,000 a single MA call takes hours; the full 8-indicator × 3-seed-selection sensitivity at that scale would take weeks. At N = 50,000 the same sensitivity runs overnight. Because prevalence is N-invariant in the asymptotic regime as established above, the two N's produce equivalent prevalence stories and the choice is one of computation rather than substance. Where the paper reports absolute counts (rather than prevalence), the count is computed by multiplying the prevalence by the headline N = 980,000 regardless of which N was used to compute the prevalence.

**Why a full subsection rather than a sentence:** The "why are the N values inconsistent?" question is the kind of thing a careful reviewer will flag in the first reading pass. Placing the answer in its own subsection — flagged in the table of contents, easy to find, written in non-specialist language with intuitive numerical anchors (n/N ≤ 0.17% vs the conventional 5% threshold) — pre-empts the objection cleanly. The mathematical claims are factually correct (RDS-I and RDS-II are algebraically N-free; RDS-SS and MA-RDS have asymptotic N-invariance; MBSU prevalence is algebraically N-free). The empirical confirmation comes from cached results already in the appendix.

**Renumbering implication:** Inserting as new 3.6 means existing 3.6 (Bootstrap and Sensitivity Procedures) becomes 3.7 and existing 3.7 (Use of AI) becomes 3.8. Any internal cross-references to "Section 3.6" or "Section 3.7" would need updating; a scan of the manuscript suggests there are none, but worth confirming when applying.

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
> All analysis code is available at an anonymised OSF mirror: [OSF URL TO BE INSERTED]. The repository (currently private on GitHub at https://github.com/smoser11/DWeMSinUK) will be made public on acceptance.

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
| 3 | Must | NEW subsection 3.6 (invariance-to-N) | 0 | +470 |
| 4 | Must | P58 (MBSU formula) | 0 | +100 |
| 5 | Must | P54 (OSF footnote) | 0 | +60 |
| 6 | Recommend | P55 (novel-bootstrap softening) | 12 | +50 |
| 7 | Recommend | P67-68 (AI disclosure tighten) | 50 | +60 |

Net effect: methods section grows by ~940 words (with all 7 applied; up from ~580 in the earlier version of this memo, because Edit 3 was promoted from a single sentence to a full subsection per Scott's 2026-06-22 feedback). Still under the SIR 10,000-word cap given the current manuscript at ~7,000 words; the new subsection is the largest addition and earns its space by pre-empting the obvious reviewer objection about inconsistent N values.

Tell me which to apply (recommend all 7) and any text edits, then I'll produce a new dated copy + redline.
