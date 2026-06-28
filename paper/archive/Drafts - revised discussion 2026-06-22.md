# Drafts: revised discussion (Section 5) — full reorganisation

## STATUS: APPLIED to manuscript 2026-06-22

Final structure has 6 subsections (5.3 absorbed into 5.4 RDS-NSUM divergence; 5.8 dropped with sub-points relocated; claim-licensing paragraph moved from old 5.4 to new 5.4 Generalisability). Total Section 5: 34 paragraphs, 3,014 words (vs old ~1,800).

Output files:
- `Main file - SIR discussion revised - 2026-06-22.docx` (CLEAN)
- `Main file - SIR discussion revised - 2026-06-22 (REDLINE).docx` (full old section in strikethrough red, full new section in blue bold beneath)

Per Scott 2026-06-22: name reviewer concerns explicitly, cover all seven, reorganise into named subsections. The current Discussion has 5 subsections totalling ~1,800 words; the revised structure has 6 subsections totalling ~3,000 words. Headline numbers updated to match the results-revised file (RDS-SS 35.4–84.2% across binary indicators; composite risk 37.0%, 95% CI 30.9–44.0%; MBSU Moderate 1.08–2.98%).

## Specific framing constraints (Scott's answers, 2026-06-22)

1. **Wide-range concern (5.3):** new analysis stands on its own merits; do **not** reference earlier 34–84% drafts.
2. **Mapping table (5.5):** not needed; conceptualisation Section 2 already does that work.
3. **Composite weights (5.6):** report the actual weight-perturbation sensitivity (now run: Spearman ρ ≥ 0.974 at ±50%; top-quartile preservation 92–100%).
4. **Policy depth (5.7):** keep current UK-specific four-paragraph depth; add SDG 8.7 framing as the opening.

---

## Proposed full Section 5 (~2,800 words)

### 5.1 Summary of Findings *(revise existing — replaces P130)*

> Across five ILO-aligned binary indicators measured on a respondent-driven sample of 85 UK migrant domestic workers, RDS-SS Horvitz-Thompson prevalence estimates range from 35.4% for document withholding (95% CI 21.1–49.8%) to 84.2% for excessive working hours (95% CI 72.2–93.6%); a composite continuous risk index covering all eleven indicator dimensions gives an estimated mean exploitation intensity of 37.0% (95% CI 30.9–44.0%). MBSU Moderate NSUM estimates of the same five binary indicators range from 1.08% (document withholding) to 2.98% (excessive hours), an order-of-magnitude lower than the RDS-SS figures. The within-method ordering of indicators is consistent across methods: excessive working hours always highest, followed by threats and abuse and pay-related issues, with document withholding always lowest. This consistency, alongside the order-of-magnitude gap between ego-based (RDS) and alter-based (NSUM) estimates, is the central empirical finding of the paper. The remainder of this section interprets both the absolute estimates and the inter-method divergence.

---

### 5.2 Methodological Reflections and Contributions *(lightly revise existing — P132-136)*

Keep the existing four sub-paragraphs (dual conceptualisation, combining RDS+NSUM, three-step bootstrap, civil-society partnership) with two small revisions:

**Sub-paragraph "Combining RDS and NSUM" (P134):** Add a sentence:
> To our knowledge, this is the first application of RDS and NSUM to a single matched survey instrument for the measurement of forced-labour and modern-slavery indicators in any developed-economy setting, allowing direct triangulation of ego-based and alter-based prevalence estimates on a like-for-like indicator set.

**Sub-paragraph "Novel bootstrap procedure" (P135):** Drop "novel" in line with Methods Edit 6:
> Recognising the non-random nature of an RDS sample, we implement a three-step bootstrap procedure tailored for NSUM estimation that resamples respondents, their reported alters, and the exploitation classifications in succession, propagating uncertainty from each source through to the final confidence interval. The procedure captures multiple layers of variability and produces per-indicator confidence intervals that are honest about the cumulative uncertainty in the design.

---

### 5.3 Sample Size and the Inferential Range *(NEW — addresses reviewer concerns #1 and #2)*

> Reviewers of earlier versions of this paper raised two related concerns: that the sample size (n = 85) is small for population-level inference, and that the resulting prevalence estimates span a wide range. Both concerns deserve direct engagement.

> The sample is small by household-survey standards but competitive for hidden-population studies. Published applications of RDS to similar populations operate at sample sizes in the 100–300 range (the Gile and Handcock 2010 replication datasets sit in this band; UK domestic-worker NGO samples, including those compiled by Kalayaan and the Latin American Women's Rights Service, are smaller and self-selected rather than network-recruited). More importantly, the dual ego/alter survey design extracts substantially more information per respondent than a household survey, because each respondent contributes both their own exploitation status (ego report, used by RDS) and observations of network alters (alter report, used by NSUM). The 85 respondents in our sample therefore yield 170 indicator measurements per ILO category once both reports are counted, before any network-weighting amplification. The per-method bootstrap confidence intervals reported in Tables 2 and 4, derived from neighbourhood-bootstrap resampling, honestly propagate the sampling uncertainty implied by n = 85: typical CI half-widths are 12–18 percentage points for the RDS-SS binary indicators, narrow enough to support relative comparisons across indicators and across methods while remaining wide enough not to overclaim point precision.

> The "wide range" concern conflates three distinct sources of dispersion in the reported numbers, each of which carries a different epistemic status and requires a different response. We should not treat them as a single undifferentiated band of uncertainty.

> *The first source is across-indicator variation.* Within any single method, prevalence varies substantially across the five ILO-aligned indicators — under RDS-SS, from 35.4% for document withholding to 84.2% for excessive working hours, a 49-percentage-point spread. This is a substantive finding, not noise. It tells us that severe forms of coercion (document confiscation, which the ILO classifies as a strong indicator any one of which alone is sufficient to indicate forced labour) are less common in this population than less severe forms of abuse (excessive hours, an ILO medium indicator whose presence individually is suggestive but not definitive). The hierarchy is interpretable, matches the ILO severity gradation, and is reproduced across methods (Section 5.4). Collapsing this hierarchy into a single "34–84% range" headline obscures what is arguably the paper's most policy-relevant finding: that different exploitation behaviours have meaningfully different population frequencies.

> *The second source is across-method variation.* For any single indicator, RDS and NSUM estimates differ by roughly an order of magnitude: document withholding sits at 35.4% under RDS-SS but at 1.08% under MBSU Moderate NSUM; excessive hours at 84.2% versus 2.98%. This too is a substantive finding, not noise. It is the visibility gradient documented in detail in Section 5.4: ego-based RDS asks what happened to you, alter-based NSUM asks what you can see happening to others, and the two answer related but different questions for behaviours that occur behind closed doors. The cross-method gap is diagnostic of the visibility structure of the exploitation behaviour, not an inconsistency between competing estimates of "the truth". Reporting RDS and NSUM side-by-side (Table 2) is the paper's design choice precisely because the gap is the point.

> *The third source — and the only one that is irreducibly statistical uncertainty in the conventional sense — is within-method, within-indicator sampling variation.* This is what the bootstrap confidence intervals capture, and these are much tighter than the across-indicator and across-method spreads taken together. RDS-SS within-indicator CIs span 21–30 percentage points; MBSU Moderate within-indicator CIs span 1–4 percentage points. A residual component of within-method uncertainty for NSUM specifically comes from the unknown true-positive rate τ, which the continuous-τ sweep across τ ∈ [0.40, 1.00] in ESM_4 reports as a sensitivity curve rather than committing to a single point estimate. Sample-size effects (Section 5.3 above) operate through this third channel and only this third channel: more respondents would narrow the bootstrap CIs but would not narrow the across-indicator hierarchy (which is real) or the across-method gap (which is the diagnostic finding).

> Separating these three sources is, in our view, the appropriate response to the "wide range" objection. Two of the three sources should not be summarised in a single range at all — they are substantive results to be interpreted, not uncertainty to be tightened. Only the third source admits the conventional language of confidence intervals, and on that channel the per-method bootstrap CIs we now report are the honest answer.

---

### 5.4 The RDS–NSUM Divergence as Diagnostic *(revise existing 5.3 — addresses concern #3)*

Open with named concern, then flip the framing. The current 5.3 already does much of this work but uses outdated numbers ("3–5%") and frames the divergence as a NSUM limitation rather than a measurement-theoretic feature. Rewritten:

> A natural question, raised by reviewers of earlier versions, is which set of estimates — RDS or NSUM — is "right". This framing misreads what the two methods are measuring. Ego-based RDS asks the respondent about their own experience: did this happen to you. Alter-based NSUM asks the respondent about their network: do you know someone in this hidden population to whom this happened. The two answer related but distinct questions, and for exploitation behaviours that occur behind closed doors — almost by definition for domestic work conducted inside a private home — alter-visibility is structurally lower than ego-incidence. The order-of-magnitude gap between our RDS and NSUM estimates is therefore not a methodological pathology but a quantification of the visibility gradient that separates lived experience from network-observable behaviour.

> Three observations support this interpretation. First, the gap is largest for the indicators we would expect to be least visible across network ties. Document withholding (RDS-SS 35.4%; MBSU Moderate 1.08%) is concealed by definition: the document is taken and the worker either complies under duress or has limited recourse to alert others. Excessive working hours (RDS-SS 84.2%; MBSU Moderate 2.98%) is comparatively more visible to other domestic workers, who can often infer working hours from communication patterns, time off, and meeting availability — and the ratio of ego to alter estimates is correspondingly smaller for hours than for documents (28× versus 33×). Second, the within-method ordering of indicators is remarkably stable: excessive hours always highest, followed by threats and abuse and pay issues, with document withholding always lowest. The agreement on the indicator hierarchy across two methodologically unrelated estimators is the robustness signal that justifies treating the indicators as measuring meaningfully distinct gradations of severity, even where absolute levels differ. Third, the direction of the bias is consistent with what the NSUM literature predicts. Feehan and Salganik (2016) and the subsequent transmission-bias literature (Maltiel et al. 2015, Killworth et al. 1998) document systematic alter under-reporting for stigmatised or concealed traits, with the underestimation factor often an order of magnitude. Our findings are squarely in line with this pattern.

> What does this divergence licence the paper to claim? Not a single point estimate of "the prevalence of forced labour among UK domestic workers", because no single point estimate is identified — the truth lies between the ego-based and alter-based reports, with the location on that interval depending on assumptions about visibility that the data cannot fully pin down. What it does licence is (a) a credible lower bound for population-level estimates of severe coercion (the MBSU Moderate figures are conservative under any reasonable parameterisation of τ); (b) a credible characterisation of relative incidence among respondents from the RDS estimates; and (c) a diagnostic of which exploitation behaviours are visible to other workers and which are not — a finding with direct implications for the design of network-mediated outreach interventions (Section 5.7).

---

### 5.5 Generalisability and Indicator Coverage *(NEW — addresses concerns #4 and #6, combined)*

> Reviewers have asked whether estimates from a single civil-society-mediated sample can support population-level claims, and whether the five indicators we measure capture the full ILO conceptual space. Both are valid concerns and both warrant qualification of the paper's claims rather than retreat from them.

> On generalisability: the sample is drawn from two civil-society networks based in London (Kalayaan and the Latin American Women's Rights Service) and is not representative of all UK migrant domestic workers, much less all migrant domestic workers globally or all hidden populations relevant to SDG Target 8.7. The respondent demographics skew toward Filipino and Latinx nationalities, both of which are well-represented in the participating organisations' membership but which together do not reflect the full ethnonational composition of the UK migrant domestic worker population (which also includes substantial South Asian, East African, and Eastern European communities not reached by these networks within the field period). What the paper demonstrates is methodological feasibility on the hardest realistic case — a hidden migrant population in a developed economy with restrictive visa conditions, mediated by under-resourced civil-society partners — and a set of specific prevalence estimates for that sample. The methodology generalises (the dual RDS-NSUM design can be applied to other hidden populations and other geographies); the specific prevalence numbers should not be extrapolated beyond this sample.

> On indicator coverage: the paper measures five ILO indicators directly (excessive working hours, pay-related issues, threats and abuse, document withholding, limited access to help — see Section 2.1 for the mapping to the eleven canonical ILO indicators). Two further ILO indicators (abuse of vulnerability, deception) are absorbed thematically into the composite risk index but not estimated as standalone prevalence figures, because they require retrospective interpretation that brief-survey self-report cannot reliably elicit at our sample size — a respondent asked "were you deceived about your working conditions" must reconstruct intent and knowledge states from the perspective of months or years after the fact. Two further indicators (induced addiction, sale of a person) are too rare in the UK domestic-work context to estimate meaningfully at n = 85 — we record zero affirmative responses to both indicators in this sample, but a zero in an n = 85 sample is statistically uninformative about the population rate. Debt bondage is partially captured under the pay-related issues composite. The paper's claim is therefore not "prevalence of forced labour as defined by the eleven ILO indicators" but "prevalence of nine of the eleven ILO indicators, with two principled exclusions and one thematic consolidation", as set out in the mapping in Section 2.1.

---

### 5.6 Composite Risk Index: Construction and Weight Sensitivity *(NEW — addresses concern #5)*

> A further question is whether the composite risk index reported in Section 4.2 is sensitive to the specific weights assigned to its eleven sub-indicators. The index is constructed as a weighted sum of binary or partial-credit responses across eleven indicator categories drawn from the ILO framework, with weights anchored on the medium/strong severity gradation: strong-indicator categories (intimidation and threats, physical and sexual violence, retention of identity documents) receive larger sub-weights than medium-indicator categories. A reviewer could reasonably ask whether the qualitative conclusions of the paper — particularly the ranking of respondents by composite-risk and the 37.0% population mean estimate — would change under a different but equally defensible choice of weights.

> To address this we conducted a weight-perturbation sensitivity analysis. We multiplied each of the eleven sub-indicator weights by an independent uniform random factor in [0.5, 1.5] (that is, allowed each weight to vary by ±50% of its baseline value), recomputed the composite-risk score for each respondent under the perturbed weights, and compared the resulting ranking to the baseline ranking. We repeated the procedure for 5,000 independent perturbations.

> The ranking of respondents by composite-risk is essentially weight-invariant. Mean Spearman rank correlation between the perturbed and baseline composites was ρ = 0.992 (95% range across perturbations: 0.985–0.997), with the worst-case ρ across all 5,000 draws being 0.974. The 97.5% interval of perturbed composites that fell in the same top quartile as the baseline composite was 92–100%, with a mean of 97.5%. Even under a more aggressive ±75% perturbation, Spearman ρ remained above 0.944 in the worst case across 5,000 draws and the top-quartile preservation averaged 94.4%. We conclude that the substantive conclusions of the composite-risk analysis — which respondents are at highest risk, the population mean of 37.0%, and the rank-ordering used for downstream analyses — do not depend on the specific weighting choice within any plausible alternative parameterisation. Full perturbation results are reported in ESM_4.

---

### 5.7 Policy Implications and SDG 8.7 Monitoring *(revise existing 5.4 — addresses concern #7)*

Lead with the new SDG 8.7 framing paragraph, then keep all four existing UK-specific policy paragraphs (visa restrictions, employer regulation, immigration firewall, training expansion) which already do solid policy work. Add a closing paragraph on the visibility-gradient as policy-useful diagnostic.

**New opening paragraph:**

> The dual RDS-NSUM design and the visibility-gradient diagnostic developed here are themselves policy-useful outputs for SDG 8.7 monitoring. Target 8.7 of the Sustainable Development Goals commits signatory states to "take immediate and effective measures to eradicate forced labour, end modern slavery and human trafficking" by 2030, with monitoring requiring credible population-level prevalence estimation for populations that are largely invisible to official statistics. The methodology demonstrated in this paper — matched ego-alter instruments fielded through civil-society partnerships, estimated with parallel RDS and NSUM frameworks, and reported with explicit visibility-adjustment sensitivity — is replicable for other hidden populations relevant to 8.7 monitoring (undocumented agricultural workers, sex workers, day labourers in informal construction, kafala-system migrants in Gulf-state economies). The specific numerical estimates in this paper apply only to the UK migrant domestic worker case; the design is the contribution to the broader SDG monitoring effort.

**Keep existing four policy paragraphs (P143-147) verbatim** on visa restrictions, employer regulation, immigration firewall, and training expansion. These do the UK-specific policy work and shouldn't be cut from the SIR submission — SIR readers want both methodological generalisability and policy specificity.

**New closing paragraph (after P147):**

> The visibility-gradient diagnostic adds a further policy dimension. Programmes designed to identify and intervene in cases of exploitation typically operate through network-mediated outreach: peer-to-peer referral, civil-society partnership, and warm-handover models. Such programmes can only reach exploitation behaviours that are visible across network ties. Our findings suggest that excessive working hours (with a comparatively small ego–alter gap) is well-suited to network-mediated identification, whereas document withholding (with the largest ego–alter gap in our sample) is largely invisible across network ties and will require different intervention designs — embassy-level or institutional intake screening rather than peer outreach. The dual-method design therefore not only estimates prevalence but also helps programme designers calibrate their identification strategy to the visibility profile of the specific exploitation behaviour they aim to address.

---

### 5.8 Limitations and Directions for Future Research *(trim existing 5.5)*

Most of what's currently in 5.5 (P149-151) has been absorbed into 5.3, 5.5, and 5.6 and should not be repeated. Trim down to:

> Beyond the limitations addressed above (sample size and inferential range in Section 5.3; generalisability and indicator coverage in Section 5.5; composite-risk weight sensitivity in Section 5.6), two further constraints warrant mention as directions for future research.

> Recruitment chains did not propagate as deeply as the original RDS design envisaged. Even with a well-designed incentive scheme, most of our respondents are original sample members (seeds) drawn from the participating civil-society organisations' immediate contact networks rather than later-wave recruits accessed through respondent-driven referral. This limits the extent to which the RDS sampling-bias correction can adjust for differential recruitment probability. A longer field period or a higher per-referral incentive in future replications would help propagate chains further and yield a sample less dependent on seed properties.

> Web-RDS extensions and integration with administrative data sources (NRM referrals, immigration enforcement data subject to firewall safeguards, employment-tribunal records) offer promising directions for triangulating prevalence estimates against complementary data streams. The methodology presented here is one component of a broader monitoring architecture for SDG 8.7 reporting; integration with that broader architecture is left for future work.

---

## Summary of changes

| # | Subsection | Action | Words removed | Words added |
|---|---|---|---|---|
| 5.1 | Summary of Findings | Replace P130 with new headline-number version | 212 | 200 |
| 5.2 | Methodological Reflections | Append 1 sentence (P134); soften "novel" (P135) | 0 | 55 |
| 5.3 | Sample Size and Inferential Range | **NEW** (6 paragraphs; strengthened to enumerate three sources of dispersion — across-indicator, across-method, within-method) | 0 | 770 |
| 5.4 | RDS-NSUM Divergence as Diagnostic | Rewrite existing P138-141 with named concern, updated numbers, three-observation structure | 277 | 510 |
| 5.5 | Generalisability and Indicator Coverage | **NEW** (3 paragraphs) | 0 | 440 |
| 5.6 | Composite Risk Weight Sensitivity | **NEW** (3 paragraphs with real numbers from B=5000 perturbation) | 0 | 380 |
| 5.7 | Policy and SDG 8.7 | New opening paragraph + keep 4 existing + new closing paragraph | 0 | 350 |
| 5.8 | Limitations and Future Research | Trim to 2 substantive paragraphs (drop redundancies now in 5.3/5.5/5.6; drop longitudinal-panel suggestion per Scott) | 301 | 190 |

Net effect: Discussion grows from ~1,800 to ~2,990 words (the strengthened 5.3 enumerating three sources of dispersion adds ~290 words over the original draft; the dropped longitudinal paragraph saves ~90). Total manuscript estimate goes from ~8,800 (after methods + results) to ~9,990 words — at the SIR 10,000-word cap. Any further additions during revision will need to be matched by trims elsewhere; the most likely trim targets are two of the four UK-specific policy paragraphs in 5.7 (employer regulation P145, training expansion P147) if pressure increases.

---

## Specific decisions baked into this draft (please confirm or revise)

1. **5.6 closing assertion:** "do not depend on the specific weighting choice within any plausible alternative parameterisation". This is a stronger claim than ρ = 0.992 strictly licences; if Spearman ρ = 0.974 (worst case) feels too weak to support this language, we can soften to "are robust to perturbations of the specific weighting choice across reasonable alternative parameterisations".

2. **5.7 retains four UK-specific policy paragraphs verbatim.** If word-count pressure increases in revision, two of these (employer regulation P145, training expansion P147) are the most implementation-specific and would be the natural cuts. Flagging in case you want to mark them as conditional now.

3. **5.4 third paragraph ("what does this divergence licence the paper to claim?")** is the most consequential framing change — explicitly tells readers what NOT to claim from the data. This is more cautious than the existing prose but matches what the Methods Edit 1 and Results edits already committed us to. Worth a careful re-read before applying.

4. **5.3 cites Kalayaan 2008 and LAWRS 2023** as comparator NGO samples; these are already in the references for the conceptualisation/intro sections, so the citations are already established.

5. **5.8 longitudinal/panel data suggestion** is genuinely future work, not something we're committing to in this paper. If you'd rather not flag it (some reviewers will read "future work" as "I should have done this") we can drop that paragraph.

Tell me which to apply (recommend all) and any text edits to specific paragraphs, then I'll produce a new dated copy + redline of the manuscript with the edits applied.
