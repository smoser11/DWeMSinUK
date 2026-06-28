# Package for Caroline — SIR submission review (23 June 2026)

This folder collects everything needed to review the full SIR-revision pass on the manuscript. The submission-ready manuscript sits in folder 01 (use the 2026-06-23 version; the 2026-06-22 file is kept alongside as a historical reference). Section-by-section redlines (showing what changed at each step) are in folder 02. The rationale memos for each round of edits are in folder 03. All four supplementary Electronic Supplementary Material files (ESM_1 through ESM_4) are in folder 04 — these are the Online Resources the main manuscript cross-references at Sections 2, 3, and 4.

The full sequence of edits ran across 22 June 2026 in eight rounds: abstract + contribution paragraph; introduction; conceptualisation (Section 2) with the ILO medium/strong indicator hierarchy explicit; methods (Section 3) with new subsection 3.6 on the invariance of prevalence to assumed N; results (Section 4) with a new headline Table 2 and preferred-estimator framing throughout; discussion (Section 5) reorganised into six named subsections that name prior reviewer concerns explicitly; and conclusion (Section 6) rewritten to lead with the visibility-gradient diagnostic and anchor to SDG 8.7 monitoring. All eight rounds are now applied to a single submission-ready file that sits at 9,992 words (8 under the SIR 10,000-word cap).

## What's in each folder

### 01 - SUBMISSION READY
The single .docx that's the actual candidate for submission to SIR. All section edits applied; word count 9,992 (main body + tables, excluding abstract / references / figure legend, which is SIR's conventional counting). Five figures and four tables embedded inline at their cross-reference points. If something's wrong with this file it's the version we need to fix.

### 02 - Section redlines
Eight files showing, in order, what changed at each round. Old text is struck through in red; new text is in blue bold. Each redline is against the previous round's clean version, not against the original starting point — so to see the cumulative change you read them in order 01 → 08. For most rounds the redline is paragraph-by-paragraph and surgical. Round 07 (Discussion) is a more substantial reorganisation (from five subsections to six, with content relocated across them), so its redline shows the entire old Section 5 in strikethrough red at the top with the entire new Section 5 in blue bold beneath rather than attempting a surgical diff.

### 03 - Drafts memos
Six markdown files documenting the rationale for each round of edits — what each proposed edit changes, why, and (where Scott raised specific concerns) the framing decisions and trade-offs we worked through before applying. These are useful if any specific edit looks puzzling in the redline and you want to see the argument behind it.

### 04 - Supplementary
The four Online Resources the main manuscript cross-references:

- `ESM_1.docx` — full construction details for the composite risk index (referenced in Section 2 and Section 4.2 of the manuscript)
- `ESM_2.docx` — survey instrument and indicator question wording (referenced jointly with ESM_1 in Section 3.2)
- `ESM_3.docx` — full description of the three-step bootstrap procedure for NSUM estimation (referenced in Section 3.4)
- `ESM_4 (current).docx` — sensitivity analyses, Bayesian model-assisted estimates, NSUM adjustment-factor tables, and the continuous-τ sweep (referenced throughout Section 4 and Section 5). This is the regenerated-figures version from the 2026-06-20 Track 1 audit fix pass; ESM_1 through ESM_3 are unchanged from the 22 May version.

## What we'd most like your eyes on

A few specific calls in the revision are worth a careful read before submission:

1. **Conclusion P2 framing of the RDS–NSUM gap.** The Discussion (5.4) and the Conclusion both argue the order-of-magnitude divergence between RDS-SS (35.4–84.2%) and MBSU Moderate NSUM (1.08–2.98%) is a substantive visibility-gradient finding rather than a methodological inconsistency. This is the paper's distinctive contribution under the SIR framing but it's a strong claim that limits what we can say about absolute prevalence. The claim-licensing paragraph in Discussion 5.4 (third paragraph of that subsection) is the most consequential framing change in the whole pass — please read it carefully.

2. **Discussion 5.3 three-source-dispersion framing.** The "wide 34–84% range" concern from the IJOPM reviewer is now answered by distinguishing across-indicator variation, across-method variation, and within-method-within-indicator sampling uncertainty. The argument is that two of the three sources are substantive findings, not noise, and should not be summarised in a single range. Worth checking that the framing reads as principled rather than defensive.

3. **Discussion 5.5 composite-weights sensitivity.** A new B = 5,000 weight-perturbation analysis shows Spearman ρ = 0.992 (mean) / 0.974 (worst case) between perturbed and baseline composite-risk rankings under ±50% perturbation of all eleven sub-indicator weights. The conclusion is "the composite-risk ranking is essentially weight-invariant" — please confirm you're comfortable with that language, which is stronger than ρ = 0.974 strictly licences.

4. **Section 4.5 sensitivity language.** The continuous-τ sweep across τ ∈ [0.40, 1.00] now sits in Section 4.5 (and is documented in detail in ESM_4). Methods edit 2 promotes this from a passing mention to a full sensitivity-analysis paragraph. If you want stronger or weaker language on what the sweep licences us to claim, this is where to flag it.

5. **UK-specific policy paragraphs in Discussion 5.6.** All four paragraphs from your original draft are retained verbatim (visa restrictions, employer regulation, immigration firewall, training expansion). These were the closest call in the revision — SIR is not a UK policy journal, and there was a real question of whether to cut them in favour of more generic SDG 8.7 framing. We kept all four on the principle that SIR readers want both methodological generalisability and policy specificity, but if you read them and think two of them (employer regulation, training expansion) are the candidates for trimming if word-count pressure increases in revision, that's the natural cut.

## What's still TODO before submission

Three items remain outstanding:

1. **OSF code-share mirror.** SIR requires an anonymised view-only OSF link to the analysis code. The repo is at github.com/smoser11/DWeMSinUK and is currently private; setting up the OSF mirror is a one-hour job that Scott still has to do before submission. Methods Section 3.4 has a placeholder `[OSF URL TO BE INSERTED]` waiting to be replaced.

2. **Final cross-reference audit.** Section numbers, table numbers, figure numbers have all been touched in this round. A final read-through to make sure no internal cross-reference points to a now-wrong number is worth one careful pass before submission.

3. **Co-author confirmation.** Most fundamentally, your green light on the revisions.

Everything in this package is also tracked in the GitHub repository at `paper/OneDrive_1_22-05-2026/SIR/`. If you'd prefer to review by browsing the repo directly rather than working from this folder, the same files are there commit-by-commit with full history.
