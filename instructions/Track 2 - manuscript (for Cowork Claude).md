# Track 2 — Manuscript revision for SIR submission

**Audience:** Future Cowork Claude session (probably me, possibly a sibling session).
**Authored by:** Cowork Claude, 2026-05-25.
**Companion document:** `instructions/Track 1 - analysis (for Claude Code).md` — the analysis work that must complete before this track can produce a finished product.

---

## The task in one sentence

Revise the DWeMSinUK manuscript for submission to Social Indicators Research, incorporating (a) the SIR-specific positioning shift documented in the strategy memo and (b) the corrected analytical results from Track 1, producing both a redlined version and a clean version of the final submission package.

## What this track is NOT

- **Do not modify R code.** Track 1 owns the analysis. If a number in the manuscript needs to change because Track 1 produced a different result, update the manuscript prose; do not edit the script that produced the number.
- **Do not regenerate figures.** Track 1 owns figure generation. Use whichever PNGs Track 1 produces.
- **Do not re-run the audit.** The audit was the diagnostic that produced Track 1's task list. After Track 1 is done, the audit's findings should be moot. If they're not, that means Track 1 is not actually complete — flag it to Scott rather than fixing it yourself.
- **Do not send any email to Caroline.** That decision is Scott's. If a follow-up draft is needed, draft it for him to review and send (or not send, as he chooses).

## Preconditions (verify these before starting)

1. **Track 1 status:** Read `R/analysis/_docs/track1_summary.md` first. If it doesn't exist, Track 1 has not been completed and Track 2 cannot produce final output. You may begin some preparatory work (verifying preconditions, reading the artifacts below) but stop before applying any analytical-number changes to the manuscript.
2. **Caroline's reply:** Check `correspondence/2026/` for a reply from Caroline to the 24 May 2026 strategy email. Her gut-checks on (a) whether the proposed contributions are authentic, (b) how novel the methodological contributions actually are, (c) whether there is a better-fit venue than SIR — these may change the positioning before you apply it.
3. **Audit report:** Read `paper/OneDrive_1_22-05-2026/SIR/Statistical audit report - 2026-05-25.docx`. You need to know what was broken so you can verify Track 1 actually fixed it.
4. **Positioning memo:** Read `paper/OneDrive_1_22-05-2026/SIR/SIR positioning analysis - 2026-05-24.docx`. The drafted replacement abstract, contribution paragraph, and conceptualization paragraph are in there.
5. **Assembled manuscript:** The working document is `paper/OneDrive_1_22-05-2026/SIR/Complete manuscript - assembled 2026-05-24.docx` — main text with figures and tables embedded inline.

## The four uncertainties acknowledged in the 24 May 2026 strategy thinking

Before you start editing, remind yourself these are JUDGMENT CALLS not facts:

1. The "OSCM-language caused the four desk-rejections" diagnosis is reasoned inference, not proof. Desk-rejection letters rarely state the real reason.
2. "Zero recent SIR articles on RDS/NSUM/modern slavery means SIR has a gap to fill" could equally mean SIR has implicitly declared this is not a fit.
3. The proposed three methodological contributions (dual ego/alter instrument, three-step bootstrap, diagnostic reading of RDS-NSUM divergence) have not been pressure-tested against HIV-/sex-work-prevalence literature. A specialist reviewer who has done similar work may not see these as novel.
4. Reframing as "methods demonstrated on the hardest realistic case" is standard at methodology journals but harder at substantive journals like SIR — reviewers may want defensible prevalence estimates or a methods-validation paper on a known population, neither of which we have.

These uncertainties were deliberately not surfaced in the email to Caroline (Scott's call, after careful discussion). Surface them naturally in dialogue with her if she raises them, or in your reasoning to Scott if you spot something that contradicts the original positioning.

## Workflow for applying the edits

Scott's stated preference (from the 25 May 2026 session) is **clean save + redline copy**:

- One clean .docx that represents what would be submitted to SIR.
- One redlined .docx that shows all changes between the prior assembled manuscript and the clean version, with insertions/deletions highlighted or struck through.

Use `python-docx` (already installed) to apply edits. For the redline, the simplest approach is colour-coding (new text in blue, deleted text in red with strikethrough) rather than Word's native Track Changes — python-docx does not support Track Changes generation well. Document in the file's title-page postscript that the redline uses colour-coding, not Word Track Changes.

## The specific edits to apply

Once Track 1 is complete and Caroline has responded (or Scott has decided to proceed without her response), apply these edits to the assembled manuscript:

### 1. Abstract

Replace the current abstract with the version drafted in the SIR positioning memo (section 3.1, "Proposed reframed abstract"). Verify the word count is under SIR's typical 250-word abstract cap. Adjust language if Caroline pushed back on specific claims.

### 2. Introduction (paragraphs 10–11 of the current draft)

Reframe to lead with SDG 8.7 monitoring among hidden vulnerable populations as the problem statement. The current introduction frames the problem through supply-chain ethics and corporate accountability — that framing must be replaced wholesale with the quality-of-life-among-hidden-populations frame.

### 3. Conceptualization section (paragraphs 14, 18)

Insert the new paragraph drafted in the positioning memo (section 3.4, "New paragraph: engagement with the SIR-relevant measurement literature"). This paragraph cites Mazziotta-Pareto, Diamantopoulos and Siguaw, Maggino, and Bartram et al. 2024 — references that are essential for SIR engagement and that are currently absent from the manuscript's bibliography.

### 4. Contribution paragraph (paragraphs 98–101)

Replace the current four-item OSCM-flavoured list with the three-contribution methodological framing drafted in the positioning memo (section 3.2). Drop the wording "contribution to OSCM literature" entirely. Drop the "CE: Do we need to re word this for JBE?" comment if it is still in the document.

### 5. Methods section — add OSF link reference

Add a short paragraph (or a footnote) in the methods section noting that the analysis code is available at an anonymised OSF link. Track 1 will set this up and document the URL in `R/analysis/_docs/osf_setup.md`. Pull the URL from there.

### 6. Results section — update numbers from corrected analysis

For every numeric value in the manuscript that was derived from the Bayesian MA estimator (Findings 4, 5, 8 in the audit), update it with the corrected post-Task 1 value. The values likely to change:

- Main paper Table 2 ("Any Exploitation 95.0% (62.2–100.0%)" and related rows) — will change after Task 1's iteration fix.
- Composite risk estimates wherever cited in the main text.
- The seed-selection sensitivity claim in section 4.4 (if any) — will need to be REWRITTEN, because Track 1 will reveal whether seed-selection actually matters once the algorithm converges. If it matters, report the new findings. If it does not (after proper convergence), say so explicitly and explain why this is itself a robustness finding.

### 7. Appendix (ESM_4) corrections

- Fix the "64 parameter combinations / 2 NSUM methods" factual error in paragraph 54 (audit Finding 2).
- Update Tables 2 and 3 with the post-Task 1 sensitivity results.
- If Track 1 generated new sensitivity analyses (continuous τ sweep, per-indicator forest plot, etc.), incorporate those tables/figures into ESM_4 with appropriate text framing.

### 8. Discussion / Conclusion reconciliation

After all numerical updates and the positioning shift, re-read the discussion and conclusion sections end-to-end. Any sentence that was true under the old positioning or the old (broken) numbers but is no longer true must be revised. Pay particular attention to:

- Claims about the 34%–84% range — replace with the per-indicator framing.
- Claims about seed-selection robustness — replace with whatever Track 1 actually showed.
- Policy implications language — this was originally written for OSCM journals (supply-chain due diligence, corporate transparency). For SIR, reframe in terms of social-indicator monitoring for SDG 8.7, not supply-chain governance.

### 9. References

- Add the SIR-relevant composite-indicator literature cited in the new conceptualization paragraph (Mazziotta-Pareto 2013, 2016; Diamantopoulos & Siguaw 2006; Diamantopoulos et al. 2008; Maggino 2017; Bartram et al. 2024).
- Add the SIR template paper if appropriate: Muñoz, Pavía & Álvarez-Verdejo (2026), "A Practical Guide to Proper Estimation and Inference of the Gini Index", Social Indicators Research, doi:10.1007/s11205-026-03831-x.
- Remove any references that were specific to OSCM / business-ethics / IR journals and are no longer cited after the positioning shift.

### 10. Word count audit

SIR's stated cap is 10,000 words including references. The current draft is ~7,858 words. After the SIR-specific additions (the new conceptualization paragraph plus expanded composite-indicator engagement), expect ~8,300–8,500 words. Verify after editing.

## Deliverables (what the final state looks like)

1. `paper/OneDrive_1_22-05-2026/SIR/Main file submission - SIR ready.docx` — the clean, edit-applied version, ready to submit.
2. `paper/OneDrive_1_22-05-2026/SIR/Main file submission - SIR ready (REDLINE).docx` — the colour-coded redline showing changes from `Complete manuscript - assembled 2026-05-24.docx`.
3. Updated `ESM_4.docx` (in the same folder) with the corrected text and updated tables/figures.
4. A short summary note `paper/OneDrive_1_22-05-2026/SIR/Track 2 changelog - <date>.md` documenting what was changed and why.
5. The other ESM files (`ESM_1.docx` codebook, `ESM_2.docx` survey questions, `ESM_3.docx` bootstrap methodology) likely need no Track 2 changes — verify and document.

## Hand-off back to Scott

When Track 2 is complete:

- Tell Scott concisely what changed and where. Reference the changelog file rather than re-describing every edit in chat.
- Flag any places where the prose has a numerical claim that depends on Track 1's still-pending work (e.g. if Track 1 didn't complete the continuous-τ sensitivity, the discussion text mentioning it should be marked TODO).
- Offer Scott the chance to send Caroline a final pre-submission review request before submission.

## Open dependencies

- Track 1 must complete (or partial state must be documented).
- Caroline's reply to the 24 May email is informative but not blocking — if she hasn't replied by the time Track 1 is done, proceed with the positioning as-is and note this in the changelog.
- Scott may want to change the venue choice after seeing Track 1 results. If Track 1 reveals that the corrected analysis is much weaker than the manuscript currently claims (e.g. credible intervals so wide that no defensible prevalence estimate is possible), the SIR pitch may not survive. Flag this to Scott rather than papering over it.
