# Draft email to Caroline Emberson — v6a ILO severity ordering fix

**To:** Caroline Emberson <caroline.emberson@nottingham.ac.uk>
**From:** Scott Moser <scott.moser@gmail.com>
**Subject:** Re: DWeMSinUK — v6a with the ILO medium/strong fix, redline attached

---

Dear Caroline,

You were right to push back on the ILO medium/strong claim, and I have a v6a on the shared folder that fixes it, plus a redline against v6 so you can see every change inline.

The short version of what was wrong. In v6 the medium/strong severity distinction was attributed to the two shorter ILO operational-indicators publications — the 2012 pamphlet and the 2025 revised edition — but neither publication actually partitions the eleven indicators that way. Both list the eleven and stop there, which is what you saw when you checked. The medium/strong vocabulary is real ILO language, but it lives in the *Handbook on Forced Labour Surveys* (ILO 2024, third edition of *Hard to See, Harder to Count*), which is a distinct publication and was not being cited. So on your comment ("this may be a hallucination"), the vocabulary itself is not fabricated, but our citations to it were wrong — the honest fix is to cite the actual source rather than to defend an attribution to publications that don't contain the distinction.

What I changed in v6a. Every place the medium/strong ordering is invoked now cites `hard24-ilo` (the 2024 handbook), either alongside or instead of the shorter operational-indicators cluster. Every place the eleven-indicator framework is invoked (without the severity claim) now cites `[@ILO11-indicators; @int25-ilo]` — the 2012 pamphlet and the 2025 revised edition, both of which you asked me to add via the comments on v5. I have also dropped `international_labour_organization_global_2017` from the indicator-framework clusters throughout, because that citation is to the ILO's *Global Estimates of Modern Slavery* prevalence report and does not discuss the eleven-indicator framework at all — it was doing no evidentiary work in those cite clusters.

While auditing that side I also caught a related error in the composite-index construction that we should own separately. The appendix prose and the Section 4.2 anchoring sentence both claimed that strong-indicator categories in the composite receive larger sub-weights than medium-indicator categories. This is not what Table A1 actually implements. Every one of the eleven ILO categories in Table A1 carries the same maximum weight of 0.09; the sub-weights within a category differ only mechanically by how many sub-questions code that category (0.09 for one, 0.045 for two, 0.03 for three, 0.0225 for four). The medium/strong labels play no role in the arithmetic. So the composite is uniform-weighted at the category level. I have rewritten the appendix prose and the Section 4.2 sentence to accurately describe this, and noted that the medium/strong labels are retained for reporting and interpretation but do not scale the weights. The numerical results in the paper are unchanged — this is a description correction, not a re-computation. The weight-perturbation robustness result (Spearman ρ = 0.992) stands and now sits more naturally, since it is what actually licences the composite ordering claim.

Two things I would like your view on before we finalise. First, the specific bundling of ILO indicators into our five binary measures — combining intimidation/threats with physical and sexual violence into a single "threats and abuse" indicator, for example, and combining restriction of movement with isolation into "limited access to help" — is now flagged in v6a as our analytical choice, informed by the qualitative literature on UK migrant domestic workers (Kalayaan 2008, Mantouvalou 2016, LAWRS 2023, and your own Yilmaz & Emberson 2023). I think that is honest, and the co-occurrence claim for threats/violence is what the Yilmaz & Emberson report documents; if you would rather anchor the bundling on a different combination of sources, please tell me and I will swap. Second, on your Table 1: I have added a sentence to the caption noting explicitly that the severity column follows the ILO handbook and that the shorter operational-indicators publications do not publish this severity ranking. If you would prefer the column heading itself to change — from "Severity" to something like "Severity (ILO handbook, hard24-ilo)" or "Handbook severity" — I can do that too; I have left the heading unchanged for now because the caption footnote does the attribution work.

The rest of the manuscript is unchanged. In particular the discussion-section framing of the RDS–NSUM visibility gradient, Table 2's headline preferred-estimator display, and the SDG 8.7 policy section are all identical to v6. The v6a fix is narrowly targeted on the citation and description errors you flagged; it does not reopen any of the wider revision decisions we settled on for v6.

Files on the shared folder:

> **`paper/SIR/manuscript-v6a.qmd`** — clean v6a source
> **`paper/SIR/appendix-v6a.qmd`** — appendix source with the corrected composite prose
> **`paper/SIR/manuscript-v6-vs-v6a-redline.docx`** — Word document with Track Changes showing every difference between v6 and v6a, one paragraph at a time; open in Word with Track Changes visible and you can accept or reject each edit individually

Happy to jump on a call this week if it is easier to work through any of this together, otherwise your read on the two flagged items above (bundling attribution, and Table 1 heading) whenever you have a window.

Best,
Scott

---

*Drafted 5 July 2026 in response to CE's comments on v5 (comments #2, #3, #5, #6, #7, #8, #14, #42 in the Word doc). The email is deliberately explicit about what was wrong in v6 — attribution error, not fabrication — because CE's comment #6 raised the possibility of AI hallucination and the honest response is to describe the specific citation error rather than deflect. Scott, please review the "your own Yilmaz & Emberson 2023" phrasing before sending — I have described it as CE's report but she may prefer different wording.*
