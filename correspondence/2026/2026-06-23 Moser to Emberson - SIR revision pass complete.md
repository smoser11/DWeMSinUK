# Draft email to Caroline Emberson — SIR revision pass complete

**To:** Caroline Emberson <caroline.emberson@nottingham.ac.uk>
**From:** Scott Moser <scott.moser@gmail.com>
**Subject:** Re: DWeMSinUK — SIR revision pass complete, package for your review

---

Dear Caroline,

The full SIR-revision pass is done. The submission-ready manuscript sits at 9,992 words — eight under the SIR 10,000-word cap — and is in the OneDrive SIR folder along with a structured review package for you.

The revision ran in eight rounds today: abstract and contribution paragraph; introduction; conceptualisation (Section 2) with the ILO medium/strong indicator hierarchy now explicit; methods (Section 3) with a new subsection 3.6 on the invariance of prevalence to assumed N (this answers the "why is the population size different in different parts of the analysis?" question reviewers will otherwise ask); results (Section 4) restructured around a preferred-estimator framing with a new headline Table 2 putting RDS-SS and MBSU Moderate NSUM side-by-side for all five binary indicators; discussion (Section 5) reorganised into six named subsections that name prior reviewer concerns explicitly; and conclusion (Section 6) rewritten to lead with the visibility-gradient diagnostic and anchor to SDG 8.7 monitoring.

The single most consequential framing change in the whole pass is the argument — developed in Discussion 5.3 and 5.4 and again in the Conclusion — that the order-of-magnitude divergence between RDS-SS (35.4–84.2% across binary indicators) and MBSU Moderate NSUM (1.08–2.98% across the same indicators) should be read as a substantive visibility-gradient diagnostic rather than as a methodological inconsistency to be reconciled. This is the paper's distinctive contribution under the SIR framing but it is a strong claim, and it limits what we can say about absolute prevalence. The "claim-licensing" paragraph (Discussion 5.4 third paragraph) makes this explicit — it tells the reader what the design does NOT licence them to take from the paper as well as what it does. I'd particularly value your read on whether you're comfortable with that paragraph as written; it is more cautious than the previous draft but matches what the new Methods edits commit us to. I've flagged the other four "specific calls" worth a careful read in the package README.

A couple of practical notes. First, the IJOPM reviewer's "wide 34–84% range" concern is now answered by a new three-source-dispersion framing in Discussion 5.3 that distinguishes across-indicator variation (substantive — different exploitation behaviours legitimately have different population frequencies), across-method variation (substantive — the visibility-gradient diagnostic), and within-method-within-indicator sampling uncertainty (the only source that is conventional statistical noise, and the only one where sample size matters). The bootstrap CIs are now reported per-method so the n=85 uncertainty is honest at the indicator level. Second, the composite risk index now has a weight-perturbation sensitivity analysis (B = 5,000 perturbations of ±50% on each of the eleven sub-indicator weights) showing Spearman ρ = 0.992 (mean) / 0.974 (worst case) between perturbed and baseline rankings — the composite-risk ranking is essentially weight-invariant.

All four of your UK-specific policy paragraphs (visa restrictions, employer regulation, immigration firewall, training expansion) are retained verbatim in Discussion 5.6, with a new SDG 8.7 framing paragraph leading the section and a new visibility-gradient programme-design paragraph closing it. This was the closest call in the revision — SIR is not a UK policy journal — but on balance keeping the policy depth seemed right for our advocacy partners' purposes.

Three things remain to be done before we submit. The OSF code-share mirror still needs setting up (the Methods section has a placeholder waiting for the URL). A final cross-reference audit is worth one careful pass — section, table, and figure numbers have all been touched. And, most importantly, your green light on the revision.

The package for your review is in the OneDrive SIR folder at:

> **Package for CE - 2026-06-23/**

It contains a README, the submission-ready manuscript (folder 01), eight section-by-section redlines (folder 02, in editing order — read 01 through 08 to see the cumulative change), six rationale memos (folder 03 — useful if any specific edit in the redlines looks puzzling), and all four Online Resources ESM_1 through ESM_4 (folder 04 — the supplementary files the main manuscript cross-references). Everything is also in the GitHub repo at github.com/smoser11/DWeMSinUK under `paper/SIR/`. If reviewing in GitHub is easier and you can send me the email address tied to your GitHub account (or set one up if you don't have one), I'll send the collaborator invite over — that offer has been outstanding from the 24 May and 11 June notes and the invite would let you pull, comment on commits, and review redlines side-by-side in the GitHub interface if you prefer.

Happy to jump on a call this week if it would be easier to talk through any of this rather than email back and forth. Otherwise, your review of the package whenever you have a window, and we can be ready to submit shortly after.

Best,
Scott

---

*Drafted 22 June 2026; updated 23 June 2026 to point at the renamed `paper/SIR/` folder layout, the 2026-06-23 submission-ready file, and the now-complete supplementary stack (ESM_1 through ESM_4) in the package. Scott, please review/edit before sending. The candid uncertainties — that the "visibility-gradient diagnostic" framing is the paper's strongest bet but also its highest-risk claim if a reviewer reads it as undercutting our own results, and that the n=85 sample is still small even with the principled defence in 5.3 — are deliberately not surfaced in the email body. Caroline will see both in the README and can raise them herself if she wants.*
