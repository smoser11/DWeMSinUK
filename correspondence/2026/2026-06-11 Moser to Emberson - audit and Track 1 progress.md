# Draft email to Caroline Emberson — audit findings and next steps

**Subject:** Re: DWeMSinUK — audit findings and next steps

---

Dear Caroline,

A quick update — and the headline is that it's a very good thing I double-checked the analysis before we touched the manuscript, because there are loads of issues I'm now glad to have a chance to fix rather than have a reviewer find them.

The audit surfaced two material problems and a handful of smaller ones. First, the Bayesian model-assisted estimates that feed main paper Table 2 and the corresponding ESM_4 tables were being computed with too few iterations of the Gile-Handcock algorithm for the chain to converge — the reported values were essentially the unadjusted sample mean dressed up as Bayesian estimates. That is why the "seed-selection sensitivity" came out byte-identical across the three methods and why some standard errors looked implausibly tight. Fix already committed; I'll re-run on my machine, with the 96-combination sensitivity going overnight. Second, all eight figures in the manuscript don't trace to the canonical pipeline — they were generated outside the tracked code, and we can't currently reproduce them from `Rscript R/00-main_pipeline.R`. Given SIR's anonymised OSF code-share requirement, that has to be fixed before submission. The smaller issues are an "ESM_4 says 64 combinations when it should say 48" documentation error (already corrected) and a numerical bug in one CI on Excessive Working Hours that I'll trace during the re-run.

None of this changes the broad shape of what we're claiming — the frequentist RDS and NSUM estimates are solid, the surgical SIR-positioning edits are already applied to a working copy of the manuscript, and the plan from here is straightforward: re-run the corrected Bayesian analysis, regenerate the figures, address the IJOPM "wide range" concern with a proper per-indicator CI display, set up the OSF mirror, and then push everything into the manuscript. A full audit report sits in the SIR folder of the repo (`Statistical audit report - 2026-05-25.docx`) if you want the detail.

One thing I want to repeat from my last note: I'd like to add you as a collaborator on the GitHub repo so you can see all of this directly (and pull/push if you want to). If you can send me the email tied to your GitHub account — or set one up; it takes about a minute — I'll send the invite over. Repo is at https://github.com/smoser11/DWeMSinUK.

But really, the main thing: it is a very good thing I caught this before we sent it anywhere, and I think we end up with a stronger paper for the effort.

Best,
Scott

---

*Drafted 11 June 2026 — Scott, please review/edit before sending. Reissues the GitHub-collaborator invite from the 24 May email since she hasn't yet sent her GitHub-linked address.*
