# Email to Caroline Emberson — SIR submission strategy

**Sent:** 24 May 2026 (from Scott Moser to Caroline Emberson, caroline.emberson@nottingham.ac.uk)
**Subject:** DWeMSinUK — repositioning for SIR, and an invite to the GitHub repo

---

Dear Caroline,

I want to bring you up to date on where I've landed after sitting with the four rejection letters (IJOPM, JBE, IRJ, BJIR) and thinking about what to do next. The short version is that I think Social Indicators Research is the right next venue, and that the work required to land it there is more about repositioning than about rewriting. I've done some preparatory work this week that I'd like your eyes on before we go any further.

**Where I think we landed wrong, four times in a row.** Reading the four rejection letters side by side, the substance of the work was never really the problem. The problem was that the paper kept being submitted into conversations it wasn't quite having. IJOPM, JBE, IRJ and BJIR are all venues with a clear disciplinary centre of gravity (OSCM ethics, business ethics, industrial relations, industrial relations again), and the paper as currently written tries to enter each of those conversations through a contribution paragraph that was originally written for IJOPM and then carried forward largely unchanged. The note in the current draft — "CE: Do we need to re word this for JBE?" — is the smoking gun. We never did, and four desk-rejections later, that's why.

**Why I think SIR is the right next move.** Social Indicators Research is a quality-of-life journal that cares about measurement of social phenomena, composite indicators, and — increasingly — SDG-aligned monitoring. Roughly half the papers SIR published in 2025 touched the SDG agenda in some way. Modern slavery and forced labour sit directly under SDG Target 8.7. The journal has not published an RDS or NSUM paper in any of the most recent 50 articles, and has never published a modern-slavery prevalence paper as far as I can tell, but it has published a steady stream of methodology-practical-guide papers applied to hard-to-measure quality-of-life-relevant phenomena — the closest published-paper template for ours is a March 2026 piece by Muñoz, Pavía and Álvarez-Verdejo titled *"A Practical Guide to Proper Estimation and Inference of the Gini Index"*. That is exactly the genre we should be imitating.

**What the repositioning looks like in practice.** The empirical analysis, the methodological exposition, and the discussion of the RDS/NSUM divergence all survive intact. What changes is the framing language at the top of the paper and one mid-paper bridging paragraph. Concretely:

1. The abstract and introduction lead with SDG 8.7 monitoring among hidden vulnerable populations as the problem, the methodological combination of RDS + NSUM as the contribution, and the UK domestic-worker sector as the application that demonstrates the methodology works on the hardest realistic case in a developed-economy setting.

2. The contribution paragraph drops the four-item OSCM-flavoured list and replaces it with three methodological contributions: the dual ego/alter instrument that produces directly comparable RDS and NSUM estimates of five ILO-aligned indicators; the three-step bootstrap inference procedure for NSUM with multiple indicators; and the diagnostic reading of the RDS-NSUM divergence as evidence about visibility of different exploitation behaviours within workers' personal networks.

3. The conceptualization section gains a short paragraph engaging the composite-indicator literature SIR cares about (Mazziotta-Pareto, Diamantopoulos on formative-vs-reflective measurement, Maggino, and the journal's own 2024 author-guidance editorial from Bartram et al.) — locating our risk index in that tradition.

4. The UK domestic-worker prevalence claim moves from "contribution" to "demonstration", which is more honest about the n=85 sample limitation and inoculates us against the small-sample objection that you and I have both been quietly worried about.

**What I've done so far this week.** Two deliverables, both in the SIR folder of the GitHub repo:

- `SIR positioning analysis - 2026-05-24.docx` — the strategy memo, with drafted replacement text for the abstract, the contribution paragraph, and the new conceptualization paragraph. Either to be accepted, edited, or rejected by you before I touch the manuscript itself.
- `Complete manuscript - assembled 2026-05-24.docx` — a single .docx containing the current manuscript with all five figures and all four tables embedded inline at their cross-reference points. The three broken `[Insert about here]` placeholders in the previous draft (Figure 1 at the end of section 3.3, Figure 2 in section 4.2, Figure 4 between sections 4.4 and 4.5) are repaired. One thing to verify: in the source files Figure 4 and Figure 5 have near-identical captions, and I couldn't tell visually which image goes in which slot — please check when you open it.

**What still needs to be done before submission.** Four pieces, in this order:

1. Audit the appendix numbers against the R pipeline end-to-end — I share your doubts about the veracity of some of the figures in the current appendix and want to verify each one replicates from the underlying scripts before we commit them to a fifth submission.
2. Strengthen the sensitivity analysis. The IJOPM reviewer's "wide 34-84% range" objection was the only piece of substantive peer-review feedback we got across all four submissions and points at something fixable. I'd like to treat the visibility-adjustment factor τ as continuous rather than binary, add a per-indicator CI display so the wide range becomes legible as cross-indicator heterogeneity rather than statistical noise, and surface the Bayesian SS-PSE intervals prominently so the small-sample uncertainty is properly quantified.
3. Insert the revised positioning text into the assembled manuscript and reconcile the resulting changes through the discussion and conclusion sections.
4. Set up an anonymised OSF link to our analysis code — SIR now requires this. The code already lives on GitHub; this is a 1–2 hour job to mirror it onto OSF as an anonymous view-only project and drop the link as a footnote in the methods section.

**The GitHub repo.** Everything — code, data (in `data/`), all drafts including the four prior submissions and rejection letters, all correspondence, and the two new deliverables this week — is at:

> **https://github.com/smoser11/DWeMSinUK**

The repo is currently private. I'd like to add you as a collaborator so you have full read/write access and can pull/push directly, leave comments on commits, and so on. If you can send me the email address that's tied to your GitHub account (or set one up — it takes about a minute), I'll send the invite over today. If you'd rather not use GitHub directly, I'm equally happy to keep mirroring everything to the OneDrive folder we've been using.

**What I'd like from you.** Ideally, your read of the SIR positioning memo and the assembled manuscript over the next week or so, and a yes/no/edit on the proposed contribution paragraph and the proposed new conceptualization paragraph. Once we agree on the framing, I'll do the appendix audit and the sensitivity analysis in parallel, then bring everything together into a clean SIR-ready manuscript for your final pass before submission.

Happy to jump on a call this week or next if it would be easier to talk through any of this rather than email back and forth.

Best,
Scott

---

*Sent verbatim from this draft on 24 May 2026. The candid uncertainties I worked through with Claude in the same conversation (about the OSCM-language diagnosis being inference rather than proof, the "SIR-as-right-venue" claim being a judgment call, and the methodological contributions needing pressure-testing against prior HIV-/sex-work-prevalence literature) are deliberately not in the sent email — Scott chose to let Caroline surface her own objections in her reply rather than preempt them with a unilateral list of caveats. See the SIR positioning analysis memo for the substantive analysis these caveats apply to.*
