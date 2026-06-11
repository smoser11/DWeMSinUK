# Memory

> Productivity working memory for the DWeMSinUK project.
> Note: this is the productivity-system working memory, separate from `CLAUDE.md` (which is the repo's project documentation for Claude Code sessions). Subsequent productivity skill calls should read this file.
> Created 25 May 2026 via `/productivity:start`.

## Me

Scott Moser. Associate Professor, School of Politics and International Relations, Faculty of Social Science, University of Nottingham (UoN). Email scott.moser@nottingham.ac.uk (institutional) / scott.moser@gmail.com (personal). Office C18, Law and Social Science (LaSS), University Park, Nottingham NG7 2RD. Phone +44 (0)115 951 4998.

## People

| Who | Role |
|-----|------|
| **CE** / **Caroline** | Caroline Emberson. Assistant Professor in Operations Management, Nottingham University Business School OMIS. Long-standing co-author on DWeMSinUK. Working days Mon/Tue/Thu/Fri. caroline.emberson@nottingham.ac.uk. Office B12, Si Yuan Building, Jubilee Campus. |
| **Selim** | Former DWeMSinUK collaborator who worked on the Risk Index (~2023–2024). Out of touch as of May 2026; both Scott and Caroline have lost contact. |

## Projects

| Name | What |
|------|------|
| **DWeMSinUK** | "Domestic Worker Exploitation and Modern Slavery in the UK." Research project measuring prevalence of labour exploitation among UK domestic workers using RDS + NSUM. n=85 (RDS sample after dropping zero-degree nodes). Currently being prepared for submission to **SIR** after four prior desk-rejections (IJOPM, JBE, IRJ, BJIR). Repo: https://github.com/smoser11/DWeMSinUK (private). |

## Terms / Acronyms

| Term | Meaning |
|------|---------|
| **SIR** | Social Indicators Research (Springer). Current target journal. Editor-in-chief David Bartram (Leicester). 10,000-word cap including references; requires anonymised OSF code share. |
| **IJOPM** | International Journal of Operations and Production Management (Emerald). Desk-rejected. |
| **JBE** | Journal of Business Ethics (Springer). Desk-rejected. |
| **IRJ** | Industrial Relations Journal (Wiley). Desk-rejected. |
| **BJIR** | British Journal of Industrial Relations (Wiley). Desk-rejected. |
| **BJM** | British Journal of Management (Wiley). Earlier candidate venue. |
| **RDS** | Respondent-Driven Sampling. Method for sampling hidden populations via chain-referral. Estimators: RDS-I (Salganik-Heckathorn), RDS-II (Volz-Heckathorn), RDS-SS (Gile's Successive Sampling). |
| **NSUM** | Network Scale-Up Method. Estimates hidden-population prevalence from general-population reports about social contacts. |
| **MBSU** | Modified Basic Scale-Up estimator. The NSUM variant used in DWeMSinUK. With δ and τ adjustment factors. |
| **GNSUM** | Generalized NSUM (Feehan & Salganik 2016). |
| **MA** | Model-Assisted (Gile & Handcock 2015). Bayesian RDS estimator. |
| **SS-PSE** | Successive Sampling Population Size Estimation. Bayesian method by Handcock, Gile, Mar (2014). |
| **NRM** | UK National Referral Mechanism for modern slavery / human trafficking. Baseline of 44,360 adult referrals used in the paper. |
| **ILO** | International Labour Organization. "Indicators of Forced Labour" framework. |
| **SDG 8.7** | Sustainable Development Goal target 8.7: end forced labour, modern slavery, human trafficking by 2030. The framing hook for SIR. |
| **ESM** | Electronic Supplementary Material (Springer convention). ESM_1 codebook, ESM_2 survey questions, ESM_3 bootstrap methodology, ESM_4 statistical appendix. |
| **UoN** | University of Nottingham. |
| **OSCM** | Operations and Supply-Chain Management. The disciplinary framing of previous submissions; being dropped for SIR. |
| **PHS** | Personal and Household Service work. Industrial category that includes domestic work. |
| **OSF** | Open Science Framework. Repository service for anonymised reviewer-facing code shares. |
| **τ** (tau) | NSUM visibility / true-positive-rate adjustment factor. |
| **δ** (delta) | NSUM degree-ratio / network-size-differences adjustment factor. |

## Preferences

- Prefers short, on-point replies. No long preambles. No self-analysis. No over-formatting.
- Prefers I just act on concrete work rather than produce planning/meta documents.
- Conversational tone (no formal headers in chat unless content really requires it).
- Uses British English in prose.
- Prefers `here::here()` for paths in R (never `setwd()`).
- Prefers clean save + colour-coded redline for manuscript edits (not Word Track Changes).
- Cited as "Scott" in email signatures (not "Scott Moser, PhD" or similar formal forms).

## Tools & Sources

- **Repo:** `~/Documents/GitHub/DWeMSinUK/` (this folder). GitHub at `smoser11/DWeMSinUK`.
- **R:** runs on Scott's machine (R 4.2+; `netclust` needs compat patch for R 4.5+).
- **Manuscript files:** `paper/OneDrive_1_22-05-2026/SIR/`. Caroline's OneDrive shared folder is the canonical source.
- **Correspondence:** `correspondence/YYYY/` with `YYYY-MM-DD <subject>` naming convention.
- **Audit report:** `paper/OneDrive_1_22-05-2026/SIR/Statistical audit report - 2026-05-25.docx`.
- **Track instructions:** `instructions/Track 1 - analysis (for Claude Code).md` and `instructions/Track 2 - manuscript (for Cowork Claude).md`.

## Recent context (May 2026)

- Caroline shared the SIR folder on 8 May 2026; Scott took two weeks to engage (was busy with marking and a grant application).
- Scott emailed Caroline a strategy proposal on 24 May 2026 (drafted with Cowork Claude). Awaiting reply.
- Audit (25 May 2026) found the MA estimator is being called with too few iterations to converge; the Bayesian "sensitivity" results in ESM_4 are statistically invalid. Fix is on Scott's task list (Track 1).
- All 8 manuscript figures are orphaned from the cached pipeline outputs — they don't hash-match anything in `output/figures/`. Track 1 must regenerate.
- UoN Business School is going through "pretty grim" times per Caroline; broader UoN context is stressful for both.

