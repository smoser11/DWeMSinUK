# DWeMSinUK

## Quick reference
- **Full name:** Domestic Worker Exploitation and Modern Slavery in the UK
- **Authors:** Scott Moser (Notts Politics & IR) + Caroline Emberson (Notts Business School); Selim contributed earlier
- **Repo:** https://github.com/smoser11/DWeMSinUK (private)
- **Current target venue:** Social Indicators Research (SIR, Springer)
- **Current state:** Manuscript draft + supplementary materials in `paper/OneDrive_1_22-05-2026/SIR/`; statistical audit revealed issues that must be fixed before submission.

## What it is

Methodological research project: combines Respondent-Driven Sampling (RDS) with the Network Scale-Up Method (NSUM) to estimate prevalence of labour exploitation among UK domestic workers — a hidden vulnerable population invisible to conventional household surveys. Five ILO-aligned indicators of forced labour are measured both ego-centrically (RDS) and alter-centrically (NSUM) via a matched-instrument survey.

- **Sample:** n=85 RDS respondents (97 surveyed; 12 dropped for zero-degree networks). 90 of 97 are transnational migrants; ~66% Filipina; 97% female.
- **Population assumed:** ~980,000 UK domestic workers (EU baseline).
- **NRM referrals baseline:** 44,360 adult/year.
- **Survey conducted:** February–July 2023 via JISC online survey (mobile-friendly).

## Submission history (four prior desk-rejections, all 2025–2026)

| Journal | Field | Date | Outcome |
|---------|-------|------|---------|
| IJOPM (Emerald) | OSCM | 2025 | Desk-rejected. Reviewer flagged the wide 34%–84% range. |
| JBE (Springer) | Business ethics | 2025 | Desk-rejected. |
| IRJ (Wiley) | Industrial relations | 2025 | Desk-rejected. |
| BJIR (Wiley) | Industrial relations | 2026 | Desk-rejected. |

The common diagnosis (acknowledged as inference, not proof): the OSCM-flavoured contribution paragraph was carried unchanged across submissions despite the journals having different disciplinary centres of gravity. See `paper/decisions/` for rejection letters.

## Current submission package (SIR)

Files in `paper/OneDrive_1_22-05-2026/SIR/`:

- `Main file submission 20-01-2026.docx` — the working draft when Caroline shared the folder on 8 May 2026.
- `ESM_1.docx` — codebook (variable definitions + 20 question screenshots).
- `ESM_2.docx` — survey questions.
- `ESM_3.docx` — methodological exposition of the three-step bootstrap.
- `ESM_4.docx` — statistical appendix (7 tables, 3 figures).
- `Figure 1.docx` through `Figure 5.docx` — main paper figures.
- `Table1.docx` through `Table4.docx` — main paper tables.
- `Complete manuscript - assembled 2026-05-24.docx` — Cowork Claude assembled single-file version with figures/tables inline.
- `Main file - SIR edits applied - 2026-05-25.docx` — clean version with surgical SIR positioning edits.
- `Main file - SIR edits applied - 2026-05-25 (REDLINE).docx` — colour-coded diff against the assembled version.
- `ESM_4 - SIR edits applied - 2026-05-25.docx` — ESM_4 with the "64/2 NSUM methods" factual error fixed.
- `SIR positioning analysis - 2026-05-24.docx` — strategy memo.
- `Statistical audit report - 2026-05-25.docx` — 8 audit findings.
- `Editorial memo - Pre-SIR submission diagnostic.docx` — earlier pre-submission diagnosis.

## Audit findings (25 May 2026, see Statistical audit report)

| # | Finding | Severity |
|---|---------|----------|
| 1 | NSUM sensitivity tables (ESM_4 4–7) match sources | Clean |
| 2 | "64 / 2 NSUM methods" claim wrong (should be 48 / 4) | Factual error (fixed in revised ESM_4) |
| 3 | All 8 manuscript figures orphaned from pipeline | Critical for SIR (code-share requirement) |
| 4 | Seed-selection sensitivity gives byte-identical results | Bug (root cause: Finding 5) |
| 5 | MA estimator iterations too few (2/3 iter, M2=25/50) | Bug (root cause) |
| 6 | ESM Table 1 CIs use different source (not a discrepancy) | Resolved |
| 7 | Excessive Working Hours upper CI < point estimate | Numerical bug |
| 8 | Composite risk: 37% (freq) vs 7% (broken Bayesian) | Consequence of Finding 5 |

## Two-track plan for fixing & submitting

- **Track 1** (analysis re-check on Scott's machine; instructions in `instructions/Track 1 - analysis (for Claude Code).md`): fix MA iterations, regenerate figures, address reviewer concerns, make pipeline reproducible end-to-end, set up OSF mirror.
- **Track 2** (manuscript revision via Cowork Claude after Track 1; instructions in `instructions/Track 2 - manuscript (for Cowork Claude).md`): incorporate corrected analysis, finalise SIR-specific framing, produce submission package.

## Key dependencies / open items

- Waiting on Caroline's reply to the 24 May 2026 strategy email.
- 5 commits sitting locally waiting for `git push`.
- Possibly: backup venue selection if SIR also rejects.
