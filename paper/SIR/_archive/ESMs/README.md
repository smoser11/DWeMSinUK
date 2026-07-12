# Archived Electronic Supplementary Material (ESM) files

The four ESM files retired here (ESM_1.docx/pdf through ESM_4.docx/pdf) are
superseded by the Appendix Book at `paper/SIR/appendix-v7/`. Their content
has been fully subsumed into the four appendix sections. This directory is
kept for revision history only — nothing in the current manuscript workflow
references these files.

## Mapping: ESM → Appendix Section

| Archived file | Old title | Now lives at |
|---|---|---|
| `ESM_1.docx` / `.pdf` | Codebook and Composite Risk Index Construction | **Appendix Section A** — Variable Codebook and Indicator Definitions (A.1 Composite Risk Index, A.2 Five Binary Exploitation Indicators, A.3 Key RDS Variables) |
| `ESM_2.docx` / `.pdf` | Survey Instrument (5 ego/alter question pairs + coding rules) | **Appendix Section B** — Survey Questions and Comparable Ego/Alter Indicators (B.1 Document Withholding through B.5 Limited Access to Help, plus B.6 General Data-Processing Notes) |
| `ESM_3.docx` / `.pdf` | Three-Step NSUM Bootstrap Procedure | **Appendix Section C** — Three-Step Bootstrap Procedure |
| `ESM_4.docx` / `.pdf` | Sensitivity Analyses (RDS-I/SS, MA-RDS, NSUM tiers, τ-sweep, weight/method/popsize robustness, LOO chain sensitivity, survey incentive design) | **Appendix Section D** — Sensitivity Analyses (D.1 Population Size and Invariance through D.8 Leave-One-Out Chain Sensitivity) |

## What confirms subsumption

- **Manuscript-v7 has zero references to ESM files.** All in-text references
  now target the appendix (`Appendix Table A.1`, `Appendix Figure D.4`,
  `Appendix Section D.8`, etc.). Grep confirms only older drafts
  (manuscript-v4g and earlier) mentioned ESM_N.
- **Every ESM_4 table and figure has a matching table/figure in Section D**
  of the appendix. Section D also expanded D.8 (LOO chain sensitivity) with
  two new tables (D.8 excluded-chain roster, D.9 full-vs-LOO comparison) not
  present in the original ESM_4.

## OSF still points at ESMs

`OSF/README.md` and `OSF/supporting/ESM_*.docx` are not affected by this
archive move. If the OSF replication package is regenerated for the current
submission, update the OSF supporting/ directory to hold the current
appendix (either the four sections rendered as .docx files, or the whole
appendix as a single supplementary PDF) and update `OSF/README.md`
accordingly.

## Restoration

If any archived ESM needs to be restored, move the file back to
`paper/SIR/` — nothing in the appendix source depends on this directory.
