# SIR submission bundle

This folder contains the files to upload to Springer's Social Indicators Research submission portal.

## Journal requirements (SIR double-blind, per Springer's Submission Guidelines, July 2026)

- Double-blind review — author identity redacted from manuscript and supplementary materials.
- Manuscript length: 5,000–10,000 words including references.
- Manuscript file format: `.docx` (or LaTeX). We render from Quarto (`.qmd` → `.docx`/`.pdf`).
- Supplementary Information (SI) preferred format: PDF.
- Each SI file must include the article title, journal name, and author info independently — see [Springer's guidelines](https://link.springer.com/journal/11205/submission-guidelines) for the exact SI header format (this can be added by the journal system at upload time, or included at the top of the SI PDF).
- Figures in the SI are numbered separately from the main text (Table A.1, D.4 etc. — already handled by the Quarto book).
- Cover letter and Title Page uploaded separately.

## Files (self-contained bundle)

This folder contains **everything needed to render** the manuscript, appendix,
and title page — no external file references. You can zip this folder and it
will render on any machine with Quarto + LaTeX + Word/Pandoc installed.

```
submission/
├── README.md                (this file)
├── manuscript.qmd           anonymised main manuscript source
├── title-page.md            separate title page source (real author info)
├── figures/                 all 9 PNG figures used by manuscript + appendix
├── references/              apa.csl + ce-library.bib + sm-cited.bib
└── appendix/                anonymised Online Appendix (Quarto Book)
    ├── _quarto.yml          book config (bibs at '../references/')
    ├── index.qmd            preface
    ├── A-codebook.qmd       Appendix A
    ├── B-survey.qmd         Appendix B
    ├── C-bootstrap.qmd      Appendix C
    └── D-sensitivity.qmd    Appendix D (D.1–D.8)
```

| File | Purpose |
|---|---|
| `manuscript.qmd` → render to `manuscript.docx` and `manuscript.pdf` | **Anonymised main manuscript** — authors, affiliations, funding, OSF link, and university references all redacted. Submit the rendered `.docx` (SIR primary) plus the `.pdf` as needed. |
| `appendix/` (Quarto Book) → render to `_output/Appendix--Quantifying-Hidden-Exploitation.pdf` | **Anonymised Online Appendix** — Sections A/B/C/D as a single supplementary PDF. Submit as SI/ESM. |
| `title-page.md` → render to `title-page.docx` and `title-page.pdf` | **Separate Title Page** — full author info, affiliations, funding, ORCID, competing interests, acknowledgements, ethics, and AI-tool disclosure. Upload to SIR's separate Title-Page slot. |

## Rendering

From this folder (`paper/SIR/submission/`):

```bash
# 1. Anonymised main manuscript (primary submission — DOCX + PDF)
quarto render manuscript.qmd --to docx --output manuscript.docx
quarto render manuscript.qmd --to pdf  --output manuscript.pdf

# 2. Anonymised Online Appendix (Quarto Book — submit the combined PDF as SI/ESM)
cd appendix
quarto render
cd ..
# → produces appendix/_output/*.pdf, .html, .docx
# → the PDF is the one for SI/ESM upload

# 3. Title Page (separate SIR upload)
quarto render title-page.md --to docx --output title-page.docx
quarto render title-page.md --to pdf  --output title-page.pdf
```

All paths are relative and internal to `submission/` — no `../references/` or
`../figures/` references escape the bundle.

## What was redacted from the manuscript for anonymisation

Mechanical redactions applied by `anonymise` script:

1. **Author YAML block** — replaced with `[Author(s) anonymised for review] / [Affiliation anonymised for review]`.
2. **`University of Nottingham`** — replaced with `[Institution anonymised for review]` in body text and appendix.
3. **Author names** where they appeared in body text.
4. **OSF view-only URL** — replaced with placeholder; the actual URL is on the Title Page and in the data-availability statement there.
5. **Funding statement** in Declarations — redacted, real statement moved to Title Page.

## What was NOT redacted — please review before uploading

1. **Partner organisation names** — Kalayaan, Voice of Domestic Workers, and Latin American Women's Rights Service are mentioned throughout the manuscript and appendix (in the case-setting section, sample-composition table, validation-channels paragraph, and generalisability discussion). These are legitimate research partners and identifying them is standard practice. However, because CE's public research profile documents partnerships with these specific NGOs, mentioning them by name **could** de-anonymise the submission. Two options:
   - Leave as-is and rely on the reviewer's professional norms not to Google partner-org combinations.
   - Redact to "three London-based civil-society organisations working with UK migrant domestic workers" throughout, and reinstate names on the Title Page and Acknowledgements.
2. **Self-citation `@yilm23-exploring` (Yilmaz & Emberson 2023)** — CE's own Rights Lab report, cited in the case-setting and conceptualisation sections. This will appear in the reference list as "Yilmaz, S., & Emberson, C. (2023)…" which reveals CE's identity to any reviewer who checks. Standard anonymisation for self-citation is:
   - Convert to `[Authors' own work, citation withheld for anonymised review]` at each in-text occurrence, and remove from the reference list until acceptance.
   - Or keep the third-person cite and remove Emberson from the reference-list entry ("Yilmaz, S., & [Author]") — messy but sometimes acceptable.
3. **Ethics reference number** — not filled in on the Title Page; add before submission.
4. **ORCID identifiers** for both authors — not filled in; add before submission.

## Word-count check

Manuscript wc as of last redaction pass:

```
$ wc -w manuscript.qmd
10037 manuscript.qmd
```

This includes YAML front-matter and comments. The rendered body word count will be lower (Quarto strips YAML during render). If the rendered `.docx` still exceeds 10,000 words including references, targeted trimming will be needed — the natural candidates are the Introduction (dense) and Section 5.6 Policy Implications.

## AI/LLM disclosure requirement (Springer)

Springer requires that LLM use be documented in the Methods section (or another suitable part of the manuscript). The current `manuscript.qmd` does not yet contain such a disclosure. **Add** either a footnote in the Methods or a short paragraph in the Declarations noting that Claude was used for AI-assisted copy editing (readability, grammar, cross-reference verification). See the wording draft on the Title Page.
