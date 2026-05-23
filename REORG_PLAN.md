# Repository reorganisation plan

**Prepared:** 22 May 2026
**Branch:** `main` at commit `722609c` (snapshot commit, pushed to origin)
**Safety net:** `_snapshots/` contains four tarballs (454 MB) preserving the pre-reorg state on disk.

---

## 1. Headline

The repo is **3.5 GB** with **~2,500 files** across **23 top-level entries**. The R code already roughly follows the intended layout described in `CLAUDE.md`, but it has accumulated:

- Two old git clones nested inside the working tree (`DWeMSinUK_1/`, `DWeMSinUK_RL/`) — 436 MB combined.
- Three "underscore-shadow" duplicates of dot-prefixed entries (`_Rproj.user/`, `_git/`, `_gitignore`) — leftover from some past file-copy operation.
- Dead/empty folders (`documentation/`, `analysis/` at root with one stray `.docx`, `tests/` at root duplicating what should be under `R/`).
- A `bak/` folder duplicating files already in `R/archive/`.
- ~10 stale variant scripts in `R/analysis/` (`_bak.R`, `_corrected.R`, `_improved.R`, `_simple.R`, `_FIXED.R` versions) that should have been archived after the August 2025 consolidation.
- 16 loose PNG/CSV files at the repo root that belong in `output/figures/` or `output/tables/`.
- 50+ versioned paper drafts in `paper/` (drafts, intermediate LaTeX compile artifacts, Quarto `_files/` dirs) — only the SIR submission folder is currently active.
- `Meetings/` (124 MB of audio) was tracked in git *before* `.gitignore` was updated to exclude it — still in history, still in the index.

After reorg the working tree will be ~600 MB (excluding `_snapshots/` and `Meetings/`), the structure will match `CLAUDE.md`, and the analysis pipeline will still run because the R folder layout is preserved.

---

## 2. Target structure (post-reorg)

```
DWeMSinUK/
├── CLAUDE.md                   # updated to reflect new structure
├── AGENTS.md
├── README.md                   # NEW: short top-level orientation
├── REORG_PLAN.md               # this file (delete after reorg complete)
├── DWeMSinUK.Rproj
├── .gitignore
│
├── R/                          # R analysis code (mostly unchanged)
│   ├── 00-main_pipeline.R
│   ├── config.R
│   ├── data_processing/
│   ├── analysis/               # CLEANED: stale variants moved to archive
│   ├── utils/
│   └── archive/                # absorbed bak/ and the variants
│
├── data/                       # unchanged
│   ├── raw/
│   ├── processed/
│   └── survey/
│
├── output/                     # unchanged structure; large binaries untracked
│   ├── figures/                # accepts the 16 loose root PNGs
│   ├── tables/
│   ├── reports/
│   └── (older nsum_* subdirs moved under output/archive/)
│
├── paper/
│   ├── current/                # symlink or copy of the active SIR submission
│   ├── OneDrive_1_22-05-2026/  # SIR submission package + Editorial memo
│   ├── archive/                # all the old IJOPM/BJMM versioned drafts
│   ├── decisions/              # the four desk-rejection PDFs (grouped)
│   ├── references/             # CitedReferencesOnly.bib, Yilmaz paper, etc.
│   └── supporting/             # Descriptive stats, Bootstrapping Methods, etc.
│
├── correspondence/             # MERGED with the new emails/ folder; sub-foldered by year
│   ├── 2023/
│   ├── 2024/
│   ├── 2025/
│   └── 2026/                   # contains what's now in emails/
│
├── Notes/                      # unchanged
├── Literature/                 # unchanged
├── Replication/                # unchanged (RDS/NSUM reference implementations)
├── GNSUM/                      # unchanged (G-NSUM specific materials)
├── tests/                      # moved under R/tests/ (NOT root)
│
└── (gitignored, on disk only)
    ├── _snapshots/             # the safety-net tarballs from 22 May 2026
    ├── Meetings/               # 124 MB of audio (already gitignored)
    ├── CRAN/                   # vendored R packages (now gitignored)
    └── .Rproj.user/, .Rhistory, .RData
```

---

## 3. Per-folder verdicts

| Current entry | Size | Verdict | Rationale |
|---|---|---|---|
| `R/` | 1.9 MB | **KEEP** with cleanup inside (see §4) | Already matches CLAUDE.md structure; just clean the stale variants. |
| `data/` | 8.2 MB | **KEEP** | Already in `raw/`, `processed/`, `survey/`. Good. |
| `output/` | 995 MB | **KEEP structure**, **UNTRACK large binaries** | The `.RData`/`.rds` bootstrap files are regenerable from R scripts. Stay on disk but `git rm --cached` so future changes don't bloat history. |
| `paper/` | 228 MB | **REORGANISE** (see §5) | Massive cleanup needed; current chaos with 50+ versioned drafts. |
| `Notes/` | 1.2 MB | **KEEP** | Markdown research notes — valuable. |
| `Literature/` | 1.4 MB | **KEEP** | 2 PDFs of cited papers. |
| `Replication/` | 724 KB | **KEEP** | Reference implementations of the methods (Gile, Handcock). |
| `GNSUM/` | 30 MB | **KEEP** | G-NSUM-specific transcripts, notes, references. |
| `correspondence/` | 43 MB | **MERGE with `emails/`** under year sub-folders | 246 .eml+.txt files from 2023-2024; the new `emails/` (1.4 MB) is the same kind of content from 2025-2026. Combine into one folder organised by year. |
| `emails/` | 1.4 MB | **MERGE INTO `correspondence/2026/`** | See above. |
| `Meetings/` | 124 MB | **KEEP on disk, `git rm --cached`** | Already gitignored but tracked in index from before. Remove from index so it stops being tracked, but keep files on disk. |
| `CRAN/` | 14 MB | **MOVE to `.gitignored`, KEEP on disk** | R package source tarballs (Neighboot, RDS, SimRDS, netclust, sspse). Belongs in `renv` or installed-via-`devtools::install_github`, not in the repo. Untrack but keep on disk. |
| `DWeMSinUK_1/` | 146 MB | **DELETE** (preserved in `_snapshots/02-...`) | Old git clone nested in working tree. |
| `DWeMSinUK_RL/` | 290 MB | **DELETE** (preserved in `_snapshots/03-...`) | Another old git clone. |
| `bak/` | 388 KB | **DELETE** (content overlaps `R/archive/` and `_snapshots/`) | Old R scripts already in `R/archive/old_versions/`. |
| `archive/` | 18 MB | **DELETE** (it's just `DWeMSinUK_old_GitHub.zip`; preserved in `_snapshots/`) | Single zip of old GitHub state; available on the remote and in snapshots. |
| `analysis/` (root) | 24 KB | **DELETE**: move `Descriptive statistics.docx` → `paper/supporting/` | One stray .docx in an otherwise empty folder. |
| `tests/` (root) | 20 KB | **MOVE** to `R/tests/` | R test scripts belong under R/. |
| `documentation/` | 8 KB | **DELETE** | Empty (only `.DS_Store`). |
| `_Rproj.user/` | 100 KB | **DELETE** | Underscore-shadow of `.Rproj.user/`, not a real folder. |
| `_git/` | 2.7 MB | **DELETE** | Underscore-shadow of `.git/`, not a real folder. |
| `_gitignore` (file) | 58 B | **DELETE** | Underscore-shadow of `.gitignore`. |
| `_snapshots/` | 454 MB | **KEEP, gitignored** | Safety net for at least the next week. |

### Root-level loose files

| File | Size | Verdict |
|---|---|---|
| `CLAUDE.md`, `AGENTS.md`, `DWeMSinUK.Rproj`, `.gitignore`, `git.R`, `test_rds_functions.R` | small | KEEP |
| `Yilmaz and Emberson - 2023 ...pdf` | 721 KB | MOVE → `paper/references/` |
| `Rplots.pdf` | 11 KB | DELETE (regenerable; in `_snapshots/04-`) |
| `comparison_plot_*.png` (10 files) | 4.9 MB total | MOVE → `output/figures/` |
| `ci_width_plot_corrected.png`, `nsum_*_final.png` | 2.5 MB total | MOVE → `output/figures/` |
| `nsum_summary_table_final.csv` | 334 KB | MOVE → `output/tables/` |
| `.RData` (34 MB workspace dump) | 34 MB | `git rm --cached` + delete from disk (in `_snapshots/04-`) |
| `.Rhistory` | 18 KB | `git rm --cached` + delete from disk (in `_snapshots/04-`) |
| `.DS_Store` | 45 KB | DELETE everywhere (gitignored) |
| `_gitignore` | 58 B | DELETE |

---

## 4. R/ cleanup — specific moves

The following stale variants in `R/analysis/` should join the existing `R/archive/` structure:

| Current | New location | Reason |
|---|---|---|
| `R/analysis/03-rds_estimation_bak.R` | `R/archive/old_versions/` | "_bak" suffix — superseded |
| `R/analysis/05-nsum_estimation_bak.R` | `R/archive/old_versions/` | "_bak" suffix — superseded |
| `R/analysis/05-nsum_estimation_corrected.R` | `R/archive/nsum_consolidation_2025/` | Intermediate version |
| `R/analysis/05-nsum_estimation_feehan_salganik.R` | `R/archive/nsum_consolidation_2025/` | Intermediate version |
| `R/analysis/05-nsum_estimation_improved.R` | `R/archive/nsum_consolidation_2025/` | Intermediate version |
| `R/analysis/05-nsum_estimation_simple.R` | `R/archive/nsum_consolidation_2025/` | Intermediate version |
| `R/analysis/05-nsum_modified_basic_estimator.R` | `R/archive/nsum_consolidation_2025/` | Intermediate version |
| `R/analysis/08-netclust_subgroup_analysis.R` | `R/archive/old_versions/` | Superseded by `_FIXED.R` version |
| `R/analysis/08b-simple_subgroup_analysis.R` | `R/archive/old_versions/` | Superseded by `_FIXED.R` version |
| `R/analysis/08-netclust_subgroup_analysis_FIXED.R` | `R/analysis/08-netclust_subgroup_analysis.R` | RENAME: drop "_FIXED" — it's the canonical version now |
| `R/analysis/08b-simple_subgroup_analysis_FIXED.R` | `R/analysis/08b-simple_subgroup_analysis.R` | RENAME: drop "_FIXED" |
| `R/analysis/NSUM/Boot_Step1_bak.r` | `R/archive/nsum_consolidation_2025/` | "_bak" suffix |
| `R/analysis/NSUM/Boot_Step2_FIXED.R` | `R/analysis/NSUM/Boot_Step2.R` | RENAME: drop "_FIXED" |

Documentation files currently in `R/analysis/` that clutter the script directory:
| Current | New location |
|---|---|
| `R/analysis/GETTING_STARTED.md` | `R/analysis/_docs/` |
| `R/analysis/NSUM_REFACTORING_NOTES.md` | `R/analysis/_docs/` |
| `R/analysis/NSUM_WORKFLOW_README.md` | `R/analysis/_docs/` |
| `R/analysis/PARALLEL_PROCESSING_FIX.md` | `R/analysis/_docs/` |
| `R/analysis/WORKFLOW_FIXES_APPLIED.md` | `R/analysis/_docs/` |
| `R/analysis/mcmc_log.txt` | DELETE (regenerable log) |

---

## 5. paper/ cleanup — specific moves

The 50+ versioned drafts go into `paper/archive/`. Only the SIR submission folder stays at top level.

| Current pattern | New location |
|---|---|
| `paper/IJOPM_paperDraft_2020508*.{qmd,tex,pdf,aux,bcf,bbl,blg,toc,log,html,run.xml,synctex.gz}` | `paper/archive/ijopm-drafts/` |
| `paper/IJOPM_paperDraft_2020509*.{qmd,tex,pdf,...}` | `paper/archive/ijopm-drafts/` |
| `paper/IJOPM_paperDraft_202051*.{qmd,tex,pdf,...}` | `paper/archive/ijopm-drafts/` (keep most recent `202051012c` as the post-IJOPM reference) |
| `paper/IJOPM_paperDraft_*_files/` (Quarto compile outputs) | `paper/archive/ijopm-drafts/` |
| `paper/IJOPM paper draft 04-07-2025.docx` | `paper/archive/ijopm-drafts/` |
| `paper/IJOPM paper draft 14-07-2025.docx` | `paper/archive/ijopm-drafts/` |
| `paper/Draft Paper SY 12.01.2024.docx` | `paper/archive/early-drafts/` |
| `paper/British Journal of Management Draft 18-04-2024.{docx,pdf}` | `paper/archive/bjmm/` |
| `paper/BJMMcorner-draft.qmd` | `paper/archive/bjmm/` |
| `paper/DWeMSinUK.Rmd` | `paper/archive/early-drafts/` |
| `paper/Main file submission 31-10-2025.docx` (the older "submitted to JBE" version) | `paper/archive/ijopm-drafts/` |
| `paper/IJOPM-10-2025-1011_Proof_hi.pdf` | `paper/archive/ijopm-drafts/` |
| `paper/9cf4cb63-35b5-4356-8df1-6b6071e7a533.pdf` (IRJ submission PDF) | `paper/archive/irj-submission/` |
| `paper/Editor's decision on BUSI-D-25-05247.pdf` | `paper/decisions/` |
| `paper/Decision on manuscript: 3508448.pdf` | `paper/decisions/` |
| `paper/Re: Decision on manuscript: 3508448.pdf` | `paper/decisions/` |
| `paper/Re: Decision on manuscript: 5552682.pdf` | `paper/decisions/` |
| `paper/CitedReferencesOnly.bib` | `paper/references/` |
| `paper/Bootstrapping Methods.{docx,pdf}` | `paper/supporting/` |
| `paper/Descriptive statistics.docx` | `paper/supporting/` |
| `paper/Book1-Table9.xlsx`, `paper/Figure5.png` | `paper/supporting/` |
| `paper/OneDrive_1_22-05-2026/` | KEEP at top of `paper/` (current submission) |

---

## 6. R script path edits required

After the moves above, the following `here()` references will still work because the moved scripts are stale variants going to `R/archive/`, and the active scripts in `R/analysis/` and `R/data_processing/` are not moving:

| Active file | Current `here()` reference | After reorg |
|---|---|---|
| `R/00-main_pipeline.R` | `here("R", "analysis", "03-rds_estimation.R")` | unchanged ✓ |
| `R/00-main_pipeline.R` | `here("R", "analysis", "05-nsum_estimation.R")` | unchanged ✓ |
| `R/analysis/05-nsum_estimation.R` | (whatever it currently sources) | unchanged ✓ |
| `R/analysis/08-netclust_subgroup_analysis.R` (post-rename) | refs to `_FIXED` may exist in CLAUDE.md but not in code | grep to confirm |

**Path edits I will need to make:**

1. **`R/analysis/08-netclust_subgroup_analysis_FIXED.R` → renamed without `_FIXED`.** Any `source()` or `here()` call elsewhere that references the `_FIXED` filename needs updating. Same for `08b-simple_subgroup_analysis_FIXED.R` and `Boot_Step2_FIXED.R`. I will grep before renaming.

2. **`CLAUDE.md` references.** The current `CLAUDE.md` lists `08-netclust_subgroup_analysis_FIXED.R` and `08b-simple_subgroup_analysis_FIXED.R` as canonical filenames. After the rename, `CLAUDE.md` needs editing to drop the `_FIXED` suffix.

3. **No path edits needed for the active analysis pipeline.** Because the `R/` top-level structure (00-main_pipeline.R, config.R, data_processing/, analysis/, utils/, archive/) is unchanged.

---

## 7. Decisions I need from Scott before executing

These are the calls I would make by default but want to confirm:

**A. The big delete list.** Confirm OK to permanently delete (snapshots preserve everything):
- `DWeMSinUK_1/` (146 MB old clone)
- `DWeMSinUK_RL/` (290 MB old clone)
- `bak/` (388 KB)
- `archive/` (18 MB, single old GitHub zip)
- `documentation/` (empty)
- `analysis/` at root (after moving the .docx out)
- `_Rproj.user/`, `_git/`, `_gitignore` (underscore shadows)

**B. CRAN/ (R package source tarballs).** I propose to `git rm --cached -r CRAN/` and add it to `.gitignore`, keeping the files on disk so `library()` calls still work. Long-term recommendation: switch to `renv` for reproducible package management. OK?

**C. Meetings/ in the index.** This 124 MB folder of audio is gitignored *now*, but 266 files inside it are still tracked from before the gitignore line was added. I'll `git rm --cached -r Meetings/` to stop tracking them. They stay on disk. OK?

**D. output/ large binaries.** The `.RData` and `.rds` files (mostly 5-20 MB bootstrap samples) are tracked but regenerable. ~800 MB total. I propose to add to `.gitignore`:
```
output/*.RData
output/*.rds
output/nsum_*/
output/archive_*/
```
and `git rm --cached` the matching files. Final result: tracked code generates the artifacts; artifacts stay on disk but don't bloat git history going forward. OK?

**E. correspondence/ + emails/ merge.** Proposed: one combined `correspondence/` folder with `2023/`, `2024/`, `2025/`, `2026/` subfolders. Or keep them separate? My recommendation: merge.

**F. paper/ archive depth.** I propose three subfolders under `paper/archive/`: `early-drafts/` (BJMM, Rmd), `ijopm-drafts/` (the 40+ IJOPM versioned files), `bjmm/` (the 2024 BJMM draft). Or just lump everything in `paper/archive/`?

---

## 8. Execution order

If you sign off on the plan, I'll execute in this order to keep each commit coherent:

1. **Commit 1 — Delete duplicates and dead folders.** `git rm` the underscore shadows, the empty `documentation/`, the root `analysis/` (after moving the .docx), and `bak/`, `archive/`. Delete `DWeMSinUK_1/` and `DWeMSinUK_RL/` from disk (also `git rm` since they may have stragglers — though git status says they're untracked).
2. **Commit 2 — Untrack large binaries (Meetings/, output/ RData, CRAN/).** Update `.gitignore` and `git rm --cached`. Files stay on disk.
3. **Commit 3 — R/ cleanup.** Move stale variants to `R/archive/`, drop `_FIXED` suffixes on canonical files, move analysis docs into `R/analysis/_docs/`.
4. **Commit 4 — paper/ cleanup.** Move all versioned drafts to `paper/archive/{ijopm-drafts,bjmm,early-drafts}`, decisions to `paper/decisions/`, references to `paper/references/`, supporting docs to `paper/supporting/`.
5. **Commit 5 — Root-level cleanup.** Move loose PNGs to `output/figures/`, move loose CSV to `output/tables/`, move Yilmaz PDF to `paper/references/`, untrack `.RData`/`.Rhistory`.
6. **Commit 6 — Merge `emails/` into `correspondence/2026/`, re-shelve correspondence by year.**
7. **Commit 7 — Move `tests/` to `R/tests/`.**
8. **Commit 8 — Update `CLAUDE.md` to describe the new structure, write `README.md`, smoke-test the R pipeline.**
9. **Commit 9 — Delete this `REORG_PLAN.md` (or move to `_snapshots/`).**

Each commit will be pushable independently, so if anything goes wrong mid-reorg the worst case is reverting one of these.

---

*End of plan.*
