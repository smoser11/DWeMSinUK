# Glossary

> Full decoder ring for the DWeMSinUK project. See `MEMORY.md` for a quick reference.

## People

- **Scott Moser** — Associate Professor, School of Politics and International Relations, University of Nottingham. Lead author on DWeMSinUK. See `memory/people/scott-moser.md`.
- **Caroline Emberson** (also **CE**, **Caroline**) — Assistant Professor in Operations Management, Nottingham University Business School. Co-author on DWeMSinUK. See `memory/people/caroline-emberson.md`.
- **Selim** — Former project collaborator who worked on the Risk Index development (~2023–2024). Out of touch as of May 2026; both Scott and Caroline have lost contact. See `memory/people/selim.md`.

## Journals (and submission history)

- **SIR** — Social Indicators Research (Springer). Quality-of-life and social-indicators journal. ~15% acceptance rate, 2000 submissions/year. 10,000-word cap. EIC David Bartram (Leicester). Currently the target venue for DWeMSinUK.
- **IJOPM** — International Journal of Operations and Production Management (Emerald). DWeMSinUK desk-rejected 2025.
- **JBE** — Journal of Business Ethics (Springer). DWeMSinUK desk-rejected 2025.
- **IRJ** — Industrial Relations Journal (Wiley). DWeMSinUK desk-rejected.
- **BJIR** — British Journal of Industrial Relations (Wiley). DWeMSinUK desk-rejected 2026.
- **BJM** — British Journal of Management (Wiley). Earlier candidate; the BJM "Methodology Corner" was an old target.

## Methods / Statistical terms

- **RDS** — Respondent-Driven Sampling. A chain-referral method for hidden populations. Original by Heckathorn (1997).
- **RDS-I** — Salganik-Heckathorn estimator (unadjusted sample mean using simple weights).
- **RDS-II** — Volz-Heckathorn estimator (degree-weighted; assumes random walk on social network).
- **RDS-SS** — Gile's Successive Sampling estimator (accounts for finite-population effects via inverse inclusion probability).
- **NSUM** — Network Scale-Up Method. Estimates hidden-population size from general-population reports about social contacts.
- **MBSU** — Modified Basic Scale-Up. NSUM variant with adjustment factors for visibility (τ) and degree ratio (δ).
- **GNSUM** — Generalized NSUM (Feehan & Salganik 2016).
- **MA estimator** — Model-Assisted estimator (Gile & Handcock 2015). The Bayesian RDS estimator implemented in the `RDS` R package as `MA.estimates()`. Parameters M1, M2, number.of.iterations control Monte Carlo convergence.
- **SS-PSE** — Successive Sampling Population Size Estimation (Handcock, Gile, Mar 2014). Bayesian; `sspse` R package.
- **Three-step bootstrap** — Custom procedure documented in ESM_3 for propagating uncertainty in NSUM estimates via RDS sample resampling.
- **τ** (tau) — NSUM visibility/transmission/true-positive-rate adjustment factor. Controls for the fact that respondents may not accurately report on hidden traits of their contacts.
- **δ** (delta) — NSUM degree-ratio adjustment factor. Controls for difference in network sizes between general and hidden populations.

## Domain terms

- **Modern slavery** — UK statutory term covering forced labour, human trafficking, slavery, and servitude (Modern Slavery Act 2015).
- **Forced labour** — ILO operational definition based on 11 indicators.
- **NRM** — UK National Referral Mechanism. The framework through which potential modern slavery victims are identified and referred for support. ~44,360 adult referrals/year baseline used in the paper.
- **ILO Indicators** — International Labour Organization's 11 indicators of forced labour (2012). The DWeMSinUK Risk Index is built from this framework.
- **Risk Index** — DWeMSinUK's continuous measure of labour exploitation exposure. Weighted aggregation of 13 binary indicators. NRM referral weight 0.35; forced labour indicators total 0.55; below-minimum-wage 0.10.
- **PHS** — Personal and Household Service work. UK industrial category containing domestic work.
- **Domestic worker** — Person employed in private households for cleaning, childcare, eldercare, etc. The hidden population in DWeMSinUK. UK estimated ~980,000.
- **Transnational migrant** — A respondent who migrated across borders for domestic work. 93% of the DWeMSinUK sample.
- **Filipina** — The dominant nationality cluster in the DWeMSinUK sample (~66%).

## Policy / SDG terms

- **SDG 8.7** — Sustainable Development Goal target 8.7: "Take immediate and effective measures to eradicate forced labour, end modern slavery and human trafficking..." by 2030. The framing hook for SIR.
- **SDG 16.2.2** — Related indicator: number of victims of human trafficking per 100,000 population.

## Project-internal acronyms

- **DWeMSinUK** — Domestic Worker Exploitation and Modern Slavery in the UK. The project name and repo name.
- **ESM** — Electronic Supplementary Material (Springer convention). ESM_1 codebook; ESM_2 survey questions; ESM_3 bootstrap methodology; ESM_4 statistical appendix.
- **OSCM** — Operations and Supply-Chain Management. Disciplinary framing of earlier submissions (IJOPM etc.); dropped for SIR.
- **CE** — Caroline Emberson (in correspondence and notes).
- **UoN** — University of Nottingham.

## Tools

- **R** — All analysis in R 4.2+. Entry point `R/00-main_pipeline.R`. Key packages: `RDS`, `sspse`, `igraph`, `boot`, `tidyverse`, `here`, `netclust` (compat issues with R 4.5+).
- **OSF** — Open Science Framework (osf.io). For anonymised reviewer-facing code shares (SIR requirement).
- **GitHub** — Repo at `smoser11/DWeMSinUK` (private).
- **OneDrive** — Caroline's shared folder for manuscript files. The `OneDrive_1_22-05-2026/SIR/` folder is the canonical paper directory.
- **Quarto** — For rendering manuscripts (`quarto::quarto_render`).
