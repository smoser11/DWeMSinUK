**You:**

I want to know all about the different ways of performing RDS estimaton in R:   both model-assisted, deisgn based, Bayesian and frequentist.
I want to know all about the esimation functios in the RDS package, the sspse package.  Here are two relevant papers, but there are others:
Gile & Handcock (2010): Model-assisted estimation.
Gile & Handcock (2015): Finite populations and sequential sampling.

I also want to know about more 'boutique' packages, e.g. Neighboot, RDStreeboot,  and also netclust <https://github.com/LJGamble/netclust>

* * *

**ChatGPT:**

To explore the different methods of respondent-driven sampling (RDS) estimation in R, could you please clarify a few points so I can tailor the research appropriately:

1. What is your primary focus or use case—population size estimation, prevalence estimation, network structure, or something else?
    
2. Should I include comparisons across methods (e.g., performance, assumptions, robustness), or focus only on describing what each package/function does?
    
3. Do you want full function-level documentation for the `RDS` and `sspse` packages, including arguments, examples, and outputs?
    
4. Should I include relevant examples of use cases from the literature or just focus on the packages and methods themselves?
    

Once I have this, I’ll dive into a thorough review of model-assisted, design-based, Bayesian, and frequentist approaches to RDS estimation in R, including `Neighboot`, `RDStreeboot`, and `netclust`.

* * *

**ChatGPT:**

To tailor this research thoroughly, could you please clarify the following:

1. Are you mainly interested in RDS estimation for population size, prevalence estimation, network structure inference, or a combination?
    
2. Would you like detailed function-level documentation for `RDS` and `sspse` packages, including parameters, usage examples, and expected outputs?
    
3. Should the review compare methods (e.g., accuracy, assumptions, robustness) or focus purely on method descriptions?
    
4. Would you like illustrative examples from applied studies or focus strictly on methodological implementation in R?
    

Once I have this, I’ll proceed with an in-depth review of model-assisted, design-based, Bayesian, and frequentist RDS estimation approaches in R, including "boutique" packages like `Neighboot`, `RDStreeboot`, and `netclust`.

* * *

**You:**

1. populatioin size but also the population distribution of a continuous individual-level attribute ('risk-index')
2. yes, annotated output
3. yes please
4. please also include relevant academic publications.

* * *

**ChatGPT:**

my best guess is

1. you want a practical, end-to-end map of RDS estimators in R—design-based, model-assisted, Bayesian and frequentist—with concrete function names, example code (including annotated outputs), and how to estimate both (a) population size and (b) the _distribution of a continuous “risk-index”_; and
    
2. you’d like this to cover the core `RDS` and `sspse` packages plus “boutique” tools such as `Neighboot`, `RDStreeboot`, and the `netclust` GitHub package—along with key papers (e.g., Gile & Handcock 2010/2015).
    

Below I go step-by-step. I start with the _what/when_ of each inferential approach, then show _how_ in R for both **population size** and **a continuous risk-index distribution**, and finish with a compact bibliography.

* * *

1) The four inferential “families” for RDS (what to use when)
=============================================================

**A. Design-based (classic RDS estimators).**

* **RDS-I / SH** (Salganik–Heckathorn, 2004) and **RDS-II / VH** (Volz–Heckathorn, 2008) treat RDS as a probability sample with inclusion probs proportional to degree. These are the baseline, especially when the _sample fraction is small_ and assumptions like random recruitment roughly hold. Implemented in `RDS::RDS.I.estimates()` and `RDS::RDS.II.estimates()`. [CRAN](https://cran.r-project.org/web/packages/RDS/RDS.pdf)[Princeton University](https://www.princeton.edu/~mjs3/salganik_heckathorn04.pdf?utm_source=chatgpt.com)[ResearchGate](https://www.researchgate.net/publication/283857994_Probability_Based_Estimation_Theory_for_Respondent_Driven_Sampling?utm_source=chatgpt.com)
    
* **Uncertainty**: use `RDS::RDS.bootstrap.intervals()` (with “Salganik” or “SRS” style uncertainty) or the more structure-aware bootstraps below. [CRAN](https://cran.r-project.org/web/packages/RDS/RDS.pdf)
    

**B. Finite-population / sequential sampling corrections (frequentist core).**

* **Gile’s Successive Sampling (SS) estimator** adjusts for _without-replacement_ sampling and non-negligible sample fractions; implemented in `RDS::RDS.SS.estimates()` (requires an N or a prior guess). Use this when your sample is a sizable share of the population or degree heterogeneity is strong. [CRAN](https://cran.r-project.org/web/packages/RDS/RDS.pdf)
    

**C. Model-assisted (network-model-assisted).**

* **Gile & Handcock (2015)** propose a _working network model_ (ERGM/ERNM) to account for seed bias and homophily; implemented as `RDS::MA.estimates()` with simulation under a fitted network model. Use this when you suspect seed bias or strong homophily that chain length cannot wash out. [CRAN](https://cran.r-project.org/web/packages/RDS/RDS.pdf)[PMC](https://pmc.ncbi.nlm.nih.gov/articles/PMC4669074/?utm_source=chatgpt.com)
    

**D. Bayesian/model-based for _population size_** (and diagnostics).

* **SS-PSE** (Successive Sampling – Population Size Estimation) is a Bayesian/MCMC approach using order-of-recruitment and degree/visibility to estimate **N**: `sspse::posteriorsize()` (+ `dsizeprior()`, `impute.visibility()`, `pospreddeg()`). Works with one or multiple RDS waves; multiple-survey pooling is in Kim & Handcock (2021). Recent visibility-model updates (McLaughlin et al., 2024) are in the current `sspse`. Use when N is unknown (typical), and you can specify reasonable priors and visibility modeling. [CRAN+1](https://cran.r-project.org/web/packages/sspse/sspse.pdf)[Oxford Academic](https://academic.oup.com/jssam/article-abstract/9/1/94/5710670?utm_source=chatgpt.com)[eScholarship](https://escholarship.org/content/qt1j00x8f8/qt1j00x8f8.pdf?utm_source=chatgpt.com)
    

**E. Boutique bootstrap variance estimators** (for _any_ statistic, including quantiles).

* **Tree bootstrap**: `RDStreeboot::treeboot.RDS()` resamples recruitment trees to get _quantiles_ and CIs for numerical traits (great for your continuous risk-index). [CRAN](https://cran.r-project.org/web/packages/RDStreeboot/RDStreeboot.pdf)[PNAS](https://www.pnas.org/doi/10.1073/pnas.1617258113?utm_source=chatgpt.com)
    
* **Neighborhood bootstrap**: `Neighboot::neighb()` bootstraps local neighborhoods using the RDS tree + degree info; integrates with `RDStreeboot` and can convert to/from `rds.data.frame`. Good for robust CIs under RDS dependence. [CRAN](https://cran.r-project.org/web/packages/Neighboot/Neighboot.pdf)
    

**F. Extensions for clustered populations.**

* **`netclust` (GitHub)** extends SS-PSE ideas to _clustered_ hidden populations (unequal cluster sizes/visibility). Consider this if you have clear sub-communities (e.g., venue-based clusters) and recruitment is cluster-structured. [GitHub](https://github.com/LJGamble/netclust)
    

* * *

2) Data setup (applies to all examples)
=======================================

Your RDS data should be an `rds.data.frame` with at least:

* `id`, `recruiter.id`, a degree/visibility variable (often `network.size`), seed flags, and your outcome(s) including `risk_index`.
    

Minimal template:

```r
library(RDS)
# df has columns: id, recruiter.id, network.size (degree), risk_index (continuous), seed, etc.
rds <- as.rds.data.frame(df, id.col = "id", recruiter.id.col = "recruiter.id",
                         network.size.variable = "network.size")
plot(rds)  # quick diagnostics: recruitment tree, degree by wave, etc.
```

(See `plot.rds.data.frame`, `convergence.plot`, `bottleneck.plot`, `cumulative.estimate` for diagnostics.) [CRAN+1](https://cran.r-project.org/web/packages/RDS/RDS.pdf)

* * *

3) Estimating the **population size N** (SS-PSE in `sspse`)
===========================================================

### 3.1 Single-survey SS-PSE (with visibility modeling)

```r
library(sspse)

# 1) Set a weakly informative prior on N (choose on the log-scale via mode or median).
prior <- dsizeprior(n = nrow(rds),                        # sample size
                    type = "beta",                        # prior family on n/N
                    mode.prior.size = 10000)             # prior mode for N (edit!)

# 2) (Optional but recommended) Improve degree/visibility via imputation + recruitment timing
fit_vis <- impute.visibility(rds.data = rds,
                             type.impute = "distribution",   # draw from conditional
                             recruit.time = NULL,            # uses sample order if NULL
                             interval = 10, burnin = 5000)   # MCMC controls

# 3) Posterior for N (SS-PSE). If visibility imputed, the function will use it.
fitN <- posteriorsize(rds, median.prior.size = 10000,      # OR mode.prior.size
                      burnin = 20000, interval = 50, samplesize = 2000)

summary(fitN)   # Posterior mean/median/mode/quantiles for N, convergence info
plot(fitN, mcmc = TRUE)  # trace/density diagnostics (ESS, mixing, etc.)
```

**What you’ll see (annotated output).**  
`summary(fitN)` prints posterior summaries for **N** (mean/median/mode, 25/50/75/90% quantiles), and `plot()` shows MCMC traces and posterior histograms. If visibility is mis-modeled, `pospreddeg(fitN)` flags degree percentile mismatches (posterior predictive diagnostics). [CRAN](https://cran.r-project.org/web/packages/sspse/sspse.pdf)

> **References.** SS-PSE is described in Handcock, Gile & Mar (2014; 2015), with multi-survey pooling in Kim & Handcock (2021) and a visibility-model upgrade in McLaughlin et al. (2024). [Project Euclid](https://projecteuclid.org/journals/electronic-journal-of-statistics/volume-8/issue-1/Estimating-hidden-population-size-using-Respondent-Driven-Sampling-data/10.1214/14-EJS923.full?utm_source=chatgpt.com)[Mark S. Handcock](https://hpmrg.github.io/sspse/?utm_source=chatgpt.com)[Oxford Academic](https://academic.oup.com/jssam/article-abstract/9/1/94/5710670?utm_source=chatgpt.com)[eScholarship](https://escholarship.org/content/qt1j00x8f8/qt1j00x8f8.pdf?utm_source=chatgpt.com)

### 3.2 Multiple RDS surveys (pooled N)

If you have two or more surveys in the same locale/population (ideal):

```r
# Example structure from sspse data sets:
data(fauxmadrona2)          # list of RDS surveys; see ?fauxmadrona2
# Fit posteriorsize to each and combine using Kim & Handcock (2021) methodology
# (see package vignette; core function remains posteriorsize with multi-survey logic)
```

The method leverages overlap/recapture info and shared visibility priors to shrink uncertainty. [Oxford Academic](https://academic.oup.com/jssam/article-abstract/9/1/94/5710670?utm_source=chatgpt.com)

### 3.3 Clustered populations

If you expect _clusters_ (e.g., venues), the vanilla SS-PSE may under- or over-estimate **N**. The experimental `netclust` package implements an SS-PSE extension to account for clustering (unequal cluster sizes / within-cluster recruitment). It’s on GitHub and requires building from source. [GitHub](https://github.com/LJGamble/netclust)

* * *

4) Estimating the **distribution of a continuous “risk-index”**
===============================================================

There are two complementary paths:

4A) Design/frequentist estimators + weighted summaries (means, CIs)
-------------------------------------------------------------------

* **RDS-II (VH)** or **SS** can estimate the _mean_ of `risk_index` with finite-population correction (for SS) and empirical-likelihood CIs.
    

```r
# VH / RDS-II mean of risk_index
vh <- RDS.II.estimates(rds.data = rds, outcome.variable = "risk_index",
                       empir.lik = TRUE)  # Empirical-likelihood CI
vh
# Annotated output: point estimate (weighted mean), 95% CI, design effect, SE, n

# SS mean (better when sample fraction is non-negligible)
ss <- RDS.SS.estimates(rds.data = rds, outcome.variable = "risk_index",
                       N = round(summary(fitN)$median))  # plug-in N from SS-PSE
ss
# Annotated output: SS-weighted mean, 95% CI (empirical likelihood by default)
```

Under the hood, `RDS` also exposes the weights so you can compute _weighted_ regression or custom summaries:

```r
w_ss <- compute.weights(rds, weight.type = "Gile's SS", N = round(summary(fitN)$median))
# Example: weighted linear regression of risk_index on covariates
fit <- svyglm(risk_index ~ age + group, design = svydesign(ids = ~1, weights = ~w_ss, data = rds))
summary(fit)  # coefficients with RDS-aware weights
```

(Functions and arguments above are in the current `RDS` manual.) [CRAN+1](https://cran.r-project.org/web/packages/RDS/RDS.pdf)

> **When to prefer SS over VH.** Use SS if (i) the sample is a sizable fraction of the target population, or (ii) degree heterogeneity is large; both are classic contexts where VH exhibits finite-population bias. [PMC](https://pmc.ncbi.nlm.nih.gov/articles/PMC4669074/?utm_source=chatgpt.com)

4B) Distributional inference (quantiles, credible intervals) via bootstrap
--------------------------------------------------------------------------

Because RDS dependence complicates analytic quantile SEs, **bootstrap methods that respect the recruitment tree** are ideal:

### (i) Tree bootstrap (`RDStreeboot`)

```r
library(RDStreeboot)

# Build the "samp" list (nodes, edges, degree, traits) from your rds.data.frame
# If you only have rds.data.frame, see Neighboot::to.rds() for conversion utilities.
samp <- list(nodes = rds$id,
             edges = list(node1 = rds$recruiter.id, node2 = rds$id),
             degree = rds$network.size,
             traits = data.frame(risk_index = rds$risk_index))

# 80% and 95% intervals for quantiles of the risk_index distribution
qs <- treeboot.RDS(samp, quant = c(0.025, 0.10, 0.50, 0.90, 0.975), B = 4000)
qs
# Annotated output: rows are traits (here risk_index), columns are requested quantiles.
```

This method resamples entire _recruitment trees_, preserving dependencies. See Baraff, McCormick & Raftery (PNAS) for details. [CRAN](https://cran.r-project.org/web/packages/RDStreeboot/RDStreeboot.pdf)[PNAS](https://www.pnas.org/doi/10.1073/pnas.1617258113?utm_source=chatgpt.com)

### (ii) Neighborhood bootstrap (`Neighboot`)

```r
library(Neighboot)
# If you simulated with RDStreeboot::sample.RDS, you can convert to rds.data.frame via to.rds(),
# and vice versa. Here we call neighb() directly on the RDS sample representation:
out <- neighb(RDS.data = samp, quant = c(0.025, 0.10, 0.50, 0.90, 0.975),
              method = "percentile", B = 4000)
out
# Annotated output: a matrix with estimated SEs and requested quantiles for each trait.
```

`Neighboot` also provides `to.rds()` to convert `sample.RDS` output to an `rds.data.frame` for seamless use with `RDS`. [CRAN](https://cran.r-project.org/web/packages/Neighboot/Neighboot.pdf)

> **Practical tip.** For a _smooth_ distribution (KDE/ECDF) plot of `risk_index`, compute an RDS weight vector (VH or SS) and use a weighted ECDF or KDE—_but_ report quantile CIs from tree/neighborhood bootstraps to respect RDS dependence.

* * *

5) Model-assisted estimation for your risk-index (correcting seed bias/homophily)
=================================================================================

When seed selection is biased or homophily is high, the **network model-assisted** estimator can reduce bias:

```r
# Model-assisted estimator (simulates RDS on working networks under an ERGM/ERNM)
ma <- MA.estimates(rds.data = rds, trait.variable = "risk_index",
                   seed.selection = "degree",   # working assumption for seed bias
                   N = round(summary(fitN)$median),
                   M1 = 250, M2 = 10, number.of.iterations = 4e4)
ma
# Annotated output: model-assisted point estimate & uncertainty for the risk_index mean.
```

The method (Gile & Handcock 2015, JRSS-A) leverages a _working network model_ to calibrate inclusion probabilities and attenuate seed bias effects. Use diagnostics (e.g., degree and recruit counts by wave) to check plausibility of the working model. [CRAN](https://cran.r-project.org/web/packages/RDS/RDS.pdf)[PubMed](https://pubmed.ncbi.nlm.nih.gov/26640328/?utm_source=chatgpt.com)

* * *

6) Putting it all together: a minimal, reproducible pipeline
============================================================

```r
# --- Setup
library(RDS); library(sspse); library(RDStreeboot); library(Neighboot)

# rds: your rds.data.frame with id, recruiter.id, network.size, risk_index
plot(rds)                       # quick validity/diagnostics
convergence.plot(rds, "risk_index")    # trend/convergence check

# --- Step 1: Estimate N (SS-PSE)
fitN <- posteriorsize(rds, median.prior.size = 10000,
                      burnin = 20000, interval = 50, samplesize = 2000)
summary(fitN)   # Posterior for N

# --- Step 2: Mean of risk_index (SS with finite-pop correction)
ss <- RDS.SS.estimates(rds, "risk_index", N = round(summary(fitN)$median))
print(ss)       # mean, 95% CI, design effect

# --- Step 3: Distributional CIs for risk_index (tree bootstrap)
samp <- list(nodes = rds$id,
             edges = list(node1 = rds$recruiter.id, node2 = rds$id),
             degree = rds$network.size,
             traits = data.frame(risk_index = rds$risk_index))

qs <- treeboot.RDS(samp, quant = c(0.05, 0.25, 0.50, 0.75, 0.95), B = 4000)
qs             # 5th/25th/median/75th/95th with bootstrap uncertainty

# --- Step 4 (optional): Model-assisted mean to adjust seed bias/homophily
ma <- MA.estimates(rds, "risk_index", N = round(summary(fitN)$median),
                   seed.selection = "degree", M1 = 250, M2 = 10,
                   number.of.iterations = 4e4)
print(ma)

# --- Step 5 (optional): Neighborhood bootstrap as a robustness check
neigh <- neighb(RDS.data = samp, quant = c(0.025, 0.50, 0.975), B = 4000)
neigh
```

**What you’ll get (annotated outputs):**

* `summary(fitN)`: “Posterior for N: mean ~…, median ~…, 95% CrI [L,U]” plus MCMC diagnostics.
    
* `ss`: “risk_index (SS): mean = …, 95% CI […], design effect = …, SE = …, n = …”.
    
* `qs`: matrix of requested quantiles with bootstrap intervals for `risk_index`.
    
* `ma`: model-assisted point estimate and uncertainty—compare to VH/SS to assess seed bias/homophily impact.
    
* `neigh`: SEs and quantiles under neighborhood bootstrap (robustness to dependence).
    

Cited function definitions and arguments are in the current CRAN manuals for `RDS`, `sspse`, `Neighboot`, and `RDStreeboot`. [CRAN+3CRAN+3CRAN+3](https://cran.r-project.org/web/packages/RDS/RDS.pdf)

* * *

7) Function cheat-sheet (by task)
=================================

| Task | Package → function(s) | Notes |
| --- | --- | --- |
| Mean/prop under VH or SH | `RDS::RDS.II.estimates()`, `RDS::RDS.I.estimates()` | `empir.lik=TRUE` for CIs; SH for categorical only. [CRAN](https://cran.r-project.org/web/packages/RDS/RDS.pdf) |
| Finite-pop mean (SS) | `RDS::RDS.SS.estimates()` | Needs **N** (use `sspse`). [CRAN](https://cran.r-project.org/web/packages/RDS/RDS.pdf) |
| Model-assisted mean | `RDS::MA.estimates()` | Controls seed bias/homophily via working network model. [CRAN](https://cran.r-project.org/web/packages/RDS/RDS.pdf) |
| Weighted regressions | `RDS::compute.weights()` + `survey::svyglm()` | Use SS weights when sample fraction isn’t negligible. [CRAN](https://cran.r-project.org/web/packages/RDS/RDS.pdf) |
| CIs via classic bootstrap | `RDS::RDS.bootstrap.intervals()` | “Salganik”, “Gile”, “SRS” options. [CRAN](https://cran.r-project.org/web/packages/RDS/RDS.pdf) |
| Quantile CIs for continuous traits | `RDStreeboot::treeboot.RDS()` | Recruitment-tree-aware bootstrap. [CRAN](https://cran.r-project.org/web/packages/RDStreeboot/RDStreeboot.pdf) |
| Neighborhood bootstrap CIs | `Neighboot::neighb()` | Works with RDStreeboot objects; `to.rds()` converter. [CRAN](https://cran.r-project.org/web/packages/Neighboot/Neighboot.pdf) |
| **Population size N** (single survey) | `sspse::posteriorsize()` + `dsizeprior()` + `impute.visibility()` + `pospreddeg()` | SS-PSE Bayesian posterior for **N** with visibility modeling. [CRAN](https://cran.r-project.org/web/packages/sspse/sspse.pdf) |
| **Population size N** (multiple surveys) | `sspse` (as above) using multi-survey framework | See Kim & Handcock (2021). [Oxford Academic](https://academic.oup.com/jssam/article-abstract/9/1/94/5710670?utm_source=chatgpt.com) |
| **Clustered populations** (experimental) | `netclust` (GitHub) | SS-PSE extension for clustering. [GitHub](https://github.com/LJGamble/netclust) |

* * *

8) Assumptions & diagnostics you should always check
====================================================

* **Degree/visibility quality.** Use `sspse::impute.visibility()` and posterior predictive checks (`pospreddeg`) if self-reported degrees are noisy or capped by coupon limits. [CRAN](https://cran.r-project.org/web/packages/sspse/sspse.pdf)
    
* **Seed bias & homophily.** If convergence plots show slow mixing or composition drift by wave, consider model-assisted `MA.estimates()` or at least compare VH vs SS vs MA. [CRAN](https://cran.r-project.org/web/packages/RDS/RDS.pdf)
    
* **Finite-population effects.** If sample fraction ≳5–10%, SS typically outperforms VH. [PMC](https://pmc.ncbi.nlm.nih.gov/articles/PMC4669074/?utm_source=chatgpt.com)
    
* **Uncertainty.** Prefer _tree/neighborhood_ bootstrap for distributional features (quantiles) because they preserve RDS dependence. [CRAN+1](https://cran.r-project.org/web/packages/RDStreeboot/RDStreeboot.pdf)
    

* * *

9) Key academic references (selected)
=====================================

**Core RDS estimators & diagnostics**

* Salganik, M. J., & Heckathorn, D. D. (2004). _Sampling and Estimation in Hidden Populations Using Respondent-Driven Sampling._ **Sociological Methodology** 34:193–239. (RDS-I / SH) [Princeton University](https://www.princeton.edu/~mjs3/salganik_heckathorn04.pdf?utm_source=chatgpt.com)
    
* Volz, E., & Heckathorn, D. D. (2008). _Probability-Based Estimation Theory for RDS._ **Journal of Official Statistics** 24(1):79–97. (RDS-II / VH) [ResearchGate](https://www.researchgate.net/publication/283857994_Probability_Based_Estimation_Theory_for_Respondent_Driven_Sampling?utm_source=chatgpt.com)
    
* Gile, K. J., & Handcock, M. S. (2010). _Respondent-Driven Sampling: An Assessment of Current Methodology._ **Sociological Methodology** 40:285–327. (assumptions, biases) [PMC](https://pmc.ncbi.nlm.nih.gov/articles/PMC3437336/?utm_source=chatgpt.com)
    
* Gile, K. J. (2011). _Improved Inference for RDS with Application to HIV Prevalence (Successive Sampling Estimator)._ **JASA** 106:135–146. (SS estimator) [JSTOR](https://www.jstor.org/stable/41415539?utm_source=chatgpt.com)
    
* Gile, K. J., & Handcock, M. S. (2015). _Network Model-Assisted Inference from RDS Data._ **JRSS-A** 178(3):619–639. (model-assisted) [PMC](https://pmc.ncbi.nlm.nih.gov/articles/PMC4669074/?utm_source=chatgpt.com)
    
* Gile, K. J., Beaudry, I. S., & Handcock, M. S. (2018). _Methods for Inference from RDS Data._ **Annual Review of Statistics & Its Application**. (survey) [Annual Reviews](https://www.annualreviews.org/doi/pdf/10.1146/annurev-statistics-031017-100704?utm_source=chatgpt.com)
    

**Uncertainty via bootstrap**

* Baraff, A. J., McCormick, T. H., & Raftery, A. E. (2016). _Estimating Uncertainty in RDS Using a Tree Bootstrap._ **PNAS**. (RDStreeboot) [PNAS](https://www.pnas.org/doi/10.1073/pnas.1617258113?utm_source=chatgpt.com)
    
* Yauck, M., & Moodie, E. E. M. (2020). _Neighbourhood Bootstrap for RDS._ (arXiv preprint; `Neighboot` package) [arXiv](https://arxiv.org/pdf/2010.00165?utm_source=chatgpt.com)
    

**Population size estimation (SS-PSE and extensions)**

* Handcock, M. S., Gile, K. J., & Mar, C. M. (2014). _Estimating Hidden Population Size Using RDS Data._ **Electronic Journal of Statistics** 8(1):1491–1521. (SS-PSE core) [Project Euclid](https://projecteuclid.org/journals/electronic-journal-of-statistics/volume-8/issue-1/Estimating-hidden-population-size-using-Respondent-Driven-Sampling-data/10.1214/14-EJS923.full?utm_source=chatgpt.com)
    
* Handcock, M. S., Gile, K. J., & Mar, C. M. (2015). _Estimating the Size of Populations at High Risk for HIV Using RDS Data._ **Biometrics** 71(1):258–266. [Mark S. Handcock](https://hpmrg.github.io/sspse/?utm_source=chatgpt.com)
    
* Kim, B. J., & Handcock, M. S. (2021). _Population Size Estimation Using Multiple RDS Surveys._ **JSSAM** 9(1):94–120. [Oxford Academic](https://academic.oup.com/jssam/article-abstract/9/1/94/5710670?utm_source=chatgpt.com)
    
* McLaughlin, K. R., et al. (2024). _Modeling the Visibility Distribution for Population Size Estimation with RDS._ **Annals of Applied Statistics** 18(1):683–703. (visibility upgrade; now in `sspse`) [Department of Statistics](https://stat.oregonstate.edu/directory/katherine-mclaughlin?utm_source=chatgpt.com)
    
* `netclust` GitHub: _Estimate the Size of Clustered Hidden Populations Using RDS Data: an Extension of SS-PSE._ (clustered extension) [GitHub](https://github.com/LJGamble/netclust)
    

**Software documentation (function-level details)**

* `RDS` package manual (current): estimators `RDS.I.estimates`, `RDS.II.estimates`, `RDS.SS.estimates`, `MA.estimates`, diagnostics & bootstrap. [CRAN](https://cran.r-project.org/web/packages/RDS/RDS.pdf)
    
* `sspse` package manual (current): `posteriorsize`, `dsizeprior`, `impute.visibility`, `pospreddeg`, summaries/plots, examples. [CRAN](https://cran.r-project.org/web/packages/sspse/sspse.pdf)
    
* `RDStreeboot` manual: `treeboot.RDS`, `sample.RDS`. [CRAN](https://cran.r-project.org/web/packages/RDStreeboot/RDStreeboot.pdf)
    
* `Neighboot` manual: `neighb`, `to.rds`. [CRAN](https://cran.r-project.org/web/packages/Neighboot/Neighboot.pdf)
    

* * *

Final quick picks for your two targets
--------------------------------------

* **Population size (N):** `sspse::posteriorsize()` (+ `impute.visibility()`, `pospreddeg()`). If multiple RDS rounds exist, use the multi-survey framework (Kim & Handcock 2021). Consider `netclust` if recruitment is clearly cluster-structured. [CRAN](https://cran.r-project.org/web/packages/sspse/sspse.pdf)[Oxford Academic](https://academic.oup.com/jssam/article-abstract/9/1/94/5710670?utm_source=chatgpt.com)[GitHub](https://github.com/LJGamble/netclust)
    
* **Distribution of a continuous “risk-index”:**
    
    1. Get the **mean** and CI with `RDS.SS.estimates()` (plugging in **N** from SS-PSE).
        
    2. Get **distributional quantiles + CIs** with `RDStreeboot::treeboot.RDS()` (and/or `Neighboot::neighb()` as a robustness check).
        
    3. (If seed bias/homophily suspected) compare to `RDS::MA.estimates()` to assess bias reduction. [CRAN+3CRAN+3CRAN+3](https://cran.r-project.org/web/packages/RDS/RDS.pdf)
        

If you want, I can adapt this to your exact data schema (variable names, number of coupons, known priors on N) and produce ready-to-run scripts that print clean, annotated tables for a paper appendix.

* * *

**You:**

Thank you. what does ' Markov chain appears to be periodic.' mean?'

* * *

**ChatGPT:**

my best guess is

1. you’re seeing a warning from an MCMC or Markov-chain routine telling you the chain keeps revisiting states in a fixed rhythm (e.g., every other step), and
    
2. that rhythmic “strobing” (called **periodicity**) prevents the _raw_ n-step distribution from settling to a single stationary distribution, even if one exists.
    

Here’s the step-by-step:

1. **What “periodic” means (formal + intuition).**
    
    * For a Markov chain with transition matrix $P$, the **period** of a state $i$ is
        
        $$ d(i) \;=\; \gcd\{\, n\ge 1 : (P^n)_{ii} > 0 \,\}.$$
        
        If $d(i)>1$, state $i$ is **periodic**. In an **irreducible** chain (every state reaches every other), all states share the same period $d$.
        
    * **Intuition:** the chain can only return to a state at times that are multiples of $d$. It “ticks” through classes of states on a fixed schedule.
        
    * A chain is **aperiodic** if $d=1$. That’s the good case for usual convergence guarantees.
        
2. **Why you should care (convergence behavior).**
    
    * If the chain is **aperiodic + irreducible + positive recurrent**, then $P^n(x,\cdot)\to \pi(\cdot)$: the law of $X_n$ converges to the stationary distribution $\pi$.
        
    * If the chain is **periodic**, even with a unique $\pi$, the sequence $P^n(x,\cdot)$ **oscillates** among $d$ residue classes and **does not** converge pointwise.
        
    * However, the **Cesàro averages** do converge: $\tfrac1N\sum_{n=1}^N P^n(x,\cdot)\to \pi(\cdot)$. That’s why empirical time-averages can still behave OK, but raw draws may look “striped.”
        
3. **The simplest example (period 2).**
    
    $$ P=\begin{pmatrix}0&1\\[2pt]1&0\end{pmatrix}.$$
    
    Starting at state 1, the chain goes $1\to 2\to 1\to 2\to \cdots$. Returns to 1 happen only at even times; $\gcd\{2,4,6,\dots\}=2$. This is the classic **bipartite**/alternating chain.
    
4. **Common real-world causes (including MCMC).**
    
    * **Bipartite transition structure:** random walk on a bipartite graph (e.g., an even cycle or grid) without self-loops.
        
    * **Always-move proposals:** if your update rule never allows staying put (no self-transition) and the state space splits into disjoint “even/odd” classes you hop between.
        
    * **Systematic scans that alternate blocks** together with deterministic/always-accepted updates can induce period-2 oscillation.
        
    * **Exact symmetries** in the model that force alternating patterns.
        
5. **Quick diagnostic checks.**
    
    * **Self-loop test:** if for _some_ state $i$, $P_{ii} > 0$, that state has period 1; in an irreducible chain this forces the whole chain to be **aperiodic**. In MCMC, any nonzero rejection probability typically creates $P_{ii}>0$.
        
    * **Graph viewpoint:** if the directed graph of positive transitions is bipartite (no odd cycles), the period is at least 2.
        
    * **Empirical tell:** traces that “stripe” (e.g., variable flips sign every step), or autocorrelation at lag 1 near $-1$, or summaries that differ sharply between even and odd iterations.
        
6. **Consequences for inference.**
    
    * **No pointwise convergence** of $X_n$ to $\pi$: histograms of the last draw can depend on whether you stop at an even vs odd iteration.
        
    * **Time-averages** (ergodic averages) can still converge, but convergence can be slower and diagnostics misleading if you ignore the oscillation.
        
    * **Thinning every $d$ steps does not fix periodicity.** It just selects one residue class and can **bias** you. The cure is to **break** periodicity, not to thin to multiples.
        
7. **How to fix it (practical remedies).**
    
    * **Add “laziness”:** replace $P$ with $P'=(1-\varepsilon)P+\varepsilon I$ (e.g., $\varepsilon=0.05$). This injects a self-loop everywhere, making the chain aperiodic while leaving $\pi$ unchanged.
        
    * **Allow rejections / self-transitions:** in Metropolis–Hastings, don’t force acceptance; ordinary acceptance rules already create $P_{ii}>0$.
        
    * **Random-scan updates:** instead of deterministic block alternation, randomly choose which block/coordinate to update.
        
    * **Break bipartiteness:** add occasional “type-switching” moves that cross the parity barrier (e.g., small-probability long-range proposals).
        
    * **Jitter deterministic steps:** add small randomness to updates that were previously exact/fixed.
        
8. **How to confirm the fix worked.**
    
    * Check that some $P'_{ii}>0$ (the code’s analog: nonzero chance to stay).
        
    * Look for disappearance of even/odd oscillations in traces; lag-1 autocorrelation should move away from $-1$.
        
    * Run multiple chains and verify that last-draw summaries no longer depend on iteration parity; standard diagnostics (R-hat, ESS) behave sensibly.
        
9. **Tie-in to RDS/MCMC contexts.**
    
    * In RDS-related Bayesian routines (e.g., population-size or visibility-model MCMC), you might get this warning if updates ping-pong between two partitions of the latent space. Adding **lazy steps** (a small self-loop) or **random-scan** updates typically resolves it without changing the target posterior.
        
10. **One-line takeaway.**
    
    * **Periodic** = the chain returns to states on a fixed schedule (period $d>1$), so $X_n$ doesn’t settle to $\pi$. Ensure **aperiodicity** (e.g., via a small self-loop) to recover standard convergence guarantees.

* * *

**You:**

I get that message after calling posteriorsize()  Should I be worried?  What can I do to rectify?

* * *

**ChatGPT:**

my best guess is

1. the warning is telling you that the MCMC inside `sspse::posteriorsize()` is alternating in a regular even/odd pattern (period-2) rather than “wandering” freely; 2) you only need to worry if that alternation is strong enough to hurt mixing or make summaries depend on whether you stop on an even vs odd iteration; and 3) you can usually fix it by making the sampler a bit “noisier” or by using the visibility submodel/imputation so there’s a genuine chance to “stay put” or move irregularly.
    

Step-by-step:

1. **Confirm whether it’s a real problem (not just a harmless diagnostic).**
    
    * Plot the built-in MCMC diagnostics:
        
        ```r
        plot(fit, mcmc = TRUE)
        ```
        
        You’re looking for even/odd striping in traces and a lag-1 ACF near −1. The `plot.sspse(..., mcmc=TRUE)` panel is provided exactly for this purpose. [CRAN](https://cran.r-project.org/web/packages/sspse/sspse.pdf)
        
    * Also inspect the returned MCMC matrix (`fit$sample`) with standard tools (it’s an `mcmc` object): check ESS and ACF. The manual notes that `sample` can be plotted/summarized with MCMC diagnostics. [CRAN](https://cran.r-project.org/web/packages/sspse/sspse.pdf)
        
    * If traces don’t show strong parity oscillation and ESS is healthy, you can proceed—the ergodic averages that `summary(fit)` reports are typically fine even when the _raw_ chain shows some periodicity. (Periodicity means returns happen on multiples of $d>1$; what matters is whether it harms convergence of time-averages.) [randomservices.org](https://www.randomservices.org/random/markov/Periodicity.html?utm_source=chatgpt.com)
        
2. **Low-intrusion tweaks that often resolve periodicity.**  
    These aim to break any deterministic “ping-pong” in the updates without changing your model.
    
    * **Increase proposal scales slightly** so not every proposal is (almost) deterministically accepted and the chain isn’t forced to hop back and forth. In `posteriorsize()` you can pass, e.g.,  
        `mu_proposal`, `nu_proposal`, `beta_0_proposal`, `beta_t_proposal`, `beta_u_proposal`, `memmu_proposal`, `memscale_proposal`. Try multiplying each by ~2 and re-run; then tune toward a moderate acceptance rate. [CRAN+1](https://cran.r-project.org/web/packages/sspse/sspse.pdf)
        
    * **Run a longer burn-in and take a thicker interval** (this doesn’t _cure_ periodicity, but it helps you diagnose stability and avoid using start-up transients): increase `burnin` (and `burnintheta`, `burninbeta`) and keep `interval` at ≥10 for production runs (the docs use `interval=10` as a typical setting). [CRAN+1](https://cran.r-project.org/web/packages/sspse/sspse.pdf)
        
    * **Randomness in visibilities helps.** If you’re not already, let the model treat reported degree with measurement error / latent visibility (the default is `visibility=TRUE`) and avoid purely deterministic imputes. Use stochastic imputation:
        
        ```r
        vis <- impute.visibility(rds, type.impute = "distribution", interval = 10, burnin = 5000)
        fit <- posteriorsize(rds, visibility = TRUE, type.impute = "distribution", ...)
        ```
        
        The imputation step and the visibility model inject variability that typically breaks even/odd oscillations. [CRAN](https://cran.r-project.org/web/packages/sspse/sspse.pdf)
        
3. **Model/diagnostic adjustments that can help.**
    
    * **Provide recruitment times** if you have them (`recruit.time`, `recruit.time2`) so the visibility model fits more flexibly; this can reduce deterministic back-and-forth driven by ordering artifacts. [CRAN](https://cran.r-project.org/web/packages/sspse/sspse.pdf)
        
    * **Check posterior predictive degree diagnostics** right after fitting:
        
        ```r
        plot(pospreddeg(fit))
        ```
        
        Systematic extremes signal misspecification in the visibility/degree model that can also manifest as awkward MCMC behavior. [CRAN](https://cran.r-project.org/web/packages/sspse/sspse.pdf)
        
    * **Multiple chains**: run with several `seed` values and verify the posterior summaries for $N$ agree across chains. `posteriorsize()` has a `seed` argument. [CRAN](https://cran.r-project.org/web/packages/sspse/sspse.pdf)
        
4. **What _not_ to rely on.**
    
    * **Thinning every 2 steps isn’t a fix.** A period-2 chain sampled at every 2nd step just locks you into one residue class. Better to _break_ periodicity (steps 2–3) than to thin. (This is the standard guidance for periodic Markov chains.) [randomservices.org](https://www.randomservices.org/random/markov/Periodicity.html?utm_source=chatgpt.com)
        
5. **A minimal “rectify and re-check” template.**
    
    ```r
    # 1) Optional: stochastic visibility imputation to add variability
    vis <- impute.visibility(rds, type.impute = "distribution", interval = 10, burnin = 5000)
    
    # 2) Refit with slightly larger proposal scales + longer burn-in
    fit <- posteriorsize(
      rds,
      visibility = TRUE,
      type.impute = "distribution",
      burnin = 20000, interval = 10, samplesize = 4000,
      mu_proposal = 0.2,  nu_proposal = 0.3,
      beta_0_proposal = 0.4, beta_t_proposal = 0.01, beta_u_proposal = 0.01,
      memmu_proposal = 0.2, memscale_proposal = 0.3,
      seed = 1
    )
    
    # 3) Diagnostics
    plot(fit, mcmc = TRUE)        # trace + ACF panels
    plot(pospreddeg(fit))         # posterior predictive p-values for degrees
    ```
    
6. **When to be concerned.**
    
    * If—even after the tweaks—the trace still alternates cleanly between two regimes, ESS stays very low, or your posterior summaries for $N$ differ meaningfully between even and odd iterations (or across seeds), then the sampler is not mixing well in your specification. Re-check the visibility distribution choice (`visibilitydistribution="cmp"/"nbinom"/"pln"`), priors on $N$ (try `priorsizedistribution="beta"` with a weaker `effective.prior.df`), and whether `include.tree`, `recruit.time`, or `unit.scale` should be toggled. See the argument list in the manual for details. [CRAN+2CRAN+2](https://cran.r-project.org/web/packages/sspse/sspse.pdf)
        

**Bottom line.** The message by itself isn’t fatal. If diagnostics look good after modest proposal-scale tuning and (optionally) stochastic visibility imputation, you can trust the posterior summaries that `posteriorsize()` reports (and still report the standard MCMC diagnostics for transparency). The package provides the knobs—proposal scales, burn-ins/intervals, visibility modeling, recruitment times—to break periodicity and restore comfortable mixing. [CRAN+2CRAN+2](https://cran.r-project.org/web/packages/sspse/sspse.pdf)

* * *

**You:**

Thank you.  How can I speed up the posteriorsize() command?  I have 128GB of RAM and 24 cores.

* * *

**ChatGPT:**

my best guess is

1. you can make `sspse::posteriorsize()` **much faster** by turning on its built-in parallelism (PSOCK or MPI),
    
2. by choosing **lighter visibility models** when scientifically acceptable (e.g., `visibilitydistribution="nbinom"` instead of the default CMP), and
    
3. by **tuning MCMC** so you get higher effective sample size (ESS) per second rather than just collecting more draws.
    

Here’s a concrete, step-by-step playbook for a 24-core / 128 GB machine.

* * *

1) Switch on built-in parallelism
---------------------------------

`posteriorsize()` has `parallel` and `parallel.type` arguments. Set `parallel = 24` (or cores-1) and use `"PSOCK"` unless you’ve configured MPI. This parallelizes the Monte Carlo sampling across workers. [rdrr](https://rdrr.io/cran/sspse/man/posteriorsize.html)

```r
library(sspse)

fit <- posteriorsize(
  rds,                                 # your rds.data.frame
  median.prior.size = 10000,           # example
  visibility = TRUE,                   # keep the visibility model if you need it
  parallel = 24,                       # use all 24 cores
  parallel.type = "PSOCK",             # portable default
  burnin = 15000, samplesize = 4000, interval = 5,
  verbose = FALSE
)
```

**Notes**

* **MPI option.** If you run on an HPC with MPI (and `Rmpi` installed), set `parallel.type="MPI"`. It can scale better on clusters but needs proper setup. [rdrr](https://rdrr.io/cran/sspse/man/posteriorsize.html)
    
* Don’t oversubscribe: set `parallel` to your **physical** cores (or cores−1) and keep other threaded libs (BLAS/OpenMP) modest to avoid contention.
    

* * *

2) Reduce computation **per** draw (when scientifically acceptable)
-------------------------------------------------------------------

These options change the model’s workload:

* **Use a lighter visibility distribution.** The default is `visibilitydistribution="cmp"` (Conway–Maxwell–Poisson), which is flexible but computationally heavier. If acceptable, try:
    
    * `visibilitydistribution="nbinom"` (fast, flexible),
        
    * or `visibilitydistribution="pln"` (Poisson–lognormal).  
        Set via `visibilitydistribution` in `posteriorsize()`. [rdrr](https://rdrr.io/cran/sspse/man/posteriorsize.html)
        

```r
fit_fast <- posteriorsize(
  rds,
  median.prior.size = 10000,
  visibility = TRUE,
  visibilitydistribution = "nbinom",   # << faster than CMP
  parallel = 24, parallel.type = "PSOCK",
  burnin = 12000, samplesize = 3000, interval = 5
)
```

* **Skip recruitment-time modeling (if not needed).** If you don’t want the time effects in the recruit model, set `recruit.time = NA` (and `recruit.time2 = NA` if using a second survey). That removes time from the log-intensity and saves work. [rdrr](https://rdrr.io/cran/sspse/man/posteriorsize.html)
    

```r
fit_notime <- posteriorsize(
  rds, median.prior.size = 10000,
  recruit.time = NA,                    # drop time effects
  visibility = TRUE, visibilitydistribution = "nbinom",
  parallel = 24, interval = 5, burnin = 12000, samplesize = 3000
)
```

* **If visibility modeling isn’t needed, turn it off.** This is a big speed boost, but only do it if you’re comfortable **not** modeling degree measurement error; you must then supply a _vector_ of network sizes in recruitment order (or pass an `rds.data.frame` but it will use reported degree as-is). `visibility = FALSE`. [rdrr](https://rdrr.io/cran/sspse/man/posteriorsize.html)
    

```r
# s = integer vector of degrees in sample order
fit_novis <- posteriorsize(
  s, median.prior.size = 10000,
  visibility = FALSE,                   # no latent visibility model
  parallel = 24, interval = 5, burnin = 8000, samplesize = 3000
)
```

* * *

3) Improve **ESS per second** (better tuning beats more draws)
--------------------------------------------------------------

* **Proposal scales.** Poor tuning inflates autocorrelation. Bump the proposal s.d.’s a bit (or down if acceptance is tiny) to target a moderate acceptance rate. The key knobs are:  
    `mu_proposal`, `nu_proposal`, `beta_0_proposal`, `beta_t_proposal`, `beta_u_proposal`, `memmu_proposal`, `memscale_proposal`. [rdrr](https://rdrr.io/cran/sspse/man/posteriorsize.html)
    

```r
fit_tuned <- posteriorsize(
  rds, median.prior.size = 10000,
  visibility = TRUE, visibilitydistribution = "nbinom",
  mu_proposal = 0.2,  nu_proposal = 0.3,
  beta_0_proposal = 0.4, beta_t_proposal = 0.01, beta_u_proposal = 0.01,
  memmu_proposal = 0.2, memscale_proposal = 0.3,
  parallel = 24, interval = 5, burnin = 15000, samplesize = 4000
)
```

* **`interval` vs speed.** `interval` is the number of proposals between saved draws. Larger `interval` **does not shorten runtime** for a fixed `samplesize`; it merely saves fewer draws to memory while still performing the proposals. For speed, **keep `interval` small** (e.g., 1–5) and instead raise parallelism and tune proposals. [rdrr](https://rdrr.io/cran/sspse/man/posteriorsize.html)
    
* **Right-size burn-in and samplesize.** Run a short pilot (e.g., 500–1000 saved draws), compute ESS for `N`, then scale `samplesize` to hit your target ESS. Don’t over-collect highly correlated draws.
    

* * *

4) Exploit your 24 cores with **multiple independent chains**
-------------------------------------------------------------

Even though `posteriorsize()` can parallelize internally, another robust pattern is **many shorter chains in parallel**, then combine if diagnostics agree.

```r
library(parallel)
seeds <- 1:24
fits <- mclapply(seeds, function(sd)
  posteriorsize(
    rds, median.prior.size = 10000,
    visibility = TRUE, visibilitydistribution = "nbinom",
    parallel = 1,                       # each worker runs 1 process
    seed = sd,
    burnin = 8000, samplesize = 1500, interval = 5
  ),
  mc.cores = 24
)

# combine N draws from all chains after checking convergence:
Ns <- do.call(c, lapply(fits, function(f) f$sample[,"N"]))
quantile(Ns, c(.025,.5,.975))  # posterior summary for N
```

(On Windows, use a PSOCK cluster via `parallel::makeCluster` and `parLapply` instead of `mclapply`.)

* * *

5) Fast vs accurate presets (you can mix)
-----------------------------------------

* **“Fast mode” (screening runs)**
    
    ```r
    visibility = FALSE            # or TRUE + "nbinom"
    visibilitydistribution = "nbinom"
    recruit.time = NA
    interval = 3–5
    parallel = 24
    burnin = 5000–8000
    samplesize = 1500–2500
    ```
    
    Use this to get a quick posterior sense of $N$.
    
* **“Accurate mode” (final runs)**
    
    ```r
    visibility = TRUE
    visibilitydistribution = "nbinom" or "pln" (CMP if needed)
    recruit.time = <your time variable>  # if informative
    interval = 5–10
    parallel = 24 (or MPI on HPC)
    burnin = 12000–20000
    samplesize = 3000–6000
    # proposal scales tuned from pilot for healthy acceptance & ESS
    ```
    

* * *

6) Quick diagnostic loop
------------------------

```r
fit <- posteriorsize(rds, median.prior.size=10000,
                     visibility=TRUE, visibilitydistribution="nbinom",
                     parallel=24, interval=5, burnin=15000, samplesize=4000)

plot(fit, mcmc = TRUE)       # trace/ACF/ESS panels
summary(fit)                  # posterior of N
plot(pospreddeg(fit))         # posterior predictive degree checks
```

All of these arguments/diagnostics are documented in the `sspse` reference for `posteriorsize()`, `plot.sspse()`, and related functions. [rdrr+1](https://rdrr.io/cran/sspse/man/posteriorsize.html)[RDocumentation](https://www.rdocumentation.org/packages/sspse/versions/1.1.0/topics/plot.sspse?utm_source=chatgpt.com)

* * *

7) Final checklist to maximize speed on your box
------------------------------------------------

* Turn on **`parallel=24`**, `parallel.type="PSOCK"` (or MPI if configured). [rdrr](https://rdrr.io/cran/sspse/man/posteriorsize.html)
    
* Prefer **`visibilitydistribution="nbinom"`** if scientifically acceptable. [rdrr](https://rdrr.io/cran/sspse/man/posteriorsize.html)
    
* Keep **`interval` small**; it controls saving, not compute. [rdrr](https://rdrr.io/cran/sspse/man/posteriorsize.html)
    
* **Tune proposals** for better ESS/s. [rdrr](https://rdrr.io/cran/sspse/man/posteriorsize.html)
    
* Consider **multiple chains in parallel**, then combine draws after checking convergence.
    
* If degree error modeling isn’t required, **`visibility=FALSE`** with a degree vector is fastest. [rdrr](https://rdrr.io/cran/sspse/man/posteriorsize.html)
    

If you share your current call (arguments and data shape), I can suggest an exact “fast” and “final” spec tailored to your study design.

* * *

**You:**

Thank you.  However, your code:
"""
fit_fast <- posteriorsize(
  rds,
  median.prior.size = 10000,
  visibility = TRUE,
  visibilitydistribution = "nbinom",   # << faster than CMP
  parallel = 24, parallel.type = "PSOCK",
  burnin = 12000, samplesize = 3000, interval = 5
)
"""
results in this error:
"""
Engaging warp drive using PSOCK ...
Error in checkForRemoteErrors(lapply(cl, recvResult)) : 
  12 nodes produced errors; first error: unused arguments (s2 = base::quote(NULL), rc = base::quote(NULL), mean.prior.visibility = base::quote(3.57917355606656), df.mean.prior.visibility = base::quote(1), sd.prior.visibility = base::quote(3.03603675405444), df.sd.prior.visibility = base::quote(3), beta_0.mean.prior = base::quote(-3), beta_t.mean.prior = base::quote(0), beta_u.mean.prior = base::quote(0), beta_0.sd.prior = base::quote(10), beta_t.sd.prior = base::quote(10), beta_u.sd.prior = base::quote(10), mem.optimism.prior = base::quote(0.865733926413355), df.mem.optimism.prior = base::quote(10001), mem.scale.prior = base::quote(2), df.mem.scale.prior = base::quote(10), mem.overdispersion = base::quote(15), mu_proposal = base::quote(0.1), nu_proposal = base::quote(0.15), beta_0_proposal = base::quote(0.2), beta_t_proposal = base::quote(0.001), beta_u_proposal = base::quote(0.001), memmu_proposal = base::quote(0.1), memscale_proposal = base::quote(0.15), visibility = base::quo
>
"""

* * *

**ChatGPT:**

my best guess is

1. the error means your worker R sessions are **not calling the same `sspse::posteriorsize()`** signature as your main session (they’re seeing a function that doesn’t accept those arguments), and
    
2. this almost always comes from **version/library-path mismatches on PSOCK workers** (or the workers not loading `sspse`/`RDS` correctly), rather than from your code per se.
    

Here’s a step-by-step way to **fix and speed up** your run.

* * *

1) Prove it’s a version / library mismatch
------------------------------------------

* In your main session:
    
    ```r
    packageVersion("sspse"); packageVersion("RDS")
    formals(sspse::posteriorsize)  # should list parallel, visibilitydistribution, proposal scales, etc.
    ```
    
    Those arguments are part of current `sspse` (see the CRAN manual’s Usage block). [CRAN+2CRAN+2](https://cran.r-project.org/web/packages/sspse/sspse.pdf)
    
* Now check a **fresh worker** has _the same_:
    
    ```r
    cl <- parallel::makeCluster(2)  # tiny test
    parallel::clusterEvalQ(cl, { library(sspse); library(RDS); 
                                 list( sspse=packageVersion("sspse"),
                                       RDS=packageVersion("RDS"),
                                       has=names(formals(sspse::posteriorsize))) })
    parallel::stopCluster(cl)
    ```
    
    If versions/arguments differ, that’s the cause of
    
    ```
    ... unused arguments (s2=..., rc=..., mean.prior.visibility=..., ...)
    ```
    

**If they differ:** update and standardize.

```r
remove.packages("sspse"); remove.packages("RDS")
install.packages(c("sspse","RDS"))  # same lib path used by workers
# restart R to clear old namespaces
```

Then re-run the little cluster check above. (The `posteriorsize()` signature in the manual includes `visibilitydistribution`, `parallel`, prior params, and proposal scales; the workers must see that same signature.) [CRAN+2CRAN+2](https://cran.r-project.org/web/packages/sspse/sspse.pdf)

* * *

2) Quick “works everywhere” solution: **outer parallel, inner serial**
----------------------------------------------------------------------

Even if you keep `sspse` parallel, the most robust setup is: **you** manage the cluster; each worker runs a **single-core** `posteriorsize()`.

```r
library(parallel)
library(sspse); library(RDS)

# 1) Make a PSOCK cluster with your 24 cores
cl <- makeCluster(24)

# 2) Ensure workers use the SAME libs and have packages loaded
clusterCall(cl, function(p) .libPaths(p), .libPaths())
clusterEvalQ(cl, { library(sspse); library(RDS) })

# 3) Export your data object
clusterExport(cl, c("rds"))  # rds is your rds.data.frame

# 4) Run 24 independent chains (inner parallel=1)
seeds <- 1:24
fits <- parLapply(cl, seeds, function(sd){
  set.seed(sd)
  sspse::posteriorsize(
    rds,
    median.prior.size = 10000,
    visibility = TRUE,
    visibilitydistribution = "nbinom",
    parallel = 1,                 # <<< inner serial avoids the PSOCK arg bug
    burnin = 12000, samplesize = 3000, interval = 5
  )
})

stopCluster(cl)

# 5) Combine draws for N after checking each fit’s diagnostics
Ns <- do.call(c, lapply(fits, function(f) f$sample[,"N"]))
quantile(Ns, c(.025,.5,.975))
```

This avoids any internal cluster setup inside `posteriorsize()` and sidesteps worker-signature issues while fully using your 24 cores.

* * *

3) If you prefer **built-in** parallel in `posteriorsize()`
-----------------------------------------------------------

After verifying identical versions on workers:

```r
fit <- sspse::posteriorsize(
  rds,
  median.prior.size = 10000,
  visibility = TRUE,
  visibilitydistribution = "nbinom",
  parallel = 24, parallel.type = "PSOCK",
  burnin = 12000, samplesize = 3000, interval = 5
)
```

If it still throws the “unused arguments” error, it means the internal PSOCK workers are still picking up a stale function body from a different lib path. In that case, stick with the **outer-parallel** pattern above (it’s also easier to debug and reproduce). The availability of these arguments and the built-in parallel knobs is documented in the CRAN manual. [CRAN](https://cran.r-project.org/web/packages/sspse/sspse.pdf)

* * *

4) Extra speed without changing science
---------------------------------------

These reduce compute **per draw** and boost **ESS/sec**:

* **Visibility model:** use `visibilitydistribution="nbinom"` (lighter than `cmp`) if scientifically acceptable. [CRAN](https://cran.r-project.org/web/packages/sspse/sspse.pdf)
    
* **Recruitment times:** if not informative, set `recruit.time = NA` to drop time effects. [CRAN](https://cran.r-project.org/web/packages/sspse/sspse.pdf)
    
* **Proposal tuning:** increase proposal s.d.’s slightly to get healthier acceptance and lower autocorrelation (see `mu_proposal`, `nu_proposal`, `beta_*_proposal`, etc.). [CRAN](https://cran.r-project.org/web/packages/sspse/sspse.pdf)
    
* **Keep `interval` small** (1–5). It controls how often you **save** draws, not how much compute is done; larger values don’t make runs faster. [CRAN](https://cran.r-project.org/web/packages/sspse/sspse.pdf)
    

* * *

5) Sanity checks after it runs
------------------------------

```r
summary(fit)            # posterior for N
plot(fit, mcmc=TRUE)    # trace + ACF + ESS panels
plot(pospreddeg(fit))   # posterior predictive degree checks
```

All are standard `sspse` diagnostics. [CRAN](https://cran.r-project.org/web/packages/sspse/sspse.pdf)[RDocumentation](https://www.rdocumentation.org/packages/sspse/versions/1.1.0/topics/plot.sspse?utm_source=chatgpt.com)

* * *

### One-liner takeaway

Make sure **workers load the same `sspse`** you’re using (identical version & args), or bypass the internal cluster and run **many single-core chains in your own PSOCK cluster**. Then tune proposals and use `visibilitydistribution="nbinom"` for big speed gains on your 24-core/128GB box. [CRAN+2CRAN+2](https://cran.r-project.org/web/packages/sspse/sspse.pdf)