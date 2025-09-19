
# Bootstrap-Based Uncertainty Estimation for NSUM with RDS Samples


1. Introduction
---------------

The **Network Scale-Up Method (NSUM)** is a powerful tool for estimating the size of hidden populations. By asking respondents about the number of people they know who belong to a hidden group, and calibrating by their social network size, we can estimate the total size of that group within a known frame population.

However, NSUM typically assumes a **simple random sample** of the frame population. In practice, researchers often rely on **Respondent-Driven Sampling (RDS)** to access hard-to-reach populations. RDS is a non-probability sampling method based on peer referral chains, and it introduces significant complexity due to:

* Unknown inclusion probabilities,
    
* Dependencies in the recruitment process,
    
* Homophily on hidden traits,
    
* Non-uniform degree distributions.
    

To adapt NSUM for use with RDS data, we must **adjust for the non-uniform sampling process**. This requires estimating each respondent’s **inclusion probability** $\pi_i$, as outlined in @feeh16-generaling and earlier in @salf06-variance. Moreover, because these inclusion probabilities vary across bootstrap samples, we must **recompute weights** for each resample.

We propose a three-step bootstrap method for estimating uncertainty in NSUM from RDS samples. This approach is modular, flexible, and compatible with multiple estimators.

* * *

2. The Three-Step Bootstrap Procedure
-------------------------------------

### Step 1: Resampling the RDS Sample

RDS is network-dependent and violates IID assumptions. Therefore, resampling must preserve recruitment structure, seed variation, and referral chains.

We consider four strategies:

1. **Tree Bootstrap**  
    Entire recruitment trees rooted in seeds are resampled with replacement. Each tree $T_j$ consists of all respondents traced to seed $j$. This preserves hierarchical dependencies and captures between-tree heterogeneity (@salg06-variance).
    
2. **Successive Sampling Bootstrap**  
    Mimics the RDS process as a form of successive sampling from a finite population. Each respondent is selected without replacement, with probabilities proportional to degree $d_i$. Requires an assumed frame population size $N$. Implemented in `RDS::gile.ss.weights()` (@gile11-inference).
    
3. **Neighborhood Bootstrap**  
    Introduced by @yauc22-neighboot, this method resamples by selecting respondents and replacing them with their **neighbors** in the recruitment graph. This maintains local network dependencies and simulates resampling from the underlying contact network.
    
4. **Chain Bootstrap**  
    Each bootstrap replicate samples chains or subchains with replacement, preserving recruiter–recruitee links. This is implemented in `surveybootstrap::rds.boot.draw.chain()` and used in studies like @weir12-comparison.
    

Each resample produces a new dataset $\mathcal{S}^{(b)}$, which is passed to Step 2.

* * *

### Step 2: Recalculate Inclusion Probabilities

RDS produces samples with **unknown and unequal probabilities of inclusion**, which must be corrected when used in NSUM estimation. This is achieved by estimating the probability $\pi_i^{(b)}$ that each individual $i$ in bootstrap replicate $b$ is included in the sample, conditional on their degree and position in the recruitment tree.

These probabilities are used to generate **sampling weights** $w_i^{(b)} = 1/\pi_i^{(b)}$, which are passed into NSUM estimators.

#### Estimation Methods

**(a) RDS-II (Volz-Heckathorn) Weights**  
Assumes the probability of selection is proportional to the respondent's degree:

$$\pi_i \propto d_i \quad \Rightarrow \quad w_i = \frac{1}{d_i}$$

These weights are normalized post hoc. This method is simple but does not account for homophily or finite population correction (@volz08-rds).

**(b) Gile’s Successive Sampling (SS) Weights**  
This method assumes RDS approximates successive sampling without replacement. The inclusion probabilities are computed by simulating from a known or assumed population size $N$. This method is implemented in `RDS::gile.ss.weights()` and adjusts for:

* Finite population effects,
    
* Degree-based sampling,
    
* Sample depletion over waves.
    

**(c) User-Defined or Model-Based Weights**  
Researchers may define weights using alternative models or Bayesian simulations. This includes post-stratification or fitting full generative models of the RDS process.

#### Software Example (R)

```r
library(RDS)
rd <- as.rds.data.frame(boot_sample, id = "id", recruiter.id = "recruiter.id")
boot_sample$ss_weight <- gile.ss.weights(rd, N = 980000)
boot_sample$vh_weight <- rds.weights(rd, weight.type = "RDS-II")
```

The output is a new dataset with respondent traits, degrees, and updated $\pi_i^{(b)}$, which are used in Step 3.

* * *

### Step 3: NSUM Estimation on Resampled and Reweighted Data

Given a bootstrap sample $\mathcal{S}^{(b)}$, we estimate the size of the hidden population $N_H$ using one of several NSUM estimators. All estimators rely on weighted sums of out-reports and degree.

#### (a) Generalized NSUM (GNSUM)

As described in @feeh16-generaling, GNSUM estimates:

$$\hat{N}_H^{(b)} = \left( \frac{\sum_{i} \frac{y_{i,H}}{\pi_i^{(b)}}}{\sum_{i} \frac{d_i}{\pi_i^{(b)}}} \right) \cdot N_F$$

Where:

* $y_{i,H}$: number of known alters in the hidden population,
    
* $d_i$: degree (known network size, e.g. from Q13),
    
* $N_F$: size of the frame population (e.g. domestic workers in UK).
    

#### (b) GNSUM with Symmetric Visibility (for Hidden Members in RDS)

In your context, some RDS respondents are ex post identified as members of the hidden population. Under the **symmetric visibility assumption**, if $i \in H$, we assume the probability that $i$ knows $j$ is the same as $j$ knows $i$. This allows **in-reports** to be incorporated.

Let:

* $I_H(i) = 1$ if $i \in H$, 0 otherwise
    
* $y_{i}^{\text{in}}$: number of other hidden members who report knowing $i$
    

Then the estimator becomes:

$$\hat{N}_H^{(b)} = \left( \frac{\sum_{i} \frac{y_{i,H} + I_H(i) \cdot y_{i}^{\text{in}}}{\pi_i^{(b)}}}{\sum_{i} \frac{d_i}{\pi_i^{(b)}}} \right) \cdot N_F$$

#### (c) Modified Basic Scale-Up (MBSU)

This estimator adjusts for key reporting biases via three correction factors:

$$\hat{N}_H^{(b)} = \left( \frac{\sum_{i} \frac{y_{i,H}}{\pi_i^{(b)}}}{\sum_{i} \frac{d_i}{\pi_i^{(b)}}} \right) \cdot N_F \cdot \frac{1}{\delta \cdot \tau \cdot \rho}$$

Where:

* $\delta$: **Transmission bias** (probability respondent is aware of alter’s status),
    
* $\tau$: **Barrier effect** (social mixing between hidden and frame population),
    
* $\rho$: **Popularity bias** (relative visibility of hidden population members).
    

These values can be:

* Estimated using **known subpopulations** (e.g., alter groups with known size),
    
* Set by expert **elicitation**,
    
* Scanned in **sensitivity analysis**.
    

#### (d) Model-Based NSUM

Bayesian implementations (e.g. @malt15-estimating) model:

* Degree distributions,
    
* Reporting error,
    
* Group visibility,
    
* Hidden population size.
    

They yield a **posterior distribution** over $N_H$, and uncertainty is embedded within the model.

Software:

* `NSUMBayes` (in `R`)
    
* Custom MCMC in `stan` or `JAGS`
    

* * *

### 4. Aggregating Bootstrap Estimates

After $B$ replicates:

$$\hat{N}_H^{(1)}, \dots, \hat{N}_H^{(B)}$$

We compute:

* **Point estimate**: $\bar{N}_H = \frac{1}{B} \sum_b \hat{N}_H^{(b)}$
    
* **Standard error**: sample SD
    
* **95% CI**: empirical percentiles (e.g., 2.5%, 97.5%)
    

* * *

### 5. References

* @feeh16-generaling
    
* @salf06-variance
    
* @gile11-inference
    
* @volz08-rds
    
* @malt15-estimating
    
* @yauc22-neighboot
    
* @weir12-comparison
    
* @salg06-variance
    
* @rust96-rescaled
    
* @rao88-resampling
    