# Applying Feehan & Salganik (2016) Equations to RDS Data for NSUM Estimation

## Theoretical Framework and Motivation

Traditional network scale-up method (NSUM) estimation relies on the basic scale-up estimator, which assumes random sampling from both frame and hidden populations (@feeh16-generalizing). However, when studying hidden populations, researchers often must use respondent-driven sampling (RDS), which produces samples through chain-referral mechanisms where individuals with larger networks have higher inclusion probabilities. The generalized network scale-up method developed by @feeh16-generalizing provides the theoretical framework needed to address this fundamental sampling challenge.

The core innovation lies in recognizing that RDS produces what @feeh16-generalizing term a "relative probability sample"—a sampling design where individual inclusion probabilities πᵢ are proportional to some known function (typically network degree) but the proportionality constant remains unknown. This framework allows for valid inference without requiring knowledge of the exact inclusion probabilities, only their relative magnitudes.

## Primary RDS Weighting Equations

### Core Visibility Estimator

The central equation for RDS weighting in NSUM estimation is Equation (7) from @feeh16-generalizing:

$$\hat{\bar{v}}_{H,F} = \frac{N_F}{N_{A \cap F}} \times \frac{\sum_{i \in s_H} \sum_j \tilde{v}_{i,A_j \cap F}/(c\pi_i)}{\sum_{i \in s_H} 1/(c\pi_i)}$$

where $\hat{\bar{v}}_{H,F}$ represents the estimated average number of in-reports (visibility) that members of the hidden population H receive from members of the frame population F. The RDS weights $w_i = 1/(c\pi_i)$ correct for the network-based sampling mechanism, where c is an unknown proportionality constant that cancels in the ratio estimator.

### Complete Generalized Scale-Up Estimator

This visibility estimate feeds into the generalized scale-up estimator (Equation 10 in @feeh16-generalizing):

$$\hat{N}_H = \frac{\hat{y}_{F,H}}{\hat{\bar{v}}_{H,F}}$$

where $\hat{y}_{F,H}$ is the estimated total number of out-reports from the frame population to the hidden population, obtained from a probability sample of the frame population using standard Horvitz-Thompson estimation.

## Mathematical Foundation for RDS Weighting

### Relative Probability Sample Theory

The theoretical foundation rests on Result C.1 in @feeh16-online, which establishes that for relative probability samples:

$$\hat{\bar{y}}_{H,A} = \frac{\sum_{i \in s_H} y_{i,A}/(c\pi_i)}{\sum_{i \in s_H} 1/(c\pi_i)}$$

is consistent and essentially unbiased for $\bar{y}_{H,A} = y_{H,A}/N_H$ under the relative probability sampling framework. This weighted sample mean estimator (also called a Hajek estimator) enables valid inference even when inclusion probabilities are known only up to a constant of proportionality.

### Connection to Network Degrees

In RDS applications, the inclusion probabilities are typically assumed to be proportional to network degree: $\pi_i \propto d_i$, where $d_i$ represents individual i's network size. This assumption is formalized in the RDS literature (Volz & Heckathorn, 2008) and means that RDS weights take the form $w_i = 1/(cd_i)$, where network degree serves as a proxy for inclusion probability.

## Required Conditions and Assumptions

### Critical Assumptions for Valid Estimation

The generalized scale-up estimator with RDS data requires several conditions to produce consistent and essentially unbiased estimates (@feeh16-generalizing, @feeh16-online):

1. **Relative Probability Sample Condition**: The RDS sample must satisfy the conditions for a relative probability sample, meaning all hidden population members have non-zero inclusion probability and these probabilities are known up to a proportionality constant.

2. **Probe Alter Condition**: The rate at which the hidden population is visible to the probe alters must equal the rate at which the hidden population is visible to the frame population:
   $$\frac{v_{H,A \cap F}}{N_{A \cap F}} = \frac{v_{H,F}}{N_F}$$

3. **Accurate Aggregate Reports about Visibility**: Hidden population members must provide accurate aggregate reports about their visibility to probe alters: $\tilde{v}_{H,A \cap F} = v_{H,A \cap F}$.

4. **No False Positive Reports**: Frame population members must not report connections to individuals who are not actually in the hidden population.

### Probe Alter Selection

The probe alter condition requires careful selection of groups A₁, A₂, ..., Aⱼ for visibility assessment. @feeh16-generalizing recommend choosing groups that are "typical of the frame population" in terms of their connections to the hidden population. For example, when estimating drug injector populations, postal workers would be preferable to drug treatment counselors as probe alters, since drug injectors are likely as visible to postal workers as to typical frame population members, whereas they would be disproportionately visible to treatment counselors.

## Practical Implementation Steps

### Step 1: RDS Data Collection and Weight Calculation

Collect RDS sample from hidden population and estimate inclusion probabilities. If assuming degree-proportional sampling, estimate network degrees $\hat{d}_i$ for each RDS respondent and construct weights $w_i = 1/\hat{d}_i$ (the proportionality constant c cancels in estimation).

### Step 2: Visibility Data Collection

Implement the "game of contacts" data collection procedure described in @feeh16-online (Section C.2). For each RDS respondent i and probe alter group Aⱼ, collect:
- Number of alters known in group Aⱼ: $y_{i,A_j}$
- Number of these alters who are aware of respondent's hidden population membership: $\tilde{v}_{i,A_j \cap F}$

### Step 3: Weighted Visibility Estimation

Apply RDS weights to estimate average visibility using Equation (7):
$$\hat{\bar{v}}_{H,F} = \frac{N_F}{N_{A \cap F}} \times \frac{\sum_{i \in s_H} \sum_j \tilde{v}_{i,A_j \cap F} \cdot w_i}{\sum_{i \in s_H} w_i}$$

### Step 4: Frame Population Estimation

Obtain probability sample from frame population and estimate total out-reports $\hat{y}_{F,H} = \sum_{i \in s_F} y_{i,H}/\pi_i$ using standard Horvitz-Thompson estimation.

### Step 5: Population Size Estimation

Combine estimates using the generalized scale-up estimator: $\hat{N}_H = \hat{y}_{F,H}/\hat{\bar{v}}_{H,F}$.

## Sensitivity Analysis and Diagnostics

### Sensitivity to Assumption Violations

@feeh16-online provides comprehensive sensitivity analysis framework (Appendix D) showing how violations of key assumptions affect estimates. For RDS applications, particular attention should be paid to:

- **Sampling weight accuracy**: Errors in degree estimation or violations of degree-proportional sampling assumption affect estimates through the bias factor $K_H = \text{cor}_H(\tilde{v}_{i,A \cap F}, \epsilon_i) \text{cv}_H(\tilde{v}_{i,A \cap F}) \text{cv}_H(\epsilon_i)$, where $\epsilon_i$ represents weight error.

- **Probe alter condition violations**: If probe alters are not representative of the frame population, estimates will be biased by factor $c_3$ where $v_{H,A \cap F}/N_{A \cap F} = c_3 \cdot v_{H,F}/N_F$.

### Empirical Diagnostic Checks

Result C.4 in @feeh16-online provides an empirical test for the probe alter condition. If precision of out-reports is similar between frame population and probe alters, then the condition is satisfied if and only if $\bar{y}_{F,H} = \bar{y}_{A \cap F,H}$, providing a testable implication.

## Advantages of the RDS Weighting Approach

The generalized scale-up method with RDS weighting provides several methodological advantages:

1. **Addresses selection bias**: Corrects for the fundamental bias that individuals with larger networks are more likely to be sampled in RDS chains.

2. **Relaxes strong assumptions**: Does not require random mixing, perfect awareness, or simple random sampling assumptions of basic scale-up methods.

3. **Accommodates complex sampling**: Handles the chain-referral dependencies and clustering inherent in RDS designs through the relative probability sampling framework.

4. **Provides sensitivity analysis**: Offers quantitative tools for assessing how assumption violations affect estimates, enabling researchers to evaluate robustness.

5. **Maintains theoretical rigor**: Builds on established survey sampling theory (Hajek estimation) while adapting to the unique challenges of hidden population research.

This framework thus enables researchers to conduct theoretically grounded NSUM estimation even when practical constraints require network-based sampling methods like RDS for accessing hidden populations.