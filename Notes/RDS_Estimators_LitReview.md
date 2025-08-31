
## 📘 Overview of RDS Estimators Used

This study evaluates multiple respondent-driven sampling (RDS) estimators to derive robust population estimates from hidden and hard-to-reach populations. Below is a summary of the principal models used in our analysis—**RDS-I**, **RDS-II**, and Bayesian and model-assisted estimators implemented in the R functions `MA.estimates()` and `posteriorsize()`—along with their foundational assumptions and limitations.

---

### 🔹 RDS-I Estimator (Volz-Heckathorn Estimator)

The **RDS-I** or **VH estimator** assumes that sampling reaches **equilibrium**, and that each recruit selects peers **uniformly at random** from their social network. It calculates the population proportion of a trait by weighting inversely by self-reported network size:

\`\`\`
P̂ = Σ (y_i / d_i) / Σ (1 / d_i)
\`\`\`

- **Assumptions**:
  - Accurate self-reported degree
  - Random recruitment from alters
  - Long recruitment chains (stationarity)
- **Limitations**:
  - Biased in the presence of **differential activity** (degree correlates with the trait)
  - Sensitive to **bottlenecks** or **homophily**

---

### 🔹 RDS-II Estimator (Gile’s Successive Sampling)

The **RDS-II** or **SS estimator** accounts for **finite population correction** using a **successive sampling approximation**. This makes it more suitable when the sample fraction is large (e.g., >10% of the population).

- **Assumptions**:
  - Degree-proportional recruitment
  - A known or well-estimated total population size
- **Limitations**:
  - Requires an estimate of total population size
  - Sensitive to degree misreporting and assumptions of uniform recruitment
- **Implementation**:
  - Available in RDS packages under SS or Gile-type estimators
  - See @gile11-improved for original method

---

### 🔹 Model-Assisted Estimator (`MA.estimates()`)

The **model-assisted estimator** uses **exponential-family random graph models (ERGMs)** to simulate the underlying social network and recruitment process. Implemented in `MA.estimates()`, this approach corrects for **seed bias**, **homophily**, and **differential recruitment probabilities**.

- **Assumptions**:
  - Correct specification of the network model
  - Access to observed degrees and recruitment links
  - Homophily and differential activity estimable from sample
- **Limitations**:
  - Requires significant computation (network simulations)
  - Model misspecification can bias estimates
- **Key References**: @gile15-network

---

### 🔹 Bayesian Population Size Estimation (`posteriorsize()`)

The `posteriorsize()` function implements a **Bayesian model for hidden population size estimation**, combining recruitment order data with a prior on the population size. It models inclusion probabilities using a **successive sampling framework**, estimating the posterior distribution of the hidden population size.

- **Assumptions**:
  - Recruitment probability related to degree
  - Accurate degree and wave information
  - Prior on population size is appropriately chosen
- **Limitations**:
  - Sensitive to prior specification
  - Performs poorly under recruitment biases
  - Does not estimate traits—focus is on **population size only**
- **Key References**:
  - @hand14-estimating
  - @hand15-estimating
  - @kim21-population
  - @mcla24-modeling

---

### ✅ Summary Table

| Estimator        | Model Type      | Adjusts for Homophily? | Requires Population Size | Handles Seed Bias | Main Use |
|------------------|-----------------|-------------------------|---------------------------|--------------------|----------|
| RDS-I (VH)       | Design-Based    | ❌ No                   | ❌ No                     | ❌ No              | Traits   |
| RDS-II (SS)      | Design-Based    | ❌ No                   | ✅ Yes                    | ❌ No              | Traits   |
| `MA.estimates()` | Model-Assisted  | ✅ Yes                  | ❌ No                     | ✅ Yes             | Traits   |
| `posteriorsize()`| Bayesian        | ⚠️ Partial             | ✅ Yes (via Prior)        | ✅ Yes             | Size     |

---

### 📚 References

- @gile11-improved: Gile, K. J. (2011). Improved inference for respondent-driven sampling data with application to HIV prevalence estimation. *Journal of the American Statistical Association*, 106(493), 135–146. https://doi.org/10.1198/jasa.2011.ap09475  
- @gile15-network: Gile, K. J., & Handcock, M. S. (2015). Network model-assisted inference from respondent-driven sampling data. *Journal of the Royal Statistical Society: Series A*, 178(3), 619–639. https://doi.org/10.1111/rssa.12091  
- @hand14-estimating: Handcock, M. S., Gile, K. J., & Mar, C. M. (2014). Estimating hidden population size using respondent-driven sampling data. *Electronic Journal of Statistics*, 8(1), 1491–1521. https://doi.org/10.1214/14-EJS923  
- @hand15-estimating: Handcock, M. S., Gile, K. J., & Mar, C. M. (2015). Estimating the size of populations at high risk for HIV using respondent-driven sampling data. *Biometrics*, 71(1), 258–266. https://doi.org/10.1111/biom.12255  
- @kim21-population: Kim, B. J., & Handcock, M. S. (2021). Population size estimation using multiple respondent-driven sampling surveys. *Journal of Survey Statistics and Methodology*, 9(1), 94–120. https://doi.org/10.1093/jssam/smy020  
- @mcla24-modeling: McLaughlin, K. R., et al. (2024). Modeling the visibility distribution for respondent-driven sampling with application to population size estimation. *Annals of Applied Statistics*, 18(1), 683–703. https://doi.org/10.1214/23-AOAS1777  

---
