
🧠 Neighborhood Bootstrap for Respondent-Driven Sampling (RDS)
==============================================================

**Yauck et al. (2022)** | [https://doi.org/10.1093/jssam/smab057](https://doi.org/10.1093/jssam/smab057)

* * *

🎯 Objective
------------

The **Neighborhood Bootstrap** estimates **sampling uncertainty** (variance, standard errors, CIs) in **RDS-II (Volz-Heckathorn) estimators**, by mimicking the **local recruitment structure** in the observed RDS network.

Compared to the Tree Bootstrap:

* It focuses on **recruiter + recruit "neighborhoods"**
    
* Avoids overestimating variance (which the Tree Bootstrap is prone to)
    
* Yields better performance in moderate-to-large sample sizes
    

* * *

🌐 Key Concepts
---------------

* The RDS sample forms a **recruitment tree** $G_T = (V_T, E_T)$, where:
    
    * $V_T$: sampled individuals
        
    * $E_T$: directed edges (who recruited whom)
        
* Each **recruiter** $r_i \in V_T$ has a set of **direct recruits**.
    
* These **local structures** are called **neighborhoods**.
    

* * *

✅ Step-by-Step Procedure
------------------------

We will include a **fully worked example** after the steps.

* * *

### **Step 1: Identify Recruiters and Their Neighborhoods**

Let:

* $R = \{r_1, r_2, ..., r_k\}$: the set of **recruiters** (nodes who recruited ≥1 others)
    
* Each recruiter $r_i$ has a **neighborhood**:
    

$$\mathcal{N}(r_i) = \{r_i\} \cup \text{Recruits of } r_i$$

* * *

### **Step 2: Resample Recruiters with Replacement**

* Sample $k$ recruiters (equal to the number of recruiters in the observed sample), **with replacement** from $R$.
    
* Duplicates are allowed.
    

* * *

### **Step 3: Build the Bootstrap Sample**

* For each sampled recruiter $r_i$, add **$\mathcal{N}(r_i)$** to the bootstrap sample.
    
* If a participant appears **multiple times** (from different neighborhoods), include **all appearances**.
    
    * This is akin to sampling with replacement at the unit level.
        

* * *

### **Step 4: Compute the Statistic**

* Compute the **Volz-Heckathorn estimator** on the full bootstrap sample:
    

$$\hat{\mu}_{VH}^{(b)} = \frac{\sum_{i \in \text{sample}^{(b)}} \frac{Z_i}{d_i}}{\sum_{i \in \text{sample}^{(b)}} \frac{1}{d_i}}$$

* $Z_i$: attribute of interest (e.g., HIV status)
    
* $d_i$: reported network degree of individual $i$
    

* * *

### **Step 5: Repeat**

* Repeat steps 2–4 **B times** (e.g., B = 1,000) to generate a distribution of $\hat{\mu}_{VH}^{(b)}$
    

* * *

### **Step 6: Estimate Uncertainty**

* **Standard Error**: Standard deviation of the $\hat{\mu}_{VH}^{(b)}$ values
    
* **95% Confidence Interval**: Percentile method:
    

$$CI_{95\%} = \left[\text{2.5th percentile},\ \text{97.5th percentile} \right] \text{ of } \left\{ \hat{\mu}_{VH}^{(1)}, ..., \hat{\mu}_{VH}^{(B)} \right\}$$

* * *

🧪 Fully Worked Example
-----------------------

### 📊 Observed RDS Sample

| ID | Recruited By | Degree $d_i$ | Attribute $Z_i$ |
| --- | --- | --- | --- |
| A | Seed | 6 | 1 |
| B | A | 3 | 1 |
| C | A | 4 | 0 |
| D | B | 2 | 1 |
| E | B | 3 | 0 |
| F | C | 2 | 1 |

### Identify Recruiters:

* A (recruited B, C) → $\mathcal{N}(A) = \{A, B, C\}$
    
* B (recruited D, E) → $\mathcal{N}(B) = \{B, D, E\}$
    
* C (recruited F) → $\mathcal{N}(C) = \{C, F\}$
    

So:

* Recruiters: A, B, C (total = 3)
    
* $$R = \{A, B, C\}$$
    

* * *

### 🔁 One Bootstrap Iteration

#### Step 2: Resample recruiters

Suppose we sample: {B, A, C}

#### Step 3: Collect neighborhoods:

* $$\mathcal{N}(B) = \{B, D, E\}$$
    
* $$\mathcal{N}(A) = \{A, B, C\}$$
    
* $$\mathcal{N}(C) = \{C, F\}$$
    

Bootstrap sample:

* A, B (twice), C (twice), D, E, F  
    → Total of 8 entries, including duplicates.
    

* * *

### Step 4: Compute the VH Estimator

Let’s list out the sample:

| ID | $Z_i$ | $d_i$ | $Z_i / d_i$ | $1 / d_i$ |
| --- | --- | --- | --- | --- |
| A | 1 | 6 | 0.167 | 0.167 |
| B | 1 | 3 | 0.333 | 0.333 |
| B | 1 | 3 | 0.333 | 0.333 |
| C | 0 | 4 | 0.000 | 0.250 |
| C | 0 | 4 | 0.000 | 0.250 |
| D | 1 | 2 | 0.500 | 0.500 |
| E | 0 | 3 | 0.000 | 0.333 |
| F | 1 | 2 | 0.500 | 0.500 |

$$\hat{\mu}_{VH}^{(b)} = \frac{0.167 + 0.333 + 0.333 + 0 + 0 + 0.5 + 0 + 0.5}{0.167 + 0.333 + 0.333 + 0.25 + 0.25 + 0.5 + 0.333 + 0.5} = \frac{1.833}{2.666} \approx 0.688$$

* * *

Repeat this **B = 1,000 times**, then compute:

* Mean
    
* Standard deviation
    
* 2.5th and 97.5th percentiles
    

* * *

💡 Why This Method Works
------------------------

* Mimics **local dependencies**: Each bootstrap sample retains the real-world dependence between recruiter and recruit.
    
* Avoids relying on **attribute transitions** (as in Salganik’s method).
    
* Less sensitive to outlier paths or long chains (as in Tree Bootstrap).
    
* Proven to be **consistent** under mild assumptions.
    

* * *

🧪 Simulation Results from the Paper
------------------------------------

### 📈 Accuracy Compared to Tree Bootstrap:

* **Tree Bootstrap**:
    
    * Overestimates variance
        
    * CI coverage often **> 97%** → **Too wide**
        
* **Neighborhood Bootstrap**:
    
    * Slightly underestimates variance for small samples
        
    * CI coverage improves as $n \to 1000$
        
    * Interval widths are much **closer to expected**
        

* * *

📦 R Package: `Neighboot`
-------------------------

Install:

```r
install.packages("Neighboot")
```

Key functions:

* `nb_resample()`
    
* `nb_var()`
    
* `nb_ci()`
    

👉 CRAN: [https://CRAN.R-project.org/package=Neighboot](https://CRAN.R-project.org/package=Neighboot)

* * *

✅ Summary Table
---------------

| Step | Description |
| --- | --- |
| 1 | Identify recruiters and their recruits |
| 2 | Sample recruiters with replacement |
| 3 | Include sampled recruiters + their recruits |
| 4 | Compute VH estimator |
| 5 | Repeat to build bootstrap distribution |
| 6 | Compute SEs and CIs |
