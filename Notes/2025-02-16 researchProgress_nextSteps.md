**Revised & Improved Research Progress Report & Next Steps**
============================================================

This document integrates the **Feb 7, 2025, meeting**, emails, and research drafts to provide a **detailed research status update** and an **explicit, step-by-step action plan**.

* * *

**1. Current State of the Project**
-----------------------------------

### **1.1 Research Completed**

✅ **Survey Data Collection & Cleaning**

* **Frame Population Defined**: Domestic workers with mobile phones in the UK.
* **Referral Data Issues Resolved**: Addressed inconsistencies in **Q105–Q115 network size questions**.
* **Missing Data Treatment**: Implemented **imputation for missing degree sizes**.

✅ **Preliminary RDS-Based Estimation**

* **Estimated Exploitation Rate**: ~**30–40% of domestic workers** report some form of exploitation.
* **Severe Cases (Modern Slavery)**: **~12% show strong indicators of modern slavery**.
* **Modeling Robustness**: Tested **VH, SSPS, and Successive Sampling estimators**.

✅ **Clarified Probe Alter Groups for NSUM**

* **NRM referrals CANNOT be used** due to **selection biases** (CE noted inconsistencies in self-reported referral rates).
* **Alternative: Use "number of known DWs per respondent" as the probe alter group**.
* **Sensitivity Analysis Required**: Must estimate effects of different assumptions on **DW population size**.

✅ **Preliminary Draft of the Research Paper**

* **Literature review drafted**, but **missing critical discussion on social network estimation trade-offs**.
* **Methodology section requires clarification of weighting strategies**.

* * *

### **1.2 Open Issues and Unresolved Questions**

🔲 **Finalizing RDS Model Selection**

* **Comparison Criteria:**
    
    1. **Bias-variance tradeoff:** Which model minimizes **variance** without introducing excessive **bias**?
    2. **Sensitivity to degree distribution assumptions:** Test how each estimator performs under different assumptions about the **degree distribution**.
    3. **Coverage of confidence intervals:** Ensure CIs are **not excessively wide**, reducing statistical uncertainty.
    4. **Consistency with external benchmarks:** Compare RDS estimates to external **labour market data** for plausibility.
* **Decisions Needed:**
    
    * **Which RDS estimator to use in the main results?**
    * **What alternative estimators should be included in the appendix?**
    * **What sensitivity checks should be run to confirm robustness?**

🔲 **Bootstrap Confidence Intervals for Social Network Data**

* **Key Coding Issues:**
    
    * `Neighboot` package **does not work**.
    * Tree-based bootstrap **needs implementation**.
* **Proposed Solutions:**
    
    1. **Jackknife resampling**:
        * Compute CIs by iteratively **removing one respondent at a time**.
        * Compare **CI width** across RDS models.
    2. **Bayesian posterior intervals for SN measures**:
        * Use **MCMC sampling to estimate uncertainty** in referral-based networks.
    3. **Custom bootstrap implementation**:
        * Resample network edges **while preserving degree distribution**.

🔲 **Comparing NSUM and RDS Estimates**

* **Key Questions:**
    
    * **How should NSUM be weighted to match RDS assumptions?**
    * **Can RDS referral patterns inform NSUM adjustments?**
* **Next Steps:**
    
    * **Apply Feehan & Salganik (2016) Equation 7** to re-weight NSUM estimates.
    * Test NSUM estimates **before and after applying RDS weights**.
    * Conduct **Monte Carlo simulations** to test robustness.

🔲 **Deciding on Key Results and Presentation**

* **Target journal requires:**
    
    * **Empirical rigor**
    * **Methodological innovation**
    * **Worker-centered insights**
* **Proposed Tables & Figures:**
    
    * **Table 1**: RDS estimates using multiple models.
    * **Table 2**: NSUM estimates with and without RDS weights.
    * **Figure 1**: Social network expansion of DWs.
    * **Figure 2**: Sensitivity analysis for probe alter group choices.

🔲 **Finalizing the Paper**

* **What’s Missing?**
    * **Data limitations**: Referral bias, degree truncation issues.
    * **Comparability section**: Why RDS and NSUM estimates may differ.
    * **Policy implications**: Practical takeaways for labour enforcement.

* * *

**2. Prioritized Next Steps and Tasks**
---------------------------------------

### **🔴 Urgent (Immediate Priority)**

1. **Finalize RDS Model Selection** (**3–5 days**)
    
    * Run **VH, SSPS, and Bayesian RDS models**.
    * Select **preferred estimator** based on:
        * **Bias-variance tradeoff**
        * **Degree sensitivity**
        * **External benchmark plausibility**
    * Compute **alternative estimates for robustness**.
2. **Decide on Probe Alter Groups for NSUM** (**2–3 days**)
    
    * Implement **"known DWs per respondent"** as the probe alter group.
    * Run **sensitivity analysis** for different assumed DW population sizes.
3. **Implement Bootstrap Confidence Intervals** (**5–7 days**)
    
    * Code **tree-based and jackknife bootstraps**.
    * Compare **Bayesian vs. frequentist intervals**.

* * *

### **🟡 Medium Priority (Next 2–3 Weeks)**

4. **Run NSUM Estimations and Compare to RDS** (**1–2 weeks**)
    
    * Implement **Feehan & Salganik weighting adjustments**.
    * Conduct **Monte Carlo simulations** to assess robustness.
5. **Refine Methodology & Literature Review Sections** (**1 week**)
    
    * Expand **social network estimation discussion**.
    * Strengthen conceptual justification for **risk exposure model**.
6. **Generate Graphs and Tables for Results** (**4–5 days**)
    
    * **Side-by-side visualizations of RDS and NSUM estimates**.

* * *

### **🟢 Long-Term Priority (Final Draft in ~6 Weeks)**

7. **Write Final Paper Sections** (**3–4 weeks**)
    
    * Integrate all **model results, robustness checks, and comparative analyses**.
8. **Prepare for Journal Submission** (**Target: Late June 2025**)
    
    * Ensure **manuscript is polished and formatted correctly**.

* * *

**Final Thoughts**
------------------

This **improved plan** provides more **precise action steps**, including:

* **Explicit decision criteria for RDS selection.**
* **A coding roadmap for bootstrap estimation.**
* **A structured approach to NSUM-RDS comparison.**