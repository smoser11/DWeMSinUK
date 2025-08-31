
## 📘 Methods: RDS Estimator Selection and Justification

Respondent-Driven Sampling (RDS) was implemented to sample a hard-to-reach population of migrant domestic workers in Lebanon, many of whom are at risk of labour exploitation and live in socially isolated settings. Given the importance of correcting for non-random recruitment and differential network visibility, we evaluated two classes of RDS estimators: **design-based** and **model-based**. Design-based methods, including the Successive Sampling Population Size Estimator (SSPS; Gile, 2011), use observed sample characteristics such as self-reported degree and recruitment wave to approximate inclusion probabilities, assuming that earlier recruits tend to have higher degrees. These estimators are particularly suited for contexts with limited auxiliary data and uncertain network topology.

In contrast, model-based estimators such as the network model-assisted estimators (Gile & Handcock, 2015) rely on explicit modeling of the underlying social network using tools like Exponential Random Graph Models (ERGMs). While powerful, these approaches require strong assumptions about homophily, recruitment independence, and known degree distributions, which may not hold in settings where domestic workers often live in employer households with limited peer contact or tightly clustered networks.

Based on diagnostic indicators from the recruitment chains—such as observed bottlenecks, short maximum wave depth (6 waves), and moderate homophily based on legal status—we determined that **design-based estimation was more appropriate** for our study. Specifically, we used the SSPS estimator, which adjusts for degree-based sampling bias without relying on priors or simulations. This approach provided a transparent and empirically grounded method suited to the challenges of RDS implementation in hidden, structurally isolated populations.

---

**References**  
- Gile, K. J. (2011). Improved inference for respondent-driven sampling data with application to HIV prevalence estimation. *Journal of the American Statistical Association*, 106(493), 135–146. https://doi.org/10.1198/jasa.2011.ap09475  
- Gile, K. J., & Handcock, M. S. (2015). Network model-assisted inference from respondent-driven sampling data. *Journal of the Royal Statistical Society: Series A*, 178(3), 619–639. https://doi.org/10.1111/rssa.12091
