
## 📘 Overview and Contrast: Network Scale-Up Methods (NSUM) vs Respondent-Driven Sampling (RDS)

Network Scale-Up Methods (NSUM) and Respondent-Driven Sampling (RDS) represent two complementary approaches for estimating characteristics or sizes of hidden populations. While both rely on social networks, they differ fundamentally in **whose networks** are measured and **how** those data are collected and modeled.

---

### 🔹 NSUM: Characteristics of Alters in Respondents’ Networks

NSUM draws on data from a **general population sample** by asking individuals how many people they know in a hidden or stigmatized group (e.g., “How many people do you know who are undocumented migrants?”). This approach assumes that people can **accurately report** about their social ties and that those ties are a **random sample** of the population. The estimates scale up from these reports based on the respondent’s total social network size and the size of the general population.

- **Core assumption**: Respondents have visibility into their alters’ hidden characteristics and networks are representative.
- **Main limitation**: Social and structural **barriers** (e.g. stigma, segregation) distort reporting and visibility—leading to underestimation, known as **transmission bias**.
- **Recent advancements**: Generalized and Bayesian NSUM models attempt to correct for visibility and recall bias using statistical adjustment or prior-informed models [@hand14-estimating; @hand15-estimating; @kim21-population; @mcla24-modeling].

---

### 🔹 RDS: Ego-based Information Through Chain-Referral Recruitment

In contrast, RDS gathers data directly from members of the **hidden population** through chain-referral (peer) recruitment. Participants report their own characteristics and may also be asked about how many peers they could have recruited. The design is structured to allow for **probability-based inference**, assuming sufficient waves of recruitment, accurate reporting of personal network size (degree), and quasi-random peer recruitment.

- **Core assumption**: Individuals’ personal networks (egos) can be used to infer sampling probability and adjust for visibility bias.
- **Main limitation**: Recruitment is often non-random, and personal network size is self-reported, which can be inaccurate or biased.
- **Advanced methods**: Successive sampling estimators (@gile11-improved) and network model-assisted estimators (@gile15-network) adjust for bias due to recruitment patterns and seed selection.

---

### 🔄 Key Distinction: Network Direction

| Dimension             | NSUM                                       | RDS                                        |
|----------------------|--------------------------------------------|---------------------------------------------|
| **Network Perspective** | Alters of general population respondents   | Egos from hidden population                 |
| **Who is surveyed?**   | General population                        | Hidden population                           |
| **Recruitment method** | None (random sample)                     | Chain-referral (peer recruitment)          |
| **Main data unit**     | “How many do you know who…” (alter reports) | “How many peers do you know?” (ego degree) |
| **Common biases**      | Visibility, barrier effects               | Degree misreporting, seed bias              |
| **Best use case**      | When hidden group cannot be sampled directly | When some hidden group members are reachable |

---

Both methods are crucial tools in studying marginalized populations. NSUM is best when the hidden population is inaccessible directly, and when general population respondents can provide proxy insight. RDS, by contrast, is more suitable when at least a portion of the hidden population can be reached via peer recruitment. In practice, combining insights from both methods—especially in triangulation studies—can yield stronger, more reliable population estimates.

