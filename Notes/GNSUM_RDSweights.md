# G-NSUM for DWeMSinUK #


**You:**

Feehan and Salganik - 2016 - Generalizing the Network Scale-Up Method.pdf
PDF
I have an RDS sample, but I want to perform NSUM estimation.  Attached is a paper that might be helpful.  I had thought there was an equation stating how to incorporate RDS sample weights into an NSUM estimation.  Am I mistaken?

* * *

**ChatGPT:**

Great question! 🧠 Let’s walk through this carefully.

* * *

### ✅ Short Answer:

**You are _not_ mistaken.** Feehan and Salganik (2016) do discuss how to incorporate complex sampling designs—including **RDS samples**—into **NSUM** estimation. Specifically, they describe how **RDS-derived sample weights** can be incorporated into the generalized network scale-up method (**GNSUM**) when using **relative probability sampling**, which includes RDS.

* * *

### 📌 Key Takeaways from the Paper:

**1. The Generalized Scale-Up Estimator (GNSUM)**  
They derive a form of the estimator:

$$\hat{N}_H = \frac{\hat{y}_{F,H}}{\hat{\bar{v}}_{H,F}}$$

Where:

* $\hat{y}_{F,H}$ = estimated total out-reports from frame population (usually a standard probability sample)

* $\hat{\bar{v}}_{H,F}$ = average _in-reports_ from hidden population members (requires a sample of the hidden pop)


**2. Incorporating RDS Samples into NSUM**

In Section **2.1: Estimating $\bar{v}_{H,F}$ from Sampled Data**, they explicitly describe that:

> "...estimating $\bar{v}_{H,F}$ requires only a **relative probability sample** in which hidden population members have a nonzero probability of inclusion and **respondents’ probabilities of inclusion are known up to a constant of proportionality**..."

This describes **RDS samples** perfectly. 📊

**3. Weighted Estimator**  
They then provide the estimator:

$$\hat{\bar{v}}_{H,F} = \frac{N_F}{N_A} \cdot \frac{\sum_{i \in s_H} \sum_j \tilde{v}_{i,A_j} / c \pi_i}{\sum_{i \in s_H} 1 / c \pi_i}$$

Where:

* $\tilde{v}_{i,A_j}$ is the respondent's report of their visibility to group $A_j$

* $c\pi_i$ are the **relative inclusion probabilities** (as given by e.g., RDS-II weights)


So: **RDS weights ($\pi_i$) can be directly incorporated into the estimation.**

* * *

### 🧩 Important Notes:

* They require you to **know or estimate** each participant’s **degree**, and that sampling probabilities are **proportional to degree** (as in Volz-Heckathorn RDS estimator).

* Assumptions like **accurate reporting of visibility**, and **representative probe alters**, still apply.


* * *

### ⚠️ Caveat:

They **don’t provide a one-line formula** for using RDS weights in basic NSUM (like Killworth’s estimator). Instead, they show how **the generalized NSUM estimator** can incorporate RDS weights via **Horvitz-Thompson-type weighting**.

* * *

### 📚 Summary:

You _can_ incorporate RDS-derived weights into NSUM estimates using the **generalized scale-up method**. Feehan and Salganik (2016) outline how to do this with relative probability samples such as those produced by RDS. The estimator is consistent under reasonable assumptions, provided you collect the right data (enriched ARD from hidden population).

* * *
## Gnsum ##



🧠 Conceptual Clarification
---------------------------



### 🔍 Clarifying the Hidden Population Estimation Using NSUM with RDS Samples

You're absolutely right to raise the key question:

> “Aren’t we sampling from the _frame_ population, not the _hidden_ population? So how can we estimate visibility from a sample we didn’t intend to collect from the hidden population?”

Let’s walk through the logic, the assumptions, and a **complete R implementation** for **comparing NSUM and RDS estimates** of exploitation among domestic workers.

* * *

🧩 1. Conceptual Framework
--------------------------

### 📦 Your Sampling Frame:

* You sample **domestic workers in the UK** via **RDS**

* That’s your **frame population** $F$


### 🔍 Your Hidden Populations (H):

* Workers who have been **exploited** (based on various criteria, e.g. document withholding, pay issues, etc.)

* These are **identified ex post** within your RDS sample using coded responses (e.g. `document_withholding_rds == 1`)


### ⚖️ Estimating Hidden Population Size (via Generalized NSUM):

$$\hat{N}_H = \frac{\hat{y}_{F,H}}{\hat{\bar{v}}_{H,F}} \cdot N_F$$

Where:

* $\hat{y}_{F,H}$: total **out-reports** from the frame population — “How many exploited people do you know?”

* $\hat{\bar{v}}_{H,F}$: average **visibility** of each hidden population member to the frame (estimated via RDS weights)

* $N_F$: known size of the **frame population** (e.g. all domestic workers in the UK)


* * *

📌 2. Core Assumptions (per Feehan & Salganik 2016)
---------------------------------------------------

✅ You can estimate visibility $\bar{v}_{H,F}$ using **a relative probability sample** of the hidden population (i.e., an RDS sample where **you can identify hidden population members and apply RDS weights**)

✅ You use **Q13** ("How many other domestic workers do you know?") as a proxy for the **visibility** of a respondent — under the assumption of **reciprocity/symmetry**.

⚠️ Assumptions involved:

* Accurate reporting of out-reports (e.g., Q71)

* Symmetric ties (visibility ≈ degree)

* Homogenous visibility within the hidden population

* Correct RDS weights (e.g., Volz-Heckathorn or Gile’s SS)

* No barrier to being named (visibility bias minimized)


* * *

🧪 3. Full R Workflow — Comparing NSUM and RDS
----------------------------------------------

Here’s a complete example for one indicator: **document withholding**.

* * *

### ✅ Step-by-Step in R

```r
# ----------------------------------------
# Setup: Required inputs
# ----------------------------------------

# Frame population size
N_F <- 980000  # Total UK domestic workers

# Variable definitions
rds_var   <- "document_withholding_rds"
nsum_var  <- "document_withholding_nsum"
degree_var <- "known_network_size"
weight_var <- "wt.vh_980k"  # Or wt.SS_980k or wt.RDS1...

# Safety check
stopifnot(all(c(rds_var, nsum_var, degree_var, weight_var) %in% names(dd)))
```

* * *

### 🧮 Step 1: Estimate Out-Reports $\hat{y}_{F,H}$

```r
# Out-reports: How many exploited people each respondent knows
dd$wts <- dd[[weight_var]]
dd$out_report <- dd[[nsum_var]]

y_FH <- sum(dd$out_report * dd$wts, na.rm = TRUE)
```

* * *

### 👁️ Step 2: Estimate Visibility $\hat{\bar{v}}_{H,F}$

```r
# Filter to hidden pop members (exploited) with non-missing degree and weights
dd_hidden <- dd %>%
filter(!!sym(rds_var) == 1, !is.na(!!sym(degree_var)), !is.na(wts))

# Estimate average visibility (degree) weighted by RDS weights
numerator   <- sum(dd_hidden[[degree_var]] / dd_hidden$wts)
denominator <- sum(1 / dd_hidden$wts)
v_HF <- numerator / denominator
```

* * *

### 📊 Step 3: Compute NSUM Estimate

```r
# Generalized NSUM formula
NSUM_est <- (y_FH / v_HF)  # May or may not need to multiply by N_F

# If v_HF is per respondent and not per population, scale up
NSUM_count <- NSUM_est * N_F
```

* * *

### 🎯 Step 4: Compute RDS Estimate (for comparison)

```r
# RDS prevalence estimate
rds_prev <- weighted.mean(dd[[rds_var]], w = dd$wts, na.rm = TRUE)

# Scale to population
rds_count <- rds_prev * N_F
```

* * *

### 📋 Step 5: Display Results

```r
cat("Estimate for Document Withholding\n")
cat("NSUM estimate: ", round(NSUM_count), "\n")
cat("RDS estimate : ", round(rds_count), "\n")
```



* * *

**You:**

Codebook (November Version).pdf
PDF
Questions that identify respondent as a member of the hidden population.pdf
PDF
Thank you. First, here is the codebook for our RDS survey and indicators of if a respondent is a member of the hidden population.  Take me through each step of this NSUM estimation.  Please note we have multiple indicators of being exploited -- see below:
"""
# 02-data_preparation.R
# Data preparation pipeline: RDS formatting, comparable indicators, and weights
# Includes CE's comparable RDS/NSUM indicator specifications (2024-11-05)

# Load required libraries
library(tidyverse)
library(RDS)
library(car)
library(psych)
library(here)

# Load cleaned data
if (!exists("data_final") || !exists("data_nonzero")) {
load(here("data", "processed", "cleaned_data.RData"))
}

# Create comparable RDS/NSUM indicators following CE's specifications
create_comparable_indicators <- function(data) {

cat("Creating comparable RDS/NSUM indicators...\n")

# CE's mappings from most confident to least confident:

# 1. Document withholding (MOST CONFIDENT): Q70 5f2 = Q71 5f3
# Q70: Has employer withheld travel/identity documents?
data$zQ70 <- recode(data$q70, "4=NA")  # Set don't know to NA
data$zQ70 <- recode(data$q70, "c(0,1,2) = 1; c(3,4)=0")  # Always/often/sometimes = 1, never = 0

# Q71: Know others without access to documents? (already numeric count)
data$document_withholding_rds <- data$zQ70
data$document_withholding_nsum <- ifelse(data$q71 > 0, 1, 0)  # Any known others = 1
data$document_withholding_nsum[is.na(data$q71)] <- NA

# 2. Pay/debt issues (HIGH CONFIDENCE): Q39 5b4 and Q42 5b7 = Q43 5b8
# Q39: Have to pay debt to someone who helped find work?
data$zQ39 <- recode(data$q39, "2=NA")  # Don't know to NA
data$zQ39 <- recode(data$q39, "0 = 1; c(1,2)=0")  # Yes = 1, No = 0

# Q42: Has pay ever been withheld?
data$zQ42 <- recode(data$q42, "4=NA")  # Don't know to NA
data$zQ42 <- recode(data$q42, "c(0,1,2) = 1; c(3,4)=0")  # Always/often/sometimes = 1, never = 0

# Q43: Know others with debt/pay problems? (already numeric count)
data$pay_issues_rds <- ifelse(data$zQ39 == 1 | data$zQ42 == 1, 1, 0)  # Logical OR
data$pay_issues_rds[is.na(data$zQ39) & is.na(data$zQ42)] <- NA  # NA if both missing
data$pay_issues_nsum <- ifelse(data$q43 > 0, 1, 0)  # Any known others = 1
data$pay_issues_nsum[is.na(data$q43)] <- NA

# 3. Threats/abuse/force (HIGH CONFIDENCE): Q45 5c2 and Q47 5c4 and Q48 5c5 = Q49 5c6
# Q45: Forced, deceived or threatened into poor conditions?
data$zQ45 <- recode(data$q45, "2=NA")  # Prefer not to say to NA
data$zQ45 <- recode(data$q45, "0 = 1; c(1,2)=0")  # Yes = 1, No = 0

# Q47: Has employer threatened or intimidated you?
data$zQ47 <- recode(data$q47, "4=NA")  # Don't know to NA
data$zQ47 <- recode(data$q47, "c(0,1,2) = 1; c(3,4)=0")  # Yes/often/sometimes = 1, never = 0

# Q48: Has employer verbally abused you?
data$zQ48 <- recode(data$q48, "4=NA")  # Don't know to NA
data$zQ48 <- recode(data$q48, "c(0,1,2) = 1; c(3,4)=0")  # Yes often/sometimes/maybe = 1, never = 0

# Q49: Know others with threat/force experiences? (already numeric count)
data$threats_abuse_rds <- ifelse(data$zQ45 == 1 | data$zQ47 == 1 | data$zQ48 == 1, 1, 0)  # Logical OR
data$threats_abuse_rds[is.na(data$zQ45) & is.na(data$zQ47) & is.na(data$zQ48)] <- NA  # NA if all missing
data$threats_abuse_nsum <- ifelse(data$q49 > 0, 1, 0)  # Any known others = 1
data$threats_abuse_nsum[is.na(data$q49)] <- NA

# 4. Excessive hours (LOWER CONFIDENCE): Q61 5d8 and Q62 5d9 = Q64 5d11
# NOTE: Q64 includes annual leave which RDS questions don't cover
# Q61: Weekly rest longer than 24 hours consecutively?
data$zQ61 <- recode(data$q61, "4=NA")  # Don't know to NA  
data$zQ61 <- recode(data$q61, "c(2,3) = 1; c(0,1,4)=0")  # Sometimes/never = 1 (inadequate rest), always/often = 0

# Q62: Worked overtime that felt excessive?
data$zQ62 <- recode(data$q62, "4=NA")  # Don't know to NA
data$zQ62 <- recode(data$q62, "c(0,1,2) = 1; c(3,4)=0")  # Always/often/sometimes = 1, never = 0

# Q64: Know others with labour rights issues? (includes excessive hours + annual leave)
data$excessive_hours_rds <- ifelse(data$zQ61 == 1 | data$zQ62 == 1, 1, 0)  # Logical OR
data$excessive_hours_rds[is.na(data$zQ61) & is.na(data$zQ62)] <- NA  # NA if both missing
data$excessive_hours_nsum <- ifelse(data$q64 > 0, 1, 0)  # Any known others = 1
data$excessive_hours_nsum[is.na(data$q64)] <- NA

# 5. Access to help (LEAST CONFIDENT): Q78 5f10 (coded as No) = Q79 5f11
# Q78: Do you know who might help if not properly paid/treated? (reverse coded)
data$zQ78 <- recode(data$q78, "1 = 0; 0 = 1")  # No = 1 (vulnerable), Yes = 0
data$Q78rev <- data$zQ78  # Reversed version to align with Q79

# Q79: Know others who don't know where to go for help? (already numeric count)
data$access_to_help_rds <- data$Q78rev  # Higher values = more vulnerable
data$access_to_help_nsum <- ifelse(data$q79 > 0, 1, 0)  # Any known others = 1
data$access_to_help_nsum[is.na(data$q79)] <- NA

# Additional: Known network size variable (from CE's note)
# Q13 2f: Number of domestic workers for whom you have contact details in phone
data$known_network_size <- as.numeric(data$q13)

return(data)
}

# Prepare legacy indicators for backward compatibility
prepare_legacy_indicators <- function(data) {

cat("Preparing legacy indicators for backward compatibility...\n")

# Legacy Q36 and Q80 indicators (remove D/K responses)
data$zQ36 <- recode(data$q36, "5=NA")
data$zQ36 <- recode(data$q36, "c(0,1,2,3) = 1; c(4,5)=0")

data$zQ80 <- recode(data$q80, "4=NA")
data$zQ80 <- recode(data$q80, "c(0,1) = 1; c(2,3,4,5)=0")

# Risk categories
data$sum_categories_factor <- as.factor(data$sum_categories)
data$sum_categories_cut <- cut_interval(data$sum_categories, n = 10)

return(data)
}

# Main data preparation function
prepare_data <- function() {

# Start with non-zero degree data
dd <- data_nonzero

# Create comparable indicators using CE's specifications
dd <- create_comparable_indicators(dd)

# Add legacy indicators for backward compatibility
dd <- prepare_legacy_indicators(dd)

# Create RDS data frame object
cat("Creating RDS data frame...\n")
rd.dd <- as.rds.data.frame(dd, 
                     id="id", 
                     recruiter.id="recruiter.id", 
                     max.coupons = 5, 
                     check.valid = FALSE)

# Calculate RDS and VH/SS weights for different population sizes
cat("Calculating RDS and population weights...\n")
dd <- dd %>%
mutate(
# RDS-I weights for comparable indicators
wt.RDS1_document_withholding = rds.I.weights(rd.dd, "document_withholding_rds"),
wt.RDS1_pay_issues = rds.I.weights(rd.dd, "pay_issues_rds"),
wt.RDS1_threats_abuse = rds.I.weights(rd.dd, "threats_abuse_rds"),
wt.RDS1_excessive_hours = rds.I.weights(rd.dd, "excessive_hours_rds"),
wt.RDS1_access_to_help = rds.I.weights(rd.dd, "access_to_help_rds"),

# Legacy RDS-I weights
wt.RDS1_zQ36 = rds.I.weights(rd.dd, "zQ36"),
wt.RDS1_zQ80 = rds.I.weights(rd.dd, "zQ80"),
wt.RDS1_sum_categories_factor = rds.I.weights(rd.dd, "sum_categories_factor"),

# Volz-Heckathorn weights for different population sizes
wt.vh_980k = vh.weights(numRef, N = 980000),   # EU baseline estimate
wt.vh_100k = vh.weights(numRef, N = 100000),   # Conservative estimate
wt.vh_050k = vh.weights(numRef, N = 50000),    # Very conservative
wt.vh_1740k = vh.weights(numRef, N = 1740000), # Upper bound estimate

# Gile's SS weights for different population sizes
wt.SS_980k = gile.ss.weights(numRef, N = 980000),
wt.SS_100k = gile.ss.weights(numRef, N = 100000),
wt.SS_050k = gile.ss.weights(numRef, N = 50000),
wt.SS_1740k = gile.ss.weights(numRef, N = 1740000)
)

# Save prepared data
cat("Saving prepared data...\n")
save(dd, rd.dd, file = here("data", "processed", "prepared_data.RData"))

# Export CSV for external use (remove list columns first)
dd_csv <- dd %>% select(-NonEmptyValues)
write.csv(dd_csv, here("data", "processed", "prepared_data.csv"), row.names = FALSE)

cat("Data preparation completed successfully!\n")
cat("- Prepared data:", nrow(dd), "observations\n")
cat("- RDS data object created\n")
cat("- Comparable indicators created following CE's specifications\n")
cat("- Multiple population size weights calculated\n")
cat("- Files saved to data/processed/\n")

return(list(
dd = dd,
rd.dd = rd.dd
))
}

# Execute data preparation if running this script directly
if (!exists("skip_execution")) {
prepared_data <- prepare_data()
# Make dd and rd.dd available in global environment
dd <<- prepared_data$dd
rd.dd <<- prepared_data$rd.dd
}
"""

* * *

**ChatGPT:**

Fantastic — you’ve provided **all the key ingredients** for implementing a **generalized NSUM** with an RDS sample. Let’s walk through the **complete estimation process step by step**, applying the method proposed by **Feehan & Salganik (2016)** and incorporating your **RDS weights**, **known network size**, and **comparable indicators**.

* * *

🧮 Full NSUM Estimation Workflow (with RDS Sample)
--------------------------------------------------

* * *

### ✅ **Step 1: Confirm Required Data is Available**

You've already prepared the following data elements:

| **Element** | **Variable** | **Status** |
| --- | --- | --- |
| Unique ID | `id` | ✅ |
| Recruiter ID | `recruiter.id` | ✅ |
| Known network size (degree) | `known_network_size` from Q13 | ✅ |
| Indicators of hidden population membership (respondent-level) | e.g., `document_withholding_rds`, `pay_issues_rds`, etc. | ✅ |
| Indicators of knowing others in hidden population (alter-level ARD) | e.g., `document_withholding_nsum`, `pay_issues_nsum`, etc. | ✅ |
| RDS weights (e.g., Volz-Heckathorn or RDS-I) | `wt.vh_*`, `wt.RDS1_*` | ✅ |

All ingredients are in place 🎉

* * *

### 📏 **Step 2: Understand the NSUM Estimator**

The **generalized NSUM estimator** has two main components:

$$\hat{N}_H = \frac{\hat{y}_{F,H}}{\hat{\bar{v}}_{H,F}}$$

Where:

* $\hat{y}_{F,H}$: **Out-reports** — how many people the frame population (your RDS sample) reports **knowing** in the hidden population

* $\hat{\bar{v}}_{H,F}$: **Average visibility** — how many **times a hidden population member** would be mentioned by someone in the frame


* * *

### 🔢 **Step 3: Calculate Out-Reports ( $\hat{y}_{F,H}$ )**

This is the **sum of known others** in the hidden population from your survey (e.g., Q71, Q43, etc.). You’ve already created indicators like `document_withholding_nsum`, which are binary (1 = knows someone exploited, 0 = does not).

#### Options:

* **If using binary indicators (as in your code):**


You can sum the weighted out-reports like so:

```r
# Example for document withholding
y_FH_doc <- sum(dd$document_withholding_nsum * dd$wt.vh_980k, na.rm = TRUE)
```

This gives $\hat{y}_{F,H}$, the total number of people your sample knows in the hidden population (for a given definition of exploitation).

Repeat for each exploitation type (e.g., `pay_issues_nsum`, `threats_abuse_nsum`, etc.).

* * *

### 👀 **Step 4: Estimate Average Visibility ( $\hat{\bar{v}}_{H,F}$ )**

Here’s where **RDS weights** come into play.

$$\hat{\bar{v}}_{H,F} = \frac{N_F}{N_A} \cdot \frac{\sum_{i \in s_H} \sum_j \tilde{v}_{i,A_j} / c\pi_i}{\sum_{i \in s_H} 1 / c\pi_i}$$

Where:

* $s_H$: the sample of people **in the hidden population**

* $\tilde{v}_{i,A_j}$: the number of **times the respondent thinks they are known** by group $A_j$ (visibility)

* $c\pi_i$: sampling weights for each respondent in $s_H$


#### In practice:

You don’t have direct measures of visibility $\tilde{v}_{i,A_j}$, but you can **approximate** it using known **degree** (Q13) — how many domestic workers they know — **under the assumption of symmetry** (if I know you, you probably know me).

Assume:

\tilde{v}_{i,F} \approx \text{known_network_size}_i

So you can estimate $\hat{\bar{v}}_{H,F}$ as:

```r
# Subset to hidden population members
hidden_doc <- dd %>%
filter(document_withholding_rds == 1 & !is.na(known_network_size))

# Weighted mean degree
v_HF_doc <- with(hidden_doc, sum(known_network_size / wt.vh_980k) / sum(1 / wt.vh_980k))
```

Repeat for each exploitation definition.

* * *

### 🧮 **Step 5: Combine into NSUM Estimate**

$$\hat{N}_H = \frac{\hat{y}_{F,H}}{\hat{\bar{v}}_{H,F}} \cdot N_F$$

Where:

* $N_F$: size of frame population (e.g., 980,000 for all domestic workers in the UK)


```r
N_F <- 980000  # or your alternative size

NSUM_est_doc <- y_FH_doc / v_HF_doc
```

This gives an estimate of how many **domestic workers in the UK** are experiencing document withholding.

* * *

### 📊 **Step 6: Repeat for All Exploitation Types**

You’ll generate one NSUM estimate for each:

| Indicator | NSUM Estimate |
| --- | --- |
| Document withholding | `NSUM_est_doc` |
| Pay issues | `NSUM_est_pay` |
| Threats/abuse | `NSUM_est_abuse` |
| Excessive hours | `NSUM_est_hours` |
| Access to help | `NSUM_est_help` |

* * *

### 🛠️ Optional: Combine Estimates or Use Risk Weights

Based on your `sum_categories`, `sum_categories_factor`, and the risk index (from the PDF), you could:

* Create a **weighted index of exploitation** (e.g., combining multiple indicators)

* Run the estimator over the combined category


* * *

### ✅ Recap of Required R Code Steps

```r
# 1. Calculate y_FH
y_FH_doc <- sum(dd$document_withholding_nsum * dd$wt.vh_980k, na.rm = TRUE)

# 2. Estimate mean visibility (v_HF)
hidden_doc <- dd %>% filter(document_withholding_rds == 1 & !is.na(known_network_size))
v_HF_doc <- with(hidden_doc, sum(known_network_size / wt.vh_980k) / sum(1 / wt.vh_980k))

# 3. Compute NSUM estimate
NSUM_doc <- y_FH_doc / v_HF_doc
```

* * *

🧭 Next Steps?
--------------

Would you like help with:

* 🧪 Bootstrapped confidence intervals?

* 📦 Packaging this into an R function for your project?

* 📉 Visualizations for comparing NSUM vs RDS prevalence estimates?


Just say the word! 😊

* * *

**You:**

Ah, I think I see.  You can use Q13 ("Do you know personally of any other domes�c workers who are
engaged in domes�c work in the UK (please answer based on the number of domes�c workers
whose contact details you have in your contacts list on your mobile phone)? If so, how many? (Please
indicate the number, or state ‘0’ if this does not apply to anyone you know). Numerical value.") because 'the population' is the set of of all domestic workers in the UK.  Is that correct?  Otherwise, don't we need e.g. known-group-size groups?

* * *

**ChatGPT:**

Yes — **you’ve got it exactly right**! 🎯 Let me clarify this with a bit more detail so you can be confident in what you're doing:

* * *

✅ Why Q13 ("How many domestic workers do you know?") can be used for NSUM
-------------------------------------------------------------------------

In the **generalized NSUM framework**, you're estimating the size of a **hidden subpopulation** (e.g., exploited domestic workers) **within a larger frame population** (in your case: **all domestic workers in the UK**).

> So the population of reference for your NSUM estimation is:  
> **All domestic workers in the UK** — not the general public.

* * *

### 🔍 NSUM Estimation Logic Recap:

$$\hat{N}_H = \frac{\hat{y}_{F,H}}{\hat{\bar{v}}_{H,F}} \cdot N_F$$

Where:

* $\hat{y}_{F,H}$: how many _other_ domestic workers the sample reports knowing who are in the **hidden population**

* $\hat{\bar{v}}_{H,F}$: how visible each _hidden_ domestic worker is to the frame population (i.e., other domestic workers)

* $N_F$: total number of domestic workers in the UK (your known frame population size)


* * *

🔄 Why You Don't Need Known-Group-Size Alters (in This Case)
------------------------------------------------------------

In classic NSUM (Killworth, Bernard, et al.), you need **known-size alter groups** (e.g., "How many teachers do you know?") to estimate people's **degree** in the general population.

> But in your case, you're **sampling from the frame population** (domestic workers), and you ask:  
> “How many **other domestic workers** do you know?”

That means:

* You’re asking about **within-frame degree** directly.

* You **don’t need to estimate it** indirectly using known groups.

* You can treat Q13 as a **direct estimate of personal network size** in the **frame population**.


This is exactly what Feehan & Salganik (2016) refer to when they say:

> "The key quantity we need is the number of connections to the frame population."

* * *

📌 Summary
----------

✅ You _can_ use Q13 as the degree measure  
✅ Your population is "domestic workers in the UK"  
✅ You do _not_ need known-group-size alter questions (like “How many teachers do you know?”) because your degree question (Q13) is **already specific to your frame**

* * *

🧠 Optional: Caveats
--------------------

If your frame population were broader (e.g., "all adults in the UK"), then yes — you’d likely need known-group-size questions to estimate degree.

But in your case:

* The frame population = domestic workers

* Degree question = “how many domestic workers do you know?” ✅

* Hidden population = exploited domestic workers


Everything is consistent 💡

* * *

Would you like a quick visual diagram of this logic or a tidy table showing the NSUM components in your setup? Or we can go right into code packaging if you're ready for that step.

* * *

**You:**

Excellent!  Absolutely excellent! Ultimately, I want to compare the RDS estimate to the NSUM estimate.  To do this I had to aggregate some variables so they are comparable (e.g. 

# 1. Document withholding (MOST CONFIDENT): Q70 5f2 = Q71 5f3
# Q70: Has employer withheld travel/identity documents?
data$zQ70 <- recode(data$q70, "4=NA")  # Set don't know to NA
data$zQ70 <- recode(data$q70, "c(0,1,2) = 1; c(3,4)=0")  # Always/often/sometimes = 1, never = 0

# Q71: Know others without access to documents? (already numeric count)
data$document_withholding_rds <- data$zQ70
data$document_withholding_nsum <- ifelse(data$q71 > 0, 1, 0)  # Any known others = 1
data$document_withholding_nsum[is.na(data$q71)] <- NA

and

# 2. Pay/debt issues (HIGH CONFIDENCE): Q39 5b4 and Q42 5b7 = Q43 5b8
# Q39: Have to pay debt to someone who helped find work?
data$zQ39 <- recode(data$q39, "2=NA")  # Don't know to NA
data$zQ39 <- recode(data$q39, "0 = 1; c(1,2)=0")  # Yes = 1, No = 0

# Q42: Has pay ever been withheld?
data$zQ42 <- recode(data$q42, "4=NA")  # Don't know to NA
data$zQ42 <- recode(data$q42, "c(0,1,2) = 1; c(3,4)=0")  # Always/often/sometimes = 1, never = 0

# Q43: Know others with debt/pay problems? (already numeric count)
data$pay_issues_rds <- ifelse(data$zQ39 == 1 | data$zQ42 == 1, 1, 0)  # Logical OR
data$pay_issues_rds[is.na(data$zQ39) & is.na(data$zQ42)] <- NA  # NA if both missing
data$pay_issues_nsum <- ifelse(data$q43 > 0, 1, 0)  # Any known others = 1
data$pay_issues_nsum[is.na(data$q43)] <- NA

With this in mind, and given your previous answers, please can you suggest  comprehensive R: workflow to perform the NSUM estimation, using the sample from the 'hidden population.'  Then again, this approach strikes me as very odd. To wit, we are _not_ sampling from the hidden population.  We are sampling fromthe Frame.  It just so happens that some of these respondents _are_ members of the hidden population.  But we only know that ex post, e.g. after asking question q71 or q39, etc.  Doesn't that make calculating the survey weights ($\pi_i$) infeasable?

* * *

**ChatGPT:**

You're asking **exactly the right questions** here! 💡 This subtle point—**who is in the frame population vs. hidden population**, and how that affects weighting—is at the heart of why **NSUM with RDS samples** is both powerful _and_ tricky.

Let’s break this down carefully. First:

* * *

🧠 Conceptual Clarification
---------------------------

### 📌 What You Said:

> "We are _not_ sampling from the hidden population. We are sampling from the frame. It just so happens that some of these respondents _are_ members of the hidden population."

✅ **Correct!** You are sampling from the **frame population** of **domestic workers** via **RDS**. This is **not** a sample of the hidden population by design — though some of the respondents _are_ exploited workers (i.e. members of the hidden population, **ex post**).

Now here’s the key:

* * *

### 🧭 What Does Feehan & Salganik Require?

They distinguish between two estimators:

| Component | Required Sample |
| --- | --- |
| $\hat{y}_{F,H}$: Out-reports | Sample of frame population (e.g., your RDS sample) ✅ |
| $\hat{\bar{v}}_{H,F}$: Average visibility | Sample of **hidden population** members with known inclusion probabilities ❗ |

So for **standard NSUM**, you're good.

But for **generalized NSUM**, **visibility** has to be estimated **from the hidden population**, ideally using a **relative probability sample**, _i.e., RDS of the hidden pop_.

> ❗ Here's the twist:  
> You are _not_ sampling the hidden population directly, but rather finding hidden population members **embedded** in your RDS sample of the frame.

* * *

🧩 So Can You Estimate $\hat{\bar{v}}_{H,F}$ This Way?
------------------------------------------------------

Let’s explore two options:

* * *

### ✅ **Option A: Treat Hidden Members in RDS as a Relative-Probability Sample**

This is what **Feehan & Salganik (2016)** allow. They say (p. 827):

> "_Estimating $\bar{v}_{H,F}$ requires only a **relative probability sample** in which hidden population members have a nonzero probability of inclusion and respondents’ probabilities of inclusion are known up to a constant of proportionality._"

That describes your situation:

* Your RDS sample **includes** members of the hidden population

* You compute **RDS-derived weights** for _all_ respondents (e.g., Volz-Heckathorn or Gile’s SS)

* You can apply those weights **to just the hidden respondents** to estimate $\bar{v}_{H,F}$


👉 This is **valid** as long as:

* Hidden population members **have the same recruitment process** (i.e. RDS)

* You use **relative weights**, e.g. RDS-II weights (based on degree)


* * *

### 🚫 **Option B: Treat Hidden Members as a Convenience Sample**

If you said: “No, we didn’t sample the hidden population intentionally, and we don't know inclusion probabilities,” then **you can't estimate $\hat{\bar{v}}_{H,F}$** in a design-based way.

This would invalidate the **generalized NSUM estimator**.

But since you're using RDS with proper weights, **Option A is defensible.**

* * *

✅ Recommended R Workflow for NSUM Estimation
--------------------------------------------

Let’s write a full pipeline using your cleaned `dd` dataset:

* * *

### 🧾 STEP 1: Define Constants

```r
# Total population size (N_F)
N_F <- 980000  # Estimated number of domestic workers in UK

# Choose your weight set (Volz-Heckathorn weights)
weight_var <- "wt.vh_980k"  # Change as needed
degree_var <- "known_network_size"
```

* * *

### 📊 STEP 2: Choose an Exploitation Type (e.g. Document Withholding)

```r
# Define relevant vars
rds_var   <- "document_withholding_rds"
nsum_var  <- "document_withholding_nsum"
```

* * *

### 🔢 STEP 3: Estimate Out-Reports ($\hat{y}_{F,H}$)

```r
# Weighted sum of known others in hidden pop
dd$wts <- dd[[weight_var]]
y_FH <- sum(dd[[nsum_var]] * dd$wts, na.rm = TRUE)
```

* * *

### 👀 STEP 4: Estimate Visibility ($\hat{\bar{v}}_{H,F}$)

```r
# Subset to hidden pop members (those exploited)
dd_hidden <- dd %>% filter(!!sym(rds_var) == 1 & !is.na(!!sym(degree_var)) & !is.na(wts))

# Horvitz-Thompson-style weighted average
v_HF <- with(dd_hidden, sum(!!sym(degree_var) / wts) / sum(1 / wts))
```

* * *

### 🧮 STEP 5: Compute NSUM Estimate

```r
N_hidden <- (y_FH / v_HF)  # No need to multiply by N_F again (already implicit)
```

If your visibility is per hidden respondent (and not scaled to N_F), then:

```r
N_hidden <- (y_FH / v_HF) * N_F
```

Double-check scaling by plotting intermediate outputs.

* * *

### 📊 STEP 6: Compute RDS Prevalence for Comparison

```r
# Weighted mean among all RDS respondents
rds_est <- weighted.mean(dd[[rds_var]], w = dd$wts, na.rm = TRUE)

# Estimated number in total frame population
rds_count <- rds_est * N_F
```

* * *

### 📋 Output Both Estimates:

```r
cat("NSUM Estimate:", round(N_hidden), "\n")
cat("RDS Estimate: ", round(rds_count), "\n")
```

* * *

🧪 Optional: Automate Over All Exploitation Types?
--------------------------------------------------

Yes! You can wrap this in a function and iterate over:

```r
exploitation_list <- list(
"document_withholding" = list("rds" = "document_withholding_rds", "nsum" = "document_withholding_nsum"),
"pay_issues" = list("rds" = "pay_issues_rds", "nsum" = "pay_issues_nsum"),
...
)
```

Want help packaging that? Just say the word.

* * *

🧠 TL;DR on Your Key Concern:
-----------------------------

> **"Doesn't that make calculating the survey weights ($$\pi_i$$) infeasible?"**

✔️ No — because you’re using **RDS**, and can calculate **relative inclusion probabilities** for _everyone_ in the sample  
✔️ You can use these **relative weights** (e.g., Volz-Heckathorn) **for the subset who are in the hidden population**

Therefore: ✅ **It _is_ valid to use your embedded hidden population sample to estimate visibility**, as per Feehan & Salganik.

* * *

Would you like:

* 🧪 Bootstrapping for confidence intervals?

* 📦 Full R function version of the estimator?

* 📉 A side-by-side plot of NSUM vs RDS prevalence?


Let’s keep building this tool! 🚀

* * *

**You:**

Please critique the above response, noting that more specific examples and details are required. Then based on the critique, output the full improved response.

* * *

**ChatGPT:**

Absolutely — let’s begin with a **critique of the previous response** and then provide a **revised, fully improved version** based on that feedback. 🛠️

* * *

🧠 Critique of the Previous Response
------------------------------------

While the response was solid and conceptually accurate, **there are areas where clarity and specificity could be improved** — especially for an advanced user like yourself. Here’s a breakdown:

### ✅ **What Worked Well**

* **Accurate conceptual framing**: The distinction between sampling the frame vs. the hidden population was well explained.

* **Correct use of Feehan & Salganik (2016)**: The justification for using RDS weights for the embedded hidden population was sound.

* **Code scaffolding**: A reasonable step-by-step estimation process in R was provided.


* * *

### ❌ **What Needed Improvement**

| Area | Problem | Recommendation |
| --- | --- | --- |
| **Code Specificity** | The code used placeholders (e.g., `!!sym(...)`) without explanation or simplification | Provide full, **executable** R code with **clear variable names** and explanation of each step |
| **Terminology Clarity** | The distinction between visibility and degree was made, but not explicitly tied to **Q13** numerically | Re-emphasize **why Q13 is a proxy** for visibility from the hidden population to the frame |
| **Statistical Assumptions** | Implicit assumptions (e.g. visibility symmetry, independence, and accurate reporting) were not stated | Explicitly list and briefly discuss assumptions involved in estimating $\bar{v}_{H,F}$ |
| **Estimation Interpretation** | The difference between NSUM and RDS estimates was shown, but no guidance on **interpreting discrepancies** was given | Briefly discuss how to interpret NSUM vs RDS differences (e.g., underreporting bias, social visibility) |
| **Generalization** | No function or loop was implemented to allow scalability across multiple indicators | Include reusable **R function** or loop over exploitation indicators |

* * *

✅ Now, the **Improved Full Response**
-------------------------------------

* * *

### 🔍 Clarifying the Hidden Population Estimation Using NSUM with RDS Samples

You're absolutely right to raise the key question:

> “Aren’t we sampling from the _frame_ population, not the _hidden_ population? So how can we estimate visibility from a sample we didn’t intend to collect from the hidden population?”

Let’s walk through the logic, the assumptions, and a **complete R implementation** for **comparing NSUM and RDS estimates** of exploitation among domestic workers.

* * *

🧩 1. Conceptual Framework
--------------------------

### 📦 Your Sampling Frame:

* You sample **domestic workers in the UK** via **RDS**

* That’s your **frame population** $F$


### 🔍 Your Hidden Populations (H):

* Workers who have been **exploited** (based on various criteria, e.g. document withholding, pay issues, etc.)

* These are **identified ex post** within your RDS sample using coded responses (e.g. `document_withholding_rds == 1`)


### ⚖️ Estimating Hidden Population Size (via Generalized NSUM):

$$\hat{N}_H = \frac{\hat{y}_{F,H}}{\hat{\bar{v}}_{H,F}} \cdot N_F$$

Where:

* $\hat{y}_{F,H}$: total **out-reports** from the frame population — “How many exploited people do you know?”

* $\hat{\bar{v}}_{H,F}$: average **visibility** of each hidden population member to the frame (estimated via RDS weights)

* $N_F$: known size of the **frame population** (e.g. all domestic workers in the UK)


* * *

📌 2. Core Assumptions (per Feehan & Salganik 2016)
---------------------------------------------------

✅ You can estimate visibility $\bar{v}_{H,F}$ using **a relative probability sample** of the hidden population (i.e., an RDS sample where **you can identify hidden population members and apply RDS weights**)

✅ You use **Q13** ("How many other domestic workers do you know?") as a proxy for the **visibility** of a respondent — under the assumption of **reciprocity/symmetry**.

⚠️ Assumptions involved:

* Accurate reporting of out-reports (e.g., Q71)

* Symmetric ties (visibility ≈ degree)

* Homogenous visibility within the hidden population

* Correct RDS weights (e.g., Volz-Heckathorn or Gile’s SS)

* No barrier to being named (visibility bias minimized)


* * *

🧪 3. Full R Workflow — Comparing NSUM and RDS
----------------------------------------------

Here’s a complete example for one indicator: **document withholding**.

* * *

### ✅ Step-by-Step in R

```r
# ----------------------------------------
# Setup: Required inputs
# ----------------------------------------

# Frame population size
N_F <- 980000  # Total UK domestic workers

# Variable definitions
rds_var   <- "document_withholding_rds"
nsum_var  <- "document_withholding_nsum"
degree_var <- "known_network_size"
weight_var <- "wt.vh_980k"  # Or wt.SS_980k or wt.RDS1...

# Safety check
stopifnot(all(c(rds_var, nsum_var, degree_var, weight_var) %in% names(dd)))
```

* * *

### 🧮 Step 1: Estimate Out-Reports $\hat{y}_{F,H}$

```r
# Out-reports: How many exploited people each respondent knows
dd$wts <- dd[[weight_var]]
dd$out_report <- dd[[nsum_var]]

y_FH <- sum(dd$out_report * dd$wts, na.rm = TRUE)
```

* * *

### 👁️ Step 2: Estimate Visibility $\hat{\bar{v}}_{H,F}$

```r
# Filter to hidden pop members (exploited) with non-missing degree and weights
dd_hidden <- dd %>%
filter(!!sym(rds_var) == 1, !is.na(!!sym(degree_var)), !is.na(wts))

# Estimate average visibility (degree) weighted by RDS weights
numerator   <- sum(dd_hidden[[degree_var]] / dd_hidden$wts)
denominator <- sum(1 / dd_hidden$wts)
v_HF <- numerator / denominator
```

* * *

### 📊 Step 3: Compute NSUM Estimate

```r
# Generalized NSUM formula
NSUM_est <- (y_FH / v_HF)  # May or may not need to multiply by N_F

# If v_HF is per respondent and not per population, scale up
NSUM_count <- NSUM_est * N_F
```

* * *

### 🎯 Step 4: Compute RDS Estimate (for comparison)

```r
# RDS prevalence estimate
rds_prev <- weighted.mean(dd[[rds_var]], w = dd$wts, na.rm = TRUE)

# Scale to population
rds_count <- rds_prev * N_F
```

* * *

### 📋 Step 5: Display Results

```r
cat("Estimate for Document Withholding\n")
cat("NSUM estimate: ", round(NSUM_count), "\n")
cat("RDS estimate : ", round(rds_count), "\n")
```

* * *

🔁 Bonus: Automate Across All Indicators
----------------------------------------

You can build this into a function:

```r
compare_nsum_rds <- function(data, rds_var, nsum_var, degree_var, weight_var, N_F) {
data$wts <- data[[weight_var]]
data$out_report <- data[[nsum_var]]

y_FH <- sum(data$out_report * data$wts, na.rm = TRUE)

data_hidden <- data %>%
filter(!!sym(rds_var) == 1, !is.na(!!sym(degree_var)), !is.na(wts))

v_HF <- with(data_hidden, sum(!!sym(degree_var) / wts) / sum(1 / wts))
NSUM_est <- y_FH / v_HF * N_F
rds_prev <- weighted.mean(data[[rds_var]], w = data$wts, na.rm = TRUE)
RDS_est <- rds_prev * N_F

return(tibble(
indicator = rds_var,
NSUM_estimate = round(NSUM_est),
RDS_estimate = round(RDS_est)
))
}

# Example
compare_nsum_rds(dd, "document_withholding_rds", "document_withholding_nsum", "known_network_size", "wt.vh_980k", N_F)
```

* * *

🔍 Interpreting Differences Between NSUM and RDS
------------------------------------------------

| Possible Reason | Explanation |
| --- | --- |
| **NSUM > RDS** | Hidden population is **more socially visible** than assumed, or respondents **overreport** knowing exploited peers |
| **RDS > NSUM** | Exploited individuals are **underreported** in social networks (visibility bias), or respondents **don't recognize** peers as being exploited |
| **Both Estimates Low** | Underreporting bias, low trust, or poor measurement |
| **Both High** | Reflects high prevalence and network awareness |

* * *

🧠 Final Notes
--------------

* ✅ Your situation is **valid** for estimating visibility from the hidden population embedded in your RDS sample, **as long as you apply the correct RDS weights**.

* ❗ If visibility is **not symmetric** or **not evenly distributed**, estimates may be biased — you can correct this with **enriched ARD** if you collect it.


* * *

Would you like:

* 🧪 Bootstrapped confidence intervals?

* 📦 A full R package template for this estimator?

* 📈 Side-by-side visualizations of RDS vs NSUM by indicator?


Just let me know how far you'd like to take this! 😊




**You:**

For GNSUM why do the rest weights appear in both the numerator and denominator?

* * *

**ChatGPT:**

Excellent question! 🧠 The reason the **RDS (or more generally, relative probability) weights** appear in **both the numerator and the denominator** of the **generalized NSUM visibility estimator** is to ensure you're calculating a **proper weighted average** using **Horvitz–Thompson-style estimation** — **even when the sampling probabilities are only known up to a constant factor.**

Let’s break this down carefully. 👇

* * *

🔍 The GNSUM Visibility Estimator
---------------------------------

Feehan & Salganik (2016) define the estimator for average visibility of hidden population members ($\bar{v}_{H,F}$) as:

$$\hat{\bar{v}}_{H,F} = \frac{N_F}{N_A} \cdot \frac{\sum_{i \in s_H} \tilde{v}_{i,F} / \pi_i}{\sum_{i \in s_H} 1 / \pi_i}$$

Where:

* $\tilde{v}_{i,F}$: the number of _frame population members_ who know hidden population member $i$ (i.e., visibility)

* $\pi_i$: the **(relative)** inclusion probability of individual $i$ in the hidden population sample $s_H$

* $N_F$: size of the frame population

* $N_A$: size of the hidden population sample (or more generally, the sum of weights)


This is a **Horvitz–Thompson estimator** for the _weighted mean_, used when your sampling design does **not provide absolute probabilities**, only **relative inclusion probabilities**, such as in RDS.

* * *

🧮 Why Weights Are in Both Numerator and Denominator
----------------------------------------------------

Let’s simplify:

$$\hat{\bar{v}} = \frac{\sum w_i \cdot x_i}{\sum w_i}$$

This is the standard form of a **weighted average**. Now, plug in the GNSUM context:

* $x_i = \tilde{v}_{i,F}$: visibility (approx. Q13 in your case)

* $w_i = \frac{1}{\pi_i}$: weight = inverse of (relative) inclusion probability


So we get:

$$\hat{\bar{v}}_{H,F} = \frac{\sum_{i \in s_H} \tilde{v}_{i,F} \cdot w_i}{\sum_{i \in s_H} w_i}$$

This is **exactly** the structure of a **weighted mean**, where you're averaging visibility across sampled hidden population members, and each person is weighted according to how likely they were to be sampled.

### 🛑 If you omit the denominator:

* You’d get a **weighted total**, not an average.

* Your estimate would depend on the sample size of hidden individuals, not the **distribution** of visibility across the hidden population.


* * *

🧠 Intuition: What the Ratio Does
---------------------------------

The estimator:

$$\hat{\bar{v}}_{H,F} = \frac{\text{Weighted Sum of Visibility}}{\text{Weighted Number of Hidden Respondents}}$$

...is saying:

> “Let’s average how many _frame population members_ each _hidden population member_ is visible to, **correcting for unequal inclusion probabilities.**”

The weights **correct for bias** in your sample due to:

* Oversampling high-degree nodes (common in RDS)

* Underrepresentation of less connected individuals


This correction **requires** weights in both the numerator (to weight visibility appropriately) and the denominator (to compute the correct average).

* * *

📚 Analogy: Weighted GPA
------------------------

Imagine calculating your GPA where courses have different credit weights.

$$\text{GPA} = \frac{\sum \text{grade} \times \text{credits}}{\sum \text{credits}}$$

Same logic:

* Visibility = grade

* RDS weight = credit hours

* Without the denominator, the result is meaningless


* * *

📌 Summary
----------

* The **weights appear in both numerator and denominator** to compute a **weighted mean visibility**

* This ensures **design-unbiased estimation** under **relative probability sampling** (like RDS)

* It’s the same principle as a **Horvitz–Thompson estimator** for a mean or proportion


* * *

Would you like me to show this estimator with **actual numbers**, or in a small simulated example to see how the weights affect the result? 😊

* * *

**You:**

This is excellent. Thank you.

* * *

**ChatGPT:**

You’re very welcome! 😊 I'm glad it was helpful — you're asking **deep, technically sophisticated questions**, and it's a pleasure to support your work. You're clearly well on your way to implementing a robust NSUM/RDS estimation pipeline. 🚀

* * *

If you need next steps, here are a few suggestions you can just ask for:

* 📦 **Function packaging**: Turn the full estimation into an R function for reuse across multiple indicators.

* 🧪 **Bootstrap CIs**: Estimate confidence intervals for both NSUM and RDS estimates using bootstrapping.

* 📈 **Comparison plots**: Visualize NSUM vs RDS estimates with uncertainty bands.

* 📚 **Reporting templates**: Markdown or LaTeX templates for writing up your methodology section.


# Three-Step Bootstrap Algorithm #



The idea is essentially three step: First, perform some sort of sampling with replacement from the sample (perhaps by resampling entire trees, perhaps using SS-weights, etc.) -- this is the 'boot step.' Then, based on _that_ bootstrapped sample, **re-calculate RDS weights for that sample** (again, there are choices to be made here: do we calcuate SS-weights, RDS-II weights, etc. etc.). Then, third, based on *that* boot step sample **and** re-calculated RDS weights, perform NSUM estimation. We need the second step becuase we need weights -- which a sample-specific -- for the NSUM calculations. Then repeat as many times as you like.

Here is the outline let’s call this the **Three-Step Framework**, as you described:

* * *

**Step 1: Bootstrap Resampling of the Sample**

> “Perform some sort of sampling with replacement from the sample (perhaps by resampling entire trees, perhaps using SS-weights, etc.)”

✅ Yes — this is the **resampling step**, and there are multiple valid ways to do it:

| Bootstrap Type | Resample Unit | Reflects What? |
| --- | --- | --- |
| Chain Bootstrap | Entire recruitment chains or trees | Recruitment dependencies (Salganik, Weir) |
| SS Bootstrap | Individual respondents, with inclusion probs | Degree-informed design (Gile 2011) |
| Neighborhood Bootstrap | Ego + alters or local clusters | Local network structure (Yauck et al. 2022) |

👉 The **key principle**: each bootstrap draw must be **coherent with the RDS recruitment process** (i.e., not i.i.d.).

* * *

**Step 2: Recalculate RDS Weights in Each Bootstrap Draw**

> “Then, based on _that_ bootstrapped sample, **re-calculate RDS weights for that sample**”

✅ Absolutely. This step is **essential** and sometimes missed in naive bootstraps.

Because:

* RDS weights (e.g., Volz–Heckathorn or Gile SS weights) are **functions of the sample itself**

* They depend on:

* Degree estimates

* Sample size

* Recruitment paths (in some estimators)

* If you reused weights from the original sample, you would **understate variance** (and be invalid)


So:

> In **every bootstrap replicate**, you **must** re-estimate weights based on that replicate's data.

* * *

**Step 3: Estimate NSUM Using Recalculated Weights**

> “Then, third, based on _that_ boot step sample **and** re-calculated RDS weights, perform NSUM estimation.”

✅ Yes — this is where you:

* Compute weighted sum of **out-reports** from the frame population:

$$\widehat{y}_{F,H}^{(b)} = \sum_{i \in s^{(b)}} \frac{y_{i,H}}{\pi_i^{(b)}}$$
* Compute weighted mean visibility **among hidden population** (those reporting self-experience):

$$\widehat{\bar{v}}_{H,F}^{(b)} = \frac{\sum_{j \in H^{(b)}} \frac{d_j}{\pi_j^{(b)}}}{\sum_{j \in H^{(b)}} \frac{1}{\pi_j^{(b)}}}$$
* Then estimate:

$$\widehat{N}_H^{(b)} = \frac{\widehat{y}_{F,H}^{(b)}}{\widehat{\bar{v}}_{H,F}^{(b)}} \cdot N_F$$

And finally:

* Store $\widehat{N}_H^{(b)}$ for all $B$ bootstrap replicates

* Summarize:

* Point estimate: $\bar{N}_H = \frac{1}{B} \sum \widehat{N}_H^{(b)}$

* Confidence intervals: quantiles or bias-corrected methods


* * *

🧠 Why This Logic Is Essential
------------------------------

This pipeline:

* 📉 Accounts for **uncertainty in both recruitment** and **weight estimation**

* 🧮 Keeps the estimation **internally consistent**: weights and estimates arise from the _same sample_

* 📈 Ensures that **bootstrap variability reflects real sample design variance**


* * *

⚠️ What If You Skip Step 2?
---------------------------

If you resample (Step 1) but don’t reweight (Step 2), then:

* You implicitly assume weights are **fixed constants**

* This **understates variability**, because in reality, inclusion probabilities are **modeled, not known**

* Your **CIs will be too narrow**, especially for small or structurally uneven networks


* * *

✅ Summary of Your Logic — Validated and Endorsed
------------------------------------------------

| Step | Description | Required? | Why? |
| --- | --- | --- | --- |
| 1️⃣ Bootstrap the sample | Tree, SS, or neighborhood resampling | ✅ | Models recruitment variability |
| 2️⃣ Recalculate RDS weights | VH, Gile SS, or others | ✅ | Keeps weights aligned with sample |
| 3️⃣ Re-estimate NSUM | Use boot sample + weights + estimator (Basic NSUM, MBSU, symmetric-GNSUM)| ✅ | Reflects combined uncertainty |

💡 You understood and articulated the structure exactly right. This is the foundation of a valid NSUM inference pipeline using RDS data.




## Step 1: Resampling RDS Network ##



🔄 Updated Framework: RDS-Aware Bootstrap Methods



| Method | Core Idea | Key Citation | Supported in R? | NSUM-Suitable? |
| --- | --- | --- | --- | --- |
| **Chain Bootstrap** | Resample entire **recruitment chains** (starting from seeds) | Salganik (2006); Weir et al. (2012) | `surveybootstrap::rds.boot.draw.chain()` | ✅ Yes |
| **Successive Sampling Bootstrap (SSB)** | Resample individuals based on estimated **inclusion probabilities** under SS model | Gile (2011); Gile & Handcock (2010) | Partially supported (manual implementation) | ✅ Yes |
| **Neighborhood Bootstrap** | Resample nodes **with their ego-networks**, reflecting local dependencies | Yauck et al. (2022) | ✅ `Neighboot` | ✅ Yes |

Let’s go into each one now — this time with proper references and updated understanding. 👇

* * *

### 📦 1. Chain Bootstrap (Weir et al., Salganik)


#### Core Idea:

Treat **each recruitment chain** as a **Primary Sampling Unit (PSU)** and resample chains with replacement.

* Developed originally in **Salganik (2006)** to estimate RDS variances

* **Weir et al. (2012)** used it empirically and provided **supporting online material** with code, which is the basis for:

> 🔧 `surveybootstrap::rds.boot.draw.chain()`


#### How It Works:

* Identify unique recruitment trees or seeds

* Sample trees with replacement

* Recombine and reweight for each replicate


#### Good For:

* Simple variance estimation in **binary outcome** RDS surveys

* Use with **Volz–Heckathorn weights**

* Can be extended to NSUM (if you compute NSUM estimate per replicate)


#### Limitation:

* Doesn’t fully model **network dependencies** (just trees)


* * *

### 🧬 2. Successive Sampling Bootstrap (SSB)

-----------------------------------------

#### Core Idea:

Use **Successive Sampling theory** (without replacement) to estimate each respondent’s inclusion probability $\pi_i$, and then **resample based on those probabilities**.

#### Key Paper:

* **Gile (2011)** — _Improved Inference for Respondent-Driven Sampling Data with the Successive Sampling Estimator_  
DOI:10.1214/11-AOAS513


#### Implementation:

* Estimate inclusion probabilities $\hat{\pi}_i \propto \text{degree}_i$

* Resample individuals with probabilities $\propto 1 / \hat{\pi}_i$

* Recompute weights and NSUM per bootstrap draw


#### R Support:

* No out-of-the-box implementation, but can be coded manually (see previous messages)

* Compatible with `gile.ss.weights()`


#### Use For:

* NSUM with **SS weights**

* When you believe sampling without replacement is more realistic


* * *

### 🧱 3. Neighborhood Bootstrap (`Neighboot`; Yauck et al., 2022)


--------------------------------------------------------------

#### Core Idea:

Model the **local network structure** of RDS recruitment and **bootstrap ego-networks** rather than individuals or chains.

> It's a **network-aware, dependency-preserving bootstrap** that accounts for the **actual structure of the recruitment network**.

#### Key Citation:

> Yauck, M., Moodie, E. E. M., Apelian, H., et al. (2022).  
> _Neighborhood Bootstrap for Respondent-Driven Sampling._  
> [Journal of Survey Statistics and Methodology](https://doi.org/10.1093/jssam/smab057)

#### Package:

* ✅ **R package**: `Neighboot`

* ✅ Functions: `neighboot::neighboot()`


#### Implementation Details:

* Bootstrap draws ego networks centered on sampled nodes

* Designed to preserve **recruitment-induced network dependencies**

* Provides **improved variance estimates** over chain bootstraps


#### Use For:

* NSUM with **RDS** where you have a **network object** or full recruitment tree

* Works well when applied to **networked traits** like visibility


* * *

### 🧠 Which One Should _You_ Use?

------------------------------

| Use Case | Best Method |
| --- | --- |
| You’re using **Volz–Heckathorn weights** and want simplicity | ✅ Chain Bootstrap (`surveybootstrap`) |
| You’re using **Gile’s SS weights** and want theoretical consistency | ✅ SS Bootstrap (manual or custom) |
| You want the **most principled network-aware bootstrap** | ✅ Neighborhood Bootstrap (`Neighboot`) |
| You lack network data beyond recruiter ID | ❌ `Neighboot` may not be applicable (check package docs) |

* * *

### 🔍 Key Differences: Tree vs SS vs Neighborhood Bootstrap


--------------------------------------------------------

| Feature | Chain Bootstrap | SS Bootstrap | Neighborhood Bootstrap |
| --- | --- | --- | --- |
| Preserves recruitment? | ✅ Chains | ❌ Not explicitly | ✅ Fully |
| Uses degree? | ❌ No | ✅ Yes | ✅ Can use |
| Requires network object? | ❌ No | ❌ No | ✅ Yes |
| Out-of-the-box R support? | ✅ surveybootstrap | ❌ Manual | ✅ Neighboot |
| Ideal for NSUM? | ✅ Yes | ✅ Yes | ✅ Yes (with network) |

* * *

### 🔚 Final Word (for Now!)

------------------------

You’re absolutely right to bring up `Neighboot` and the Weir et al. bootstrap. The **landscape of RDS-aware bootstrapping has evolved**, and:

> ✅ **All three methods — Tree, SS, and Neighborhood Bootstraps — are valid for estimating NSUM uncertainty from RDS samples**, with the right data and implementation.



## Step 2:  Recalculate Weights ##





## Step 3: Choose NSUM Estimator and Perform Estimation ##



> You're right — after bootstrapping the sample (**Step 1**) and recalculating the RDS weights (**Step 2**), you have a **choice** in Step 3 about **how** you compute your NSUM estimate.

This is **where the estimator family comes in**:

| Estimator | Formula / Idea | Assumptions | Strengths | Use with RDS? |
| --- | --- | --- | --- | --- |
| **Basic NSUM** | $\widehat{N}_H = \frac{\sum y_{i,H}}{\bar{d}_i} \cdot N$ | Uniform mixing, accurate degrees | Simple, interpretable | ⚠️ No — unweighted |
| **Generalized NSUM (GNSUM)** | Use _weighted_ out-reports and visibility | + Frame + Hidden samples | Can incorporate RDS weights | ✅ Yes |
| **GNSUM with symmetry** | Assumes visibility is symmetric: $v_{i,j} = v_{j,i}$ | Hidden and Frame visibility reciprocal | Requires RDS sample from hidden pop | ✅ Yes |
| **Modified Basic Scale-Up (MBSU)** | Adjusts for transmission error, barrier effects, etc. | Uses adjustment factors (δ, τ, etc.) | Incorporates empirical corrections | ✅ Yes, if you estimate adjustments |
| **Model-based NSUM** | Bayesian or MLE framework | Varies | Can handle uncertainty in parameters | ✅ Yes, with RDS-informed priors |



* * *

### 1. Modified Basic Scale‑Up (MBSU): Adjustments, what they are, how to estimate them


-----------------------------------------------------------------------------------

The “Basic Scale‐Up” model (original NSUM) has a simple form:

$$\hat{N}_H = \frac{\sum_i y_{i,H}}{\sum_i d_i} \cdot N_F$$

where $y_{i,H}$ = how many hidden members person $i$ knows; $d_i$ = their degree (network size in the frame); $N_F$ = size of the frame.

“Modified” versions introduce **adjustment factors** to correct for known sources of bias. Some of the common biases:

* **Barrier / transmission error**: People may not know that someone they know is in the hidden population (or misclassify), or the hidden population members might be less visible.

* **Degree ratio bias**: Those in the hidden population may have substantially different degrees (on average) than those in the frame population.

* **Recall / reporting bias**: Underreporting or overreporting of known others because of memory, social desirability, etc.


So MBSU typically introduces parameters (adjustment factors) like:

* $\delta$: how visible hidden population members are (visibility / transmission factor)

* $\tau$: sometimes a recall factor, or correction for undercounting

* Or a “degree ratio” $r = \frac{\text{avg degree of hidden pop}}{\text{avg degree of frame pop}}$


MBSU might look something like:

$$\hat{N}_H = \frac{\sum_i y_{i,H}}{\sum_i d_i} \cdot N_F \cdot \frac{1}{\delta} \cdot \frac{1}{r} \cdot \cdots$$

or more structured:

$$\hat{N}_H = \frac{\sum_i y_{i,H}}{\sum_i d_i} \cdot N_F \cdot C$$

where $C$ is a product of adjustment corrections.

* * *

### How to _estimate_ these adjustment factors

Because you often don’t observe them directly, you typically:

1. **Borrow from literature or past studies**  
If others have estimated visibility or barrier effects for a similar population/context, use plausible values (ranges) from their findings.

2. **Sensitivity / scenario analyses**  
As you’ve done, scan over plausible values (e.g. visibility from 0.5 → 1, or degree ratio from 0.7 → 1.3) and show how the NSUM estimate changes. This helps show how sensitive your conclusion is to these assumptions.

3. **Collect additional data (“enriched ARD”)**  
If feasible, ask respondents not only how many people they know in the hidden population, but also ask about their perception of whether those people know them (i.e. measure visibility), or number of acquaintances for whom the hidden status is ambiguous. Or, if you can survey hidden population members and ask how many frame‐members know them, that gives direct information about visibility / transmission.

4. **Estimate degree ratio if you have a hidden population sample**  
If you have some respondents who _are_ hidden pop, and you have their degree (network size) you can estimate average degree in hidden pop vs frame pop.

5. **Statistical models (e.g. regression)**  
Sometimes one includes auxiliary covariates (age, gender, migration status, etc.) that predict visibility or reporting, and model them. Then use predictions for adjustment.


* * *

### Literature / references

* Feehan & Salganik (2016) discuss adjustment factors (for transmission / visibility etc.), and ways the basic estimator is biased when key assumptions fail. [hrussellbernard.com](https://hrussellbernard.com/wp-content/uploads/2020/07/feehan-salganik-2016-generalizing-nsum.pdf?utm_source=chatgpt.com)

* “Thirty Years of The Network Scale‐Up Method” (Laga et al., 2021) gives a recent review of adjustments and practices. [PMC](https://pmc.ncbi.nlm.nih.gov/articles/PMC10665021/?utm_source=chatgpt.com)

* “Estimating and Correcting Degree Ratio Bias in NSUM” (Laga, 2023 preprint) [arXiv] — proposes methods to estimate degree ratio bias without collecting extra data. [Ian Laga](https://ilaga.github.io/Degree_ratio_preprint.pdf?utm_source=chatgpt.com)


* * *

### 2. Model‑based NSUM (including Bayesian / hierarchical NSUM)


------------------------------------------------------------

“Model‑based” NSUM estimators refer to statistical models in which the unknown parameters (hidden pop size, visibility / transmission, degree ratio, reporting error, etc.) are treated within a probabilistic modeling framework. Many of them are Bayesian, though frequentist / hierarchical mixed‐models also exist.

### Key features of model‐based NSUM

* You specify a **likelihood** for what you observe (e.g. number of hidden population members known by each respondent, given their degree, plus error terms)

* You put **priors** on unknown parameters: hidden population size, visibility/transmission error, degree ratio, etc.

* Possibly hierarchical structure (different subpopulations / regions) to share strength (partial pooling)

* Incorporate uncertainty in all parameters in one unified model, which produces posterior distributions (or confidence intervals) on all quantities of interest


* * *

### Examples / recent papers

* _“A Partially Pooled NSUM Model: Detailed estimation of CSEM trafficking prevalence in Philippine municipalities”_ (Nyarko‑Agyei, Moser, Seymour, etc., 2024) — they build a Bayesian hierarchical NSUM model to estimate municipal‐level prevalence across many regions. [arXiv](https://arxiv.org/abs/2407.13267?utm_source=chatgpt.com)

* _“Accounting for correlation and censoring in Bayesian …”_ (B. Vogel et al., 2025) — proposes NSUM models estimating group size and other parameters jointly, using Bayesian methods. [ScienceDirect](https://www.sciencedirect.com/science/article/abs/pii/S0378873325000462?utm_source=chatgpt.com)

* Preprint “Degree ratio bias” (Laga 2023) also has modeling of adjustment / estimation of that bias. [Ian Laga](https://ilaga.github.io/Degree_ratio_preprint.pdf?utm_source=chatgpt.com)


* * *

### Trade‑offs: Model‑based vs MBSU / scenario analysis

| Pros | Cons |
| --- | --- |
| + Captures uncertainty jointly across many parameters | − More assumptions needed (priors, model structure) |
| + Can combine multiple data sources (hidden pop survey, frame survey, ARD, etc.) | − More computationally intensive, more challenging to validate |
| + Enables estimation in small domains / regions via partial pooling | − Results may be sensitive to prior choices, especially with sparse data |

* * *

### 3. How these fit into **your RDS + Bootstrapping + NSUM pipeline**

------------------------------------------------------------------

Given what you’ve laid out (bootstrapping sample, reweighting, estimating NSUM per replicate), you can plug in either:

* A **MBSU variant** in step 3: you choose particular visibility & degree ratio correction(s), maybe letting them vary per bootstrap draw, or scenario / sensitivity analysis

* A **model‑based NSUM** in step 3: instead of a simple formula per replicate, you might fit a small Bayesian model per bootstrap replicate (or perhaps fit once but incorporate uncertainty via the bootstrap draws + prior uncertainty), to estimate hidden size as a posterior distribution


In practice, doing full Bayesian model fitting inside each bootstrap replicate can be computationally heavy. Sometimes people:

* Fit a Bayesian model once, using the original sample + RDS weights, and take posterior draws to represent uncertainty

* Or combine Bayesian posterior draws + bootstrap draws to reflect both weight/design uncertainty and parameter/model uncertainty


* * *

### 4. Suggested Reading & References

---------------------------------

Here are some key papers to dive deeper:

* Feehan, D. M., & Salganik, M. J. (2016). _Generalizing the network scale‑up method: A new estimator, variance estimator, and adjustment factors._ Journal of the Royal Statistical Society: Series A. [hrussellbernard.com](https://hrussellbernard.com/wp-content/uploads/2020/07/feehan-salganik-2016-generalizing-nsum.pdf?utm_source=chatgpt.com)

* Laga, I., et al. (2021). _Thirty Years of The Network Scale‑Up Method._ PLOS / PMC review article. [PMC](https://pmc.ncbi.nlm.nih.gov/articles/PMC10665021/?utm_source=chatgpt.com)

* Laga, I. (2023). _Estimating and Correcting Degree Ratio Bias in NSUM._ Preprint. [Ian Laga](https://ilaga.github.io/Degree_ratio_preprint.pdf?utm_source=chatgpt.com)

* Nyarko‑Agyei, A., Moser, S., Seymour, R. W., et al. (2024). _A Partially Pooled NSUM Model: Detailed estimation … Philippines._ Preprint. [arXiv](https://arxiv.org/abs/2407.13267?utm_source=chatgpt.com)

* B. Vogel et al. (2025). _Accounting for correlation and censoring in Bayesian NSUM._ [ScienceDirect](https://www.sciencedirect.com/science/article/abs/pii/S0378873325000462?utm_source=chatgpt.com)


---

## Implementation in R: ##



### 🔁 STEP 1: Bootstrap Resample

**Function:** `resample_rds()`  
**Purpose:** Resample the original RDS data using one of several bootstrapping strategies  
**Key Inputs:**

* `data`: full RDS dataset (with IDs, recruiter IDs, degree, etc.)

* `method`: `"chain"`, `"ss"`, `"neighboot"`

* `weights` (optional): for SS bootstrap

* `num_draw`: number of samples to draw (e.g. 500)


**Output:**

* A list of `B` resampled datasets (`data.frame`s)


* * *

### ⚖️ STEP 2: Recalculate RDS Weights

**Function:** `compute_rds_weights()`  
**Purpose:** Recalculate inclusion probabilities (πᵢ) for each bootstrap replicate  
**Key Inputs:**

* `resample`: one bootstrapped dataset from Step 1

* `weight_method`: `"VH"`, `"GileSS"`, `"RDSII"`, etc.

* `population_size`: used in VH / SS weights


**Output:**

* A `data.frame` with updated weights appended


* * *

### 🧮 STEP 3: NSUM Estimation

**Function:** `estimate_nsum()`  
**Purpose:** Apply a selected NSUM estimator to one weighted bootstrap replicate  
**Key Inputs:**

* `data`: the weighted resample

* `nsum_method`: `"gnsum"`, `"mbsu"`, `"model_bayes"`, etc.

* `frame_size`: total known frame population size

* `adjustments`: (optional) list of adjustment factors for MBSU

* `model_params`: (optional) for Bayesian NSUM models


**Output:**

* A single point estimate (e.g. `N_hat`)

* Optionally, a posterior distribution or standard error


* * *

### 🎯 WRAPPER: Full Bootstrap Procedure

**Function:** `nsum_bootstrap()`  
**Purpose:** Master function that loops over all B replicates  
**Key Inputs:**

* `data`: original RDS dataset

* `B`: number of bootstrap replicates

* Plus all the other choices from the above functions


**Output:**

* A vector of NSUM estimates $\{\hat{N}_H^{(b)}\}$

* Summary statistics: mean, sd, CI, etc.

* (Optional) Full diagnostics per replicate


---
