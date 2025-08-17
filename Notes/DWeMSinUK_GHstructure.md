
# Running Priorities: #


Based on the documents and upcoming meeting, here are the key priorities to focus on:

Technical Analysis Steps


Implement neighborhood bootstrap for RDS inference
Apply equations from Feehan & Salganik 2016 to combine RDS/NSUM approaches
Compare corresponding survey questions between RDS and NSUM methods:

Q70/Q71 (document withholding)
Q39+Q42/Q43 (pay issues)
Q45+Q47+Q48/Q49 (abuse/threats)
Q61+Q62/Q64 (excessive hours)
Q78/Q79 (access to help)




Population Parameters


Use ~980,000 as UK domestic worker population estimate (EU data)
Use 44,360 as NRM adult referrals baseline
Address treatment of "don't know" responses and zero network size claims
Consider separate analysis for Filipino subgroup


Risk Index Implementation


Clean coding for 13 risk categories with proper weightings
NRM referral (0.35)
Forced labor indicators (0.55 total)
Below minimum wage (0.10)


Draft Development


Update methods section with technical details

Add results from statistical analysis once complete

Position for IJOPM special issue deadline (Sept 2025)

Consider addressing how multiple estimation approaches increase confidence



----



Based on the provided files, here's a suggested organization and workflow:
1. Create a clean project structure:

DWeMSinUK/
├── R/
│   ├── data_processing/
│   │   ├── 01-data_cleaning.R
│   │   └── 02-data_preparation.R
│   ├── analysis/
│   │   ├── 03-rds_estimation.R
│   │   ├── 04-bootstrap_analysis.R 
│   │   └── 05-nsum_estimation.R
│   └── utils/
│       └── helper_functions.R
├── data/
│   ├── raw/
│   └── processed/
└── output/
    ├── figures/
    └── tables/

2. Consolidate existing files:
- Merge common functionality from various versions of similar files
- The provided files show overlapping code for RDS analysis and bootstrapping
- Centralize shared functions (e.g., calculateOutDegreeIncludingZeros) in helper_functions.R
3. Suggested workflow:
a. Data Processing:
r
# 01-data_cleaning.R
- Import raw data
- Clean column names
- Handle missing values
- Basic data validation
- Export cleaned data
# 02-data_preparation.R
- Create network variables
- Calculate degree measures
- Prepare RDS format data
- Export analysis-ready datasets

b. Analysis:
r
# 03-rds_estimation.R
- RDS.SS.estimates
- RDS.I.estimates
- RDS.II.estimates
- Model Assisted
- Model comparisons
- 
# 04-bootstrap_analysis.R
- Neighborhood bootstrap
- Tree bootstrap
- Confidence intervals
# 05-nsum_estimation.R
- Network scale-up estimates
- Hidden population size estimates

4. Version Control:
- Use Git for tracking changes
- Create meaningful commit messages
- Use branches for different analysis approaches
5. Documentation:
- Add README.md explaining project structure
- Comment code thoroughly
- Document data processing decisions
- Track analysis decisions and results