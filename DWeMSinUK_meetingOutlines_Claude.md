

Meeting Date: September 14, 2023
Speakers: SJM (Scott), CE (Caroline), SY (Selim)

Agenda:
- Discuss utilizing bootstrapping techniques for calculating uncertainty estimates around exploitation estimates
- Clarify understanding of structure of network data from survey (node1, node2 representations)
- Explain methodology behind network bootstrap techniques
- Consider timeline for progress given teaching constraints, division of labor

Key Decisions:
- Use neighborhood bootstrap method rather than tree bootstrap method
- Automate Steps 1-5: Equations from GNSUM protocol to calculate exploitation estimate including known populations (Step 4), averages (Step 5)
- Explore creating a risk index variable based on a composite of survey questions related to exploitation


Action Items:
- SJM: Consider incorporating second known population group (number of Filipino domestic workers) for subset estimates
- SY: Send adjacency matrix file showing referral network and automate Steps 1-5 GNSUM calculations
- CE: Select survey questions to include in proposed risk index

Discussion Points:
- Details of network bootstrap methodology and necessary data structure
- Limitations around sample representativeness
- Suggest comparing estimates from neighborhood bootstrap method to estimates from scale-up method as a sanity check

Attachments:
- Sample adjacency matrix data file (contents not included)

Next Steps:
- Automate Steps 1-5 GNSUM calculations to generate exploitation estimate
- Implement network bootstrap methodology
- Push progress on methods before teaching starts again in October

Meeting Date: October 20, 2023
Speakers: SY (Selim), CE (Caroline), SJM (Scott)

Agenda:
- Address SY’s questions regarding GNSUM calculations and output interpretations
- Discuss process for using bootstrap to obtain variance estimates and confidence intervals

Key Decisions:
- Use scale-up method with known population sizes of domestic workers for denominator
- Create automations for Steps 1-5 GNSUM calculations for overall sample and Filipino subset

Action Items:
- SY: Automate Steps 1-5 GNSUM calculations
- SJM: Work on implementing network bootstrap

Discussion Points:
- Interpretation of output from GNSUM calculations
- Use of overseas domestic worker population and National Referral Mechanism statistics as known population sizes
- Overview of bootstrap methodology to obtain uncertainty estimates

Attachments:
- None

Next Steps:
- Automate Steps 1-5 GNSUM calculations
- Implement network bootstrap methodology
- Reconvene meeting in mid-December 2023 to reassess progress


Meeting Date: November 10, 2023

Summary:
- Clarification that all survey respondents indicated exploitation to some degree
- CE asks if simple statistics could be run to sense-check high resulting proportion
- SJM suggests checking the sample proportion with modern slavery indicators vs fancy methods initially
- Discussion of policy relevance and timeline constraints
- SJM explains Bayesian perspective to obtain distribution vs frequentist point estimates
- Considerations for measuring distribution of risk vs total risk
- Update on exploring RDS and scale-up methods showing promise for contribution

SJM To Do List:

1. Send variables to be used to dichotomize modern slavery indicator
   - Use NRM indicator and forced labour indicators
   - Send questions to SY

2. Obtain proportion estimate this week using off-the-shelf RDS methods
   - Produce estimate with uncertainty
   - Compare to initial numbers as sanity check

3. Consider options for quantifying distribution of risk index
   - Review additional RDS papers
   - Assess utility to readers
   - Discuss limitations and pioneering aspects

4. Pursue approach combining RDS and scale-up
   - Aim to account for non-random sampling
   - Review dial and Saul papers for guidance

5. Circulate proportion estimate results

6. Book mid-December meeting once marking completed




Meeting Date: November 10, 2023
Speakers: CE (Caroline), Speaker 1 (potentially SY), Speaker 3 (unidentified)

Agenda:
- Discuss current status and next steps for research project
- Provide update based on CE’s summary email
- Discuss use of literature papers (Nook, Guile) in review
- Clarify questions around risk assessment variables

Key Decisions:
- Include both Nook papers in literature review
- Base risk assessment groupings on composite measures from survey questions and sub-indicators

Action Items:
- None discussed

Discussion Points:
- All survey respondents indicated some exploitation based on risk assessment variables
- Interest and questions on how risk assessment variables were coded

Attachments:
- Summary email from CE (contents not included)

Next Steps:
- Finalize coding of risk assessment variables
- Share latest risk coding with team to review

Meeting Date: December 8, 2023
Speakers: CE (Caroline), SY (Selim), SJM (Scott)

Agenda:
- Discuss research progress and next steps
- Consider timeframes and objectives

Key Decisions:
- Continue working to estimate size of hidden domestic worker population experiencing modern slavery in UK using generalized network scale-up methods
- Fully utilize SY’s time through the end of December

Action Items:
- SY to contribute to background research and writing
- CE to consolidate meeting notes into draft paper

Discussion Points:
- Research has potential for policy impact given upcoming government review
- Discussed limitations around non-random sample
- Suggest comparing estimates across methods as a sanity check

Attachments:
- None discussed

Next Steps:
- Continue with calculations using existing data
- Draft initial paper consolidating methods and findings thus far
- Reconvene meeting in January 2024 to reassess progress


Meeting Date: December 8, 2023

Summary:
- Pleasantries and health update (CE recovering, SJM and SY doing well)
- CE thanks SJM for draft paper and notes it captures useful ideas while needing more polish
- Sections needing SJM input: methodology details, results, data analysis
- SY confirms availability to contribute network structure graphic and needs guidance on estimating uncertainty
- Discussion around target journal, audience, and narrative
- Consider converting draft to LaTeX for notation and equations
- Propose draft completion timeline with next meeting in early January

SJM To Do List:

1. Add detailed methodology section
   - Include specifics on data analysis procedures performed
   - Overview of calculations utilized
   - Explain bootstrap process
   - Describe handling of non-random sampling

2. Provide guidance to SY on adding:
   - Network structure graphic
   - Details on point estimates and confidence intervals

3. Consider target journal and refine framing/context appropriately

4. Offer to share literature summaries on methods to assist CE in drafting background

5. Upon draft completion, convert document to LaTeX format

6. Submit availability for early January meeting to finalize draft
