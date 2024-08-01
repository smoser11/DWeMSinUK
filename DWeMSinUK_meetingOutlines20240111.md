To create a more detailed and comprehensive summary of each meeting, including extensive sub-items and specific information from the uploaded files, I'll expand upon the earlier summary. I'll also include detailed discussions between yourself and CE regarding conceptualizing and measuring experiences of modern slavery.

#### Expanded Meeting Summary on September 14, 2023
- **Agenda & Discussion Topics**: Focus on bootstrapping techniques and methodology for estimating the number of domestic workers experiencing modern slavery.
  - **Sub-items**:
    - Detailed discussion on the generalized network scale-up method (GNSUM).
    - Exploration of neighborhood bootstrap techniques.
- **Key Decisions**:
  - Adoption of neighborhood bootstrap method.
  - **Sub-items**:
    - Decision to create a unique ID system for data tracking.
    - Emphasis on the importance of accurate network data structure.
- **Action Items**:
  - SJM: Oversee the incorporation of a second known population group.
  - **Sub-items**:
    - SY: Automate GNSUM calculations.
    - CE: Select and finalize survey questions related to modern slavery indicators.
- **Discussion Points**:
  - Importance of representing the sample accurately in the network.
  - **Sub-items**:
    - Discussion on the potential biases in sample selection.
    - Comparison of current estimates with previous studies.
- **Next Steps**:
  - Automate GNSUM calculations.
  - **Sub-items**:
    - Implement network bootstrap methodology.
    - Begin preliminary analysis using automated calculations.

#### Expanded Meeting Summary on October 20, 2023
- **Agenda & Discussion Topics**: Addressing technical questions on GNSUM, focusing on bootstrap for variance estimation.
  - **Sub-items**:
    - In-depth discussion on the implementation of bootstrap methodology.
    - Consideration of overseas domestic worker visa data for known population sizes.
- **Key Decisions**:
  - Utilization of scale-up method.
  - **Sub-items**:
    - Agreement to automate GNSUM steps for overall and specific population samples.
- **Action Items**:
  - SJM: Implement network bootstrap and oversee the recalculations.
  - **Sub-items**:
    - SY: Complete the automation of GNSUM steps.
- **Discussion Points**:
  - Interpretation of GNSUM output.
  - **Sub-items**:
    - Concerns about the representativeness of the overseas domestic worker data.
- **Next Steps**:
  - Continue with automation of GNSUM.
  - **Sub-items**:
    - Implement and test network bootstrap methods.

#### Expanded Meeting Summary on November 10, 2023
- **Summary**: Focus on survey exploitation indications and policy relevance, exploring Bayesian methods.
  - **SJM To-Do List**:
    - Send variables for modern slavery indicator.
    - **Sub-items**:
      - Obtain proportion estimates using Bayesian methods.
      - Consider different approaches for quantifying the risk index.
      - Circulate results of proportion estimates among the team.

#### Expanded Meeting Summary on December 8, 2023
- **Agenda & Discussion Topics**: Research progress, timeframe considerations, and paper drafting.
  - **Key Decisions**:
    - Continue with GNSUM methods.
    - **Sub-items**:
      - Ensure full utilization of SY's time for research and drafting.
  - **Action Items**:
    - SY: Contribute to background research.
    - **Sub-items**:
      - CE: Consolidate notes into a coherent draft.
  - **Discussion Points**:
    - Potential policy impacts.
    - **Sub-items**:
      - Discussion of sample limitations and method comparison.
  - **Next Steps**:
    - Continue

Continuing from where we left off:

#### Expanded Meeting Summary on December 8, 2023 (Cont.)
- **Next Steps**:
  - Draft the initial paper focusing on GNSUM methodologies and results.
  - **Sub-items**:
    - Prepare the draft for early January review.
    - SJM to provide guidance on including detailed methodology, data analysis, and calculation overviews in the draft.
    - Consider the potential policy impacts and sample limitations in the discussion section of the paper.

### SJM's Comprehensive To-Do List (Expanded):
1. **Methodology Detailing**: Add comprehensive details on data analysis procedures and calculation methods.
   - Include a description of the bootstrap process and handling of non-random sampling.
   - Guide SY on incorporating a network structure graphic and estimating uncertainty.
2. **Paper Framing and Context**:
   - Refine the framing and context of the paper for the target journal.
   - Share literature summaries on various methods to assist CE in drafting.
3. **Drafting and Formatting**:
   - Convert the completed draft into LaTeX format, ensuring technical accuracy and readability.
4. **Coordination and Scheduling**:
   - Submit availability for the early January meeting.
   - Schedule regular check-ins to monitor progress and address any emerging issues.

This expanded summary provides a detailed and comprehensive overview of the meetings, including specific actions and discussions around conceptualizing and measuring experiences of modern slavery. It also outlines a clear set of next steps and action items for you (SJM) to follow, ensuring the project progresses effectively.


---


Thank you for sharing the additional emails. Here is an updated summary incorporating the new information:

**August 21, 2023**
- SY confirms he has exported the survey data and codebook to share with SJM
- Meeting scheduled for August 22

**August 22, 2023**
- SJM asks about sampling weights and primary sampling units
- CE provides thoughts on estimating sampling weights and identifying domestic workers as primary sampling unit

**October 11, 2023**
- SY shares R script with initial coding of GNSUM calculation steps
- Requests feedback from SJM, especially on steps 9-10

**October 19, 2023**
- SJM asks SY to clarify meaning of 'node1' and 'node2' in data
- Requests SY indicate which R script sections calculate which quantities
- Meeting scheduled for October 20

**November 3, 2023**
- CE proposes revised approach focusing only on respondents reporting abuse
- Requests SY update data coding:
 - Insert columns for 13 risk categories and assign weights
 - Sum categories into overall risk index from 0 to 1
 - Add column indicating any exploitation
 - Share updated data set
- Meeting scheduled for November 10

**November 10, 2023**
- SJM explores proportion estimates of population experiencing modern slavery
- Finds estimates around 30-40%, seems high
- Shares initial analysis of risk index distribution
- Parking analysis for now due to teaching load
- Requests GitHub setup for code sharing (SY)
- Plans to circle back in mid-December

**Updated To Do List for SJM:**

1. Send forced labour indicators for modern slavery classification (Q36, Q80)

2. Generate proportion estimate this week using off-the-shelf RDS methods

3. Consider options to quantify distribution of risk index

4. Look into combining RDS and NSUM estimators

5. Share RDS proportion estimate results

6. Book mid-December meeting to reassess progress

Let me know if you need any clarification or have additional suggestions in light of the new information provided!


---



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

Please let me know if the added details and specifics help provide clarification or if you have any other suggestions for improvement. Thank you again for your guidance - it is very helpful for strengthening my analytical skills.


Map Gile papers to CRAN/ R: packages/ functions.
