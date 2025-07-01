Diagnostics for respondent-driven sampling
K. J. Gile, L. G. Johnston and M. J. Salganik
J. R. Statist. Soc. A, 178 (2015), 241 -- 269


This document describes the R code to produce the analyses in the paper.

Unfortunately, the data are not available for distribution.  We will, however, describe the structure of the data so that users can understand what we did and, if they wish, adapt the code or their data accordingly.

Our data were from 12 studies:  each of 3 populations surveyed in each of four cities in the Dominican Republic.  Primary data cleaning resulted in one .RData file for each study, with naming convention:  "gXpYv03.RData", where "X" takes values in {1,2,3} indexing the populations, "Y" takes values in {1,2,3,4} and indexes the cities.  Each file contains a single data.frame object name DF.  DF contains one row for each survey respondent, and a large number of data fields.  NOTE:  The rows are ordered in the chronological order of the surveys.  Not all data fields are present in across populations, and occasionally the same question has different numbers in different populations.   In most analyses, the data fields used are obvious from the context.  Here are a few that may not be obvious:

IsSeed:		Takes value "1" for Seeds, "0" otherwise
Wave:		Wave of recruitment
UsedX:		Where X is one of {1,2,3}, Indicator of whether the Xth coupon was returned
NChildren:  	The number of successful recruitments

Variable names ending in "r" were answered on the "return" questionnaire

The four primary social network questions, labeled with letters in the paper are:
P201 (D)
P202 (E)
P203 (F)
P204 (G)

Code to reproduce our analyses are divided into several files.  There are three data-cleaning/general function files, at least one of which is called before each section of specialized code.  Specialized code is grouped thematically, so that one file may cover related sections of both the main paper and supporting information.

General files:

prepare_functions.R
load_and_prepare_dr_data.R
reproduce_functions.R

Specialized Code by file:  

reproduce_sample_size.R - Figure 2
WithReplacement.R - Figures 3, S1, S2, S3
VHandSS.R - Figure 4, S4, S5, Table S1
reproduce_convergence.R - Figure 6(a), 7(a), S6
reproduce_bottleneck.R - Figure 6(b), 7(b), S7, S8
reproduce_allpoints.R - Figure 7(c) 
Reciprocation.R - Table 3, Section S3
TimeDynamics.R - Figure S9, S10, S11
reproduce_network_size.R - Section 7.2, Figure 8, Figure S12, S13
ParticipationBias.R Figure 9, 10, 11, Table 4, 5, 6, S3
NonresponseFollowup.R Table S4, S5

Code by paper section:

** Section 1:
Figure 1 (rec. tree) - constructed separately using Pajek
** Section 2:
(no code)
** Section 3:
Figure 2:  reproduce_sample_size.R
** Section 4:
4.2, Figure 3:  WithReplacement.R
4.3,  WithReplacement.R
4.4, Figure 4:  VHandSS.R
** Section 5:
Figure 6,7:  reproduce_convergence.R, reproduce_bottleneck.R, reproduce_allpoints.R
** Section 6:
Table 3:  Reciprocation.R
** Section 7: 
7.1  TimeDynamics.R
7.2  reproduce_network_size.R
7.3 Figure 8:  reproduce_network_size.R
** Section 8:
8.1, Figure 9: ParticipationBias.R
8.2, Figure 10:  ParticipationBias.R
8.3, Table 4:  ParticipationBias.R
8.4, Table 5, 6, Figure 11:  ParticipationBias.R

Supporting Information:

**S1:  
S1.1, Figure S1,S2:  WithReplacement.R
S1.2, Figure S3:  WithReplacement.R
S1.3, Figure S4,S5, Table S1:  VHandSS.R
**S2:
Figure S6:  reproduce_convergence.R
Figure S7:  reproduce_bottleneck.R
Figure S8:  reproduce_bottleneck.R
** S3:
(numbers in text) Reciprocation.R
** S4:
S4.1, Figure S9, S10, S11: DegreeTimeDynamics.R
S4.2, Figure S12, S13: reproduce_network_size.R
** S5:
Table S3:  ParticipationBias.R
** S6:
Table S4, S5:  NonresponseFollowup.R


Krista J. Gile
Department of Mathematics and Statistics
University of Massachusetts
Amherst
MA 01003-9305
USA

E-mail: gile@math.umass.edu


