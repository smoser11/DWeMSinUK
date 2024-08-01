# Replication files for Feehan et al. (2015) "Blah blah" . . .  

# NOTE: be that your working directory is set to the directory containing
#       this file (reproduce.r) before running the rest of the code

source(file.path("code", "se-rwanda-prep.r"))
# takes the raw dataset and produces the analysis dataset that gets used by other files
# creates 'data/se-rwanda-data.RData'

source(file.path("code", "se-rwanda-bootstrap.r"))
# takes all of the bootstrap resamples
# requires about one day to run
# creates `out/rw-bootstrap-resamples.RData`

source(file.path("code" , "se-rwanda-degree-estimates.r"))
# produces Figure 2
# produces Web Figure 1
# produces `avg_degree_estimates.log` which has the point estimates we mention in the paper

source(file.path("code", "se-rwanda-ic.r"))
# summarizes and plots the results of the internal consistency checks
# produces Figure 3
# produces Web Figure 4

source(file.path("code", "se-rwanda-estimates.r"))
# calculates estimates for the four hidden populations
# produces entries for Web Table 6
# produces numbers used in text for example of Web Table 7 
# produces Figure 4
# produces Web Figure 2

source(file.path("code", "se-rwanda-bias-illustration.r"))
# uses the sensitivity analysis framework to show different blended estimates based on different assumed biases
# produces Figure 5

source(file.path("code", "se-rwanda-balance.r"))
# checks the balance between the two experimental arms
# produces balance-check-hh-tab.tex (hh covariate tests)
# produces hh-balance-check.log (hansen + bowers omnibus test)
