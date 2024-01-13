README
======

This folder contains the code used to produce the figures for
"Quantity vs quality: A survey experiment to improve the network scale-up
method"


replication steps
=================

1. The dataset is publicly available, but we do not have permission to distribute
   it; you must apply to the DHS for access. Once you get access, you can
   download the file `rwiq6adt.zip` from 
  (http://www.measuredhs.com/data/dataset/Rwanda_Special_2011.cfm?flag=1)[https://www.measuredhs.com/data/dataset/Rwanda_Special_2011.cfm?flag=1].
  You should download the 'individual raw Stata dataset' (`rwiq6adt.zip`).
  You should save it in the 'data' subdirectory and expand the zip archive, meaning
  that there will be a file called `data/RWIQ6ADT/RWIQ6AFL.DTA`. Then copy the datafile
  into the 'data' directory, so that there is a file called `data/RWIQ6AFL.DTA`.

2. be sure you have the installed the following packages (the versions we used are in parens)
  * R (version 3.2.1)
  * ggplot2 (version 1.0.1.9003)
  * dplyr (version 0.4.2)
  * plyr (version 1.8.3)
  * tidyr (version 0.2.0)
  * devtools (version 1.8.0)
  * stargazer (version 5.2)
  * RItools (version 0.1-12)
  * gridExtra (version 2.0.0)
  * car (version 2.0-26)

  Note that, as of 2015-08-25, installing version 1.0.1.9003 of ggplot2 requires
  using `install_github` from the `devtools` package.

3. use install_github to get two more packages that are required:

```{r}
  # you need the devtools packages for install_github
  require(devtools)  

  install_github("dfeehan/networkreporting@networkreporting-0.0.2.9000")
  install_github("dfeehan/dhstools@dhstools-0.0.2")
```

4. edit the file `code/se-rwanda-directories.r`; follow the instructions in
   that file to change the path so that it matches your computer's setup

5. run the code listed in 'code files' below *in the order listed*;
   plots will be saved to the `out/` directory

IMPORTANT NOTE:
===============
* running the bootstrap code without any parallelization on a modern machine
  (a 2014-era mac laptop) can take on the order of a full day

code files for replication
==========================

`replicate.r`
-----------
* running this file should run the files below and produce all of the replication
  output. be warned that this can take about a day or longer, depending on how fast your
  computer is.

`code/se-rwanda-prep.r`
-----------------------
* takes the raw dataset and produces the analysis dataset that gets
  used by other files
* creates 'data/se-rwanda-data.RData'

`code/se-rwanda-bootstrap.r`
----------------------------
* takes all of the bootstrap resamples
  (requires about one day to run)
* creates `out/rw-bootstrap-resamples.RData`

`code/se-rwanda-degree-estimates.r`
------------------------------------------
* produces Figure 2
* produces Web Figure 1
* produces `avg_degree_estimates.log` which has the point estimates we mention
  in the paper

`code/se-rwanda-ic.r`
------------------------------------------
* summarizes and plots the results of the internal consistency checks
* produces Figure 3
* produces Web Figure 4

`code/se-rwanda-estimates.r`
------------------------------------------
* calculates estimates for the four hidden populations
* produces entries for Web Table 6
* produces numbers used in text for example of Web Table 7 
* produces Figure 4
* produces Web Figure 2

`code/se-rwanda-bias-illustration.r`
------------------------------------------
* uses the sensitivity analysis framework to show different blended
  estimates based on different assumed biases
* produces Figure 5

`code/se-rwanda-balance.r`
--------------------------
* checks the balance between the two experimental arms
* produces balance-check-hh-tab.tex (hh covariate tests)
* produces hh-balance-check.log (hansen + bowers omnibus test)

