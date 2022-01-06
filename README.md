# cellcollapsing_public
Repository for Chapter 4 of my PhD thesis: "When should we collapse cells for Multilevel Regression and Poststratification?"

# Authors
- Yuxiang Gao
- Lauren Kennedy
- Daniel Simpson

# Steps to reproduce results in the interval coverage Monte Carlo case study (Section 5.1 in the manuscript)

Figures 2-7, 11, 12 are produced with the below steps:

1. Download all the files in ```cellcollapsing_public/interval_coverage_casestudy/```

2. The variables in ```logRENTcentered_simulationstudy_v4.R``` that control the simulation output are:

- ```Jvec = c(3,6,12,18)``` : Simulation results for ```3, 6, 12, 18``` income groups are produced. ```6``` income groups is a collapsed version of ```12``` income groups. ```3``` income groups is a collpased version of ```6``` income groups.
- ```iterations = 1000``` : ```1000``` simulation iterations
- ```Dsizevec = c(1000)``` : Main survey size is ```1000```
- ```ACSstratasizevec = c(15)``` : Auxillary survey size is ```825 = 15*55```

  
Produce simulation data files based on 1000 simulation iterations for all 9 combinations of main survey size = ```1000, 2000, 4000``` and auxillary survey size = ```15*55, 25*55, 50*55```.

3. Run ```visualizesimulations_logRENTcentered_v4.Rmd``` with the settings below to visualize MRP interval coverage for the 9 simulation settings in step 2:

```r
ACSstratasizevec = c(15,25,50)
Dsizevec = c(1000,2000,4000)
```

## Notes:

- The file ```nystate_fiveyear2015_2019.rds``` is the IPUMS USA data from 2015-2019, for New York state [1]. This file was retrieved from https://usa.ipums.org/usa/index.shtml
- Step 2 should be run in 9 parallel processes for speed. 

# Steps to reproduce results in the cell collapsing case study (Section 5.2 in the manuscript)

1. Download all the files in ```cellcollapsing_public/discretization_casestudy/```

2. Run ```Figure_8_comparing_ppd_and_pld.Rmd``` for all 9 combinations of ```p = 0.5, 0.75, 0.9``` and ```ACSstratasize = 3, 8, 15```. This is done by changing the code chunk at the beginning of the RMD file. This will produce the 9 rds files below

```r
comparing_linpred_pp_1000_3_90 = readRDS("comparing_linpred_pp_1000_3_90.rds")
comparing_linpred_pp_1000_3_75 = readRDS("comparing_linpred_pp_1000_3_75.rds")
comparing_linpred_pp_1000_3_50 = readRDS("comparing_linpred_pp_1000_3_50.rds")
comparing_linpred_pp_1000_8_90 = readRDS("comparing_linpred_pp_1000_8_90.rds")
comparing_linpred_pp_1000_8_75 = readRDS("comparing_linpred_pp_1000_8_75.rds")
comparing_linpred_pp_1000_8_50 = readRDS("comparing_linpred_pp_1000_8_50.rds")
comparing_linpred_pp_1000_15_90 = readRDS("comparing_linpred_pp_1000_15_90.rds")
comparing_linpred_pp_1000_15_75 = readRDS("comparing_linpred_pp_1000_15_75.rds")
comparing_linpred_pp_1000_15_50 = readRDS("comparing_linpred_pp_1000_15_50.rds")
```

3. Run the final code chunk to produce the png for Figure 9. I have done this for seed = ```set.seed(30628)```.



# References

- [1] Steven Ruggles, Sarah Flood, Sophia Foster, Ronald Goeken, Jose Pacas, Megan Schouweiler and Matthew Sobek. *IPUMS USA: Version 11.0* [dataset]. Minneapolis, MN: IPUMS, 2021. https://doi.org/10.18128/D010.V11.0
