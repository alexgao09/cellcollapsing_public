# cellcollapsing_public
Repository for Chapter 4 of my PhD thesis: "When should we collapse cells for Multilevel Regression and Poststratification?"

# Authors
- Yuxiang Gao
- Lauren Kennedy
- Daniel Simpson

# Steps to reproduce results in the interval coverage case study (Section 5.1 in manuscript)

Figures 2-7, 11, 12 are produced with the below steps:

1. The variables in ```logRENTcentered_simulationstudy_v4.R``` that control the simulation output are:

- ```Jvec = c(3,6,12,18)``` : Simulation results for 3, 6, 12, 18 income groups are produced. 6 income groups is a collapsed version of 12 income groups. 3 income groups is a collpased version of 6 income groups.
- ```iterations = 1000``` : 1000 simulation iterations
- ```Dsizevec = c(1000)``` : Main survey size is 1000
- ```ACSstratasizevec = c(15)``` : Auxillary survey size is 825 ``` = 15*55```

  
Produce simulation data files based on 1000 simulation iterations for all 9 combinations of main survey size = ```1000, 2000, 4000``` and auxillary survey size = ```15*55, 25*55, 50*55```.
