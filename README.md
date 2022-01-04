# cellcollapsing_public
Repository for Chapter 4 of my PhD thesis: "When should we collapse cells for Multilevel Regression and Poststratification?"

# Authors
- Yuxiang Gao
- Lauren Kennedy
- Daniel Simpson

# Steps to reproduce results in the interval coverage case study (Section 5.1 in manuscript)

Figures 2-7, 11, 12 are produced with the below steps:

1. Run ```logRENTcentered_simulationstudy_v4.R``` with the settings: 

  ```Jvec = c(3,6,12,18)```, ```iterations = 1000```, ```Dsizevec = c(1000)```, ```ACSstratasizevec = c(15)```
