# this file builds the simulation study that was layed out in the simulation design.R file on June 23 meeting
# v1 uses 5 year ACS 2015-2019
# v2 performs subpop estimation for group INCTOT > 79K and J = 6,12,18
# v3 uses subgroup O as <= 17.5K
# v4 uses subgroup O as <= 48K

# https://usa.ipums.org/usa-action/data_requests/download

# https://data.census.gov/cedsci/table?q=S0101&tid=ACSST1Y2019.S0101

#library(ipumsr)
library(tidyverse)
library(brms)
library(tidybayes)
library(survey)
library(parallel)
library(foreach)
library(doParallel)


#no_cores = detectCores()
registerDoParallel(cores=3)

#ddi <- read.csv("usa_00008.csv")


# get NY state 
# https://data.census.gov/cedsci/table?q=United%20States&g=0400000US36&y=2019&tid=ACSST1Y2019.S0101
#nystate = ddi %>% filter(STATEFIP==36)
nystate = readRDS("nystate_fiveyear2015_2019.rds")




#### STEP 1: CREATE TARGET POPULATION P #####
# TARGET POPULATION P has covariate INCTOT and true values RENT. PUMA is used to perform stratified sampling
# The ppl of NY county are used 
# OUTPUT: P as a dataframe, estimand as average log rent in P (CENTERED).


# P is New York City without survey weights
# COUNTYFIP: 61=New York county, 47=Kings county, 81=Queens county, 5=Bronx county, 85=Richmond county
# The reason INCTOT > 0 is because I don't want a bunch of individuals with 0 INCTOT which will make 
# quantile(x=P$INCTOT, probs = seq(0,1, by=1/J)) produce non-unique quantiles for J = 16
P = nystate %>% 
  filter(COUNTYFIP %in% c(61,47,81,5,85) & (RENT > 0) & (INCTOT > 0) & !(INCTOT %in% c(9999998,9999999))) %>% 
  select(RENT, INCTOT, PUMA)

P$logRENTcentered = log(P$RENT) - mean(log(P$RENT))

# estimand is the average log(RENT) after centering
estimand = mean(P$logRENTcentered) # should be 0

estimand_subpop = P %>% filter(INCTOT <= 48000) %>% summarise(mean(logRENTcentered)) %>% unlist
# subpopestimandJindices contains indices of of income bracket for subpopestimand, which is the average log(RENT) of people who earn less than 33000
# To check:
# cut(P$INCTOT,
#     breaks=quantile(x=P$INCTOT, probs = seq(0,1, by=1/J)),
#     include.lowest = TRUE
# )

subpopestimandJindices = list()
subpopestimandJindices[["18"]] = 1:12
subpopestimandJindices[["12"]] = 1:8
subpopestimandJindices[["6"]] = 1:4
subpopestimandJindices[["3"]] = 1:2

# STEP 2: DEFINE THE NOBS TO TWEAK ####

# Jvec are income brackets to analyze
Jvec = c(3,6,12,18)
# iterations is number of sim iterations
iterations = 1000
# Dsize is nonrep sample size
Dsizevec = c(500)
# ACSstratasize is the size of random sample in each PUMA strata when performing a stratified random sample of P
ACSstratasizevec = c(15)



# get population sizes of PUMA in P
P_PUMAsize = P %>% group_by(PUMA) %>% summarise(N = n())

# Define probs to do PPSWOR for D1,D2,D3
P$INCTOTprobD1 = 0
P[P$INCTOT<=median(P$INCTOT), c("INCTOTprobD1")] = 0.5
P[P$INCTOT>median(P$INCTOT), c("INCTOTprobD1")] = 0.5
P$INCTOTprobD2 = 0
P[P$INCTOT<=median(P$INCTOT), c("INCTOTprobD2")] = 0.75
P[P$INCTOT>median(P$INCTOT), c("INCTOTprobD2")] = 0.25
P$INCTOTprobD3 = 0
P[P$INCTOT<=median(P$INCTOT), c("INCTOTprobD3")] = 0.9
P[P$INCTOT>median(P$INCTOT), c("INCTOTprobD3")] = 0.1
# 
# P$INCTOTprobD3 = 0
# P[P$INCTOT<=20000, c("INCTOTprobD3")] = 0.9
# P[P$INCTOT>20000, c("INCTOTprobD3")] = 0.1



# STEP 3: DEFINE LISTS TO STORE SIMULATION VALUES ####
# OUTPUT BELOW: 

# population values below
# \hat{N}_j for j in [J]
Nhatvecs = list()
# \sqrt( \hat{Var} (\hat{N}_j) )
seNhatvecs = list()
# MRP posterior samples for samples D1,D2,D3
MRPposteriorsamplesvec_D1 = list()
MRPposteriorsamplesvec_D2 = list()
MRPposteriorsamplesvec_D3 = list()
# MRP naive SD for D1,D2,D3 (middle term of corollary 0.1)
MRPsdnaivevec_D1 = list()
MRPsdnaivevec_D2 = list()
MRPsdnaivevec_D3 = list()
# MRP SD with uncertainty \sqrt( \hat{Var} (\hat{N}_j) ) for D1,D2,D3
MRPsdvec_D1 = list()
MRPsdvec_D2 = list()
MRPsdvec_D3 = list()

# D1mean is mean(D1$RENT), D1sdmean is sd(D1$RENT)/dim(D1)[1]
D1mean = list()
D2mean = list()
D3mean = list()
D1sdmean = list()
D2sdmean = list()
D3sdmean = list()

# subpop values below
# seNhat_subpopratio_vecs contains \hat{V} ( \hat{N}_j / \hat{N_subpop}  )
seNhat_subpopratio_vecs = list()
# \hat{N}_j for j in [J]
Nhatvecs_subpop = list()
# \sqrt( \hat{Var} (\hat{N}_j) )
seNhatvecs_subpop = list()
# MRP posterior samples for samples D1,D2,D3
MRPposteriorsamplesvec_D1_subpop = list()
MRPposteriorsamplesvec_D2_subpop = list()
MRPposteriorsamplesvec_D3_subpop = list()
# MRP naive SD for D1,D2,D3 (middle term of corollary 0.1)
MRPsdnaivevec_D1_subpop = list()
MRPsdnaivevec_D2_subpop = list()
MRPsdnaivevec_D3_subpop = list()
# MRP SD with uncertainty \sqrt( \hat{Var} (\hat{N}_j) ) for D1,D2,D3
MRPsdvec_D1_subpop = list()
MRPsdvec_D2_subpop = list()
MRPsdvec_D3_subpop = list()

# D1mean is mean(D1$RENT), D1sdmean is sd(D1$RENT)/dim(D1)[1]
D1mean_subpop = list()
D2mean_subpop = list()
D3mean_subpop = list()
D1sdmean_subpop = list()
D2sdmean_subpop = list()
D3sdmean_subpop = list()

D1naivePS = list()
D2naivePS = list()
D3naivePS = list()
D1naivePS_subpop = list()
D2naivePS_subpop = list()
D3naivePS_subpop = list()


# STEP 4: START SIMULATION ####
# INPUT: ACSstratasizevec are ACS sizes, Dsizevec are sizes of MRP sample, Jvec are income brackets
for (ACSstratasize in ACSstratasizevec) {
  for (Dsize in Dsizevec) {
    
    for (J in Jvec) {
      
      # create J incomebrackets in target population P
      P$incomebracket = cut(P$INCTOT,
                            breaks=quantile(x=P$INCTOT, probs = seq(0,1, by=1/J)),
                            include.lowest = TRUE
      )
      
      # rename incomebracket levels
      levels(P$incomebracket) = 1:J
      
      # population values below
      Nhatvecs[[toString(J)]] =  matrix(0, nrow = iterations, ncol = J)
      seNhatvecs[[toString(J)]] = matrix(0, nrow = iterations, ncol = J)
      MRPposteriorsamplesvec_D1[[toString(J)]] = matrix(0, nrow = iterations, ncol = 1000)
      MRPposteriorsamplesvec_D2[[toString(J)]] = matrix(0, nrow = iterations, ncol = 1000)
      MRPposteriorsamplesvec_D3[[toString(J)]] = matrix(0, nrow = iterations, ncol = 1000)
      MRPsdnaivevec_D1[[toString(J)]] = rep(0, iterations)
      MRPsdnaivevec_D2[[toString(J)]] = rep(0, iterations)
      MRPsdnaivevec_D3[[toString(J)]] = rep(0, iterations)
      MRPsdvec_D1[[toString(J)]] = rep(0, iterations)
      MRPsdvec_D2[[toString(J)]] = rep(0, iterations)
      MRPsdvec_D3[[toString(J)]] = rep(0, iterations)
      D1mean[[toString(J)]] = rep(0, iterations)
      D2mean[[toString(J)]] = rep(0, iterations)
      D3mean[[toString(J)]] = rep(0, iterations)
      D1sdmean[[toString(J)]] = rep(0, iterations)
      D2sdmean[[toString(J)]] = rep(0, iterations)
      D3sdmean[[toString(J)]] = rep(0, iterations)
      
      # subpop values below
      seNhat_subpopratio_vecs[[toString(J)]] = matrix(
        0, nrow = iterations,
        ncol = length(subpopestimandJindices[[toString(J)]])
      )
      Nhatvecs_subpop[[toString(J)]] =  matrix(0, nrow = iterations, ncol = length(subpopestimandJindices[[toString(J)]]))
      seNhatvecs_subpop[[toString(J)]] = matrix(0, nrow = iterations, ncol = length(subpopestimandJindices[[toString(J)]]))
      MRPposteriorsamplesvec_D1_subpop[[toString(J)]] = matrix(0, nrow = iterations, ncol = 1000)
      MRPposteriorsamplesvec_D2_subpop[[toString(J)]] = matrix(0, nrow = iterations, ncol = 1000)
      MRPposteriorsamplesvec_D3_subpop[[toString(J)]] = matrix(0, nrow = iterations, ncol = 1000)
      MRPsdnaivevec_D1_subpop[[toString(J)]] = rep(0, iterations)
      MRPsdnaivevec_D2_subpop[[toString(J)]] = rep(0, iterations)
      MRPsdnaivevec_D3_subpop[[toString(J)]] = rep(0, iterations)
      MRPsdvec_D1_subpop[[toString(J)]] = rep(0, iterations)
      MRPsdvec_D2_subpop[[toString(J)]] = rep(0, iterations)
      MRPsdvec_D3_subpop[[toString(J)]] = rep(0, iterations)
      D1mean_subpop[[toString(J)]] = rep(0, iterations)
      D2mean_subpop[[toString(J)]] = rep(0, iterations)
      D3mean_subpop[[toString(J)]] = rep(0, iterations)
      D1sdmean_subpop[[toString(J)]] = rep(0, iterations)
      D2sdmean_subpop[[toString(J)]] = rep(0, iterations)
      D3sdmean_subpop[[toString(J)]] = rep(0, iterations)
      
      D1naivePS[[toString(J)]] = rep(0, iterations)
      D2naivePS[[toString(J)]] = rep(0, iterations)
      D3naivePS[[toString(J)]] = rep(0, iterations)
      D1naivePS_subpop[[toString(J)]] = rep(0, iterations)
      D2naivePS_subpop[[toString(J)]] = rep(0, iterations)
      D3naivePS_subpop[[toString(J)]] = rep(0, iterations)
      
      
      # STEP 5: SAMPLE D1,D2,D3 AND FIT HIERARCHICAL MODEL TO THEM ####
      # OUTPUT: model_D1, model_D2, model_D3 ARE MODELS FOR RENT
      # sample D1
      D1indices = sample(x=1:dim(P)[1],
                         prob=P$INCTOTprobD1,
                         replace=FALSE,
                         size=Dsize)
      
      D1 = P[D1indices,]
      # sample D2
      D2indices = sample(x=1:dim(P)[1],
                         prob=P$INCTOTprobD2,
                         replace=FALSE,
                         size=Dsize)
      
      D2 = P[D2indices,]
      # sample D3
      D3indices = sample(x=1:dim(P)[1],
                         prob=P$INCTOTprobD3,
                         replace=FALSE,
                         size=Dsize)
      
      D3 = P[D3indices,]
      
      
      model_D1 = brm(formula = logRENTcentered ~ (1|incomebracket),
                     prior = c(
                       prior(normal(0, 5), class=Intercept), # change sd to 2
                       prior(normal(0, 5), class = sd),
                       prior(normal(0, 5), class = sigma)
                     ),
                     data = D1,
                     chains=1,
                     cores=1)
      
      model_D2 = brm(formula = logRENTcentered ~ (1|incomebracket),
                     prior = c(
                       prior(normal(0, 5), class=Intercept), # change sd to 2
                       prior(normal(0, 5), class = sd),
                       prior(normal(0, 5), class = sigma)
                     ),
                     data = D2,
                     chains=1,
                     cores=1)
      
      model_D3 = brm(formula =logRENTcentered ~ (1|incomebracket),
                     prior = c(
                       prior(normal(0, 5), class=Intercept), # change sd to 2
                       prior(normal(0, 5), class = sd),
                       prior(normal(0, 5), class = sigma)
                     ),
                     data = D3,
                     chains=1,
                     cores=1)
      
      
      
      
      # STEP 6: RUN SIMULATION INTERATIONS 1 TO iterations ####
      for (i in 1:iterations) {
        print(paste(ACSstratasize, Dsize, J, i))
        
        # STEP 7: GET STRATIFIED RS TO GET pseudo-ACS ####
        # OUTPUT: pseudoACS_weighted IS STRATIFIED RS, STRATIFIED BY PUMA. pseudoACS_svydesign_rake_fpc IS THE survey OBJECT
        # NOTES: N IS KNOWN IN THIS SAMPLING DESIGN TO GET pseudoACS_weighted
        pseudoACS_unweighted = c()
        
        # Get stratified sample pseudoACS_unweighted, where strata are PUMA
        for (PUMA_ in P_PUMAsize$PUMA) {
          pseudoACS_unweighted = rbind(pseudoACS_unweighted,
                                       P %>% filter(PUMA==PUMA_) %>% sample_n(ACSstratasize)
          )
        }
        
        # create dummy variable columns for incomebracket in pseudoACS_unweighted
        pseudoACS_unweighted = fastDummies::dummy_cols(pseudoACS_unweighted, select_columns = c("incomebracket"))
        pseudoACS_unweighted = left_join(pseudoACS_unweighted, P_PUMAsize, by = c("PUMA"))
        
        
        for (j1 in 1:J) {
          for (j2 in 1:J) {
            if (j1 != j2) {
              
              j_index = c(j1,j2)
              index_vector = rep(0, dim(pseudoACS_unweighted)[1])
              
              for (j in j_index) {
                index_vector = index_vector + pseudoACS_unweighted[,c(paste0("incomebracket_", j))]
              }
              
              pseudoACS_unweighted[c(paste0("incomebracket_", j1, "_", j2))] = index_vector
              
            }
          }
        }
        
        # create indices to get hat{V}( \frac{\hat{N}_j}{\hat{N}_subpop} ), for target subpop
        pseudoACS_unweighted$subpop = rowSums(pseudoACS_unweighted[,paste0("incomebracket_",subpopestimandJindices[[toString(J)]])])
        
        # sampling without replacement in strata
        pseudoACS_svydesign_fpc = svydesign(id=~1,
                                            strata=~PUMA,
                                            data = pseudoACS_unweighted,
                                            fpc=~N
        )
        
        pseudoACS_svydesign_rake_fpc = rake(design = pseudoACS_svydesign_fpc,
                                            list(~PUMA),
                                            list(data.frame(P %>% group_by(PUMA) %>% summarise(Freq=n())))
        )
        
        # pseudoACS_weighted has a column with weights for every individual
        pseudoACS_weighted = data.frame(pseudoACS_unweighted,
                                        weights = weights(pseudoACS_svydesign_rake_fpc))
        
        
        # STEP 8: ESTIMATE \hat{N}_j #### 
        # OUTPUT: 
        # STORE \hat{N}_j, for j in [J] IN Nhatvecs
        # STORE \sqrt( \hat{Var} (\hat{N}_j) ), for j in [J] in seNhatvecs
        # STORE  \sqrt( \hat{Var} ( \frac{\hat{N}_j)}{\hat{N}_subpop} ) for j in [J] in seNhat_subpopratio_vecs
        temp_counter = 1
        for (j in 1:J) {
          Nhatvecs[[toString(J)]][i,j] = svytotal(~eval(parse(text=paste0("incomebracket_", j))),
                                                  pseudoACS_svydesign_rake_fpc)[1]
          
          
          seNhatvecs[[toString(J)]][i,j] = as.data.frame(svytotal(~eval(parse(text=paste0("incomebracket_", j))),
                                                                  pseudoACS_svydesign_rake_fpc))[,2]
          
          
          if (j %in% subpopestimandJindices[[toString(J)]]) {
            
            seNhat_subpopratio_vecs[[toString(J)]][i, temp_counter] = sqrt(
              svyratio(
                numerator = ~eval(parse(text=paste0("incomebracket_", j))),
                denominator = ~subpop,
                design = pseudoACS_svydesign_rake_fpc
              )[2] %>% unlist
            )
            
            temp_counter = temp_counter + 1
          }
          
        }
        
        
        
        
        # STEP 9: GET MRP SAMPLE D1,D2,D3 from P TO UPDATE model_D1, model_D2, model_d3 ####
        # OUTPUT: modelupdate AND d1mean, d1sdmean, d2mean, d2sdmean,d3mean, d3sdmean
        # sample D1
        D1indices = sample(x=1:dim(P)[1],
                           prob=P$INCTOTprobD1,
                           replace=FALSE,
                           size=Dsize)
        
        D1 = P[D1indices,]
        
        # sample D2
        D2indices = sample(x=1:dim(P)[1],
                           prob=P$INCTOTprobD2,
                           replace=FALSE,
                           size=Dsize)
        
        D2 = P[D2indices,]
        
        # sample D3
        D3indices = sample(x=1:dim(P)[1],
                           prob=P$INCTOTprobD3,
                           replace=FALSE,
                           size=Dsize)
        
        D3 = P[D3indices,]
        
        
        D1mean[[toString(J)]][i] = mean(D1$logRENTcentered)
        D2mean[[toString(J)]][i] = mean(D2$logRENTcentered)
        D3mean[[toString(J)]][i] = mean(D3$logRENTcentered)
        D1sdmean[[toString(J)]][i] = sd(D1$logRENTcentered)/sqrt(dim(D1)[1])
        D2sdmean[[toString(J)]][i] = sd(D2$logRENTcentered)/sqrt(dim(D2)[1])
        D3sdmean[[toString(J)]][i] = sd(D3$logRENTcentered)/sqrt(dim(D3)[1])
        
        D1mean_subpop[[toString(J)]][i] = D1 %>% filter(INCTOT <= 48000) %>% summarise(mean(logRENTcentered)) %>% unlist
        D2mean_subpop[[toString(J)]][i] = D2 %>% filter(INCTOT <= 48000) %>% summarise(mean(logRENTcentered)) %>% unlist
        D3mean_subpop[[toString(J)]][i] = D3 %>% filter(INCTOT <= 48000) %>% summarise(mean(logRENTcentered)) %>% unlist
        D1sdmean_subpop[[toString(J)]][i] = D1 %>% filter(INCTOT <= 48000) %>% summarise(sd(logRENTcentered)/sqrt(n())) %>% unlist
        D2sdmean_subpop[[toString(J)]][i] = D2 %>% filter(INCTOT <= 48000) %>% summarise(sd(logRENTcentered)/sqrt(n())) %>% unlist
        D3sdmean_subpop[[toString(J)]][i] = D3 %>% filter(INCTOT <= 48000) %>% summarise(sd(logRENTcentered)/sqrt(n())) %>% unlist
        
        
        # STEP 10: GET POSTERIOR SAMPLES ####
        # OUTPUT: posterior means (\hat{theta}_j)_{j=1}^J, posterior covariance matrix \Sigma_{theta | D, J}. Do for each unique J.
        # thetadraws_D1 is 1000 x J MATRIX WITH POSTERIOR DRAWS FOR EACH INCOME BRACKET  
        modelupdate = foreach (i=1:3) %dopar% {
          #print(i)
          update(object = eval(parse(text=paste0("model_D", i))),
                 newdata = eval(parse(text=paste0("D", i))))
        }
        
        
        
        
        # thetadraws_D1 stores posterior samples for cells j in [J]
        thetadraws_D1 = matrix(data = 0, nrow = 1000, ncol = J)
        thetadraws_D2 = matrix(data = 0, nrow = 1000, ncol = J)
        thetadraws_D3 = matrix(data = 0, nrow = 1000, ncol = J)
        
        observed_incomebracketindex_D1 = as.numeric(unique(D1$incomebracket))
        observed_incomebracketindex_D2 = as.numeric(unique(D2$incomebracket))
        observed_incomebracketindex_D3 = as.numeric(unique(D3$incomebracket))
        # get posterior draws of each cell j, which will be used to posterior means and posterior covariance matrix
        for (j in 1:J) {
          
          # D1 ****
          if (j %in% observed_incomebracketindex_D1) {
            thetadraws_D1[,j] = unlist(
              (modelupdate[[1]] %>% spread_draws(b_Intercept))[,4] + 
                (modelupdate[[1]] %>% spread_draws(r_incomebracket[incomebracket,]) %>% filter(incomebracket==j))[,2]
            )
          }
          else {
            print(j)
            thetadraws_D1[,j] = unlist(
              (modelupdate[[1]] %>% spread_draws(b_Intercept))[,4]
            )
          }
          
          
          # D2 ****
          if (j %in% observed_incomebracketindex_D2) {
            thetadraws_D2[,j] = unlist(
              (modelupdate[[2]] %>% spread_draws(b_Intercept))[,4] + 
                (modelupdate[[2]] %>% spread_draws(r_incomebracket[incomebracket,]) %>% filter(incomebracket==j))[,2]
            )
          }
          else {
            print(paste(2,j))
            thetadraws_D2[,j] = unlist(
              (modelupdate[[2]] %>% spread_draws(b_Intercept))[,4]
            )
          }
          
          
          # D3 ****
          if (j %in% observed_incomebracketindex_D3) {
            thetadraws_D3[,j] = unlist(
              (modelupdate[[3]] %>% spread_draws(b_Intercept))[,4] + 
                (modelupdate[[3]] %>% spread_draws(r_incomebracket[incomebracket,]) %>% filter(incomebracket==j))[,2]
            )
          }
          else {
            print(paste(3,j))
            thetadraws_D3[,j] = unlist(
              (modelupdate[[3]] %>% spread_draws(b_Intercept))[,4]
            )
          }
          
          
        }
        
        # thetadraws_subpop_D1,2,3 are for subpop mrp estimation
        thetadraws_subpop_D1 = thetadraws_D1[,subpopestimandJindices[[toString(J)]]]
        thetadraws_subpop_D2 = thetadraws_D2[,subpopestimandJindices[[toString(J)]]]
        thetadraws_subpop_D3 = thetadraws_D3[,subpopestimandJindices[[toString(J)]]]
        
        # STEP 11: POSTSTRATIFICATION STEP ####
        # OUTPUT BELOW:
        # Perform poststratification. sum(Nhatvecs[[toString(J)]][i,]) should equal dim(P)[1] as N is fixed in this case. 
        # Store MRP posterior samples for D1,D2,D3 ****
        
        # Nhatvecs_subpop is the calculated \hat{N}_j for j in subpop in this current iteration i
        Nhatvecs_subpop[[toString(J)]][i,] = Nhatvecs[[toString(J)]][i,subpopestimandJindices[[toString(J)]]]
        
        
        # population level estimand ####
        MRPposteriorsamplesvec_D1[[toString(J)]][i,] = as.vector(thetadraws_D1 %*% as.matrix(Nhatvecs[[toString(J)]][i,]) / sum(Nhatvecs[[toString(J)]][i,]))
        MRPposteriorsamplesvec_D2[[toString(J)]][i,] = as.vector(thetadraws_D2 %*% as.matrix(Nhatvecs[[toString(J)]][i,]) / sum(Nhatvecs[[toString(J)]][i,]))
        MRPposteriorsamplesvec_D3[[toString(J)]][i,] = as.vector(thetadraws_D3 %*% as.matrix(Nhatvecs[[toString(J)]][i,]) / sum(Nhatvecs[[toString(J)]][i,]))
        
        
        # Naive MRP standard error
        MRPsdnaivevec_D1[[toString(J)]][i] = sd(MRPposteriorsamplesvec_D1[[toString(J)]][i,])
        MRPsdnaivevec_D2[[toString(J)]][i] = sd(MRPposteriorsamplesvec_D2[[toString(J)]][i,])
        MRPsdnaivevec_D3[[toString(J)]][i] = sd(MRPposteriorsamplesvec_D3[[toString(J)]][i,])
        
        
        # subpop level estimand ####
        MRPposteriorsamplesvec_D1_subpop[[toString(J)]][i,] = as.vector(thetadraws_subpop_D1 %*% as.matrix(Nhatvecs_subpop[[toString(J)]][i,]) / sum(Nhatvecs_subpop[[toString(J)]][i,]))
        MRPposteriorsamplesvec_D2_subpop[[toString(J)]][i,] = as.vector(thetadraws_subpop_D2 %*% as.matrix(Nhatvecs_subpop[[toString(J)]][i,]) / sum(Nhatvecs_subpop[[toString(J)]][i,]))
        MRPposteriorsamplesvec_D3_subpop[[toString(J)]][i,] = as.vector(thetadraws_subpop_D3 %*% as.matrix(Nhatvecs_subpop[[toString(J)]][i,]) / sum(Nhatvecs_subpop[[toString(J)]][i,]))
        
        # Naive MRP standard error
        MRPsdnaivevec_D1_subpop[[toString(J)]][i] = sd(MRPposteriorsamplesvec_D1_subpop[[toString(J)]][i,])
        MRPsdnaivevec_D2_subpop[[toString(J)]][i] = sd(MRPposteriorsamplesvec_D2_subpop[[toString(J)]][i,])
        MRPsdnaivevec_D3_subpop[[toString(J)]][i] = sd(MRPposteriorsamplesvec_D3_subpop[[toString(J)]][i,])
        
        # MRP standard error with standard error 
        # get \bold{{\Sigma}_{J,N}} for the first and last term of Corollary 0.1
        
        # VarNj1_plus_Nj2 function gets \hat{V} ( \hat{N}_{j1} + \hat{N}_{j1} )
        VarNj1_plus_Nj2 = function(j1,j2) {
          return(
            (
              (as.data.frame(svytotal(~eval(parse(text=paste0("incomebracket_", j1,"_",j2))),
                                      pseudoACS_svydesign_rake_fpc))[2])^2 %>% unlist)
          )
        }
        
        # hatSigma_J_N_upper is upper triangular matrix with diagonal of \hat{\Sigma}_{J,N}
        hatSigma_J_N_upper = matrix(0,J,J)
        for (j1 in 1:J) {
          for (j2 in j1:J) {
            #print(paste0("top: ",c(j1,j2)))
            if (j1 == j2) {
              hatSigma_J_N_upper[j1,j2] = (seNhatvecs[[toString(J)]][i,j1])^2
            } else {
              hatSigma_J_N_upper[j1,j2] = 1/2*(VarNj1_plus_Nj2(j1,j2) - 
                                                 (seNhatvecs[[toString(J)]][i,j1])^2 - 
                                                 (seNhatvecs[[toString(J)]][i,j2])^2
              )
            }
          }
        }
        
        # hatSigma_J_N_lower_nodiagonal is lower triangular matrix WITHOUT diagonal of \hat{\Sigma}_{J,N}
        hatSigma_J_N_lower_nodiagonal = t(hatSigma_J_N_upper)
        diag(hatSigma_J_N_lower_nodiagonal) = 0
        
        # hatSigma_J_N is \hat{\Sigma}_{J,N}
        hatSigma_J_N = hatSigma_J_N_upper + hatSigma_J_N_lower_nodiagonal
        
        
        # VarNj1_plus_Nj2_over_Nsubpop gets \hat{V} ( \frac{\hat{Nj1 + Nj2}}{\hat{N}_subpop} ) when j1 != j2
        # j1_index,j2_index are indices in 1:J
        # j1,j2 are indices in 1:length(subpopestimandJindices[[toString(J)]])
        VarNj1_plus_Nj2_over_Nsubpop = function(j1,j2) {
          j1_index = (subpopestimandJindices[[toString(J)]])[j1]
          j2_index = (subpopestimandJindices[[toString(J)]])[j2]
          return(
            svyratio(
              numerator = ~eval(parse(text=paste0("incomebracket_", j1_index,"_",j2_index))),
              denominator = ~subpop,
              design = pseudoACS_svydesign_rake_fpc
            )[2] %>% unlist
          )
        }
        
        subpopJ_length = length(subpopestimandJindices[[toString(J)]])
        
        # hatSigma_J_vecNoverN_upper is upper triangular matrix with diagonal of \hat{\Sigma}_{J,\frac{\vec{N}}{N}} in Lemma 0.2
        hatSigma_J_vecNoverN_upper = matrix(0, subpopJ_length, subpopJ_length)
        for (j1 in 1:subpopJ_length) {
          for (j2 in j1:subpopJ_length) {
            #print(paste0("top: ",c(j1,j2)))
            if (j1 == j2) {
              hatSigma_J_vecNoverN_upper[j1,j2] = (seNhat_subpopratio_vecs[[toString(J)]][i,j1])^2
            } else {
              hatSigma_J_vecNoverN_upper[j1,j2] = 1/2*(VarNj1_plus_Nj2_over_Nsubpop(j1,j2)) -
                1/2*(seNhat_subpopratio_vecs[[toString(J)]][i,j1])^2 - 1/2*(seNhat_subpopratio_vecs[[toString(J)]][i,j2])^2
            }
          }
        }
        
        hatSigma_J_vecNoverN_lower_nodiagonal = t(hatSigma_J_vecNoverN_upper)
        diag(hatSigma_J_vecNoverN_lower_nodiagonal) = 0
        
        # hatSigma_J_N is hatSigma_J_vecNoverN is \hat{\Sigma}_{J,\vec{N}/N }
        hatSigma_J_vecNoverN = hatSigma_J_vecNoverN_upper + hatSigma_J_vecNoverN_lower_nodiagonal
        
        
        
        
        # STORE POPULATION LEVEL ESTIMAND'S STANDARD AND DERIVED MRP SD ####
        # MRPsdvec_D1 contains derived MRP sd estimate for D1. This is corollary 0.1
        MRPsdvec_D1[[toString(J)]][i] = sqrt(
          (1/dim(P)[1])^2 * sum(diag( cov(thetadraws_D1) %*% hatSigma_J_N )) + 
            var(MRPposteriorsamplesvec_D1[[toString(J)]][i,]) +
            (1/dim(P)[1])^2 * ( t(as.matrix(colMeans(thetadraws_D1))) %*% hatSigma_J_N %*% as.matrix(colMeans(thetadraws_D1)) )
        ) %>% as.numeric
        
        
        
        MRPsdvec_D2[[toString(J)]][i] = sqrt(
          (1/dim(P)[1])^2 * sum(diag( cov(thetadraws_D2) %*% hatSigma_J_N )) + 
            var(MRPposteriorsamplesvec_D2[[toString(J)]][i,]) +
            (1/dim(P)[1])^2 * ( t(as.matrix(colMeans(thetadraws_D2))) %*% hatSigma_J_N %*% as.matrix(colMeans(thetadraws_D2)) )
        ) %>% as.numeric
        
        
        MRPsdvec_D3[[toString(J)]][i] = sqrt(
          (1/dim(P)[1])^2 * sum(diag( cov(thetadraws_D3) %*% hatSigma_J_N )) + 
            var(MRPposteriorsamplesvec_D3[[toString(J)]][i,]) +
            (1/dim(P)[1])^2 * ( t(as.matrix(colMeans(thetadraws_D3))) %*% hatSigma_J_N %*% as.matrix(colMeans(thetadraws_D3)) )
        ) %>% as.numeric
        
        
        # STORE SUB-POPULATION LEVEL ESTIMAND'S STANDARD AND DERIVED MRP SD ####
        MRPsdvec_D1_subpop[[toString(J)]][i] = sqrt(
          sum(diag( cov(thetadraws_subpop_D1) %*% hatSigma_J_vecNoverN )) +
            var(MRPposteriorsamplesvec_D1_subpop[[toString(J)]][i,]) +
            1/(sum(Nhatvecs_subpop[[toString(J)]][i,]))^2*( t(as.matrix(colMeans(thetadraws_subpop_D1) - mean(MRPposteriorsamplesvec_D1_subpop[[toString(J)]][i,]))) %*% 
                                                              hatSigma_J_N[ subpopestimandJindices[[toString(J)]], subpopestimandJindices[[toString(J)]] ] %*% 
                                                              (as.matrix(colMeans(thetadraws_subpop_D1) - mean(MRPposteriorsamplesvec_D1_subpop[[toString(J)]][i,]))) )
        ) %>% as.numeric
        
        MRPsdvec_D2_subpop[[toString(J)]][i] = sqrt(
          sum(diag( cov(thetadraws_subpop_D2) %*% hatSigma_J_vecNoverN )) +
            var(MRPposteriorsamplesvec_D2_subpop[[toString(J)]][i,]) +
            1/(sum(Nhatvecs_subpop[[toString(J)]][i,]))^2*( t(as.matrix(colMeans(thetadraws_subpop_D2) - mean(MRPposteriorsamplesvec_D2_subpop[[toString(J)]][i,]))) %*% 
                                                              hatSigma_J_N[ subpopestimandJindices[[toString(J)]], subpopestimandJindices[[toString(J)]] ] %*% 
                                                              (as.matrix(colMeans(thetadraws_subpop_D2) - mean(MRPposteriorsamplesvec_D2_subpop[[toString(J)]][i,]))) )
        ) %>% as.numeric
        
        MRPsdvec_D3_subpop[[toString(J)]][i] = sqrt(
          sum(diag( cov(thetadraws_subpop_D3) %*% hatSigma_J_vecNoverN )) +
            var(MRPposteriorsamplesvec_D3_subpop[[toString(J)]][i,]) +
            1/(sum(Nhatvecs_subpop[[toString(J)]][i,]))^2*( t(as.matrix(colMeans(thetadraws_subpop_D3) - mean(MRPposteriorsamplesvec_D3_subpop[[toString(J)]][i,]))) %*% 
                                                              hatSigma_J_N[ subpopestimandJindices[[toString(J)]], subpopestimandJindices[[toString(J)]] ] %*% 
                                                              (as.matrix(colMeans(thetadraws_subpop_D3) - mean(MRPposteriorsamplesvec_D3_subpop[[toString(J)]][i,]))) )
        ) %>% as.numeric
        
        
        
        # include naive poststrafication ####
        
        D1naivePS[[toString(J)]][i] = sum(
          Nhatvecs[[toString(J)]][i,] * 
            ( D1 %>% 
                group_by(incomebracket) %>% 
                summarise(m = mean(logRENTcentered)) %>% 
                arrange(incomebracket) %>% 
                select(m) %>%
                unlist
            )/sum(Nhatvecs[[toString(J)]][i,])
        )
        
        D2naivePS[[toString(J)]][i] = sum(
          Nhatvecs[[toString(J)]][i,] * 
            ( D2 %>% 
                group_by(incomebracket) %>% 
                summarise(m = mean(logRENTcentered)) %>% 
                arrange(incomebracket) %>% 
                select(m) %>%
                unlist
            )/sum(Nhatvecs[[toString(J)]][i,])
        )
        
        D3naivePS[[toString(J)]][i] = sum(
          Nhatvecs[[toString(J)]][i,] * 
            ( D3 %>% 
                group_by(incomebracket) %>% 
                summarise(m = mean(logRENTcentered)) %>% 
                arrange(incomebracket) %>% 
                select(m) %>%
                unlist
            )/sum(Nhatvecs[[toString(J)]][i,])
        )
        
        
        
        D1naivePS_subpop[[toString(J)]][i] = sum(
          Nhatvecs[[toString(J)]][i, subpopestimandJindices[[toString(J)]]] * 
            ( D1 %>% 
                filter(INCTOT <= 48000) %>%
                group_by(incomebracket) %>% 
                summarise(m = mean(logRENTcentered)) %>% 
                arrange(incomebracket) %>% 
                select(m) %>%
                unlist
            )/sum(Nhatvecs[[toString(J)]][i, subpopestimandJindices[[toString(J)]]])
        )
        
        D2naivePS_subpop[[toString(J)]][i] = sum(
          Nhatvecs[[toString(J)]][i, subpopestimandJindices[[toString(J)]]] * 
            ( D2 %>% 
                filter(INCTOT <= 48000) %>%
                group_by(incomebracket) %>% 
                summarise(m = mean(logRENTcentered)) %>% 
                arrange(incomebracket) %>% 
                select(m) %>%
                unlist
            )/sum(Nhatvecs[[toString(J)]][i, subpopestimandJindices[[toString(J)]]])
        )
        
        D3naivePS_subpop[[toString(J)]][i] = sum(
          Nhatvecs[[toString(J)]][i, subpopestimandJindices[[toString(J)]]] * 
            ( D3 %>% 
                filter(INCTOT <= 48000) %>%
                group_by(incomebracket) %>% 
                summarise(m = mean(logRENTcentered)) %>% 
                arrange(incomebracket) %>% 
                select(m) %>%
                unlist
            )/sum(Nhatvecs[[toString(J)]][i, subpopestimandJindices[[toString(J)]]])
        )
        
        
        
        
        
      }
      
    }
    
    # STEP 12: SAVE FILES FOR THIS CURRENT ACSstratasize and Dsize ####
    saveRDS(object = MRPposteriorsamplesvec_D1, file = paste0(ACSstratasize, "_", Dsize, "_MRPposteriorsamplesvec_D1_fiveyearACS.rds"))
    saveRDS(object = MRPposteriorsamplesvec_D2, file = paste0(ACSstratasize, "_", Dsize, "_MRPposteriorsamplesvec_D2_fiveyearACS.rds"))
    saveRDS(object = MRPposteriorsamplesvec_D3, file = paste0(ACSstratasize, "_", Dsize, "_MRPposteriorsamplesvec_D3_fiveyearACS.rds"))
    
    saveRDS(object = MRPsdnaivevec_D1, file = paste0(ACSstratasize, "_", Dsize, "_MRPsdnaivevec_D1_fiveyearACS.rds"))
    saveRDS(object = MRPsdnaivevec_D2, file = paste0(ACSstratasize, "_", Dsize, "_MRPsdnaivevec_D2_fiveyearACS.rds"))
    saveRDS(object = MRPsdnaivevec_D3, file = paste0(ACSstratasize, "_", Dsize, "_MRPsdnaivevec_D3_fiveyearACS.rds"))
    
    saveRDS(object = MRPsdvec_D1, file = paste0(ACSstratasize, "_", Dsize, "_MRPsdvec_D1_fiveyearACS.rds"))
    saveRDS(object = MRPsdvec_D2, file = paste0(ACSstratasize, "_", Dsize, "_MRPsdvec_D2_fiveyearACS.rds"))
    saveRDS(object = MRPsdvec_D3, file = paste0(ACSstratasize, "_", Dsize, "_MRPsdvec_D3_fiveyearACS.rds"))
    
    saveRDS(object = D1mean, file = paste0(ACSstratasize, "_", Dsize, "_D1mean_fiveyearACS.rds"))
    saveRDS(object = D2mean, file = paste0(ACSstratasize, "_", Dsize, "_D2mean_fiveyearACS.rds"))
    saveRDS(object = D3mean, file = paste0(ACSstratasize, "_", Dsize, "_D3mean_fiveyearACS.rds"))
    saveRDS(object = D1sdmean, file = paste0(ACSstratasize, "_", Dsize, "_D1sdmean_fiveyearACS.rds"))
    saveRDS(object = D2sdmean, file = paste0(ACSstratasize, "_", Dsize, "_D2sdmean_fiveyearACS.rds"))
    saveRDS(object = D3sdmean, file = paste0(ACSstratasize, "_", Dsize, "_D3sdmean_fiveyearACS.rds"))
    
    
    
    saveRDS(object = MRPposteriorsamplesvec_D1_subpop, file = paste0(ACSstratasize, "_", Dsize, "_MRPposteriorsamplesvec_D1_subpop_fiveyearACS.rds"))
    saveRDS(object = MRPposteriorsamplesvec_D2_subpop, file = paste0(ACSstratasize, "_", Dsize, "_MRPposteriorsamplesvec_D2_subpop_fiveyearACS.rds"))
    saveRDS(object = MRPposteriorsamplesvec_D3_subpop, file = paste0(ACSstratasize, "_", Dsize, "_MRPposteriorsamplesvec_D3_subpop_fiveyearACS.rds"))
    
    saveRDS(object = MRPsdnaivevec_D1_subpop, file = paste0(ACSstratasize, "_", Dsize, "_MRPsdnaivevec_D1_subpop_fiveyearACS.rds"))
    saveRDS(object = MRPsdnaivevec_D2_subpop, file = paste0(ACSstratasize, "_", Dsize, "_MRPsdnaivevec_D2_subpop_fiveyearACS.rds"))
    saveRDS(object = MRPsdnaivevec_D3_subpop, file = paste0(ACSstratasize, "_", Dsize, "_MRPsdnaivevec_D3_subpop_fiveyearACS.rds"))
    
    saveRDS(object = MRPsdvec_D1_subpop, file = paste0(ACSstratasize, "_", Dsize, "_MRPsdvec_D1_subpop_fiveyearACS.rds"))
    saveRDS(object = MRPsdvec_D2_subpop, file = paste0(ACSstratasize, "_", Dsize, "_MRPsdvec_D2_subpop_fiveyearACS.rds"))
    saveRDS(object = MRPsdvec_D3_subpop, file = paste0(ACSstratasize, "_", Dsize, "_MRPsdvec_D3_subpop_fiveyearACS.rds"))
    
    saveRDS(object = D1mean_subpop, file = paste0(ACSstratasize, "_", Dsize, "_D1mean_subpop_fiveyearACS.rds"))
    saveRDS(object = D2mean_subpop, file = paste0(ACSstratasize, "_", Dsize, "_D2mean_subpop_fiveyearACS.rds"))
    saveRDS(object = D3mean_subpop, file = paste0(ACSstratasize, "_", Dsize, "_D3mean_subpop_fiveyearACS.rds"))
    saveRDS(object = D1sdmean_subpop, file = paste0(ACSstratasize, "_", Dsize, "_D1sdmean_subpop_fiveyearACS.rds"))
    saveRDS(object = D2sdmean_subpop, file = paste0(ACSstratasize, "_", Dsize, "_D2sdmean_subpop_fiveyearACS.rds"))
    saveRDS(object = D3sdmean_subpop, file = paste0(ACSstratasize, "_", Dsize, "_D3sdmean_subpop_fiveyearACS.rds"))
    
    saveRDS(object = Nhatvecs_subpop, file = paste0(ACSstratasize, "_", Dsize, "_Nhatvecs_subpop_fiveyearACS.rds"))
    
    saveRDS(object = D1naivePS, file = paste0(ACSstratasize, "_", Dsize, "_D1naivePS_fiveyearACS.rds"))
    saveRDS(object = D2naivePS, file = paste0(ACSstratasize, "_", Dsize, "_D2naivePS_fiveyearACS.rds"))
    saveRDS(object = D3naivePS, file = paste0(ACSstratasize, "_", Dsize, "_D3naivePS_fiveyearACS.rds"))
    saveRDS(object = D1naivePS_subpop, file = paste0(ACSstratasize, "_", Dsize, "_D1naivePS_subpop_fiveyearACS.rds"))
    saveRDS(object = D2naivePS_subpop, file = paste0(ACSstratasize, "_", Dsize, "_D2naivePS_subpop_fiveyearACS.rds"))
    saveRDS(object = D3naivePS_subpop, file = paste0(ACSstratasize, "_", Dsize, "_D3naivePS_subpop_fiveyearACS.rds"))
    
  }
  
}

