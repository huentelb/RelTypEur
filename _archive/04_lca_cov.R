## A typology of nuclear and extended family relations in the United States
## Bettina Hünteler
## 23.02.2024
## huenteler@wiso.uni-koeln.de


#### 04 RUN LCA ####


library(multilevLCA)



### 04a PREPARING COVARIATES ####
USA_lc <- USA %>% 
  # select variables necessary for analyzing classes
  dplyr::select(anc_id, kin_cat_med, kin_cat_small, kin_typ, kin_cat.l, kin_cat, anc_gnd, anc_gnd.l, anc_eth, anc_his2, anc_his1, anc_bic, anc_age,
                anc_eduall, anc_emp.l, anc_emp,
                rel_cnf, rel_cnt, rel_tra, rel_clo) %>% 
  
  # generate "CLASS" containing most likely class based on chosen cluster solution (lcx)
  # generate 'RACE' variable (1. Asian-American, 2. White, 3. Black, 4. Other (incl Pacific Islander)) 
    # Asian
    mutate(
      race = ifelse(anc_eth == 1, 1,
                  # White
                  ifelse(anc_eth == 2, 2, 
                         # Black
                         ifelse(anc_eth == 3, 3, 
                                # Other
                                ifelse(anc_eth == 4 | anc_eth == 5, 4, 5)))),
      # factor "RACE"
      race.l = factor(as.character(race), levels = c("1","2","3","4"), 
                      labels = c("Asian",
                                 "White",
                                 "Black",
                                 "Other")),
         # binary "RACE"
         asian = ifelse(race == 1, 1, 0),
         white = ifelse(race == 2, 1, 0),
         black = ifelse(race == 3, 1, 0),
         other = ifelse(race == 4, 1, 0),
         
         
         
         # (wave 2 only, many missings!) generate 'ETHNICITY' variable (1. Asian-American, 2. Non-Hispanic White, 3. Hispanic White, 4. Non-Hispanic Black, 5. Hispanic Black, 6. Other (incl Pacific Islander))
         # Non-Hispanic Asian
          eth = ifelse(anc_eth == 2 & anc_his2 == 1, 1,
                       # Hispanic (anc_his2 (from Hispanic, Latinx or Spanish origin))
                       ifelse(anc_his2 == 2, 2, 
                              # Non-Hispanic Black
                              ifelse(anc_eth == 3 & anc_his2 == 1, 3, 
                                     # Non-Hispanic Asian
                                     ifelse(anc_eth == 1 & anc_his2 == 1, 4, 5)))),
          eth = ifelse(is.na(anc_his2), 6, eth),
          # factor "ETHNICITY"
          eth.l = factor(as.character(eth), levels = c("1","2","3","4","5"), 
                         labels = c("Non-Hispanic White",
                                    "Hispanic",
                                    "Non-Hispanic Black",
                                    "Non-Hispanic Asian",
                                    "Non-Hispanic Other")),
         # binary "ETHNICITY"
         eth1_nhwhite   = ifelse(eth == 1, 1, 0),
         eth2_hisp      = ifelse(eth == 2, 1, 0), 
         eth3_nhblack   = ifelse(eth == 3, 1, 0),
         eth4_nhasian   = ifelse(eth == 4, 1, 0),
         eth5_nhother   = ifelse(eth == 5, 1, 0),
         
         
         
         # "IMMIGRANT"
         mig = ifelse(anc_bic == 2, 1, 0),
         mig.l = factor(as.character(mig), levels = c("0","1"), 
                        labels = c("Born in USA",
                                   "Migration background")),
         
         
         # "FEMALE"
         female = ifelse(anc_gnd == 2, 1, 0),
         female.l = factor(as.character(female), levels = c("0", "1"),
                           labels = c("Male",
                                      "Female")),
         
         # EMPLYOMENT
         lfs = ifelse(anc_emp >= 4, 4, anc_emp),
         lfs.l = factor(as.character(lfs), levels = c("1", "2", "3", "4"),
                        labels = c("Full-time employed",
                                   "Part-time employed",
                                   "Self-employed",
                                   "Not in paid work")),
         fte = ifelse(lfs == 1, 1, 0),
         pte = ifelse(lfs == 2, 1, 0), 
         se  = ifelse(lfs == 3, 1, 0),
         nw  = ifelse(lfs == 4, 1, 0),
         
         # EDUCATION
         edu.l = factor(as.character(anc_eduall), levels = c("1", "2", "3"),
                        labels = c("Low", "Medium", "High")),
         low = ifelse(anc_eduall == 1, 1, 0),
         med = ifelse(anc_eduall == 2, 1, 0),
         hi  = ifelse(anc_eduall == 3, 1, 0),
         
         
         # KIN TYPE binary
         kin01_f = ifelse(kin_cat.l == "Father", 1, 0),
         kin02_m = ifelse(kin_cat.l == "Mother", 1, 0),
         kin03_bro = ifelse(kin_cat.l == "Brother", 1, 0),
         kin04_sis = ifelse(kin_cat.l == "Sister", 1, 0),
         kin05_pgf = ifelse(kin_cat.l == "Paternal grandfather", 1, 0),
         kin06_mgf = ifelse(kin_cat.l == "Maternal grandfather", 1, 0),
         kin07_pgm = ifelse(kin_cat.l == "Paternal grandmother", 1, 0),
         kin08_mgm = ifelse(kin_cat.l == "Maternal grandmother", 1, 0),
         kin09_phs = ifelse(kin_cat.l == "Paternal halfsibling", 1, 0),
         kin10_mhs = ifelse(kin_cat.l == "Maternal halfsibling", 1, 0),
         kin11_pun = ifelse(kin_cat.l == "Paternal uncle", 1, 0),
         kin12_mun = ifelse(kin_cat.l == "Maternal uncle", 1, 0),
         kin13_pau = ifelse(kin_cat.l == "Paternal aunt", 1, 0),
         kin14_mau = ifelse(kin_cat.l == "Maternal aunt", 1, 0),
         kin15_pcou = ifelse(kin_cat.l == "Paternal cousin", 1, 0),
         kin16_mcou = ifelse(kin_cat.l == "Maternal cousin", 1, 0))


# Add included relationship indicators
# 1. Subset indicators to one df
help <- subset(lca.df[, c("anc_id", "kin_typ", 
                          "adv_lca", "cmf_lca", "mon_lca", "cou_lca", "sup_lca", "clo_lca", "cnf_lca", "cnt_lca", "tra_lca")])

# 2. Recode relationship indicators to 0 and 1
rel_ind <- help %>% 
  mutate(adv_lca = adv_lca - 1,
         cmf_lca = cmf_lca - 1,
         mon_lca = mon_lca - 1,
         cou_lca = cou_lca - 1,
         sup_lca = sup_lca - 1,
         cnt_lca = cnt_lca - 1,
         cnf_lca = cnf_lca - 1,
         clo_lca = clo_lca - 1,
         tra_lca = tra_lca - 1)

# 3. Merge to USA_lc data frame by anchor id and kin id
USA_lc <- merge(USA_lc, rel_ind, by = c("anc_id", "kin_typ"), all.x = TRUE)



#### 04a LCA WITH STEPMIX ####

#https://colab.research.google.com/drive/1MzGHRO5kfs9OT3cRICJ1Ey94PHHnxFdO#scrollTo=ywWACtFwgT71
#https://github.com/Labo-Lacourse/StepMixR

# install.packages("stepmixr", "fossil")
library(stepmixr)
library(fossil)


# Install StepMix in Python
library(reticulate)
#py_install("StepMix")
# install.stepmix()
#pipinstall stepmix

# 1. Original LCA model to see how it matches poLCA
# 2. Naive three-step LCA model to see how it matches poLCA + multinom regression
# 3. Corrected three-step LCA model following (Vermunt 2010)

  
k <- ""  # number of classes
m <- 3000 # maximum number iterations
r <- 5 # number of times to estimate models (nrep > 1 automates search for global max)

seed <- 240792
set.seed(240792)

k <- 5

#### 1. Original LCA model ####




# BINARY 1-STEP measurement indicators (= poLCA)

# Define measurement variables
lc3step_X_bin.df <- USA_lc %>%
  dplyr::select(tra_lca, cnt_lca, cnf_lca, clo_lca, sup_lca, mon_lca)

write.csv(lc3step_X_bin.df, 
          na = "NaN", 
          "/Users/Bettina/sciebo/projects/Kinmatrix/analyses/data/Xbin.csv")


m1_bin <- stepmix(n_components = k, # number of classes
                  n_steps = 1, 
                  correction = , # None -> naive model
                  measurement = "binary_nan",
                  max_iter = m,
                  n_init = 1, 
                  verbose = 1, 
                  random_state = seed)


fit_1bin <- fit(m1_bin, 
                X = lc3step_X_bin.df)




# # CONTINUOUS 1-STEP measurement indicators
# 
# # Define measurement variables
# lc3step_X_cnt.df <- USA_lc %>%
#   dplyr::select(rel_tra, rel_cnt, rel_cnf, rel_clo, sup_lca, mon_lca)
# 
# 
# m1_cnt <- stepmix(n_components = k, # number of classes
#                   n_steps = 1, 
#                   correction = , # None -> naive model
#                   measurement = "continuous_nan",
#                   max_iter = m,
#                   n_init = r, 
#                   verbose = 1, 
#                   random_state = seed)
# 
# 
# fit_1cnt <- fit(m1_cnt, 
#                 X = lc3step_X_cnt.df)


# (CATEGORICAL 1-STEP measurement indicators)
  

# MIXED 1-STEP measurement indicators (binary + continuous)
lc3step_X_cnt_std.df <- USA_lc %>%
  # replace travel time by ethnicity-kintype-specific median
  # group_by(eth, kin_cat) %>% 
  # mutate(rel_tra = ifelse(is.na(rel_tra), median(rel_tra, na.rm = TRUE), rel_tra)) %>% 
  # ungroup() %>% 
  # standardize scales to 0 - 1
  mutate(rel_tra = (rel_tra - min(rel_tra, na.rm = TRUE)) / (max(rel_tra, na.rm = TRUE) - min(rel_tra, na.rm = TRUE)),
         rel_cnt = (rel_cnt - min(rel_cnt, na.rm = TRUE)) / (max(rel_cnt, na.rm = TRUE) - min(rel_cnt, na.rm = TRUE)),
         rel_cnf = (rel_cnf - min(rel_cnf, na.rm = TRUE)) / (max(rel_cnf, na.rm = TRUE) - min(rel_cnf, na.rm = TRUE)),
         rel_clo = (rel_clo - min(rel_clo, na.rm = TRUE)) / (max(rel_clo, na.rm = TRUE) - min(rel_clo, na.rm = TRUE))) %>%
  # invert negatively-coded scales
  mutate(rel_tra = max(rel_tra, na.rm = TRUE) - rel_tra,
         rel_cnt = max(rel_cnt, na.rm = TRUE) - rel_cnt) %>% 
  dplyr::select(tra_lca, rel_cnt, cnf_lca, rel_clo, sup_lca, mon_lca)  
  
write.csv(lc3step_X_cnt_std.df, 
          na = "NaN", 
          "/Users/Bettina/sciebo/projects/Kinmatrix/analyses/data/Xcnt_std.csv")

# standardized
lc3step_X_cnt.df <- USA_lc %>%
  # replace travel time by ethnicity-kintype-specific median
  # group_by(eth, kin_cat) %>% 
  ## standardize scales to 0 - 1
  # mutate(rel_tra = (rel_tra - min(rel_tra, na.rm = TRUE)) / (max(rel_tra, na.rm = TRUE) - min(rel_tra, na.rm = TRUE)),
  #        rel_cnt = (rel_cnt - min(rel_cnt, na.rm = TRUE)) / (max(rel_cnt, na.rm = TRUE) - min(rel_cnt, na.rm = TRUE)),
  #        rel_cnf = (rel_cnf - min(rel_cnf, na.rm = TRUE)) / (max(rel_cnf, na.rm = TRUE) - min(rel_cnf, na.rm = TRUE)),
  #        rel_clo = (rel_clo - min(rel_clo, na.rm = TRUE)) / (max(rel_clo, na.rm = TRUE) - min(rel_clo, na.rm = TRUE))) %>%
  # # ungroup() %>% 
  # invert negatively-coded scales
  mutate(rel_tra = max(rel_tra, na.rm = TRUE)+1 - rel_tra,
         rel_cnt = max(rel_cnt, na.rm = TRUE)+1 - rel_cnt) %>% 
  dplyr::select(tra_lca, rel_cnt, cnf_lca, rel_clo, sup_lca, mon_lca)  

write.csv(lc3step_X_cnt.df, 
          na = "NaN", 
          "/Users/Bettina/sciebo/projects/Kinmatrix/analyses/data/Xcnt.csv")


# (Unspecified variables are simply not included in mixed_data)
  md <- mixed_descriptor(
    data = lc3step_X_cnt.df, 
    continuous = 1:4, 
    binary = 5:6)

m1_mixed <- stepmix(n_components = k, # number of classes
              n_steps = 1, 
              correction = , # None -> naive model
              measurement = md$descriptor,
              max_iter = m,
              n_init = r, 
              verbose = 1, 
              random_state = seed)

fit_1mixed <- fit(m1_mixed, 
               X = lc3step_X_cnt.df)




folder.graph.step3 <- paste0(folder.graph.model,"3-step/")
ifelse(!dir.exists(folder.graph.step3), dir.create(folder.graph.step3), "Folder already exists")
save(fit_1, file = paste0(folder.graph.step3,"fit_1.RData"))


# Save class membership to original df
# lca.df[,'pred1'] 
# pred1 <- predict(fit_1mixed,
#         X = lc3step_X_cnt.df[c(1:63672), ],)
# lca.df[,'m1_predclass'] <- predict(fit_1, X = lc4_1step.df)
# lca.df$m1_predclass2 <- predict(fit_1, X = lc4_1step.df)

# The cross tabulation and the Rand Score show that our groups somewhat align with the flower types, 
# but not as well as those obtained with the continuous LCA model from the previous section.
#fossil::rand.index(as.numeric(iris$Species), iris$cat_pred)






#### 2. Naive three-step LCA model ####


# Structural variabels
lc4_3step_Zp.df <- USA_lc %>%
  dplyr::select(
    # kinype (ref: father)
    kin02_m,
    kin03_bro,
    kin04_sis,
    kin05_pgf,
    kin06_mgf,
    kin07_pgm,
    kin08_mgm,
    kin09_phs,
    kin10_mhs,
    kin11_pun,
    kin12_mun,
    kin13_pau,
    kin14_mau,
    kin15_pcou, 
    kin16_mcou, 
    # controls (refs: nhwhite, male)
    eth2_hisp,   
    eth3_nhblack,  
    eth4_nhasian, 
    eth5_nhother,
    female)


write.csv(lc4_3step_Zp.df, 
          na = "NaN", 
          "/Users/Bettina/sciebo/projects/Kinmatrix/analyses/data/Z.csv")


# Structural variabels for PREDICTED PROBABILITIES
lc4_3step_Zpp.df <- USA_lc %>%
  dplyr::select(
    # kinype (ref: father)
    kin01_f,
    kin02_m,
    kin03_bro,
    kin04_sis,
    kin05_pgf,
    kin06_mgf,
    kin07_pgm,
    kin08_mgm,
    kin09_phs,
    kin10_mhs,
    kin11_pun,
    kin12_mun,
    kin13_pau,
    kin14_mau,
    kin15_pcou, 
    kin16_mcou, 
    # controls (refs: nhwhite, male)
    eth1_nhwhite,
    eth2_hisp,   
    eth3_nhblack,  
    eth4_nhasian, 
    eth5_nhother,
    female)
write.csv(lc4_3step_Zpp.df, 
          na = "NaN", 
          "/Users/Bettina/sciebo/projects/Kinmatrix/analyses/data/Zpp.csv")


# numerical optimization method for covariates
covariate_params = list(
  method = 'newton-raphson', 
  max_iter = as.integer(1), 
  intercept = TRUE)


# NAIVE-BINARY
m3_bin_n <- stepmix(n_components = k, # number of classes
              n_steps = 1, # for three-step-model with fixed measurement param -> 2: for measurement and structural model simultaneously, 2
              correction = , # "None" -> naive model
              measurement = "binary_nan",
              structural = "covariate",
              structural_params = covariate_params,
              max_iter = m,
              n_init = r, 
              verbose = 1, 
              random_state = seed)


fit_3bin_n <- fit(m3_bin_n, 
             X = lc3step_X_bin.df,
             Y = lc4_3step_Zp.df)

# save(fit_3_n, file = paste0(folder.graph.step3,"fit_3_n.RData"))

pr1 <- predict(fit_3_n, lc3step_X_bin.df, lc4_3step_Zp.df)

USA_lc$pr1 <- predict(fit_3_n, lc3step_X_bin.df, lc4_3step_Zp.df)

modelfit2 <- score(fit_3_n)
# 
# model.fit <- fit(model, continuous_data_nan)
# 
# # Save class membership predictions to df
# continuous_data_nan[,'continuous_pred_nan'] <- predict(model.fit, continuous_data_nan)
# 
# table(USA_lc[,'predict_3'], FUN = mean)




#### 3. Bias-adjusted three-step LCA model with ML ####


m3_ml <- stepmix(n_components = k, # number of classes
            n_steps = 3, # for three-step-model with fixed measurement param -> 2: for measurement and structural model simultaneously, 2
            correction = "ML", # ML correction from Vermunt, 2010 ("None" -> naive model)
            measurement = "binary_nan",
            structural = "covariate",
            structural_params = covariate_params,
            max_iter = m,
            n_init = r, 
            verbose = 1, 
            random_state = seed)

# Fit model
fit_3_ml <- fit(m3_ml, 
             X = lc3step_X.df,
             Y = lc4_3step_Zp.df)
save(fit_3_ml, file = paste0(folder.graph.step3,"fit_3_ml.RData"))



bs_params_3_ml = bootstrap_stats(fit_3_ml, 
                                 X = lc3step_X.df, 
                                 Y = lc4_3step_Zp.df, 
                                 n_repetitions = 50)
save(bs_params_3_ml, file = paste0(folder.graph.step3,"bs_params_3_ml.RData"))



#### load estimated models ####
load(paste0(folder.graph.step3,"fit_1.RData"))
load(paste0(folder.graph.step3,"fit_3_n.RData"))
load(paste0(folder.graph.step3,"fit_3_ml.RData"))
load(paste0(folder.graph.step3,"bs_params_3_ml.RData"))

## Félix' code
level_header <- c('model', 'model_name', 'param', 'class_no', 'variable')
bs_params_3_ml[['samples']][, level_header] %>%
  unique

# measurement model means
x1 <- bs_params_3_ml[['mm_mean']][['mm_mean']][1,]
x2 <- bs_params_3_ml[['mm_mean']][['mm_mean']][2,]



for(n in 1:6) 
  bs_params_3_ml[['mm_mean']][['mm_mean']][n,]




# structural model means
x = bs_params_3_ml[['sm_mean']][['sm_mean']][1,]

lapply(x, class)



##### Extracting the coeficients (try outs)
bs_params_3_ml[['samples']][51,]

df <- data.frame(matrix(NA, ncol = 7))
names(df) <- c('model', 'model_name', 'param', 'class_no', 'variable', 'value', 'rep')
for(n in 1:7000){
  print(n)
  r <-  bs_params_3_ml[['samples']][n,]
  r <- as.data.frame(r)
  df <- rbind(df, r)
}

df <- data.frame(matrix(NA, ncol = 7))
names(df) <- c('model', 'model_name', 'param', 'class_no', 'variable', 'value', 'rep')
r <-  bs_params_3_ml[['samples']][n,]
r <- as.data.frame(r)
df[1,] <- r


lapply(r, class)

xxx <- r[['rep']]

print(xxx)
py_to_r(xxx, convert = TRUE)


#### 3. Bias-adjusted three-step LCA model with BCH ####

m3_bch <- stepmix(n_components = k, # number of classes
              n_steps = 3, # for three-step-model with fixed measurement param -> 2: for measurement and structural model simultaneously, 2
              correction = "BCH", # ML correction from Vermunt, 2010 ("None" -> naive model)
              measurement = "binary_nan",
              structural = "covariate",
              structural_params = covariate_params,
              max_iter = m,
              n_init = r, 
              verbose = 1, 
              random_state = seed)


# Fit model
fit_3_bch <- fit(m3_bch, 
             X = lc3step_X.df,
             Y = lc4_3step_Zp.df)
save(fit_3_bch, file = paste0(folder.graph.step3,"fit_3_bch.RData"))


# # Save class membership to original df
# lca.df[,'pred3'] <- predict(fit3, 
#                             X = lc4_3step.df[c(1:6), ],
#                             Y = lc4_3step.df[c(7:13), ],)




#### 4. Two-step LCA model (stepmix) ####


m2 <- stepmix(n_components = k, # number of classes
              n_steps = 2, # two-step estimation
              measurement = "binary",
              structural = "covariate",
              structural_params = covariate_params,
              max_iter = m,
              n_init = 2, 
              verbose = 1, 
              random_state = seed)

# Fit model
fit_2 <- fit(m2, 
                X = lc3step_X.df,
                Y = lc4_3step_Zp.df)

# Cannot save class membership to original df because no class membership is predicted,
# eliminating the problem of classification uncertainty




# Code based on: https://statistics.ohlsen-web.de/latent-class-analysis-polca/

# Fitting an LCM in poLCA based on: http://daob.nl/wp-content/uploads/2015/07/ESRA-course-slides.pdf 

# not recommended: ONE STEP APPROACH: Define function (response ~ predictors) -> Relationship indicators to be included
f_1 <- cbind(cnf_lca, cnt_lca, clo_lca, mon_lca, adv_lca, cmf_lca) ~ kin_cat.l + anc_age + anc_gnd + anc_eth.l + anc_bic.l + anc_emp.l + anc_eduall.l


k <- ""  # number of classes
m <- 3000 # maximum number iterations
r <- 15 # number of times to estimate models (nrep > 1 automates search for global max)

set.seed(240792)

k <- 4
lc4_1 <- poLCA(f_1, 
             lca.df, 
             nclass = k,
             maxiter = m,
             nrep = r,
             na.rm = FALSE) 

summary(lc4_1)

## TWO-STEP APPROACH based on Lyrvall et al 2024 (multilevel LCA)






## THREE-STEP APPROACH based on Bolck, Croon & Hagenaars (2004) and expanded by Vermunt (2010) -> VERMUNT METHOD

# 1. LCA model including only LC indicator variables
# 2. Most likely class variable based on LC posterior distribution
# 3. Most-likely-class <- predictors, accounting for misclassification from step 2 


# Following application from https://www.stata.com/meeting/uk22/slides/UK22_Tompsett.pdf


## Step 1               ####
## Regular LCA model

k <- ""  # number of classes
m <- 3000 # maximum number iterations
r <- 15 # number of times to estimate models (nrep > 1 automates search for global max)

set.seed(240792)

# k <- 4
# lc4 <- poLCA(f, 
#              lca.df, 
#              nclass = k,
#              maxiter = m,
#              nrep = r,
#              na.rm = FALSE) 

load(paste0(folder.graph.hi,model,"/lc4.RData"))

# Adjust to ideal class
lc <- lc4



## Step 2             ####
## 2.1 Identify most likely class (modal) W 
library(data.table)
USA_lc3 <- USA

probs <- as.data.table(lc$posterior)
USA_lc3$W <- modclass <- apply(probs,1,which.max)



## 2.2 Probability of misclassifying the classes (not assigning the true class) Q

# Obtain P(C = j|W = i)
nclass = 4
Ptable <- cbind(probs,modclass)
Pmatrix <- matrix(0,nclass,nclass)
Npmatrix <- matrix(0,nclass,nclass)
for (i in 1:nclass) {
  for (j in 1:nclass) {
    Pmatrix[i,j] <- sum(subset(Ptable,modclass==i)[,..j])
      Npmatrix[i,j] <- Pmatrix[i,j]*table(modclass)[i]
  }}

# The Q matrix is then calculated as
denom <- colSums(Npmatrix)
Qmatrix <- matrix(0,nclass,nclass)
for (i in 1:nclass) {
  for (j in 1:nclass) {
    Qmatrix[j,i]<-Npmatrix[i,j]/denom[j]
  }}


## Step 3             ####
## Refit LCA including W as single manifest variable + including covariates


## NOT FOUND ANY IMPLEMENTATION IN R YET --> in UK22 slides, they moved to stata to run SEM while accounting for Q
# Adjust probabilities in Q to fit model in STATA (with reference class 4)
lQ <- log(Qmatrix/Qmatrix[,4])
lQ

# m <- dimension of lQ != 0 
m <- 4*3

USA_lc3$lq <- c(as.vector(t(lQ[,-4])),rep(0,(n-m)))












#### 04b TWO STEP LCA MODEL (multilevLCA) ####
# requires 0-1 binary indicators
# Lyrvall, J., Di Mari, R., Bakk, Z., Oser, J., & Kuha, J. (2024). multilevLCA: An R Package for Single-Level and Multilevel Latent Class Analysis with Covariates (arXiv:2305.07276). arXiv. http://arxiv.org/abs/2305.07276


# 1. Classic LCA to compare with poLCA output
data <- USA_lc 
Y <- c("cnf_lca", "cnt_lca", "clo_lca", "mon_lca", "adv_lca", "cmf_lca")
iT <- k  
lca2_empty <- multiLCA(data, 
                       Y, 
                       iT, 
                       fixedpars = 1,
                       extout = TRUE,
                       verbose = 1) # one-step estimator

lca2_empty

plot(lca2_empty,
     main = "Empty 2-step model")

vU_modal_empty <- as.data.frame(lca2_empty$vU_modal)
round(prop.table(table(vU_modal$C)), 2)
lca2_empty$vPi


unit_class_prop[unit_class_prop[,"kin_cat.l.Mother"] == 1,] 

# 2. Two-step LCA to include covariates
Z <- c("kin_cat.l", "anc_age", "female.l", "race.l", "mig.l", "lfs.l", "edu.l")
lca2 <- multiLCA(data, 
                 Y, 
                 iT,
                 Z = Z,
                 extout = TRUE,
                 fixedpars = 1,
                 verbose = 1) # default: two-step estimator (Di Mari et al. 2023)

lca2
plot(lca2,
     main = "2-step model")

vU_modal_2 <- as.data.frame(lca2$vU_modal)
round(prop.table(table(vU_modal_2$C)), 2)

# two-stage estimator
lca2.2 <- multiLCA(data, 
                   Y, 
                   iT,
                   Z = Z,
                   extout = TRUE,
                   fixedpars = 2,
                   verbose = 1) # two-stage estimator 

lca2.2
plot(lca2.2,
     main = "2-stage model")




#### 04c TABLES ####

## Class proportions by covariates
# Mothers
unit_class_prop <- lca2$mU 
unit_class_prop_M <- unit_class_prop[unit_class_prop[,"kin_cat.l.Mother"] == 1,] 
class_prop_M <- apply(unit_class_prop_M[,c("C1","C2","C3","C4")], 2, mean)


unit_class_prop.2 <- lca2.2$mU 
unit_class_prop_M.2 <- unit_class_prop.2[unit_class_prop.2[,"kin_cat.l.Mother"] == 1,] 
class_prop_M.2 <- apply(unit_class_prop_M.2[,c("C1","C2","C3","C4")], 2, mean)

round(class_prop_M, 2)
round(class_prop_M.2, 2)


# Paternal cousins
unit_class_prop_PC <- unit_class_prop[unit_class_prop[,"kin_cat.l.Paternal cousin"] == 1,] 
class_prop_PC <- apply(unit_class_prop_PC[,c("C1","C2","C3","C4")], 2, mean)

unit_class_prop_PC.2 <- unit_class_prop.2[unit_class_prop.2[,"kin_cat.l.Paternal cousin"] == 1,] 
class_prop_PC.2 <- apply(unit_class_prop_PC.2[,c("C1","C2","C3","C4")], 2, mean)

round(class_prop_PC, 2)
round(class_prop_PC.2, 2)

save(lca2, file = paste0(folder.graph.model,"2step_lca2.RData"))
save(lca2.2, file = paste0(folder.graph.model,"2step_lca2_2.RData"))
save(lca2_empty, file = paste0(folder.graph.model,"2step_lca2_empty.RData"))

# Make table containing all class probabilities by all covariates (pred class probs)
class_props <- apply(unit_class_prop_M[,c("C1","C2","C3","C4")], 2, mean)




table <- as.data.frame(round(lca2[["cGamma"]], 2)) %>% 
  cbind(round(lca2$SEs_cor_gamma, 3)) 
rownames(table) <- c("Intercept", 
                     "Mother",
                     "Brother",
                     "Sister",
                     "Paternal grandfather",
                     "Maternal grandfather",
                     "Paternal grandmother",
                     "Maternal grandmother",
                     "Paternal halfsibling",
                     "Maternal halfsibling",
                     "Paternal uncle",
                     "Maternal uncle",
                     "Paternal aunt",
                     "Maternal aunt",
                     "Paternal cousin",
                     "Maternal cousin",
                     "Age", "Female", "White", "Black", "Other", "Migration Background",
                     "Part-time Employment", "Self-employed", "Not in paid work", 
                     "Medium edu", "High edu")





#### # Mixed with poLCA ####
f <- cbind(rel_cnf, rel_cnt, rel_clo, mon_lca, sup_lca, rel_tra)~1
k <- ""  # number of classes
m <- 3000 # maximum number iterations
r <- 5 # number of times to estimate models (nrep > 1 automates search for global max)

set.seed(240792)

polca_mix <- USA_lc %>%
  # replace travel time by ethnicity-kintype-specific median
  group_by(eth, kin_cat) %>% 
  mutate(rel_tra = ifelse(is.na(rel_tra), median(rel_tra, na.rm = TRUE), rel_tra)) %>% 
  ungroup() %>% 
  # invert negatively-coded scales
  mutate(rel_tra = max(rel_tra, na.rm = TRUE) - rel_tra,
         rel_cnt = max(rel_cnt, na.rm = TRUE) - rel_cnt) %>% 
  dplyr::select(rel_tra, rel_cnt, rel_cnf, rel_clo, sup_lca, mon_lca)  %>% 
  mutate(
    rel_cnf = rel_cnf + 1, 
    rel_cnt = rel_cnt + 1, 
    rel_clo = rel_clo + 1, 
    mon_lca = mon_lca + 1, 
    sup_lca = sup_lca + 1, 
    rel_tra = rel_tra + 1
  )

k <- 5 # Loglinear independence model
lc5_mix <- poLCA(f, 
                 polca_mix, 
                 nclass = k,
                 maxiter = m,
                 nrep = r,
                 na.rm = FALSE) # retain cases with item missing (instead of case-wise deletion)

lcmodel_mix <- reshape2::melt(lc5_mix$probs, level=2)
lcmodel_mix %>% 
  filter(Var2 == "Pr(2)") %>% 
  ggplot(
    aes(x = factor(L2), 
        y = value, 
        colour = Var1, group = Var1, shape = Var1)) + 
  geom_line(size = .8) +
  geom_point(size = 4) +
  geom_text(
    aes(label = round(value, digits = 2)), 
    vjust = -2, size = 3, fontface = "bold", show.legend = FALSE, position = position_dodge(.4)) +
  labs(x = "Manifest items", 
       y = "Conditional item response probabilities", 
       title = paste0("Latent Class Analysis (poLCA with continuous and binary)")) +
  scale_x_discrete(labels = c("rel_clo" = "Closeness",
                              "rel_cnf" = "Conflict",
                              "rel_cnt" = "Contact",
                              "mon_lca" = "Money",
                              "sup_lca" = "Support",
                              "rel_tra" = "Proximity"),
                   limits = c("rel_tra", "rel_cnt", "rel_clo", "sup_lca", "mon_lca", "rel_cnf")) +
  scale_color_okabeito(name = "Latent Classes", 
                       labels = class.lab.p) +
  scale_shape_discrete(name = "Latent Classes", 
                       labels = class.lab.p) 