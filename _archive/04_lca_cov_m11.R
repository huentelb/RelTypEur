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
                anc_eduall, anc_emp.l, anc_emp) %>% 
  
  # generate "CLASS" containing most likely class based on chosen cluster solution (lcx)
  mutate(# generate 'RACE' variable (1. Asian-American, 2. White, 3. Black, 4. Other (incl Pacific Islander)) 
         # Asian
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
         # Asian
         eth = ifelse(anc_eth == 1 & anc_his2 == 1, 1,
                      # Non-Hispanic White (anc_eth 2 (white) + anc_his2 (NOT from Hispanic, Latinx or Spanish origin))
                      ifelse(anc_eth == 2 & anc_his2 == 1, 2, 
                             # Hispanic White (anc_eth 2 (white) + anc_his2 (from Hispanic, Latinx or Spanish origin))
                             ifelse(anc_eth == 2 & anc_his2 == 2, 3, 
                                    # Black
                                    ifelse(anc_eth == 3 & anc_his2 == 1, 4,
                                           # Hispanic Non-white 
                                           ifelse(anc_eth != 2 & anc_his2 == 2, 5, 6))))),
         eth = ifelse(is.na(anc_his2) & is.na(eth), 6, eth),
         # factor "ETHNICITY"
         eth.l = factor(as.character(eth), levels = c("1","2","3","4","5","6"), 
                        labels = c("Non-Hispanic Asian",
                                   "Non-Hispanic White",
                                   "Hispanic White",
                                   "Non-Hispanic Black",
                                   "Hispanic Non-White",
                                   "Missing")),
         # binary "ETHNICITY"
         eth_nhasian   = ifelse(eth == 1, 1, 0),
         eth_nhwhite   = ifelse(eth == 2, 1, 0), 
         eth_hwhite    = ifelse(eth == 3, 1, 0),
         eth_nhblack   = ifelse(eth == 4, 1, 0),
         eth_hnonwhite = ifelse(eth == 5, 1, 0),
         eth_missing   = ifelse(eth == 6, 1, 0),
         
         
         
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
         father = ifelse(kin_cat.l == "Father", 1, 0),
         mother = ifelse(kin_cat.l == "Mother", 1, 0),
         brother = ifelse(kin_cat.l == "Brother", 1, 0),
         sister = ifelse(kin_cat.l == "Sister", 1, 0),
         patgf = ifelse(kin_cat.l == "Paternal grandfather", 1, 0),
         matgf = ifelse(kin_cat.l == "Maternal grandfather", 1, 0),
         patgm = ifelse(kin_cat.l == "Paternal grandmother", 1, 0),
         matgm = ifelse(kin_cat.l == "Maternal grandmother", 1, 0),
         paths = ifelse(kin_cat.l == "Paternal halfsibling", 1, 0),
         maths = ifelse(kin_cat.l == "Maternal halfsibling", 1, 0),
         patun = ifelse(kin_cat.l == "Paternal uncle", 1, 0),
         matun = ifelse(kin_cat.l == "Maternal uncle", 1, 0),
         patau = ifelse(kin_cat.l == "Paternal aunt", 1, 0),
         matau = ifelse(kin_cat.l == "Maternal aunt", 1, 0),
         patcou = ifelse(kin_cat.l == "Paternal cousin", 1, 0),
         matcou = ifelse(kin_cat.l == "Maternal cousin", 1, 0))


# Add included relationship indicators
# 1. Subset indicators to one df
help <- subset(lca.df[, c("anc_id", "kin_typ", 
                          "adv_lca", "clo_lca", "cmf_lca", "cnf_lca", "cnt_lca", "mon_lca", "cou_lca", "emo_lca", "tra_lca")])

# 2. Recode relationship indicators to 0 and 1
rel_ind <- help %>% 
  mutate(adv_lca = adv_lca - 1,
         cmf_lca = cmf_lca - 1,
         mon_lca = mon_lca - 1,
         cnt_lca = cnt_lca - 1,
         cnf_lca = cnf_lca - 1,
         clo_lca = clo_lca - 1,
         cou_lca = cou_lca - 1,
         emo_lca = emo_lca - 1,
         tra_lca = tra_lca - 1)

# 3. Merge to USA_lc data frame by anchor id and kin id
USA_lc <- merge(USA_lc, rel_ind, by = c("anc_id", "kin_typ"), all.x = TRUE)




# Number of classes
k <- 4  




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

#### 04c LCA WITH STEPMIX ####

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
r <- 2 # number of times to estimate models (nrep > 1 automates search for global max)

seed <- 240792
set.seed(240792)

k <- 4  

#### 1. Original LCA model ####

# Binary latent class model
lc4_3step_X.df <- USA_lc %>%
  dplyr::select(cnf_lca, cnt_lca, clo_lca, emo_lca, mon_lca, tra_lca)


m1 <- stepmix(n_components = k, # number of classes
                n_steps = 1, # for three-step-model with fixed measurement param -> 2: for measurement and structural model simultaneously, 2
                correction = , # None -> naive model
                measurement = "binary_nan",
                max_iter = m,
                n_init = r, 
                verbose = 1, 
                random_state = seed)

# Fit model
fit_1 <- fit(m1, 
             X = lc4_3step_X.df)

folder.graph.step3 <- paste0(folder.graph.model,"3-step/")
ifelse(!dir.exists(folder.graph.step3), dir.create(folder.graph.step3), "Folder already exists")
save(fit_1, file = paste0(folder.graph.step3,"fit_1.RData"))


# Save class membership to original df
# lca.df[,'pred1'] <- predict(fit_1, 
#                             X = lc4_1step.df[c(1:6), ],)
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
    mother, brother, sister, patgf, matgf, patgm, matgm, paths, maths, 
    patun, matun, patau, matau, patcou, matcou,
    # controls (refs: nhwhite, med edu, male)
    eth_hwhite, eth_black, eth_asian, eth_miss,
    female, anc_age, low, hi)



# numerical optimization method for covariates
covariate_params = list(
  method = 'newton-raphson', 
  max_iter = as.integer(1), 
  intercept = TRUE )



m3_n <- stepmix(n_components = k, # number of classes
              n_steps = 1, # for three-step-model with fixed measurement param -> 2: for measurement and structural model simultaneously, 2
              correction = , # "None" -> naive model
              measurement = "binary_nan",
              structural = "covariate",
              structural_params = covariate_params,
              max_iter = m,
              n_init = r, 
              verbose = 1, 
              random_state = seed)




# Fit model
fit_3_n <- fit(m3_n, 
             X = lc4_3step_X.df,
             Y = lc4_3step_Zp.df)

save(fit_3_n, file = paste0(folder.graph.step3,"fit_3_n.RData"))

# pr1 <- predict(fit_2, lc4_3step_X.df, lc4_3step_Zp.df)
# 
# USA_lc$pr1 <- predict(fit_2, lc4_3step_X.df, lc4_3step_Zp.df)
# 
# modelfit2 <- score(fit_2)
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
             X = lc4_3step_X.df,
             Y = lc4_3step_Zp.df)
save(fit_3_ml, file = paste0(folder.graph.step3,"fit_3_ml.RData"))



bs_params_3_ml = bootstrap_stats(fit_3_ml, 
                                 X = lc4_3step_X.df, 
                                 Y = lc4_3step_Zp.df, 
                                 n_repetitions = 100)

level_header = c('model', 'model_name', 'param', 'class_no', 'variable')
bs_params_3_ml[['samples']][, level_header] %>% unique

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
             X = lc4_3step_X.df,
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
                X = lc4_3step_X.df,
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

