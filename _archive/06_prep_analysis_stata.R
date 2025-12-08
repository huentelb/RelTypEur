## A typology of nuclear and extended family relations in the United States
## Bettina Hünteler
## 23.02.2024
## huenteler@wiso.uni-koeln.de


#### 06 CLUSTER DESCRIPTION ####

# Which model is best?
# best <- "lc4"


## 06a Prepare data frame containing all relevant variables for analysis

# 1. Include relevant vars
# 2. Modify for tables

stata_setup <- stata


stata <- stata_setup
stata$W <- modclass <- apply(probs,1,which.max)

stata <- stata %>% 
  # select variables necessary for analyzing classes
  dplyr::select(anc_id, kin_cat_med, kin_cat_small, kin_typ, kin_cat_l, anc_gnd, anc_gnd_l, anc_eth, anc_his2, anc_his1, anc_bic, anc_age,
                anc_eduall, anc_emp_l, anc_emp, W) %>% 
  
        # generate "CLASS" containing most likely class based on chosen cluster solution (lcx)
  mutate(class = lc$predclass,
         
         # define CLASS LABELS
         class_l = factor(as.character(class), levels = c("1","2","3","4"), 
                          labels = class.lab),
         
         
         # generate 'RACE' variable (1. Asian-American, 2. White, 3. Black, 4. Other (incl Pacific Islander)) 
                 # Asian
          race = ifelse(anc_eth == 1, 1,
                        # White
                        ifelse(anc_eth == 2, 2, 
                               # Black
                               ifelse(anc_eth == 3, 3, 
                                      # Other
                                      ifelse(anc_eth == 4 | anc_eth == 5, 4, 5)))),
         # factor "RACE"
         race_l = factor(as.character(race), levels = c("1","2","3","4"), 
                         labels = c("Asian",
                                    "White",
                                    "Black",
                                    "Other")),
         
         
         
         # (wave 2 only, many missings!) generate 'ETHNICITY' variable (1. Asian-American, 2. Non-Hispanic White, 3. Hispanic White, 4. Non-Hispanic Black, 5. Hispanic Black, 6. Other (incl Pacific Islander))
               # Asian
         eth = ifelse(anc_eth == 1, 1,
                       # Non-Hispanic White (anc_eth 2 (white) + anc_his2 (NOT from Hispanic, Latinx or Spanish origin))
                       ifelse(anc_eth == 2 & anc_his2 == 1, 2, 
                              # Hispanic White (anc_eth 2 (white) + anc_his2 (from Hispanic, Latinx or Spanish origin))
                              ifelse(anc_eth == 2 & anc_his2 == 2, 3, 
                                     # Non-Hispanic Black
                                     ifelse(anc_eth == 3 & anc_his2 == 1, 4,
                                            # Hispanic Black
                                            ifelse(anc_eth == 3 & anc_his2 == 2, 5, 6))))),
         # factor "ETHNICITY"
         eth_l = factor(as.character(eth), levels = c("1","2","3","4","5","6"), 
                         labels = c("Asian",
                                    "Non-Hispanic White",
                                    "Hispanic White",
                                    "Non-Hispanic Black",
                                    "Hispanic Black",
                                    "Missing")),
         
         
         # "IMMIGRANT"
         mig = ifelse(anc_bic == 2, 1, 0),
         mig_l = factor(as.character(mig), levels = c("0","1"), 
                        labels = c("Born in USA",
                                   "Migration background")),
         
         
         # "FEMALE"
         female = ifelse(anc_gnd == 2, 1, 0),
         female_l = factor(as.character(female), levels = c("0", "1"),
                           labels = c("Male",
                                      "Female")),
         
         # EMPLYOMENT
         lfs = ifelse(anc_emp >= 4, 4, anc_emp),
         lfs_l = factor(as.character(lfs), levels = c("1", "2", "3", "4"),
                        labels = c("Full-time employed",
                                   "Part-time employed",
                                   "Self-employed",
                                   "Not in paid work")),
         
         # EDUCATION
         edu_l = factor(as.character(anc_eduall), levels = c("1", "2", "3"),
                        labels = c("Low", "Medium", "High")))
  
# Add included relationship indicators
# 1. Subset indicators to one df
help <- subset(lca.df[, c("anc_id", "kin_typ",
                             "adv_lca", "clo_lca", "cmf_lca", "cnf_lca", "cnt_lca", "mon_lca")])

# 2. Recode relationship indicators to 0 and 1
rel_ind <- help %>%
  mutate(adv_lca = adv_lca - 1,
         cmf_lca = cmf_lca - 1,
         mon_lca = mon_lca - 1,
         cnt_lca = cnt_lca - 1,
         cnf_lca = cnf_lca - 1,
         clo_lca = clo_lca - 1)

# 3. Merge to USA_lc data frame by anchor id and kin id
stata <- merge(stata, rel_ind, by = c("anc_id", "kin_typ"), all.x = TRUE)




# Some tabulations
table(USA_lc$class)
table(USA_lc$race.l, useNA = "always") # generated
table(USA_lc$eth.l, useNA = "always") # generated
table(USA_lc$anc_eth, useNA = "always") # original
table(USA_lc$anc_his2, useNA = "always") # original
table(USA_lc$anc_his1, useNA = "always") # original

table(USA_lc$anc_his2, USA_lc$anc_his1, useNA = "always")
table(USA_lc$eth.l, USA_lc$anc_eth, useNA = "always")

table(USA_lc$race.l, USA_lc$mig.l, useNA = "always")
round(prop.table(table(USA_lc$eth.l, USA_lc$mig.l, useNA = "always"), 2), 2)
table(USA_lc$kin_cat_small, useNA = "always")



library(haven)

# first row of dataset contains transposed matrix -> are being "retransposed" into matrix for SEM in stata, therefore, MUST be in first line!
stata$lq <- c(as.vector(t(lQ[,-4])),rep(0,(n-m)))

# drop variable mc.factor
write_dta(stata, paste0(folder.data, "USA_lc.dta"), 
          label = NULL)
