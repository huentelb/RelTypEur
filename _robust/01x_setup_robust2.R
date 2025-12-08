## A typology of nuclear and extended family relations in the United States
## Bettina Hünteler
## 26.04.2024
## huenteler@wiso.uni-koeln.de

#### 01 SET UP AND DATA CLEANING ####

## Set (up) working directories
setwd("/Users/Bettina/sciebo/projects/Kinmatrix/analyses/")
folder <- getwd()

folder.code <- paste0(folder,"/code/")
folder.graph <- paste0(folder,"/graphs/241114/robust_all_FIML/")
ifelse(!dir.exists(folder.graph), dir.create(folder.graph), "Folder already exists")
folder.graph.hi <- folder.graph # store as higher level graph folder, too (for later)
folder.data <- paste0(folder,"/data/")
ifelse(!dir.exists(folder.data), dir.create(folder.data), "Folder already exists")


## Install/load required packages
# install.packages(c("foreign", "RColorBrewer", "dplyr", "ggplot2", "flextable", "reshape2", "patchwork", "gcookbook", "ggstatsplot"))
library(foreign)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(flextable)
library(tidyr)
library(reshape2)
library(patchwork)
library(gcookbook)
library(ggstatsplot)

# install.packages("poLCA")
library(poLCA)
# install.packages("see")
library(see)

# Load data
# factors <- read.dta("/Users/Bettina/Documents/datasets/KINMATRIX/kinmatrix-prepared_v03_s12.dta", convert.factors = TRUE)
# save(factors, file = "/Users/Bettina/Documents/datasets/KINMATRIX/factors_v03.RData")
# original <- read.dta("/Users/Bettina/Documents/datasets/KINMATRIX/kinmatrix-prepared_v03_s12.dta", convert.factors = FALSE)
# save(original, file = "/Users/Bettina/Documents/datasets/KINMATRIX/kinmatrix-prepared_v03_s12.RData")

load("/Users/Bettina/Documents/datasets/KINMATRIX/kinmatrix-prepared_v03_s12.RData")
load("/Users/Bettina/Documents/datasets/KINMATRIX/factors_v03.RData")

# add suffix to vars in factors dataframe
suffix <- paste0(names(factors), ".l")
names(factors) <- suffix

# bind KINMATRIX with and without labels (factors) together
orlab <- bind_cols(original, factors)





## 01a SAMPLE SELECITON ####

# Filter to USA sample and to kin that is alive
USA_base <- orlab %>% 
  filter(anc_cou == 10, # USA sample
         # kin that is alive
         kin_ls == 1, 
         # biological kin only (no step-siblings or parents' partners)
         kin_cat != 12 & kin_cat != 20 & kin_cat != 10 & kin_cat != 18)


# count number of individuals in sample  4,980      
n_ind <- n_distinct(USA_base$anc_id)

# count number of observations in sample
size <- nrow(USA_base)





## 01b CASE-WISE DELETION DUE TO MISSINGS ####

# Drop if missing in
# GENDER (n = 46)         anc_gnd == 3: other or no gender (too few cases 887 obs)
USA <- USA_base %>% 
  filter(anc_gnd >= 0 & anc_gnd != 3) 
# display how many dropped
d_gnd     <- size - nrow(USA) 
d_gnd_ind <- n_ind - n_distinct(USA$anc_id)

print(paste0("Gender: ", d_gnd, " observations dropped, belonging to ", d_gnd_ind, " individuals"))

# updated sample size
n_ind <- n_distinct(USA$anc_id)
size <- nrow(USA)

# RACE (n = 7,143)        anc_eth
USA <- USA %>% 
  mutate(anc_eth_old = anc_eth,
         anc_eth_old.l = anc_eth.l,
         anc_eth = anc_eth_new,
         anc_eth.l = anc_eth_new.l) %>% 
  filter(anc_eth >= 0)
# display how many dropped
d_eth     <- size - nrow(USA)
d_eth_ind <- n_ind - n_distinct(USA$anc_id)

print(paste0("Race: ", d_eth, " observations dropped, belonging to ", d_eth_ind, " individuals"))

# updated sample size
n_ind <- n_distinct(USA$anc_id)
size <- nrow(USA)


# ETHNICITY (HISPANIC) (n = 4,981)        anc_his2
USA <- USA %>% 
  filter(anc_his2 >= 0)
# display how many dropped
d_his     <- size - nrow(USA)
d_his_ind <- n_ind - n_distinct(USA$anc_id)

print(paste0("Hispanic: ", d_his, " observations dropped, belonging to ", d_his_ind, " individuals"))

# updated sample size
n_ind <- n_distinct(USA$anc_id)
size <- nrow(USA)


# RACE-ETHNICITY is NON-HISPANIC OTHER (n = 2,354)        anc_eth
USA <- USA %>% 
  filter(anc_eth < 4 | anc_his2 == 2)
# display how many dropped
d_eth     <- size - nrow(USA)
d_eth_ind <- n_ind - n_distinct(USA$anc_id)

print(paste0("Race (Other): ", d_eth, " observations dropped, belonging to ", d_eth_ind, " individuals"))


# Remaining sample
n_ind <- n_distinct(USA$anc_id)
size <- nrow(USA)
print(paste0(size, " observations remaining, belonging to ", n_ind, " individuals"))


## 01d MISSING IMPUTATION ####

# Impute missing values in covariates 

# AGE -> mean (anc_age)
print(paste0(sum(USA$anc_age < 0), " observations missing in AGE"))

# HEALTH -> mean (anc_hea)
print(paste0(sum(USA$anc_hea < 0), " observations missing in HEALTH"))
USA$anc_hea[USA$anc_hea == -1] <- mean(USA$anc_hea) 
print(paste0(sum(USA$anc_hea < 0), " observations missing in HEALTH after imputation"))

# EDUCATION -> medium (anc_eduus -> US spec; anc_eduall -> harmonized to low, medium, high)
print(paste0(sum(USA$anc_eduall < 0), " observations missing in EDUCATION"))
table(USA$anc_eduall.l, USA$anc_eduall)
USA$anc_eduall.l[USA$anc_eduall.l == "-2. Prefer not to answer"] <- "2. Mid" 
USA$anc_eduall[USA$anc_eduall == -2] <- 2 
print(paste0(sum(USA$anc_eduall < 0), " observations missing in EDUCATION after imputation"))
table(USA$anc_eduall.l, USA$anc_eduall)

# EMPLOYMENT STATUS -> mode (anc_emp)
print(paste0(sum(USA$anc_emp < 0), " observations missing in EMPLOYMENT"))
table(USA$anc_emp.l)

# (Has children (n = 0)    anc_chin
# Age of youngest child?  anc_byc#)





## 01e REMAINING MISSING CODES ####

# Indicate missing values as NA
USA[USA == -1] <- NA # Don't know
USA[USA == -2] <- NA # Prefer not to answer / no answer
USA[USA == -3] <- NA # Does not apply (for filtered questions and if asked in W2 but not participated)
USA[USA == -4] <- NA # ?
USA[USA == -5] <- NA # Incomplete data

USA$rel_adv1.l[USA$rel_adv1.l == "-5. Incomplete data"] <- NA
USA$rel_clo.l[USA$rel_clo.l == "-5. Incomplete data"]   <- NA
USA$rel_cmf1.l[USA$rel_cmf1.l == "-5. Incomplete data"] <- NA
USA$rel_cnf.l[USA$rel_cnf.l == "-5. Incomplete data"]   <- NA
USA$rel_cnt.l[USA$rel_cnt.l == "-5. Incomplete data"]   <- NA
USA$rel_mon1.l[USA$rel_mon1.l == "-5. Incomplete data"] <- NA 
USA$rel_cou1.l[USA$rel_cou1.l == "-5. Incomplete data"] <- NA 
USA$rel_tra.l[USA$rel_tra.l == "-5. Incomplete data"]   <- NA
USA$rel_tra.l[USA$rel_tra.l == "-1. Don't know"]        <- NA


## 01f REODER AND GROUP KIN CATEGORIES ####
# Re-order kin by kin type (nuclear vs extended instead of paternal vs maternal lineage)
USA$kin_cat.l <- factor(USA$kin_cat.l, levels =  c("1. Father",
                                                   "2. Mother",
                                                   "3. Brother",
                                                   "4. Sister",
                                                   "5. Paternal grandfather",
                                                   "13. Maternal grandfather",
                                                   "6. Paternal grandmother",
                                                   "14. Maternal grandmother",
                                                   "11. Paternal halfsibling",
                                                   "19. Maternal halfsibling",
                                                   "7. Paternal uncle",
                                                   "15. Maternal uncle",
                                                   "8. Paternal aunt",
                                                   "16. Maternal aunt",
                                                   "9. Paternal cousin",
                                                   "17. Maternal cousin"))
# Remove numbers from levels
levels(USA$kin_cat.l) <- c("Father",
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
                           "Maternal cousin")

# Group same type of kin together 
USA <- USA %>% 
  mutate(
    # Ignore matrilineal or patrilineal lineage
    kin_cat_med = ifelse(kin_cat.l == "Father", "Father",
                              ifelse(kin_cat.l == "Mother", "Mother",
                                     ifelse(kin_cat.l == "Brother", "Brother",
                                            ifelse(kin_cat.l == "Sister", "Sister",
                                                   ifelse(kin_cat.l == "Paternal grandfather" | kin_cat.l == "Maternal grandfather", "Grandfather",
                                                          ifelse(kin_cat.l == "Paternal grandmother" | kin_cat.l == "Maternal grandmother", "Grandmother",
                                                                 ifelse(kin_cat.l == "Paternal halfsibling" | kin_cat.l == "Maternal halfsibling", "Halfsibling",
                                                                        ifelse(kin_cat.l == "Paternal uncle" | kin_cat.l == "Maternal uncle", "Uncle",
                                                                               ifelse(kin_cat.l == "Paternal aunt" | kin_cat.l == "Maternal aunt", "Aunt", 
                                                                                      ifelse(kin_cat.l == "Paternal cousin" | kin_cat.l == "Maternal cousin", "Cousin", 
                                                                                             kin_cat.l)))))))))),
    # order levels
    kin_cat_med = factor(kin_cat_med, levels = c("Father", "Mother", "Brother", "Sister", "Grandfather", "Grandmother", "Halfsibling", "Uncle", "Aunt", "Cousin")),
         
    # Ignore gender of kin
    kin_cat_small = ifelse(kin_cat_med == "Father" | kin_cat_med == "Mother", "Parents",
                          ifelse(kin_cat_med == "Brother" | kin_cat_med == "Sister", "Siblings",
                                 ifelse(kin_cat_med == "Grandfather" | kin_cat_med == "Grandmother", "Grandparents",
                                        ifelse(kin_cat_med == "Uncle" | kin_cat_med == "Aunt", "Uncles and Aunts",
                                               ifelse(kin_cat_med == "Halfsibling", "Halfsiblings",
                                               "Cousins"))))),
    
    # order levels
    kin_cat_small = factor(kin_cat_small, levels = c("Parents", "Siblings", "Grandparents", "Halfsiblings", "Uncles and Aunts", "Cousins")))


# LAST LINE OF CODE #
