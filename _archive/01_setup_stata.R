## A typology of nuclear and extended family relations in the United States
## Bettina Hünteler
## 26_04_2024
## huenteler@wiso_uni-koeln_de

#### 01 SET UP AND DATA CLEANING ####

## Set (up) working directories
setwd("/Users/Bettina/sciebo/projects/Kinmatrix/analyses/")
folder <- getwd()

folder.code <- paste0(folder,"/code/")
folder.graph <- paste0(folder,"/graphs/240813/")
ifelse(!dir.exists(folder.graph), dir.create(folder.graph), "Folder already exists")
folder.graph_hi <- folder.graph # store as higher level graph folder, too (for later)
folder.data <- paste0(folder,"/data/")
ifelse(!dir.exists(folder.data), dir.create(folder.data), "Folder already exists")


## Install/load required packages
# install_packages(c("foreign", "RColorBrewer", "dplyr", "ggplot2", "flextable", "reshape2", "patchwork", "gcookbook", "ggstatsplot"))
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

# install_packages("poLCA")
library("poLCA")

# Load data
# factors <- read_dta("/Users/Bettina/Documents/datasets/KINMATRIX/prepared_v02_s12_dta", convert_factors = TRUE)
# save(factors, file = "/Users/Bettina/Documents/datasets/KINMATRIX/factors_RData")
# original <- read_dta("/Users/Bettina/Documents/datasets/KINMATRIX/prepared_v02_s12_dta", convert_factors = FALSE)
# save(original, file = "/Users/Bettina/Documents/datasets/KINMATRIX/prepared_v02_RData")

load("/Users/Bettina/Documents/datasets/KINMATRIX/prepared_v02.RData")
load("/Users/Bettina/Documents/datasets/KINMATRIX/factors.RData")

# add suffix to vars in factors dataframe
suffix <- paste0(names(factors), "_l")
names(factors) <- suffix

# bind KINMATRIX with and without labels (factors) together
orlab <- bind_cols(original, factors)





## 01a SAMPLE SELECITON ####

# Filter to stata sample and to kin that is alive
USA_base <- orlab %>% 
  filter(anc_cou == 10, # stata sample
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
# Gender (n = 46)         anc_gnd == 3: other or no gender (too few cases 887 obs)
stata <- USA_base %>% 
  filter(anc_gnd >= 0 & anc_gnd != 3) 
# display how many dropped
d_gnd     <- size - nrow(stata) 
d_gnd_ind <- n_ind - n_distinct(stata$anc_id)

print(paste0("Gender: ", d_gnd, " observations dropped, belonging to ", d_gnd_ind, " individuals"))

    # updated sample size
    n_ind <- n_distinct(stata$anc_id)
    size <- nrow(stata)

# Race (n = 4,981)        anc_eth
stata <- stata %>% 
  filter(anc_eth >= 0)
# display how many dropped
d_eth     <- size - nrow(stata)
d_eth_ind <- n_ind - n_distinct(stata$anc_id)

print(paste0("Race: ", d_eth, " observations dropped, belonging to ", d_eth_ind, " individuals"))

    # updated sample size
    n_ind <- n_distinct(stata$anc_id)
    size <- nrow(stata)


# Religion (yes/no)       anc_rel1 -> W2 only
stata <- stata %>% 
  filter(anc_rel1 >= 0 | anc_rel1 == -3)
# display how many dropped
d_rel1      <- size - nrow(stata)
d_rel1_ind  <- n_ind - n_distinct(stata$anc_id)

print(paste0("Relgion: ", d_rel1, " observations dropped, belonging to ", d_rel1_ind, " individuals"))

    # updated sample size
    n_ind <- n_distinct(stata$anc_id)
    size <- nrow(stata)

# Religiousness (freq)    anc_rel3 -> W2 only
stata <- stata %>% 
  filter(anc_rel3 >= 0 | anc_rel3 == -3)
# display how many dropped
d_rel3      <- size - nrow(stata)
d_rel3_ind  <- n_ind - n_distinct(stata$anc_id)

print(paste0("Religiousness: ", d_rel3, " observations dropped, belonging to ", d_rel3_ind, " individuals"))

    # updated sample size
    n_ind <- n_distinct(stata$anc_id)
    size <- nrow(stata)


# Ethnicity (anc_his2) -> do not drop all but make sub-analysis with those for whom it applies (wave 1 resp_ only)

    
    
    

## 01c CASE-WISE DELETION DUE TO TOO FEW REL INDICATORS ANSWERED ####

help <- stata %>% 
  mutate(rel_mis = rowSums(stata[, c("rel_adv1", "rel_clo", "rel_cmf1",
                                         "rel_cnf", "rel_cnt", "rel_mon1")] < 0))

# Keep if maximum 2 missings
stata <- help %>% 
  filter(rel_mis <= 2)
# display how many dropped
d_rel_mis      <- size - nrow(stata)
d_rel_mis_ind  <- n_ind - n_distinct(stata$anc_id)

print(paste0("Relationship Indicators: ", d_rel_mis, " observations dropped, belonging to ", d_rel_mis_ind, " individuals"))

# Remaining sample
n_ind <- n_distinct(stata$anc_id)
size <- nrow(stata)
print(paste0(size, " observations remaining, belonging to ", n_ind, " individuals"))





## 01d MISSING IMPUTATION ####

# Impute missing values in covariates 

# AGE -> mean (anc_age)
print(paste0(sum(stata$anc_age < 0), " observations missing in AGE"))

# HEALTH -> mean (anc_hea)
print(paste0(sum(stata$anc_hea < 0), " observations missing in HEALTH"))
stata$anc_hea[stata$anc_hea == -1] <- mean(stata$anc_hea) 
print(paste0(sum(stata$anc_hea < 0), " observations missing in HEALTH after imputation"))

# EDUCATION -> medium (anc_eduus -> US spec; anc_eduall -> harmonized to low, medium, high)
print(paste0(sum(stata$anc_eduall < 0), " observations missing in EDUCATION"))
table(stata$anc_eduall_l, stata$anc_eduall)
stata$anc_eduall_l[stata$anc_eduall_l == "-2_ Prefer not to answer"] <- "2_ Mid" 
stata$anc_eduall[stata$anc_eduall == -2] <- 2 
print(paste0(sum(stata$anc_eduall < 0), " observations missing in EDUCATION after imputation"))
table(stata$anc_eduall_l, stata$anc_eduall)

# EMPLOYMENT STATUS -> mode (anc_emp)
print(paste0(sum(stata$anc_emp < 0), " observations missing in EMPLOYMENT"))
table(stata$anc_emp_l)

# (Has children (n = 0)    anc_chin
# Age of youngest child?  anc_byc#)





## 01e REMAINING MISSING CODES ####

# Indicate missing values as NA
stata[stata == -1] <- NA # Don't know
stata[stata == -2] <- NA # Prefer not to answer / no answer
stata[stata == -3] <- NA # Does not apply (for filtered questions and if asked in W2 but not participated)
stata[stata == -4] <- NA # ?
stata[stata == -5] <- NA # Incomplete data


## 01f REODER AND GROUP KIN CATEGORIES ####
# Re-order kin by kin type (nuclear vs extended instead of paternal vs maternal lineage)
stata$kin_cat_l <- factor(stata$kin_cat_l, levels =  c("1. Father",
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
levels(stata$kin_cat_l) <- c("Father",
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
stata <- stata %>% 
  mutate(
    # Ignore matrilineal or patrilineal lineage
    kin_cat_med = ifelse(kin_cat_l == "Father", "Father",
                              ifelse(kin_cat_l == "Mother", "Mother",
                                     ifelse(kin_cat_l == "Brother", "Brother",
                                            ifelse(kin_cat_l == "Sister", "Sister",
                                                   ifelse(kin_cat_l == "Paternal grandfather" | kin_cat_l == "Maternal grandfather", "Grandfather",
                                                          ifelse(kin_cat_l == "Paternal grandmother" | kin_cat_l == "Maternal grandmother", "Grandmother",
                                                                 ifelse(kin_cat_l == "Paternal halfsibling" | kin_cat_l == "Maternal halfsibling", "Halfsibling",
                                                                        ifelse(kin_cat_l == "Paternal uncle" | kin_cat_l == "Maternal uncle", "Uncle",
                                                                               ifelse(kin_cat_l == "Paternal aunt" | kin_cat_l == "Maternal aunt", "Aunt", 
                                                                                      ifelse(kin_cat_l == "Paternal cousin" | kin_cat_l == "Maternal cousin", "Cousin", 
                                                                                             kin_cat_l)))))))))),
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
