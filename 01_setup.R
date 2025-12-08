## A typology of nuclear and extended family relations in Europe
## Bettina Hünteler
## 17.11.2025
## bhuenteler@diw.de

#### 01 SET UP AND DATA CLEANING ####

## Set (up) working directories
setwd("/Users/bhuenteler/Library/CloudStorage/OneDrive-DIWBerlin/projects/Kinmatrix/RelTypEur/analyses")
folder <- getwd()

folder.code <- paste0(folder,"/code/")
folder.graph <- paste0(folder,"/graphs/251104/")
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
library(poLCA)
library(see)

# Load data 
# factors <- read.dta("/Users/Bettina/Documents/datasets/KINMATRIX/kinmatrix-prepared_v03_s12.dta", convert.factors = TRUE)
# save(factors, file = "/Users/Bettina/Documents/datasets/KINMATRIX/factors_v03.RData")
# original <- read.dta("/Users/Bettina/Documents/datasets/KINMATRIX/kinmatrix-prepared_v03_s12.dta", convert.factors = FALSE)
# save(original, file = "/Users/Bettina/Documents/datasets/KINMATRIX/kinmatrix-prepared_v03_s12.RData")

load("/Users/bhuenteler/Documents/datasets/KINMATRIX/kinmatrix-prepared_v03_s12.RData")
load("/Users/bhuenteler/Documents/datasets/KINMATRIX/factors_v03.RData")

# add suffix to vars in factors dataframe
suffix <- paste0(names(factors), ".l")
names(factors) <- suffix

# bind KINMATRIX with and without labels (factors) together
orlab <- bind_cols(original, factors)





## 01a SAMPLE SELECITON ####

# Filter to EU sample and to kin that is alive
EU_base <- orlab %>% 
  filter(anc_cou != 10, # EU sample
         # kin that is alive
         kin_ls == 1, 
         # biological kin only (no step-siblings or parents' partners)
         kin_cat != 12 & kin_cat != 20 & kin_cat != 10 & kin_cat != 18)


# count number of individuals in sample  4,980      
n_ind <- n_distinct(EU_base$anc_id)

# count number of observations in sample
size <- nrow(EU_base)

print(paste0("N = ", n_ind, " individuals in sample, with a total of n = ", size, " ego-kin dyads."))




## 01b CASE-WISE DELETION DUE TO MISSINGS ####

# Drop if missing in
# GENDER (n = 46)         anc_gnd == 3: other or no gender (too few cases 597 obs)
EU <- EU_base %>% 
  filter(anc_gnd >= 0 & anc_gnd != 3) 
# display how many dropped
d_gnd     <- size - nrow(EU) 
d_gnd_ind <- n_ind - n_distinct(EU$anc_id)

print(paste0("Gender: ", d_gnd, " observations dropped, belonging to ", d_gnd_ind, " individuals"))

    # updated sample size
    n_ind <- n_distinct(EU$anc_id)
    size <- nrow(EU)

print(paste0("N = ", n_ind, " individuals in sample, with a total of n = ", size, " ego-kin dyads."))
    
    
## 01c MISSING DATA IN REL INDICATORS ####
print(paste0(sum(EU$rel_adv1 < 0), " observations missing in ADVICE"))
print(paste0(sum(EU$rel_clo < 0), " observations missing in CLOSENESS"))
print(paste0(sum(EU$rel_cmf1 < 0), " observations missing in COMFORTED"))
print(paste0(sum(EU$rel_cnf < 0), " observations missing in CONFLICT"))
print(paste0(sum(EU$rel_cnt < 0), " observations missing in CONTACT"))
print(paste0(sum(EU$rel_mon1 < 0), " observations missing in MONEY"))
print(paste0(sum(EU$rel_tra < 0), " observations missing in DISTANCE"))
print(paste0(sum(EU$rel_cou1 < 0), " observations missing in COUNTED ON"))



# By country
folder.graph <- paste0(folder.graph.hi,"missings/")
ifelse(!dir.exists(folder.graph), dir.create(folder.graph), "Folder already exists")

w = 10
h = 6.8

pdf(file = paste0(folder.graph, paste0("per_adv_cou.pdf")), 
    width = w, height = h)
EU %>% 
  ggplot(aes(rel_adv1)) +
  geom_bar(aes(y = after_stat(prop))) +
  labs(x = "", 
       y = "Proportion",
       title = "Advice received from") +
  facet_wrap(~anc_cou.l) +
  scale_x_continuous(breaks = c(-5:2)) +
  theme(legend.position = "none")
dev.off()

pdf(file = paste0(folder.graph, paste0("per_clo_cou.pdf")), 
    width = w, height = h)
EU %>% 
  ggplot(aes(rel_clo)) +
  geom_bar(aes(y = after_stat(prop))) +
  labs(x = "", 
       y = "Proportion",
       title = "Emotional Closeness") +
  facet_wrap(~anc_cou.l) +
  scale_x_continuous(breaks = c(-5:5)) +
  theme(legend.position = "none")
dev.off()


pdf(file = paste0(folder.graph, paste0("per_cmf_cou.pdf")), 
    width = w, height = h)
EU %>% 
  ggplot(aes(rel_cmf1)) +
  geom_bar(aes(y = after_stat(prop))) +
  labs(x = "", 
       y = "Proportion",
       title = "Comfort received from") +
  facet_wrap(~anc_cou.l) +
  scale_x_continuous(breaks = c(-5:2)) +
  theme(legend.position = "none")
dev.off()

pdf(file = paste0(folder.graph, paste0("per_adv_cou.pdf")), 
    width = w, height = h)


pdf(file = paste0(folder.graph, paste0("per_cnt_cou.pdf")), 
    width = w, height = h)
EU %>% 
  ggplot(aes(rel_cnt)) +
  geom_bar(aes(y = after_stat(prop))) +
  labs(x = "", 
       y = "Proportion",
       title = "Contact Frequency") +
  facet_wrap(~anc_cou.l) +
  scale_x_continuous(breaks = c(-5:6)) +
  theme(legend.position = "none")
dev.off()


pdf(file = paste0(folder.graph, paste0("per_cnf_cou.pdf")), 
    width = w, height = h)
EU %>% 
  ggplot(aes(rel_cnf)) +
  geom_bar(aes(y = after_stat(prop))) +
  labs(x = "", 
       y = "Proportion",
       title = "Conflict") +
  facet_wrap(~anc_cou.l) +
  scale_x_continuous(breaks = c(-5:4)) +
  theme(legend.position = "none")
dev.off()

pdf(file = paste0(folder.graph, paste0("per_cou_cou.pdf")), 
    width = w, height = h)
EU %>% 
  ggplot(aes(rel_cou1)) +
  geom_bar(aes(y = after_stat(prop))) +
  labs(x = "", 
       y = "Proportion",
       title = "Can count on...") +
  facet_wrap(~anc_cou.l) +
  scale_x_continuous(breaks = c(-5:2)) +
  theme(legend.position = "none")
dev.off()

pdf(file = paste0(folder.graph, paste0("per_mon_cou.pdf")), 
    width = w, height = h)
EU %>% 
  ggplot(aes(rel_mon1)) +
  geom_bar(aes(y = after_stat(prop))) +
  labs(x = "", 
       y = "Proportion",
       title = "Money received from") +
  facet_wrap(~anc_cou.l) +
  scale_x_continuous(breaks = c(-5:2)) +
  theme(legend.position = "none")
dev.off()

pdf(file = paste0(folder.graph, paste0("per_tra_cou.pdf")), 
    width = w, height = h)
EU %>% 
  ggplot(aes(rel_tra)) +
  geom_bar(aes(y = after_stat(prop))) +
  labs(x = "", 
       y = "Proportion",
       title = "Can count on...") +
  facet_wrap(~anc_cou.l) +
  scale_x_continuous(breaks = c(-5:5)) +
  theme(legend.position = "none")
dev.off()




# Remaining sample
n_ind <- n_distinct(EU$anc_id)
size <- nrow(EU)
print(paste0(size, " observations remaining, belonging to ", n_ind, " individuals"))







## 01d MISSING IMPUTATION ####


# Impute missing values in covariates 

# AGE -> mean (anc_age)
print(paste0(sum(EU$anc_age < 0), " observations missing in AGE"))

# HEALTH -> mean (anc_hea)
print(paste0(sum(EU$anc_hea < 0), " observations missing in HEALTH"))
EU$anc_hea[EU$anc_hea == -1] <- mean(EU$anc_hea) 
print(paste0(sum(EU$anc_hea < 0), " observations missing in HEALTH after imputation"))

# EDUCATION -> medium (anc_eduus -> US spec; anc_eduall -> harmonized to low, medium, high)
print(paste0(sum(EU$anc_eduall < 0), " observations missing in EDUCATION"))
table(EU$anc_eduall.l, EU$anc_eduall)
EU$anc_eduall.l[EU$anc_eduall.l == "-2. Prefer not to answer"] <- "2. Mid" 
EU$anc_eduall[EU$anc_eduall == -2] <- 2 
print(paste0(sum(EU$anc_eduall < 0), " observations missing in EDUCATION after imputation"))
table(EU$anc_eduall.l, EU$anc_eduall)

# EMPLOYMENT STATUS -> mode (anc_emp)
print(paste0(sum(EU$anc_emp < 0), " observations missing in EMPLOYMENT"))
table(EU$anc_emp.l)






## 01e REMAINING MISSING CODES ####

# Indicate missing values as NA
EU[EU == -1] <- NA # Don't know
EU[EU == -2] <- NA # Prefer not to answer / no answer
EU[EU == -3] <- NA # Does not apply (for filtered questions and if asked in W2 but not participated)
EU[EU == -4] <- NA # ?
EU[EU == -5] <- NA # Incomplete data


## 01f REODER AND GROUP KIN CATEGORIES ####
# Re-order kin by kin type (nuclear vs extended instead of paternal vs maternal lineage)
EU$kin_cat.l <- factor(EU$kin_cat.l, levels =  c("1. Father",
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
levels(EU$kin_cat.l) <- c("Father",
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
EU <- EU %>% 
  mutate(
    # Ignore matrilineal or patrilineal LINEAGE
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
         
    # Ignore GENDER of kin
    kin_cat_small = ifelse(kin_cat_med == "Father" | kin_cat_med == "Mother", "Parents",
                          ifelse(kin_cat_med == "Brother" | kin_cat_med == "Sister", "Siblings",
                                 ifelse(kin_cat_med == "Grandfather" | kin_cat_med == "Grandmother", "Grandparents",
                                        ifelse(kin_cat_med == "Uncle" | kin_cat_med == "Aunt", "Uncles and Aunts",
                                               ifelse(kin_cat_med == "Halfsibling", "Halfsiblings",
                                               "Cousins"))))),
    
    # order levels
    kin_cat_small = factor(kin_cat_small, levels = c("Parents", "Siblings", "Grandparents", "Halfsiblings", "Uncles and Aunts", "Cousins")))


# LAST LINE OF CODE #
