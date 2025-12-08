## A typology of nuclear and extended family relations in the United States
## Bettina Hünteler
## 07.05.2024
## huenteler@wiso.uni-koeln.de


#### 03 LPA PREPARATIONS ####

## 03a KIN SELECTION FOR LPA ####

## 03b PREPARE VARS FOR LPA ####

# recode binary variables to 0 no - 1 yes
# recode cont variables to higher values = more of x
lpa.df <- kin %>% 
  mutate(mon = ifelse(rel_mon1 == 2, 1, 0),
         adv = ifelse(rel_adv1 == 2, 1, 0),
         cmf = ifelse(rel_cmf1 == 2, 1, 0),
         cnt = 7 - rel_cnt)



## 04a RUN LPA ####
# install.packages("tidyLPA")
library(tidyLPA)
# Use only package = 'mclust' when calling estimate_profiles()

# with standardized vars
lpa1 <- lpa.df %>% 
  dplyr::select(adv, rel_clo, cmf, rel_cnf, cnt, mon) %>% 
  single_imputation() %>% # impute missing values using mix pckg
  scale() %>% # z-standardize vars
  estimate_profiles(1:7)


## 04b DIFFERENT MODEL SPEC ####

# Model 1: Equal variances,   covariances = 0
# Model 2: Unequal variances, covariances = 0
# Model 3: Equal variabnes,   equal covariances
# Models 4-5 only in Mplus
# Model 6: Unequal variances, unequal covariances

lpa2 <- lpa.df[1:500,] %>%
  dplyr::select(rel_cnf, rel_cnt, rel_clo, mon, adv, cmf) %>%
  single_imputation() %>% # impute missing values using mix pckg
  scale() %>% # z-standardize vars
  estimate_profiles(1:7, models = 2)
warnings()
lpa2

lpa3 <- lpa.df[1:500,] %>%
  dplyr::select(rel_cnf, rel_cnt, rel_clo, mon, adv, cmf) %>%
  single_imputation() %>% # impute missing values using mix pckg
  scale() %>% # z-standardize vars
  estimate_profiles(1:7, models = 3)
warnings()
lpa3

lpa6 <- lpa.df[1:500,] %>%
  dplyr::select(rel_cnf, rel_cnt, rel_clo, mon, adv, cmf) %>%
  single_imputation() %>% # impute missing values using mix pckg
  scale() %>% # z-standardize vars
  estimate_profiles(1:7, models = 6)
warnings()
lpa6

## 05a COMPARE CLUSTER SOLUTIONS ####

lpa1
compare_solutions(lpa1, statistics = c("AIC", "BIC"))

# more fit indices
get_fit(lpa1)

set_flextable_defaults(font.family = "Times New Roman", 
                       text.align = "center", part = "body")
ftab <- flextable(get_fit(lpa1)) 
ftab <- set_caption(ftab, caption = paste0("Classification Criteria Over Models (", kin.L,")"))
save_as_docx(ftab, path = paste0(folder.graph.hi,"critLPA_",kin.l,".docx"))

# best model?
best <- 5

### 05b PROFILE PLOTS ####

# 1. Bars reflecting a confidence interval for the class centroids
# 2. Boxes reflecting the standard deviations within each class; 
# a box encompasses +/- 64% of the observations in a normal distribution

# Set corresponding dir for graphs
folder.graph <- paste0(folder.graph.hi, "lp", best, "_output/")
ifelse(!dir.exists(folder.graph), dir.create(folder.graph), "Folder already exists")

plot_profiles(lpa1, rawdata = FALSE)

png(file = paste0(folder.graph, paste0("profplot_",kin.l,"_", best, ".png")), 
    width = w, height = h)
plot_profiles(lpa1[[best]], rawdata = FALSE, add_line = TRUE, sd = TRUE, ci = FALSE) 
dev.off()

### 05c DISTRIBUTIONS PLOTS ####

png(file = paste0(folder.graph, paste0("densplot_",kin.l,"_", best, ".png")), 
    width = w, height = h)
plot_density(lpa1[[best]])
dev.off()

plot_bivariate(lpa1[[best]])


### 05d ESTIMATES FOR VARS IN PROFILES (mean and var) ####
get_estimates(lpa1[[best]])


### 05e EXTRATCT CLASS MEMBERSHIP ####
get_data(lpa1) %>% 
  filter(classes_number == 4) %>% 
  arrange(id)

lpa_out <- get_data(lpa1) %>% 
  filter(classes_number == best) %>% 
  filter(Class_prob == 1) %>% # select most likely class
  transmute(id, factor(Class), rel_cnf, rel_cnt, rel_clo, mon, adv, cmf) %>% 
  arrange(id)

head(lpa_out, 10)
