## A typology of nuclear and extended family relations in the United States
## Bettina Hünteler
## 09.04.2025
## huenteler@wiso.uni-koeln.de

#### 10 ROBUSTNESS CHECK: EXCLUSION OF MISSING CASES ####


# Set corresponding dir for graphs
folder.graph <- paste0(folder.graph.hi, "robust_dropped/")
ifelse(!dir.exists(folder.graph), dir.create(folder.graph), "Folder already exists")


library(tidyverse) # for rownames_to_column
library(janitor)


##### 10a DATA PREP ####

# start after running 01_setup.R

# Add indicator if part of analytical sample (1) or not (0)
drop <- drop %>% 
  mutate(sample = 0,
         sample.l = as.factor("Dropped (missing)"),
         sample2 = 0,
         sample2.l = as.factor("Dropped (all)"))
keep <- keep %>% 
  mutate(sample = 1,
         sample.l = as.factor("Sample")) %>% 
  mutate(sample2 = ifelse(anc_eth < 4 | anc_his2 == 2, 1, 0),
         sample2.l = factor(ifelse(anc_eth < 4 | anc_his2 == 2, "Sample (final)", "Dropped (all)")))



# Bind dropped and sample members into one dataframe for comparison
sample <- drop %>% 
  # update race/ethnicity variable for the dropped
  mutate(anc_eth_old = anc_eth, 
         anc_eth_old.l = anc_eth.l,
         anc_eth = anc_eth_new,
         anc_eth.l = anc_eth_new.l) %>% 
  rbind(keep)


# Re-order kin by kin type (nuclear vs extended instead of paternal vs maternal lineage)
sample$kin_cat.l <- factor(sample$kin_cat.l, levels =  c("1. Father",
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
levels(sample$kin_cat.l) <- c("Father",
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
sample <- sample %>% 
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





# Further dataprep (sample on kin level)
sample_k <- sample %>% 
  dplyr::select(anc_id, kin_cat_med, kin_cat_small, kin_typ, kin_cat.l, anc_gnd, anc_gnd.l, anc_eth, anc_his2, anc_his1, anc_bic, anc_age,
                anc_eduall, anc_emp.l, anc_emp, dwe, kin_nam,
                sample, sample.l, sample2, sample2.l, anc_eth.l, anc_his2.l, 
                rel_cnf, rel_cnt, rel_tra, rel_clo, rel_mon1, rel_adv1, rel_cmf1, rel_cou1) %>% 
    
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
      
    # "GENDER"
    gnd.fem = ifelse(anc_gnd == 2, 1, 0),
    gnd.mal = ifelse(anc_gnd == 1, 1, 0),
    gnd.mis = ifelse(anc_gnd == -2 | anc_gnd == 3, 1, 0),
    
    # EDUCATION
    edu.l = factor(as.character(anc_eduall), levels = c("1", "2", "3"),
                   labels = c("Low", "Medium", "High")),
    edu.low = ifelse(anc_eduall == 1, 1, 0),
    edu.med = ifelse(anc_eduall == 2, 1, 0),
    edu.hi  = ifelse(anc_eduall == 3, 1, 0),
    
    
    # NUCLEAR vs EXTENDED
    nuclear = ifelse(kin_cat_med == "Father" |
                       kin_cat_med == "Mother" |
                       kin_cat_med == "Brother" |
                       kin_cat_med == "Sister", "Nuclear", "Extended"),
    
    # NUCLEAR vs SEMI vs EXTENDED
    nucl_ext = ifelse(nuclear == "Nuclear", "Nuclear", 
                      ifelse(kin_cat_med == "Grandfather" |
                               kin_cat_med == "Grandmother" |
                               kin_cat_med == "Halfsibling", "Extended-nuclear", "Extended")),
    
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
    kin16_mcou = ifelse(kin_cat.l == "Maternal cousin", 1, 0),
    
    # KIN CAT MED binary
    kin01_med = ifelse(kin_cat_med == "Father", 1, 0),
    kin02_med = ifelse(kin_cat_med == "Mother", 1, 0),
    kin03_med = ifelse(kin_cat_med == "Brother", 1, 0),
    kin04_med = ifelse(kin_cat_med == "Sister", 1, 0),
    kin05_med = ifelse(kin_cat_med == "Grandfather", 1, 0),
    kin06_med = ifelse(kin_cat_med == "Grandmother", 1, 0),
    kin07_med = ifelse(kin_cat_med == "Halfsibling", 1, 0),
    kin08_med = ifelse(kin_cat_med == "Uncle", 1, 0),
    kin09_med = ifelse(kin_cat_med == "Aunt", 1, 0),
    kin10_med = ifelse(kin_cat_med == "Cousin", 1, 0)) %>% 
    
    
    # Relationship indicators
    # continuous -> apply cut-off; missing -> below median cut-off (1)
    mutate(cnf_lca = ifelse(rel_cnf <= 1 | rel_cnf < 0, 1, 2),
           cnf_lca.m = ifelse(rel_cnf == -5, 1, 0),
           cnf_lca.no = ifelse(rel_cnf == 1, 1, 0),
           cnf_lca.yes = ifelse(rel_cnf >= 2, 1, 0)) %>% # 1. none at all - 4. a great deal (at least "a little" 2)
    mutate(cnt_lca = ifelse(rel_cnt >= 4 |  rel_cnt < 0, 1, 2),
           cnt_lca.m = ifelse(rel_cnt == -5, 1, 0),
           cnt_lca.no = ifelse(rel_cnt >= 4, 1, 0),
           cnt_lca.yes = ifelse(rel_cnt >= 1 & rel_cnt <= 3, 1, 0)) %>% # 1. daily - 6. never (at least "once per month" 3)
    mutate(tra_lca = ifelse(rel_tra >= 5 |  rel_tra < 0, 1, 2),
           tra_lca.m = ifelse(rel_tra < 0, 1, 0),
           tra_lca.no = ifelse(rel_tra == 5, 1, 0),
           tra_lca.yes = ifelse(rel_tra >= 1 & rel_tra <= 4, 1, 0)) %>% # 1. same building - 5. >=1 hrs (within 1 hour)
    mutate(clo_lca = ifelse(rel_clo <= 3 |  rel_clo < 0, 1, 2),
           clo_lca.m = ifelse(rel_clo == -5, 1, 0),
           clo_lca.no = ifelse(rel_clo >= 1 & rel_clo <= 3, 1, 0),
           clo_lca.yes = ifelse(rel_clo >= 4, 1, 0)) %>% # 1. not at all - 5. very close (at least "very close")
    # binary -> if missing, generate binary indicators; else keep values as are
    mutate(mon_lca = ifelse(rel_mon1 == 1 | rel_mon1 == -5, 1, 2),
           mon_lca.m = ifelse(rel_mon1 == -5, 1, 0),
           mon_lca.no  = ifelse(rel_mon1 == 1,  1, 0),
           mon_lca.yes = ifelse(rel_mon1 == 2,  1, 0),
           
           adv_lca = ifelse(is.na(rel_adv1), 1, rel_adv1),
           adv_lca.m = ifelse(rel_adv1 == -5, 1, 0),
           adv_lca.no = ifelse(rel_adv1 == 1, 1, 0),
           adv_lca.yes = ifelse(rel_adv1 == 2, 1, 0),
           
           cmf_lca = ifelse(is.na(rel_cmf1), 1, rel_cmf1),
           cmf_lca.m = ifelse(rel_cmf1 == -5, 1, 0),
           cmf_lca.no = ifelse(rel_cmf1 == 1, 1, 0),
           cmf_lca.yes = ifelse(rel_cmf1 == 2, 1, 0),
           
           cou_lca = ifelse(is.na(rel_cou1), 1, rel_cou1),
           cou_lca.m = ifelse(rel_cou1 == -5, 1, 0),
           cou_lca.no = ifelse(rel_cou1 == 1, 1, 0),
           cou_lca.yes = ifelse(rel_cou1 == 2, 1, 0)) %>% 
    # generate variable for support combo of comfort and advice (any given)
    mutate(rel_sup = ifelse(rel_adv1 == 1 & rel_cmf1 == 1 & rel_cou1 == 1, 1, 2),
           rel_sup.l = factor(as.character(rel_sup), levels = c("1","2"), 
                              labels = c("1. Did not support", "2. Supported")),
           # with imputing missings for LCA
           sup_lca = ifelse(    (adv_lca == 1) &
                                (cmf_lca == 1) & 
                                (cou_lca == 1), 1, 2),
           sup_lca.l = factor(as.character(sup_lca), levels = c("1","2"), 
                                labels = c("1. Did not support", "2. Supported")))


##### 10b CROSS-TABS ####
# Cross-tabulations dropped x characteristics; with missings as a 'valid' values

# 1. Gender, age, education, employment status
# 2. Kin counts per individual per kin type
# 3. Relationship indicators by kin group


library(flextable)
install.packages(c('crosstable', 'descr'))
library(crosstable)
library(gmodels)
library(descr)


# EGO LEVEL

# 1. Count the number of each kin for each ego
sample_e <- sample_k %>% 
  group_by(anc_id) %>%
  mutate(kin01_total = sum(kin01_f    == 1), # count total number of kin for each ego
         kin02_total = sum(kin02_m    == 1),
         kin03_total = sum(kin03_bro  == 1),
         kin04_total = sum(kin04_sis  == 1),
         kin05_total = sum(kin05_pgf  == 1),
         kin06_total = sum(kin06_mgf  == 1),
         kin07_total = sum(kin07_pgm  == 1),
         kin08_total = sum(kin08_mgm  == 1),
         kin09_total = sum(kin09_phs  == 1),
         kin10_total = sum(kin10_mhs  == 1),
         kin11_total = sum(kin11_pun  == 1),
         kin12_total = sum(kin12_mun  == 1),
         kin13_total = sum(kin13_pau  == 1),
         kin14_total = sum(kin14_mau  == 1),
         kin15_total = sum(kin15_pcou == 1),
         kin16_total = sum(kin16_mcou == 1),
         kin_total   = n()) %>% # count number of all kin
  slice_head(n = 1) %>% # keep only first row for each ego
  ungroup()


# Sample (missing) -> sample.l
drop_kin_e <- crosstable(sample_e, 
           c(anc_age, gnd.fem, gnd.mal, gnd.mis, edu.hi, edu.med, edu.low, 
             kin01_total,
             kin02_total,
             kin03_total,
             kin04_total,
             kin05_total,
             kin06_total,
             kin07_total,
             kin08_total,
             kin09_total,
             kin10_total,
             kin11_total,
             kin12_total,
             kin13_total,
             kin14_total,
             kin15_total,
             kin16_total,
             kin_total), 
           by = sample.l, 
           unique_numeric = 1, # number of non-missing different levels a variable to be treated as numeric
           showNA = "ifany", 
           #effect = TRUE, # (for t-test and Wald test -> as if I run a bivariate logit/reg in stata)
           test = TRUE,
           # percent_pattern="{p_col} ({n})",
           percent_pattern="{p_col}",
           num_digits = 2, 
           funs = mean) %>% 
  as_flextable(show_test_name = FALSE)

drop_kin_e
save_as_docx("Table 1" = drop_kin_e, path = paste0(folder.graph, "Ego_dropped.docx"), align = "left")

# Sample 2 (final) -> sample2.l
drop_kin_e <- crosstable(sample_e, 
                         c(anc_age, gnd.fem, gnd.mal, gnd.mis, edu.hi, edu.med, edu.low, 
                           kin01_total,
                           kin02_total,
                           kin03_total,
                           kin04_total,
                           kin05_total,
                           kin06_total,
                           kin07_total,
                           kin08_total,
                           kin09_total,
                           kin10_total,
                           kin11_total,
                           kin12_total,
                           kin13_total,
                           kin14_total,
                           kin15_total,
                           kin16_total,
                           kin_total), 
                         by = sample2.l, 
                         unique_numeric = 1, # number of non-missing different levels a variable to be treated as numeric
                         showNA = "ifany", 
                         #effect = TRUE, # (for t-test and Wald test -> as if I run a bivariate logit/reg in stata)
                         test = TRUE,
                         # percent_pattern="{p_col} ({n})",
                         percent_pattern="{p_col}",
                         num_digits = 2, 
                         funs = mean) %>% 
  as_flextable(show_test_name = FALSE)

drop_kin_e
save_as_docx("Table 1" = drop_kin_e, path = paste0(folder.graph, "Ego_dropped2.docx"), align = "left")


# KIN LEVEL
# Sample (missing) -> sample.l
drop_kin_k <- crosstable(sample_k, 
           c(tra_lca.m,
             tra_lca.no, 
             tra_lca.yes,
             cnt_lca.m,
             cnt_lca.no, 
             cnt_lca.yes,
             clo_lca.m,
             clo_lca.no, 
             clo_lca.yes,
             adv_lca.m,
             adv_lca.no, 
             adv_lca.yes,
             cmf_lca.m,
             cmf_lca.no, 
             cmf_lca.yes,
             cou_lca.m,
             cou_lca.no, 
             cou_lca.yes,
             mon_lca.m,
             mon_lca.no, 
             mon_lca.yes,
             cnf_lca.m,
             cnf_lca.no, 
             cnf_lca.yes), 
           by = sample.l, 
           unique_numeric = 1,
           showNA = "ifany", 
           # effect = TRUE, # (for t-test and Wald test -> as if I run a bivariate logit/reg in stata)
           test = TRUE,
           # percent_pattern="{p_col} ({n})",
           percent_pattern="{p_col}",
           num_digits = 2, 
           funs = mean) %>% 
  as_flextable(show_test_name = FALSE)

drop_kin_k
save_as_docx("Table 1" = drop_kin_k, path = paste0(folder.graph, "Rel_dropped.docx"), align = "left")



# Sample 2 (final) -> sample2.l
drop_kin_k <- crosstable(sample_k, 
                         c(tra_lca.m,
                           tra_lca.no, 
                           tra_lca.yes,
                           cnt_lca.m,
                           cnt_lca.no, 
                           cnt_lca.yes,
                           clo_lca.m,
                           clo_lca.no, 
                           clo_lca.yes,
                           adv_lca.m,
                           adv_lca.no, 
                           adv_lca.yes,
                           cmf_lca.m,
                           cmf_lca.no, 
                           cmf_lca.yes,
                           cou_lca.m,
                           cou_lca.no, 
                           cou_lca.yes,
                           mon_lca.m,
                           mon_lca.no, 
                           mon_lca.yes,
                           cnf_lca.m,
                           cnf_lca.no, 
                           cnf_lca.yes), 
                         by = sample2.l, 
                         unique_numeric = 1,
                         showNA = "ifany", 
                         # effect = TRUE, # (for t-test and Wald test -> as if I run a bivariate logit/reg in stata)
                         test = TRUE,
                         # percent_pattern="{p_col} ({n})",
                         percent_pattern="{p_col}",
                         num_digits = 2, 
                         funs = mean) %>% 
  as_flextable(show_test_name = FALSE)

drop_kin_k
save_as_docx("Table 1" = drop_kin_k, path = paste0(folder.graph, "Rel_dropped2.docx"), align = "left")




# COMPARISONS OF REL INDICATORS BY KIN TYPE

# Overall
crosstable(sample_k, 
           c(tra_lca.m,
             tra_lca.no, 
             tra_lca.yes,
             cnt_lca.m,
             cnt_lca.no, 
             cnt_lca.yes,
             clo_lca.m,
             clo_lca.no, 
             clo_lca.yes,
             adv_lca.m,
             adv_lca.no, 
             adv_lca.yes,
             cmf_lca.m,
             cmf_lca.no, 
             cmf_lca.yes,
             cou_lca.m,
             cou_lca.no, 
             cou_lca.yes,
             mon_lca.m,
             mon_lca.no, 
             mon_lca.yes,
             cnf_lca.m,
             cnf_lca.no, 
             cnf_lca.yes), 
           by = c(sample2.l, kin_cat_med), 
           unique_numeric = 1,
           showNA = "ifany", 
           percent_pattern="{p_col}",
           num_digits = 2, 
           funs = mean) %>% 
  as_flextable(show_test_name = FALSE)


drop_kin_k1  <- sample_k %>% 
  filter(kin01_med == 1) %>% 
  crosstable(c(tra_lca.m,
               tra_lca.no, 
               tra_lca.yes,
               cnt_lca.m,
               cnt_lca.no, 
               cnt_lca.yes,
               clo_lca.m,
               clo_lca.no, 
               clo_lca.yes,
               adv_lca.m,
               adv_lca.no, 
               adv_lca.yes,
               cmf_lca.m,
               cmf_lca.no, 
               cmf_lca.yes,
               cou_lca.m,
               cou_lca.no, 
               cou_lca.yes,
               mon_lca.m,
               mon_lca.no, 
               mon_lca.yes,
               cnf_lca.m,
               cnf_lca.no, 
               cnf_lca.yes), 
                         by = sample.l,
                         unique_numeric = 1,
                         showNA = "ifany", 
                         # effect = TRUE, # (for t-test and Wald test -> as if I run a bivariate logit/reg in stata)
                         test = TRUE,
                         # percent_pattern="{p_col} ({n})",
                         percent_pattern="{p_col}",
                         num_digits = 2, 
                         funs = mean) %>% 
  as_flextable(show_test_name = FALSE)

save_as_docx("Kin 01 - Father" = drop_kin_k1, 
             path = paste0(folder.graph, "Rel_dropped_k1.docx"), align = "left")


drop_kin_k2  <- sample_k %>% 
  filter(kin02_med == 1) %>% 
  crosstable(c(tra_lca.m,
               tra_lca.no, 
               tra_lca.yes,
               cnt_lca.m,
               cnt_lca.no, 
               cnt_lca.yes,
               clo_lca.m,
               clo_lca.no, 
               clo_lca.yes,
               adv_lca.m,
               adv_lca.no, 
               adv_lca.yes,
               cmf_lca.m,
               cmf_lca.no, 
               cmf_lca.yes,
               cou_lca.m,
               cou_lca.no, 
               cou_lca.yes,
               mon_lca.m,
               mon_lca.no, 
               mon_lca.yes,
               cnf_lca.m,
               cnf_lca.no, 
               cnf_lca.yes), 
             by = sample.l,
             unique_numeric = 1,
             showNA = "ifany", 
             # effect = TRUE, # (for t-test and Wald test -> as if I run a bivariate logit/reg in stata)
             test = TRUE,
             # percent_pattern="{p_col} ({n})",
             percent_pattern="{p_col}",
             num_digits = 2, 
             funs = mean) %>% 
  as_flextable(show_test_name = FALSE)

save_as_docx("Kin 02 - Mother" = drop_kin_k2, 
             path = paste0(folder.graph, "Rel_dropped_k2.docx"), align = "left")


drop_kin_k3  <- sample_k %>% 
  filter(kin03_med == 1) %>% 
  crosstable(c(tra_lca.m,
               tra_lca.no, 
               tra_lca.yes,
               cnt_lca.m,
               cnt_lca.no, 
               cnt_lca.yes,
               clo_lca.m,
               clo_lca.no, 
               clo_lca.yes,
               adv_lca.m,
               adv_lca.no, 
               adv_lca.yes,
               cmf_lca.m,
               cmf_lca.no, 
               cmf_lca.yes,
               cou_lca.m,
               cou_lca.no, 
               cou_lca.yes,
               mon_lca.m,
               mon_lca.no, 
               mon_lca.yes,
               cnf_lca.m,
               cnf_lca.no, 
               cnf_lca.yes), 
             by = sample.l,
             unique_numeric = 1,
             showNA = "ifany", 
             # effect = TRUE, # (for t-test and Wald test -> as if I run a bivariate logit/reg in stata)
             test = TRUE,
             # percent_pattern="{p_col} ({n})",
             percent_pattern="{p_col}",
             num_digits = 2, 
             funs = mean) %>% 
  as_flextable(show_test_name = FALSE)

save_as_docx("Kin 03 - Brothers" = drop_kin_k3, 
             path = paste0(folder.graph, "Rel_dropped_k3.docx"), align = "left")


drop_kin_k4  <- sample_k %>% 
  filter(kin04_med == 1) %>% 
  crosstable(c(tra_lca.m,
               tra_lca.no, 
               tra_lca.yes,
               cnt_lca.m,
               cnt_lca.no, 
               cnt_lca.yes,
               clo_lca.m,
               clo_lca.no, 
               clo_lca.yes,
               adv_lca.m,
               adv_lca.no, 
               adv_lca.yes,
               cmf_lca.m,
               cmf_lca.no, 
               cmf_lca.yes,
               cou_lca.m,
               cou_lca.no, 
               cou_lca.yes,
               mon_lca.m,
               mon_lca.no, 
               mon_lca.yes,
               cnf_lca.m,
               cnf_lca.no, 
               cnf_lca.yes), 
             by = sample.l,
             unique_numeric = 1,
             showNA = "ifany", 
             # effect = TRUE, # (for t-test and Wald test -> as if I run a bivariate logit/reg in stata)
             test = TRUE,
             # percent_pattern="{p_col} ({n})",
             percent_pattern="{p_col}",
             num_digits = 2, 
             funs = mean) %>% 
  as_flextable(show_test_name = FALSE)

save_as_docx("Kin 04 - Sisters" = drop_kin_k4, 
             path = paste0(folder.graph, "Rel_dropped_k4.docx"), align = "left")


drop_kin_k5  <- sample_k %>% 
  filter(kin01_med == 1) %>% 
  crosstable(c(tra_lca.m,
               tra_lca.no, 
               tra_lca.yes,
               cnt_lca.m,
               cnt_lca.no, 
               cnt_lca.yes,
               clo_lca.m,
               clo_lca.no, 
               clo_lca.yes,
               adv_lca.m,
               adv_lca.no, 
               adv_lca.yes,
               cmf_lca.m,
               cmf_lca.no, 
               cmf_lca.yes,
               cou_lca.m,
               cou_lca.no, 
               cou_lca.yes,
               mon_lca.m,
               mon_lca.no, 
               mon_lca.yes,
               cnf_lca.m,
               cnf_lca.no, 
               cnf_lca.yes), 
             by = sample.l,
             unique_numeric = 1,
             showNA = "ifany", 
             # effect = TRUE, # (for t-test and Wald test -> as if I run a bivariate logit/reg in stata)
             test = TRUE,
             # percent_pattern="{p_col} ({n})",
             percent_pattern="{p_col}",
             num_digits = 2, 
             funs = mean) %>% 
  as_flextable(show_test_name = FALSE)

save_as_docx("Kin 05 - Grandfathers" = drop_kin_k5, 
             path = paste0(folder.graph, "Rel_dropped_k5.docx"), align = "left")


drop_kin_k6  <- sample_k %>% 
  filter(kin06_med == 1) %>% 
  crosstable(c(tra_lca.m,
               tra_lca.no, 
               tra_lca.yes,
               cnt_lca.m,
               cnt_lca.no, 
               cnt_lca.yes,
               clo_lca.m,
               clo_lca.no, 
               clo_lca.yes,
               adv_lca.m,
               adv_lca.no, 
               adv_lca.yes,
               cmf_lca.m,
               cmf_lca.no, 
               cmf_lca.yes,
               cou_lca.m,
               cou_lca.no, 
               cou_lca.yes,
               mon_lca.m,
               mon_lca.no, 
               mon_lca.yes,
               cnf_lca.m,
               cnf_lca.no, 
               cnf_lca.yes), 
             by = sample.l,
             unique_numeric = 1,
             showNA = "ifany", 
             # effect = TRUE, # (for t-test and Wald test -> as if I run a bivariate logit/reg in stata)
             test = TRUE,
             # percent_pattern="{p_col} ({n})",
             percent_pattern="{p_col}",
             num_digits = 2, 
             funs = mean) %>% 
  as_flextable(show_test_name = FALSE)

save_as_docx("Kin 06 - Grandmothers" = drop_kin_k6, 
             path = paste0(folder.graph, "Rel_dropped_k6.docx"), align = "left")


drop_kin_k7  <- sample_k %>% 
  filter(kin07_med == 1) %>% 
  crosstable(c(tra_lca.m,
               tra_lca.no, 
               tra_lca.yes,
               cnt_lca.m,
               cnt_lca.no, 
               cnt_lca.yes,
               clo_lca.m,
               clo_lca.no, 
               clo_lca.yes,
               adv_lca.m,
               adv_lca.no, 
               adv_lca.yes,
               cmf_lca.m,
               cmf_lca.no, 
               cmf_lca.yes,
               cou_lca.m,
               cou_lca.no, 
               cou_lca.yes,
               mon_lca.m,
               mon_lca.no, 
               mon_lca.yes,
               cnf_lca.m,
               cnf_lca.no, 
               cnf_lca.yes), 
             by = sample.l,
             unique_numeric = 1,
             showNA = "ifany", 
             # effect = TRUE, # (for t-test and Wald test -> as if I run a bivariate logit/reg in stata)
             test = TRUE,
             # percent_pattern="{p_col} ({n})",
             percent_pattern="{p_col}",
             num_digits = 2, 
             funs = mean) %>% 
  as_flextable(show_test_name = FALSE)

save_as_docx("Kin 07 - Halfsiblings" = drop_kin_k7, 
             path = paste0(folder.graph, "Rel_dropped_k7.docx"), align = "left")


drop_kin_k8  <- sample_k %>% 
  filter(kin08_med == 1) %>% 
  crosstable(c(tra_lca.m,
               tra_lca.no, 
               tra_lca.yes,
               cnt_lca.m,
               cnt_lca.no, 
               cnt_lca.yes,
               clo_lca.m,
               clo_lca.no, 
               clo_lca.yes,
               adv_lca.m,
               adv_lca.no, 
               adv_lca.yes,
               cmf_lca.m,
               cmf_lca.no, 
               cmf_lca.yes,
               cou_lca.m,
               cou_lca.no, 
               cou_lca.yes,
               mon_lca.m,
               mon_lca.no, 
               mon_lca.yes,
               cnf_lca.m,
               cnf_lca.no, 
               cnf_lca.yes), 
             by = sample.l,
             unique_numeric = 1,
             showNA = "ifany", 
             # effect = TRUE, # (for t-test and Wald test -> as if I run a bivariate logit/reg in stata)
             test = TRUE,
             # percent_pattern="{p_col} ({n})",
             percent_pattern="{p_col}",
             num_digits = 2, 
             funs = mean) %>% 
  as_flextable(show_test_name = FALSE)

save_as_docx("Kin 08 - Uncles" = drop_kin_k8, 
             path = paste0(folder.graph, "Rel_dropped_k8.docx"), align = "left")


drop_kin_k9  <- sample_k %>% 
  filter(kin09_med == 1) %>% 
  crosstable(c(tra_lca.m,
               tra_lca.no, 
               tra_lca.yes,
               cnt_lca.m,
               cnt_lca.no, 
               cnt_lca.yes,
               clo_lca.m,
               clo_lca.no, 
               clo_lca.yes,
               adv_lca.m,
               adv_lca.no, 
               adv_lca.yes,
               cmf_lca.m,
               cmf_lca.no, 
               cmf_lca.yes,
               cou_lca.m,
               cou_lca.no, 
               cou_lca.yes,
               mon_lca.m,
               mon_lca.no, 
               mon_lca.yes,
               cnf_lca.m,
               cnf_lca.no, 
               cnf_lca.yes), 
             by = sample.l,
             unique_numeric = 1,
             showNA = "ifany", 
             # effect = TRUE, # (for t-test and Wald test -> as if I run a bivariate logit/reg in stata)
             test = TRUE,
             # percent_pattern="{p_col} ({n})",
             percent_pattern="{p_col}",
             num_digits = 2, 
             funs = mean) %>% 
  as_flextable(show_test_name = FALSE)

save_as_docx("Kin 09 - Aunts" = drop_kin_k9, 
             path = paste0(folder.graph, "Rel_dropped_k9.docx"), align = "left")


drop_kin_k10  <- sample_k %>% 
  filter(kin10_med == 1) %>% 
  crosstable(c(tra_lca.m,
               tra_lca.no, 
               tra_lca.yes,
               cnt_lca.m,
               cnt_lca.no, 
               cnt_lca.yes,
               clo_lca.m,
               clo_lca.no, 
               clo_lca.yes,
               adv_lca.m,
               adv_lca.no, 
               adv_lca.yes,
               cmf_lca.m,
               cmf_lca.no, 
               cmf_lca.yes,
               cou_lca.m,
               cou_lca.no, 
               cou_lca.yes,
               mon_lca.m,
               mon_lca.no, 
               mon_lca.yes,
               cnf_lca.m,
               cnf_lca.no, 
               cnf_lca.yes), 
             by = sample.l,
             unique_numeric = 1,
             showNA = "ifany", 
             # effect = TRUE, # (for t-test and Wald test -> as if I run a bivariate logit/reg in stata)
             test = TRUE,
             # percent_pattern="{p_col} ({n})",
             percent_pattern="{p_col}",
             num_digits = 2, 
             funs = mean) %>% 
  as_flextable(show_test_name = FALSE)

save_as_docx("Kin 10 - Cousins" = drop_kin_k10, 
             path = paste0(folder.graph, "Rel_dropped_k10.docx"), align = "left")


# sample_stata <- sample %>%
#   mutate(
#     kin_cat_l = kin_cat.l,
#     eth = anc_eth.l,
#     female = female.l,
#     edu = edu.l) %>%
#   dplyr::select(anc_id, kin_typ, kin_cat_med, kin_cat_small, kin_cat_l, anc_age, female, edu,
#                 adv_lca, clo_lca, cmf_lca, cnf_lca, cnt_lca, mon_lca, cou_lca, sup_lca, tra_lca, dwe, kin_nam,
#                 sample, hi, med, low)

# library(haven)
# drop variable mc.factor
# haven::write_dta(sample_stata, paste0(folder.data, "robust_drop.dta"), label = NULL)


##### 10c LCA ####


# Define function (response ~ predictors) -> Relationship indicators to be included
f <- cbind(cnf_lca, cnt_lca, clo_lca, mon_lca, sup_lca, tra_lca)~1

k <- ""  # number of classes
m <- 3000 # maximum number iterations
r <- 15 # number of times to estimate models (nrep > 1 automates search for global max)

set.seed(240792)

k <- 5
lc5 <- poLCA(f, 
             sample_k, 
             nclass = k,
             maxiter = m,
             nrep = r,
             na.rm = FALSE) 

best <- "lc5"
lc <- lc5

# Across probabilities of class membership --> can differ from predicted classes (based on modal value)
post <- as.character(c("Class 1", "Class 2", "Class 3", "Class 4", "Class 5")) %>%  
  bind_cols(c(round(colMeans(lc$posterior)*100,1)))

# Population shares of classes
print(post)

# Estimated class memberships -> this is based on modal class
round(prop.table(table(lc$predclass)),4)*100

# Store best model (ADJUST HERE TO CHOICE!!!)
lcmodel <- reshape2::melt(lc$probs, level=2)


# Class labels
class.lab <- as.character(c("Tight-knit",
                            "Connected-but-\nautonomous", 
                            "Disharmonious-\nbut-supportive",
                            "Intimate-but-distant",
                            "Detached"))

# attention: shares must be adjusted to class order if not 1-5!
class.lab.p <- as.character(c(paste0("Tight-knit (", post[4,2], "%)"),
                              paste0("Connected-but-\nautonomous (", post[5,2], "%)"),
                              paste0("Disharmonious-\nbut-supportive (", post[3,2], "%)"),
                              paste0("Intimate-but-distant (", post[2,2], "%)"),
                              paste0("Detached (", post[1,2], "%)")))


w = 800 
h = 500

# Profile Plot
png(file = paste0(folder.graph, paste0("profileplot.png")), 
    width = w, height = h)
lcmodel %>% 
  filter(Var2 == "Pr(2)") %>% 
  ggplot(
    aes(x = factor(L2), 
        y = value, 
        colour = Var1, group = Var1, shape = Var1)) + 
  geom_line(size = .8) +
  geom_point(size = 4) +
  geom_text(
    aes(label = round(value, digits = 2)), 
    vjust = -1.5, size = 3, fontface = "bold", show.legend = FALSE, position = position_dodge(.4)) +
  labs(x = "Manifest items", 
       y = "Conditional item response probabilities", 
       title = "All cases (missings NOT excluded)") +
  scale_x_discrete(labels = c("clo_lca" = "Emotional\ncloseness",
                              "cnf_lca" = "Conflict",
                              "cnt_lca" = "Frequency\nof contact",
                              "mon_lca" = "Financial\nsupport",
                              "sup_lca" = "Emotional\nsupport",
                              "tra_lca" = "Geographic\nproximity"),
                   limits = c("tra_lca", "cnt_lca", "clo_lca", "sup_lca", "mon_lca", "cnf_lca")) +
  scale_color_okabeito(name = "Class", 
                       labels = class.lab.p,
                       # order of legend = highest probs to lowest probs
                       limits = c("class 4: ",
                                  "class 5: ",
                                  "class 3: ",
                                  "class 2: ",
                                  "class 1: ")) +
  scale_shape_discrete(name = "Class", 
                       labels = class.lab.p,
                       limits = c("class 4: ",
                                  "class 5: ",
                                  "class 3: ",
                                  "class 2: ",
                                  "class 1: ")) +
  # increase font size of legend and spacing between labels
  theme(legend.text = element_text(
    lineheight = .8,
    size = 10), 
    legend.key.height = unit(.8, "cm"))
dev.off()

USA_stata_robust <- sample_k %>%
  mutate(class = lc$predclass,
         
         # define CLASS LABELS
         class.l = factor(as.character(class), levels = c("1","2","3","4", "5"), 
                          labels = class.lab)) %>% 
  mutate(
    kin_cat_l = kin_cat.l,
    race = race.l,
    female = gnd.fem,
    class = class.l) %>%
  
  dplyr::select(anc_id, kin_typ, kin_cat_med, kin_cat_small, kin_cat_l, anc_age, race, female, 
                adv_lca, clo_lca, cmf_lca, cnf_lca, cnt_lca, mon_lca, cou_lca, sup_lca, tra_lca, class, dwe, kin_nam)

library(haven)
# drop variable mc.factor
write_dta(USA_stata_robust, paste0(folder.data, "USA_robust.dta"), label = NULL)



##### 10d PRED KIN COUNTS BY CLASS ####
# (KIN CAT L; no race-ethnicity diffs) 
library(readxl)
pred_class_overall <- read_excel(paste0(folder.graph,"predprobs_overall.xlsx"), sheet = "predprobs")

pp_overall <-  
  pred_class_overall %>% 
  ggplot(aes(x = as.factor(kincat), 
             y = pp)) +
  geom_bar(aes(fill = as.factor(class)),
           stat = "identity",
           position = position_stack(reverse = TRUE),
           color = "Black",
           size = .4) +
  geom_text(aes(label = paste0(round(pp*100)), group = kincat),
            position = position_stack(vjust = .5),
            size = 3.2) +
  scale_fill_okabeito(name = "Class",
                      labels = class.lab) +
  scale_x_discrete(labels = c("1" = "Father",
                              "2" = "Mother",
                              "3" = "Brother",
                              "4" = "Sister",
                              "5" = "Paternal grandfather",
                              "6" = "Maternal grandfather",
                              "7" = "Paternal grandmother",
                              "8" = "Maternal grandmother",
                              "9" = "Paternal halfsibling",
                              "10" = "Maternal halfsibling",
                              "11" = "Paternal uncle",
                              "12" = "Maternal uncle",
                              "13" = "Paternal aunt",
                              "14" = "Maternal aunt",
                              "15" = "Paternal cousin",
                              "16" = "Maternal cousin")) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +  
  # coord_flip(expand = 0) + # for horizontal bars
  labs(x = "", 
       y = "Predicted Probability (%)",
       title = "All cases (missings NOT included)")
png(file = paste0(folder.graph, paste0("pp_overall.png")), 
    width = w, height = h)
pp_overall
dev.off()


pcount_overall <- 
  pred_class_overall %>% 
  ggplot(aes(x = as.factor(kincat), 
             y = pred_num)) +
  geom_bar(aes(fill = as.factor(class)),
           stat = "identity",
           position = position_stack(reverse = TRUE),
           color = "Black",
           size = .4) +
  # geom_text(aes(label = paste0(round(pred_num, 2)), group = kincat),
  #           position = position_stack(vjust = .5),
  #           size = 3.2) +
  scale_fill_okabeito(name = "Class",
                      labels = class.lab) +
  scale_x_discrete(labels = c("1" = "Father",
                              "2" = "Mother",
                              "3" = "Brother",
                              "4" = "Sister",
                              "5" = "Paternal grandfather",
                              "6" = "Maternal grandfather",
                              "7" = "Paternal grandmother",
                              "8" = "Maternal grandmother",
                              "9" = "Paternal halfsibling",
                              "10" = "Maternal halfsibling",
                              "11" = "Paternal uncle",
                              "12" = "Maternal uncle",
                              "13" = "Paternal aunt",
                              "14" = "Maternal aunt",
                              "15" = "Paternal cousin",
                              "16" = "Maternal cousin")) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +  
  # coord_flip(expand = 0) + # for horizontal bars
  labs(x = "", 
       y = "Average Predicted Count of Kin per Respondent") 

png(file = paste0(folder.graph, paste0("pcount_overall.png")), 
    width = w, height = h)
pcount_overall
dev.off()

## Show both plots in one graph
bars <-  pp_overall + pcount_overall +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom") +
  # increase font size of legend and spacing between labels
  theme(legend.text = element_text(
    lineheight = .8,
    size = 10), 
    legend.key.width = unit(.5, "cm")) 
png(file = paste0(folder.graph, paste0("pred_overall.png")), 
    width = w, height = h)
bars
dev.off()


