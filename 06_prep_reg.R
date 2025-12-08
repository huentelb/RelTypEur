## A typology of nuclear and extended family relations in Europe
## Bettina Hünteler
## 17.11.2025
## bhuenteler@diw.de


#### 06 PREPARE DATA FRAME FOR FURTHER ANALYSIS ####


# 1. Include relevant vars in dataframe
# 2. Modify for tables

EU_lc <- EU %>% 
  # select variables necessary for analyzing classes
  dplyr::select(anc_id, kin_cat_med, kin_cat_small, kin_typ, kin_cat.l, anc_gnd, anc_gnd.l, anc_cou, anc_cou.l, anc_bic, anc_age,
                anc_eduall, anc_emp.l, anc_emp, dwe, kin_nam) %>% 
  
        # generate "CLASS" containing most likely class based on chosen cluster solution (lcx)
  mutate(class = lc$predclass,
         
         # define CLASS LABELS
         class.l = factor(as.character(class), levels = c("1","2","3","4", "5"), 
                          labels = class.lab)) %>% 
         
         
         # generate covariates
         mutate(
           
           # "FEMALE"
           female = ifelse(anc_gnd == 2, 1, 0),
           female.l = factor(as.character(female), levels = c("0", "1"),
                             labels = c("Male",
                                        "Female")),
           
           
           # EDUCATION
           edu.l = factor(as.character(anc_eduall), levels = c("1", "2", "3"),
                          labels = c("Low", "Medium", "High")),
           low = ifelse(anc_eduall == 1, 1, 0),
           med = ifelse(anc_eduall == 2, 1, 0),
           hi  = ifelse(anc_eduall == 3, 1, 0),
           
           
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
           kin16_mcou = ifelse(kin_cat.l == "Maternal cousin", 1, 0))
 
 
# Add included relationship indicators
# 3. Subset indicators to one df
help <- subset(lca.df[, c("anc_id", "kin_typ",
                          "adv_lca", "clo_lca", "cmf_lca", "cnf_lca", "cnt_lca", "mon_lca", "cou_lca", "sup_lca", "tra_lca")])

# 4. Recode relationship indicators to 0 and 1 (previously 1 and 2 for poLCA)
rel_ind <- help %>%
  mutate(adv_lca = adv_lca - 1,
         cmf_lca = cmf_lca - 1,
         mon_lca = mon_lca - 1,
         cnt_lca = cnt_lca - 1,
         cnf_lca = cnf_lca - 1,
         clo_lca = clo_lca - 1,
         cou_lca = cou_lca - 1,
         sup_lca = sup_lca - 1,
         tra_lca = tra_lca - 1)



# 5. Export dataframe to .dta-stata dataset (no "." allowed in variable names; replace by "_")

# Merge to EU_lc data frame by anchor id and kin id
EU_lc <- merge(EU_lc, rel_ind, by = c("anc_id", "kin_typ"), all.x = TRUE)

EU_stata <- EU_lc %>%
  mutate(
    kin_cat_l = kin_cat.l,
    cntry = anc_cou.l,
    female = female.l,
    edu = edu.l,
    class = class.l) %>%
  dplyr::select(anc_id, kin_typ, kin_cat_med, kin_cat_small, kin_cat_l, anc_age, cntry, female, edu,
                adv_lca, clo_lca, cmf_lca, cnf_lca, cnt_lca, mon_lca, cou_lca, sup_lca, tra_lca, class, dwe, kin_nam)

library(haven)
# drop variable mc.factor
write_dta(EU_stata, paste0(folder.data, "EU_lc.dta"))


# LAST LINE OF CODE #


