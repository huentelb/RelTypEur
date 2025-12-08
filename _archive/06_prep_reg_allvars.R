## A typology of nuclear and extended family relations in the United States
## Bettina Hünteler
## 30.11.2024
## huenteler@wiso.uni-koeln.de


#### 06 PREPARE DATA FRAME FOR FURTHER ANALYSIS ####


# 1. Include relevant vars in dataframe
# 2. Modify for tables

USA_lc <- USA %>% 
  # select variables necessary for analyzing classes
  dplyr::select(anc_id, kin_cat_med, kin_cat_small, kin_typ, kin_cat.l, anc_gnd, anc_gnd.l, anc_eth, anc_his2, anc_his1, anc_bic, anc_age,
                anc_eduall, anc_emp.l, anc_emp, dwe) %>% 
  
        # generate "CLASS" containing most likely class based on chosen cluster solution (lcx)
  mutate(class = lc$predclass,
         
         # define CLASS LABELS
         class.l = factor(as.character(class), levels = c("1","2","3","4","5"), 
                          labels = class.lab)) %>% 
         
         
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
           
           
           
           # generate 'ETHNICITY' variable (1. Non-Hispanic White, 2. Hispanic, 3. Non-Hispanic Black, 4. Non-Hispanic Asian)
           # Non-Hispanic White
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
                          labels = c("White",
                                     "Hispanic",
                                     "Black",
                                     "Asian",
                                     "Other"
                                    # "Non-Hispanic White",
                                    # "Hispanic",
                                    # "Non-Hispanic Black",
                                    # "Non-Hispanic Asian",
                                    # "Non-Hispanic Other"
                                     )),
           
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
# 1. Subset indicators to one df
help <- subset(lca.df[, c("anc_id", "kin_typ",
                          "adv_lca", "clo_lca", "cmf_lca", "cnf_lca", "cnt_lca", "mon_lca", "cou_lca", "sup_lca", "tra_lca")])

# 2. Recode relationship indicators to 0 and 1
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



# # 3. Merge to USA_lc data frame by anchor id and kin id
# USA_lc <- merge(USA_lc, rel_ind, by = c("anc_id", "kin_typ"), all.x = TRUE)
# 
# USA_stata <- USA_lc %>%
#   mutate(
#     kin_cat_l = kin_cat.l,
#     race = race.l,
#     eth = eth.l,
#     female = female.l,
#     edu = edu.l,
#     class = class.l) %>%
#   dplyr::select(anc_id, kin_typ, kin_cat_med, kin_cat_small, kin_cat_l, anc_age, race, eth, female, edu,
#                 adv_lca, clo_lca, cmf_lca, cnf_lca, cnt_lca, mon_lca, cou_lca, sup_lca, tra_lca, class, dwe)
# 
# library(haven)
# # drop variable mc.factor
# write_dta(USA_stata, paste0(folder.data, "USA_lc.dta"))


# LAST LINE OF CODE #


