## A typology of nuclear and extended family relations in the United States
## Bettina Hünteler
## 23.02.2024
## huenteler@wiso.uni-koeln.de


#### 07 CLUSTER COMPARISON ####

# Which model is best?
best <- "lc4"

load(paste0(folder.graph.hi,"M9/lc4.RData"))
lc4_m9 <- lc4

load(paste0(folder.graph.hi,"M11/lc4.RData"))
lc4_m11 <- lc4

load(paste0(folder.graph.hi,"M3/lc4.RData"))
lc4_m3 <- lc4

load(paste0(folder.graph.hi,"M5/lc4.RData"))
lc4_m5 <- lc4

class.lab3 <- as.character(c("Tightknit", "Close", "Distant", "Ambivalent"))
class.lab5 <- as.character(c("Tightknit", "Close", "Distant", "Ambivalent"))
class.lab9 <- as.character(c("Close", "Ambivalent", "Tightknit", "Distant"))
class.lab11 <- as.character(c("Close", "Ambivalent", "Tightknit", "Distant"))


USA_lc <- USA %>% 
  # select variables necessary for analyzing classes
  dplyr::select(anc_id, kin_cat, kin_cat.l, anc_gnd, anc_gnd.l, anc_eth, anc_his2, anc_his1, anc_bic, anc_age) %>% 
  
  # generate "CLASS" containing most likely class based on chosen cluster solution (lcx)
  mutate(class_m3 = lc4_m3$predclass,
         
         # define CLASS LABELS
         class.l_m3 = factor(as.character(class_m3), levels = c("1","2","3","4"), 
                          labels = class.lab3),
         
         class_m5 = lc4_m5$predclass,
              class.l_m5 = factor(as.character(class_m5), levels = c("1","2","3","4"), 
                                    labels = class.lab5),
         class_m9 = lc4_m9$predclass,
         class.l_m9 = factor(as.character(class_m9), levels = c("1","2","3","4"), 
                             labels = class.lab9),
         class_m11 = lc4_m11$predclass,
         class.l_m11 = factor(as.character(class_m11), levels = c("1","2","3","4"), 
                             labels = class.lab11),
         
         
         # generate numerical help variable to GROUP KIN for more structured display of vars
         # nuclear
         help = ifelse(kin_cat <= 4, 1, 
                       # grandparents
                       ifelse(kin_cat == 5 | kin_cat == 6 | kin_cat == 13 | kin_cat == 14, 2, 
                              # aunts/uncles
                              ifelse(kin_cat == 7 | kin_cat == 8 | kin_cat == 15 | kin_cat == 16, 3, 
                                     # cousins & halfsib (rest non-bio -> 5)
                                     ifelse(kin_cat == 9 | kin_cat == 17 | kin_cat == 11 | kin_cat == 19, 4, 5)))),
         
         # generate factor group variable
         group = factor(as.character(help), levels = c("1","2","3","4","5"), 
                        labels = c("Nuclear",
                                   "Grandparents",
                                   "Aunts and uncles",
                                   "Cousins and halfsiblings",
                                   "Non-biological kin")), 
         
         
         
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
         race.l = factor(as.character(race), levels = c("1","2","3","4"), 
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
         eth.l = factor(as.character(eth), levels = c("1","2","3","4","5","6","7"), 
                        labels = c("Asian",
                                   "Non-Hispanic White",
                                   "Hispanic White",
                                   "Non-Hispanic Black",
                                   "Hispanic Black",
                                   "Missing",
                                   "Other")),
         
         
         # factor "IMMIGRANT"
         mig.l = factor(as.character(anc_bic), levels = c("1","2"), 
                        labels = c("Born in USA",
                                   "Not born in USA")))


save(factors, file = "/Users/Bettina/Documents/datasets/KINMATRIX/comp.RData")


dis_m5 <- USA_lc %>% 
  ggplot(aes(x=class.l_m5)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 

dis_m9 <- USA_lc %>% 
  ggplot(aes(x=class.l_m9)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 

dis_m11 <- USA_lc %>% 
  ggplot(aes(x=class.l_m11)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 

dis_m3 <- USA_lc %>% 
  ggplot(aes(x=class.l_m3)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 

library("gridExtra")
grid.arrange(dis_m3, dis_m5, dis_m9, dis_m11, ncol = 2)

comp11_5 <- table(USA_lc$class.l_m5, USA_lc$class.l_m11)
comp11_5_p <- prop.table(comp11_5, 1)

xtabs(~ class.l_m5 + class.l_m11, USA_lc)
chisq.test(USA_lc$class.l_m5, USA_lc$class.l_m11)


table(USA_lc$class.l_m3)
table(USA_lc$class.l_m5)
table(USA_lc$class.l_m9)
table(USA_lc$class.l_m11)


