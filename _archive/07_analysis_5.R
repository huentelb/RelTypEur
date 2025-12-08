## A typology of nuclear and extended family relations in the United States
## Bettina Hünteler
## 23.02.2024
## huenteler@wiso.uni-koeln.de


#### 06 CLUSTER DESCRIPTION ####

# Which model is best?
best <- "lc4"

lpa.df %>% 
  ggplot(aes(x=kin_cat.l)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 


# Store group sizes as values for labelling
asian <- USA %>% 
  filter(anc_eth == 1) %>% 
  nrow()
white <- USA %>% 
  filter(anc_eth == 2) %>% 
  nrow()
black <- USA %>% 
  filter(anc_eth == 3) %>% 
  nrow()
other <- USA %>% 
  filter(anc_eth > 3) %>% 
  nrow()
male <- USA %>% 
  filter(anc_gnd == 1) %>% 
  nrow()
female <- USA %>% 
  filter(anc_gnd == 2) %>% 
  nrow()
mig <- USA %>% 
  filter(anc_bic == 2) %>% 
  nrow()
nomig <- USA %>% 
  filter(anc_bic == 1) %>% 
  nrow()



USA_lc <- USA %>% 
  # select variables necessary for analysing classes
  dplyr::select(anc_id, kin_cat, kin_cat.l, anc_gnd, anc_gnd.l, anc_eth, anc_his2, anc_bic, anc_age) %>% 
  # generate "CLASS" containing most likely class based on chosen cluster solution (lcx)
  mutate(class = lc5$predclass,
         
         # define class labels
         class.l = factor(as.character(class), levels = c("1","2","3","4"), 
                          labels = class.lab),
         
         # generate numerical help variable to group kin for more structured display of vars
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
           
          # generate 'RACE' variable (1. Asian, 2. White, 3. Black, 4. Other) 
          # Asian
          race = ifelse(anc_eth == 1, 1,
                        # White
                        ifelse(anc_eth == 2, 2, 
                               # Black
                               ifelse(anc_eth == 3, 3, 4))),
         # factor "RACE"
         race.l = factor(as.character(race), levels = c("1","2","3","4"), 
                         labels = c("Asian",
                                    "White",
                                    "Black",
                                    "Other")),
         
         # factor "IMMIGRANT"
         mig.l = factor(as.character(anc_bic), levels = c("1","2"), 
                        labels = c("Born in USA",
                                   "Not born in USA")))




## Use this code chunk when you have understood how questions were asked etc.
# generate 'RACE' variable (1. White, 2. Black, 3. Latinx, 4. Asian, 5. Other/NA) -> SOME REMAINING MISSINGS!!!
# Latinx (start with them because some overlap with 1 and 2 but I trust anc_his2 more than anc_eth)
# race = ifelse(anc_his2 == 2, 3,
#               # White
#               ifelse(anc_eth == 2, 1, 
#                      # Black
#                      ifelse(anc_eth == 3, 2,
#                             # Asian
#                             ifelse(anc_eth == 1 , 4, 5)))),
# # factor "RACE"
# race.l = factor(as.character(race), levels = c("1","2","3","4","5"), 
#                 labels = c("White",
#                            "Black",
#                            "Latinx",
#                            "Asian",
#                            "Other/NA")),


table(USA_lc$class)
table(USA_lc$race.l, useNA = "always")
table(USA_lc$anc_eth, useNA = "always")
table(USA_lc$anc_his2, useNA = "always")
table(USA_lc$mig.l, useNA = "always")
table(USA_lc$group, useNA = "always")

## Stacked bar plot for proportion of kin in each class
png(file = paste0(folder.graph, paste0("clasize_",kin.l,"_",best,".png")), 
    width = 800, height = 500)
USA_lc %>% 
  filter(!is.na(class)) %>% 
  ggplot(aes(class.l, fill = kin_cat.l, color = group)) +
  geom_bar(color = "white") +
  scale_fill_viridis_d(name = "Kin type", option = "viridis") +
  labs(x = "Class", y = "Number of kin")
dev.off()


## Stacked bar plot for proportion of kin group in each class
png(file = paste0(folder.graph, paste0("clasizegr_",kin.l,"_",best,".png")), 
    width = 800, height = 500)
USA_lc %>% 
  filter(!is.na(class)) %>% 
  ggplot(aes(class.l, fill = group)) +
  geom_bar(color = "white") +
  scale_fill_viridis_d(name = "Kin group", option = "viridis") +
  labs(x = "Class", y = "Number of kin")
dev.off()


## Proportional stacked bar plot
png(file = paste0(folder.graph, paste0("kininclass_",kin.l,"_",best,".png")), 
    width = 800, height = 500)
USA_lc %>% 
  filter(!is.na(class)) %>% 
  ggplot(aes(class.l, fill = kin_cat.l)) +
  geom_bar(stat = "count", position = "fill", color = "white") +
  scale_fill_viridis_d(name = "Kin type", option = "viridis") +
  labs(x = "Class", y = "Proportion of kin")
dev.off()

png(file = paste0(folder.graph, paste0("kininclassgr_",kin.l,"_",best,".png")), 
    width = 800, height = 500)
USA_lc %>% 
  filter(!is.na(class)) %>% 
  ggplot(aes(class.l, fill = group)) +
  geom_bar(stat = "count", position = "fill", color = "white") +
  scale_fill_viridis_d(name = "Kin type", option = "viridis") +
  labs(x = "Class", y = "Proportion of kin") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))
dev.off()


## Proportion to be in each class by kin type
png(file = paste0(folder.graph, paste0("classinkin_",kin.l,"_",best,".png")), 
    width = 800, height = 1000)
USA_lc %>% 
  filter(!is.na(class)) %>% 
  ggplot(aes(kin_cat.l, fill = class.l)) +
  geom_bar(stat = "count", position = "fill", color = "white") +
  scale_fill_viridis_d(name = "Class") +
  labs(x = "", y = "Distribution of classes") +
  facet_wrap(~group, scales = "free_x", nrow = 4)  +
  theme(legend.position = "bottom") 
dev.off()

USA_lc %>% 
  filter(!is.na(class)) %>% 
  ggplot(aes(kin_cat.l, fill = class.l)) +
  geom_bar() +
  scale_fill_viridis_d(name = "Class") +
  # scale_fill_brewer(type = "seq", palette = "YlGnBu", name = "Class") +
  labs(x = "Class", y = "Distribution of classes") +
  theme(legend.position = "bottom")
  

## 06x SES STRATIFICATION ####

## By race (still includes some remaining missings; dunno why)

png(file = paste0(folder.graph, paste0("classinkin_nuclear_",best,"_byrace.png")), 
    width = 800, height = 500)
USA_lc %>% 
  filter(!is.na(class) & group == "Nuclear") %>% 
  ggplot(aes(race.l, fill = class.l)) +
  geom_bar(stat = "count", position = "fill", color = "white") +
  scale_fill_viridis_d(name = "Class") +
  labs(x = "", y = "Distribution of classes",
       title = "Relationship types by ethnicity (nuclear kin only)") +
  facet_wrap(~race.l, scales = "free")  +
  theme(legend.position = "bottom") 
dev.off()

png(file = paste0(folder.graph, paste0("classinkin_all_",best,"_byrace.png")), 
    width = 800, height = 500)
USA_lc %>% 
  filter(!is.na(class)) %>% 
  ggplot(aes(race.l, fill = class.l)) +
  geom_bar(stat = "count", position = "fill", color = "white") +
  scale_fill_viridis_d(name = "Class") +
  labs(x = "", y = "Distribution of classes",
       title = "Relationship types by ethnicity (all kin)") +
  facet_wrap(~race.l, scales = "free")  +
  theme(legend.position = "bottom") 
dev.off()

## By gender (excludes "prefer not to answer", n=48)

png(file = paste0(folder.graph, paste0("classinkin_nuclear_", best,"_bygnd.png")), 
    width = 800, height = 500)
USA_lc %>% 
  filter(!is.na(class) & group == "Nuclear" & anc_gnd > 0) %>% 
  ggplot(aes(anc_gnd.l, fill = class.l)) +
  geom_bar(stat = "count", position = "fill", color = "white") +
  scale_fill_viridis_d(name = "Class") +
  labs(x = "", y = "Distribution of classes",
       title = "Relationship types by ethnicity (nuclear kin only)") + 
  facet_wrap(~anc_gnd.l, scales = "free")  +
  theme(legend.position = "bottom") 
dev.off()


png(file = paste0(folder.graph, paste0("classinkin_all_",best,"_bygnd.png")), 
    width = 800, height = 500)
USA_lc %>% 
  filter(!is.na(class) & anc_gnd > 0) %>% 
  ggplot(aes(anc_gnd.l, fill = class.l)) +
  geom_bar(stat = "count", position = "fill", color = "white") +
  scale_fill_viridis_d(name = "Class") +
  labs(x = "", y = "Distribution of classes",
       title = "Relationship types by ethnicity (all kin)") +
  facet_wrap(~anc_gnd.l, scales = "free")  +
  theme(legend.position = "bottom") 
dev.off()

## By migration background (excludes "prefer not to answer", n=48)

png(file = paste0(folder.graph, paste0("classinkin_nuclear_",best,"_bymig.png")), 
    width = 800, height = 500)
USA_lc %>% 
  filter(!is.na(class) & group == "Nuclear") %>% 
  ggplot(aes(mig.l, fill = class.l)) +
  geom_bar(stat = "count", position = "fill", color = "white") +
  scale_fill_viridis_d(name = "Class") +
  labs(x = "", y = "Distribution of classes",
       title = "Relationship types by ethnicity (nuclear kin only)") + 
  facet_wrap(~mig.l, scales = "free")  +
  theme(legend.position = "bottom") 
dev.off()


png(file = paste0(folder.graph, paste0("classinkin_all_",best,"_bymig.png")), 
    width = 800, height = 500)
USA_lc %>% 
  filter(!is.na(class)) %>% 
  ggplot(aes(mig.l, fill = class.l)) +
  geom_bar(stat = "count", position = "fill", color = "white") +
  scale_fill_viridis_d(name = "Class") +
  labs(x = "", y = "Distribution of classes",
       title = "Relationship types by ethnicity (all kin)") +
  facet_wrap(~mig.l, scales = "free")  +
  theme(legend.position = "bottom") 
dev.off()


#### 08 SAMPLE DESCRIPTION ####
library(vtable)
st(USA_lc, 
   vars = c("group", "anc_age", "anc_gnd.l", "race.l","mig.l"),
   labels = c("Kin type", "Age", "Gender", "Race", "Migration background"))

st(USA_lc, 
   vars = c("kin_cat.l"),
   labels = c("Kin"),
   summ=c("mean(x)",
          "sd(x)"),
   summ.names = c("Count",
                  "Percentage"))

st(USA_lc, 
   vars = c("group", "class.l", "anc_age", "anc_gnd.l", "race.l","mig.l"),
   labels = c("Kin type", "Relationship type", "Age", "Gender", "Race", "Migration background"),
   out = "csv",
   file = paste0(folder.graph.model, "sumtable_", best))
