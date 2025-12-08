## A typology of nuclear and extended family relations in the United States
## Bettina Hünteler
## 23.02.2024
## huenteler@wiso.uni-koeln.de


data <- USA_lc %>% 
   group_by(female.l, class.l) %>% 
   summarise(n = n()) %>% 
   group_by(class.l) %>% 
   mutate(prop = n/sum(n)*100) %>% 
   ungroup 


tab1 <- USA_lc %>% 
   group_by(anc_id, kin_cat.l) %>% 
   summarise(n = n()) %>% 
   group_by(anc_id) %>% 
   mutate(prop = n/sum()) %>% 
   ungroup

#### 08 DESCRIPTIVE TABLES ####
library(flextable)
library(vtable)

controls <- as.character(c("anc_age", "female.l", "race.l", "eth.l", "mig.l", "lfs.l", "edu.l"))
controls.l <- as.character(c("Age", "Gender", "Race", "Ethnicity", "Migration background", "Labor force status", "Education"))

kin_avg <- as.character(c("father", "mother", "brother", "sister", "patgf", "matgf", "patgm", 
                          "matgm", "paths", "maths", "patun", "matun", "patau", "matau", "patcou", "matcou"))
kin_avg.l <- as.character(c("Father",
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
                            "Maternal cousin"))


#### FLEXTABLE ####
tab1_vars <- USA_lc %>% 
   dplyr::select(kin_cat.l, all_of(controls))
# summarize vars in dataframe (over gender)
obj <- summarizor(tab1_vars, by = "female.l", overall_label = "Overall")
obj
ft <- as_flextable(obj, 
                   spread_first_col = TRUE, 
                   separate_with = "variable",
                   row_title = c("Kin", "Age", "Race", "Ethnicity", "Migration", "LFS", "Edu"))
ft

#### SUMTABLE ####

st(USA_lc, 
   vars = c("group", controls),
   labels = c("Kin type", controls.l))

st(USA_lc, 
   vars = c("kin_cat.l"),
   labels = c("Kin"),
   summ=c("mean(x)",
          "sd(x)"),
   summ.names = c("Count",
                  "Percentage"))

## Classes
st(USA_lc, 
   vars = c("class.l"),
   labels = c("Relationship types"),
   summ=c("mean(x)",
          "sd(x)"),
   summ.names = c("Count",
                  "Percentage"))

## Relationship Indicators
st(USA_lc, 
   vars = c("cnf_lca", "cnt_lca", "clo_lca", "mon_lca", "adv_lca", "cmf_lca"),
   labels = c("Conflict", "Contact", "Closeness", "Money", "Advice", "Comfort"),
   group = "class.l", 
   summ=c("mean(x)"),
   summ.names = c("Mean"))

st(USA_lc, 
   vars = c("adv_lca", "clo_lca", "cmf_lca", "cnf_lca", "cnt_lca", "mon_lca"),
   labels = c("Advice", "Closeness", "Comfort", "Conflict", "Contact", "Money"),
   group = "class.l", 
   summ=c("mean(x)"),
   summ.names = c("Mean"))

## TABLE 1
st(USA_lc, 
   vars = c("kin_cat.l", kin_avg, controls),
   labels = c("Kin type", kin_avg.l, controls.l),
   group = "female.l",
   numformat = "comma",
   out = "csv",
   file = paste0(folder.graph.model, "table_1_", model))

st(USA_lc, 
   vars = c("kin_cat.l", controls),
   labels = c("Kin type", controls.l),
   numformat = "comma",
   out = "csv",
   file = paste0(folder.graph.model, "table_1_total_", model))



## TABLE 2
st(USA_lc, 
   vars = c("kin_cat.l", controls),
   labels = c("Kin type", controls.l),
   group = "class.l", 
   group.test = TRUE,
   numformat = "comma",
   out = "csv",
   file = paste0(folder.graph.model, "table_2_catl_", model))

st(USA_lc, 
   vars = c("kin_cat_small", controls),
   labels = c("Kin type", controls.l),
   group = "class.l", 
   group.test = TRUE,
   numformat = "comma",
   out = "csv",
   file = paste0(folder.graph.model, "table_2_catsmall", model))

st(USA_lc, 
   vars = c("kin_cat_med", controls),
   labels = c("Kin type", controls.l),
   group = "class.l", 
   group.test = TRUE,
   numformat = "comma",
   out = "csv",
   file = paste0(folder.graph.model, "table_2_catmed", model))