## A typology of nuclear and extended family relations in Europe
## Bettina Hünteler
## 17.11.2025
## bhuenteler@diw.de


#### 02 UNIVARIATE BAR PLOTS (Relationship indicators) ####


# Only based on kin with known name

## Set plot scheme to minimal
theme_set(theme_minimal())

folder.graph <- paste0(folder.graph.hi, "barplots/")
ifelse(!dir.exists(folder.graph), dir.create(folder.graph), "Folder already exists")



w = 10
h = 6.8

# Plot distribution of kin (histogram)
pdf(file = paste0(folder.graph, paste0("kin.pdf")), 
    width = w, height = h)
EU %>% 
  ggplot(aes(kin_cat.l)) +
  geom_bar() +
  labs(x = "Category of Kin", 
       y = "Count") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 
dev.off()



######  02a FREQUENCY OF CONTACT ####

# Bar plots by kin category (histogram)
pdf(file = paste0(folder.graph, paste0("hist_cnt.pdf")), 
    width = w, height = h)
EU %>% 
  filter(!is.na(rel_cnt)) %>% 
  ggplot(aes(rel_cnt.l)) +
  geom_bar() +
  labs(x = "", 
       y = "Count",
       title = "Contact Frequency") +
  facet_wrap(~kin_cat.l) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
dev.off()


# Bar plots by kin categories (proportion)

# Extract factor levels for labels (cutting out levels for missing values and only keeping last 6 true levels)
cnt.l <-  tail(as.character(levels(EU$rel_cnt.l)), 6)

pdf(file = paste0(folder.graph, paste0("per_cnt.pdf")), 
    width = w, height = h)
EU %>% 
  filter(!is.na(rel_cnt)) %>% 
  ggplot(aes(rel_cnt)) +
  geom_bar(aes(y = after_stat(prop))) +
  labs(x = "", 
       y = "Proportion",
       title = "Contact Frequency") +
  facet_wrap(~kin_cat.l) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6),
                     labels = cnt.l) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
dev.off()



######  02b FREQUENCY OF CONFLICT ###

# Bar plots by kin category (histogram)
pdf(file = paste0(folder.graph, paste0("hist_cnf.pdf")), 
    width = w, height = h)
EU %>% 
  filter(!is.na(rel_cnf)) %>% 
  ggplot(aes(rel_cnf.l)) +
  geom_bar() +
  labs(x = "", 
       y = "Proportion",
       title = "Conflict") +
  facet_wrap(~kin_cat.l) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
dev.off()

# Bar plots by kin category (proportion)

# Extract factor levels for labels (cutting out levels for missing values and only keeping last 4 true levels)
cnf.l <-  tail(as.character(levels(EU$rel_cnf.l)), 4)

pdf(file = paste0(folder.graph, paste0("per_cnf.pdf")), 
    width = w, height = h)
EU %>% 
  filter(!is.na(rel_cnf)) %>% 
  ggplot(aes(rel_cnf)) +
  geom_bar(aes(y = after_stat(prop))) +
  labs(x = "", 
       y = "Proportion",
       title = "Conflict") +
  facet_wrap(~kin_cat.l) +
  scale_x_continuous(breaks = c(1,2,3,4),
                     labels = cnf.l) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
dev.off()



###### 02c GEOGRAPHIC DISTANCE ####

# Bar plots by kin category (histogram)
pdf(file = paste0(folder.graph, paste0("hist_tra.pdf")), 
    width = w, height = h)
EU %>% 
  filter(!is.na(rel_tra)) %>% 
  ggplot(aes(rel_tra.l)) +
  geom_bar() +
  labs(x = "", 
       y = "Proportion",
       title = "Geographic Proximity") +
  facet_wrap(~kin_cat.l) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
dev.off()

# Bar plots by kin category (proportion)

# Extract factor levels for labels (cutting out levels for missing values and only keeping last 5 true levels)
tra.l <-  tail(as.character(levels(EU$rel_tra.l)), 5)

pdf(file = paste0(folder.graph, paste0("per_tra.pdf")), 
    width = w, height = h)
EU %>% 
  filter(!is.na(rel_tra)) %>% 
  ggplot(aes(rel_tra)) +
  geom_bar(aes(y = after_stat(prop))) +
  labs(x = "", 
       y = "Proportion",
       title = "Geographic Proximity") +
  facet_wrap(~kin_cat.l) +
  scale_x_continuous(breaks = c(1,2,3,4,5),
                     labels = tra.l) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
dev.off()




###### 02d EMOTIONAL CLOSENESS ####

# Bar plots by kin category (histogram)
pdf(file = paste0(folder.graph, paste0("hist_clo.pdf")), 
    width = w, height = h)
EU %>% 
  filter(!is.na(rel_clo)) %>% 
  ggplot(aes(rel_clo.l)) +
  geom_bar() +
  labs(x = "", 
       y = "Proportion",
       title = "Emotional Closeness") +
  facet_wrap(~kin_cat.l) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
dev.off()

# Bar plots by kin category (proportion)

# Extract factor levels for labels (cutting out levels for missing values and only keeping last 5 true levels)
clo.l <-  tail(as.character(levels(EU$rel_clo.l)), 5)

pdf(file = paste0(folder.graph, paste0("per_clo.pdf")), 
    width = w, height = h)
EU %>% 
  filter(!is.na(rel_clo)) %>% 
  ggplot(aes(rel_clo)) +
  geom_bar(aes(y = after_stat(prop))) +
  labs(x = "", 
       y = "Proportion",
       title = "Emotional Closeness") +
  facet_wrap(~kin_cat.l) +
  scale_x_continuous(breaks = c(1,2,3,4,5),
                     labels = clo.l) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
dev.off()



###### 02e MONEY RECEIVED ####

# Bar plots by kin category (histogram)
pdf(file = paste0(folder.graph, paste0("hist_mon1.pdf")), 
    width = w, height = h)
EU %>% 
  filter(!is.na(rel_mon1)) %>% 
  ggplot(aes(rel_mon1.l)) +
  geom_bar() +
  labs(x = "", 
       y = "Proportion",
       title = "Financial Support") +
  facet_wrap(~kin_cat.l) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
dev.off()

# Bar plots by kin category (proportion)

# Extract factor levels for labels (cutting out levels for missing values and only keeping last 2 true levels)
mon1.l <-  tail(as.character(levels(EU$rel_mon1.l)), 2)

pdf(file = paste0(folder.graph, paste0("per_mon1.pdf")), 
    width = w, height = h)
EU %>% 
  filter(!is.na(rel_mon1)) %>% 
  ggplot(aes(rel_mon1)) +
  geom_bar(aes(y = after_stat(prop))) +
  labs(x = "", 
       y = "Proportion",
       title = "Financial Support") +
  facet_wrap(~kin_cat.l) +
  scale_x_continuous(breaks = c(1,2),
                     labels = mon1.l) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
dev.off()




###### 02f ADVICE RECEIVED ####

# Bar plots by kin category (histogram)
pdf(file = paste0(folder.graph, paste0("hist_adv1.pdf")), 
    width = w, height = h)
EU %>% 
  filter(!is.na(rel_adv1)) %>% 
  ggplot(aes(rel_adv1.l)) +
  geom_bar() +
  labs(x = "", 
       y = "Proportion",
       title = "Advice Received from") +
  facet_wrap(~kin_cat.l) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
dev.off()

# Bar plots by kin category (proportion)

# Extract factor levels for labels (cutting out levels for missing values and only keeping last 2 true levels)
adv1.l <-  tail(as.character(levels(EU$rel_adv1.l)), 2)

pdf(file = paste0(folder.graph, paste0("per_adv1.pdf")), 
    width = w, height = h)
EU %>% 
  filter(!is.na(rel_adv1)) %>% 
  ggplot(aes(rel_adv1)) +
  geom_bar(aes(y = after_stat(prop))) +
  labs(x = "", 
       y = "Proportion",
       title = "Advice Received from") +
  facet_wrap(~kin_cat.l) +
  scale_x_continuous(breaks = c(1,2),
                     labels = adv1.l) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
dev.off()




###### 02g COMFORTED BY ####

# Bar plots by kin category (histogram)
pdf(file = paste0(folder.graph, paste0("hist_cmf1.pdf")), 
    width = w, height = h)
EU %>% 
  filter(!is.na(rel_cmf1)) %>% 
  ggplot(aes(rel_cmf1.l)) +
  geom_bar() +
  labs(x = "", 
       y = "Proportion",
       title = "Comfort Received from") +
  facet_wrap(~kin_cat.l) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
dev.off()

# Bar plots by kin category (proportion)

# Extract factor levels for labels (cutting out levels for missing values and only keeping last 2 true levels)
cmf1.l <-  tail(as.character(levels(EU$rel_cmf1.l)), 2)

pdf(file = paste0(folder.graph, paste0("per_cmf1.pdf")), 
    width = w, height = h)
EU %>% 
  filter(!is.na(rel_cmf1)) %>% 
  ggplot(aes(rel_cmf1)) +
  geom_bar(aes(y = after_stat(prop))) +
  labs(x = "", 
       y = "Proportion",
       title = "Comfort Received from") +
  facet_wrap(~kin_cat.l) +
  scale_x_continuous(breaks = c(1,2),
                     labels = cmf1.l) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
dev.off()




###### 02h COUNT ON ####

# Bar plots by kin category (histogram)
pdf(file = paste0(folder.graph, paste0("hist_cou1.pdf")), 
    width = w, height = h)
EU %>% 
  filter(!is.na(rel_cou1)) %>% 
  ggplot(aes(rel_cou1.l)) +
  geom_bar() +
  labs(x = "", 
       y = "Proportion",
       title = "Can count on...") +
  facet_wrap(~kin_cat.l) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
dev.off()

# Bar plots by kin category (proportion)

# Extract factor levels for labels (cutting out levels for missing values and only keeping last 2 true levels)
cou1.l <-  tail(as.character(levels(EU$rel_cou1.l)), 2)

pdf(file = paste0(folder.graph, paste0("per_cou1.pdf")), 
    width = w, height = h)
EU %>% 
  filter(!is.na(rel_cou1)) %>% 
  ggplot(aes(rel_cou1)) +
  geom_bar(aes(y = after_stat(prop))) +
  labs(x = "", 
       y = "Proportion",
       title = "Can count on...") +
  facet_wrap(~kin_cat.l) +
  scale_x_continuous(breaks = c(1,2),
                     labels = cou1.l) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
dev.off()



# LAST LINE OF CODE #

