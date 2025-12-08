## A typology of nuclear and extended family relations in the United States
## Bettina Hünteler
## 30.11.2024
## huenteler@wiso.uni-koeln.de

#### 09 PLOT PREDICTED PROBABILITIES & AVG KIN COUNTS FROM MARGINS (STATA) ####


# set to corresponding folder (should already exist from lcaout)
folder.graph <- paste0(folder.graph.model, best, "_output/")
ifelse(!dir.exists(folder.graph), dir.create(folder.graph), "Folder already exists")

library(readxl)
library(flextable)
library(gt)


# set default width and heights of graphs and set width of dogding bars
w <- 800
h <- 500
dodge_width <- .9



##### OVERALL (KIN CAT L; no race-ethnicity diffs) ##### 

# read excel file
pred_class_overall <- read_excel(paste0(folder.data,"predprobs_overall.xlsx"), sheet = "predprobs")

# predicted probabilities
pp_overall <-  
  pred_class_overall %>% 
  ggplot(aes(x = as.factor(kincat), 
             y = pp)) +
  geom_bar(aes(fill = as.factor(class)),
           stat = "identity",
           position = position_stack(reverse = TRUE),
           color = "black",
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
       y = "Predicted Probability (%)")
png(file = paste0(folder.graph, paste0("pp_overall.png")), 
    width = w, height = h)
pp_overall
dev.off()


# predicted kin count
pcount_overall <- 
  pred_class_overall %>% 
  ggplot(aes(x = as.factor(kincat), 
             y = pred_num)) +
  geom_bar(aes(fill = as.factor(class)),
           stat = "identity",
           position = position_stack(reverse = TRUE),
           color = "black",
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



#### PREDICTED PROBABILITIES ####

##### NUCLEAR VS EXTENDED ##### 

# read excel file
pred_ext_class_n <- read_excel(paste0(folder.data,"predprobs.xlsx"), sheet = "predprobs")

class.lab.wrap <- c("1" = "Tight-knit",
                    "2" = "Connected-but-\nautonomous", 
                    "3" = "Ambivalent",
                    "4" = "Intimate-\nbut-distant",
                    "5" = "Detached")

# By Race-ethnicity
png(file = paste0(folder.graph, paste0("pp_n_eth.png")), 
    width = w, height = h)
pred_ext_class_n %>% 
  ggplot(aes(x = pp, 
             y = factor(eth.l, levels = rev(sort(unique(eth.l)))), 
             fill = factor(nuclear))) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = dodge_width),
           color = "black") +  # Stacked by default
  geom_vline(xintercept = 0, color = "black") +  # Adds a black line at 0.0
  geom_errorbarh(aes(xmin = lb, xmax = ub), 
                 height = 0.2, color = "black",
                 position = position_dodge(width = dodge_width)) +  # Adds CI lines
  geom_text(aes(label = paste0(round(pp*100))),
            position = position_dodge(width = dodge_width),
            size = 3.2, hjust = -1 )+
  scale_x_continuous(expand = expansion(mult = c(0, 0.25)),
                     labels = function(x) x * 100) +  # Extend x-axis range
  facet_wrap(~ class, nrow = 1, 
             labeller = as_labeller(class.lab.wrap)) +  # Facet by race-ethnicity
  labs(x = "Predicted Probability (%)", 
       y = "Race/Ethnicity",
       title = "Predicted Probabilities for Class Membership \n(based on multinomial logistic regression controlling for gender, weighted)") +
  scale_y_discrete(labels = c("1" = "White", 
                              "2" = "Hispanic", 
                              "3" = "Black", 
                              "4" = "Asian")) +
  scale_fill_manual(values = c("0" = "#2c7bb6", "1" = "#d7191c"),
                    name = "Kin category",
                    labels = c("0" = "Extended", "1" = "Nuclear")) +
  guides(fill = guide_legend(reverse = TRUE)) + # reverse order of items in legend
  theme_minimal() +
  theme(strip.text = element_text(size = 10),  # Adjust x-axis text for readability
        panel.spacing = unit(1.1, "lines"))
dev.off()


# By kin
png(file = paste0(folder.graph, paste0("pp_n_eth_bykin.png")), 
    width = w, height = h)
pred_ext_class_n %>% 
  ggplot(aes(x = pp, 
             y = factor(nuclear), 
             fill = factor(eth.l, levels = rev(sort(unique(eth.l)))))) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = dodge_width),
           color = "black",
           size =.3) +  # Stacked by default
  geom_vline(xintercept = 0, color = "black") +  # Adds a black line at 0.0
  geom_errorbarh(aes(xmin = lb, xmax = ub), 
                 height = 0.2, color = "black",
                 position = position_dodge(width = dodge_width)) +  # Adds CI lines
  geom_text(aes(label = paste0(round(pp*100))),
            position = position_dodge(width = dodge_width),
            size = 3.2, hjust = -.8 )+
  scale_x_continuous(expand = expansion(mult = c(0, 0.25)),
                     labels = function(x) x * 100) +  # Extend x-axis range
  facet_wrap(~ class, nrow = 1, 
             labeller = as_labeller(class.lab.wrap)) +  # Facet by race-ethnicity
    labs(x = "Predicted Probability (%)", 
       y = "",
       title = "Predicted Probabilities for Class Membership \n(based on multinomial logistic regression controlling for gender, weighted)") +
  scale_y_discrete(labels = c("0" = "Distant-extended", 
                              "1" = "Nuclear")) + 
  scale_fill_brewer(name = "Race/Ethnicity",
                    labels = c("1" = "White", 
                               "2" = "Hispanic", 
                               "3" = "Black", 
                               "4" = "Asian"), 
                    palette = "GnBu") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(strip.text = element_text(size = 10),  # Adjust x-axis text for readability
        panel.spacing = unit(1.1, "lines"))
dev.off()






##### KIN_CAT_EXTRA-SMALL #####

# read excel file
pred_ext_class_xs <- read_excel(paste0(folder.data,"predprobs_xs.xlsx"), sheet = "predprobs")

# By race-ethnicity
pred_ext_class_xs %>% 
  group_by(class, eth.l) %>%  # Group by relevant categories
  arrange(desc(kincat)) %>%  # Ensure consistent sorting first
  mutate(
    rev_label = rev(paste0(round(pp*100))),  # Reverse the labels
    rev_pp = rev(pp)  # Reverse the positions as well
  ) %>%
  ungroup() %>% 
  ggplot(aes(x = pp, 
             y = factor(eth.l, levels = rev(sort(unique(eth.l)))), 
             fill = factor(kincat, levels = rev(sort(unique(kincat)))))) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = dodge_width),
           color = "black") +  # Stacked by default
  geom_vline(xintercept = 0, color = "black") +  # Adds a black line at 0.0
  geom_errorbarh(aes(xmin = lb, xmax = ub), 
                 height = 0.4, color = "black",
                 position = position_dodge(width = dodge_width)) +  # Adds CI lines
  geom_text(aes(x = rev_pp, label = rev_label, group = kincat),
              position = position_dodge(width = dodge_width),
              size = 3, hjust = -.8) +
  facet_wrap(~ class, nrow = 1, 
             labeller = as_labeller(class.lab.wrap)) +  # Facet by race-ethnicity
  scale_x_continuous(expand = expansion(mult = c(0, 0.25)),
                     labels = function(x) x * 100) +  # Extend x-axis range
  labs(x = "Predicted Probability (%)", 
       y = "Race/Ethnicity",
       title = "Predicted Probabilities for Class Membership \n(based on multinomial logistic regression controlling for gender, weighted)") +
  scale_y_discrete(labels = c("1" = "White", 
                              "2" = "Hispanic", 
                              "3" = "Black", 
                              "4" = "Asian")) +
  scale_fill_manual(values = c("0" = "#d7191c", 
                               "1" = "#ffffbf",
                               "2" = "#2c7bb6"),
                    name = "Kin category",
                    labels = c("0" = "Nuclear", 
                               "1" = "Extended-nuclear",
                               "2" = "Distant-extended")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(strip.text = element_text(size = 10),  # Adjust x-axis text for readability
        panel.spacing = unit(1.1, "lines"))



# By kin
png(file = paste0(folder.graph, paste0("pp_xs_eth_bykin.png")), 
    width = w, height = h)
pred_ext_class_xs %>% 
  group_by(class, kincat) %>%  # Group by relevant categories
  arrange(desc(eth.l)) %>%  # Ensure consistent sorting first
  mutate(
    rev_label = rev(paste0(round(pp*100))),  # Reverse the labels
    rev_pp = rev(pp)  # Reverse the positions as well
  ) %>%
  ungroup() %>% 
  ggplot(aes(x = pp, 
             y = factor(kincat, levels = rev(sort(unique(kincat)))), 
             fill = factor(eth.l, levels = rev(sort(unique(eth.l)))))) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = dodge_width),
           color = "black",
           size =.3) +  # Stacked by default
  geom_vline(xintercept = 0, color = "black") +  # Adds a black line at 0.0
  geom_errorbarh(aes(xmin = lb, xmax = ub), 
                 height = 0.4, color = "black",
                 position = position_dodge(width = dodge_width)) +  # Adds CI lines
  geom_text(aes(x = rev_pp, label = rev_label, group = eth.l),
            position = position_dodge(width = dodge_width),
            size = 3, hjust = -.8) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.25)),
                     labels = function(x) x * 100) +  # Extend x-axis range
  facet_wrap(~ class, nrow = 1, 
             labeller = as_labeller(class.lab.wrap),
             scales = "fixed") +  # Facet by race-ethnicity
  labs(x = "Predicted Probability (%)", 
       y = "",
       title = "Predicted Probabilities for Class Membership \n(based on multinomial logistic regression controlling for gender, weighted)") +
  scale_y_discrete(labels = c("0" = "Nuclear", 
                              "1" = "Extended-nuclear",
                              "2" = "Distant-extended")) + 
  scale_fill_brewer(name = "Race/Ethnicity",
                    labels = c("1" = "White", 
                               "2" = "Hispanic", 
                               "3" = "Black", 
                               "4" = "Asian"), 
                    palette = "GnBu") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(strip.text = element_text(size = 10),  # Adjust x-axis text for readability
        panel.spacing = unit(1.1, "lines"))
dev.off()








##### KIN_CAT_SMALL #####

# read excel file
pred_ext_class_s <- read_excel(paste0(folder.data,"predprobs_s.xlsx"), sheet = "predprobs")

# By race-ethnicity
png(file = paste0(folder.graph, paste0("pp_small_eth.png")), 
    width = w, height = h)
pred_ext_class_s %>% 
  ggplot(aes(x = pp, 
             y = factor(eth.l, levels = rev(sort(unique(eth.l)))), 
             fill = factor(kincat))) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = dodge_width),
           color = "black") +  # Stacked by default
  geom_vline(xintercept = 0, color = "black") +  # Adds a black line at 0.0
  geom_errorbarh(aes(xmin = lb, xmax = ub), 
                 height = 0.4, color = "black",
                 position = position_dodge(width = dodge_width)) +  # Adds CI lines
  facet_wrap(~ class, nrow = 1, 
             labeller = as_labeller(class.lab.wrap)) +  # Facet by race-ethnicity
  labs(x = "Predicted Probability (%)", 
       y = "Race/Ethnicity",
       title = "Predicted Probabilities for Class Membership \n(based on multinomial logistic regression controlling for gender, weighted)") +
  scale_y_discrete(labels = c("1" = "White", 
                              "2" = "Hispanic", 
                              "3" = "Black", 
                              "4" = "Asian")) +
  scale_fill_brewer(name = "Kin category",
                    labels = c("1" = "Parents", 
                               "2" = "Siblings",
                               "3" = "Grandparents",
                               "4" = "Halfsiblings",
                               "5" = "Aunts & Uncles",
                               "6" = "Cousins"), 
                    palette = "RdYlBu") +
  theme_minimal() +
  theme(strip.text = element_text(size = 10),  # Adjust x-axis text for readability
        panel.spacing = unit(1.1, "lines"))
dev.off()


# By kin
png(file = paste0(folder.graph, paste0("pp_s_eth_bykin.png")), 
    width = w, height = h)
pred_ext_class_s %>% 
  group_by(class, kincat) %>%  # Group by relevant categories
  arrange(desc(eth.l)) %>%  # Ensure consistent sorting first
  mutate(
    rev_label = rev(paste0(round(pp*100))),  # Reverse the labels
    rev_pp = rev(pp)  # Reverse the positions as well
  ) %>%
  ungroup() %>% 
  ggplot(aes(x = pp, 
             y = factor(kincat, levels = rev(sort(unique(kincat)))), 
             fill = factor(eth.l, levels = rev(sort(unique(eth.l)))))) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = dodge_width),
           color = "black",
           size =.3) +  # Stacked by default
  geom_vline(xintercept = 0, color = "black") +  # Adds a black line at 0.0
  geom_errorbarh(aes(xmin = lb, xmax = ub), 
                 height = 0.4, color = "black",
                 position = position_dodge(width = dodge_width)) +  # Adds CI lines
  geom_text(aes(x = rev_pp, label = rev_label, group = eth.l),
            position = position_dodge(width = dodge_width),
            size = 3, hjust = -.9) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.25)),
                     labels = function(x) x * 100) +  # Extend x-axis range
  facet_wrap(~ class, nrow = 1, 
             labeller = as_labeller(class.lab.wrap)) +  # Facet by race-ethnicity
  labs(x = "Predicted Probability (%)", 
       y = "",
       title = "Predicted Probabilities for Class Membership \n(based on multinomial logistic regression controlling for gender, weighted)") +
  scale_y_discrete(labels = c("1" = "Parents", 
                              "2" = "Siblings",
                              "3" = "Grandparents",
                              "4" = "Halfsiblings",
                              "5" = "Aunts & Uncles",
                              "6" = "Cousins")) + 
  scale_fill_brewer(name = "Race/Ethnicity",
                    labels = c("1" = "White", 
                               "2" = "Hispanic", 
                               "3" = "Black", 
                               "4" = "Asian"), 
                    palette = "GnBu") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(strip.text = element_text(size = 10),  # Adjust x-axis text for readability
        panel.spacing = unit(1.1, "lines"))
dev.off()






##### KIN_CAT_MEDIUM #####

# read excel file
pred_ext_class_m <- read_excel(paste0(folder.data,"predprobs_m.xlsx"), sheet = "predprobs")

# Create the stacked bar plot
png(file = paste0(folder.graph, paste0("pp_med_eth.png")), 
    width = w, height = h)
pred_ext_class_m %>% 
  ggplot(aes(x = pp, 
             y = factor(eth.l, levels = rev(sort(unique(eth.l)))), 
             fill = factor(kincat))) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = dodge_width),
           color = "black",
           size =.3) +  # Stacked by default
  geom_vline(xintercept = 0, color = "black") +  # Adds a black line at 0.0
  geom_errorbarh(aes(xmin = lb, xmax = ub), 
                 height = 0.4, color = "black",
                 position = position_dodge(width = dodge_width)) +  # Adds CI lines
  facet_wrap(~ class, nrow = 1, 
             labeller = as_labeller(class.lab.wrap)) +  # Facet by race-ethnicity
  labs(x = "Predicted Probability (%)", 
       y = "Race/Ethnicity",
       title = "Predicted Probabilities for Class Membership \n(based on multinomial logistic regression controlling for gender, weighted)") +
  scale_y_discrete(labels = c("1" = "White", 
                              "2" = "Hispanic", 
                              "3" = "Black", 
                              "4" = "Asian")) +
  scale_fill_brewer(name = "Kin category",
                    labels = c("1" = "Father", 
                               "2" = "Mother", 
                               "3" = "Brothers", 
                               "4" = "Sisters", 
                               "5" = "Grandfathers", 
                               "6" = "Grandmothers", 
                               "7" = "Halfsiblings", 
                               "8" = "Uncles", 
                               "9" = "Aunts", 
                               "10" = "Cousins"), 
                    palette = "RdYlBu") +
  theme_minimal() +
  theme(strip.text = element_text(size = 10),  # Adjust x-axis text for readability
        panel.spacing = unit(1.1, "lines"))
dev.off()

# By kin
png(file = paste0(folder.graph, paste0("pp_med_eth_bykin.png")), 
    width = w, height = h)
pred_ext_class_m %>% 
  group_by(class, kincat) %>%  # Group by relevant categories
  arrange(desc(eth.l)) %>%  # Ensure consistent sorting first
  mutate(
    rev_label = rev(paste0(round(pp*100))),  # Reverse the labels
    rev_pp = rev(pp)  # Reverse the positions as well
  ) %>%
  ungroup() %>% 
  ggplot(aes(x = pp, 
             y = factor(kincat, levels = rev(sort(unique(kincat)))), 
             fill = factor(eth.l, levels = rev(sort(unique(eth.l)))))) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = dodge_width),
           color = "black",
           size =.3) +  # Stacked by default
  geom_vline(xintercept = 0, color = "black") +  # Adds a black line at 0.0
  geom_errorbarh(aes(xmin = lb, xmax = ub), 
                 height = 0.4, color = "black",
                 position = position_dodge(width = dodge_width)) +  # Adds CI lines
  geom_text(aes(x = rev_pp, label = rev_label, group = eth.l),
            position = position_dodge(width = dodge_width),
            size = 2.8, hjust = -.9) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.25)),
                     labels = function(x) x * 100) +  # Extend x-axis range
  facet_wrap(~ class, nrow = 1, 
             scales = "fixed",
             labeller = as_labeller(class.lab.wrap)) +  # Facet by race-ethnicity
  labs(x = "Predicted Probability (%)", 
       y = "",
       title = "Predicted Probabilities for Class Membership \n(based on multinomial logistic regression controlling for gender, weighted)") +
  scale_y_discrete(labels = c("1" = "Father", 
                              "2" = "Mother", 
                              "3" = "Brothers", 
                              "4" = "Sisters", 
                              "5" = "Grandfathers", 
                              "6" = "Grandmothers", 
                              "7" = "Halfsiblings", 
                              "8" = "Uncles", 
                              "9" = "Aunts", 
                              "10" = "Cousins")) + 
  scale_fill_brewer(name = "Race/Ethnicity",
                    labels = c("1" = "White", 
                               "2" = "Hispanic", 
                               "3" = "Black", 
                               "4" = "Asian"), 
                    palette = "GnBu") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(strip.text = element_text(size = 10),  # Adjust x-axis text for readability
        panel.spacing = unit(1.1, "lines"))
dev.off()






##### KIN_CAT_LARGE #####

# read excel file
pred_ext_class_l <- read_excel(paste0(folder.data,"predprobs_l.xlsx"), sheet = "predprobs")

# Create the stacked bar plot
png(file = paste0(folder.graph, paste0("pp_l_eth.png")), 
    width = w, height = h)
pred_ext_class_l %>% 
  ggplot(aes(x = pp, 
             y = factor(eth.l, levels = rev(sort(unique(eth.l)))), 
             fill = factor(kincat))) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = dodge_width),
           color = "black",
           size =.3) +  # Stacked by default
  geom_vline(xintercept = 0, color = "black") +  # Adds a black line at 0.0
  geom_errorbarh(aes(xmin = lb, xmax = ub), 
                 height = 0.4, color = "black",
                 position = position_dodge(width = dodge_width)) +  # Adds CI lines
  facet_wrap(~ class, nrow = 1, 
             labeller = as_labeller(class.lab.wrap)) +  # Facet by race-ethnicity
  labs(x = "Predicted Probability (%)", 
       y = "Race/Ethnicity",
       title = "Predicted Probabilities for Class Membership \n(based on multinomial logistic regression controlling for gender, weighted)") +
  scale_y_discrete(labels = c("1" = "White", 
                              "2" = "Hispanic", 
                              "3" = "Black", 
                              "4" = "Asian")) +
  scale_fill_viridis_d(name = "Kin category",
                    labels = c("1" = "Father", 
                               "2" = "Mother", 
                               "3" = "Brothers", 
                               "4" = "Sisters", 
                               "5" = "Paternal Grandfather", 
                               "6" = "Maternal Grandfather", 
                               "7" = "Paternal Grandmother", 
                               "8" = "Maternal Grandmother", 
                               "9" = "Paternal Halfsiblings", 
                               "10" = "Maternal Halfsiblings",
                               "11" = "Paternal uncles",
                               "12" = "Maternal uncles",
                               "13" = "Paternal aunts",
                               "14" = "Maternal aunts",
                               "15" = "Paternal cousins",
                               "16" = "Maternal cousins")) +
  theme_minimal() +
  theme(strip.text = element_text(size = 10),  # Adjust x-axis text for readability
        panel.spacing = unit(1.1, "lines"))
dev.off()



# By kin
png(file = paste0(folder.graph, paste0("pp_l_eth_bykin.png")), 
    width = w, height = 1000)
pred_ext_class_l %>% 
  group_by(class, kincat) %>%  # Group by relevant categories
  arrange(desc(eth.l)) %>%  # Ensure consistent sorting first
  mutate(
    rev_label = rev(paste0(round(pp*100))),  # Reverse the labels
    rev_pp = rev(pp)  # Reverse the positions as well
  ) %>%
  ungroup() %>% 
  ggplot(aes(x = pp, 
             y = factor(kincat, levels = rev(sort(unique(kincat)))), 
             fill = factor(eth.l, levels = rev(sort(unique(eth.l)))))) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = dodge_width),
           color = "black",
           size =.3) +  # Stacked by default
  geom_vline(xintercept = 0, color = "black") +  # Adds a black line at 0.0
  geom_errorbarh(aes(xmin = lb, xmax = ub), 
                 height = 0.4, color = "black",
                 position = position_dodge(width = dodge_width)) +  # Adds CI lines
  geom_text(aes(x = rev_pp, label = rev_label, group = eth.l),
            position = position_dodge(width = dodge_width),
            size = 2.8, hjust = -.9) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.25)),
                     labels = function(x) x * 100) +  # Extend x-axis range
  facet_wrap(~ class, nrow = 1, 
             scales = "fixed",
             labeller = as_labeller(class.lab.wrap)) +  # Facet by race-ethnicity
  labs(x = "Predicted Probability (%)", 
       y = "",
       title = "Predicted Probabilities for Class Membership \n(based on multinomial logistic regression controlling for gender, weighted)") +
  scale_y_discrete(labels = c("1" = "Father", 
                              "2" = "Mother", 
                              "3" = "Brothers", 
                              "4" = "Sisters", 
                              "5" = "Paternal Grandfather", 
                              "6" = "Maternal Grandfather", 
                              "7" = "Paternal Grandmother", 
                              "8" = "Maternal Grandmother", 
                              "9" = "Paternal Halfsiblings", 
                              "10" = "Maternal Halfsiblings",
                              "11" = "Paternal uncles",
                              "12" = "Maternal uncles",
                              "13" = "Paternal aunts",
                              "14" = "Maternal aunts",
                              "15" = "Paternal cousins",
                              "16" = "Maternal cousins")) + 
  scale_fill_brewer(name = "Race/Ethnicity",
                    labels = c("1" = "White", 
                               "2" = "Hispanic", 
                               "3" = "Black", 
                               "4" = "Asian"), 
                    palette = "GnBu") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(strip.text = element_text(size = 10),  # Adjust x-axis text for readability
        panel.spacing = unit(1.1, "lines"))
dev.off()




#### PREDICTED KIN COUNTS ####

##### NUCLEAR VS EXTENDED ##### 



# read excel file
pred_ext_class_n <- read_excel(paste0(folder.data,"predprobs.xlsx"), sheet = "predprobs")

###### Stacked bar plots ----
png(file = paste0(folder.graph, paste0("prednum_n_eth.png")), 
    width = w, height = h)
ggplot(pred_ext_class_n, aes(x = as.factor(class), y = pred_num, fill = as.factor(nuclear))) +
  geom_bar(stat = "identity", position = "stack",
           color = "black",
           size = .4) +  # Stacked by default
  geom_text(aes(label = round(pred_num, 1)), 
            position = position_stack(vjust = 0.5),  # Place text in the middle of each segment
            size = 3, color = "black") +  # Adjust text size and color
  facet_wrap(~ eth.l, nrow = 1, 
             labeller = as_labeller(c("1" = "White", 
                                      "2" = "Hispanic", 
                                      "3" = "Black", 
                                      "4" = "Asian"))) +  # Facet by race-ethnicity
  labs(x = "", 
       y = "Average Predicted Count of Kin per Respondent",
       title = "Average Number of Kin by Class, Kin Category and Race/Ethnicity \n(based on predicted probabalities controlling for gender, weighted)") +
  scale_x_discrete(labels = class.lab) +  
  scale_fill_manual(values = c("0" = "#2c7bb6", "1" = "#d7191c"),
                      name = "Kin category",
                      labels = c("0" = "Extended", "1" = "Nuclear")) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        strip.text = element_text(size = 10))  # Adjust x-axis text for readability
dev.off()


###### Dodged bar plots ----
# By race-ethnicity
png(file = paste0(folder.graph, paste0("prednum_n_eth_byrace.png")), 
    width = w, height = h)
pred_ext_class_n %>% 
  group_by(class, eth.l) %>%  # Group by relevant categories
  arrange(desc(nuclear)) %>%  # Ensure consistent sorting first
  mutate(
    rev_label = rev(pred_num),  # Reverse the labels
    rev_pp = rev(pred_num)  # Reverse the positions as well
  ) %>%
  ungroup() %>%
  ggplot(aes(x = pred_num, 
             y = factor(eth.l, levels = rev(sort(unique(eth.l)))), 
             fill = factor(nuclear, levels = rev(sort(unique(nuclear)))))) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = dodge_width),
           color = "black",
           size = .25) +  # Stacked by default
  geom_vline(xintercept = 0, color = "black") +  # Adds a black line at 0.0
  geom_text(aes(label = round(pred_num, 1)),
            position = position_dodge(width = dodge_width),  # Place text in the middle of each segment
            size = 3, hjust = -.3, color = "black") +  # Adjust text size and color
  facet_wrap(~ class, nrow = 1, 
             labeller = as_labeller(class.lab.wrap),
             scale = "fixed") +  # Facet by race-ethnicity
  scale_x_continuous(expand = expansion(mult = c(0, .6))) +  # Extend x-axis range
  labs(x = "Average Predicted Count of Kin per Respondent",
       y = "Race/Ethnicity",
       title = "Average Number of Kin by Class, Kin Category and Race/Ethnicity \n(based on predicted probabalities controlling for gender, weighted)") +
  scale_y_discrete(labels = c("1" = "White", 
                              "2" = "Hispanic", 
                              "3" = "Black", 
                              "4" = "Asian")) +
  scale_fill_manual(values = c("0" = "#2c7bb6", "1" = "#d7191c"),
                    name = "Kin category",
                    labels = c("0" = "Extended", "1" = "Nuclear")) +  
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(strip.text = element_text(size = 10),  # Adjust x-axis text for readability
        panel.spacing = unit(1.1, "lines"))
dev.off()


# By kin
png(file = paste0(folder.graph, paste0("prednum_n_eth_bykin.png")), 
    width = w, height = h)
pred_ext_class_n %>% 
  ggplot(aes(x = pred_num, 
             y = factor(nuclear, levels = rev(sort(unique(nuclear)))), 
             fill = factor(eth.l, levels = rev(sort(unique(eth.l)))))) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = dodge_width),
           color = "black",
           size = .3) +  # Stacked by default
  geom_vline(xintercept = 0, color = "black") +  # Adds a black line at 0.0
  geom_text(aes(label = round(pred_num, 1)),
            position = position_dodge(width = dodge_width),  # Place text in the middle of each segment
            size = 3, hjust = -.3, color = "black") +  # Adjust text size and color
  scale_x_continuous(expand = expansion(mult = c(0, 0.6))) +  # Extend x-axis range
  facet_wrap(~ class, nrow = 1, 
             labeller = as_labeller(class.lab.wrap),
             scales = "fixed") +  # Facet by kin-category
  labs(x = "Average Predicted Count of Kin per Respondent",
       y = "Kin category",
       title = "Average Number of Kin by Class, Kin Category and Race/Ethnicity \n(based on predicted probabalities controlling for gender, weighted)") +
  scale_y_discrete(labels = c("0" = "Extended", 
                              "1" = "Nuclear")) + 
  scale_fill_brewer(name = "Race/Ethnicity",
                    labels = c("1" = "White", 
                               "2" = "Hispanic", 
                               "3" = "Black", 
                               "4" = "Asian"), 
                    palette = "GnBu") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(strip.text = element_text(size = 10),  # Adjust x-axis text for readability
        panel.spacing = unit(1.1, "lines"))
dev.off()



##### KIN_CAT_EXTRA-SMALL #####

# read excel file
pred_ext_class_xs <- read_excel(paste0(folder.data,"predprobs_xs.xlsx"), sheet = "predprobs")


###### Stacked bar plot ----
png(file = paste0(folder.graph, paste0("prednum_xs_eth.png")), 
    width = w, height = h)
ggplot(pred_ext_class_xs, aes(x = as.factor(class), y = pred_num, fill = as.factor(kincat))) +
  geom_bar(stat = "identity", position = "stack",
           color = "black",
           size = .4) +  # Stacked by default
  # geom_text(aes(label = round(pred_num, 1)),
  #           position = position_stack(vjust = 0.5),  # Place text in the middle of each segment
  #           size = 3, color = "black") +  # Adjust text size and color
  facet_wrap(~ eth.l, nrow = 1, 
             labeller = as_labeller(c("1" = "White", 
                                      "2" = "Hispanic", 
                                      "3" = "Black", 
                                      "4" = "Asian"))) +  # Facet by race-ethnicity
  labs(x = "", 
       y = "Average Predicted Count of Kin per Respondent",
       title = "Average Number of Kin by Class, Kin Category and Race/Ethnicity \n(based on predicted probabalities controlling for gender, weighted)") +
  scale_x_discrete(labels = class.lab) +  
  scale_fill_manual(values = c("0" = "#d7191c", 
                               "1" = "#ffffbf",
                               "2" = "#2c7bb6"),
                    name = "Kin category",
                    labels = c("0" = "Nuclear", 
                               "1" = "Extended-nuclear",
                               "2" = "Distant-extended")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        strip.text = element_text(size = 10))  # Adjust x-axis text for readability
dev.off()


###### Dodged bar plots ----
# By race-ethnicity
png(file = paste0(folder.graph, paste0("prednum_xs_eth_byrace.png")), 
    width = w, height = h)
pred_ext_class_xs %>% 
  group_by(class, eth.l) %>%  # Group by relevant categories
  arrange(desc(kincat)) %>%  # Ensure consistent sorting first
  mutate(
    rev_label = rev(pred_num),  # Reverse the labels
    rev_pp = rev(pred_num)  # Reverse the positions as well
  ) %>%
  ungroup() %>%
  ggplot(aes(x = pred_num, 
             y = factor(eth.l, levels = rev(sort(unique(eth.l)))), 
             fill = factor(kincat, levels = rev(sort(unique(kincat)))))) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = dodge_width),
           color = "black",
           size = .25) +  # Stacked by default
  geom_vline(xintercept = 0, color = "black") +  # Adds a black line at 0.0
  geom_text(aes(label = round(pred_num, 1)),
            position = position_dodge(width = dodge_width),  # Place text in the middle of each segment
            size = 3, hjust = -.3, color = "black") +  # Adjust text size and color
  facet_wrap(~ class, nrow = 1, 
             labeller = as_labeller(class.lab.wrap),
             scale = "fixed") +  # Facet by race-ethnicity
  scale_x_continuous(expand = expansion(mult = c(0, .6))) +  # Extend x-axis range
  labs(x = "Average Predicted Count of Kin per Respondent",
       y = "Race/Ethnicity",
       title = "Average Number of Kin by Class, Kin Category and Race/Ethnicity \n(based on predicted probabalities controlling for gender, weighted)") +
  scale_y_discrete(labels = c("1" = "White", 
                              "2" = "Hispanic", 
                              "3" = "Black", 
                              "4" = "Asian")) +
  scale_fill_manual(values = c("0" = "#d7191c", 
                               "1" = "#ffffbf",
                               "2" = "#2c7bb6"),
                    name = "Kin category",
                    labels = c("0" = "Nuclear", 
                               "1" = "Extended-nuclear",
                               "2" = "Distant-extended")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(strip.text = element_text(size = 10),  # Adjust x-axis text for readability
        panel.spacing = unit(1.1, "lines"))
dev.off()


# By kin
png(file = paste0(folder.graph, paste0("prednum_xs_eth_bykin.png")), 
    width = w, height = h)
pred_ext_class_xs %>% 
  ggplot(aes(x = pred_num, 
             y = factor(kincat, levels = rev(sort(unique(kincat)))), 
             fill = factor(eth.l, levels = rev(sort(unique(eth.l)))))) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = dodge_width),
           color = "black",
           size = .3) +  # Stacked by default
  geom_vline(xintercept = 0, color = "black") +  # Adds a black line at 0.0
  geom_text(aes(label = round(pred_num, 1)),
            position = position_dodge(width = dodge_width),  # Place text in the middle of each segment
            size = 3, hjust = -.3, color = "black") +  # Adjust text size and color
  scale_x_continuous(expand = expansion(mult = c(0, 0.6))) +  # Extend x-axis range
  facet_wrap(~ class, nrow = 1, 
             labeller = as_labeller(class.lab.wrap),
             scales = "fixed") +  # Facet by kin-category
  labs(x = "Average Predicted Count of Kin per Respondent",
       y = "Kin category",
       title = "Average Number of Kin by Class, Kin Category and Race/Ethnicity \n(based on predicted probabalities controlling for gender, weighted)") +
  scale_y_discrete(labels = c("0" = "Nuclear", 
                              "1" = "Extended-nuclear",
                              "2" = "Distant-extended")) + 
  scale_fill_brewer(name = "Race/Ethnicity",
                    labels = c("1" = "White", 
                               "2" = "Hispanic", 
                               "3" = "Black", 
                               "4" = "Asian"), 
                    palette = "GnBu") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(strip.text = element_text(size = 10),  # Adjust x-axis text for readability
        panel.spacing = unit(1.1, "lines"))
dev.off()


##### KIN_CAT_SMALL #####

# read excel file
pred_ext_class_s <- read_excel(paste0(folder.data,"predprobs_s.xlsx"), sheet = "predprobs")


###### Stacked bar plots ----
png(file = paste0(folder.graph, paste0("prednum_s_eth.png")), 
    width = w, height = h)
ggplot(pred_ext_class_s, aes(x = as.factor(class), y = pred_num, fill = as.factor(kincat))) +
  geom_bar(stat = "identity", position = "stack",
           color = "black",
           size = .4) +  # Stacked by default
  # geom_text(aes(label = round(pred_num, 1)),
  #           position = position_stack(vjust = 0.5),  # Place text in the middle of each segment
  #           size = 3, color = "black") +  # Adjust text size and color
  facet_wrap(~ eth.l, nrow = 1, 
             labeller = as_labeller(c("1" = "White", 
                                      "2" = "Hispanic", 
                                      "3" = "Black", 
                                      "4" = "Asian"))) +  # Facet by race-ethnicity
  labs(x = "", 
       y = "Average Predicted Count of Kin per Respondent",
       title = "Average Number of Kin by Class, Kin Category and Race/Ethnicity \n(based on predicted probabalities controlling for gender, weighted)") +
  scale_x_discrete(labels = class.lab) +  
  scale_fill_brewer(name = "Kin category",
                    labels = c("1" = "Parents", 
                               "2" = "Siblings",
                               "3" = "Grandparents",
                               "4" = "Halfsiblings",
                               "5" = "Aunts & Uncles",
                               "6" = "Cousins"), 
                    palette = "RdYlBu") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        strip.text = element_text(size = 10))  # Adjust x-axis text for readability
dev.off()



###### Dodged bar plots ----
# By race-ethnicity
png(file = paste0(folder.graph, paste0("prednum_s_eth_byrace.png")), 
    width = w, height = h)
pred_ext_class_s %>% 
  group_by(class, eth.l) %>%  # Group by relevant categories
  arrange(desc(kincat)) %>%  # Ensure consistent sorting first
  mutate(
    rev_label = rev(pred_num),  # Reverse the labels
    rev_pp = rev(pred_num)  # Reverse the positions as well
  ) %>%
  ungroup() %>%
  ggplot(aes(x = pred_num, 
             y = factor(eth.l, levels = rev(sort(unique(eth.l)))), 
             fill = factor(kincat, levels = rev(sort(unique(kincat)))))) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = dodge_width),
           color = "black",
           size = .25) +  # Stacked by default
  geom_vline(xintercept = 0, color = "black") +  # Adds a black line at 0.0
  geom_text(aes(label = round(pred_num, 1)),
            position = position_dodge(width = dodge_width),  # Place text in the middle of each segment
            size = 3, hjust = -.3, color = "black") +  # Adjust text size and color
  facet_wrap(~ class, nrow = 1, 
             labeller = as_labeller(class.lab.wrap),
             scale = "fixed") +  # Facet by race-ethnicity
  scale_x_continuous(expand = expansion(mult = c(0, .6))) +  # Extend x-axis range
  labs(x = "Average Predicted Count of Kin per Respondent",
       y = "Race/Ethnicity",
       title = "Average Number of Kin by Class, Kin Category and Race/Ethnicity \n(based on predicted probabalities controlling for gender, weighted)") +
  scale_y_discrete(labels = c("1" = "White", 
                              "2" = "Hispanic", 
                              "3" = "Black", 
                              "4" = "Asian")) +
  scale_fill_brewer(name = "Kin category",
                    labels = c("1" = "Parents", 
                               "2" = "Siblings",
                               "3" = "Grandparents",
                               "4" = "Halfsiblings",
                               "5" = "Aunts & Uncles",
                               "6" = "Cousins"), 
                    palette = "RdYlBu") +  
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(strip.text = element_text(size = 10),  # Adjust x-axis text for readability
        panel.spacing = unit(1.1, "lines"))
dev.off()


# By kin
png(file = paste0(folder.graph, paste0("prednum_s_eth_bykin.png")), 
    width = w, height = h)
pred_ext_class_s %>% 
  ggplot(aes(x = pred_num, 
             y = factor(kincat, levels = rev(sort(unique(kincat)))), 
             fill = factor(eth.l, levels = rev(sort(unique(eth.l)))))) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = dodge_width),
           color = "black",
           size = .3) +  # Stacked by default
  geom_vline(xintercept = 0, color = "black") +  # Adds a black line at 0.0
  geom_text(aes(label = round(pred_num, 1)),
            position = position_dodge(width = dodge_width),  # Place text in the middle of each segment
            size = 3, hjust = -.3, color = "black") +  # Adjust text size and color
  scale_x_continuous(expand = expansion(mult = c(0, 0.6))) +  # Extend x-axis range
  facet_wrap(~ class, nrow = 1, 
             labeller = as_labeller(class.lab.wrap),
             scales = "fixed") +  # Facet by kin-category
  labs(x = "Average Predicted Count of Kin per Respondent",
       y = "Kin category",
       title = "Average Number of Kin by Class, Kin Category and Race/Ethnicity \n(based on predicted probabalities controlling for gender, weighted)") +
  scale_y_discrete(labels = c("1" = "Parents", 
                              "2" = "Siblings",
                              "3" = "Grandparents",
                              "4" = "Halfsiblings",
                              "5" = "Aunts & Uncles",
                              "6" = "Cousins")) + 
  scale_fill_brewer(name = "Race/Ethnicity",
                    labels = c("1" = "White", 
                               "2" = "Hispanic", 
                               "3" = "Black", 
                               "4" = "Asian"), 
                    palette = "GnBu") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(strip.text = element_text(size = 10),  # Adjust x-axis text for readability
        panel.spacing = unit(1.1, "lines"))
dev.off()


##### KIN_CAT_MEDIUM #####

# read excel file
pred_ext_class_m <- read_excel(paste0(folder.data,"predprobs_m.xlsx"), sheet = "predprobs")


###### Stacked bar plots ----
png(file = paste0(folder.graph, paste0("prednum_m_eth.png")), 
    width = w, height = h)
ggplot(pred_ext_class_m, aes(x = as.factor(class), y = pred_num, fill = as.factor(kincat))) +
  geom_bar(stat = "identity", position = "stack",
           color = "black",
           size = .4) +  # Stacked by default
  # geom_text(aes(label = round(pred_num, 1)), 
  #           position = position_stack(vjust = 0.5),  # Place text in the middle of each segment
  #           size = 3, color = "black") +  # Adjust text size and color
  facet_wrap(~ eth.l, nrow = 1, 
             labeller = as_labeller(c("1" = "White", 
                                      "2" = "Hispanic", 
                                      "3" = "Black", 
                                      "4" = "Asian"))) +  # Facet by race-ethnicity
  labs(x = "", 
       y = "Average Predicted Count of Kin per Respondent",
       title = "Average Number of Kin by Class, Kin Category and Race/Ethnicity \n(based on predicted probabalities controlling for gender, weighted)") +
  scale_x_discrete(labels = class.lab) +  
  scale_fill_brewer(name = "Kin category",
                    labels = c("1" = "Father", 
                               "2" = "Mother", 
                               "3" = "Brothers", 
                               "4" = "Sisters", 
                               "5" = "Grandfathers", 
                               "6" = "Grandmothers", 
                               "7" = "Halfsiblings", 
                               "8" = "Uncles", 
                               "9" = "Aunts", 
                               "10" = "Cousins"), 
                    palette = "RdYlBu") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        strip.text = element_text(size = 10))  # Adjust x-axis text for readability
dev.off()


###### Dodged bar plots ----
# By race-ethnicity
png(file = paste0(folder.graph, paste0("prednum_m_eth_byrace.png")), 
    width = w, height = h)
pred_ext_class_m %>% 
  group_by(class, eth.l) %>%  # Group by relevant categories
  arrange(desc(kincat)) %>%  # Ensure consistent sorting first
  mutate(
    rev_label = rev(pred_num),  # Reverse the labels
    rev_pp = rev(pred_num)  # Reverse the positions as well
  ) %>%
  ungroup() %>%
  ggplot(aes(x = pred_num, 
             y = factor(eth.l, levels = rev(sort(unique(eth.l)))), 
             fill = factor(kincat, levels = rev(sort(unique(kincat)))))) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = dodge_width),
           color = "black",
           size = .25) +  # Stacked by default
  geom_vline(xintercept = 0, color = "black") +  # Adds a black line at 0.0
  geom_text(aes(label = round(pred_num, 1)),
            position = position_dodge(width = dodge_width),  # Place text in the middle of each segment
            size = 3, hjust = -.3, color = "black") +  # Adjust text size and color
  facet_wrap(~ class, nrow = 1, 
             labeller = as_labeller(class.lab.wrap),
             scale = "fixed") +  # Facet by race-ethnicity
  scale_x_continuous(expand = expansion(mult = c(0, .6))) +  # Extend x-axis range
  labs(x = "Average Predicted Count of Kin per Respondent",
       y = "Race/Ethnicity",
       title = "Average Number of Kin by Class, Kin Category and Race/Ethnicity \n(based on predicted probabalities controlling for gender, weighted)") +
  scale_y_discrete(labels = c("1" = "White", 
                              "2" = "Hispanic", 
                              "3" = "Black", 
                              "4" = "Asian")) +
  scale_fill_brewer(name = "Kin category",
                    labels = c("1" = "Father", 
                               "2" = "Mother", 
                               "3" = "Brothers", 
                               "4" = "Sisters", 
                               "5" = "Grandfathers", 
                               "6" = "Grandmothers", 
                               "7" = "Halfsiblings", 
                               "8" = "Uncles", 
                               "9" = "Aunts", 
                               "10" = "Cousins"), 
                    palette = "RdYlBu") +  
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(strip.text = element_text(size = 10),  # Adjust x-axis text for readability
        panel.spacing = unit(1.1, "lines"))
dev.off()


# By kin
png(file = paste0(folder.graph, paste0("prednum_m_eth_bykin.png")), 
    width = w, height = h)
pred_ext_class_m %>% 
  ggplot(aes(x = pred_num, 
             y = factor(kincat, levels = rev(sort(unique(kincat)))), 
             fill = factor(eth.l, levels = rev(sort(unique(eth.l)))))) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = dodge_width),
           color = "black",
           size = .3) +  # Stacked by default
  geom_vline(xintercept = 0, color = "black") +  # Adds a black line at 0.0
  geom_text(aes(label = round(pred_num, 1)),
            position = position_dodge(width = dodge_width),  # Place text in the middle of each segment
            size = 3, hjust = -.3, color = "black") +  # Adjust text size and color
  scale_x_continuous(expand = expansion(mult = c(0, 0.6))) +  # Extend x-axis range
  facet_wrap(~ class, nrow = 1, 
             labeller = as_labeller(class.lab.wrap),
             scales = "fixed") +  # Facet by kin-category
  labs(x = "Average Predicted Count of Kin per Respondent",
       y = "Kin category",
       title = "Average Number of Kin by Class, Kin Category and Race/Ethnicity \n(based on predicted probabalities controlling for gender, weighted)") +
  scale_y_discrete(labels = c("1" = "Father", 
                              "2" = "Mother", 
                              "3" = "Brothers", 
                              "4" = "Sisters", 
                              "5" = "Grandfathers", 
                              "6" = "Grandmothers", 
                              "7" = "Halfsiblings", 
                              "8" = "Uncles", 
                              "9" = "Aunts", 
                              "10" = "Cousins")) + 
  scale_fill_brewer(name = "Race/Ethnicity",
                    labels = c("1" = "White", 
                               "2" = "Hispanic", 
                               "3" = "Black", 
                               "4" = "Asian"), 
                    palette = "GnBu") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(strip.text = element_text(size = 10),  # Adjust x-axis text for readability
        panel.spacing = unit(1.1, "lines"))
dev.off()



##### KIN_CAT_LARGE #####

# read excel file
pred_ext_class_l <- read_excel(paste0(folder.data,"predprobs_l.xlsx"), sheet = "predprobs")


###### Stacked bar plots ----
png(file = paste0(folder.graph, paste0("prednum_l_eth.png")), 
    width = w, height = h)
ggplot(pred_ext_class_l, aes(x = as.factor(class), y = pred_num, fill = as.factor(kincat))) +
  geom_bar(stat = "identity", position = "stack",
           color = "black",
           size = .4) +  # Stacked by default
  # geom_text(aes(label = round(pred_num, 1)), 
  #           position = position_stack(vjust = 0.5),  # Place text in the middle of each segment
  #           size = 3, color = "black") +  # Adjust text size and color
  facet_wrap(~ eth.l, nrow = 1, 
             labeller = as_labeller(c("1" = "White", 
                                      "2" = "Hispanic", 
                                      "3" = "Black", 
                                      "4" = "Asian"))) +  # Facet by race-ethnicity
  labs(x = "", 
       y = "Average Predicted Count of Kin per Respondent",
       title = "Average Number of Kin by Class, Kin Category and Race/Ethnicity \n(based on predicted probabalities controlling for gender, weighted)") +
  scale_x_discrete(labels = class.lab) +  
  scale_fill_viridis_d(name = "Kin category",
                       labels = c("1" = "Father", 
                                  "2" = "Mother", 
                                  "3" = "Brothers", 
                                  "4" = "Sisters", 
                                  "5" = "Paternal Grandfather", 
                                  "6" = "Maternal Grandfather", 
                                  "7" = "Paternal Grandmother", 
                                  "8" = "Maternal Grandmother", 
                                  "9" = "Paternal Halfsiblings", 
                                  "10" = "Maternal Halfsiblings",
                                  "11" = "Paternal uncles",
                                  "12" = "Maternal uncles",
                                  "13" = "Paternal aunts",
                                  "14" = "Maternal aunts",
                                  "15" = "Paternal cousins",
                                  "16" = "Maternal cousins")) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        strip.text = element_text(size = 10))  # Adjust x-axis text for readability
dev.off()


###### Dodged bar plots ----
# By race-ethnicity
png(file = paste0(folder.graph, paste0("prednum_l_eth_byrace.png")), 
    width = w, height = h)
pred_ext_class_l %>% 
  group_by(class, eth.l) %>%  # Group by relevant categories
  arrange(desc(kincat)) %>%  # Ensure consistent sorting first
  mutate(
    rev_label = rev(pred_num),  # Reverse the labels
    rev_pp = rev(pred_num)  # Reverse the positions as well
  ) %>%
  ungroup() %>%
  ggplot(aes(x = pred_num, 
             y = factor(eth.l, levels = rev(sort(unique(eth.l)))), 
             fill = factor(kincat, levels = rev(sort(unique(kincat)))))) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = dodge_width),
           color = "black",
           size = .25) +  # Stacked by default
  geom_vline(xintercept = 0, color = "black") +  # Adds a black line at 0.0
  geom_text(aes(label = round(pred_num, 1)),
            position = position_dodge(width = dodge_width),  # Place text in the middle of each segment
            size = 3, hjust = -.3, color = "black") +  # Adjust text size and color
  facet_wrap(~ class, nrow = 1, 
             labeller = as_labeller(class.lab.wrap),
             scale = "fixed") +  # Facet by race-ethnicity
  scale_x_continuous(expand = expansion(mult = c(0, .6))) +  # Extend x-axis range
  labs(x = "Average Predicted Count of Kin per Respondent",
       y = "Race/Ethnicity",
       title = "Average Number of Kin by Class, Kin Category and Race/Ethnicity \n(based on predicted probabalities controlling for gender, weighted)") +
  scale_y_discrete(labels = c("1" = "White", 
                              "2" = "Hispanic", 
                              "3" = "Black", 
                              "4" = "Asian")) +
  scale_fill_viridis_d(name = "Kin category",
                       labels = c("1" = "Father", 
                                  "2" = "Mother", 
                                  "3" = "Brothers", 
                                  "4" = "Sisters", 
                                  "5" = "Paternal Grandfather", 
                                  "6" = "Maternal Grandfather", 
                                  "7" = "Paternal Grandmother", 
                                  "8" = "Maternal Grandmother", 
                                  "9" = "Paternal Halfsiblings", 
                                  "10" = "Maternal Halfsiblings",
                                  "11" = "Paternal uncles",
                                  "12" = "Maternal uncles",
                                  "13" = "Paternal aunts",
                                  "14" = "Maternal aunts",
                                  "15" = "Paternal cousins",
                                  "16" = "Maternal cousins")) +  
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(strip.text = element_text(size = 10),  # Adjust x-axis text for readability
        panel.spacing = unit(1.1, "lines"))
dev.off()


# By kin
png(file = paste0(folder.graph, paste0("prednum_l_eth_bykin.png")), 
    width = w, height = h)
pred_ext_class_l %>% 
  ggplot(aes(x = pred_num, 
             y = factor(kincat, levels = rev(sort(unique(kincat)))), 
             fill = factor(eth.l, levels = rev(sort(unique(eth.l)))))) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = dodge_width),
           color = "black",
           size = .3) +  # Stacked by default
  geom_vline(xintercept = 0, color = "black") +  # Adds a black line at 0.0
  geom_text(aes(label = round(pred_num, 1)),
            position = position_dodge(width = dodge_width),  # Place text in the middle of each segment
            size = 3, hjust = -.3, color = "black") +  # Adjust text size and color
  scale_x_continuous(expand = expansion(mult = c(0, 0.6))) +  # Extend x-axis range
  facet_wrap(~ class, nrow = 1, 
             labeller = as_labeller(class.lab.wrap),
             scales = "fixed") +  # Facet by kin-category
  labs(x = "Average Predicted Count of Kin per Respondent",
       y = "Kin category",
       title = "Average Number of Kin by Class, Kin Category and Race/Ethnicity \n(based on predicted probabalities controlling for gender, weighted)") +
  scale_y_discrete(labels = c("1" = "Father", 
                              "2" = "Mother", 
                              "3" = "Brothers", 
                              "4" = "Sisters", 
                              "5" = "Paternal Grandfather", 
                              "6" = "Maternal Grandfather", 
                              "7" = "Paternal Grandmother", 
                              "8" = "Maternal Grandmother", 
                              "9" = "Paternal Halfsiblings", 
                              "10" = "Maternal Halfsiblings",
                              "11" = "Paternal uncles",
                              "12" = "Maternal uncles",
                              "13" = "Paternal aunts",
                              "14" = "Maternal aunts",
                              "15" = "Paternal cousins",
                              "16" = "Maternal cousins")) + 
  scale_fill_brewer(name = "Race/Ethnicity",
                    labels = c("1" = "White", 
                               "2" = "Hispanic", 
                               "3" = "Black", 
                               "4" = "Asian"), 
                    palette = "GnBu") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(strip.text = element_text(size = 10),  # Adjust x-axis text for readability
        panel.spacing = unit(1.1, "lines"))
dev.off()


# LAST LINE OF CODE #
