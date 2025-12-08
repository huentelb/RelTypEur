#### PREDICTED PROBABILITIES FROM MARGINS (STATA) ####


# set to corresponding folder (should already exist from lcaout)
folder.graph <- paste0(folder.graph.model, "DEFO/")
ifelse(!dir.exists(folder.graph), dir.create(folder.graph), "Folder already exists")

folder.data <- "/Users/Bettina/Library/CloudStorage/OneDrive-DIWBerlin/projects/Kinmatrix/analyses/data/"

library(readxl)
library(flextable)
library(patchwork)
library(gt)


### GRAPHS ####
w <- 800
h <- 500
dodge_width <- .9
theme_set(theme_minimal())

##### OVERALL (KIN CAT L; no race-ethnicity diffs) ##### 

pred_class_overall <- read_excel(paste0(folder.data,"predprobs_overall.xlsx"), sheet = "predprobs")

library(tidyverse)

class.lab <- as.character(c("Eng verbunden",
                            "Verbunden-\naber-autonom", 
                            "Unharmonsich-\naber-unterstützend",
                            "Vertraut-\naber-entfernt",
                            "Distanziert"))

pred_class_overall %>% 
  ggplot(aes(x = fct_rev(as.factor(kincat)), 
             y = pred_num)) +
  geom_bar(aes(fill = as.factor(class)),
           stat = "identity",
           position = position_stack(reverse = TRUE),
           color = "Black",
           size = .4) +
  geom_text(aes(label = paste0(round(pred_num, 2)), group = kincat),
            position = position_stack(vjust = .5),
            size = 3.2,) +
  scale_fill_manual(values = c("1" = "#961e1e", #
                               "2" = "#222220",
                               "3" = "#524e4c",
                               "4" = "#a6a3a1",
                               "5" = "#eae9e4"),
                    name = "Class",
                    labels = class.lab) +
  scale_x_discrete(labels = c("1" = "Vater",
                              "2" = "Mutter",
                              "3" = "Brüder",
                              "4" = "Schwestern",
                              "5" = "Großvater (väterl.)",
                              "6" = "Großvater (mütterl.)",
                              "7" = "Großmutter (väterl.)",
                              "8" = "Großmutter (mütterl.)",
                              "9" = "Halbgeschwister (väterl.)",
                              "10" = "Halbgeschwister (mütterl.)",
                              "11" = "Onkel (väterl.)",
                              "12" = "Onkel (mütterl.)",
                              "13" = "Tanten (väterl.)",
                              "14" = "Tanten (mütterl.)",
                              "15" = "Cousins/Cousinen (v.)",
                              "16" = "Cousins/Cousinen (m.)")) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1, colour = "black", size = 12),
        axis.text.y = element_text(color = "black", size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size=14),
        axis.title.x = element_text(size = 14)) +  
  coord_flip(expand = 0) + # for horizontal bars
  labs(x = "", 
       y = "Durchschnittliche Zahl Verwandter pro Teilnehmenden nach Beziehungstyp") 

ggsave(file = paste0(folder.graph, paste0("pcount_overall.eps")),
       width = 13.5,
       height = 6)
 



###### Dodged bar plots ----
# By race-ethnicity

##### KIN_CAT_EXTRA-SMALL #####

class.lab.wrap <- c("1" = "Eng verbunden",
                    "2" = "Verbunden-\naber-autonom", 
                    "3" = "Unharmonsich-\naber-unterstützend",
                    "4" = "Vertraut-\naber-entfernt",
                    "5" = "Distanziert")

pred_ext_class_xs <- read_excel(paste0(folder.data,"predprobs_xs.xlsx"), sheet = "predprobs")

# Group 1 plot -> Classes 1-4 with smaller x-axis range
plot_group1 <- pred_ext_class_xs %>%
  filter(class %in% c(1, 2, 3, 4)) %>%
  ggplot(aes(x = pred_num, 
             y = factor(eth.l, levels = rev(sort(unique(eth.l)))), 
             fill = factor(kincat, levels = rev(sort(unique(kincat)))))) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = dodge_width),
           color = "Black",
           size = .25) +
  geom_vline(xintercept = 0, color = "Black") +
  geom_text(aes(label = round(pred_num, 1)),
            position = position_dodge(width = dodge_width),
            size = 4, hjust = -.3, color = "Black") +
  facet_wrap(~ class, nrow = 1, 
             labeller = as_labeller(class.lab.wrap),
             scale = "fixed") +  # Facet by race-ethnicity
  scale_x_continuous(expand = expansion(mult = c(0, .5))) +  # Extend x-axis range
  labs(x = "Durchschnittliche Zahl Verwandter pro Teilnehmenden",
       y = "'Race'/Ethnische Herkunft",
       fill = "Verwandtschaftskategorie") +
  scale_y_discrete(labels = c("1" = "NH White", 
                              "2" = "Hispanic", 
                              "3" = "NH Black", 
                              "4" = "NH Asian")) +
  scale_fill_manual(values = c("0" = "#961e1e", 
                               "1" = "#222220",
                               "2" = "#a6a3a1"),
                    labels = c("0" = "Kernfamilie", 
                               "1" = "Erweiterte Kernfamilie",
                               "2" = "Erweiterte Verwandtschaft")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(strip.text = element_text(size = 14), 
        panel.spacing = unit(1.1, "lines"),   # Adjust x-axis title position
        axis.title.x = element_text(hjust = .7, size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(colour = "black", size = 12),
        axis.text.y = element_text(color = "black", size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size= 14),
        legend.position = "bottom")

plot_group1

# Group 2 plot -> Class 5 with larger x-axis range
plot_group2 <- pred_ext_class_xs %>%
  filter(class == 5) %>%
  ggplot(aes(x = pred_num, 
             y = factor(eth.l, levels = rev(sort(unique(eth.l)))), 
             fill = factor(kincat, levels = rev(sort(unique(kincat)))))) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = dodge_width),
           color = "Black",
           size = .25) +
  geom_vline(xintercept = 0, color = "Black") +
  geom_text(aes(label = round(pred_num, 1)),
            position = position_dodge(width = dodge_width),
            size = 4, hjust = -.3, color = "Black") +
  facet_wrap(~ class, 
             labeller = labeller(class = class.lab.wrap),
             scales = "fixed") +
  scale_x_continuous(expand = expansion(mult = c(0, .5))) +
  labs(x = "",
       y = "'Race'/Ethnische Herkunft",
       fill = "Verwandtschaftskategorie") +
  scale_y_discrete(labels = c("1" = "NH White", 
                              "2" = "Hispanic", 
                              "3" = "NH Black", 
                              "4" = "NH Asian")) +
  scale_fill_manual(values = c("0" = "#961e1e", 
                               "1" = "#222220",
                               "2" = "#a6a3a1"),
                    labels = c("0" = "Kernfamilie", 
                               "1" = "Erweiterte Kernfamilie",
                               "2" = "Erweiterte Verwandtschaft")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(strip.text = element_text(size = 14), 
        panel.spacing = unit(1.1, "lines"),  # Colored background for Group 2 (panel 5))
        axis.title.x = element_text(hjust = .9, size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(colour = "#961e1e", face = "bold", size = 12),
        axis.text.y = element_text(color = "black", size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size= 14),
        legend.position = "bottom")
plot_group2
# Combine the two plots using patchwork
plot_group1 + plot_group2 +
  plot_layout(widths = c(4, 1),
              guides = 'collect',
              axis_titles = 'collect',
              axes = 'collect') & 
  theme(legend.position = "bottom")


ggsave(file = paste0(folder.graph, paste0("pcount_byrace.eps")),
       width = 13.5,
       height = 6)

#### LAST LINE ###





