#### PREDICTED PROBABILITIES FROM MARGINS (STATA) ####


# set to corresponding folder (should already exist from lcaout)
folder.graph <- paste0(folder.graph.model, best, "_output/")
ifelse(!dir.exists(folder.graph), dir.create(folder.graph), "Folder already exists")

folder.data <- "/Users/bhuenteler/Library/CloudStorage/OneDrive-DIWBerlin/projects/Kinmatrix/RelTypEur/analyses/data/"

library(readxl)
library(flextable)
library(patchwork)
library(gt)


### GRAPHS ####
w <- 800
h <- 500
dodge_width <- .9
theme_set(theme_minimal())

class.lab <- c("1" = "Tight-knit",
               "2" = "Connected-but-autonomous", 
               "3" = "Disharmonious-but-supportive",
               "4" = "Intimate-but-distant",
               "5" = "Detached")

class.lab.wrap <- c("1" = "Tight-knit",
                    "2" = "Connected-but-\nautonomous", 
                    "3" = "Disharmonious-\nbut-supportive",
                    "4" = "Intimate-\nbut-distant",
                    "5" = "Detached")


##### OVERALL (KIN CAT L; no country diffs) ##### 

pred_class_overall <- read_excel(paste0(folder.data,"predprobs_overall.xlsx"), sheet = "predprobs")

pp_overall <-
  pred_class_overall %>% 
  ggplot(aes(x = as.factor(kincat), 
             y = pp)) +
  geom_bar(aes(fill = as.factor(class)),
           stat = "identity",
           position = position_stack(reverse = TRUE),
           color = "Black") +
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
    scale_y_continuous(labels = function(x) x * 100) +
    theme(legend.position = "bottom",
          axis.text.y = element_text(color = "black", size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, color = "black", size = 10)) +  
  # coord_flip(expand = 0) + # for horizontal bars
  labs(x = "", 
       y = "Predicted probability (%)")
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
           color = "black") +
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
        axis.text.y = element_text(color = "black", size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, color = "black", size = 10)) +  
  # coord_flip(expand = 0) + # for horizontal bars
  labs(x = "", 
       y = "Average predicted count of kin per respondent") 

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

pdf(file = paste0(folder.graph, paste0("pred_overall.pdf")), 
    width = 10, height = 6.5)
bars
dev.off()






#### PREDICTED KIN COUNTS FROM MARGINS (STATA) ####


##### KIN_CAT_EXTRA-SMALL #####

pred_ext_class_xs <- read_excel(paste0(folder.data,"predprobs_xs.xlsx"), sheet = "predprobs")


###### Dodged bar plots ----
# By Country

# Group 1 plot -> Classes 1-4 with smaller x-axis range
plot_group1 <- pred_ext_class_xs %>%
  filter(class %in% c(1, 2, 3, 4)) %>%
  ggplot(aes(x = pred_num, 
             y = factor(cntry.l, levels = rev(sort(unique(cntry.l)))), 
             fill = factor(kincat, levels = rev(sort(unique(kincat)))))) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = dodge_width),
           color = "Black") +
  geom_vline(xintercept = 0, color = "Black") +
  geom_text(aes(label = round(pred_num, 1)),
            position = position_dodge(width = dodge_width),
            size = 3, hjust = -.3, color = "Black") +
  facet_wrap(~ class, nrow = 1, 
             labeller = as_labeller(class.lab.wrap),
             scale = "fixed") +  # Facet by Country
  scale_x_continuous(expand = expansion(mult = c(0, .5))) +  # Extend x-axis range
  labs(x = "Average predicted count of kin per respondent",
       y = "Country",
       fill = "Kin category") +
  scale_y_discrete(labels = c("1" = "IT", 
                              "2" = "NL", 
                              "3" = "DE", 
                              "4" = "PL",
                              "5" = "UK",
                              "6" = "SE",
                              "7" = "DK",
                              "8" = "FI",
                              "9" = "NO")) +
  scale_fill_manual(values = c("0" = "#d7191c", 
                               "1" = "#ffffbf",
                               "2" = "#2c7bb6"),
                    labels = c("0" = "Nuclear", 
                               "1" = "Nuclear-extended",
                               "2" = "Distant-extended")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(strip.text = element_text(size = 10), 
        panel.spacing = unit(1.1, "lines"),
        axis.title.x = element_text(hjust = .9),
        axis.text = element_text(color = "black"),
        title.text = element_text(color = "black"))  # Adjust x-axis title position
plot_group1

# Group 2 plot -> Class 5 with larger x-axis range
plot_group2 <- pred_ext_class_xs %>%
  filter(class == 5) %>%
  ggplot(aes(x = pred_num, 
             y = factor(cntry.l, levels = rev(sort(unique(cntry.l)))), 
             fill = factor(kincat, levels = rev(sort(unique(kincat)))))) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = dodge_width),
           color = "Black") +
  geom_vline(xintercept = 0, color = "Black") +
  geom_text(aes(label = round(pred_num, 1)),
            position = position_dodge(width = dodge_width),
            size = 3, hjust = -.3, color = "Black") +
  facet_wrap(~ class, 
             labeller = labeller(class = class.lab.wrap),
             scales = "fixed") +
  scale_x_continuous(expand = expansion(mult = c(0, .5))) +
  labs(x = "",
       y = "Country",
       fill = "Kin category") +
  scale_y_discrete(labels = c("1" = "IT", 
                              "2" = "NL", 
                              "3" = "DE", 
                              "4" = "PL",
                              "5" = "UK",
                              "6" = "SE",
                              "7" = "DK",
                              "8" = "FI",
                              "9" = "NO")) +
  scale_fill_manual(values = c("0" = "#d7191c", 
                               "1" = "#ffffbf",
                               "2" = "#2c7bb6"),
                    labels = c("0" = "Nuclear", 
                               "1" = "Nuclear-extended",
                               "2" = "Distant-extended")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(strip.text = element_text(size = 10), 
        panel.spacing = unit(1.1, "lines"),
        axis.text = element_text(color = "black"),
        title.text = element_text(color = "black"))  # Colored background for Group 2 (panel 5))
plot_group2
# Combine the two plots using patchwork
png(file = paste0(folder.graph, paste0("prednum_xs_cntry_bycntry.png")), 
    width = w, height = h)
plot_group1 + plot_group2 +
  plot_layout(widths = c(4, 1),
              guides = 'collect',
              axis_titles = 'collect',
              axes = 'collect') 
dev.off()

pdf(file = paste0(folder.graph, paste0("prednum_xs_cntry_bycntry.pdf")), 
    width = 10, height = 6)
plot_group1 + plot_group2 +
  plot_layout(widths = c(4, 1),
              guides = 'collect',
              axis_titles = 'collect',
              axes = 'collect')
dev.off()

ggsave(file = paste0(folder.graph, paste0("prednum_xs_cntry_bycntry.svg")), 
       width=10, height=6)





#### LAST LINE ###
