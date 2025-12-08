## A typology of nuclear and extended family relations in the United States
## Bettina Hünteler
## 23.02.2024
## huenteler@wiso.uni-koeln.de

# install.packages("waffle")
library(waffle)

#### 07 DISTRIBUTION PLOTS ####


# set to corresponding folder (should already exist from lcaout)
folder.graph <- paste0(folder.graph.model, best, "_output/")
ifelse(!dir.exists(folder.graph), dir.create(folder.graph), "Folder already exists")

# USA_lc <- USA_lc %>% 
#   # generate "CLASS" containing most likely class based on chosen cluster solution (lcx)
#   mutate(class = lc$predclass,
#          
#          # define CLASS LABELS
#          class.l = factor(as.character(class), levels = c("1","2","3","4"), 
#                           labels = class.lab))




#### 07a Plots: Distribution of kin in each class ####

## Stacked bar plot for proportion of kin in each class
png(file = paste0(folder.graph, paste0("kininclass_",kin.l,"_",best,"_total.png")), 
    width = 800, height = 500)
USA_lc %>% 
  filter(!is.na(class)) %>% 
  ggplot(aes(class.l, fill = kin_cat.l, color = kin_cat_med)) +
  geom_bar(color = "white") +
  scale_fill_viridis_d(name = "Kin type", option = "viridis") +
  labs(x = "Class", y = "Number of kin")
dev.off()



## Stacked bar plot for proportion of kin group in each class
png(file = paste0(folder.graph, paste0("kingrinclass",kin.l,"_",best,"_total.png")), 
    width = 800, height = 500)
USA_lc %>% 
  filter(!is.na(class)) %>% 
  ggplot(aes(class.l, fill = kin_cat_small)) +
  geom_bar(color = "white") +
  scale_fill_viridis_d(name = "Kin group", option = "viridis") +
  labs(x = "Class", y = "Number of kin")
dev.off()


png(file = paste0(folder.graph, paste0("kinmed_inclass",kin.l,"_",best,"_total.png")), 
    width = 800, height = 500)
USA_lc %>% 
  filter(!is.na(class)) %>% 
  ggplot(aes(class.l, fill = kin_cat_med)) +
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


png(file = paste0(folder.graph, paste0("kingrinclass",kin.l,"_",best,".png")), 
    width = w, height = h)
USA_lc %>% 
  filter(!is.na(class)) %>% 
  ggplot(aes(class.l, fill = kin_cat_small)) +
  geom_bar(stat = "count", position = "fill", color = "white") +
  scale_fill_viridis_d(name = "Kin type", option = "viridis") +
  labs(x = "Class", y = "Proportion of kin") 
dev.off()






#### 07b Plots: Distribution of classes in kin ####
## Proportion to be in each class by kin type

# Build aggregate data set for plotting proportions!
library(forcats)
data <- USA_lc %>% 
  group_by(kin_cat.l, class.l) %>% 
  summarise(n = n()) %>% 
  group_by(kin_cat.l) %>% 
  mutate(prop = n/sum(n)) %>% 
  ungroup 

cink_prop <-
  data %>% 
  ggplot(aes(x = kin_cat.l, y = prop)) +
  geom_bar(aes(fill = class.l),
           stat = "identity",
           position = position_stack(reverse = TRUE),
           color = "black",
           size = .4) +
  geom_text(aes(label = paste0(round(prop*100)), group = kin_cat.l),
            position = position_stack(vjust = .5),
            size = 3.2) +
  scale_fill_okabeito(name = "Class") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +  
  # coord_flip(expand = 0) + # for horizontal bars
  labs(x = "", y = "Proportion")
png(file = paste0(folder.graph, paste0("classinkin_",kin.l,"_",best,".png")), 
    width = w, height = h)
cink_prop
dev.off()



# Average number of kin 
# Step 1: Create all possible class.l x kin_cat.l combinations for each individual
# Generate a complete grid of anc_id x kin_cat.l x class.l combinations
expanded_relationships <- USA_lc %>%
  distinct(anc_id, kin_cat.l, class.l) %>%  # Select unique anc_id, kin_cat_med, class.l combinations
  complete(anc_id, kin_cat.l, class.l, fill = list(count = 0))  # Complete combinations with count = 0 if missing

# Step 2: Count the relationships per anc_id, kin type, and class.l
relationship_counts <- USA_lc %>%
  group_by(anc_id, kin_cat.l, class.l) %>%
  summarise(count = n(), .groups = 'drop') 

# Step 3: Join with expanded_relationships to ensure all combinations exist for each individual
relationship_counts_complete <- expanded_relationships %>%
  left_join(relationship_counts, by = c("anc_id", "kin_cat.l", "class.l")) %>%
  mutate(count = coalesce(count, 0))  # Replace NA with 0 for missing combinations

# Step 4: Calculate the average count of each class.l per kin type
average_counts <- relationship_counts_complete %>%
  group_by(kin_cat.l, class.l) %>%
  summarise(avg_count = mean(count), .groups = 'drop')

  

# Step 5: Pivot to create a table with kin types as rows and class types as columns
result_table <- average_counts %>%
  pivot_wider(names_from = class.l, values_from = avg_count, values_fill = list(avg_count = 0))

# Transform result_table back to a long format for plotting
plot_data <- result_table %>%
  pivot_longer(cols = -kin_cat.l, names_to = "class.l", values_to = "avg_count")

# Step 6: Define the order of `class.l` based on original data
plot_data$class.l <- factor(plot_data$class.l, levels = class.lab)

# Create the stacked bar plot (Average count of kin)
cink_total <- 
ggplot(plot_data, aes(x = kin_cat.l, y = avg_count)) +
  geom_bar(aes(fill = class.l),
           stat = "identity",
           position = position_stack(reverse = TRUE),
           color = "black",
           size = .4) +# Stacked by default
  # geom_text(aes(label = round(avg_count, 1)), 
  #           position = position_stack(vjust = 0.5),  # Place text in the middle of each segment
  #           size = 3, color = "white") +  # Adjust text size and color
  labs(x = "", y = "Average Count of Kin per Anchor") +
  scale_fill_okabeito(name = "Class") +  # Use viridis_d color palette
  theme_minimal() +
  theme(legend.position = "bottom",
                axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 
cink_total

# cink_total <-
# USA_lc %>%
#   filter(!is.na(class)) %>%
#   ggplot(aes(kin_cat.l, fill = class.l)) +
#   geom_bar(position = position_stack(reverse = TRUE)) +
#   scale_fill_okabeito(name = "Class") +
#   # geom_text(aes(label = ..count..), stat = "count",
#   #           vjust = -.3, colour = "black") +
#   # scale_fill_brewer(type = "seq", palette = "YlGnBu", name = "Class") +
#   labs(x = "", y = "Kin counts") +
#   theme(legend.position = "bottom",
#         axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
# png(file = paste0(folder.graph, paste0("classinkin_total_",kin.l,"_",best,".png")), 
#     width = w, height = h)
# cink_total
# dev.off()




## dodged
USA_lc %>% 
  filter(!is.na(class)) %>% 
  ggplot(aes(x = kin_cat.l, group = class.l)) +
  geom_bar(stat = "count", position = "dodge") 
  

cink_total_d <-
USA_lc %>% 
  filter(!is.na(class)) %>% 
  ggplot(aes(x = kin_cat.l, group = class.l, fill = class.l)) +
  geom_bar(position = "dodge") +
  scale_fill_okabeito(name = "Class") +
  labs(x = "", y = "Kin counts") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 
png(file = paste0(folder.graph, paste0("classinkin_total_",kin.l,"_",best,"dodge.png")), 
    width = w, height = h)
cink_total_d
dev.off()




## Combine graphs on proportion and total classes in kin
## Show all plots in one graph
bars <-  cink_prop + cink_total +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")
png(file = paste0(folder.graph, paste0("classinkin_comb_",kin.l,"_",best,".png")), 
    width = w, height = h)
bars
dev.off()



cink_prop_s <- USA_lc %>% 
  filter(!is.na(class)) %>% 
  ggplot(aes(kin_cat_small, fill = class.l)) +
  geom_bar(stat = "count", position = "fill", color = "white") +
  scale_fill_okabeito(name = "Class") +
  labs(x = "", y = "Proportion") +
  theme(legend.position = "bottom") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))
png(file = paste0(folder.graph, paste0("classingroup_",kin.l,"_",best,".png")), 
    width = w, height = h)  
cink_prop_s 
dev.off()


cink_total_s <- USA_lc %>% 
  filter(!is.na(class)) %>% 
  ggplot(aes(kin_cat_small, fill = class.l)) +
  geom_bar() +
  scale_fill_okabeito(name = "Class") +
  labs(x = "", y = "Kin counts") +
  theme(legend.position = "bottom") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

png(file = paste0(folder.graph, paste0("classingroup_total_",kin.l,"_",best,".png")), 
    width = w, height = h)  
cink_total_s
dev.off()



## Combine graphs on proportion and total classes in kin
## Show all plots in one graph
bars <- cink_total_s + cink_prop_s +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")
png(file = paste0(folder.graph, paste0("classinkin_comb_s_",kin.l,"_",best,".png")), 
    width = w, height = h)
bars
dev.off()


#### 07c Plots: By race ####

png(file = paste0(folder.graph, paste0("classinkin_nuclear_",best,"_byrace.png")), 
    width = 800, height = 500)
USA_lc %>% 
  filter(!is.na(class) & kin_cat_small == "Parents" | kin_cat_small == "Siblings") %>% 
  ggplot(aes(race.l, fill = class.l)) +
  geom_bar(stat = "count", position = "fill", color = "white") +
  scale_fill_okabeito(name = "Class") +
  labs(x = "", y = "Distribution of classes",
       title = "Relationship types by ethnicity (nuclear kin only)") +
  facet_wrap(~race.l, scales = "free")  +
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 
dev.off()

png(file = paste0(folder.graph, paste0("classinkin_all_",best,"_byrace.png")), 
    width = 800, height = 500)
USA_lc %>% 
  filter(!is.na(class)) %>% 
  ggplot(aes(race.l, fill = class.l)) +
  geom_bar(stat = "count", position = "fill", color = "white") +
  scale_fill_okabeito(name = "Class") +
  labs(x = "", y = "Distribution of classes",
       title = "Relationship types by ethnicity (all kin)") +
  facet_wrap(~race.l, scales = "free")  +
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 
dev.off()


png(file = paste0(folder.graph, paste0("classinkin_nuclear_",best,"_byeth.png")), 
    width = 800, height = 500)
USA_lc %>% 
  filter(!is.na(class) & kin_cat_small == "Parents" | kin_cat_small == "Siblings") %>% 
  ggplot(aes(eth.l, fill = class.l)) +
  geom_bar(stat = "count", position = "fill", color = "white") +
  scale_fill_okabeito(name = "Class") +
  labs(x = "", y = "Distribution of classes",
       title = "Relationship types by ethnicity (nuclear kin only)") +
  facet_wrap(~eth.l, scales = "free")  +
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 
dev.off()

png(file = paste0(folder.graph, paste0("classinkin_all_",best,"_byeth.png")), 
    width = 800, height = 500)
USA_lc %>% 
  filter(!is.na(class)) %>% 
  ggplot(aes(race.l, fill = class.l)) +
  geom_bar(stat = "count", position = "fill", color = "white") +  
  scale_fill_okabeito(name = "Class") +
  labs(x = "", y = "Distribution of classes",
       title = "Relationship types by ethnicity (all kin)") +
  facet_wrap(~race.l, scales = "free")  +
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 
dev.off()


#### 07d Plots: By gender ####
## By gender (excludes "prefer not to answer", n=48)

png(file = paste0(folder.graph, paste0("classinkin_nuclear_", best,"_bygnd.png")), 
    width = 800, height = 500)
USA_lc %>% 
  filter(!is.na(class) & kin_cat_small == "Parents" | kin_cat_small == "Siblings") %>% 
  ggplot(aes(female.l, fill = class.l)) +
  geom_bar(stat = "count", position = "fill", color = "white") +
  scale_fill_okabeito(name = "Class") +
  labs(x = "", y = "Distribution of classes",
       title = "Relationship types by gender (nuclear kin only)") + 
  facet_wrap(~female.l, scales = "free")  +
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 
dev.off()


png(file = paste0(folder.graph, paste0("classinkin_all_",best,"_bygnd.png")), 
    width = 800, height = 500)
USA_lc %>% 
  filter(!is.na(class)) %>% 
  ggplot(aes(female.l, fill = class.l)) +
  geom_bar(stat = "count", position = "fill", color = "white") +
  scale_fill_okabeito(name = "Class") +
  labs(x = "", y = "Distribution of classes",
       title = "Relationship types by gender (all kin)") +
  facet_wrap(~female.l, scales = "free")  +
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 
dev.off()



#### 07e Plots: By migration background ####
## By migration background 

png(file = paste0(folder.graph, paste0("classinkin_nuclear_",best,"_bymig.png")), 
    width = 800, height = 500)
USA_lc %>% 
  filter(!is.na(class) & kin_cat_small == "Parents" | kin_cat_small == "Siblings") %>% 
  ggplot(aes(mig.l, fill = class.l)) +
  geom_bar(stat = "count", position = "fill", color = "white") +
  scale_fill_okabeito(name = "Class") +
  labs(x = "", y = "Distribution of classes",
       title = "Relationship types by ethnicity (nuclear kin only)") + 
  facet_wrap(~mig.l, scales = "free")  +
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 
dev.off()


png(file = paste0(folder.graph, paste0("classinkin_all_",best,"_bymig.png")), 
    width = 800, height = 500)
USA_lc %>% 
  filter(!is.na(class)) %>% 
  ggplot(aes(mig.l, fill = class.l)) +
  geom_bar(stat = "count", position = "fill", color = "white") +
  scale_fill_okabeito(name = "Class") +
  labs(x = "", y = "Distribution of classes",
       title = "Relationship types by ethnicity (all kin)") +
  facet_wrap(~mig.l, scales = "free")  +
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 
dev.off()



