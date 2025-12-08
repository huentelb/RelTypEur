## A typology of nuclear and extended family relations in Europe
## Bettina Hünteler
## 17.11.2025
## bhuenteler@diw.de


#### 03 LCA PREPARATIONS ####

###### 03a KIN SELECTION FOR LCA ####


# Define for which kin types LCA should be conducted and change labels Below median
kin <- EU # %>%
  # only keep ...
  # filter(kin_cat == 4)

# Adjust to selected kin for automated labeling and storing of graphs
kin.l <- "all" # for file names
kin.L <- "All" # for labels




###### 03b CUT-OFF CRITERIA ####
kin_name <- kin %>% 
  # medians based on kin for whom name was known
  filter(kin_nam == 1) 
med_cnf <- median(kin_name$rel_cnf, na.rm = TRUE)
med_cnt <- median(kin_name$rel_cnt, na.rm = TRUE)
med_tra <- median(kin_name$rel_tra, na.rm = TRUE)
med_clo <- median(kin_name$rel_clo, na.rm = TRUE)

med_cnf
med_cnt
med_tra
med_clo

med_cou <- kin_name %>% 
  dplyr::select(anc_cou.l, rel_clo, rel_cnf, rel_cnt, rel_tra) %>% 
  group_by(anc_cou.l) %>% 
  summarise(across(everything(), median, na.rm=TRUE))

# Above median the median -> 2
# incl and Below median median -> 1

# Model name (different model for different cut-offs)
model <- "M11_med_cnf2"


lca.df <- kin %>%
  # continuous -> apply cut-off
  mutate(cnf_lca = ifelse(rel_cnf <= 2 | is.na(rel_cnf), 1, 2)) %>% # 1. none at all - 4. a great deal (at least "a little" 2)
  mutate(cnt_lca = ifelse(rel_cnt >= med_cnt | is.na(rel_cnt), 1, 2)) %>% # 1. daily - 6. never (at least "once per month" 3)
  mutate(tra_lca = ifelse(rel_tra >= med_tra | is.na(rel_tra), 1, 2)) %>% # 1. same building - 5. >=1 hrs (within 1 hour)
  mutate(clo_lca = ifelse(rel_clo <= med_clo | is.na(rel_clo), 1, 2)) %>% # 1. not at all - 5. very close (at least "very close")
  # binary -> if missing, set to 1; else keep values as are
  mutate(mon_lca = ifelse(is.na(rel_mon1), 1, rel_mon1),
         adv_lca = ifelse(is.na(rel_adv1), 1, rel_adv1),
         cmf_lca = ifelse(is.na(rel_cmf1), 1, rel_cmf1),
         cou_lca = ifelse(is.na(rel_cou1), 1, rel_cou1)) %>% 
  # generate variable for support combo of comfort and advice (any given)
  mutate(rel_sup = ifelse(rel_adv1 == 1 & rel_cmf1 == 1 & rel_cou1 == 1, 1, 2),
         rel_sup.l = factor(as.character(rel_sup), levels = c("1","2"), 
                            labels = c("1. Did not support", "2. Supported")),
         # with imputing missings for LCA
         sup_lca = ifelse(  (adv_lca == 1 | is.na(rel_adv1)) &
                            (cmf_lca == 1 | is.na(rel_cmf1)) & 
                            (cou_lca == 1 | is.na(rel_cou1)), 1, 2),
         sup_lca.l = factor(as.character(sup_lca), levels = c("1","2"), 
                         labels = c("1. Did not support", "2. Supported")))








# Bar plot for SUPPORT RECEIVED (histogram)
pdf(file = paste0(folder.graph, paste0("hist_sup.pdf")), 
    width = 10, height = 6.8)
lca.df %>% 
  filter(kin_nam == 1, !is.na(rel_sup),
         kin_nam == 1) %>% 
  ggplot(aes(rel_sup.l)) +
  geom_bar() +
  labs(x = "", 
       y = "Proportion",
       title = "Social Support") +
  facet_wrap(~kin_cat.l) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
dev.off()

# Bar plots by types (proportion)
sup.l <-  tail(as.character(levels(lca.df$sup_lca.l)), 2)

pdf(file = paste0(folder.graph, paste0("per_sup.pdf")), 
    width = 10, height = 6.8)
lca.df %>% 
  filter(kin_nam == 1, !is.na(rel_sup),
         kin_nam == 1) %>% 
  ggplot(aes(rel_sup)) +
  geom_bar(aes(y = after_stat(prop))) +
  labs(x = "", 
       y = "Proportion",
       title = "Social Support") +
  facet_wrap(~kin_cat.l) +
  scale_x_continuous(breaks = c(1,2),
                     labels = sup.l) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
dev.off()


# load packages
library(gt)
library(gtExtras)
library(dplyr)


library(dplyr)
library(gt)
library(gtExtras)
library(tidyr)

# Define relationship indicators to summarize
rel_vars <- c("rel_tra", "rel_cnt", "rel_clo", "rel_sup", "rel_mon1", "rel_cnf")

# Recode binary indicators from 1,2 notation to 0,1 notation
app_tab <- lca.df %>% 
  mutate(rel_sup = ifelse(rel_sup == 1, 0, 1),
         rel_mon1 = ifelse(rel_mon1 == 1, 0, 1))

# Summarize data by kin_cat.l using across()
summary_table <- app_tab %>%
  group_by(kin_cat.l) %>%
  summarise(
    across(all_of(rel_vars), 
           list(min = ~min(.x, na.rm = TRUE),
                max = ~max(.x, na.rm = TRUE),
                mean = ~round(mean(.x, na.rm = TRUE),2),
                p50 = ~median(.x, na.rm = TRUE)),
           .names = "{fn}_{.col}"),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -kin_cat.l,
    names_to = c("Statistic", "Variable"),
    names_pattern = "(min|max|mean|p50)_(.*)"
  ) %>%
  pivot_wider(names_from = Statistic, values_from = value)


# Generate histograms separately for each kin_cat.l group
hist_data <- bind_rows(lapply(rel_vars, function(var) {
  app_tab %>%
    group_by(kin_cat.l) %>%
    summarise(Variable = var, Histogram = list(get(var)), .groups = "drop")
}))

# Merge summary stats with histogram data, properly selecting only relevant columns
final_table <- summary_table %>%
  left_join(hist_data, by = c("kin_cat.l", "Variable")) %>%
  select(kin_cat.l, Variable, min, max, mean, p50, Histogram)  # Ensure Histogram is correctly selected

# Apply custom labels to the `Variable` column
final_table <- final_table %>%
  mutate(Variable = recode(Variable,
                           "rel_tra" = "Geographic proximity <i>(1. Same building - 5. ≥ 1 hour)</i>",
                           "rel_cnt" = "Frequency of contact <i>(1. ≥ several times per week - 6. Never)</i>",
                           "rel_clo" = "Emotional closeness <i>(1. Not at all close - 5. Very close)</i>",
                           "rel_sup" = "Social support <i>(0. Did not support - 1. Supported)</i>",
                           "rel_mon1" = "Financial support <i>(0. Did not give money - 1. Gave money)</i>",
                           "rel_cnf" = "Conflict <i>(1. None at all - 4. A great deal)</i>"
                           
  ))

# Create gt table
gt_tbl <- final_table %>%
  gt(rowname_col = "Variable", groupname_col = "kin_cat.l", row_group_as_column = TRUE) %>%
  fmt_markdown() %>% 
  cols_label(
    min = "Min",
    max = "Max",
    mean = "Mean",
    p50 = "p50",
    Histogram = "Distribution"
  ) %>%
  cols_align(align = "center", columns = c(min, max, mean, p50, Histogram)) %>% 
  gt_plt_dist(column = "Histogram", type = "histogram", 
              fill = "black",
              bw = .5)

# Display table
gt_tbl






###### 03c HISTOGRAMS SHOWING CUT-OFFS  ####
folder.graph.model <- paste0(folder.graph.hi, model, "/")
ifelse(!dir.exists(folder.graph.model), dir.create(folder.graph.model), "Folder already exists")

folder.graph <- paste0(folder.graph.model, "cutoffs/")
ifelse(!dir.exists(folder.graph), dir.create(folder.graph), "Folder already exists")

w = 800
h = 500

###### BY KIN CATEGORY ####

## CONFLICT
# store number of !NA values 
n <- lca.df %>% 
  filter(kin_nam == 1,
         !is.na(rel_cnf)) %>% 
  nrow()

# draw plot by kin
cnf <- lca.df %>% 
  filter(kin_nam == 1,
         !is.na(rel_cnf)) %>% 
  ggplot(aes(rel_cnf.l, fill = factor(cnf_lca))) +
  geom_bar() +
  labs(x = "", 
       y = "Frequency",
       title = paste0("Conflict (n=", n,")")) +
  facet_wrap(~kin_cat.l, scale = "free_y") +
  scale_fill_brewer(name = "Cut-off ", 
                    labels = c("Below median", "Above median"),
                    palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = .9, vjust = 1),
        plot.title = element_text(size = 11))
png(file = paste0(folder.graph,"cnf_", kin.l,".png"), 
    width = w, height = h)
cnf
dev.off()


## CONTACT
n <- lca.df %>% 
  filter(kin_nam == 1,
         !is.na(rel_cnt)) %>% 
  nrow()
cnt <- lca.df %>% 
  filter(kin_nam == 1, !is.na(rel_cnt)) %>% 
  ggplot(aes(rel_cnt.l, fill = factor(cnt_lca))) +
  geom_bar() +
  labs(x = "", 
       y = "Frequency",
       title = paste0("Frequency of Contact (n=", n,")")) +
  facet_wrap(~kin_cat.l, scale = "free_y") +
  scale_fill_brewer(name = "Cut-off ", 
                    labels = c("Below median", "Above median"),
                    palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = .9, vjust = 1),
        plot.title = element_text(size = 11))
png(file = paste0(folder.graph,"cnt_", kin.l,".png"), 
    width = w, height = h)
cnt
dev.off()


## PROXIMITY
n <- lca.df %>% 
  filter(kin_nam == 1, !is.na(rel_tra)) %>% 
  nrow()
tra <- lca.df %>% 
  filter(kin_nam == 1, !is.na(rel_tra)) %>% 
  ggplot(aes(rel_tra.l, fill = factor(tra_lca))) +
  geom_bar() +
  labs(x = "", 
       y = "Frequency",
       title = paste0("Geographic Proximity (n=", n,")")) +
  facet_wrap(~kin_cat.l, scale = "free_y") +
  scale_fill_brewer(name = "Cut-off ", 
                    labels = c("Below median", "Above median"),
                    palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = .9, vjust = 1),
        plot.title = element_text(size = 11))
png(file = paste0(folder.graph,"tra_", kin.l,".png"), 
    width = w, height = h)
tra
dev.off()


## EMOTIONAL CLOSENESS
n <- lca.df %>% 
  filter(kin_nam == 1, !is.na(rel_clo)) %>% 
  nrow()
clo <- lca.df %>% 
  filter(kin_nam == 1, !is.na(rel_clo)) %>% 
  ggplot(aes(rel_clo.l, fill = factor(clo_lca))) +
  geom_bar() +
  labs(x = "", 
       y = "Frequency",
       title = paste0("Emotional Closeness (n=", n,")")) +
  facet_wrap(~kin_cat.l, scale = "free_y") +
  scale_fill_brewer(name = "Cut-off ", 
                    labels = c("Below median", "Above median"),
                    palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = .9, vjust = 1),
        plot.title = element_text(size = 11))
png(file = paste0(folder.graph,"clo_", kin.l,".png"), 
    width = w, height = h)
clo
dev.off()


## FINANCIAL SUPPORT
n <- lca.df %>% 
  filter(kin_nam == 1, !is.na(rel_mon1)) %>% 
  nrow()
mon <- lca.df %>% 
  filter(kin_nam == 1, !is.na(rel_mon1)) %>% 
  ggplot(aes(rel_mon1.l, fill = factor(mon_lca))) +
  geom_bar() +
  labs(x = "", 
       y = "Frequency",
       title = paste0("Money Given (n=", n,")")) +
  facet_wrap(~kin_cat.l, scale = "free_y") +
  scale_fill_brewer(name = "Cut-off \n (yes/no)", 
                    labels = c("Below median", "Above median"),
                    palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = .9, vjust = 1),
        plot.title = element_text(size = 11))
png(file = paste0(folder.graph,"mon_", kin.l,".png"), 
    width = w, height = h)
mon
dev.off()


## ADVICE GIVEN
n <- lca.df %>% 
  filter(kin_nam == 1, !is.na(rel_adv1)) %>% 
  nrow()
adv <- lca.df %>% 
  filter(kin_nam == 1, !is.na(rel_adv1)) %>% 
  mutate(n = nrow(!is.na(rel_adv1))) %>% 
  ggplot(aes(rel_adv1.l, fill = factor(adv_lca))) +
  geom_bar() +
  labs(x = "", 
       y = "Frequency",
       title = paste0("Advice Given (n=", n,")")) +
  facet_wrap(~kin_cat.l, scale = "free_y") +
  scale_fill_brewer(name = "Cut-off \n (yes/no)", 
                    labels = c("Below median", "Above median"),
                    palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = .9, vjust = 1),
        plot.title = element_text(size = 11))
png(file = paste0(folder.graph,"adv_", kin.l,".png"), 
    width = w, height = h)
adv
dev.off()


## COMFORTED
n <- lca.df %>% 
  filter(kin_nam == 1, !is.na(rel_cmf1)) %>% 
  nrow()
cmf <- lca.df %>% 
  filter(kin_nam == 1, !is.na(rel_cmf1)) %>% 
  mutate(n = nrow(!is.na(rel_cmf1))) %>% 
  ggplot(aes(rel_cmf1.l, fill = factor(cmf_lca))) +
  geom_bar() +
  labs(x = "", 
       y = "Frequency",
       title = paste0("Comforted (n=", n,")")) +
  facet_wrap(~kin_cat.l, scale = "free_y") +
  scale_fill_brewer(name = "Cut-off \n (yes/no)", 
                    labels = c("Below median", "Above median"),
                    palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = .9, vjust = 1),
        plot.title = element_text(size = 11))
png(file = paste0(folder.graph,"cmf_", kin.l,".png"), 
    width = w, height = h)
cmf
dev.off()


## COUNT ON
n <- lca.df %>% 
  filter(kin_nam == 1, !is.na(rel_cou1)) %>% 
  nrow()
cou <- lca.df %>% 
  filter(kin_nam == 1, !is.na(rel_cou1)) %>% 
  mutate(n = nrow(!is.na(rel_cou1))) %>% 
  ggplot(aes(rel_cou1.l, fill = factor(cou_lca))) +
  geom_bar() +
  labs(x = "", 
       y = "Frequency",
       title = paste0("Counted on (n=", n,")")) +
  facet_wrap(~kin_cat.l, scale = "free_y") +
  scale_fill_brewer(name = "Cut-off \n (yes/no)", 
                    labels = c("Below median", "Above median"),
                    palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = .9, vjust = 1),
        plot.title = element_text(size = 11))
png(file = paste0(folder.graph,"cou_", kin.l,".png"), 
    width = w, height = h)
cou
dev.off()

## Social Support
n <- lca.df %>% 
  filter(kin_nam == 1, !is.na(rel_sup)) %>% 
  nrow()
sup <- lca.df %>% 
  filter(kin_nam == 1, !is.na(rel_sup)) %>% 
  mutate(n = nrow(!is.na(rel_sup))) %>% 
  ggplot(aes(rel_sup.l, fill = factor(sup_lca))) +
  geom_bar() +
  labs(x = "", 
       y = "Frequency",
       title = paste0("Supported (n=", n,")")) +
  facet_wrap(~kin_cat.l, scale = "free_y") +
  scale_fill_brewer(name = "Cut-off \n (yes/no)", 
                    labels = c("Below median", "Above median"),
                    palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = .9, vjust = 1),
        plot.title = element_text(size = 11))
png(file = paste0(folder.graph,"sup_", kin.l,".png"), 
    width = w, height = h)
sup
dev.off()



###### OVER ALL KIN COMBINED ####

## CONFLICT
n <- lca.df %>% 
  filter(kin_nam == 1, !is.na(rel_cnf)) %>% 
  nrow()
cnf <- lca.df %>% 
  filter(kin_nam == 1, !is.na(rel_cnf)) %>% 
  ggplot(aes(rel_cnf.l, fill = factor(cnf_lca))) +
  geom_bar(color = "black") +
  labs(x = "", 
       y = "Frequency",
       title = paste0("Conflict (n=", n,")")) +
  scale_fill_brewer(name = "Cut-off", 
                    labels = c("Below median", "Above median"),
                    palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = .9, vjust = 1),
        plot.title = element_text(size = 11))
png(file = paste0(folder.graph,"cnf.png"), 
    width = w, height = h)
cnf
dev.off()


## CONTACT
n <- lca.df %>% 
  filter(kin_nam == 1, !is.na(rel_cnt)) %>% 
  nrow()
cnt <- lca.df %>% 
  filter(kin_nam == 1, !is.na(rel_cnt)) %>% 
  ggplot(aes(rel_cnt.l, fill = factor(cnt_lca))) +
  geom_bar(color = "black") +
  labs(x = "", 
       y = "Frequency",
       title = paste0("Frequency of Contact (n=", n,")")) +
  scale_fill_brewer(name = "Cut-off", 
                    labels = c("Below median", "Above median"),
                    palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = .9, vjust = 1),
        plot.title = element_text(size = 11))
png(file = paste0(folder.graph,"cnt.png"), 
    width = w, height = h)
cnt
dev.off()


## PROXIMITY
n <- lca.df %>% 
  filter(kin_nam == 1, !is.na(rel_tra)) %>% 
  nrow()
tra <- lca.df %>% 
  filter(kin_nam == 1, !is.na(rel_tra)) %>% 
  ggplot(aes(rel_tra.l, fill = factor(tra_lca))) +
  geom_bar(color = "black") +
  labs(x = "", 
       y = "Frequency",
       title = paste0("Geographic Proximity (n=", n,")")) +
  scale_fill_brewer(name = "Cut-off", 
                    labels = c("Below median", "Above median"),
                    palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = .9, vjust = 1),
        plot.title = element_text(size = 11))
png(file = paste0(folder.graph,"tra.png"), 
    width = w, height = h)
tra
dev.off()



## EMOTIONAL CLOSENESS
n <- lca.df %>% 
  filter(kin_nam == 1, !is.na(rel_clo)) %>% 
  nrow()
clo <- lca.df %>% 
  filter(kin_nam == 1, !is.na(rel_clo)) %>% 
  ggplot(aes(rel_clo.l, fill = factor(clo_lca))) +
  geom_bar(color = "black") +
  labs(x = "", 
       y = "Frequency",
       title = paste0("Emotional Closeness (n=", n,")")) +
  scale_fill_brewer(name = "Cut-off", 
                    labels = c("Below median", "Above median"),
                    palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = .9, vjust = 1),
        plot.title = element_text(size = 11))
png(file = paste0(folder.graph,"clo.png"), 
    width = w, height = h)
clo
dev.off()


## FINANCIAL SUPPORT
n <- lca.df %>% 
  filter(kin_nam == 1, !is.na(rel_mon1)) %>% 
  nrow()
mon <- lca.df %>% 
  filter(kin_nam == 1, !is.na(rel_mon1)) %>% 
  ggplot(aes(rel_mon1.l, fill = factor(mon_lca))) +
  geom_bar(color = "black") +
  labs(x = "", 
       y = "Frequency",
       title = paste0("Financial Support (n=", n,")")) +
  scale_fill_brewer(name = "Cut-off", 
                    labels = c("Below median", "Above median"),
                    palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = .9, vjust = 1),
        plot.title = element_text(size = 11))
png(file = paste0(folder.graph,"mon.png"), 
    width = w, height = h)
mon
dev.off()


## ADVICE RECEIVED
n <- lca.df %>% 
  filter(kin_nam == 1, !is.na(rel_adv1)) %>% 
  nrow()
adv <- lca.df %>% 
  filter(kin_nam == 1, !is.na(rel_adv1)) %>% 
  ggplot(aes(rel_adv1.l, fill = factor(adv_lca))) +
  geom_bar(color = "black") +
  labs(x = "", 
       y = "Frequency",
       title = paste0("Advice Given (n=", n,")")) +
  scale_fill_brewer(name = "Cut-off", 
                    labels = c("Below median", "Above median"),
                    palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = .9, vjust = 1),
        plot.title = element_text(size = 11))
png(file = paste0(folder.graph,"adv.png"), 
    width = w, height = h)
adv
dev.off()


## COMFORTED
n <- lca.df %>% 
  filter(kin_nam == 1, !is.na(rel_cmf1)) %>% 
  nrow()
cmf <- lca.df %>% 
  filter(kin_nam == 1, !is.na(rel_cmf1)) %>% 
  ggplot(aes(rel_cmf1.l, fill = factor(cmf_lca))) +
  geom_bar(color = "black") +
  labs(x = "", 
       y = "Frequency",
       title = paste0("Comforted (n=", n,")")) +
  scale_fill_brewer(name = "Cut-off", 
                    labels = c("Below median", "Above median"),
                    palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = .9, vjust = 1),
        plot.title = element_text(size = 11))
png(file = paste0(folder.graph,"cmf.png"), 
    width = w, height = h)
cmf
dev.off()


## COUNT ON
n <- lca.df %>% 
  filter(kin_nam == 1, !is.na(rel_cou1)) %>% 
  nrow()
cou <- lca.df %>% 
  filter(kin_nam == 1, !is.na(rel_cou1)) %>% 
  ggplot(aes(rel_cou1.l, fill = factor(cou_lca))) +
  geom_bar(color = "black") +
  labs(x = "", 
       y = "Frequency",
       title = paste0("Counted on (n=", n,")")) +
  scale_fill_brewer(name = "Cut-off", 
                    labels = c("Below median", "Above median"),
                    palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = .9, vjust = 1),
        plot.title = element_text(size = 11))
png(file = paste0(folder.graph,"cou.png"), 
    width = w, height = h)
cou
dev.off()


## Social Support
n <- lca.df %>% 
  filter(kin_nam == 1, !is.na(rel_sup)) %>% 
  nrow()
sup <- lca.df %>% 
  filter(kin_nam == 1, !is.na(rel_sup)) %>% 
  ggplot(aes(rel_sup.l, fill = factor(sup_lca))) +
  geom_bar(color = "black") +
  labs(x = "", 
       y = "Frequency",
       title = paste0("Social Support (n=", n,")")) +
  scale_fill_brewer(name = "Cut-off", 
                    labels = c("Below median", "Above median"),
                    palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = .9, vjust = 1),
        plot.title = element_text(size = 11))
png(file = paste0(folder.graph,"sup.png"), 
    width = w, height = h)
sup
dev.off()


## Show all plots in one graph

bars <- tra + cnt + clo + sup + mon + cnf +
  plot_layout(ncol = 3, guides = "collect")
png(file = paste0(folder.graph,"vardist.png"), 
    width = w, height = h)
bars
dev.off()



### IMPUTE MISSINGS ####
# Impute missing values in relationship indicators


print(paste0(sum(is.na(lca.df$rel_tra)), " observations (", round(sum(is.na(kin$rel_tra))/size*100, 2), "%) missing in DISTANCE"))
lca.df$rel_tra[is.na(lca.df$rel_tra)] <- 5 # 1 hour or longer
lca.df$rel_tra.l[EU$rel_tra.l == "-5. Incomplete data"] <- "5. 1 hour or longer" 
lca.df$rel_tra.l[EU$rel_tra.l == "-1. Don't know"] <- "5. 1 hour or longer" 
print(paste0(sum(is.na(lca.df$rel_tra)), " observations missing in DISTANCE after imputation"))



print(paste0(sum(is.na(lca.df$rel_cnt)), " observations (", round(sum(is.na(kin$rel_cnt))/size*100, 2), "%) missing in CONTACT"))
lca.df$rel_cnt[is.na(EU$rel_cnt)] <- 6 # never contact 
lca.df$rel_cnt.l[EU$rel_cnt.l == "-5. Incomplete data"] <- "6. Never" 
print(paste0(sum(is.na(lca.df$rel_cnt)), " observations missing in CONTACT after imputation"))



print(paste0(sum(is.na(lca.df$rel_clo)), " observations (", round(sum(is.na(kin$rel_clo))/size*100, 2), "%) missing in CLOSENESS"))
lca.df$rel_clo[is.na(EU$rel_clo)] <- 1 # not at all close
lca.df$rel_clo.l[EU$rel_clo.l == "-5. Incomplete data"] <- "1. Not at all close" 
print(paste0(sum(is.na(lca.df$rel_clo)), " observations missing in CLOSENESS after imputation"))


print(paste0(sum(is.na(lca.df$rel_adv1)), " observations (", round(sum(is.na(kin$rel_adv1))/size*100, 2), "%) missing in ADVICE"))
lca.df$rel_adv1[is.na(lca.df$rel_adv1)] <- 1 # not given
lca.df$rel_adv1.l[lca.df$rel_adv1.l == "-5. Incomplete data"] <- "1. Has not given advice" 
print(paste0(sum(is.na(lca.df$rel_adv1)), " observations missing in ADVICE after imputation"))

print(paste0(sum(is.na(kin$rel_cmf1)), " observations (", round(sum(is.na(kin$rel_cmf1))/size*100, 2), "%) missing in COMFORTED"))
lca.df$rel_cmf1[is.na(lca.df$rel_cmf1)] <- 1 # not given
lca.df$rel_cmf1.l[EU$rel_cmf1.l == "-5. Incomplete data"] <- "1. Did not comfort" 
print(paste0(sum(is.na(lca.df$rel_cmf1)), " observations missing in COMFORTED after imputation"))

print(paste0(sum(is.na(lca.df$rel_cou1)), " observations (", round(sum(is.na(kin$rel_cou1))/size*100, 2), "%) missing in COUNTED ON"))
lca.df$rel_cou1[is.na(lca.df$rel_cou1)] <- 1 # not count on
lca.df$rel_cou1.l[EU$rel_cou1.l == "-5. Incomplete data"] <- "1. Could not count on" 
print(paste0(sum(is.na(lca.df$rel_cou1)), " observations missing in COUNTED ON after imputation"))



print(paste0(sum(is.na(lca.df$rel_mon1)), " observations (", round(sum(is.na(kin$rel_mon1))/size*100, 2), "%) missing in MONEY"))
lca.df$rel_mon1[is.na(EU$rel_mon1)] <- 1 # not given
lca.df$rel_mon1.l[EU$rel_mon1.l == "-5. Incomplete data"] <- "1. Has not given money" 
print(paste0(sum(is.na(lca.df$rel_mon1)), " observations missing in MONEY after imputation"))



print(paste0(sum(is.na(lca.df$rel_cnf)), " observations (", round(sum(is.na(kin$rel_cnf))/size*100, 2), "%) missing in CONFLICT"))
lca.df$rel_cnf[is.na(EU$rel_cnf)] <- 1 # no conflict at all
lca.df$rel_cnf.l[EU$rel_cnf.l == "-5. Incomplete data"] <- "1. None at all" 
print(paste0(sum(is.na(lca.df$rel_cnf)), " observations missing in CONFLICT after imputation"))




# LAST LINE OF CODE #

