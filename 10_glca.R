## ---------------------------------------------------------------------------
## Testing measurement invariance of relationship-type classes across
## countries, using the R package glca (Kim, Jeon, Chang & Chung, 2022)
## --------------------------------------------------------------------------

# Country-specific poLCA

m <- 3000 # maximum number iterations
r <- 30 
f <- cbind(cnf_lca, cnt_lca, clo_lca, mon_lca, sup_lca, tra_lca) ~ 1
k = 5

set.seed(240792)

##### UK ####
uk <- lca.df %>% 
  filter(anc_cou.l == "1. UK")

lc_uk <- poLCA(f, 
               uk, 
             nclass = k,
             maxiter = m,
             nrep = r,
             na.rm = FALSE) 

probs_uk <- reshape2::melt(lc_uk$probs, level=2)

probs_uk %>% 
  filter(Var2 == "Pr(2)") %>% 
  ggplot(
    aes(x = factor(L2), 
        y = value, 
        colour = Var1, group = Var1, shape = Var1)) + 
  geom_line(size = .8) +
  geom_point(size = 4) +
  geom_text(
    aes(label = round(value, digits = 2)), 
    vjust = -1.5, size = 3, fontface = "bold", show.legend = FALSE, position = position_dodge(.4)) +
  labs(x = "Manifest items", 
       y = "Conditional item response probabilities") +
  scale_x_discrete(labels = c("clo_lca" = "Emotional\ncloseness",
                              "cnf_lca" = "Conflict",
                              "cnt_lca" = "Frequency\nof contact",
                              "mon_lca" = "Financial\nsupport",
                              "sup_lca" = "Social\nsupport",
                              "tra_lca" = "Geographic\nproximity"),
                   limits = c("tra_lca", "cnt_lca", "clo_lca", "sup_lca", "mon_lca", "cnf_lca")) +
  # increase font size of legend and spacing between labels
  theme(legend.text = element_text(
    lineheight = .8,
    size = 10), 
    legend.key.height = unit(.8, "cm")) +
  theme_minimal()

uk1 <- probs_uk %>% 
  filter(Var2 == "Pr(2)") %>%
  mutate(class = ifelse(Var1 == "class 1: ", "Tight-knit",
                        ifelse(Var1 == "class 2: ", "Connected-but-autonomous",
                               ifelse(Var1 == "class 3: ", "Intimate-but-distant",
                                      ifelse(Var1 == "class 4: ", "Disharmonious-but-supportive",
                                             ifelse(Var1 == "class 5: ", "Detached", "Error"))))),
         cntry = "UK") %>% 
  dplyr::select(cntry, class, L2, value)

uk1 %>% 
  ggplot(
    aes(x = factor(L2), 
        y = value, 
        colour = class, group = class, shape = class)) + 
  geom_line(size = .8) +
  geom_point(size = 4) +
  geom_text(
    aes(label = round(value, digits = 2)), 
    vjust = -1.5, size = 3, fontface = "bold", show.legend = FALSE, position = position_dodge(.4)) +
  labs(x = "Manifest items", 
       y = "Conditional item response probabilities") +
  scale_x_discrete(labels = c("clo_lca" = "Emotional\ncloseness",
                              "cnf_lca" = "Conflict",
                              "cnt_lca" = "Frequency\nof contact",
                              "mon_lca" = "Financial\nsupport",
                              "sup_lca" = "Social\nsupport",
                              "tra_lca" = "Geographic\nproximity"),
                   limits = c("tra_lca", "cnt_lca", "clo_lca", "sup_lca", "mon_lca", "cnf_lca")) +
  # increase font size of legend and spacing between labels
  theme(legend.text = element_text(
    lineheight = .8,
    size = 10), 
    legend.key.height = unit(.8, "cm")) +
  theme_minimal()


##### DE ####
de <- lca.df %>% 
  filter(anc_cou.l == "2. Germany")

lc_de <- poLCA(f, 
               de, 
               nclass = k,
               maxiter = m,
               nrep = r,
               na.rm = FALSE) 
probs_de <- reshape2::melt(lc_de$probs, level=2)

probs_de %>% 
  filter(Var2 == "Pr(2)") %>% 
  ggplot(
    aes(x = factor(L2), 
        y = value, 
        colour = Var1, group = Var1, shape = Var1)) + 
  geom_line(size = .8) +
  geom_point(size = 4) +
  geom_text(
    aes(label = round(value, digits = 2)), 
    vjust = -1.5, size = 3, fontface = "bold", show.legend = FALSE, position = position_dodge(.4)) +
  labs(x = "Manifest items", 
       y = "Conditional item response probabilities") +
  scale_x_discrete(labels = c("clo_lca" = "Emotional\ncloseness",
                              "cnf_lca" = "Conflict",
                              "cnt_lca" = "Frequency\nof contact",
                              "mon_lca" = "Financial\nsupport",
                              "sup_lca" = "Social\nsupport",
                              "tra_lca" = "Geographic\nproximity"),
                   limits = c("tra_lca", "cnt_lca", "clo_lca", "sup_lca", "mon_lca", "cnf_lca")) +
  # increase font size of legend and spacing between labels
  theme(legend.text = element_text(
    lineheight = .8,
    size = 10), 
    legend.key.height = unit(.8, "cm")) +
  theme_minimal() 


de1 <- probs_de %>% 
  filter(Var2 == "Pr(2)") %>%
  mutate(class = ifelse(Var1 == "class 5: ", "Tight-knit",
                        ifelse(Var1 == "class 1: ", "Connected-but-autonomous",
                               ifelse(Var1 == "class 3: ", "Intimate-but-distant",
                                      ifelse(Var1 == "class 2: ", "Disharmonious-but-supportive",
                                             ifelse(Var1 == "class 4: ", "Detached", "Error"))))),
         cntry = "DE") %>% 
  dplyr::select(cntry, class, L2, value)

##### PL ####
pl <- lca.df %>% 
  filter(anc_cou.l == "3. Poland")

lc_pl <- poLCA(f, 
               pl, 
               nclass = k,
               maxiter = m,
               nrep = r,
               na.rm = FALSE) 

probs_pl <- reshape2::melt(lc_pl$probs, level=2)

probs_pl %>% 
  filter(Var2 == "Pr(2)") %>% 
  ggplot(
    aes(x = factor(L2), 
        y = value, 
        colour = Var1, group = Var1, shape = Var1)) + 
  geom_line(size = .8) +
  geom_point(size = 4) +
  geom_text(
    aes(label = round(value, digits = 2)), 
    vjust = -1.5, size = 3, fontface = "bold", show.legend = FALSE, position = position_dodge(.4)) +
  labs(x = "Manifest items", 
       y = "Conditional item response probabilities") +
  scale_x_discrete(labels = c("clo_lca" = "Emotional\ncloseness",
                              "cnf_lca" = "Conflict",
                              "cnt_lca" = "Frequency\nof contact",
                              "mon_lca" = "Financial\nsupport",
                              "sup_lca" = "Social\nsupport",
                              "tra_lca" = "Geographic\nproximity"),
                   limits = c("tra_lca", "cnt_lca", "clo_lca", "sup_lca", "mon_lca", "cnf_lca")) +
  # increase font size of legend and spacing between labels
  theme(legend.text = element_text(
    lineheight = .8,
    size = 10), 
    legend.key.height = unit(.8, "cm")) +
  theme_minimal()

pl1 <- probs_pl %>% 
  filter(Var2 == "Pr(2)") %>%
  mutate(class = ifelse(Var1 == "class 2: ", "Tight-knit",
                        ifelse(Var1 == "class 5: ", "Connected-but-autonomous",
                               ifelse(Var1 == "class 3: ", "Intimate-but-distant",
                                      ifelse(Var1 == "class 4: ", "Disharmonious-but-supportive",
                                             ifelse(Var1 == "class 1: ", "Detached", "Error"))))),
         cntry = "PL") %>% 
  dplyr::select(cntry, class, L2, value)

#### IT ####
it <- lca.df %>% 
  filter(anc_cou.l == "4. Italy")

lc_it <- poLCA(f, 
               it, 
               nclass = k,
               maxiter = m,
               nrep = r,
               na.rm = FALSE) 

probs_it <- reshape2::melt(lc_it$probs, level=2)

probs_it %>% 
  filter(Var2 == "Pr(2)") %>% 
  ggplot(
    aes(x = factor(L2), 
        y = value, 
        colour = Var1, group = Var1, shape = Var1)) + 
  geom_line(size = .8) +
  geom_point(size = 4) +
  geom_text(
    aes(label = round(value, digits = 2)), 
    vjust = -1.5, size = 3, fontface = "bold", show.legend = FALSE, position = position_dodge(.4)) +
  labs(x = "Manifest items", 
       y = "Conditional item response probabilities") +
  scale_x_discrete(labels = c("clo_lca" = "Emotional\ncloseness",
                              "cnf_lca" = "Conflict",
                              "cnt_lca" = "Frequency\nof contact",
                              "mon_lca" = "Financial\nsupport",
                              "sup_lca" = "Social\nsupport",
                              "tra_lca" = "Geographic\nproximity"),
                   limits = c("tra_lca", "cnt_lca", "clo_lca", "sup_lca", "mon_lca", "cnf_lca")) +
  # increase font size of legend and spacing between labels
  theme(legend.text = element_text(
    lineheight = .8,
    size = 10), 
    legend.key.height = unit(.8, "cm")) +
  theme_minimal() 


it1 <- probs_it %>% 
  filter(Var2 == "Pr(2)") %>%
  mutate(class = ifelse(Var1 == "class 2: ", "Tight-knit",
                        ifelse(Var1 == "class 1: ", "Connected-but-autonomous",
                               ifelse(Var1 == "class 5: ", "Intimate-but-distant",
                                      ifelse(Var1 == "class 3: ", "Disharmonious-but-supportive",
                                             ifelse(Var1 == "class 4: ", "Detached", "Error"))))),
         cntry = "IT") %>% 
  dplyr::select(cntry, class, L2, value)


it1 %>% 
  ggplot(
    aes(x = factor(L2), 
        y = value, 
        colour = class, group = class, shape = class)) + 
  geom_line(size = .8) +
  geom_point(size = 4) +
  geom_text(
    aes(label = round(value, digits = 2)), 
    vjust = -1.5, size = 3, fontface = "bold", show.legend = FALSE, position = position_dodge(.4)) +
  labs(x = "Manifest items", 
       y = "Conditional item response probabilities") +
  scale_x_discrete(labels = c("clo_lca" = "Emotional\ncloseness",
                              "cnf_lca" = "Conflict",
                              "cnt_lca" = "Frequency\nof contact",
                              "mon_lca" = "Financial\nsupport",
                              "sup_lca" = "Social\nsupport",
                              "tra_lca" = "Geographic\nproximity"),
                   limits = c("tra_lca", "cnt_lca", "clo_lca", "sup_lca", "mon_lca", "cnf_lca")) +
  # increase font size of legend and spacing between labels
  theme(legend.text = element_text(
    lineheight = .8,
    size = 10), 
    legend.key.height = unit(.8, "cm")) +
  theme_minimal()



##### SE ####
se <- lca.df %>% 
  filter(anc_cou.l == "5. Sweden")

lc_se <- poLCA(f, 
               se, 
               nclass = k,
               maxiter = m,
               nrep = r,
               na.rm = FALSE) 

probs_se <- reshape2::melt(lc_se$probs, level=2)

probs_se %>% 
  filter(Var2 == "Pr(2)") %>% 
  ggplot(
    aes(x = factor(L2), 
        y = value, 
        colour = Var1, group = Var1, shape = Var1)) + 
  geom_line(size = .8) +
  geom_point(size = 4) +
  geom_text(
    aes(label = round(value, digits = 2)), 
    vjust = -1.5, size = 3, fontface = "bold", show.legend = FALSE, position = position_dodge(.4)) +
  labs(x = "Manifest items", 
       y = "Conditional item response probabilities") +
  scale_x_discrete(labels = c("clo_lca" = "Emotional\ncloseness",
                              "cnf_lca" = "Conflict",
                              "cnt_lca" = "Frequency\nof contact",
                              "mon_lca" = "Financial\nsupport",
                              "sup_lca" = "Social\nsupport",
                              "tra_lca" = "Geographic\nproximity"),
                   limits = c("tra_lca", "cnt_lca", "clo_lca", "sup_lca", "mon_lca", "cnf_lca")) +
  # increase font size of legend and spacing between labels
  theme(legend.text = element_text(
    lineheight = .8,
    size = 10), 
    legend.key.height = unit(.8, "cm")) +
  theme_minimal() 

se1 <- probs_se %>% 
  filter(Var2 == "Pr(2)") %>%
  mutate(class = ifelse(Var1 == "class 3: ", "Tight-knit",
                        ifelse(Var1 == "class 5: ", "Connected-but-autonomous",
                               ifelse(Var1 == "class 1: ", "Intimate-but-distant",
                                      ifelse(Var1 == "class 4: ", "Disharmonious-but-supportive",
                                             ifelse(Var1 == "class 2: ", "Detached", "Error"))))),
         cntry = "SE") %>% 
  dplyr::select(cntry, class, L2, value)


##### DK ####
dk <- lca.df %>% 
  filter(anc_cou.l == "6. Denmark")

lc_dk <- poLCA(f, 
               dk, 
               nclass = k,
               maxiter = m,
               nrep = r,
               na.rm = FALSE) 


probs_dk <- reshape2::melt(lc_dk$probs, level=2)

probs_dk %>% 
  filter(Var2 == "Pr(2)") %>% 
  ggplot(
    aes(x = factor(L2), 
        y = value, 
        colour = Var1, group = Var1, shape = Var1)) + 
  geom_line(size = .8) +
  geom_point(size = 4) +
  geom_text(
    aes(label = round(value, digits = 2)), 
    vjust = -1.5, size = 3, fontface = "bold", show.legend = FALSE, position = position_dodge(.4)) +
  labs(x = "Manifest items", 
       y = "Conditional item response probabilities") +
  scale_x_discrete(labels = c("clo_lca" = "Emotional\ncloseness",
                              "cnf_lca" = "Conflict",
                              "cnt_lca" = "Frequency\nof contact",
                              "mon_lca" = "Financial\nsupport",
                              "sup_lca" = "Social\nsupport",
                              "tra_lca" = "Geographic\nproximity"),
                   limits = c("tra_lca", "cnt_lca", "clo_lca", "sup_lca", "mon_lca", "cnf_lca")) +
  # increase font size of legend and spacing between labels
  theme(legend.text = element_text(
    lineheight = .8,
    size = 10), 
    legend.key.height = unit(.8, "cm")) +
  theme_minimal() 


dk1 <- probs_dk %>% 
  filter(Var2 == "Pr(2)") %>%
  mutate(class = ifelse(Var1 == "class 1: ", "Tight-knit",
                        ifelse(Var1 == "class 4: ", "Connected-but-autonomous",
                               ifelse(Var1 == "class 5: ", "Intimate-but-distant",
                                      ifelse(Var1 == "class 3: ", "Disharmonious-but-supportive",
                                             ifelse(Var1 == "class 2: ", "Detached", "Error"))))),
         cntry = "DK") %>% 
  dplyr::select(cntry, class, L2, value)


##### FI ####
fi <- lca.df %>% 
  filter(anc_cou.l == "7. Finland")

lc_fi <- poLCA(f, 
               fi, 
               nclass = k,
               maxiter = m,
               nrep = r,
               na.rm = FALSE) 

probs_fi <- reshape2::melt(lc_fi$probs, level=2)

probs_fi %>% 
  filter(Var2 == "Pr(2)") %>% 
  ggplot(
    aes(x = factor(L2), 
        y = value, 
        colour = Var1, group = Var1, shape = Var1)) + 
  geom_line(size = .8) +
  geom_point(size = 4) +
  geom_text(
    aes(label = round(value, digits = 2)), 
    vjust = -1.5, size = 3, fontface = "bold", show.legend = FALSE, position = position_dodge(.4)) +
  labs(x = "Manifest items", 
       y = "Conditional item response probabilities") +
  scale_x_discrete(labels = c("clo_lca" = "Emotional\ncloseness",
                              "cnf_lca" = "Conflict",
                              "cnt_lca" = "Frequency\nof contact",
                              "mon_lca" = "Financial\nsupport",
                              "sup_lca" = "Social\nsupport",
                              "tra_lca" = "Geographic\nproximity"),
                   limits = c("tra_lca", "cnt_lca", "clo_lca", "sup_lca", "mon_lca", "cnf_lca")) +
  # increase font size of legend and spacing between labels
  theme(legend.text = element_text(
    lineheight = .8,
    size = 10), 
    legend.key.height = unit(.8, "cm")) +
  theme_minimal() 


fi1 <- probs_fi %>% 
  filter(Var2 == "Pr(2)") %>%
  mutate(class = ifelse(Var1 == "class 5: ", "Tight-knit",
                        ifelse(Var1 == "class 2: ", "Connected-but-autonomous",
                               ifelse(Var1 == "class 3: ", "Intimate-but-distant",
                                      ifelse(Var1 == "class 1: ", "Disharmonious-but-supportive",
                                             ifelse(Var1 == "class 4: ", "Detached", "Error"))))),
         cntry = "FI") %>% 
  dplyr::select(cntry, class, L2, value)


##### NO ####
no <- lca.df %>% 
  filter(anc_cou.l == "8. Norway")

lc_no <- poLCA(f, 
               no, 
               nclass = k,
               maxiter = m,
               nrep = r,
               na.rm = FALSE) 

probs_no <- reshape2::melt(lc_no$probs, level=2)

probs_no %>% 
  filter(Var2 == "Pr(2)") %>% 
  ggplot(
    aes(x = factor(L2), 
        y = value, 
        colour = Var1, group = Var1, shape = Var1)) + 
  geom_line(size = .8) +
  geom_point(size = 4) +
  geom_text(
    aes(label = round(value, digits = 2)), 
    vjust = -1.5, size = 3, fontface = "bold", show.legend = FALSE, position = position_dodge(.4)) +
  labs(x = "Manifest items", 
       y = "Conditional item response probabilities") +
  scale_x_discrete(labels = c("clo_lca" = "Emotional\ncloseness",
                              "cnf_lca" = "Conflict",
                              "cnt_lca" = "Frequency\nof contact",
                              "mon_lca" = "Financial\nsupport",
                              "sup_lca" = "Social\nsupport",
                              "tra_lca" = "Geographic\nproximity"),
                   limits = c("tra_lca", "cnt_lca", "clo_lca", "sup_lca", "mon_lca", "cnf_lca")) +
  # increase font size of legend and spacing between labels
  theme(legend.text = element_text(
    lineheight = .8,
    size = 10), 
    legend.key.height = unit(.8, "cm")) +
  theme_minimal() 


no1 <- probs_no %>% 
  filter(Var2 == "Pr(2)") %>%
  mutate(class = ifelse(Var1 == "class 1: ", "Tight-knit",
                        ifelse(Var1 == "class 2: ", "Connected-but-autonomous",
                               ifelse(Var1 == "class 5: ", "Intimate-but-distant",
                                      ifelse(Var1 == "class 3: ", "Disharmonious-but-supportive",
                                             ifelse(Var1 == "class 4: ", "Detached", "Error"))))),
         cntry = "NO") %>% 
  dplyr::select(cntry, class, L2, value)


##### NL ####
nl <- lca.df %>% 
  filter(anc_cou.l == "9. Netherlands")

lc_nl <- poLCA(f, 
               nl, 
               nclass = k,
               maxiter = m,
               nrep = r,
               na.rm = FALSE) 

probs_nl <- reshape2::melt(lc_nl$probs, level=2)

probs_nl %>% 
  filter(Var2 == "Pr(2)") %>% 
  ggplot(
    aes(x = factor(L2), 
        y = value, 
        colour = Var1, group = Var1, shape = Var1)) + 
  geom_line(size = .8) +
  geom_point(size = 4) +
  geom_text(
    aes(label = round(value, digits = 2)), 
    vjust = -1.5, size = 3, fontface = "bold", show.legend = FALSE, position = position_dodge(.4)) +
  labs(x = "Manifest items", 
       y = "Conditional item response probabilities") +
  scale_x_discrete(labels = c("clo_lca" = "Emotional\ncloseness",
                              "cnf_lca" = "Conflict",
                              "cnt_lca" = "Frequency\nof contact",
                              "mon_lca" = "Financial\nsupport",
                              "sup_lca" = "Social\nsupport",
                              "tra_lca" = "Geographic\nproximity"),
                   limits = c("tra_lca", "cnt_lca", "clo_lca", "sup_lca", "mon_lca", "cnf_lca")) +
  # increase font size of legend and spacing between labels
  theme(legend.text = element_text(
    lineheight = .8,
    size = 10), 
    legend.key.height = unit(.8, "cm")) +
  theme_minimal() 


nl1 <- probs_nl %>% 
  filter(Var2 == "Pr(2)") %>%
  mutate(class = ifelse(Var1 == "class 3: ", "Tight-knit",
                        ifelse(Var1 == "class 4: ", "Connected-but-autonomous",
                               ifelse(Var1 == "class 5: ", "Intimate-but-distant",
                                      ifelse(Var1 == "class 2: ", "Disharmonious-but-supportive",
                                             ifelse(Var1 == "class 1: ", "Detached", "Error"))))),
         cntry = "NL") %>% 
  dplyr::select(cntry, class, L2, value)


##### pooled ####

lcmodel %>% 
  filter(Var2 == "Pr(2)") %>% 
  ggplot(
    aes(x = factor(L2), 
        y = value, 
        colour = Var1, group = Var1, shape = Var1)) + 
  geom_line(size = .8) +
  geom_point(size = 4) +
  geom_text(
    aes(label = round(value, digits = 2)), 
    vjust = -1.5, size = 3, fontface = "bold", show.legend = FALSE, position = position_dodge(.4)) +
  labs(x = "Manifest items", 
       y = "Conditional item response probabilities") +
  scale_x_discrete(labels = c("clo_lca" = "Emotional\ncloseness",
                              "cnf_lca" = "Conflict",
                              "cnt_lca" = "Frequency\nof contact",
                              "mon_lca" = "Financial\nsupport",
                              "sup_lca" = "Social\nsupport",
                              "tra_lca" = "Geographic\nproximity"),
                   limits = c("tra_lca", "cnt_lca", "clo_lca", "sup_lca", "mon_lca", "cnf_lca")) +
  # increase font size of legend and spacing between labels
  theme(legend.text = element_text(
    lineheight = .8,
    size = 10), 
    legend.key.height = unit(.8, "cm")) +
  theme_minimal()

pooled1 <- lcmodel %>% 
  filter(Var2 == "Pr(2)") %>%
  mutate(class = ifelse(Var1 == "class 5: ", "Tight-knit",
                        ifelse(Var1 == "class 3: ", "Connected-but-autonomous",
                               ifelse(Var1 == "class 4: ", "Intimate-but-distant",
                                      ifelse(Var1 == "class 2: ", "Disharmonious-but-supportive",
                                             ifelse(Var1 == "class 1: ", "Detached", "Error"))))),
         cntry = "Pooled") %>% 
  dplyr::select(cntry, class, L2, value)



#### Compare ####
compare <- pooled1 %>% 
  rbind(uk1, de1, pl1, it1, se1, dk1, fi1, no1, nl1)

plot_data <- compare %>%
  filter(class == "Tight-knit")

# split into "regular" countries vs. the pooled estimate,
# so pooled can be drawn last (i.e., on top of everything else)
plot_data_countries <- plot_data %>% filter(cntry != "Pooled")
plot_data_pooled     <- plot_data %>% filter(cntry == "Pooled")

ggplot(
  plot_data_countries,
  aes(x = factor(L2),
      y = value,
      colour = cntry, group = cntry)) +
  geom_line(size = .8) +
  geom_point(size = 2) +
  # pooled layers added last -> drawn on top of country lines
  geom_line(
    data = plot_data_pooled,
    aes(x = factor(L2), y = value, group = cntry),
    colour = "black", size = 1.3) +
  geom_point(
    data = plot_data_pooled,
    aes(x = factor(L2), y = value, group = cntry),
    colour = "black", size = 2.5) +
  geom_text(
    data = plot_data_pooled,
    aes(x = factor(L2), y = value, label = round(value, digits = 2)),
    vjust = -1.5, size = 3, fontface = "bold",
    colour = "black", show.legend = FALSE, position = position_dodge(.4)) +
  labs(x = "Manifest items",
       y = "Conditional item response probabilities",
       title = "Tight-knit") +
  scale_x_discrete(labels = c("clo_lca" = "Emotional\ncloseness",
                              "cnf_lca" = "Conflict",
                              "cnt_lca" = "Frequency\nof contact",
                              "mon_lca" = "Financial\nsupport",
                              "sup_lca" = "Social\nsupport",
                              "tra_lca" = "Geographic\nproximity"),
                   limits = c("tra_lca", "cnt_lca", "clo_lca", "sup_lca", "mon_lca", "cnf_lca")) +
  # theme_minimal() must come BEFORE the custom theme() tweaks below,
  # otherwise it would overwrite them
  theme_minimal() +
  theme(
    legend.text = element_text(lineheight = .8, size = 10),
    legend.key.height = unit(.8, "cm"),
    # extra padding all around so the title doesn't overlap the plot
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    plot.title = element_text(margin = margin(b = 15))
  )


plot_data <- compare %>%
  filter(class == "Intimate-but-distant")

# split into "regular" countries vs. the pooled estimate,
# so pooled can be drawn last (i.e., on top of everything else)
plot_data_countries <- plot_data %>% filter(cntry != "Pooled")
plot_data_pooled     <- plot_data %>% filter(cntry == "Pooled")

ggplot(
  plot_data_countries,
  aes(x = factor(L2),
      y = value,
      colour = cntry, group = cntry)) +
  geom_line(size = .8) +
  geom_point(size = 2) +
  # pooled layers added last -> drawn on top of country lines
  geom_line(
    data = plot_data_pooled,
    aes(x = factor(L2), y = value, group = cntry),
    colour = "black", size = 1.3) +
  geom_point(
    data = plot_data_pooled,
    aes(x = factor(L2), y = value, group = cntry),
    colour = "black", size = 2.5) +
  geom_text(
    data = plot_data_pooled,
    aes(x = factor(L2), y = value, label = round(value, digits = 2)),
    vjust = -1.5, size = 3, fontface = "bold",
    colour = "black", show.legend = FALSE, position = position_dodge(.4)) +
  labs(x = "Manifest items",
       y = "Conditional item response probabilities",
       title = "Intimate-but-distant") +
  scale_x_discrete(labels = c("clo_lca" = "Emotional\ncloseness",
                              "cnf_lca" = "Conflict",
                              "cnt_lca" = "Frequency\nof contact",
                              "mon_lca" = "Financial\nsupport",
                              "sup_lca" = "Social\nsupport",
                              "tra_lca" = "Geographic\nproximity"),
                   limits = c("tra_lca", "cnt_lca", "clo_lca", "sup_lca", "mon_lca", "cnf_lca")) +
  # theme_minimal() must come BEFORE the custom theme() tweaks below,
  # otherwise it would overwrite them
  theme_minimal() +
  theme(
    legend.text = element_text(lineheight = .8, size = 10),
    legend.key.height = unit(.8, "cm"),
    # extra padding all around so the title doesn't overlap the plot
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    plot.title = element_text(margin = margin(b = 15))
  )



plot_data <- compare %>%
  filter(class == "Connected-but-autonomous")

# split into "regular" countries vs. the pooled estimate,
# so pooled can be drawn last (i.e., on top of everything else)
plot_data_countries <- plot_data %>% filter(cntry != "Pooled")
plot_data_pooled     <- plot_data %>% filter(cntry == "Pooled")

ggplot(
  plot_data_countries,
  aes(x = factor(L2),
      y = value,
      colour = cntry, group = cntry)) +
  geom_line(size = .8) +
  geom_point(size = 2) +
  # pooled layers added last -> drawn on top of country lines
  geom_line(
    data = plot_data_pooled,
    aes(x = factor(L2), y = value, group = cntry),
    colour = "black", size = 1.3) +
  geom_point(
    data = plot_data_pooled,
    aes(x = factor(L2), y = value, group = cntry),
    colour = "black", size = 2.5) +
  geom_text(
    data = plot_data_pooled,
    aes(x = factor(L2), y = value, label = round(value, digits = 2)),
    vjust = -1.5, size = 3, fontface = "bold",
    colour = "black", show.legend = FALSE, position = position_dodge(.4)) +
  labs(x = "Manifest items",
       y = "Conditional item response probabilities",
       title = "Connected-but-autonomous") +
  scale_x_discrete(labels = c("clo_lca" = "Emotional\ncloseness",
                              "cnf_lca" = "Conflict",
                              "cnt_lca" = "Frequency\nof contact",
                              "mon_lca" = "Financial\nsupport",
                              "sup_lca" = "Social\nsupport",
                              "tra_lca" = "Geographic\nproximity"),
                   limits = c("tra_lca", "cnt_lca", "clo_lca", "sup_lca", "mon_lca", "cnf_lca")) +
  # theme_minimal() must come BEFORE the custom theme() tweaks below,
  # otherwise it would overwrite them
  theme_minimal() +
  theme(
    legend.text = element_text(lineheight = .8, size = 10),
    legend.key.height = unit(.8, "cm"),
    # extra padding all around so the title doesn't overlap the plot
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    plot.title = element_text(margin = margin(b = 15))
  )



plot_data <- compare %>%
  filter(class == "Detached")

# split into "regular" countries vs. the pooled estimate,
# so pooled can be drawn last (i.e., on top of everything else)
plot_data_countries <- plot_data %>% filter(cntry != "Pooled")
plot_data_pooled     <- plot_data %>% filter(cntry == "Pooled")

ggplot(
  plot_data_countries,
  aes(x = factor(L2),
      y = value,
      colour = cntry, group = cntry)) +
  geom_line(size = .8) +
  geom_point(size = 2) +
  # pooled layers added last -> drawn on top of country lines
  geom_line(
    data = plot_data_pooled,
    aes(x = factor(L2), y = value, group = cntry),
    colour = "black", size = 1.3) +
  geom_point(
    data = plot_data_pooled,
    aes(x = factor(L2), y = value, group = cntry),
    colour = "black", size = 2.5) +
  geom_text(
    data = plot_data_pooled,
    aes(x = factor(L2), y = value, label = round(value, digits = 2)),
    vjust = -1.5, size = 3, fontface = "bold",
    colour = "black", show.legend = FALSE, position = position_dodge(.4)) +
  labs(x = "Manifest items",
       y = "Conditional item response probabilities",
       title = "Detached") +
  scale_x_discrete(labels = c("clo_lca" = "Emotional\ncloseness",
                              "cnf_lca" = "Conflict",
                              "cnt_lca" = "Frequency\nof contact",
                              "mon_lca" = "Financial\nsupport",
                              "sup_lca" = "Social\nsupport",
                              "tra_lca" = "Geographic\nproximity"),
                   limits = c("tra_lca", "cnt_lca", "clo_lca", "sup_lca", "mon_lca", "cnf_lca")) +
  ylim(0, 1) +
  # theme_minimal() must come BEFORE the custom theme() tweaks below,
  # otherwise it would overwrite them
  theme_minimal() +
  theme(
    legend.text = element_text(lineheight = .8, size = 10),
    legend.key.height = unit(.8, "cm"),
    # extra padding all around so the title doesn't overlap the plot
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    plot.title = element_text(margin = margin(b = 15))
  )



plot_data <- compare %>%
  filter(class == "Disharmonious-but-supportive")

# split into "regular" countries vs. the pooled estimate,
# so pooled can be drawn last (i.e., on top of everything else)
plot_data_countries <- plot_data %>% filter(cntry != "Pooled")
plot_data_pooled     <- plot_data %>% filter(cntry == "Pooled")

ggplot(
  plot_data_countries,
  aes(x = factor(L2),
      y = value,
      colour = cntry, group = cntry)) +
  geom_line(size = .8) +
  geom_point(size = 2) +
  # pooled layers added last -> drawn on top of country lines
  geom_line(
    data = plot_data_pooled,
    aes(x = factor(L2), y = value, group = cntry),
    colour = "black", size = 1.3) +
  geom_point(
    data = plot_data_pooled,
    aes(x = factor(L2), y = value, group = cntry),
    colour = "black", size = 2.5) +
  geom_text(
    data = plot_data_pooled,
    aes(x = factor(L2), y = value, label = round(value, digits = 2)),
    vjust = -1.5, size = 3, fontface = "bold",
    colour = "black", show.legend = FALSE, position = position_dodge(.4)) +
  labs(x = "Manifest items",
       y = "Conditional item response probabilities",
       title = "Disharmonious-but-supportive") +
  scale_x_discrete(labels = c("clo_lca" = "Emotional\ncloseness",
                              "cnf_lca" = "Conflict",
                              "cnt_lca" = "Frequency\nof contact",
                              "mon_lca" = "Financial\nsupport",
                              "sup_lca" = "Social\nsupport",
                              "tra_lca" = "Geographic\nproximity"),
                   limits = c("tra_lca", "cnt_lca", "clo_lca", "sup_lca", "mon_lca", "cnf_lca")) +
  # theme_minimal() must come BEFORE the custom theme() tweaks below,
  # otherwise it would overwrite them
  theme_minimal() +
  theme(
    legend.text = element_text(lineheight = .8, size = 10),
    legend.key.height = unit(.8, "cm"),
    # extra padding all around so the title doesn't overlap the plot
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    plot.title = element_text(margin = margin(b = 15))
  )

### GLCA ####

# install.packages("glca")
library(glca)

# invert items because they are matched to poLCA logic (2 = y; 1 = n) 
# glca logic reads 1 = y; 2 = n

lca.df <- lca.df %>% 
  mutate(tra_lca2 = ifelse(tra_lca == 2, 1, 2),
         cnt_lca2 = ifelse(cnt_lca == 2, 1, 2),
         clo_lca2 = ifelse(clo_lca == 2, 1, 2),
         sup_lca2 = ifelse(sup_lca == 2, 1, 2),
         mon_lca2 = ifelse(mon_lca == 2, 1, 2),
         cnf_lca2 = ifelse(cnf_lca == 2, 1, 2))

# Items: same set as before
f <- item(tra_lca2, cnt_lca2, clo_lca2, sup_lca2, mon_lca2, cnf_lca2) ~ 1


k <- 5  # number of classes chosen from earlier (poLCA) enumeration step

## ---------------------------------------------------------------------------
## 1. Single-group baseline (pooled, ignoring country) - for reference only - constant over inits
## ---------------------------------------------------------------------------
lca_pooled <- glca(f, data = lca.df, nclass = k, n.init = 30, seed = 240792,
                   verbose = FALSE)
summary(lca_pooled)
# plot(lca_pooled)

## ---------------------------------------------------------------------------
## 2. Configural model: same number of classes per country, but item-response
##    probabilities AND class sizes estimated FREELY per country
##    (measure.inv = FALSE) -> check if 5 classes good option across all countries
## ---------------------------------------------------------------------------
mg_configural <- glca(f, group = anc_cou.l, data = lca.df, nclass = k,
                      measure.inv = FALSE, n.init = 30, seed = 240792,
                      verbose = FALSE)
summary(mg_configural)
# plot(mg_configural)

## ---------------------------------------------------------------------------
## 3. Fully measurement-invariant model: item-response probabilities
##    constrained EQUAL across countries; only class prevalence (class size)
##    is allowed to vary by country (measure.inv = TRUE, the default)
## ---------------------------------------------------------------------------
mg_invariant <- glca(f, group = anc_cou.l, data = lca.df, nclass = k,
                     measure.inv = TRUE, n.init = 30, seed = 240792,
                     verbose = FALSE)
summary(mg_invariant)
# plot(mg_invariant)

## ---------------------------------------------------------------------------
## 4. Formal test of measurement invariance:
##    chi-square likelihood-ratio test comparing the invariant (constrained,
##    nested) model against the configural (unconstrained) model
## ---------------------------------------------------------------------------
gofglca(lca_pooled, mg_invariant, mg_configural, test = "chisq")

# A significant chi-square difference (mg_configural vs mg_invariant) indicates
# that full measurement invariance does NOT hold, i.e. the meaning of the
# relationship-type classes is not directly comparable across countries.
# In that case, proceed to partial invariance (step 5).

## ---------------------------------------------------------------------------
## 5. Diagnosing which items are non-invariant
##    Compare item-response probabilities per country from the configural
##    model to see where they diverge most (a descriptive first pass)
## ---------------------------------------------------------------------------
configural_probs <- mg_configural$param$rho   # list of item-response probs by group
print(configural_probs)

# Optionally: visualize per-country class profiles
plot(mg_configural, "all")

## A more formal partial-invariance search: free one item at a time from the
## measurement-invariance constraint, refit, and test improvement in fit via
## chi-square LRT against the fully invariant model. Repeat, freeing the item
## that most improves fit, until remaining lack of fit is acceptable or all
## substantial non-invariance has been located. Example for one item
## ("cnf_lca") freed across groups while all others remain invariant:
##
## NOTE: glca's measure.inv argument is currently an all-or-nothing switch
## (TRUE/FALSE for the whole item set), so testing PARTIAL invariance with
## glca requires manually constructing item-specific multi-group comparisons,
## e.g. by fitting separate single-item-relaxed models via parameter
## restrictions, or by using Latent GOLD / Mplus (which natively support
## item-by-item invariance constraints) for that more granular step. A
## practical compromise in R: fit the configural model, inspect which items'
## response probabilities differ most by country in `configural_probs`
## (step 5 above), and treat those items as flagged for cautious
## interpretation or exclusion from country comparisons, even without a
## formal item-level chi-square test for each one.

## ---------------------------------------------------------------------------
## 6. If (partial) invariance is supported: substantive comparison
##    Once you are satisfied that item-response probabilities are
##    (approximately) invariant, country differences can be meaningfully
##    interpreted as differences in class PREVALENCE (how common each
##    relationship type is) rather than differences in what the classes mean.
## ---------------------------------------------------------------------------
summary(mg_invariant)        # country-specific class proportions
plot(mg_invariant)           # pooled (invariant) item-response profiles