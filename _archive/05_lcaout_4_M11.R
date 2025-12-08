## A typology of nuclear and extended family relations in the United States
## Bettina Hünteler
## 23.02.2024
## huenteler@wiso.uni-koeln.de

#### 05 ANALYSE LCA OUTPUT (model fit + response probs) ####

model <- "M11"

load(paste0(folder.graph.hi,model,"/results.RData"))
load(paste0(folder.graph.hi,model,"/lc1.RData"))
load(paste0(folder.graph.hi,model,"/lc2.RData"))
load(paste0(folder.graph.hi,model,"/lc3.RData"))
load(paste0(folder.graph.hi,model,"/lc4.RData"))
load(paste0(folder.graph.hi,model,"/lc5.RData"))
load(paste0(folder.graph.hi,model,"/lc6.RData"))
load(paste0(folder.graph.hi,model,"/lc7.RData"))

folder.graph.model <- paste0(folder.graph.hi, model, "/")
ifelse(!dir.exists(folder.graph.model), dir.create(folder.graph.model), "Folder already exists")




# Which model is best?
best <- "lc4"
lc <- lc4



# Set corresponding dir for graphs
# folder.graph <- paste0(folder.graph.hi, best, "_output/")
folder.graph <- paste0(folder.graph.model, best, "_output/")
ifelse(!dir.exists(folder.graph), dir.create(folder.graph), "Folder already exists")



## 05a FIT INDICES


# Store table in word
set_flextable_defaults(font.family = "Times New Roman", 
                       text.align = "center", part = "body")
ftab <- flextable(results) 
ftab <- set_caption(ftab, caption = paste0("Classification Criteria Over Models (", kin.L,")"))
save_as_docx(ftab, path = paste0(folder.graph.model,"critLCA_",kin.l,".docx"))



## Classification criteria
# Average posterior latent class probability (should be over .8 -> Bauer 2020, p. 260)

avepp <- 
  round(aggregate(x = lc$posterior, by = list(lc$predclass), FUN = "mean"), 2)
colnames(avepp) <- c("Class","1","2","3","4")

ftab <- flextable(avepp)
ftab <- set_caption(ftab, caption = paste0("Average Posterior Latent Class Probability"))
ftab <- footnote(ftab, 
                 i = c(1, 2, 3, 4),
                 j = c(2, 3, 4, 5),
                 ref_symbols = "a",
                 value = as_paragraph("Note: Values ≥.90 are ideal; values <.80 are unacceptable (see Weller et al. 2020)"))
save_as_docx(ftab, path = paste0(folder.graph,"avepp_",kin.l,".docx"))


## 05b ELBOW PLOT ####

# Convert results to long format by criteria
results2 <- gather(results, Criteria, Value, 4:8)
results2$Value <- round(results2$Value, digits = 2)
 
fit.plot  <-
  ggplot(results2) + 
  geom_point(aes(x=Model,y=Value),size=3) +
  geom_line(aes(Model, Value, group = 1)) +
  labs(x = "", y="", title = paste0("Elbow Plot for ",kin.L," (",model,")")) + 
  facet_grid(Criteria ~. , scales = "free") +
  theme(panel.grid.major.x = element_blank() ,
        panel.grid.major.y = element_line(colour="grey", size=0.5),
        legend.title = element_text(size = 16, face = 'bold'),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 16),
        legend.text =  element_text(size=16),
        axis.line = element_line(colour = "black")) # Achsen etwas dicker


png(file = paste0(folder.graph.model, paste0("elbow_",kin.l,".png")), 
    width = w, height = h)
fit.plot
dev.off()



## 05c SELECT PREFERRED LCA SOLUTION ####

# Across probabilities of class membership --> can differ from predicted classes (based on modal value)
post <- as.character(c("Class 1", "Class 2", "Class 3", "Class 4")) %>%  
  bind_cols(c(round(colMeans(lc$posterior)*100,2)))

# Population shares of classes
print(post)

# Estimated class memberships
round(prop.table(table(lc$predclass)),4)*100

# Store best model (ADJUST HERE TO CHOICE!!!)
lcmodel <- reshape2::melt(lc$probs, level=2)

# lcmodel_m11 <- lcmodel




## 05d RESPONSE PROBABILITIES ####

# ...for interpreting the classes

# Class labels
class.lab <- as.character(c("Tightknit",
                            "Close", 
                            "Distant",
                            "Ambivalent"))

class.lab.p <- as.character(c(paste0("Tightknit (", post[1,2], "%)"),
                            paste0("Close (", post[2,2], "%)"),
                            paste0("Distant (", post[3,2], "%)"),
                            paste0("Ambivalent (", post[4,2], "%)")))
class.lab4 <- class.lab
lc4_m11 <- lc4





# Conditional Item Response Probabilities
# Stacked Bar Plot
png(file = paste0(folder.graph, paste0("condprop_",kin.l,"_", best, ".png")), 
    width = w, height = h)
ggplot(lcmodel,aes(x = L2, y = value, fill = Var2)) + 
  geom_bar(
    stat = "identity", position = "stack") +
  geom_text(
    aes(label = round(value, digits = 2)), position = position_stack(.1), size = 3) +
  facet_grid(Var1 ~ .) +
  scale_fill_brewer(type = "seq", 
                    palette = "YlGnBu",
                    labels = c("No", "Yes"), 
                    name = "Response")  +
  labs(
    x = "Manifest items",
    y = "Probability", 
    fill ="", 
    title = paste0("Conditional item response probabilities (",kin.L,", ",model,")")) + 
  scale_x_discrete(labels = c("sup_lca" = "Support",
                              "clo_lca" = "Closeness",
                              "cnf_lca" = "Conflict",
                              "cnt_lca" = "Contact",
                              "mon_lca" = "Money"),
                   limits = c("sup_lca", "cnt_lca", "clo_lca", "cnf_lca", "mon_lca")) +
  theme(
    axis.text.y=element_blank(), 
    axis.ticks.y=element_blank(),
    panel.grid.major.y=element_blank()) +
  guides(fill = guide_legend(reverse=TRUE))
dev.off()





# Profile Plot
png(file = paste0(folder.graph, paste0("latprof_",kin.l,"_", best, ".png")), 
    width = 800, height = 500)
lcmodel %>% 
  filter(Var2 == "Pr(2)") %>% 
ggplot(
  aes(x = factor(L2), 
      y = value, 
      colour = Var1, group = Var1, shape = Var1)) + 
  geom_line(size = .8) +
  geom_point(size = 3) +
  geom_text(
    aes(label = round(value, digits = 2)), 
    vjust = -2, size = 3, fontface = "bold", show.legend = FALSE, position = position_dodge(.4)) +
  labs(x = "Manifest items", 
       y = "Conditional item response probabilities", 
       title = paste0("Latent Class Analysis (",kin.L,", ", model, ")")) +
  scale_x_discrete(labels = c("sup_lca" = "Support",
                              "clo_lca" = "Closeness",
                              "cnf_lca" = "Conflict",
                              "cnt_lca" = "Contact",
                              "mon_lca" = "Money"),
                   limits = c("sup_lca", "cnt_lca", "clo_lca", "cnf_lca", "mon_lca")) +
  scale_color_brewer(name = "Latent Classes", 
                       labels = class.lab,
                     palette = "Paired") +
  scale_shape_discrete(name = "Latent Classes", 
                       labels = class.lab) 
dev.off()

# As Bars (by relationship indicators)
png(file = paste0(folder.graph, paste0("latprof_bars_",kin.l,"_", best, ".png")), 
    width = 800, height = 500)
lcmodel %>% 
  filter(Var2 == "Pr(2)") %>% 
ggplot(aes(x = factor(L2), y = value, fill = Var1, group = Var1)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(value, digits = 2)), 
            position = position_dodge(.9), vjust = -1.2, size = 2.5) +
  labs(x = "Manifest items", y = "Conditional item response probabilities", 
       title = paste0("Latent Class Analysis (",kin.L,", ", model, ")")) +
  scale_x_discrete(labels = c("sup_lca" = "Support",
                              "clo_lca" = "Closeness",
                              "cnf_lca" = "Conflict",
                              "cnt_lca" = "Contact",
                              "mon_lca" = "Money"),
                   limits = c("sup_lca", "cnt_lca", "clo_lca", "cnf_lca", "mon_lca")) +
  scale_fill_brewer(name = "Latent Classes", 
                      labels = class.lab,
                      palette = "Paired") 
dev.off()

# As Bars (by classes)
png(file = paste0(folder.graph, paste0("latprof_byclass_",kin.l,"_", best, ".png")), 
    width = 800, height = 500)
lcmodel %>% 
  filter(Var2 == "Pr(2)") %>% 
  ggplot(
    aes(x = factor(Var1), 
        y = value, 
        fill = L2)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(value, digits = 2)), 
            position = position_dodge(.9), vjust = -1.2, size = 2.5, show.legend = FALSE) +
  labs(x = "Latent classes", 
       y = "Conditional item response probabilities", 
       title = paste0("Latent Class Analysis (",kin.L,", ", model, ")")) +
  scale_x_discrete(labels = class.lab.p) +
  scale_fill_brewer(name = "Relationship indicators", 
                    labels = c("sup_lca" = "Support",
                               "clo_lca" = "Closeness",
                               "cnf_lca" = "Conflict",
                               "cnt_lca" = "Contact",
                               "mon_lca" = "Money"),
                    limits = c("sup_lca", "cnt_lca", "clo_lca", "cnf_lca", "mon_lca"),
                    palette = "PuOr") +
  theme(
    axis.title = element_text(size = 12))
dev.off()

