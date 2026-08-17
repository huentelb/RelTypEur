## A typology of nuclear and extended family relations in Europe
## Bettina Hünteler
## 17.11.2025
## bhuenteler@diw.de

#### 05 ANALYSE LCA OUTPUT (model fit + response probs) ####

# model <- "M11_median"

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




# Which model is best? (Store for automated selection in following code and labels, etc.)
best <- "lc5"
lc <- lc5


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
# Average posterior latent class probability

avepp <- 
  round(aggregate(x = lc$posterior, by = list(lc$predclass), FUN = "mean"), 2)
colnames(avepp) <- c("Class","1","2","3","4", "5")

ftab <- flextable(avepp)
ftab <- set_caption(ftab, caption = paste0("Average Posterior Latent Class Probability"))
ftab <- footnote(ftab, 
                 i = c(1, 2, 3, 4, 5),
                 j = c(2, 3, 4, 5, 6),
                 ref_symbols = "a",
                 value = as_paragraph("Note: Values ≥.90 are ideal; values <.80 (see Weller et al. 2020) or <.70 are unacceptable (see Maysn, 2013)"))
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
post <- as.character(c("Class 1", "Class 2", "Class 3", "Class 4", "Class 5")) %>%  
  bind_cols(c(round(colMeans(lc$posterior)*100,1)))

# Population shares of classes
print(post)

# Estimated class memberships -> this is based on modal class
round(prop.table(table(lc$predclass)),4)*100

# Store best model (ADJUST HERE TO CHOICE!!!)
lcmodel <- reshape2::melt(lc$probs, level=2)

# lcmodel_m11 <- lcmodel







## 05d RESPONSE PROBABILITIES ####

# ...for interpreting the classes

# Class labels
class.lab <- as.character(c("Tight-knit",
                            "Connected-but-\nautonomous", 
                            "Disharmonious-\nbut-supportive",
                            "Intimate-but-distant",
                            "Detached"))

# attention: shares must be adjusted to class order if not 1-5!
class.lab.p <- as.character(c(paste0("Tight-knit (", post[5,2], "%)"),
                              paste0("Connected-but-\nautonomous (", post[3,2], "%)"),
                              paste0("Disharmonious-\nbut-supportive (", post[2,2], "%)"),
                              paste0("Intimate-but-distant (", post[4,2], "%)"),
                              paste0("Detached (", post[1,2], "%)")))



# Conditional Item Response Probabilities
# Stacked Bar Plot

w = 800
h = 500

# Add values from US study
#C1: Detached
#C2: Disharm
#C3: Connected
#C4: Intimate
#C5: Tight

#              det, dis, con, int, tigh 
US_values <- c(.18, .87, .28, .09, .30, # cnf
               .01, .63, .85, .34, 1.0, # cnt
               .03, .08, .55, .63, .96, # clo
               .00, .25, .08, .08, .35, # mon
               .06, .59, .08, .77, .97, # sup
               .12, .56, .56, .25, .58  # tra
              )

lcmodel_US <- lcmodel %>% 
  filter(Var2 == "Pr(2)") %>% 
  cbind(US_values)

# change order of bars
lcmodel_US$L2 <- factor(lcmodel_US$L2,
                        levels = c("tra_lca", "cnt_lca", "clo_lca", "sup_lca", "mon_lca", "cnf_lca"))


png(file = paste0(folder.graph, paste0("latprof_",kin.l,"_", best, "_UScomp.png")), 
    width = 800, height = 500)
lcmodel_US %>%
  ggplot(aes(x = factor(Var1),
             y = value,
             fill = L2)) +
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black") +
  geom_text(aes(label = gsub("^0\\.", ".", round(value, digits = 2))),
            position = position_dodge(.9), 
            vjust = -1.5, 
            size = 3.3, 
            show.legend = FALSE) +
  geom_point(
    aes(x = factor(Var1),
        y = US_values),
    position = position_dodge(width = .9),
    size = 2.5,
    color = "black",
    shape = 18,
    show.legend = FALSE
  ) +
  labs(x = "Class",
       y = "Conditional item response probabilities") +
  scale_x_discrete(labels = class.lab,
                   limits = c("class 5: ",
                              "class 3: ",
                              "class 2: ",
                              "class 4: ",
                              "class 1: ")) +
  scale_y_continuous(limits = c(0, 1.12),
                     breaks = seq(0, 1, .25),
                     expand = expansion(mult = c(0, 0))) +
  scale_fill_brewer(type = "div", palette = "RdBu",
                    name = "Relationship\nindicators",
                    labels = c("clo_lca" = "Emotional closeness",
                               "cnf_lca" = "Conflict",
                               "cnt_lca" = "Frequency of contact",
                               "mon_lca" = "Financial support",
                               "sup_lca" = "Social support",
                               "tra_lca" = "Geographic proximity")) +
  theme(axis.title.x = element_text(size = 13, margin = margin(t = 12)),
        axis.title.y = element_text(size = 13, margin = margin(r = 12)),
        axis.text = element_text(size = 12, color = "black"),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))
dev.off()

