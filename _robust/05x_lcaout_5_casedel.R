## A typology of nuclear and extended family relations in the United States
## Bettina Hünteler
## 23.02.2024
## huenteler@wiso.uni-koeln.de

#### 05 ANALYSE LCA OUTPUT (model fit + response probs) ####

## 05a FIT INDICES

# Store table in word
set_flextable_defaults(font.family = "Times New Roman", 
                       text.align = "center", part = "body")
ftab <- flextable(results) 
ftab <- set_caption(ftab, caption = paste0("Classification Criteria Over Models (", kin.L,")"))
save_as_docx(ftab, path = paste0(folder.graph.model,"critLCA_",kin.l,".docx"))



## 05b ELBOW PLOT ####

# Convert results to long format by criteria
results2 <- gather(results, Criteria, Value, 4:8)
results2$Value <- round(results2$Value, digits = 2)

fit.plot  <-
  ggplot(results2) + 
  geom_point(aes(x=Model,y=Value),size=3) +
  geom_line(aes(Model, Value, group = 1)) +
  labs(x = "", y="", title = paste0("Elbow Plot for ",kin.L)) + 
  facet_grid(Criteria ~. , scales = "free") +
  theme(panel.grid.major.x = element_blank() ,
        panel.grid.major.y = element_line(colour="grey", size=0.5),
        legend.title = element_text(size = 16, face = 'bold'),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 16),
        legend.text =  element_text(size=16),
        axis.line = element_line(colour = "black")) # Achsen etwas dicker


png(file = paste0(folder.graph.hi, paste0("elbow_",kin.l,".png")), 
    width = w, height = h)
fit.plot
dev.off()



## 05c SELECT PREFERRED LCA SOLUTION ####

# Which model is best?
best <- "lc5"

# (ADJUST HERE TO CHOICE!!!)
post <- as.character(c("Class 1", "Class 2", "Class 3", "Class 4", "Class 5")) %>%  
  bind_cols(c(round(colMeans(lc5$posterior)*100,2)))

# Population shares of classes
print(post)

# Store best model (ADJUST HERE TO CHOICE!!!)
lcmodel <- reshape2::melt(lc5$probs, level=2)

# Set corresponding dir for graphs
# folder.graph <- paste0(folder.graph.hi, best, "_output/")
folder.graph <- paste0(folder.graph.model, best, "_output/")
ifelse(!dir.exists(folder.graph), dir.create(folder.graph), "Folder already exists")


## 05d RESPONSE PROBABILITIES ####

# ...for interpreting the classes

# Conditional Item Response Probabilities
# Stacked Bar Plot
png(file = paste0(folder.graph, paste0("condprop_",kin.l,"_", best, ".png")), 
    width = w, height = h)
ggplot(lcmodel,aes(x = L2, y = value, fill = Var2)) + 
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = round(value, digits = 2)), position = position_stack(.1), size = 3) +
  facet_grid(Var1 ~ .) +
  scale_fill_brewer(type = "seq", palette = "YlGnBu")  +
  labs(x = "Manifest items",y="Share of responses", fill ="", 
       title = paste0("Conditional item response probabilities (",kin.L,")")) +
  theme(axis.text.y=element_blank(),
                    axis.ticks.y=element_blank(),                    
                    panel.grid.major.y=element_blank()) +
  guides(fill = guide_legend(reverse=TRUE))
dev.off()


# Class labels
class.lab <- as.character(c("1. Class",
                            "2. Class", 
                            "3. Class",
                            "4. Class",
                            "5. Class"))

# Profile Plot
png(file = paste0(folder.graph, paste0("latprof_",kin.l,"_", best, ".png")), 
    width = 800, height = 500)
lcmodel %>% 
  filter(Var2 == "Pr(2)") %>% 
  ggplot(aes(x = factor(L2), y = value, colour = Var1, group = Var1, shape = Var1)) + 
  geom_line(size = .8) +
  geom_point(size = 3) +
  geom_text(aes(label = round(value, digits = 2)), vjust = -2, size = 3) +
  labs(x = "Manifest items", y = "Conditional item response probabilities", 
       title = paste0("Latent Profile Plot (",kin.L,", ", model, ")")) +
  scale_color_discrete(name = "Latent Classes", 
                       labels = class.lab) +
  scale_shape_discrete(name = "Latent Classes", 
                       labels = class.lab)
dev.off()



