## A typology of nuclear and extended family relations in the United States
## Bettina Hünteler
## 19.08.2024
## huenteler@wiso.uni-koeln.de



#https://bookdown.org/chua/ber642_advanced_regression/multinomial-logistic-regression.html
#https://stats.oarc.ucla.edu/r/dae/multinomial-logistic-regression/

#### 09 REGRESSION ####

# install.packages(c("nnet", "ggeffects", "marginaleffects", "car", "effects", "sandwich", "lmtest", "clusterSEs", "mclogit"))
library(nnet)
library(gtsummary)
library(magrittr)
library(broom)
library(ggeffects)
library(marginaleffects)
library(car)
library(kableExtra)
library(sandwich)
library(lmtest)
library(clusterSEs)
library(mlogit)
library(mclogit)

# load functions
# 1. to calculate robust SE (relationships clustered in respondents)
source(paste0(folder.code,"function_robustse.R"))
# 2. to display regression results in wide format
source(paste0(folder.code,"function_multinom_pivot_wider.R"))


# Set reference categories for regression
USA_lc$class.r <- relevel(USA_lc$class.l, ref = "Distant")

USA_lc$race.r  <- relevel(USA_lc$race.l,  ref = "White")
USA_lc$eth.r   <- relevel(USA_lc$eth.l,   ref = "Non-Hispanic White")
USA_lc$edu.r   <- relevel(USA_lc$edu.l,   ref = "Medium")


#### 09a MODEL ####

# Using mclogit

m1 <- mclogit(cbind(class.r, anc_id) ~ kin_cat.l, data = USA_lc)



# fit model without controls (SE not yet clustered -> differ from results in STATA; estimates do not differ, as would be expected)
bivar_l <- multinom(class.r ~ kin_cat.l, data = USA_lc, model = TRUE )
summary(bivar_l)
print(tidy(bivar_l, conf.int = TRUE), n=100)

bivar_s <- multinom(class.r ~ kin_cat_small, data = USA_lc, model = TRUE )
summary(bivar_s)
print(tidy(bivar_s, conf.int = TRUE), n=100)

# fit model with controls (SE not yet clustered -> differ from results in STATA; estimates do not differ, as would be expected)
full_l <-  multinom(class.r ~ kin_cat.l + anc_age + female.l + race.r + lfs.l + edu.r, data = USA_lc, model = TRUE)
summary(full_l)
print(tidy(full_l, conf.int = TRUE), n=100)


tidy(full_l, conf.int = TRUE) %>% 
  kable() %>% 
  kable_styling("basic", full_width = FALSE)
glance(full_l)


# multinom model tabulated with gtsummary in wide format
bivar_l_gt <-
  nnet::multinom(class.r ~ kin_cat_small + anc_age + female.l + race.r + mig.l + lfs.l + edu.r, data = USA_lc, model = TRUE) %>%
  tbl_regression(exponentiate = TRUE) %>%
  multinom_pivot_wider()
bivar_l_gt

full_l_gt <-
  nnet::multinom(class.r ~ kin_cat_small + anc_age + female.l + race.r + mig.l + lfs.l + edu.r, data = USA_lc, model = TRUE) %>%
  tbl_regression(exponentiate = TRUE) %>%
  multinom_pivot_wider()
full_l_gt

## BY HAND ESTIMATIONS (AUTOMATICALLY DONE WITH TIDY)
# z-scores for the model (Wald z) 
# z_b <- summary(bivar_l)$coefficients/summary(bivar_l)$standard.errors
# z_b
# 
# z_f <- summary(full_l)$coefficients/summary(full_l)$standard.errors
# z_f
# 
# # 2-tailed z test
# p_b <- round(((1- pnorm(abs(z_b), 0, 1)) * 2), digits = 4)
# p_b
# 
# p_f <- round(((1- pnorm(abs(z_f), 0, 1)) * 2), digits = 4)
# p_f

#### 09b MODEL FIT ####

# Check the predicted probability for each class
head(full_l$fitted.values,30)

# We can get the predicted result by use predict function
head(predict(full_l),30)

# Test the goodness of fit 
chisq.test(USA_lc$class.r,predict(full_l))

# Calculate Pseudo R2
# install.packages("DescTools")
library("DescTools")

PseudoR2(full_l, which = c("CoxSnell", "Nagelkerke", "McFadden"))


# Use the lmtest package to run Likelihood Ratio Tests
# Test if main effects are significant
# install.packages("lmtest")
library(lmtest)
lrtest(full_l, "group") # X2 = 27445, p = .000***
lrtest(full_l, "anc_age") # X2 = 86.176, p = .000***
lrtest(full_l, "female.l") # X2 = 197.33, p = .000***
# ...


#### 09c PARAMETER ESTIMATES ####

# extract the coefficients from the model and exponentiate -> relative log odds
exp(coef(full_l))

# predicted probabilities
head(pp <- fitted(full_l))

#https://bookdown.org/sarahwerth2024/CategoricalBook/multinomial-logit-regression-r.html#running-a-mlr-in-r

#### 9d PREDICTED PROBABILITIES
pprob_bivar_l <- ggeffect(bivar_l, terms = "kin_cat.l")
pprob <- ggeffect(full_l, terms = "group")
plot(pprob)

# Plotted as POINTS
ggplot(pprob_bivar_l,
       aes(x = x, 
           y = predicted, 
           color = response.level, 
           group = response.level)) +
  geom_point(size = 2, position = position_dodge(width=.5)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high,
                    color = response.level, group = response.level),
                width = .1, position = position_dodge(width=.5)) +
  scale_color_brewer(palette = "PuOr",
                     labels = c("Close",
                                "Ambivalent",
                                "Tightknit",
                                "Distant"),
                     name = "Class") +
  scale_x_discrete(limits = kin_order) +
  labs(
    x = "Kin type",
    y = "Predicted probability") +
  theme(
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))



# Plotted as BARS
ggplot(pprob_bivar_l,
       aes(x = x, 
           y = predicted, 
           fill = response.level, 
           group = response.level)) +
  geom_bar(stat = "identity", position = position_dodge(width=.95)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high,
                    color = response.level, group = response.level),
                width = .1, position = position_dodge(width=.95)) +
  scale_fill_brewer(palette = "PuOr",
                     labels = c("Close",
                                "Ambivalent",
                                "Tightknit",
                                "Distant"),
                     name = "Class") +
  scale_color_brewer(palette = "PuOr",
                    labels = c("Close",
                               "Ambivalent",
                               "Tightknit",
                               "Distant"),
                    name = "Class") +
  scale_x_discrete(limits = kin_order) +
  labs(
    x = "Kin type",
    y = "Predicted probability") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  )



# Plotting FULL MODEL
pprob_full_kin <- ggeffect(full_l, terms = "kin_cat.l")
# pprob_full <- ggeffect(full_l)
# plot(pprob)

# Plotted as POINTS
ggplot(pprob_full_kin,
       aes(x = x, y = predicted, color = response.level, group = response.level)) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    size = .3, position = position_jitter(width=.5)) +
  geom_text(
      aes(label = round(predicted, digits = 2), vjust = -0.25), 
      size = 3, show.legend = FALSE) +
  scale_color_brewer(palette = "PuOr",
                     labels = c("Close",
                                "Ambivalent",
                                "Tightknit",
                                "Distant"),
                     name = "Class") +
  scale_x_discrete(limits = kin_order) +
  labs(
    x = "Kin type",
    y = "Predicted probability") +
  theme(
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


# Plotted as BARS (dodged)
ggplot(pprob_full_kin,
       aes(x = x, y = predicted, fill = response.level, group = response.level)) +
  geom_bar(stat = "identity", position = position_dodge(width = .95)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high,
                    color = response.level, group = response.level),
                width = .1, position = position_dodge(width = .95)) +
  geom_text(aes(label = round(predicted, digits = 2), vjust = -2), 
            size = 2.5, show.legend = FALSE, position = position_dodge(width = .95)) +
  scale_fill_brewer(palette = "PuOr",
                    labels = c("Close",
                               "Ambivalent",
                               "Tightknit",
                               "Distant"),
                    name = "Class") +
  scale_color_brewer(palette = "PuOr",
                     labels = c("Close",
                                "Ambivalent",
                                "Tightknit",
                                "Distant"),
                     name = "Class") +
  scale_x_discrete(limits = kin_order) +
  labs(
    x = "Kin type",
    y = "Predicted probability") +
  theme(
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  ) 
