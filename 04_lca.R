## A typology of nuclear and extended family relations in Europe
## Bettina Hünteler
## 17.11.2025
## bhuenteler@diw.de


#### 04 RUN LCA ####

# Code based on: https://statistics.ohlsen-web.de/latent-class-analysis-polca/

# Define function (response ~ predictors) -> Relationship indicators to be included
f <- cbind(cnf_lca, cnt_lca, clo_lca, mon_lca, sup_lca, tra_lca)~1


# Loglinear independence model (nclass = 1)

k <- ""  # number of classes
m <- 3000 # maximum number iterations
r <- 15 # number of times to estimate models (nrep > 1 automates search for global max)

set.seed(240792)

k <- 1 # Loglinear independence model
lc1 <- poLCA(f, 
            lca.df, 
            nclass = k,
            maxiter = m,
            nrep = r,
            na.rm = FALSE) # retain cases with item missing (instead of case-wise deletion)

k <- 2
lc2 <- poLCA(f, 
             lca.df, 
             nclass = k,
             maxiter = m,
             nrep = r,
             na.rm = FALSE) 

k <- 3
lc3 <- poLCA(f, 
             lca.df, 
             nclass = k,
             maxiter = m,
             nrep = r,
             na.rm = FALSE) 

k <- 4
lc4 <- poLCA(f, 
             lca.df, 
             nclass = k,
             maxiter = m,
             nrep = r,
             na.rm = FALSE) 

k <- 5
lc5 <- poLCA(f, 
             lca.df, 
             nclass = k,
             maxiter = m,
             nrep = r,
             na.rm = FALSE) 

k <- 6
lc6 <- poLCA(f, 
             lca.df, 
             nclass = k,
             maxiter = m,
             nrep = r,
             na.rm = FALSE) 

k <- 7
lc7 <- poLCA(f, 
             lca.df, 
             nclass = k,
             maxiter = m,
             nrep = r,
             na.rm = FALSE) 


results <- data.frame(Model = c("Model 1"),
                      log_likelihood = lc1$llik,
                      df = lc1$resid.df,
                      BIC =lc1$bic,
                      ABIC =  (-2*lc1$llik) + ((log((lc1$N + 2)/24)) * lc1$npar),
                      CAIC = (-2*lc1$llik) + lc1$npar * (1 + log(lc1$N)), 
                      likelihood_ratio=lc1$Gsq)

results$Model<-as.integer(results$Model)

results[1,1] <- c("Model 1")
results[2,1] <- c("Model 2")
results[3,1] <- c("Model 3")
results[4,1] <- c("Model 4")
results[5,1] <- c("Model 5")
results[6,1] <- c("Model 6")
results[7,1] <- c("Model 7")
 
  
results[2,2] <- lc2$llik
results[3,2] <- lc3$llik
results[4,2] <- lc4$llik
results[5,2] <- lc5$llik
results[6,2] <- lc6$llik
results[7,2] <- lc7$llik
 
results[2,3] <- lc2$resid.df
results[3,3] <- lc3$resid.df
results[4,3] <- lc4$resid.df
results[5,3] <- lc5$resid.df
results[6,3] <- lc6$resid.df
results[7,3] <- lc7$resid.df
 
results[2,4] <- lc2$bic # BIC
results[3,4] <- lc3$bic
results[4,4] <- lc4$bic
results[5,4] <- lc5$bic
results[6,4] <- lc6$bic
results[7,4] <- lc7$bic
 
results[2,5] <- (-2*lc2$llik) + ((log((lc2$N + 2)/24)) * lc2$npar) #aBIC (adjusted BIC)
results[3,5] <- (-2*lc3$llik) + ((log((lc3$N + 2)/24)) * lc3$npar)
results[4,5] <- (-2*lc4$llik) + ((log((lc4$N + 2)/24)) * lc4$npar)
results[5,5] <- (-2*lc5$llik) + ((log((lc5$N + 2)/24)) * lc5$npar)
results[6,5] <- (-2*lc6$llik) + ((log((lc6$N + 2)/24)) * lc6$npar)
results[7,5] <- (-2*lc7$llik) + ((log((lc7$N + 2)/24)) * lc7$npar)
 
results[2,6] <-  (-2*lc2$llik) + lc2$npar * (1 + log(lc2$N)) #cAIC (consistent AIC)
results[3,6] <-  (-2*lc3$llik) + lc3$npar * (1 + log(lc3$N))
results[4,6] <-  (-2*lc4$llik) + lc4$npar * (1 + log(lc4$N))
results[5,6] <-  (-2*lc5$llik) + lc5$npar * (1 + log(lc5$N))
results[6,6] <-  (-2*lc6$llik) + lc6$npar * (1 + log(lc6$N))
results[7,6] <-  (-2*lc7$llik) + lc7$npar * (1 + log(lc7$N))

results[2,7] <- lc2$Gsq # likelihood-ratio
results[3,7] <- lc3$Gsq
results[4,7] <- lc4$Gsq
results[5,7] <- lc5$Gsq
results[6,7] <- lc6$Gsq
results[7,7] <- lc7$Gsq


# Entropy function (add to results)
entropy <- function (p) sum(-p*log(p))
results$R2_entropy

results[1,8] <- NA

error_prior <- entropy(lc2$P) # class proportions model 2
error_post <- mean(apply(lc2$posterior,1, entropy),na.rm = TRUE)
results[2,8] <- round(((error_prior-error_post) / error_prior),3)

error_prior<-entropy(lc3$P) # class proportions model 3
error_post<-mean(apply(lc3$posterior,1, entropy),na.rm = TRUE)
results[3,8]<-round(((error_prior-error_post) / error_prior),3)

error_prior<-entropy(lc4$P) # class proportions model 4
error_post<-mean(apply(lc4$posterior,1, entropy),na.rm = TRUE)
results[4,8]<-round(((error_prior-error_post) / error_prior),3)

error_prior<-entropy(lc5$P) # class proportions model 5
error_post<-mean(apply(lc5$posterior,1, entropy),na.rm = TRUE)
results[5,8]<-round(((error_prior-error_post) / error_prior),3)

error_prior<-entropy(lc6$P) # class proportions model 6
error_post<-mean(apply(lc6$posterior,1, entropy),na.rm = TRUE)
results[6,8]<-round(((error_prior-error_post) / error_prior),3)

error_prior<-entropy(lc7$P) # class proportions model 7
error_post<-mean(apply(lc7$posterior,1, entropy),na.rm = TRUE)
results[7,8]<-round(((error_prior-error_post) / error_prior),3)


## Relative Entropy (from Bauer, 2020 adjusted from Collins and Lanza, 2010)
# https://stackoverflow.com/questions/33000511/entropy-measure-polca-mplus

lca_entropy <- function(x) { 
  1 - ((sum(-x$posterior*log(x$posterior)))/(nrow(x$posterior)*log(ncol(x$posterior)))) 
  } 
lca_models <- list(lc2, lc3, lc4, lc5, lc6, lc7) 
entropy <- sapply(lca_models, FUN = lca_entropy) 
round(entropy, 2)

results[1,9] <- NA
results[2,9] <- round(entropy[1], 3)
results[3,9] <- round(entropy[2], 3)
results[4,9] <- round(entropy[3], 3)
results[5,9] <- round(entropy[4], 3)
results[6,9] <- round(entropy[5], 3)
results[7,9] <- round(entropy[6], 3)


# combining results to a dataframe (with Entropy as var lab)
colnames(results) <- c("Model","log-likelihood","resid. df","BIC","aBIC","cAIC","likelihood-ratio","Entropy", "relative Entropy")
lca_results <- results
print(results)


# Store LCA results as files to not having to re-run the models again
save(results, file = paste0(folder.graph.model,"results.RData"))
save(lc1, file = paste0(folder.graph.model,"lc1.RData"))
save(lc2, file = paste0(folder.graph.model,"lc2.RData"))
save(lc3, file = paste0(folder.graph.model,"lc3.RData"))
save(lc4, file = paste0(folder.graph.model,"lc4.RData"))
save(lc5, file = paste0(folder.graph.model,"lc5.RData"))
save(lc6, file = paste0(folder.graph.model,"lc6.RData"))
save(lc7, file = paste0(folder.graph.model,"lc7.RData"))


# LAST LINE OF CODE #
