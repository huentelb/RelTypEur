# https://cimentadaj.github.io/blog/2016-09-19-obtaining-robust-standard-errors-and-odds-ratios/obtaining-robust-standard-errors-and-odds-ratios-for-logistic-regression-in-r/

# This function estimates robust standad error for glm objects and
# returns coefficients as either logit, odd ratios or probabilities.
# logits are default
# argument x must be glm model.


# Credit to Achim here:
# http://stackoverflow.com/questions/27367974/
# different-robust-standard-errors-of-logit-regression-in-stata-and-r
# for the code in line 14 and 15

robustse <- function(x, coef = c("logit", "odd.ratio", "probs")) {
  suppressMessages(suppressWarnings(library(lmtest)))
  suppressMessages(suppressWarnings(library(sandwich)))
  
  sandwich1 <- function(object, ...) sandwich(object) *
    nobs(object) / (nobs(object) - 1)
  # Function calculates SE's
  mod1 <- coeftest(x, vcov = sandwich1) 
  # apply the function over the variance-covariance matrix
  
  if (coef == "logit") {
    return(mod1) # return logit with robust SE's
  } else if (coef == "odd.ratio") {
    mod1[, 1] <- exp(mod1[, 1]) # return odd ratios with robust SE's
    mod1[, 2] <- mod1[, 1] * mod1[, 2]
    return(mod1)
  } else {
    mod1[, 1] <- (mod1[, 1]/4) # return probabilites with robust SE's
    mod1[, 2] <- mod1[, 2]/4
    return(mod1)
  }
}