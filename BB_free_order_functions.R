# =========================================================================== #
# R functions for the adapted free-order model selection for a BB model, from the paper
# The function 'fbselectBB' selects the polynomial degrees of the norm-predictor for all parameters, given a random effects structure on mu
# "Adjusting for non-representativeness in continuous norming using multilevel-regression and post-stratification".

# Authors: Klazien de Vries, Marieke E. Timmerman, Anja F. Ernst, and Casper J. Albers 
# Version: 19-11-2024
# =========================================================================== #

library(gamlss); 
library(foreign)
library(gamlss.tr)

#' Evaluates a string
str_eval <- function(x) {return(eval(parse(text=x), envir = parent.frame()))} 

#' Fits a BB distribution with certain polynomial degrees for mu and sigma
#' 
#' @param mydata : sample on which to fit the distribution
#' @param d_mu : polynomial degree for mu
#' @param d_sigma : polynomial degree for sigma
#' @param re_structure : string, specifying a random effects structure, e.g. "+random(Education)" or "+random(Education)+random(Sex)" or "+random(Education:Sex)"
#' @return BIC of a fitted gamlss model
get_model <- function(mydata,d_mu,d_sigma, re_structure){
  # set mod_degree to 1 (constant) if polynomial degree is 0
  if (d_mu == 0){
    mod_mu_degree <- "1"
  } else {
    mod_mu_degree <- paste("poly(X, ", d_mu,")", sep = "")
  }
  if (d_sigma == 0){
    mod_sigma_degree <- "1"
  } else {
    mod_sigma_degree <- paste("poly(X, ", d_sigma,")", sep = "")
  }
 
    tryCatch({mod <- str_eval(paste("gamlss::gamlss(Score ~ ",mod_mu_degree, re_structure,", sigma.formula = ~ ",mod_sigma_degree,", family = 'BB', data = mydata, method = RS(1000), pb = max_score, trace = FALSE)", sep = ""))
    return(mod)}, warning= function(w)
    {print(w)
      mod <- str_eval(paste("gamlss::gamlss(Score ~ ",mod_mu_degree, re_structure,", sigma.formula = ~ ",mod_sigma_degree,", family = 'BB', data = mydata, method = CG(1000), pb = max_score, trace = FALSE)", sep = ""))
      return(mod)},
    error=function(e) {
      print(e)
      mod <- str_eval(paste("gamlss::gamlss(Score ~ ",mod_mu_degree, re_structure,",sigma.formula = ~ ",mod_sigma_degree,", family = 'BB', data = mydata, method = CG(1000), pb = max_score, trace = FALSE)", sep = ""))
      return(mod)})  
}


#' Obtain BIC for a GAMLSS model with BB distribution with certain polynomial degrees for mu and sigma
#' 
#' @param mydata : sample on which to fit the distribution
#' @param d_mu : polynomial degree for mu
#' @param d_sigma : polynomial degree for sigma
#' @param re_structure : string, specifying a random effects structure, e.g. "+random(Education)" or "+random(Education)+random(Sex)" or "+random(Education:Sex)"
#' @return a fitted gamlss model
get_bic <- function(mydata, d_mu,d_sigma, re_structure){
  # set mod_degree to 1 (constant) if polynomial degree is 0
  if (d_mu == 0){
    mod_mu_degree <- "1"
  } else {
    mod_mu_degree <- paste("poly(X, ", d_mu,")", sep = "")
  }
  if (d_sigma == 0){
    mod_sigma_degree <- "1"
  } else {
    mod_sigma_degree <- paste("poly(X, ", d_sigma,")", sep = "")
  }

  tryCatch({mod <- str_eval(paste("gamlss::gamlss(Score ~ ",mod_mu_degree,re_structure,", sigma.formula = ~ ",mod_sigma_degree,", family = 'BB', data = mydata, method = RS(1000), pb = max_score, trace = FALSE)", sep = ""))
  mod_bic <- BIC(mod)
  return(mod_bic)}, warning= function(w)
    {print(w)
      mod_bic <- Inf
      return(mod_bic)},
    error=function(e) {
      mod_bic <- Inf
      return(mod_bic)}) 
  }


#' Free order model selection procedure for BB distribution (forward backward selection)
#' 
#' @param mydata : sample on which to fit the distribution, contains the scores, ages, and adjustment variables
#' @param score : test scores
#' @param age : ages
#' @param max_score : maximum test score
#' @param re_structure : string, specifying a random effects structure, e.g. "+random(Education)" or "+random(Education)+random(Sex)" or "+random(Education:Sex)"
#' @return selected BB model

fbselectBB <- function(mydata, score, age, max_score, re_structure){
  mydata$Score <- cbind(score, max_score-score)
  mydata$X <- age
  # set starting polynomial degrees
  i.mu <-2
  i.sigma <-1
  # compute BIC of current model
  i.bic <- get_bic(mydata, i.mu,i.sigma, re_structure)
  best.new.bic <- i.bic -1
  
  tryCatch({while (best.new.bic < i.bic){
    i.bic <- get_bic(mydata, i.mu,i.sigma, re_structure)
    # candidate next models
    if(i.mu >=1){
      new.mu <- c(i.mu-1,i.mu+1)
    } else {
      new.mu <- c(i.mu+1)}
    if(i.sigma >=1){
      new.sigma <- c(i.sigma-1,i.sigma+1)
    } else {
      new.sigma <- c(i.sigma+1)}
    
    new.musigma <- rbind(expand.grid(mu = i.mu, sigma = new.sigma),expand.grid(mu = new.mu,sigma = i.sigma))
    new.bics <- mapply(get_bic, new.musigma$mu, new.musigma$sigma, MoreArgs = list(mydata = mydata, re_structure =re_structure))
    names(new.bics) <- 1:nrow(new.musigma)
    best.new.bic <- min(unlist(new.bics))
    
    if (best.new.bic < i.bic){
      id.best <- as.numeric(names(which.min(unlist(new.bics))))
      i.mu <- new.musigma$mu[id.best]
      i.sigma <- new.musigma$sigma[id.best]
    }
  }}, error=function(e) {
    print(e)
  })
  best_model <- get_model(mydata,i.mu,i.sigma, re_structure)
  return(best_model)
}

