## Loading packages
library(future)
library(future.apply)
library(parallel)
library(hdrm)
library(glmnet)
library(Matrix)


##*******************************************************************
## ------------------ Function to generate the data  ----------------------
##*******************************************************************
##

generate_data <- function(n_samp, rho) {
  sim  <- gen_data(
    n    = n_samp,
    p    = 20,
    p1   = 5,
    beta = c(0.5/3, 1/3, 1.5/3, 2/3, 2.5/3, rep(0, 15)),
    corr = "exchangeable",
    rho  = rho
  )
  # Combine X matrix and y vector into a single data frame
  dat <- as.data.frame(sim$X)
  dat$y <- sim$y
  return(dat)   
}

##*******************************************************************
## ------------------ Function to do backwards p-value selection  ----------------------
##*******************************************************************
##

backward_pval <- function(data, crit = 0.05) {
  
  predictors <- setdiff(names(data), "y")
  
  for (i in 1:length(predictors)) {
    
    fit <- lm(as.formula(paste("y ~", paste(predictors, collapse = "+"))), 
              data = data)
    
    parms <- as.data.frame(summary(fit)$coefficients)
    parms$parm <- rownames(parms)
    parms <- parms[parms$parm != "(Intercept)", ]
    
    worst_row <- parms[which.max(parms[, 4]), ]
    
    if (worst_row[, 4] > crit) {
      predictors <- setdiff(predictors, worst_row$parm)
      
      if (length(predictors) == 0) break
      
    } else {
      break
    }
  }
  
  if (length(predictors) > 0) {
    lm(as.formula(paste("y ~", paste(predictors, collapse = "+"))), data = data)
  } else {
    lm(y ~ 1, data = data)  
  }
}  


##*******************************************************************
## ------------------ Function to do backwards AIC selection  ----------------------
##*******************************************************************
##

backward_AIC <- function(data) {
  step(
    lm(y ~ ., data = data),
    direction = "backward",
    trace     = 0,
    k         = 2
  )
}


##*******************************************************************
## ------------------ Function to do backwards BIC selection  ----------------------
##*******************************************************************
##

backward_BIC <- function(data) {
  step(
    lm(y ~ ., data = data),
    direction = "backward",
    trace     = 0,
    k         = log(nrow(data))
  )
}

##*******************************************************************
## ------------------ Function to run lasso  ----------------------
##*******************************************************************
##

lasso_min <- function(data) {
  x <- as.matrix(data[, setdiff(names(data), "y")])
  y <- data$y
  
  cv_fit <- cv.glmnet(x, y, alpha = 1, nfolds = 10, standardize = TRUE)
  
  # extract non-zero coefficients (excluding intercept)
  coefs     <- coef(cv_fit, s = "lambda.min")
  selected  <- rownames(coefs)[which(coefs != 0)]
  selected  <- selected[selected != "(Intercept)"]
  
  # refit with lm to return same class as backward_AIC/BIC/pval
  if (length(selected) > 0) {
    lm(as.formula(paste("y ~", paste(selected, collapse = "+"))), data = data)
  } else {
    lm(y ~ 1, data = data)
  }
}

lasso_1se <- function(data) {
  x <- as.matrix(data[, setdiff(names(data), "y")])
  y <- data$y
  
  cv_fit <- cv.glmnet(x, y, alpha = 1, nfolds = 10, standardize = TRUE)
  
  # extract non-zero coefficients (excluding intercept)
  coefs     <- coef(cv_fit, s = "lambda.1se")
  selected  <- rownames(coefs)[which(coefs != 0)]
  selected  <- selected[selected != "(Intercept)"]
  
  # refit with lm to return same class as backward_AIC/BIC/pval
  if (length(selected) > 0) {
    lm(as.formula(paste("y ~", paste(selected, collapse = "+"))), data = data)
  } else {
    lm(y ~ 1, data = data)
  }
}

##*******************************************************************
## ------------------ Function to run elastic net  ----------------------
##*******************************************************************
##

enet_min <- function(data) {
  x <- as.matrix(data[, setdiff(names(data), "y")])
  y <- data$y
  
  cv_fit <- cv.glmnet(x, y, alpha = 0.5, nfolds = 10, standardize = TRUE)
  
  # extract non-zero coefficients (excluding intercept)
  coefs     <- coef(cv_fit, s = "lambda.min")
  selected  <- rownames(coefs)[which(coefs != 0)]
  selected  <- selected[selected != "(Intercept)"]
  
  # refit with lm to return same class as backward_AIC/BIC/pval
  if (length(selected) > 0) {
    lm(as.formula(paste("y ~", paste(selected, collapse = "+"))), data = data)
  } else {
    lm(y ~ 1, data = data)
  }
}

enet_1se <- function(data) {
  x <- as.matrix(data[, setdiff(names(data), "y")])
  y <- data$y
  
  cv_fit <- cv.glmnet(x, y, alpha = 0.5, nfolds = 10, standardize = TRUE)
  
  # extract non-zero coefficients (excluding intercept)
  coefs     <- coef(cv_fit, s = "lambda.1se")
  selected  <- rownames(coefs)[which(coefs != 0)]
  selected  <- selected[selected != "(Intercept)"]
  
  # refit with lm to return same class as backward_AIC/BIC/pval
  if (length(selected) > 0) {
    lm(as.formula(paste("y ~", paste(selected, collapse = "+"))), data = data)
  } else {
    lm(y ~ 1, data = data)
  }
}

##*******************************************************************
## ------------------ Function to run all  ----------------------
##*******************************************************************
##



run_all <- function(n_samp, rho) {
  data <- generate_data(n_samp, rho)
  
  list(
    pval  = backward_pval(data),
    aic = backward_AIC(data),
    bic = backward_BIC(data),
    lasso_min = lasso_min(data),
    lasso_1se = lasso_1se(data),
    enet_min = enet_min(data),
    enet_1se = enet_1se(data)
  )
}

##*******************************************************************
## ------------------ Function to refit all in lm  ----------------------
##*******************************************************************
##

get_selected_vars <- function(fit) {
  names(coef(fit))[-1]  
}

refit_all <- function(n_samp, rho) {
  
  data <- generate_data(n_samp, rho)
  fits <- run_all(n_samp, rho)
  
  refit_model <- function(fit) {
    
    vars <- names(coef(fit))[-1]
    
    if (length(vars) == 0) {
      return(lm(y ~ 1, data = data))
    }
    
    formula <- reformulate(vars, response = "y")
    
    lm(formula, data = data)
  }
  
  list(
    pval      = refit_model(fits$pval),
    aic       = refit_model(fits$aic),
    bic       = refit_model(fits$bic),
    lasso_min = refit_model(fits$lasso_min),
    lasso_1se = refit_model(fits$lasso_1se),
    enet_min  = refit_model(fits$enet_min),
    enet_1se  = refit_model(fits$enet_1se)
  )
}


##*******************************************************************
## ------------------ Extract model info (bias, CI, etc.) ------------
##*******************************************************************
##
true_beta <- c(0.5/3, 1/3, 1.5/3, 2/3, 2.5/3, rep(0, 15))
names(true_beta) <- sprintf("V%02d", 1:20)

extract_model_info <- function(fit) {
  
  if (!inherits(fit, "lm")) {
    stop("Input must be an lm object")
  }
  
  mod_summ <- summary(fit)$coefficients
  ci <- confint(fit)
  
  vars <- rownames(mod_summ)
  vars <- vars[vars != "(Intercept)"]
  
  out <- data.frame(
    var = vars,
    est = mod_summ[vars, "Estimate"],
    se  = mod_summ[vars, "Std. Error"],
    p   = mod_summ[vars, "Pr(>|t|)"],
    low = ci[vars, 1],
    high = ci[vars, 2]
  )
  
  rownames(out) <- NULL
  
  out$true <- true_beta[out$var]
  out$bias <- out$est - out$true
  out$cover <- (out$low <= out$true & out$high >= out$true)
  
  out
}

##*******************************************************************
## ------------------ Simulation (1000 runs) ----------------------
##*******************************************************************
##

evaluate_run <- function(n_samp, rho) {
  
  fits <- refit_all(n_samp, rho)
  
  results <- lapply(fits, function(fit) {
    extract_model_info(fit)
   })
   
   results
}

set.seed(6642)

all_results <- future_lapply(1:1000, function(n_sim) {
  evaluate_run(250, 0)
}, future.seed = TRUE)

##*******************************************************************
## ------------------ SIMULATION (500 runs) ----------------------
##*******************************************************************
##

models <- names(all_results[[1]])
p <- 20
noise_vars <- names(true_beta)[6:20]
alpha <- 0.05

type1_array <- array(
  0,
  dim = c(1000, p, length(models)),
  dimnames = list(
    NULL,
    sprintf("V%02d", 1:20),
    models
  )
)

for (b in 1:1000) {
  for (m in models) {
    
    res <- all_results[[b]][[m]]
    
    # res contains all variables in the refitted model
    for (j in 1:nrow(res)) {
      
      var  <- res$var[j]
      pval <- res$p[j]
      
      # joint event: variable appears in model AND is significant
      if (!is.na(var) && var %in% sprintf("V%02d", 1:20)) {
        type1_array[b, var, m] <- as.integer(pval < alpha)
      }
    }
  }
}


type1_per_var <- apply(
  type1_array[, noise_vars, , drop = FALSE],
  c(2, 3),
  mean
)

type1_avg <- colMeans(type1_per_var)

type1_avg


##*******************************************************************
## ------------------ Running all  ----------------------
##*******************************************************************
##

set.seed(6642)

# Case 1a: N=250, independent
case_1a <- run_all(n_samp = 250, rho = 0)

# Case 1b: N=250, correlated  <-- set rho with your group (e.g. 0.5)
case_1b <- run_all(n_samp = 250, rho = 0.35)

# Case 2a: N=500, independent
case_2a <- run_all(n_samp = 500, rho = 0)

# Case 2b: N=500, correlated
case_2b <- run_all(n_samp = 500, rho = 0.35)