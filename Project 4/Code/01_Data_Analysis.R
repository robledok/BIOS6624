## Loading packages
library(future)
library(future.apply)
library(parallel)
library(hdrm)
library(glmnet)
library(Matrix)

##*******************************************************************
## ------------------ Function to for one simulation ----------------------
##*******************************************************************
##

simulate_func <- function(n, rho){
  # Define true betas
  true_beta <- c(0.5/3, 1/3, 1.5/3, 2/3, 2.5/3, rep(0, 15))
  # Generate the data
  sim_dat <- gen_data(n = n,
                      p = 20,
                      p1 = 5,
                      beta = true_beta,
                      family = 'gaussian',
                      corr = 'exchangeable',
                      rho = rho
  ) 
  # Putting the data into a dataframe
  dat <- data.frame(y = sim_dat$y, sim_dat$X)
  # Fitting the full model
  full_mod <- lm(y ~ ., data = dat)
  
  # ---------------------------
  # Backward selection by p-values
  # ---------------------------
  pval_selec <- step(full_mod,
                     direction = 'backward',
                     trace = 0,
                     k =  qchisq(1 - 0.05, 1)
  )
  # Creating results dataframe for p-value selection (from Carter's notes)
  pval_res <- harvest(model = pval_selec,  # result model object from the reduction
                      betas   = true_beta,       # simulation target betas
                      alpha   = 0.05        # criteria for coverage and error computation
  )
  pval_res$method <- 'p-value'
  pval_res$n      <- n
  pval_res$rho    <- rho
  
  # ---------------------------
  # Backward selection by AIC
  # ---------------------------
  aic_selec <- step(full_mod,
                     direction = 'backward',
                     trace = 0,
                     k =  2
  )
  # Creating results dataframe for AIC selection
  aic_res <- harvest(model = aic_selec,  
                      betas   = true_beta,       
                      alpha   = 0.05       
  )
  aic_res$method <- 'AIC'
  aic_res$n      <- n
  aic_res$rho    <- rho
  
  # ---------------------------
  # Backward selection by BIC
  # ---------------------------
  bic_selec <- step(full_mod,
                    direction = 'backward',
                    trace = 0,
                    k =  log(nrow(dat))
  )
  # Creating results dataframe for BIC selection
  bic_res <- harvest(model = bic_selec,  
                     betas   = true_beta,       
                     alpha   = 0.05       
  )
  bic_res$method <- 'AIC'
  bic_res$n      <- n
  bic_res$rho    <- rho
  
  # ---------------------------
  # LASSO
  # ---------------------------
  x <- as.matrix(dat[, -1])
  y <- dat$y
  
  # Now doing cross validation
  lasso_cv <- cv.glmnet(x, y, alpha = 1, nfolds = 10, standardize = TRUE, family = "gaussian")
  # Fitting model using minimum lambda
  lasso_min_fit <- glmnet(
    x,
    y,
    alpha = 1,
    lambda = lasso_cv$lambda.min,
    family = "gaussian"
  )
  # Selected variables
  lasso_min_coef <- coef(lasso_min_fit)
  lasso_selected_min_vars <- rownames(lasso_min_coef)[which(lasso_min_coef != 0)]
  lasso_selected_min_vars <- lasso_selected_min_vars[lasso_selected_min_vars != "(Intercept)"]
  # Refit OLS on selected variables for inference
  if(length(lasso_selected_min_vars) > 0){
    
    lasso_min_formula <- as.formula(
      paste("y ~", paste(lasso_selected_min_vars, collapse = " + "))
    )
    
    lasso_min_refit <- lm(lasso_min_formula, data = dat)
    
  } else {
    
    lasso_min_refit <- lm(y ~ 1, data = dat)
  }
  # Harvest results
  lasso_min_res <- harvest(
    model = lasso_min_refit,
    betas = true_beta,
    alpha = 0.05
  )
  
  lasso_min_res$method <- "LASSO Minimum"
  lasso_min_res$n      <- n
  lasso_min_res$rho    <- rho
  
  # Fitting model using 1 SE lambda
  lasso_1se_fit <- glmnet(
    x,
    y,
    alpha = 1,
    lambda = lasso_cv$lambda.1se,
    family = "gaussian"
  )
  # Selected variables
  lasso_1se_coef <- coef(lasso_1se_fit)
  lasso_selected_1se_vars <- rownames(lasso_1se_coef)[which(lasso_1se_coef != 0)]
  lasso_selected_1se_vars <- lasso_selected_1se_vars[lasso_selected_1se_vars != "(Intercept)"]
  # Refit OLS on selected variables for inference
  if(length(lasso_selected_1se_vars) > 0){
    
    lasso_1se_formula <- as.formula(
      paste("y ~", paste(lasso_selected_1se_vars, collapse = " + "))
    )
    
    lasso_1se_refit <- lm(lasso_1se_formula, data = dat)
    
  } else {
    
    lasso_1se_refit <- lm(y ~ 1, data = dat)
  }
  # Harvest results
  lasso_1se_res <- harvest(
    model = lasso_1se_refit,
    betas = true_beta,
    alpha = 0.05
  )
  
  lasso_1se_res$method <- "LASSO 1 SE"
  lasso_1se_res$n      <- n
  lasso_1se_res$rho    <- rho
  
  # ---------------------------
  # Elastic Net
  # ---------------------------
  x <- as.matrix(dat[, -1])
  y <- dat$y
  
  # Now doing cross validation
  enet_cv <- cv.glmnet(x, y, alpha = 1, nfolds = 10, standardize = TRUE, family = "gaussian")
  # Fitting model using minimum lambda
  enet_min_fit <- glmnet(
    x,
    y,
    alpha = 0.5,
    lambda = enet_cv$lambda.min,
    family = "gaussian"
  )
  # Selected variables
  enet_min_coef <- coef(enet_min_fit)
  enet_selected_min_vars <- rownames(enet_min_coef)[which(enet_min_coef != 0)]
  enet_selected_min_vars <- enet_selected_min_vars[enet_selected_min_vars != "(Intercept)"]
  # Refit OLS on selected variables for inference
  if(length(enet_selected_min_vars) > 0){
    
    enet_min_formula <- as.formula(
      paste("y ~", paste(enet_selected_min_vars, collapse = " + "))
    )
    
    enet_min_refit <- lm(enet_min_formula, data = dat)
    
  } else {
    
    enet_min_refit <- lm(y ~ 1, data = dat)
  }
  # Harvest results
  enet_min_res <- harvest(
    model = enet_min_refit,
    betas = true_beta,
    alpha = 0.05
  )
  
  enet_min_res$method <- "Elastic Net Minimum"
  enet_min_res$n      <- n
  enet_min_res$rho    <- rho
  
  # Fitting model using 1 SE lambda
  enet_1se_fit <- glmnet(
    x,
    y,
    alpha = 0.5,
    lambda = enet_cv$lambda.1se,
    family = "gaussian"
  )
  # Selected variables
  enet_1se_coef <- coef(enet_1se_fit)
  enet_selected_1se_vars <- rownames(enet_1se_coef)[which(enet_1se_coef != 0)]
  enet_selected_1se_vars <- enet_selected_1se_vars[enet_selected_1se_vars != "(Intercept)"]
  # Refit OLS on selected variables for inference
  if(length(enet_selected_1se_vars) > 0){
    
    enet_1se_formula <- as.formula(
      paste("y ~", paste(enet_selected_1se_vars, collapse = " + "))
    )
    
    enet_1se_refit <- lm(enet_1se_formula, data = dat)
    
  } else {
    
    enet_1se_refit <- lm(y ~ 1, data = dat)
  }
  # Harvest results
  enet_1se_res <- harvest(
    model = enet_1se_refit,
    betas = true_beta,
    alpha = 0.05
  )
  
  enet_1se_res$method <- "Elastic Net 1 SE"
  enet_1se_res$n      <- n
  enet_1se_res$rho    <- rho
  
  # ---------------------------
  # Combine and return results
  # ---------------------------
  selec_results <- rbind(
    pval_res,
    aic_res,
    bic_res,
    lasso_min_res,
    lasso_1se_res,
    enet_min_res,
    enet_1se_res
  )
  
  return(selec_results)
  
}