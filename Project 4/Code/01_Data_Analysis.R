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
  
  # Backwards selection by p-values
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
  
  # Backwards selection by AIC
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
  
  # Backwards selection by BIC
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
}