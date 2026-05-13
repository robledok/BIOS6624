## Loading packages
library(future)
library(future.apply)
library(parallel)
library(hdrm)
library(glmnet)
library(Matrix)
library(forcats)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)

##*******************************************************************
## ------------------ Results Extraction Function (Carter's Notes) ----------------------
##*******************************************************************
##

harvest <- function(model, betas, alpha = 0.05) {
  # Extract model results
  estimates <- as.data.frame(summary(model)$coefficients) 
  # Add variable names as a column
  estimates <- within(estimates, {
    betas <- row.names(estimates)
  })
  # Find 95% confidence intervals
  CIs <- as.data.frame(confint.default(model, level = 1-alpha))
  names(CIs) <- c('LCL','UCL')
  # Add variable names as a column
  CIs <- within(CIs, {
    betas <- row.names(CIs)
  })
  # Defining our variable names
  var_names <- sprintf("V%02d", 1:length(betas))
  # Combing results into dataframe
  res_dat <- data.frame(
    variables = var_names,
    true_values = betas
  ) %>%
    # Adding new variables
    within({
      # Defining what it means to be selected (if selected 1, if not 0)
      selected <- ifelse(variables %in% row.names(estimates), 1, 0)
      # Defining significance (if significant < 0.05 characterize as 1, if not 0)
      signif   <- ifelse(variables %in% row.names(estimates[estimates[,4] < alpha,]), 1, 0)
      # Defining our true non zero betas (1 if true non zero, 0 if not)
      true_non_zero <- ifelse(variables %in% sprintf("V%02d", 1:5), 1, 0)
    }) %>%
    # Merge confidence intervals into the results dataframe
    merge(CIs,
          by.x = 'variables',
          by.y = 'betas',
          all.x = T) %>%
    # Adding new variable
    within({
      # Defining what it means to be covered
      covered <- ifelse(LCL <= true_values & true_values <= UCL, 1, 0)
    })
  # If a true non-zero variable was NOT selected, coverage should count as failure (0)
  res_dat[is.na(res_dat$covered) == T & 
            res_dat$selected == 0 & 
            res_dat$true_non_zero == 1,'covered'] <- 0 
  # If a true zero variable was NOT selected, coverage should count as success (1)
  res_dat[is.na(res_dat$covered) == T & 
            res_dat$selected == 0 & 
            res_dat$true_non_zero == 0,'covered'] <- 1 
  # Adding estimates to data
  res_dat$estimate <- estimates$Estimate[
    match(res_dat$variables, estimates$betas)
  ]
  # Returning results
  return(res_dat)
}

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
  # Harvest results
  pval_res <- harvest(model = pval_selec,  
                      betas   = true_beta,  
                      alpha   = 0.05       
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
  # Harvest results
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
  # Harvest results
  bic_res <- harvest(model = bic_selec,  
                     betas   = true_beta,       
                     alpha   = 0.05       
  )
  bic_res$method <- 'BIC'
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

##*******************************************************************
## ------------------ Matrix for Scenarios ----------------------
##*******************************************************************
##
scenarios <- expand.grid(
  n = c(250, 500),
  rho = c(0, 0.35, 0.7)
)

##*******************************************************************
## ------------------ Function to Run Many Simulations  ----------------------
##*******************************************************************
##

# From Carter's notes
run_sims <- function(scenario){
  # Make sure profile has only 1 row
  stopifnot(nrow(scenario) == 1)
  # Create an empty list to store results
  res <- list()
  # Repeat the simulation 1000 times
  for(iter in 1:7500) {
    # Run one simulation
    tmp <- simulate_func(
      n   = scenario$n,
      rho = scenario$rho
    )
    # Save which simulation number this is
    tmp$iter <- iter
    # Store the result
    res[[iter]] <- tmp
  }
  # Combine all simulation results into one dataframe
  res <- do.call("rbind", res)
  # Return final results
  return(res)
}

##*******************************************************************
## ------------------ Running Simulations in Parallel  ----------------------
##*******************************************************************
##

n_scenarios <- nrow(scenarios)
plan(multisession, workers = n_scenarios)
set.seed(6624)
system.time({
  simres <- future_lapply (1:n_scenarios, function(i){
    run_sims(scenario = scenarios[i,])
  }
  ,future.seed=TRUE)
})
simres <- do.call('rbind', simres)
# Stop the cluster
plan(sequential)

# Saving data so I don't have to keep rerunning
saveRDS(simres, "DataProcessed/simulation_results")

##*******************************************************************
## ------------------ Bias and Coverage  ----------------------
##*******************************************************************
##

res_variable <- simres %>%
  group_by(method, n, rho, variables, true_values) %>%
  summarize(
    bias     =  mean(ifelse(is.na(estimate), 0, estimate) - true_values),
    coverage = mean(covered)*100,
    .groups  = "drop"
  )

# Releveling variables
method_levels <- c("p-value", "AIC", "BIC", "LASSO Minimum", "LASSO 1 SE", 
                   "Elastic Net Minimum", "Elastic Net 1 SE")
res_variable <- res_variable %>%
  mutate(method = factor(method, levels = method_levels))

# ---------------------------
# Graph of Bias (Only focusing on Signal Variables)
# ---------------------------
grid_labeller <- labeller(
  n   = as_labeller(function(y) paste0("N = ", y)),
  rho = as_labeller(function(x) paste0("ρ = ", x))
)

res_variable %>%
  filter(true_values != 0) %>%
  mutate(method = factor(method, levels = method_levels)) %>%
  ggplot(aes(x = variables, y = method, fill = bias)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(bias, 2)), size = 3) +
  facet_grid(n ~ rho, labeller = grid_labeller) +
  scale_fill_gradient2(
    low = "#1dbde6",
    high = "#f20094",
    midpoint = 0,
    limits = c(-0.2, 0.2)
  ) +
  theme_bw() +
  labs(x = "Variable", y = "Method", fill = "Bias") +
  theme(legend.position = "bottom")


# ---------------------------
# Graph of Coverage (Only focusing on Signal Variables)
# ---------------------------
res_variable %>%
  filter(true_values != 0) %>%
  mutate(method = factor(method, levels = method_levels)) %>%
  ggplot(aes(x = variables, y = method, fill = coverage)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(coverage, 2)), size = 3) +
  facet_grid(n ~ rho, labeller = grid_labeller) +
  scale_fill_gradient2(
    low = "#ff0065",
    mid = "#ffbbcf",
    high = "white",
    midpoint = 50,
    limits = c(20, 100)
  ) +
  theme_bw() +
  labs(x = "Variable", y = "Method", fill = "Coverage (%)") +
  theme(legend.position = "bottom")

##*******************************************************************
## ------------------ Selection Frequency  ----------------------
##*******************************************************************
##
selection_freq <- simres %>%
  group_by(method, n, rho, variables, true_values) %>%
  summarize(
    selection_rate = mean(selected == 1) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    method    = factor(method, levels = method_levels),
    true_zero = ifelse(true_values == 0, "Null (β = 0)", "Signal (β ≠ 0)")
  )

selection_freq %>%
  ggplot(aes(x = variables, y = method, fill = selection_rate)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(selection_rate, 1)), size = 2.5) +
  facet_grid(n ~ rho, labeller = grid_labeller) +
  scale_fill_gradient2(
    low = "#F9B0CF",
    mid = "#FEFFD6",
    high = "#afd8a9",
    midpoint = 50,
    limits = c(0, 100)
  ) +
  geom_vline(xintercept = 5.5, linetype = "dashed", color = "black") +  
  theme_bw() +
  labs(
    x     = "Variable",
    y     = "Method",
    fill  = "Selection Rate (%)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

##*******************************************************************
## ------------------ Type I and II Error, FPR, and TPR  ----------------------
##*******************************************************************
##
# Type I and II Error, TPR, and FPR by Variable
type_error_variable <- simres %>%
  mutate(
    # Type I error: null variable selected AND significant
    type1_error = (true_non_zero == 0 & selected == 1 & signif == 1),
    # Type II error: signal variable not selected OR not significant
    type2_error = (true_non_zero == 1 & (selected == 0 | signif == 0)),
    # TPR and FPR — selection only
    true_pos = (true_non_zero == 1 & selected == 1),
    false_pos = (true_non_zero == 0 & selected == 1),
  )
# Type I and II Error, TPR, and FPR by Method
type_error_method <- type_error_variable %>%
  group_by(method, n, rho, iter) %>%
  summarize(
    # Type I: selected AND significant null vars / all null vars
    type1_error = sum(type1_error) / sum(true_non_zero == 0),
    # Type II: not selected OR not significant signal vars / all signal vars
    type2_error = sum(type2_error) / sum(true_non_zero == 1),
    # TPR/sensitivity: selected signal vars / all signal vars (selection only)
    true_positive_rate  = sum(true_pos) / sum(true_non_zero == 1),
    # FPR: selected null vars / all null vars (selection only)
    false_positive_rate = sum(false_pos) / sum(true_non_zero == 0),
    .groups = "drop"
  ) %>%
  group_by(method, n, rho) %>%
  summarize(
    type1_error         = mean(type1_error),
    type2_error         = mean(type2_error),
    true_positive_rate  = mean(true_positive_rate),
    false_positive_rate = mean(false_positive_rate),
    .groups = "drop"
  )

# Releveling variables
type_error_method <- type_error_method %>%
  mutate(method = factor(method, levels = method_levels))

# ---------------------------
# Graph of Type I and II Error
# ---------------------------

error_plot_dat <- type_error_method %>%
  pivot_longer(
    cols = c(type1_error, type2_error),
    names_to = "error_type",
    values_to = "rate"
  )

colors <- brewer.pal(7, "Accent")
ref_lines <- data.frame(
  y = c(0.05),
  line_type = c("Ideal Type I Error")
)

ggplot(error_plot_dat, aes(x = method, y = rate, fill = error_type)) +
  geom_col(position = "dodge") +
  geom_hline(
    data = ref_lines,
    aes(yintercept = y, linetype = line_type, color = line_type)) +
  facet_grid(n ~ rho, labeller = grid_labeller) +
  scale_fill_manual(
    values = colors,
    labels = c("Type I Error", "Type II Error")) +
  scale_linetype_manual(
    values = c("Ideal Type I Error" = "dashed")) +
  scale_color_manual(
    values = c("Ideal Type I Error" = "forestgreen")) +
  theme_light() +
  labs(
    x = "Method",
    y = "Error Rate",
    fill = NULL,
    linetype = NULL,
    color = NULL
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    strip.text = element_text(color = "black"),
    strip.background = element_rect(fill = "grey80", color = NA)
  )

# ---------------------------
# Graph of TPR and FPR
# ---------------------------

# True Positive
ggplot(type_error_method, aes(x = method, y = true_positive_rate, fill = method)) +
  geom_col(position = "dodge") +
  geom_hline(aes(yintercept = 1), color = "black", linetype = "dashed") +
  facet_grid(n ~ rho, labeller = grid_labeller) +
  scale_fill_manual(values = colors) +
  theme_light() +
  labs(
    x = "Method",
    y = "Rate"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        strip.text = element_text(color = "black"),
        strip.background = element_rect(fill = "grey80", color = NA))

# False Positive
ggplot(type_error_method, aes(x = method, y = false_positive_rate, fill = method)) +
  geom_col(position = "dodge") +
  geom_hline(aes(yintercept = 0), color = "black", linetype = "dashed") +
  ylim(0, 1) +
  facet_grid(n ~ rho, labeller = grid_labeller) +
  scale_fill_manual(values = colors) +
  theme_light() +
  labs(
    x = "Method",
    y = "Rate"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        strip.text = element_text(color = "black"),
        strip.background = element_rect(fill = "grey80", color = NA))