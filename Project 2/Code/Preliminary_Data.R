## Loading packages
library(tidyverse)
library(ggplot2)
library(corrplot)
library(kableExtra)
library(powertools)

## Reading in the data
dat_p2 <- read.csv("DataRaw/PrelimData.csv")

##*******************************************************************
## ------------------ Correlation Matrix and Plot  ----------------------
##*******************************************************************
##

cor_matrix <- cor(dat_p2, use = "complete.obs")
corrplot(cor_matrix, method = 'color', type = 'lower', addCoef.col = 'black', col = COL2('PiYG'))

##*******************************************************************
## ------------------ Correlations Between Variables  ----------------------
##*******************************************************************
##

# Between Outcomes
cor.test(dat_p2$CVLT_CNG3, dat_p2$CORT_CNG3)

# Between Predictors
cor.test(dat_p2$IL_6, dat_p2$MCP_1)

# Between Outcomes and Predictors
cor.test(dat_p2$IL_6, dat_p2$CVLT_CNG3)
cor.test(dat_p2$IL_6, dat_p2$CORT_CNG3)
cor.test(dat_p2$MCP_1, dat_p2$CVLT_CNG3)
cor.test(dat_p2$MCP_1, dat_p2$CORT_CNG3)

##*******************************************************************
## ------------------ Creating Table of Results ----------------------
##*******************************************************************
##

cor_dat <- data.frame(
  Variables = c("IL6 and MCP1", "IL6 and CVLT", 'IL6 and Cortical Thickness',
                "MCP1 and CVLT", "MCP1 and Cortical Thickness", "CVLT and Cortical Thickness"),
  Estimate = c(cor.test(dat_p2$IL_6, dat_p2$MCP_1)$estimate, cor.test(dat_p2$IL_6, dat_p2$CVLT_CNG3)$estimate,
               cor.test(dat_p2$IL_6, dat_p2$CORT_CNG3)$estimate,
               cor.test(dat_p2$MCP_1, dat_p2$CVLT_CNG3)$estimate, cor.test(dat_p2$MCP_1, dat_p2$CORT_CNG3)$estimate,
               cor.test(dat_p2$CVLT_CNG3, dat_p2$CORT_CNG3)$estimate),
  Pval = c(cor.test(dat_p2$IL_6, dat_p2$MCP_1)$p.value, cor.test(dat_p2$IL_6, dat_p2$CVLT_CNG3)$p.value,
                        cor.test(dat_p2$IL_6, dat_p2$CORT_CNG3)$p.value,
                        cor.test(dat_p2$MCP_1, dat_p2$CVLT_CNG3)$p.value, cor.test(dat_p2$MCP_1, dat_p2$CORT_CNG3)$p.value,
                        cor.test(dat_p2$CVLT_CNG3, dat_p2$CORT_CNG3)$p.value)
)

# Used ChatGPT to help format
cor_dat %>%
  dplyr::mutate(
    Estimate = round(as.numeric(Estimate), 3),
    Pval = format.pval(Pval, digits = 3, eps = 0.001)
  ) %>%
  kable(
    align = "lcc",
    col.names = c("Variable Pair", "Correlation (r)", "p-value"),
    booktabs = TRUE
  ) %>%
  kable_styling(
    full_width = FALSE,
    bootstrap_options = "condensed",
    position = "center"
  ) %>%
  group_rows("Between Predictors", 1, 1) %>%
  group_rows("Between Predictors and Outcomes", 2, 5) %>%
  group_rows("Between Outcomes", 6, 6) 

##*******************************************************************
## ------------------ Power Calculations  ----------------------
##*******************************************************************
##

# Defining a function to compute power
power_range <- function(predictor, outcome, p, alpha) {
  
  # Compute correlation
  r <- cor(dat_p2[[predictor]], dat_p2[[outcome]], use = "complete.obs")
  
  # Base R^2
  r_sq_base <- r^2
  
  # Adjusted R^2
  r_sq_values <- c(
    r_sq_base * 0.5,
    r_sq_base * 0.6,
    r_sq_base * 0.7,
    r_sq_base * 0.8,
    r_sq_base * 0.9,
    r_sq_base
  )
  
  # Run power for each R^2
  results <- lapply(r_sq_values, function(Rsq_val) {
    mlrF.overall(
      N = 175,
      p = p,
      Rsq = Rsq_val,
      alpha = alpha
    )
  })
  
  # Return clean table
  power_vals <- sapply(results, function(x) x)
  
  print(sprintf("%s and %s Power Results", predictor, outcome))
  data.frame(
    Rsq = r_sq_values,
    Power = power_vals
  )
}

# Using the function for Aim 1 with Bonferroni Correction
power_range(predictor = "IL_6", outcome = "CVLT_CNG3", p = 4, alpha= 0.05/4)
power_range(predictor = "IL_6", outcome = "CORT_CNG3", p = 4, alpha= 0.05/4)
power_range(predictor = "MCP_1", outcome = "CVLT_CNG3", p = 4, alpha= 0.05/4)
power_range(predictor = "MCP_1", outcome = "CORT_CNG3", p = 4, alpha= 0.05/4)