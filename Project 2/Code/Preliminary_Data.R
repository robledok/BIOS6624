## Loading packages
library(tidyverse)
library(ggplot2)
library(corrplot)
library(kableExtra)
library(powertools)

## Reading in the data
dat_p2 <- read.csv("DataRaw/PrelimData.csv")

## Creating a year 1 - baseline variable
dat_p2 <- dat_p2 %>%
  mutate(
    cort_diff = -1 * CORT_CNG3,
    cvlt_dff = -1 * CVLT_CNG3
  )

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
cor.test(dat_p2$cvlt_dff, dat_p2$cort_diff)

# Between Predictors
cor.test(dat_p2$IL_6, dat_p2$MCP_1)

# Between Outcomes and Predictors
cor.test(dat_p2$IL_6, dat_p2$cvlt_dff)
cor.test(dat_p2$IL_6, dat_p2$cort_diff)
cor.test(dat_p2$MCP_1, dat_p2$cvlt_dff)
cor.test(dat_p2$MCP_1, dat_p2$cort_diff)

# Creating Table of Correlation Results
cor_dat <- data.frame(
  Variables = c("IL6 and MCP1", "IL6 and CVLT", 'IL6 and Cortical Thickness',
                "MCP1 and CVLT", "MCP1 and Cortical Thickness", "CVLT and Cortical Thickness"),
  Estimate = c(cor.test(dat_p2$IL_6, dat_p2$MCP_1)$estimate, cor.test(dat_p2$IL_6, dat_p2$cvlt_dff)$estimate,
               cor.test(dat_p2$IL_6, dat_p2$cort_diff)$estimate,
               cor.test(dat_p2$MCP_1, dat_p2$cvlt_dff)$estimate, cor.test(dat_p2$MCP_1, dat_p2$cort_diff)$estimate,
               cor.test(dat_p2$cvlt_dff, dat_p2$cort_diff)$estimate),
  Pval = c(cor.test(dat_p2$IL_6, dat_p2$MCP_1)$p.value, cor.test(dat_p2$IL_6, dat_p2$cvlt_dff)$p.value,
                        cor.test(dat_p2$IL_6, dat_p2$cort_diff)$p.value,
                        cor.test(dat_p2$MCP_1, dat_p2$cvlt_dff)$p.value, cor.test(dat_p2$MCP_1, dat_p2$cort_diff)$p.value,
                        cor.test(dat_p2$cvlt_dff, dat_p2$cort_diff)$p.value)
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
## ------------------ Power Calculation Aim 1 ----------------------
##*******************************************************************
##

# Correlation values
r_vals <- seq(0.2, 0.7, by = 0.05)

# Alpha values
alpha_vals <- c(0.05/4, 0.05/6, 0.05/24)

# Initialize a data frame for our results
final_results <- data.frame(Radj = r_vals)

# Looping through alpha
for (a in alpha_vals) {
  
  # Initizalizing a vector to save our results
  power_vals <- numeric(length(r_vals)) 
  
  # Looping through our correlation values
  for (i in seq_along(r_vals)) {
    # Conducting the power calculation
    power_vals[i] <- corr.1samp(N = 175, rhoA = r_vals[i], alpha = a)
  }
  
  # Defining column names
  
  col_name <- paste0("Alpha_", round(a, 5))
  # Saving the power for each alpha column
  final_results[[col_name]] <- power_vals * 100
}

# Creating Table 4 for the paper (used ChatGPT to help format)
final_results %>%
  slice(1:7) %>%
  kable(align = "lccc",
        col.names = c("Correlation", "\u03B1 = 0.05/4", "\u03B1 = 0.05/6", 
                      "\u03B1 = 0.05/24"),
        digits = 2,
        caption = "Table 4: Power Calculation for Aim 1") %>%
  row_spec(0, bold = TRUE, color = "black", background = "#c4d9f1") %>%
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("condensed", "hover", "striped"),
    position = "center"
  ) %>%
  column_spec(1, width = "10em") %>%  
  column_spec(2:4, width = "7em") %>%
  add_header_above(c(" " = 1, "Power (%)" = 3), background = "#d9d9d9", color = "black")
  

##*******************************************************************
## ------------------ Power Calculation Aim 2 ----------------------
##*******************************************************************
##

