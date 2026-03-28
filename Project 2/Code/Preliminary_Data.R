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
a1_results <- data.frame(Radj = r_vals)

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
  a1_results[[col_name]] <- power_vals * 100
}

# Creating Table 4 for the paper (used ChatGPT to help format)
a1_results %>%
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

# List of values we are using
N_pos_vals <- c(50, 75, 88, 125, 150)
alpha_vals <- c(0.05/4, 0.05/6, 0.05/24)

# Initialize results data frame
a2_results <- data.frame()

# Loop through amyloid+ group sizes
for (N_pos in N_pos_vals) {
  
  # Calculate amyloid- group size
  N_neg <- 175 - N_pos
  
  # Calculate ratio between groups
  n_ratio <- N_pos / N_neg
  
  # Loop through correlation values
  for (rho_p in seq(0.2, 0.7, by = 0.1)) {
    for (rho_n in seq(0, 0.7, by = 0.1)) {
      
      # Skip invalid correlations (only keep rho_p > rho_n)
      if (rho_p > rho_n) {
        
        rho_diff <- rho_p - rho_n
        
        # Loop through alpha values
        for (a in alpha_vals) {
          
          # Compute power
          power_val <- corr.2samp(
            n1 = N_neg,
            n.ratio = n_ratio,
            rho1 = rho_n,
            rho2 = rho_p,
            alpha = a
          )
          
          # Store results
          a2_results <- rbind(a2_results, data.frame(
            Alpha    = round(a, 5),
            N_pos    = N_pos,
            N_neg    = N_neg,
            Rho_neg  = rho_n,
            Rho_pos  = rho_p,
            Rho_diff = round(rho_diff, 2),
            Power    = power_val * 100
          ))
        }
      }
    }
  }
}

# Make sure Alpha is a factor with labels
a2_results$Alpha_label <- factor(a2_results$Alpha,
                           levels = c(0.00208, 0.00833, 0.0125),
                           labels = c("\u03B1 = 0.05/24", "\u03B1 = 0.05/6", "\u03B1 = 0.05/4"))
a2_results$Rho_label <- factor(a2_results$Rho_diff,
                                 levels = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),
                                 labels = c("\u0394\u03C1 = 0.1",
                                            "\u0394\u03C1 = 0.2",
                                            "\u0394\u03C1 = 0.3", 
                                            "\u0394\u03C1 = 0.4",
                                            "\u0394\u03C1 = 0.5",
                                            "\u0394\u03C1 = 0.6",
                                            "\u0394\u03C1 = 0.7"))
a2_results$N_pos_label <- factor(a2_results$N_pos,
                               levels = c(50, 75, 88, 125, 150),
                               labels = c("N+ = 50",
                                          "N+ = 75",
                                          "N+ = 88", 
                                          "N+ = 125",
                                          "N+ = 150"))
a2_results %>%
  filter(Rho_diff <= 0.5) %>%
ggplot(aes(x = Rho_pos, y = Power, color = Alpha_label)) +
  geom_line() +
  geom_hline(aes(yintercept = 80, color = "80% Power"), linetype = "dashed") +
  facet_grid(Rho_label ~ N_pos_label) +
  scale_color_manual(
    values = c(
      "\u03B1 = 0.05/4"  = "mediumpurple3",
      "\u03B1 = 0.05/6"  = "seagreen4",
      "\u03B1 = 0.05/24" = "palevioletred3",
      "80% Power"        = "black"
    )
  ) +
  labs(
    title = "Power vs Amyloid+ Correlation",
    x = "Amyloid+ Correlation (\u03C1)",
    y = "Power (%)",
    color = "",
    caption = "\u0394\u03C1 refers to the correlation of the amyloid+ group minus the correlation of the amyloid- group."
  ) +
  ylim(0, 100) +
  theme_bw() +
  theme(legend.position = "bottom")