## Loading packages
library(tidyverse)
library(ggplot2)
library(corrplot)
library(kableExtra)

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