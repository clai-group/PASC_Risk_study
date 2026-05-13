# GEE Model: Binary Outcome ~ Covariates
# Correlation structure: exchangeable | Family: binomial

library(geepack)   
library(rms)      
library(dplyr)    

study_data <- ###load study data with the variables used in the study
  
# Model fit
gee_fit <- geeglm(
  PASC.any ~ age_decade +
    sex_cd +
    race_group +
    vaccination_status_binary +
    yrqt +
    rcs(CHARLSON_INDEX, 4) +
    hispanic +
    severity,
  data    = gee.dat.no.organ,
  family  = binomial,
  corstr  = "exchangeable",
  id      = EMPI
)


# Extract coefficients and compute odds ratios with 95%  CIs
coefs <- summary(gee_fit)$coefficients
or_df <- data.frame(
  Variable  = rownames(coefs),
  OR        = exp(coefs[, "Estimate"]),
  CI_lower  = exp(coefs[, "Estimate"] - 1.96 * coefs[, "Std.err"]),
  CI_upper  = exp(coefs[, "Estimate"] + 1.96 * coefs[, "Std.err"]),
  P_value   = coefs[, "Pr(>|W|)"]
)

# Drop intercept, spline terms, and reference-level rows
or_df <- or_df[-c(1, 6, 7, 10:12, 14), ]

# Assign readable labels
or_df$group <- c(
  "Age per-decade",
  "Male vs. Female",
  "Black vs. White",
  "Asian vs. White",
  "Vaccinated vs. Unvaccinated",
  "Year-quarter",
  "Hispanic vs. Non-hispanic",
  "Hospitalized vs. Non-hospitalized",
  "ICU/Ventilation vs. Non-hospitalized"
)


# Final results table
gee_results <- or_df %>%
  dplyr::select(group, OR, CI_lower, CI_upper, P_value)

row.names(gee_results) <- NULL






