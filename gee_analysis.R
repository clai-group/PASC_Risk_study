# gee_analysis.R
# Primary GEE logistic regression model for PASC risk factors.
#
# Outcome            : PASC per infection episode (PASC.any)
# Clustering unit    : patient (EMPI)
# Working correlation: exchangeable
# Variance           : cluster-robust (sandwich)
#
# Consistent with the manuscript:
#   - race and ethnicity modeled as a single combined variable
#   - Charlson Comorbidity Index modeled with a restricted cubic spline
#   - year-quarter modeled as a continuous index (single estimate)
#   - no infection-sequence term
#
# Usage:
#   study_data <- read.csv("demo_data.csv")   # or your analysis-ready data
#   source("gee_analysis.R")
#   print(gee_results)

library(geepack)
library(rms)
library(dplyr)

if (!exists("study_data")) {
  stop("Assign your analysis-ready data frame to `study_data` before sourcing.")
}

dat <- study_data

## Reference categories to match the manuscript
dat$sex_cd <- relevel(factor(dat$sex_cd), ref = "Female")
dat$race_eth_combined <- relevel(factor(dat$race_eth_combined),
                                 ref = "Non-Hispanic White")
dat$severity <- relevel(factor(dat$severity), ref = "Non-hospitalized")

## GEE requires records ordered by cluster id
dat <- dat[order(dat$EMPI), ]

## Model fit
gee_fit <- geeglm(
  PASC.any ~ age_decade +
    sex_cd +
    race_eth_combined +
    vaccination_status_binary +
    yrqt_num +
    rcs(CHARLSON_INDEX, 4) +
    severity,
  data   = dat,
  family = binomial,
  id     = EMPI,
  corstr = "exchangeable"
)

## Odds ratios with 95% CIs
coefs <- summary(gee_fit)$coefficients
or_df <- data.frame(
  term     = rownames(coefs),
  OR       = exp(coefs[, "Estimate"]),
  CI_lower = exp(coefs[, "Estimate"] - 1.96 * coefs[, "Std.err"]),
  CI_upper = exp(coefs[, "Estimate"] + 1.96 * coefs[, "Std.err"]),
  P_value  = 2 * pnorm(-abs(coefs[, "Estimate"] / coefs[, "Std.err"])),  # two-sided Wald; avoids P reported as 0
  row.names = NULL,
  stringsAsFactors = FALSE
)

## Drop the intercept and the Charlson spline basis terms
or_df <- or_df[!grepl("Intercept|CHARLSON", or_df$term), ]

## Readable labels, derived from term names (order-independent)
label_term <- function(term) {
  term <- sub("^age_decade$", "Age (per decade)", term)
  term <- sub("^sex_cdMale$", "Male vs. Female", term)
  term <- sub("^vaccination_status_binary$", "Vaccinated vs. Unvaccinated", term)
  term <- sub("^yrqt_num$", "Year-quarter", term)
  term <- sub("^race_eth_combined(.*)$", "\\1 vs. Non-Hispanic White", term)
  term <- sub("^severity(.*)$", "\\1 vs. Non-hospitalized", term)
  term
}
or_df$group <- vapply(or_df$term, label_term, character(1))

## Final results table
gee_results <- or_df %>%
  dplyr::select(group, OR, CI_lower, CI_upper, P_value)
row.names(gee_results) <- NULL

print(gee_results)
