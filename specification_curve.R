# specification_curve.R
# Specification curve analysis of the age (per decade) -> PASC association.
#
# Consistent with the manuscript:
#   - age modeled continuously per decade (the exposure), as in the primary model
#   - adjustment covariates varied across specifications: Charlson spline,
#     acute severity, vaccination status, and year-quarter
#   - subsamples defined by sex, combined race/ethnicity, and three-level age group
#   - the small "Other" and "Unknown" race/ethnicity subsamples are excluded
#
# Usage:
#   specification_dat <- read.csv("demo_data.csv")   # or your analysis-ready data
#   source("specification_curve.R")
#   plot(results)

library(specr)
library(rms)
library(dplyr)
library(ggplot2)
library(stringr)

if (!exists("specification_dat")) {
  stop("Assign your analysis-ready data frame to `specification_dat` before sourcing.")
}

## Logistic model used in each specification
log_glm <- function(formula, data) {
  glm(formula, data, family = binomial(link = "logit"))
}

specs <- setup(
  data     = specification_dat,
  x        = "age_decade",
  y        = "PASC.any",
  controls = c("rcs(CHARLSON_INDEX, 4)", "severity",
               "vaccination_status_binary", "yrqt_num"),
  model    = "log_glm",
  subsets  = list(
    sex_cd            = c("Female", "Male"),
    race_eth_combined = c("Non-Hispanic White", "Non-Hispanic Black",
                          "Non-Hispanic Asian", "Non-Hispanic Other",
                          "Hispanic", "Unknown"),
    age_group         = c("<45 years old", "45-65 years old", ">65 years old")
  )
)

results <- specr(specs)

## Convert log-odds to the odds-ratio scale
results$data["OR"]           <- exp(results$data["estimate"])
results$data["or.conf.low"]  <- exp(results$data["conf.low"])
results$data["or.conf.high"] <- exp(results$data["conf.high"])

## Exclude the small "Other" and "Unknown" race/ethnicity subsamples
results$data <- results$data %>%
  filter(!grepl("other",   subsets, ignore.case = TRUE) &
         !grepl("unknown", subsets, ignore.case = TRUE))

## Specification curve
plot(results)
