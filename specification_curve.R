# specification_curve.R
# Specification curve analysis of the age (per decade) -> PASC association.
#
# Usage:
#   specification_dat <- read.csv("demo_data.csv")   # or your analysis-ready data
#   source("specification_curve.R")
#   plot(results)

library(specr)
library(splines)
library(dplyr)
library(ggplot2)
library(stringr)

if (!exists("specification_dat")) {
  stop("Assign your analysis-ready data frame to `specification_dat` before sourcing.")
}

## Covariates as named in the manuscript (Figure 4 labels)
specification_dat$charlson_spline <- ns(specification_dat$CHARLSON_INDEX, df = 3)
names(specification_dat)[names(specification_dat) == "PASC.any"] <- "PASC"
names(specification_dat)[names(specification_dat) == "vaccination_status_binary"] <- "Vaccination_status"

## Logistic model used in each specification
log_glm <- function(formula, data) {
  glm(formula, data, family = binomial(link = "logit"))
}

specs <- setup(
  data     = specification_dat,
  x        = "age_decade",
  y        = "PASC",
  controls = c("charlson_spline", "severity",
               "Vaccination_status", "yrqt_num"),
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
