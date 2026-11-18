# mediation_analysis.R
# Causal mediation decomposition of the age (per decade) -> PASC effect.
#
# Two mediators are examined in SEPARATE analyses:
#   (1) comorbidity burden (Charlson Comorbidity Index, truncated at 5)
#   (2) acute infection severity (dichotomized)
#
# Estimated overall and within age strata (<65 and >=65 years).
# Consistent with the manuscript:
#   - mediator models fitted by linear regression, PASC outcome model logistic
#   - 95% CIs from 1,000 nonparametric bootstrap resamples (seed 333 before each mediate() call)
#
# Usage:
#   study_data <- read.csv("demo_data.csv")   # or your analysis-ready data
#   source("mediation_analysis.R")
#   print(mediation_results)

library(mediation)

if (!exists("study_data")) {
  stop("Assign your analysis-ready data frame to `study_data` before sourcing.")
}

dat <- study_data
dat$sex_cd <- relevel(factor(dat$sex_cd), ref = "Female")
dat$race_eth_combined <- relevel(factor(dat$race_eth_combined),
                                 ref = "Non-Hispanic White")
dat$severity <- relevel(factor(dat$severity), ref = "Non-hospitalized")

## Mediator recodings
## Charlson truncated at 5, treated as a continuous score
dat$CHARLSON_INDEX_cat <- ifelse(dat$CHARLSON_INDEX >= 5, 5, dat$CHARLSON_INDEX)
## Severity dichotomized: 0 = non-hospitalized, 1 = hospitalized or higher
dat$severity_binary <- ifelse(dat$severity == "Non-hospitalized", 0, 1)

## Run both mediation pathways on a given data subset
## yrqt_cat = TRUE: year-quarter categorical in the comorbidity mediator model
## (as in the overall analysis reported in the manuscript)
run_mediation <- function(d, sims = 1000, seed = 333, yrqt_cat = FALSE) {

  # Pathway 1: comorbidity burden (three-level severity as covariate)
  if (yrqt_cat) {
    med_com <- glm(CHARLSON_INDEX_cat ~ age_decade + sex_cd + factor(yrqt_num) +
                     race_eth_combined + severity + vaccination_status_binary,
                   data = d, family = gaussian)
  } else {
    med_com <- glm(CHARLSON_INDEX_cat ~ age_decade + sex_cd + yrqt_num +
                     race_eth_combined + severity + vaccination_status_binary,
                   data = d, family = gaussian)
  }
  out_com <- glm(PASC.any ~ age_decade + CHARLSON_INDEX_cat + yrqt_num +
                   sex_cd + race_eth_combined + severity + vaccination_status_binary,
                 data = d, family = binomial(link = "logit"))
  set.seed(seed)
  m_com <- mediate(med_com, out_com, treat = "age_decade",
                   mediator = "CHARLSON_INDEX_cat", boot = TRUE, sims = sims)

  # Pathway 2: acute severity
  med_sev <- glm(severity_binary ~ age_decade + sex_cd + yrqt_num +
                   race_eth_combined + CHARLSON_INDEX_cat + vaccination_status_binary,
                 data = d, family = gaussian)
  out_sev <- glm(PASC.any ~ age_decade + CHARLSON_INDEX_cat + yrqt_num +
                   sex_cd + race_eth_combined + severity_binary + vaccination_status_binary,
                 data = d, family = binomial(link = "logit"))
  set.seed(seed)
  m_sev <- mediate(med_sev, out_sev, treat = "age_decade",
                   mediator = "severity_binary", boot = TRUE, sims = sims)

  summ <- function(m, mediator) {
    data.frame(mediator = mediator,
               ACME = m$d.avg, ACME_low = m$d.avg.ci[1], ACME_high = m$d.avg.ci[2], ACME_p = m$d.avg.p,
               ADE = m$z.avg, ADE_low = m$z.avg.ci[1], ADE_high = m$z.avg.ci[2], ADE_p = m$z.avg.p,
               total_effect = m$tau.coef, total_low = m$tau.ci[1], total_high = m$tau.ci[2], total_p = m$tau.p,
               prop_mediated = m$n.avg, row.names = NULL)
  }
  rbind(summ(m_com, "Comorbidity"), summ(m_sev, "Severity"))
}

## Overall and age-stratified (age_decade = age / 10, so 6.5 == 65 years)
overall <- run_mediation(dat, yrqt_cat = TRUE)
under65 <- run_mediation(dat[dat$age_decade <  6.5, ])
over65  <- run_mediation(dat[dat$age_decade >= 6.5, ])
overall$stratum <- "Overall"
under65$stratum <- "Age <65"
over65$stratum  <- "Age >=65"

mediation_results <- rbind(overall, under65, over65)[,
  c("stratum", "mediator", "ACME", "ACME_low", "ACME_high", "ACME_p",
    "ADE", "ADE_low", "ADE_high", "ADE_p",
    "total_effect", "total_low", "total_high", "total_p", "prop_mediated")]
row.names(mediation_results) <- NULL

print(mediation_results)
