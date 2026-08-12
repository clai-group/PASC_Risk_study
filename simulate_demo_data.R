# simulate_demo_data.R
# Generates a synthetic dataset matching the column structure of the
# PASC Aging Study, so the analysis code can be run and inspected.
#
# The outcome is drawn at random and is NOT associated with any covariate;
# estimates from demo data are meaningless and must not be interpreted.
#
# Output: demo_data.csv written to the working directory.

## --- settings (edit as desired) ---
n_patients <- 100   # unique patients; each contributes 2 infection episodes
seed       <- 42    # change for a different random draw
## ----------------------------------

set.seed(seed)
n <- n_patients * 2

## Patient cluster ID (2 episodes per patient)
EMPI <- rep(seq_len(n_patients), each = 2)

## Demographics
age_decade <- round(runif(n, min = 3.0, max = 8.5), 1)   # age / 10, ~30-85y
sex_cd <- sample(c("Female", "Male"), n, replace = TRUE, prob = c(0.633, 0.367))

## Combined race and ethnicity (single mutually exclusive variable)
race_eth_combined <- sample(
  c("Non-Hispanic White", "Non-Hispanic Black", "Non-Hispanic Asian",
    "Non-Hispanic Other", "Hispanic", "Unknown"),
  n, replace = TRUE,
  prob = c(0.55, 0.18, 0.10, 0.07, 0.07, 0.03)
)

## Clinical variables
vaccination_status_binary <- rbinom(n, 1, prob = 0.70)   # 1 = fully vaccinated

quarters <- c("2020Q1", "2020Q2", "2020Q3", "2020Q4",
              "2021Q1", "2021Q2", "2021Q3", "2021Q4")
yrqt     <- sample(quarters, n, replace = TRUE)   # human-readable label
yrqt_num <- match(yrqt, quarters)                 # numeric index used in models

CHARLSON_INDEX <- pmin(rpois(n, lambda = 2), 10)

severity <- sample(
  c("Non-hospitalized", "Hospitalized", "ICU/Ventilation"),
  n, replace = TRUE, prob = c(0.65, 0.25, 0.10)
)

age_group <- cut(
  age_decade * 10,
  breaks = c(0, 45, 65, Inf),
  labels = c("<45 years old", "45-65 years old", ">65 years old"),
  right  = FALSE
)

## Outcome (random; no real associations encoded)
PASC.any <- rbinom(n, 1, prob = 0.30)

demo_data <- data.frame(
  EMPI, age_decade, sex_cd, race_eth_combined,
  vaccination_status_binary, yrqt, yrqt_num,
  CHARLSON_INDEX, severity, age_group, PASC.any,
  stringsAsFactors = FALSE
)

write.csv(demo_data, "demo_data.csv", row.names = FALSE)

cat("Simulation complete.\n",
    "  Patients :", n_patients, "\n",
    "  Rows     :", n, "\n",
    sprintf("  PASC rate: ~%.0f%%\n", 100 * mean(PASC.any)),
    "  Output   : demo_data.csv\n")
