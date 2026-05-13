# simulate_demo_data.R
# Generates a synthetic dataset matching the structure of the PASC Aging Study
#
# Users can adjust `n_patients` and `seed` to produce datasets of any size.
# Output: demo_data.csv written to the working directory.


set.seed(123)
n_patients <- 100    # number of unique patients (each contributes 2 rows)
n <- n_patients * 2   # total rows (2 encounters per patient)

# Patient cluster ID 
EMPI <- rep(seq_len(n_patients), each = 2)

# Demographics 
age_decade <- round(runif(n, min = 3.0, max = 8.0), 1)   # age/10; range ~30–80
sex_cd <- sample(c("Female", "Male"), n, replace = TRUE)
race_group <- sample(
  c("White", "Black", "Asian", "Other", "Unknown"),
  n, replace = TRUE,
  prob = c(0.55, 0.22, 0.13, 0.07, 0.03)
)
race <- race_group   # alias used by specification_curve.R

hispanic <- rbinom(n, 1, prob = 0.15)

# Clinical variables 

vaccination_status_binary <- rbinom(n, 1, prob = 0.70)
Vaccination_status <- ifelse(vaccination_status_binary == 1,
                             "Vaccinated", "Unvaccinated")

quarters <- c("2020Q1", "2020Q2", "2020Q3", "2020Q4",
              "2021Q1", "2021Q2", "2021Q3", "2021Q4")
yrqt     <- sample(quarters, n, replace = TRUE)
yrqt_num <- match(yrqt, quarters)   

CHARLSON_INDEX  <- pmin(rpois(n, lambda = 2), 10)   

severity <- sample(
  c("Non-hospitalized", "Hospitalized", "ICU/Ventilation"),
  n, replace = TRUE,
  prob = c(0.65, 0.25, 0.10)
)

age_group <- cut(
  age_decade * 10,
  breaks = c(0, 45, 65, Inf),
  labels = c("<45 years old", "45-65 years old", ">65 years old"),
  right  = FALSE
)

# Outcome 
PASC.any <- rbinom(n, 1, prob = 0.30)

# Export 
demo_data <- data.frame(
  EMPI,
  age_decade,
  sex_cd,
  race_group,
  race,
  hispanic,
  vaccination_status_binary,
  Vaccination_status,
  yrqt,
  yrqt_num,
  CHARLSON_INDEX,
  severity,
  age_group,
  PASC.any
)

output_file <- "demo_data.csv"
write.csv(demo_data, output_file, row.names = FALSE)


