library(specr)
library(dplyr)
library(ggplot2)
library(stringr)

specification_dat = ###load study data with the variables used in the study

## Set up the model to use in the specification analysis
log_glm <- function(formula, data) {
  glm(formula, data, family = binomial(link = "logit"))
}

specs <- setup(data = specification_dat,
               x = "age_group_10years",
               y = "PASC",
               controls = c("charlson_index","variant", "severity",  "Vaccination_status"),
               model = "log_glm",
               subsets = list(sex_cd = c("Female", "Male"),
                              race = c("White","Black", "Asian", "Other", "Unknown"),
                              age_group = c("<45 years old", "45-65 years old", ">65 years old")))

specs_summary = summary(specs, rows = 100)
results = specr(specs)
results$data["OR"] = exp(results$data["estimate"])
results$data["or.conf.low"] = exp(results$data["conf.low"])
results$data["or.conf.high"] = exp(results$data["conf.high"])
results$data = results$data %>%
  filter(!grepl("other", subsets, ignore.case = TRUE) & 
           !grepl("unknow", subsets, ignore.case = TRUE))

## Generate the specification analysis curve
plot(results)
