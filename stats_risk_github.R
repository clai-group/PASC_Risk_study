library(dplyr)
library(gee)
library(readr)


study_data <- ###load study data with the variables used in the study
  
gee_fit_all = gee::gee(PASC.any ~ age_group_10years + sex_cd + race_group + variant +
                         charlson_index + I(charlson_index^2) + hispanic  +severity, 
                   data = gee.dat.no.organ , 
                   family = binomial(logit), 
                   corstr = "exchangeable",
                   id = EMPI)
summary_gee <- summary(gee_fit_all)
gee_coef = coef(gee_fit_all)
gee_or = round(exp(gee_coef),3)
gee_sd <- summary_gee$coefficients[, "Robust S.E."]
gee_LB <- round(exp(gee_coef - 1.96 * gee_sd),3)
gee_UB <- round(exp(gee_coef + 1.96 * gee_sd),3)
gee_results <- data.frame(OR = gee_or, 
                          LB = gee_LB, 
                          UB = gee_UB)
gee_results_all = gee_results[-c(1,6,7,12),]
gee_results_all$group <-c("Age groups by 10 years",
                      "Male vs. Female",
                      "Black vs. White",
                      "Asian vs. White",
                      "Delta vs. Alpha",
                      "Omicron vs. Alpha",
                      "Charlson index",
                      "Charlson index sqaured",
                      "Hispanic vs. Non-hispanic",
                      #"Fully vaccinated/Boosted vs. Not Fully vaccinated",
                      "Hospitalized vs. Non-hospitalized",
                      "Icu/ventilation vs. Non-hospitalized")
gee_results_all = gee_results_all %>% dplyr::select(group, OR, LB, UB)
row.names(gee_results_all) = NULL









