library(forestplot)
library(dplyr)
library(gee)
library(readr)


study_data <- ###load study data with the variables used in the study

## Fit a GEE model
gee_fit = gee::gee(PASC.any ~  age_group_10years + sex_cd + race_group + variant + charlson_index +
                     hispanic + vaccination_status_cat +severity, 
                   data = study_data , 
                   family = binomial(logit), 
                   corstr = "exchangeable",
                   id = EMPI)
summary_gee <- summary(gee_fit)
gee_coef = coef(gee_fit)
gee_or = round(exp(gee_coef),3)
gee_sd <- summary_gee$coefficients[, "Robust S.E."]
gee_LB <- round(exp(gee_coef - 1.96 * gee_sd),3)
gee_UB <- round(exp(gee_coef + 1.96 * gee_sd),3)
gee_results <- data.frame(OR = gee_or, 
                          LB = gee_LB, 
                          UB = gee_UB)
gee_results = gee_results[-c(1,6,7,12),]
gee_results$group <-c("Age groups by 10 years",
                      "Male vs. Female",
                      "Black vs. White",
                      "Asian vs. White",
                      "Delta vs. Alpha",
                      "Omicron vs. Alpha",
                      "Charlson index",
                      "Hispanic vs. Non-hispanic",
                      "Fully vaccinated/Boosted vs. Not Fully vaccinated",
                      "Hospitalization vs. Not severe",
                      "Icu/ventilation vs. Not severe")
gee_results = gee_results %>% dplyr::select(group, OR, LB, UB)
row.names(gee_results) = NULL


## Forest plot to show odds ratio
txt_gp <- fpTxtGp(label = gpar(fontfamily = "sans", cex = 1),
                  ticks = gpar(fontfamily = "sans", cex = 1.1),
                  xlab  = gpar(fontfamily = "sans", cex = 1),
                  title = gpar(fontfamily = "sans", cex = 1.2))

styles <- fpShapesGp(
  lines = list(
    gpar(col = "deepskyblue4"),
    gpar(col = "deepskyblue4"),
    gpar(col = "deepskyblue4"),
    gpar(col = "deepskyblue4"),
    gpar(col = "deepskyblue4"),
    gpar(col = "deepskyblue4"),
    gpar(col = "grey"),
    gpar(col = "deepskyblue4"),
    gpar(col = "deepskyblue4"),
    gpar(col = "deepskyblue4"),
    gpar(col = "grey"),
    gpar(col = "deepskyblue4"),
    gpar(col = "deepskyblue4")
  ),
  box = list(
    gpar(fill = "deepskyblue4"),
    gpar(fill = "deepskyblue4"),
    gpar(fill = "deepskyblue4"),
    gpar(fill = "deepskyblue4"),
    gpar(fill = "deepskyblue4"),
    gpar(fill = "deepskyblue4"),
    gpar(fill = "grey"),
    gpar(fill = "deepskyblue4"),
    gpar(fill = "deepskyblue4"),
    gpar(fill = "deepskyblue4"),
    gpar(fill = "grey"),
    gpar(fill = "deepskyblue4"),
    gpar(fill = "deepskyblue4")
  ) 
)

fp = gee_results %>%
  forestplot(labeltext=c(group, OR, LB, UB),  
             mean = OR,
             lower = LB,
             upper = UB,
             xlab="Odds Ratio (95% CI)",
             hrzl_lines=list("3" = gpar(lwd=1, columns=c(1:4), col="gray50"),
                             "4" = gpar(lwd=1, columns=c(1:4), col="gray50"),
                             "5" = gpar(lwd=1, columns=c(1:4), col="gray50"),
                             "6" = gpar(lwd=1, columns=c(1:4), col="gray50"),
                             "7" = gpar(lwd=1, columns=c(1:4), col="gray50"),
                             "8" = gpar(lwd=1, columns=c(1:4), col="gray50"),
                             "9" = gpar(lwd=1, columns=c(1:4), col="gray50"),
                             "10" = gpar(lwd=1,columns=c(1:4), col="gray50"), 
                             "11" = gpar(lwd=1, columns=c(1:4), col="gray50"),
                             "12" = gpar(lwd=1, columns=c(1:4), col="gray50"),
                             "13" = gpar(lwd=1, columns=c(1:4), col="gray50")) ,
             txt_gp=txt_gp,
             #col = fpColors(box = "orange", lines = "black"),
             zero=1, cex=0.9, lineheight = "auto", boxsize=0.3, colgap=unit(6,"mm"),
             lwd.ci=2, ci.vertices=F, ci.vertices.height = 0.4,shapes_gp = styles) %>%
  fp_add_header(group = c("", "Group"),
                OR = c("", "Odds Ratio"),
                LB = c("", "95% LB"),
                UB = c("", "95% UB"))# %>% fp_set_zebra_style("#f9f9f9")

fp
