# ============================================================
# 06_regression_models.R
# Purpose: Fit all regression models. No table formatting here —
#          that happens in 07_results_tables.R
# Input:   svy_nhis, svy_nhis_hpv, svy_nhis_shingles, svy_cancer
#          (from 05_descriptive_tables.R, cascading back through
#           04 -> 03 -> 02 -> 01)
# Output:  flu_results, covid_results, tetanus_results,
#          pneumo_results, hpv_results, shingles_results,
#          cancer_flu_results, cancer_covid_results, etc.
# ============================================================

source(here("scripts", "05_descriptive_tables.R"))

library(survey)
library(broom)
library(dplyr)

# ============================================================
# PART 1: Does cancer history predict vaccine uptake?
# (general NHIS adult population)
# ============================================================

run_vaccine_analysis <- function(svy_obj, vaccine_var, vaccine_name) {
  formula_unadj <- as.formula(paste(vaccine_var, "~ cancer_binary"))
  model_unadj <- svyglm(formula_unadj, design = svy_obj, family = quasibinomial())
  
  formula_adj <- as.formula(paste(
    vaccine_var,
    "~ cancer_binary + age_group + sex + race + empl_stat + health_insurance + income_45k"
  ))
  model_adj <- svyglm(formula_adj, design = svy_obj, family = quasibinomial())
  
  list(
    vaccine_name = vaccine_name,
    unadjusted = tidy(model_unadj, exponentiate = TRUE, conf.int = TRUE),
    adjusted   = tidy(model_adj, exponentiate = TRUE, conf.int = TRUE)
  )
}

flu_results      <- run_vaccine_analysis(svy_nhis, "flu_binary", "Influenza (past 12 months)")
covid_results    <- run_vaccine_analysis(svy_nhis, "covid_binary", "COVID-19 (at least 1 dose)")
tetanus_results  <- run_vaccine_analysis(svy_nhis, "tetanus_binary", "Tetanus (past 10 years)")
pneumo_results   <- run_vaccine_analysis(svy_nhis, "pneumo_binary", "Pneumonia (ever)")
hpv_results      <- run_vaccine_analysis(svy_nhis_hpv, "hpv_binary", "HPV (ages 18-64)")
shingles_results <- run_vaccine_analysis(svy_nhis_shingles, "shingles_binary", "Shingles (ages 50+)")

# ============================================================
# PART 2: Among cancer patients only, what predicts vaccination?
# ============================================================

svy_cancer_hpv      <- svy_cancer %>% filter(AGEP_A <= 64)
svy_cancer_shingles <- svy_cancer %>% filter(AGEP_A >= 50)

run_cancer_predictor_model <- function(svy_obj, vaccine_var, vaccine_name) {
  predictors <- "age_group + sex + race + empl_stat + health_insurance + income_45k"
  formula_full <- as.formula(paste(vaccine_var, "~", predictors))
  model <- svyglm(formula_full, design = svy_obj, family = quasibinomial())
  list(
    vaccine_name = vaccine_name,
    results = tidy(model, exponentiate = TRUE, conf.int = TRUE)
  )
}

cancer_flu_results      <- run_cancer_predictor_model(svy_cancer, "flu_binary", "Influenza Vaccination")
cancer_covid_results    <- run_cancer_predictor_model(svy_cancer, "covid_binary", "COVID-19 Vaccination")
cancer_tetanus_results  <- run_cancer_predictor_model(svy_cancer, "tetanus_binary", "Tetanus Vaccination")
cancer_pneumo_results   <- run_cancer_predictor_model(svy_cancer, "pneumo_binary", "Pneumonia Vaccination")
cancer_hpv_results      <- run_cancer_predictor_model(svy_cancer_hpv, "hpv_binary", "HPV Vaccination (ages 18-64)")
cancer_shingles_results <- run_cancer_predictor_model(svy_cancer_shingles, "shingles_binary", "Shingles Vaccination (ages 50+)")

cat("✅ 06_regression_models.R complete — model objects ready for 07_results_tables.R\n")