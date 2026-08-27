# ============================================================
# 07_results_tables.R
# Purpose: Format the model objects from 06_regression_models.R
#          into final, saved gt() tables
# Input:   flu_results, covid_results, ..., cancer_flu_results, ...
#          (from 06_regression_models.R, cascading through
#           05 -> 04 -> 03 -> 02 -> 01)
# Output:  HTML/PNG/DOCX tables saved to /output
# ============================================================

source(here("scripts", "06_regression_models.R"))

library(dplyr)
library(gt)

# ============================================================
# Helper: turn a term name like "raceAsian" or "age_group35-49"
# into a readable label
# ============================================================
clean_predictor_name <- function(term) {
  case_when(
    term == "cancer_binary" ~ "Cancer history (Yes vs. No)",
    term == "age_group35-49" ~ "Age 35-49 (vs. 18-34)",
    term == "age_group50-64" ~ "Age 50-64 (vs. 18-34)",
    term == "age_group65-74" ~ "Age 65-74 (vs. 18-34)",
    term == "age_group75+" ~ "Age 75+ (vs. 18-34)",
    term == "sexFemale" ~ "Female (vs. Male)",
    term == "raceBlack/African American" ~ "Black/African American (vs. White)",
    term == "raceAsian" ~ "Asian (vs. White)",
    term == "raceAmerican Indian/Alaska Native" ~ "American Indian/Alaska Native (vs. White)",
    term == "raceNative Hawaiian/Pacific Islander" ~ "Native Hawaiian/Pacific Islander (vs. White)",
    term == "raceOther race" ~ "Other race (vs. White)",
    term == "empl_statNot employed" ~ "Not employed (vs. Employed)",
    term == "health_insuranceNo" ~ "No health insurance (vs. Yes)",
    term == "income_45k$45,000 or more" ~ "Income ≥$45,000 (vs. <$45,000)",
    TRUE ~ term
  )
}

# ============================================================
# TABLE A: Cancer effect on vaccine uptake — unadjusted vs. adjusted,
# across all six vaccines
# ============================================================

extract_cancer_or <- function(results_list) {
  format_row <- function(df, model_label) {
    df %>%
      filter(term == "cancer_binary") %>%
      mutate(
        Vaccine = results_list$vaccine_name,
        Model = model_label,
        OR = sprintf("%.2f", estimate),
        CI = sprintf("(%.2f-%.2f)", conf.low, conf.high),
        P_value = case_when(
          p.value < 0.001 ~ "<0.001",
          p.value < 0.01 ~ sprintf("%.3f", p.value),
          TRUE ~ sprintf("%.2f", p.value)
        )
      ) %>%
      select(Vaccine, Model, OR, CI, P_value)
  }
  bind_rows(
    format_row(results_list$unadjusted, "Unadjusted"),
    format_row(results_list$adjusted, "Adjusted")
  )
}

all_vaccines_comparison <- bind_rows(
  extract_cancer_or(flu_results),
  extract_cancer_or(covid_results),
  extract_cancer_or(tetanus_results),
  extract_cancer_or(pneumo_results),
  extract_cancer_or(hpv_results),
  extract_cancer_or(shingles_results)
)

table_all_vaccines <- all_vaccines_comparison %>%
  gt(groupname_col = "Vaccine") %>%
  tab_header(
    title = md("**Association between Cancer History and Vaccine Uptake**"),
    subtitle = "National Health Interview Survey (NHIS) 2022"
  ) %>%
  cols_label(Model = md("**Model**"), OR = md("**OR**"), CI = md("**95% CI**"), P_value = md("**P-value**")) %>%
  tab_style(style = list(cell_fill(color = "#E8E8E8"), cell_text(weight = "bold", size = px(13))), locations = cells_row_groups()) %>%
  tab_style(style = cell_text(align = "center"), locations = cells_body(columns = c(OR, CI, P_value))) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = Model, rows = Model == "Adjusted")) %>%
  tab_footnote(
    footnote = "Adjusted models control for age group, sex, race/ethnicity, employment status, health insurance coverage, and household income. HPV vaccine analysis restricted to ages 18-64; Shingles vaccine analysis restricted to ages 50+."
  ) %>%
  tab_source_note(source_note = "OR = Odds Ratio; CI = Confidence Interval") %>%
  tab_options(table.font.size = 11, heading.title.font.size = 14, heading.subtitle.font.size = 12, row_group.font.weight = "bold")

gtsave(table_all_vaccines, here("output", "all_vaccines_cancer_comparison.html"))
gtsave(table_all_vaccines, here("output", "all_vaccines_cancer_comparison.png"))
gtsave(table_all_vaccines, here("output", "all_vaccines_cancer_comparison.docx"))

# ============================================================
# TABLE B: Full adjusted model, one table per vaccine
# ============================================================

create_full_model_table <- function(results_list) {
  results_list$adjusted %>%
    filter(term != "(Intercept)") %>%
    mutate(
      Predictor = clean_predictor_name(term),
      `OR (95% CI)` = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
      `P-value` = case_when(
        p.value < 0.001 ~ "<0.001***",
        p.value < 0.01 ~ sprintf("%.3f**", p.value),
        p.value < 0.05 ~ sprintf("%.3f*", p.value),
        TRUE ~ sprintf("%.3f", p.value)
      )
    ) %>%
    select(Predictor, `OR (95% CI)`, `P-value`) %>%
    gt() %>%
    tab_header(
      title = md(paste0("**Multivariable Logistic Regression: ", results_list$vaccine_name, "**")),
      subtitle = "National Health Interview Survey (NHIS) 2022"
    ) %>%
    cols_label(Predictor = md("**Variable**")) %>%
    tab_footnote(footnote = "*** p<0.001, ** p<0.01, * p<0.05") %>%
    tab_source_note(source_note = "Reference categories: No cancer history, Age 18-34, Male, White race, Employed, Has health insurance, Income <$45,000") %>%
    cols_align(align = "left", columns = Predictor) %>%
    cols_align(align = "center", columns = c(`OR (95% CI)`, `P-value`)) %>%
    tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = Predictor, rows = Predictor == "Cancer history (Yes vs. No)")) %>%
    tab_options(table.font.size = 11, heading.title.font.size = 14)
}

vaccine_full_tables <- list(
  flu = create_full_model_table(flu_results),
  covid = create_full_model_table(covid_results),
  tetanus = create_full_model_table(tetanus_results),
  pneumo = create_full_model_table(pneumo_results),
  hpv = create_full_model_table(hpv_results),
  shingles = create_full_model_table(shingles_results)
)

for (vax_name in names(vaccine_full_tables)) {
  gtsave(vaccine_full_tables[[vax_name]], here("output", paste0(vax_name, "_vaccine_full_model.html")))
}

# ============================================================
# TABLE C: Predictors of vaccination AMONG CANCER PATIENTS,
# one table per vaccine
# ============================================================

create_cancer_patient_table <- function(results_list) {
  results_list$results %>%
    filter(term != "(Intercept)") %>%
    mutate(
      Predictor = clean_predictor_name(term),
      `OR (95% CI)` = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
      `P-value` = case_when(
        p.value < 0.001 ~ "<0.001***",
        p.value < 0.01 ~ sprintf("%.3f**", p.value),
        p.value < 0.05 ~ sprintf("%.3f*", p.value),
        TRUE ~ sprintf("%.3f", p.value)
      )
    ) %>%
    select(Predictor, `OR (95% CI)`, `P-value`) %>%
    gt() %>%
    tab_header(
      title = md(paste0("**Predictors of ", results_list$vaccine_name, " Among Cancer Patients**")),
      subtitle = "National Health Interview Survey (NHIS) 2022"
    ) %>%
    cols_label(Predictor = md("**Variable**")) %>%
    tab_footnote(footnote = "*** p<0.001, ** p<0.01, * p<0.05. Analysis restricted to individuals with cancer history.") %>%
    tab_source_note(source_note = "Reference categories: Age 18-34, Male, White race, Employed, Has health insurance, Income <$45,000") %>%
    cols_align(align = "left", columns = Predictor) %>%
    cols_align(align = "center", columns = c(`OR (95% CI)`, `P-value`)) %>%
    tab_options(table.font.size = 11, heading.title.font.size = 14)
}

cancer_patient_tables <- list(
  flu = create_cancer_patient_table(cancer_flu_results),
  covid = create_cancer_patient_table(cancer_covid_results),
  tetanus = create_cancer_patient_table(cancer_tetanus_results),
  pneumo = create_cancer_patient_table(cancer_pneumo_results),
  hpv = create_cancer_patient_table(cancer_hpv_results),
  shingles = create_cancer_patient_table(cancer_shingles_results)
)

for (vax_name in names(cancer_patient_tables)) {
  gtsave(cancer_patient_tables[[vax_name]], here("output", paste0("cancer_patients_", vax_name, "_predictors.html")))
}

# ============================================================
# Console summary
# ============================================================

cat("\n========================================\n")
cat("SUMMARY: CANCER EFFECT ON VACCINE UPTAKE\n")
cat("========================================\n\n")

print_summary <- function(results_list) {
  unadj <- results_list$unadjusted %>% filter(term == "cancer_binary")
  adj   <- results_list$adjusted %>% filter(term == "cancer_binary")
  cat(results_list$vaccine_name, "\n")
  cat("  Unadjusted OR:", sprintf("%.2f (%.2f-%.2f), p=%.4f\n", unadj$estimate, unadj$conf.low, unadj$conf.high, unadj$p.value))
  cat("  Adjusted OR:  ", sprintf("%.2f (%.2f-%.2f), p=%.4f\n", adj$estimate, adj$conf.low, adj$conf.high, adj$p.value))
}

print_summary(flu_results)
print_summary(covid_results)
print_summary(tetanus_results)
print_summary(pneumo_results)
print_summary(hpv_results)
print_summary(shingles_results)

cat("\n✅ 07_results_tables.R complete — pipeline finished.\n")