# ============================================================
# 05_descriptive_tables.R
# Purpose: Build all descriptive (non-modeling) tables:
#          Table 1 for HINTS6, HPV beliefs by cancer status,
#          Table 1 for NHIS, and vaccine uptake by cancer status
# Input:   svy_hints, svy_nhis, svy_nhis_hpv, svy_nhis_shingles
#          (from 04_qc_checks.R, cascading through 03 -> 02 -> 01)
# Output:  HTML/PNG/DOCX tables saved to /output
# ============================================================

source(here("scripts", "04_qc_checks.R"))

library(dplyr)
library(srvyr)
library(tidyr)
library(gt)

# ============================================================
# TABLE 1: HINTS6 sociodemographics by cancer status
# ============================================================

create_table1 <- function(svy_obj, var_name, var_label) {
  svy_obj %>%
    group_by(across(all_of(var_name)), EverHadCancer) %>%
    summarize(
      n = unweighted(n()),
      weighted_pct = survey_mean(na.rm = TRUE) * 100
    ) %>%
    filter(!is.na(!!sym(var_name)), !is.na(EverHadCancer)) %>%
    mutate(
      Variable = var_label,
      Category = as.character(!!sym(var_name)),
      weighted_pct_formatted = sprintf("%.1f%% (±%.1f)", weighted_pct, weighted_pct_se)
    ) %>%
    ungroup() %>%
    select(Variable, Category, EverHadCancer, n, weighted_pct, weighted_pct_se, weighted_pct_formatted)
}

table1_gender <- create_table1(svy_hints, "BirthGender", "Gender")
table1_age    <- create_table1(svy_hints, "AgeGrpB", "Age group")
table1_edu    <- create_table1(svy_hints, "EducA", "Education")
table1_race   <- create_table1(svy_hints, "RaceEthn5", "Race/Ethnicity")
table1_income <- create_table1(svy_hints, "HHInc", "Household income")
table1_region <- create_table1(svy_hints, "CENSREG", "Census region")

table1_hints_complete <- bind_rows(
  table1_gender, table1_age, table1_edu,
  table1_race, table1_income, table1_region
) %>%
  ungroup()

sample_size_hints <- table1_hints_complete %>%
  filter(
    Variable == "Gender",
    !grepl("Missing|Multiple|Unreadable|Never Seen", Category, ignore.case = TRUE)
  ) %>%
  summarize(total = sum(n)) %>%
  pull(total) %>%
  as.numeric()

table1_hints_final <- table1_hints_complete %>%
  filter(!grepl("Missing|Multiple|Unreadable|Never Seen", Category, ignore.case = TRUE)) %>%
  select(Variable, Category, EverHadCancer, n, weighted_pct) %>%
  pivot_wider(names_from = EverHadCancer, values_from = c(n, weighted_pct), names_sep = "_") %>%
  mutate(
    Yes_Cancer = sprintf("%d (%.1f%%)", n_Yes, weighted_pct_Yes),
    No_Cancer  = sprintf("%d (%.1f%%)", n_No, weighted_pct_No),
    Variable = as.character(Variable),
    Category = as.character(Category)
  ) %>%
  select(Variable, Category, Yes_Cancer, No_Cancer)

table1_hints_gt <- table1_hints_final %>%
  gt(groupname_col = "Variable") %>%
  tab_header(
    title = md("**Table 1: Sociodemographic Characteristics by Cancer Status**"),
    subtitle = paste0("Health Information National Trends Survey (HINTS 6) (n=",
                      format(sample_size_hints, big.mark = ","), ")")
  ) %>%
  cols_label(
    Category = md("**Variables**"),
    Yes_Cancer = md("**Yes**<br>(Ever had cancer)<br>n (%)"),
    No_Cancer = md("**No**<br>(Never had cancer)<br>n (%)")
  ) %>%
  tab_style(style = list(cell_fill(color = "#E8E8E8"), cell_text(weight = "bold", size = px(13))),
            locations = cells_row_groups()) %>%
  tab_style(style = cell_text(align = "center"), locations = cells_column_labels(columns = c(Yes_Cancer, No_Cancer))) %>%
  tab_style(style = cell_text(align = "center"), locations = cells_body(columns = c(Yes_Cancer, No_Cancer))) %>%
  tab_options(
    table.font.size = 11, heading.title.font.size = 14, heading.subtitle.font.size = 11,
    row_group.font.weight = "bold", column_labels.font.weight = "bold"
  ) %>%
  tab_footnote(
    footnote = "Percentages are weighted to represent the U.S. adult population.",
    locations = cells_title(groups = "subtitle")
  ) %>%
  tab_source_note(source_note = "Missing data excluded from analysis.")

gtsave(table1_hints_gt, here("output", "table1_HINTS6.html"))
gtsave(table1_hints_gt, here("output", "table1_HINTS6.png"))
gtsave(table1_hints_gt, here("output", "table1_HINTS6.docx"))

# ============================================================
# HPV BELIEFS by cancer status (HINTS6)
# ============================================================

create_hpv_belief_table <- function(svy_obj, belief_var, belief_label) {
  svy_obj %>%
    group_by(EverHadCancer, !!sym(belief_var)) %>%
    summarize(
      n = unweighted(n()),
      weighted_pct = survey_mean(na.rm = TRUE) * 100
    ) %>%
    filter(!is.na(!!sym(belief_var)), !is.na(EverHadCancer)) %>%
    ungroup() %>%
    mutate(Question = belief_label, Response = as.character(!!sym(belief_var))) %>%
    select(Question, Response, EverHadCancer, n, weighted_pct, weighted_pct_se)
}

create_hpv_cervical_table <- function(svy_obj, belief_label) {
  svy_obj %>%
    filter(!is.na(HeardHPV), HeardHPV == "Yes" | HeardHPV == 1 | HeardHPV == "1") %>%
    group_by(EverHadCancer, HPVCauseCancer_Cervical) %>%
    summarize(
      n = unweighted(n()),
      weighted_pct = survey_mean(na.rm = TRUE) * 100
    ) %>%
    filter(!is.na(HPVCauseCancer_Cervical), !is.na(EverHadCancer)) %>%
    ungroup() %>%
    mutate(Question = belief_label, Response = as.character(HPVCauseCancer_Cervical)) %>%
    select(Question, Response, EverHadCancer, n, weighted_pct, weighted_pct_se)
}

hpv_heard   <- create_hpv_belief_table(svy_hints, "HeardHPV", "Heard of HPV")
hpv_cause   <- create_hpv_cervical_table(svy_hints, "HPV causes cervical cancer (among those who heard of HPV)")
hpv_vaccine <- create_hpv_belief_table(svy_hints, "HeardHPVVaccine2", "Heard of HPV vaccine")

hpv_beliefs_complete <- bind_rows(hpv_heard, hpv_cause, hpv_vaccine) %>% ungroup()

hpv_beliefs_clean <- hpv_beliefs_complete %>%
  filter(
    !grepl("Missing|Multiple|Unreadable|Never Seen|Inapplicable|Commission Error", Response, ignore.case = TRUE),
    !grepl("Missing|Multiple|Unreadable|Never Seen", as.character(EverHadCancer), ignore.case = TRUE)
  ) %>%
  mutate(
    Response = case_when(
      grepl("^1$|^Yes$", Response, ignore.case = TRUE) ~ "Yes",
      grepl("^2$|^No$", Response, ignore.case = TRUE) ~ "No",
      grepl("Not sure", Response, ignore.case = TRUE) ~ "Not sure",
      TRUE ~ Response
    ),
    EverHadCancer = case_when(
      grepl("^1$|^Yes$", as.character(EverHadCancer), ignore.case = TRUE) ~ "Yes",
      grepl("^2$|^No$", as.character(EverHadCancer), ignore.case = TRUE) ~ "No",
      TRUE ~ as.character(EverHadCancer)
    )
  )

hpv_beliefs_wide <- hpv_beliefs_clean %>%
  select(Question, Response, EverHadCancer, n, weighted_pct) %>%
  pivot_wider(id_cols = c(Question, Response), names_from = EverHadCancer,
              values_from = c(n, weighted_pct), names_sep = "_")

hpv_beliefs_final <- hpv_beliefs_wide %>%
  mutate(
    Cancer_Yes = sprintf("%d (%.1f%%)", n_Yes, weighted_pct_Yes),
    Cancer_No  = sprintf("%d (%.1f%%)", n_No, weighted_pct_No)
  ) %>%
  select(Question, Response, Cancer_Yes, Cancer_No) %>%
  filter(!is.na(Cancer_Yes) & !is.na(Cancer_No))

sample_size_hpv <- hpv_beliefs_complete %>%
  filter(
    Question == "Heard of HPV",
    !grepl("Missing|Multiple|Unreadable|Never Seen|Inapplicable|Commission Error", Response, ignore.case = TRUE)
  ) %>%
  summarize(total = sum(n)) %>%
  pull(total) %>%
  as.numeric()

hpv_beliefs_gt <- hpv_beliefs_final %>%
  gt(groupname_col = "Question") %>%
  tab_header(
    title = md("**HPV Beliefs by Cancer Status**"),
    subtitle = paste0("Health Information National Trends Survey (HINTS 6) (n=", format(sample_size_hpv, big.mark = ","), ")")
  ) %>%
  cols_label(
    Response = md("**Response**"),
    Cancer_Yes = md("**Yes**<br>(Ever had cancer)<br>n (%)"),
    Cancer_No = md("**No**<br>(Never had cancer)<br>n (%)")
  ) %>%
  tab_style(style = list(cell_fill(color = "#E8E8E8"), cell_text(weight = "bold", size = px(13))), locations = cells_row_groups()) %>%
  tab_style(style = cell_text(align = "center"), locations = cells_column_labels(columns = c(Cancer_Yes, Cancer_No))) %>%
  tab_style(style = cell_text(align = "center"), locations = cells_body(columns = c(Cancer_Yes, Cancer_No))) %>%
  tab_options(
    table.font.size = 11, heading.title.font.size = 14, heading.subtitle.font.size = 11,
    row_group.font.weight = "bold", column_labels.font.weight = "bold"
  ) %>%
  tab_footnote(
    footnote = "Percentages are weighted to represent the U.S. adult population. Percentages within each cancer status group sum to 100%. For 'HPV causes cervical cancer', percentages are calculated only among respondents who had heard of HPV.",
    locations = cells_title(groups = "subtitle")
  ) %>%
  tab_source_note(source_note = "Missing data and non-response categories excluded from analysis.")

gtsave(hpv_beliefs_gt, here("output", "hpv_beliefs_by_cancer.html"))
gtsave(hpv_beliefs_gt, here("output", "hpv_beliefs_by_cancer.png"))
gtsave(hpv_beliefs_gt, here("output", "hpv_beliefs_by_cancer.docx"))

# ---- Output-validation check: promised from 04 — do percentages sum to ~100%? ----
cat("\n---- Verification: HPV belief percentages should sum to ~100% per group ----\n")
print(
  hpv_beliefs_clean %>%
    group_by(Question, EverHadCancer) %>%
    summarize(total_pct = sum(weighted_pct), .groups = "drop") %>%
    arrange(Question, EverHadCancer)
)

# ============================================================
# TABLE 1: NHIS sociodemographics by cancer status
# ============================================================

create_table1_nhis <- function(svy_obj, var_name, var_label) {
  svy_obj %>%
    group_by(across(all_of(var_name)), cancer_binary) %>%
    summarize(
      n = unweighted(n()),
      weighted_pct = survey_mean(na.rm = TRUE) * 100
    ) %>%
    filter(!is.na(!!sym(var_name)), !is.na(cancer_binary)) %>%
    mutate(
      Variable = var_label,
      Category = as.character(!!sym(var_name)),
      weighted_pct_formatted = sprintf("%.1f%% (±%.1f)", weighted_pct, weighted_pct_se)
    ) %>%
    ungroup() %>%
    select(Variable, Category, cancer_binary, n, weighted_pct, weighted_pct_se, weighted_pct_formatted)
}

table1_sex_nhis       <- create_table1_nhis(svy_nhis, "sex", "Sex")
table1_age_nhis       <- create_table1_nhis(svy_nhis, "age_group", "Age group")
table1_race_nhis      <- create_table1_nhis(svy_nhis, "race", "Race/Ethnicity")
table1_empl_nhis      <- create_table1_nhis(svy_nhis, "empl_stat", "Employment status")
table1_insurance_nhis <- create_table1_nhis(svy_nhis, "health_insurance", "Health insurance coverage")
table1_income_nhis    <- create_table1_nhis(svy_nhis, "income_45k", "Household income")

table1_nhis_complete <- bind_rows(
  table1_sex_nhis, table1_age_nhis, table1_race_nhis,
  table1_empl_nhis, table1_insurance_nhis, table1_income_nhis
) %>%
  ungroup()

sample_size_nhis_t1 <- table1_nhis_complete %>%
  filter(
    Variable == "Sex",
    !cancer_binary %in% c("Refused", "Don't Know"),
    !Category %in% c("Refused", "Don't Know")
  ) %>%
  summarize(total = sum(n)) %>%
  pull(total) %>%
  as.numeric()

table1_nhis_final <- table1_nhis_complete %>%
  filter(
    !cancer_binary %in% c("Refused", "Don't Know"),
    !Category %in% c("Refused", "Don't Know", "Not Ascertained", NA)
  ) %>%
  mutate(
    cancer_binary = case_when(
      cancer_binary == 1 ~ "Yes",
      cancer_binary == 0 ~ "No",
      TRUE ~ as.character(cancer_binary)
    )
  ) %>%
  select(Variable, Category, cancer_binary, n, weighted_pct) %>%
  pivot_wider(names_from = cancer_binary, values_from = c(n, weighted_pct), names_sep = "_") %>%
  mutate(
    Yes_Cancer = sprintf("%d (%.1f%%)", n_Yes, weighted_pct_Yes),
    No_Cancer  = sprintf("%d (%.1f%%)", n_No, weighted_pct_No),
    Variable = as.character(Variable),
    Category = as.character(Category)
  ) %>%
  select(Variable, Category, Yes_Cancer, No_Cancer)

table1_nhis_gt <- table1_nhis_final %>%
  gt(groupname_col = "Variable") %>%
  tab_header(
    title = md("**Table 1: Sociodemographic Characteristics by Cancer Status**"),
    subtitle = paste0("National Health Interview Survey (NHIS) 2022 (n=", format(sample_size_nhis_t1, big.mark = ","), ")")
  ) %>%
  cols_label(
    Category = md("**Variables**"),
    Yes_Cancer = md("**Yes**<br>(Ever had cancer)<br>n (%)"),
    No_Cancer = md("**No**<br>(Never had cancer)<br>n (%)")
  ) %>%
  tab_style(style = list(cell_fill(color = "#E8E8E8"), cell_text(weight = "bold", size = px(13))), locations = cells_row_groups()) %>%
  tab_style(style = cell_text(align = "center"), locations = cells_column_labels(columns = c(Yes_Cancer, No_Cancer))) %>%
  tab_style(style = cell_text(align = "center"), locations = cells_body(columns = c(Yes_Cancer, No_Cancer))) %>%
  tab_options(
    table.font.size = 11, heading.title.font.size = 14, heading.subtitle.font.size = 11,
    row_group.font.weight = "bold", column_labels.font.weight = "bold"
  ) %>%
  tab_footnote(
    footnote = "Income categories based on ratio to federal poverty level (FPL); approximately <$45,000 represents <200% FPL and \u2265$45,000 represents \u2265200% FPL for average family size.",
    locations = cells_title(groups = "subtitle")
  ) %>%
  tab_source_note(source_note = "Missing data excluded from analysis.")

gtsave(table1_nhis_gt, here("output", "table1_NHIS2022.html"))
gtsave(table1_nhis_gt, here("output", "table1_NHIS2022.png"))
gtsave(table1_nhis_gt, here("output", "table1_NHIS2022.docx"))

# ============================================================
# VACCINE UPTAKE by cancer status (NHIS), all response categories
# ============================================================

create_vaccine_table <- function(svy_obj, vaccine_var, vaccine_label) {
  svy_obj %>%
    group_by(CANEV_A, !!sym(vaccine_var)) %>%
    summarize(
      n = unweighted(n()),
      weighted_pct = survey_mean(na.rm = TRUE) * 100
    ) %>%
    filter(!is.na(!!sym(vaccine_var)), !is.na(CANEV_A)) %>%
    ungroup() %>%
    mutate(Vaccine = vaccine_label, Response = as.character(!!sym(vaccine_var))) %>%
    select(Vaccine, Response, CANEV_A, n, weighted_pct, weighted_pct_se)
}

vaccine_flu      <- create_vaccine_table(svy_nhis, "SHTFLU12M_A", "Influenza vaccine (past 12 months)")
vaccine_covid    <- create_vaccine_table(svy_nhis, "SHTCVD191_A", "COVID-19 vaccine (at least 1 dose)")
vaccine_tetanus  <- create_vaccine_table(svy_nhis, "SHTTETANUS_A", "Tetanus shot (past 10 years)")
vaccine_pneumo   <- create_vaccine_table(svy_nhis, "SHTPNUEV_A", "Pneumonia vaccine (ever)")
vaccine_hpv      <- create_vaccine_table(svy_nhis_hpv, "SHTHPV_A", "HPV vaccine (ages 18-64)")
vaccine_shingles <- create_vaccine_table(svy_nhis_shingles, "SHTSHINGL1_A", "Shingles vaccine (ages 50+)")

vaccine_complete <- bind_rows(
  vaccine_flu, vaccine_covid, vaccine_tetanus,
  vaccine_pneumo, vaccine_hpv, vaccine_shingles
) %>%
  ungroup()

vaccine_labeled <- vaccine_complete %>%
  mutate(
    Response = case_when(
      Response == "1" ~ "Yes",
      Response == "2" ~ "No",
      Response == "7" ~ "Refused",
      Response == "8" ~ "Not Ascertained",
      Response == "9" ~ "Don't Know",
      TRUE ~ Response
    ),
    CANEV_A = case_when(
      as.character(CANEV_A) == "1" ~ "Yes",
      as.character(CANEV_A) == "2" ~ "No",
      as.character(CANEV_A) == "7" ~ "Refused",
      as.character(CANEV_A) == "8" ~ "Not Ascertained",
      as.character(CANEV_A) == "9" ~ "Don't Know",
      TRUE ~ as.character(CANEV_A)
    )
  ) %>%
  filter(!CANEV_A %in% c("Refused", "Not Ascertained", "Don't Know"))

vaccine_wide <- vaccine_labeled %>%
  select(Vaccine, Response, CANEV_A, n, weighted_pct) %>%
  pivot_wider(id_cols = c(Vaccine, Response), names_from = CANEV_A, values_from = c(n, weighted_pct), names_sep = "_")

vaccine_final <- vaccine_wide %>%
  mutate(
    Cancer_Yes = sprintf("%d (%.1f%%)", n_Yes, weighted_pct_Yes),
    Cancer_No  = sprintf("%d (%.1f%%)", n_No, weighted_pct_No)
  ) %>%
  select(Vaccine, Response, Cancer_Yes, Cancer_No) %>%
  filter(!is.na(Cancer_Yes) & !is.na(Cancer_No))

sample_size_vaccine <- vaccine_complete %>%
  mutate(
    CANEV_A = case_when(
      as.character(CANEV_A) == "1" ~ "Yes",
      as.character(CANEV_A) == "2" ~ "No",
      as.character(CANEV_A) == "7" ~ "Refused",
      as.character(CANEV_A) == "8" ~ "Not Ascertained",
      as.character(CANEV_A) == "9" ~ "Don't Know",
      TRUE ~ as.character(CANEV_A)
    )
  ) %>%
  filter(
    Vaccine == "Influenza vaccine (past 12 months)",
    !CANEV_A %in% c("Refused", "Not Ascertained", "Don't Know")
  ) %>%
  summarize(total = sum(n)) %>%
  pull(total) %>%
  as.numeric()

vaccine_gt <- vaccine_final %>%
  gt(groupname_col = "Vaccine") %>%
  tab_header(
    title = md("**Table: Vaccine Uptake by Cancer History**"),
    subtitle = paste0("National Health Interview Survey (NHIS) 2022 (n=", format(sample_size_vaccine, big.mark = ","), ")")
  ) %>%
  cols_label(
    Response = md("**Response**"),
    Cancer_Yes = md("**Yes**<br>(Ever had cancer)<br>n (%)"),
    Cancer_No = md("**No**<br>(Never had cancer)<br>n (%)")
  ) %>%
  tab_style(style = list(cell_fill(color = "#E8E8E8"), cell_text(weight = "bold", size = px(13))), locations = cells_row_groups()) %>%
  tab_style(style = cell_text(align = "center"), locations = cells_column_labels(columns = c(Cancer_Yes, Cancer_No))) %>%
  tab_style(style = cell_text(align = "center"), locations = cells_body(columns = c(Cancer_Yes, Cancer_No))) %>%
  tab_style(
    style = cell_fill(color = "#FFF3CD"),
    locations = cells_body(rows = Response %in% c("Refused", "Not Ascertained", "Don't Know"))
  ) %>%
  tab_options(
    table.font.size = 11, heading.title.font.size = 14, heading.subtitle.font.size = 11,
    row_group.font.weight = "bold", column_labels.font.weight = "bold"
  ) %>%
  tab_footnote(
    footnote = "Percentages are weighted to represent the U.S. adult population. HPV vaccine limited to ages 18-64; Shingles vaccine limited to ages 50+.",
    locations = cells_title(groups = "subtitle")
  ) %>%
  tab_source_note(source_note = "Rows highlighted in yellow represent missing or non-response data.")

gtsave(vaccine_gt, here("output", "nhis_vaccine_uptake_by_cancer.html"))
gtsave(vaccine_gt, here("output", "nhis_vaccine_uptake_by_cancer.png"))
gtsave(vaccine_gt, here("output", "nhis_vaccine_uptake_by_cancer.docx"))

cat("\n✅ 05_descriptive_tables.R complete — 4 tables saved to /output.\n")
