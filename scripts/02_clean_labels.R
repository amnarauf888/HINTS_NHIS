# ============================================================
# 02_clean_labels.R
# Purpose: Filter to analysis population and recode raw survey
#          codes into labeled, analysis-ready variables
# Input:   HINTS6_raw, Adult_NHIS_raw (from 01_import.R)
# Output:  HINTS6_clean, NHI22_Adult_clean
# ============================================================

# ---- Make sure raw data exists ----
source(here("scripts", "01_import.R"))

library(dplyr)

# ============================================================
# HINTS6: keep only respondents with a clear cancer history answer
# ============================================================
HINTS6_clean <- HINTS6_raw %>%
  filter(EverHadCancer %in% c('Yes', 'No'))

# ============================================================
# NHIS Adult: keep only respondents with a clear cancer history answer
# ============================================================
NHI22_Adult_clean <- Adult_NHIS_raw %>%
  filter(CANEV_A %in% c(1, 2))

# ============================================================
# NHIS Adult: create 5-category age group (to match HINTS structure)
# ============================================================
NHI22_Adult_clean <- NHI22_Adult_clean %>%
  mutate(
    age_group = case_when(
      AGEP_A >= 18 & AGEP_A <= 34 ~ "18-34",
      AGEP_A >= 35 & AGEP_A <= 49 ~ "35-49",
      AGEP_A >= 50 & AGEP_A <= 64 ~ "50-64",
      AGEP_A >= 65 & AGEP_A <= 74 ~ "65-74",
      AGEP_A >= 75 ~ "75+",
      TRUE ~ NA_character_
    )
  )

# ============================================================
# NHIS Adult: recode all analysis variables
# ============================================================
NHI22_Adult_clean <- NHI22_Adult_clean %>%
  mutate(
    # ---- Binary outcomes ----
    flu_binary = case_when(
      SHTFLU12M_A == 1 ~ 1,
      SHTFLU12M_A == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    cancer_binary = case_when(
      CANEV_A == 1 ~ 1,
      CANEV_A == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    covid_binary = case_when(
      SHTCVD191_A == 1 ~ 1,
      SHTCVD191_A == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    tetanus_binary = case_when(
      SHTTETANUS_A == 1 ~ 1,
      SHTTETANUS_A == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    pneumo_binary = case_when(
      SHTPNUEV_A == 1 ~ 1,
      SHTPNUEV_A == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    hpv_binary = case_when(
      SHTHPV_A == 1 ~ 1,
      SHTHPV_A == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    shingles_binary = case_when(
      SHTSHINGL1_A == 1 ~ 1,
      SHTSHINGL1_A == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # ---- Sex ----
    sex = case_when(
      SEX_A == 1 ~ "Male",
      SEX_A == 2 ~ "Female",
      TRUE ~ NA_character_
    ),
    sex = factor(sex, levels = c("Male", "Female")),
    
    # ---- Race/ethnicity ----
    race = case_when(
      RACEALLP_A == 1 ~ "White",
      RACEALLP_A == 2 ~ "Black/African American",
      RACEALLP_A == 3 ~ "American Indian/Alaska Native",
      RACEALLP_A == 4 ~ "American Indian/Alaska Native",
      RACEALLP_A == 5 ~ "Native Hawaiian/Pacific Islander",
      RACEALLP_A == 6 ~ "Native Hawaiian/Pacific Islander",
      RACEALLP_A == 7 ~ "Asian",
      RACEALLP_A == 8 ~ "Other race",
      RACEALLP_A %in% c(97, 99) ~ NA_character_,
      TRUE ~ NA_character_
    ),
    race = factor(race, levels = c("White", "Black/African American",
                                   "Asian", "American Indian/Alaska Native",
                                   "Native Hawaiian/Pacific Islander", "Other race")),
    
    # ---- Employment status ----
    empl_stat = case_when(
      EMPLASTWK_A == 1 ~ "Employed",
      EMPLASTWK_A == 2 ~ "Not employed",
      EMPLASTWK_A %in% c(7, 9) ~ NA_character_,
      TRUE ~ NA_character_
    ),
    empl_stat = factor(empl_stat, levels = c("Employed", "Not employed")),
    
    # ---- Age group as ordered factor ----
    age_group = factor(age_group, levels = c("18-34", "35-49", "50-64", "65-74", "75+")),
    
    # ---- Health insurance ----
    health_insurance = case_when(
      NOTCOV_A == 1 ~ "No",
      NOTCOV_A == 2 ~ "Yes",
      NOTCOV_A %in% c(7, 9) ~ NA_character_,
      TRUE ~ NA_character_
    ),
    health_insurance = factor(health_insurance, levels = c("Yes", "No")),
    
    # ---- Income: binary at ~$45k (~200% FPL) ----
    income_45k = case_when(
      RATCAT_A %in% c(1, 2, 3, 4, 5, 6, 7) ~ "Less than $45,000",
      RATCAT_A %in% c(8, 9, 10, 11, 12, 13, 14) ~ "$45,000 or more",
      RATCAT_A == 98 ~ NA_character_,
      TRUE ~ NA_character_
    ),
    income_45k = factor(income_45k, levels = c("Less than $45,000", "$45,000 or more")),
    
    # ---- Income: detailed poverty ratio categories ----
    income_poverty_cat = case_when(
      RATCAT_A %in% c(1, 2, 3) ~ "<100% FPL",
      RATCAT_A %in% c(4, 5, 6, 7) ~ "100-199% FPL",
      RATCAT_A %in% c(8, 9) ~ "200-299% FPL",
      RATCAT_A %in% c(10, 11, 12, 13, 14) ~ "≥300% FPL",
      RATCAT_A == 98 ~ NA_character_,
      TRUE ~ NA_character_
    ),
    income_poverty_cat = factor(income_poverty_cat,
                                levels = c("<100% FPL", "100-199% FPL",
                                           "200-299% FPL", "≥300% FPL"))
  )

# ============================================================
# NHIS Adult: keep only complete cases on core covariates
# (vaccine-specific variables like HPV/shingles are NOT required
#  here, since those apply only to age-restricted subgroups later)
# ============================================================
NHI22_Adult_clean <- NHI22_Adult_clean %>%
  filter(
    !is.na(flu_binary),
    !is.na(cancer_binary),
    !is.na(sex),
    !is.na(race),
    !is.na(age_group),
    !is.na(empl_stat),
    !is.na(health_insurance),
    !is.na(income_45k)
  )

# ---- Quick sanity check on row counts ----
cat("HINTS6_clean rows:", nrow(HINTS6_clean), "\n")
cat("NHI22_Adult_clean rows:", nrow(NHI22_Adult_clean), "\n")