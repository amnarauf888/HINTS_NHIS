# ============================================================
# 04_qc_checks.R
# Purpose: Validate that filtering and recoding in 02_clean_labels.R
#          did what we expect, before anything gets analyzed
# Input:   HINTS6_raw, HINTS6_clean, Adult_NHIS_raw, NHI22_Adult_clean
#          (from 03_derive_survey_design.R, which cascades back
#           through 02 and 01)
# Output:  none saved — this script prints checks to the console
# ============================================================

source(here("scripts", "03_derive_survey_design.R"))

library(dplyr)

# ============================================================
# CHECK 1: Did the cancer-history filter behave as expected?
# Compare raw vs. filtered counts side by side
# ============================================================
cat("---- HINTS6: EverHadCancer, before filtering ----\n")
print(HINTS6_raw %>% count(EverHadCancer, sort = TRUE))

cat("\n---- HINTS6: EverHadCancer, after filtering ----\n")
print(HINTS6_clean %>% count(EverHadCancer, sort = TRUE))

cat("\n---- NHIS Adult: CANEV_A, before filtering ----\n")
print(Adult_NHIS_raw %>% count(CANEV_A, sort = TRUE))

cat("\nHINTS6_clean rows:", nrow(HINTS6_clean), "\n")
cat("NHI22_Adult_clean rows:", nrow(NHI22_Adult_clean), "\n")

# ============================================================
# CHECK 2: Did age_group get built correctly?
# Manually eyeball a random sample against the source variable
# ============================================================
cat("\n---- Random sample: AGEP_A vs. derived age_group ----\n")
print(
  NHI22_Adult_clean %>%
    select(AGEP_A, age_group) %>%
    slice_sample(n = 10)
)

# ============================================================
# CHECK 3: Any unexpected missingness in key recoded variables?
# useNA = "ifany" surfaces NAs that a plain table() would hide
# ============================================================
cat("\n---- Health insurance ----\n")
print(table(NHI22_Adult_clean$health_insurance, useNA = "ifany"))

cat("\n---- Income (binary at $45k) ----\n")
print(table(NHI22_Adult_clean$income_45k, useNA = "ifany"))

cat("\n---- Income (poverty ratio categories) ----\n")
print(table(NHI22_Adult_clean$income_poverty_cat, useNA = "ifany"))

# ============================================================
# CHECK 4: Do related variables cross-tabulate sensibly?
# (a sanity check, not a hypothesis test)
# ============================================================
cat("\n---- Cross-tab: income x insurance ----\n")
print(table(NHI22_Adult_clean$income_45k, NHI22_Adult_clean$health_insurance))

cat("\n---- Cross-tab: poverty category x cancer status ----\n")
print(table(NHI22_Adult_clean$income_poverty_cat, NHI22_Adult_clean$cancer_binary))

cat("\n✅ QC checks complete — review output above for anything unexpected.\n")