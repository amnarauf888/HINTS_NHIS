# ============================================================
# 03_derive_survey_design.R
# Purpose: Build every survey design object ONCE, so no other
#          script has to construct or update() one again
# Input:   HINTS6_clean, NHI22_Adult_clean (from 02_clean_recode.R)
# Output:  svy_hints, svy_nhis, svy_nhis_hpv, svy_nhis_shingles,
#          svy_cancer
# ============================================================

# ---- Make sure clean data exists ----
source(here("scripts", "02_clean_labels.R"))

library(survey)
library(srvyr)

# ============================================================
# HINTS6 survey design (replicate weights, jackknife)
# ============================================================
svy_hints <- as_survey_rep(
  .data       = HINTS6_clean,
  weights     = PERSON_FINWT0,
  repweights  = num_range(prefix = "PERSON_FINWT", range = 1:50),
  type        = "JKn",
  scale       = 0.98,
  rscales     = rep(1, times = 50)
)

# ============================================================
# NHIS survey design (main design, used for all vaccine models)
# ============================================================
svy_nhis <- as_survey_design(
  .data   = NHI22_Adult_clean,
  ids     = PPSU,
  strata  = PSTRAT,
  weights = WTFA_A,
  nest    = TRUE
)

# ============================================================
# Age-restricted subsets, built FROM svy_nhis (not rebuilt from scratch)
# ============================================================

# HPV vaccine: ages 18-64 only
svy_nhis_hpv <- svy_nhis %>%
  filter(AGEP_A <= 64)

# Shingles vaccine: ages 50+ only
svy_nhis_shingles <- svy_nhis %>%
  filter(AGEP_A >= 50)

# ============================================================
# Cancer-patients-only subset, for Question 2 (predictors among
# cancer patients specifically)
# ============================================================
svy_cancer <- svy_nhis %>%
  filter(cancer_binary == 1)

# ---- Quick sanity check ----
cat("svy_nhis unweighted n:        ", nrow(svy_nhis), "\n")
cat("svy_nhis_hpv unweighted n:    ", nrow(svy_nhis_hpv), "\n")
cat("svy_nhis_shingles unweighted n:", nrow(svy_nhis_shingles), "\n")
cat("svy_cancer unweighted n:      ", nrow(svy_cancer), "\n")