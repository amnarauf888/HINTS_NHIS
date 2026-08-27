# ============================================================
# 01_import.R
# Purpose: Load raw HINTS6 and NHIS 2022 data into R
# Output:  HINTS6_raw, Adult_NHIS_raw, Child_NHIS_raw
# ============================================================

# ---- Load packages ----
library(here)   # for reliable file paths that work on any machine
library(dplyr)  # for basic data wrangling used later

# ---- Load HINTS 6 (2022) data ----
load(here("data", "raw", "hints6_public.rda"))
HINTS6_raw <- public

# ---- Load NHIS (2022) Adult data ----
Adult_NHIS_raw <- read.csv(here("data", "raw", "adult22.csv"))

# ---- Load NHIS (2022) Child data ----
Child_NHIS_raw <- read.csv(here("data", "raw", "child22.csv"))

# ---- Check dimensions ----
dim(HINTS6_raw)
dim(Adult_NHIS_raw)
dim(Child_NHIS_raw)


