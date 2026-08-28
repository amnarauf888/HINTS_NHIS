# Derived Variable Descriptions (Claude-drafted)

**flu_binary**: flu_binary equals 1 if SHTFLU12M_A equals 1 (received flu vaccine in past 12 months), equals 0 if SHTFLU12M_A equals 2 (did not receive flu vaccine), and is otherwise set to missing (NA).

**cancer_binary**: cancer_binary: This variable recoded from CANEV_A is set to 1 if CANEV_A equals 1 (indicating a history of cancer), set to 0 if CANEV_A equals 2 (indicating no history of cancer), and set to missing (NA) for all other values.

**age_group**: This variable categorizes respondents' age (AGEP_A) into six-band groups—18-34, 35-49, 50-64, 65-74, and 75+—to allow analysis of survey responses by broad age ranges.

**health_insurance**: This variable indicates whether the respondent was uninsured at the time of the survey, recoded from NOTCOV_A so that a value of 1 becomes "No" (has coverage), a value of 2 becomes "Yes" (uninsured), and responses of 7 (Refused) or 9 (Don't Know) are recoded as missing (NA).

**income_45k**: RATCAT_A categories 1–7 are recoded as "Less than $45,000," and categories 8–14 are recoded as "$45,000 or more," to create the derived variable income_45k.

