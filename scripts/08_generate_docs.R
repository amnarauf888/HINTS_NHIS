# ============================================================
# 08_generate_docs.R
# Purpose: Use the Claude API to draft plain-language codebook
#          descriptions for the derived variables created in
#          02_clean_labels.R.
#
# IMPORTANT: This script sends ONLY variable names and short
# text descriptions of recoding logic (already written as
# comments/strings below). It does NOT read, load, or send any
# respondent-level data from HINTS6_clean or NHI22_Adult_clean.
# ============================================================

library(tidyllm)

# ---- Recoding logic itfor each derived variable, as plain text ----
# (This is manually maintained — it should match 02_clean_labels.R)
variable_logic <- list(
  flu_binary        = "SHTFLU12M_A == 1 becomes 1 (received flu vaccine), == 2 becomes 0, else NA",
  cancer_binary     = "CANEV_A == 1 becomes 1 (has cancer history), == 2 becomes 0, else NA",
  age_group         = "AGEP_A grouped into bands: 18-34, 35-49, 50-64, 65-74, 75+",
  health_insurance  = "NOTCOV_A == 1 becomes 'No', == 2 becomes 'Yes', 7/9 (refused/don't know) become NA",
  income_45k        = "RATCAT_A categories 1-7 become 'Less than $45,000'; 8-14 become '$45,000 or more'"
)

# ---- Ask Claude to draft a one-sentence codebook description for each ----
draft_codebook_entry <- function(var_name, recode_logic) {
  prompt <- paste0(
    "Write a single, plain-language sentence for a codebook, describing ",
    "a derived survey variable named '", var_name, "'. ",
    "Here is exactly how it was constructed: ", recode_logic
  )
  llm_message(prompt) |> chat(claude()) |> get_reply()
}

codebook_drafts <- purrr::imap(variable_logic, draft_codebook_entry)

# ---- Print results to review before adding to real documentation ----
for (var_name in names(codebook_drafts)) {
  cat("**", var_name, "**\n", codebook_drafts[[var_name]], "\n\n", sep = "")
}

# ---- Save results to a file, not just the console ----
output_lines <- c("# Derived Variable Descriptions (Claude-drafted)", "")
for (var_name in names(codebook_drafts)) {
  output_lines <- c(output_lines, paste0("**", var_name, "**: ", codebook_drafts[[var_name]]), "")
}
writeLines(output_lines, here("docs", "derived_variable_descriptions.md"))
cat("\n✅ Saved descriptions to docs/derived_variable_descriptions.md\n")