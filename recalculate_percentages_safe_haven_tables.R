# Recalculate percentages so we have enough decimals for pedants

library(tidyverse)
library(readxl)
library(openxlsx)


tables_descriptives_cohort <- 
  read_excel(path = "../Safe Haven Exports/Descriptive summary of cohort & individuals with any records/Descriptive_summary_cohort_by_whether_they_had_any_records_prior_to_death.xlsx", 
  sheet = 1) %>%
  slice(-nrow(.)) %>%  # remove last row - contains footnote
  select(case,sex,has_no_records_prior_to_death, denominator=N)  # rename N to denominator for use later

urban_rural_tbl <- 
  read_excel(
    path = "../Safe Haven Exports/Descriptive summary of cohort & individuals with any records/Descriptive_summary_cohort_by_whether_they_had_any_records_prior_to_death.xlsx",
    sheet = "Urban-rural indicator"
    ) %>%
  slice(-nrow(.)) %>%
  pivot_longer(
    cols = matches("records"), 
    names_to = "case_and_records", 
    values_to = "n_prop"
    ) %>%
  extract(
    col = case_and_records, 
    into = c("case","has_no_records_prior_to_death", "sex"), 
    regex = "^(case|control)_(.*)_(both|male|female)$",
    remove = TRUE
  ) %>%
  mutate(
    has_no_records_prior_to_death = if_else(has_no_records_prior_to_death == "no_records", true = "Yes", false = "No"),
    n = str_extract(n_prop, pattern = "^\\d+\\s") %>% as.integer()  # extract N from n_prop
  ) %>%
  left_join(tables_descriptives_cohort) %>%  # find denominator
  mutate(n_prop = paste0(
    format(n, big.mark = ",", trim = TRUE),
    " (",
    scales::percent(n/denominator, accuracy = 0.1),
    ")"
  )) %>%
  arrange(case, has_no_records_prior_to_death, Indicator)
  
write_csv(x = urban_rural_tbl, file = "C:/Users/40011625/OneDrive - Edinburgh Napier University/CHASE Publications/Results paper 1/freq_urban_rural_by_hospital_records_or_none.csv")
