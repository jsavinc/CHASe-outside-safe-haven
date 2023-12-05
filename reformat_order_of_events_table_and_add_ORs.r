
# Preamble ----------------------------------------------------------------

# Script to reformat order of events table & add ORs and CIs
# For reporting as Table 3 in the results paper 1

library(tidyverse)  # for data manipulation
library(openxlsx)  # for writing excel files
library(readxl)  # for reading excel files



# Load data ---------------------------------------------------------------

tbl_order_of_events <- 
  read_excel(
    path = "X:/R1369/CSO FULL grant/Safe Haven Exports/2023-10-20 relative order & regressions continued/relative_order_first_adverse_or_mh_event_before_18_and_lifetime_corrected.xlsx",
    # path = "X:/R1369/CSO FULL grant/Safe Haven Exports/2022-02-02/relative_order_first_adverse_or_mh_event_before_18_and_lifetime.xlsx",  # that's the older, incorrect version!
    sheet = 1
    ) %>%
  filter(!str_starts(case, pattern= "Frequency of"))  # remove footnote

# Helper functions --------------------------------------------------------

## this assumes you have a n_case, denominator_case and n_control, denominator_control variables
add_odds_ratio_and_ci <- function(data_tbl) {
  data_tbl %>%
    mutate(
      or = (n_case/(denominator_case-n_case))/(n_control/(denominator_control-n_control)),
      or_low = exp(log(or)+(qnorm(0.025)*sqrt(1/n_case+1/(denominator_case-n_case)+1/(n_control)+1/(denominator_control-denominator_case)))),
      or_high = exp(log(or)+(qnorm(0.975)*sqrt(1/n_case+1/(denominator_case-n_case)+1/(n_control)+1/(denominator_control-denominator_case)))),
      or_formatted = paste0(  # construct a neat OR with CI, e.g. 1.23 [0.95-1.95]
        format(or, nsmall = 2, digits = 2, trim = TRUE),
        " [",
        format(or_low, nsmall = 2, digits = 2, trim = TRUE),
        "-",
        format(or_high, nsmall = 2, digits = 2, trim = TRUE),
        "]"
      )
    )
}



# Reformat table ----------------------------------------------------------

tbl_reformated <- 
  tbl_order_of_events %>%
  mutate(across(c(n_under_18, n_lifetime, denominator), as.integer)) %>%
  select(-c(n_lifetime, n_prop_lifetime, max_days_apart)) %>%  # only reporting <18 data, not lifetime data
  rename(n = n_under_18, n_prop = n_prop_under_18) %>%
  mutate(  # suppress small numbers
    n = if_else(
      order == "mh, adversity",
      true = round(n, digits = -1),  # round to nearest 10
      false = n
    ),
    order = if_else(
      order == "mh, adversity",
      true = "mh, adversity*",  # add asterisk so I can explain rounding in footnote
      false = order
    ),
    n_prop = glue::glue("{scales::comma(n)} ({scales::label_percent(accuracy = 0.1)(n/denominator)})"),
  ) %>%
  pivot_wider(names_from = case, values_from = c(n, denominator, n_prop)) %>%
  add_odds_ratio_and_ci

tbl_clean <-
  tbl_reformated %>%
  select(-c(n_case, n_control, denominator_case, denominator_control, or, or_low, or_high)) %>%
  mutate(
    sex = factor(sex, levels = c("both", "male", "female"), labels = c("Both", "Male", "Female")),
    # order = factor(order, levels = unique(order), labels = c("Adversity, then MH", "MH, then Adversity", "Both simultaneously", "Adversity only", "MH only"))
    order = factor(order, levels = unique(order), labels = c("Adversity, then MH", "MH, then Adversity*", "Both simultaneously", "Adversity only", "MH only"))  # added asterisk to MH, then adversity
  ) %>%
  arrange(
    sex, order
  )

# Save new table ----------------------------------------------------------

## Note: 28 days apart means that events were considered simultaneous if they
## happened within 28 days of each other

tbl_clean %>%
  write.xlsx(x = ., file = "C:/Users/40011625/OneDrive - Edinburgh Napier University/CHASE Publications/Results paper 1/Table_order_of_events_under_18_raw.xlsx")
  