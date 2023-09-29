## Reformatting odds ratio table for conditional regression models


# packages ----------------------------------------------------------------

library(tidyverse)

# load data ---------------------------------------------------------------

raw_model_m <- read_csv("X:/R1369/CSO FULL grant/Safe Haven Exports/2023-09-12 conditional logistic regressions/clogit_subcat_male_1.csv")
raw_model_f <- read_csv("X:/R1369/CSO FULL grant/Safe Haven Exports/2023-09-12 conditional logistic regressions/clogit_subcat_female_1.csv")

# 
# raw_model_m <- read_csv("X:/R1369/CSO FULL grant/Safe Haven Exports/2023-09-12 conditional logistic regressions/clogit_subcat_male_2.csv")
# raw_model_f <- read_csv("X:/R1369/CSO FULL grant/Safe Haven Exports/2023-09-12 conditional logistic regressions/clogit_subcat_female_2.csv")
# 

descriptives_simple <- read_csv("X:/R1369/CSO FULL grant/Safe Haven Exports/2023-09-12 conditional logistic regressions/regression_descriptives_adversities_simple.csv")
descriptives_subcat <- read_csv("X:/R1369/CSO FULL grant/Safe Haven Exports/2023-09-12 conditional logistic regressions/regression_descriptives_adversities_subcategorised.csv")

# constants ---------------------------------------------------------------

table_variable_names <-
  tribble(
    ~short_name, ~long_name,
    "n_episodes_mvr_under_18_factor", "MVR episodes, aged <18",
    "n_episodes_ccs_under_18_factor", "MH episodes, aged <18",
    # "n_episodes_schnitzer_without_caries_under_10_factor", "Episodes with codes suggestive of maltreatment or neglect (Schnitzer et al., 2011), excluding dental caries, aged <10",
    "n_episodes_schnitzer_without_caries_under_10_factor", "Episodes with codes suggestive of maltreatment or neglect, excluding dental caries, aged <10",
    "n_episodes_poisoning_under_18_factor", "Poisoning episodes, aged <18",
    "mother_died_before_age_18", "Maternal death, aged <18",
    "care_experienced_or_no_fixed_abode_before_age_18", "Episodes indicating care experience or no fixed abode, aged <18",
    "n_episodes_mvr_assault_maltreatment_under_18_factor", "MVR episodes: assault or maltreatment, aged <18",
    "n_episodes_mvr_social_or_undetermined_under_18_factor", "MVR episodes: adverse soc. circumstances or undetermined intent, aged <18",
    "n_episodes_ccs_self_harm_under_18_factor", "MH episodes: self harm, aged <18",
    "n_episodes_ccs_alcohol_under_18_factor", "MH episodes: alcohol-related, aged <18",
    "n_episodes_ccs_not_self_harm_or_alcohol_under_18_factor", "MH episodes: neither self harm nor alcohol-related, aged <18"
  )


# functions ---------------------------------------------------------------

format_ors <- function(ci_low, ci_high) {
  paste0(
    format(round(ci_low,2), nsmall=2), "-", format(round(ci_high,2), nsmall=2)
  )
}

format_p_value <- function(p_value) {
  if_else(
    condition = p_value < 0.0001,
    true = "<.0001",
    # false = Hmisc::format.pval(x = p_value, digits = 4, nsmall=2)
    false = as.character(rstatix::p_round(x = p_value, digits = 3))
  )
}

# wrangle data ------------------------------------------------------------

models_to_combine <-
  map(
    .x = list(raw_model_m, raw_model_f),
    .f = function(model) {
      model %>%
        mutate(
          or = round(or, 2) %>% format(., nsmall=1),
          or_ci = format_ors(ci_low = or_ci_low, ci_high = or_ci_high), 
          p.value = format_p_value(p.value)
        ) %>%
        select(variable, level, or, or_ci, p=p.value)
    }
  ) %>%
  set_names(c("m","f"))

combined_models <-
  full_join(
    x = models_to_combine$m,
    y = models_to_combine$f,
    by = join_by(variable, level),
    suffix = c("_m", "_f")
  ) %>%
  mutate(
    variable = table_variable_names$long_name[match(x = variable, table = table_variable_names$short_name)]
  )


# wrangle descriptives ----------------------------------------------------

descriptives <-
  bind_rows(
    descriptives_subcat,
    descriptives_simple %>% slice(10:17)
  ) %>%
  mutate(
    Variable = if_else(str_starts(Characteristic, pattern = "[:alpha:]"), Characteristic, NA_character_),
    Variable =table_variable_names$long_name[match(x = Variable, table = table_variable_names$short_name)],
    Levels = if_else(!str_starts(Characteristic, pattern = "[:alpha:]"), Characteristic, NA_character_)
    ) %>%
  fill(Variable, .direction = "down") %>%
  mutate(
    across(
      matches("p-value"), 
      ~if_else(!is.na(lag(.x, default = NA)), lag(.x, default = NA), .x) %>%   # move all p-values one row down
        replace_na(" ")  # stupid excel thing where it merges blank cells
      )
    ) %>%
  drop_na(`case (male), N = 1,599`) %>%
  relocate(Variable, Levels) %>%
  select(-Characteristic) %>%
  replace_na(list(Levels="Yes"))

# save --------------------------------------------------------------------

combined_models %>%
  write_csv(file = "C:/Users/40011625/OneDrive - Edinburgh Napier University/CHASE Publications/Results paper 1/BJPsych/BJPsych Open revision/Table_4_raw.csv")

descriptives %>%
  write_csv(file = "C:/Users/40011625/OneDrive - Edinburgh Napier University/CHASE Publications/Results paper 1/BJPsych/BJPsych Open revision/descriptives_of_regression_variables.csv")
