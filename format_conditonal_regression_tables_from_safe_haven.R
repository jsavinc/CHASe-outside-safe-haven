## Reformatting odds ratio table for conditional regression models


# packages ----------------------------------------------------------------

library(tidyverse)

# load data ---------------------------------------------------------------

raw_model_m <- read_csv("X:/R1369/CSO FULL grant/Safe Haven Exports/2023-09-12 conditional logistic regressions/clogit_simple_male_1.csv")
raw_model_f <- read_csv("X:/R1369/CSO FULL grant/Safe Haven Exports/2023-09-12 conditional logistic regressions/clogit_simple_female_1.csv")
# 
# raw_model_m <- read_csv("X:/R1369/CSO FULL grant/Safe Haven Exports/2023-09-12 conditional logistic regressions/clogit_subcat_male_1.csv")
# raw_model_f <- read_csv("X:/R1369/CSO FULL grant/Safe Haven Exports/2023-09-12 conditional logistic regressions/clogit_subcat_female_1.csv")

# 
# raw_model_m <- read_csv("X:/R1369/CSO FULL grant/Safe Haven Exports/2023-09-12 conditional logistic regressions/clogit_subcat_male_2.csv")
# raw_model_f <- read_csv("X:/R1369/CSO FULL grant/Safe Haven Exports/2023-09-12 conditional logistic regressions/clogit_subcat_female_2.csv")
# 

descriptives_simple <- read_csv("X:/R1369/CSO FULL grant/Safe Haven Exports/2023-09-12 conditional logistic regressions/regression_descriptives_adversities_simple.csv")
descriptives_subcat <- read_csv("X:/R1369/CSO FULL grant/Safe Haven Exports/2023-09-12 conditional logistic regressions/regression_descriptives_adversities_subcategorised.csv")

univariate_regressions_simple <- read_csv("X:/R1369/CSO FULL grant/Safe Haven Exports/2023-09-12 conditional logistic regressions/univariate_conditional_regressions_simple.csv")
univariate_regressions_subcat <- read_csv("X:/R1369/CSO FULL grant/Safe Haven Exports/2023-09-12 conditional logistic regressions/univariate_conditional_regressions_subcategorised.csv")

# constants ---------------------------------------------------------------

table_variable_names <-
  tribble(
    ~short_name, ~long_name,
    "n_episodes_mvr_under_18_factor", "MVR admissions, aged <18",
    "n_episodes_ccs_under_18_factor", "MH admissions, aged <18",
    # "n_episodes_schnitzer_without_caries_under_10_factor", "Admissions with codes suggestive of maltreatment or neglect (Schnitzer et al., 2011), excluding dental caries, aged <10",
    "n_episodes_schnitzer_without_caries_under_10_factor", "Admissions with codes suggestive of maltreatment or neglect, excluding dental caries, aged <10",
    "n_episodes_poisoning_under_18_factor", "Accidental poisoning admissions, aged <18",
    "mother_died_before_age_18", "Maternal death, aged <18",
    "care_experienced_or_no_fixed_abode_before_age_18", "Admissions indicating care experience or no fixed abode, aged <18",
    "n_episodes_mvr_assault_maltreatment_under_18_factor", "MVR admissions: assault or maltreatment, aged <18",
    "n_episodes_mvr_social_or_undetermined_under_18_factor", "MVR admissions: adverse soc. circumstances or undetermined intent, aged <18",
    "n_episodes_ccs_self_harm_under_18_factor", "MH admissions: self harm, aged <18",
    "n_episodes_ccs_alcohol_under_18_factor", "MH admissions: alcohol-related, aged <18",
    "n_episodes_ccs_not_self_harm_or_alcohol_under_18_factor", "MH admissions: neither self harm nor alcohol-related, aged <18"
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

## first, change univariate regressions to wide format
univariate_regressions <-
  bind_rows(
    univariate_regressions_simple, 
    univariate_regressions_subcat
    ) %>%
  mutate(or_ci = 
   paste0( or = round(or, 2) %>% format(., nsmall=1), " [", format_ors(or_ci_low,or_ci_high), "]")
  ) %>%
  select(variable, level, sex, or_ci, p_value ) %>%
  pivot_wider(names_from = sex, values_from = c(or_ci,p_value)) %>%
  mutate(level = if_else(level==TRUE, "Yes", level)) %>%
  rename(
    Variable = variable, Level = level
    )

descriptives <-
  bind_rows(
    descriptives_subcat,
    # descriptives_simple %>% slice(10:17)
    descriptives_simple
  ) %>%
  mutate(
    Variable = if_else(str_starts(Characteristic, pattern = "[:alpha:]"), Characteristic, NA_character_),
    Level = if_else(!str_starts(Characteristic, pattern = "[:alpha:]"), Characteristic, NA_character_)
    ) %>%
  fill(Variable, .direction = "down") %>%
  select(-matches("p-value")) %>%  # don't use Fisher's test, use the OR p-values instead, which are merged further down
  # mutate(  # skip using
  #   across(
  #     matches("p-value"), 
  #     ~if_else(!is.na(lag(.x, default = NA)), lag(.x, default = NA), .x) %>%   # move all p-values one row down
  #       replace_na(" ")  # stupid excel thing where it merges blank cells
  #     )
  #   ) %>%
  drop_na(`case (male), N = 1,599`) %>%
  relocate(Variable, Level) %>%
  select(-Characteristic) %>%
  replace_na(list(Level="Yes")) %>%
  left_join(univariate_regressions, join_by(Variable,Level)) %>%
  mutate(
    Variable = table_variable_names$long_name[match(x = Variable, table = table_variable_names$short_name)],
    across(matches("p_value"), format_p_value)
    ) %>%
  filter(Level!="0") %>%
  relocate(or_ci_male, p_value_male, .after = `control (male), N = 10,537`)

# save --------------------------------------------------------------------

combined_models %>%
  write_csv(file = "C:/Users/40011625/OneDrive - Edinburgh Napier University/CHASE Publications/Results paper 1/BJPsych/BJPsych Open revision/table_clogit_simple_1.csv")

descriptives %>%
  write_csv(file = "C:/Users/40011625/OneDrive - Edinburgh Napier University/CHASE Publications/Results paper 1/BJPsych/BJPsych Open revision/descriptives_of_regression_variables.csv")


# odds ratio plot ---------------------------------------------------------

ors_for_plot <-
  bind_rows(
    raw_model_m %>% mutate(sex="Male"), 
    raw_model_f %>% mutate(sex = "Female")
  ) %>%
  mutate(
    variable = table_variable_names$long_name[match(x = variable, table = table_variable_names$short_name)],
    level = if_else(level == "TRUE", "Yes", level),
    label = paste0(
      str_wrap(variable, width = 40),
      ":\n",
      level
    ),
    label = factor(label, levels = unique(label)) %>% fct_rev(),
    sex = factor(sex, levels = c("Male","Female"))
  )
    
  
(
fig_plot_subcategorised_adversities <-
  ors_for_plot %>%
  ggplot(aes(y = label, x = or, shape = sex)) +
  geom_vline(xintercept = 1, colour = "grey50", linetype = "dashed") +
  geom_point(position = position_dodge(width=0.5), size = 2.5) +
  geom_errorbar(aes(xmin = or_ci_low, xmax = or_ci_high), position = position_dodge(width=0.5), width = 0) +
  theme_minimal() +
  scale_x_log10(breaks = c(seq(from=0.1, to=0.9, by=0.1), 1:10, 15, 30), labels = c(as.character(seq(from=0.1, to=0.9, by=0.1)), 1:10, 15, 30)) +
  theme(
    legend.position = "top", 
    legend.title = element_blank(), 
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_text(size = 11),
  ) +
  labs(
    y = NULL,
    x = "Adjusted odds ratio"
  ) +
  NULL
)

fig_plot_subcategorised_adversities %>%
  ggsave(
    filename = "C:/Users/40011625/OneDrive - Edinburgh Napier University/CHASE Publications/Results paper 1/BJPsych/BJPsych Open revision/or_plot_clogit_simple.png",
    bg = "white",
    dpi = 300,
    width = 10,
    height = 8
  )


fig_plot_subcategorised_adversities %>%
  ggsave(
    filename = "C:/Users/40011625/OneDrive - Edinburgh Napier University/CHASE Publications/Results paper 1/BJPsych/BJPsych Open revision/or_plot_clogit_simple.pdf",
    bg = "white",
    dpi = 300,
    width = 10,
    height = 8
  )
