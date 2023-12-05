## Additional table for CHASe supplementary file
## merging the subcat regression without self-harm as an explanatory variable


# packages ----------------------------------------------------------------

library(tidyverse)

# load data ---------------------------------------------------------------

## models (subcat) without SH
without_model_m_raw <- read_csv("X:/R1369/CSO FULL grant/Safe Haven Exports/2023-10-20 relative order & regressions continued/clogit_male_selfharm_with_vs_without.csv")
without_model_f_raw <- read_csv("X:/R1369/CSO FULL grant/Safe Haven Exports/2023-10-20 relative order & regressions continued/clogit_female_selfharm_with_vs_without.csv")

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

extract_or_and_ci <- function(data_tbl, col) {
  col = enquo(col)
  
  data_tbl %>%
  extract(
    col = !!col, into = c("or", "or_ci_low", "or_ci_high"), 
    regex = "(\\d+\\.{0,1}\\d*) \\[(\\d+\\.{0,1}\\d*)\\-(\\d+\\.{0,1}\\d*)\\]", 
    convert = TRUE
  )
}

# wrangle data ------------------------------------------------------------

## extract the OR and confidence interval
without_model_m <-
  without_model_m_raw %>%
  # I unfortunately named these as if I was comparing a glm and a clogit model
  # In fact, or_ci_glm is the model with self-harm, and or_ci_clog is the model without self-harm
  select(-or_ci_glm) %>%
  extract_or_and_ci(col = or_ci_clog)

without_model_f <-
  without_model_f_raw %>%
  # I unfortunately named these as if I was comparing a glm and a clogit model
  # In fact, or_ci_glm is the model with self-harm, and or_ci_clog is the model without self-harm
  select(-or_ci_glm) %>%
  extract_or_and_ci(col = or_ci_clog)

## deal with the model with self-harm also
with_model_m <-
  raw_model_m %>%
  select(-or_ci_clog) %>%
  extract_or_and_ci(col = or_ci_glm)
with_model_f <-
  raw_model_f %>%
  select(-or_ci_clog) %>%
  extract_or_and_ci(col = or_ci_glm)

## models without self-harm for table
models_to_combine <-
  map(
    .x = list(without_model_m, without_model_f),
    .f = function(model) {
      model %>%
        mutate(
          or = round(or, 2) %>% format(., nsmall=1),
          or_ci = format_ors(ci_low = or_ci_low, ci_high = or_ci_high)
        ) %>%
        select(variable, level, or, or_ci)
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
    variable = table_variable_names$long_name[match(x = variable, table = table_variable_names$short_name)],
  ) %>%
  ## replace with " " where self-harm variable OR and CI was
  mutate(across(.cols = starts_with("or"), ~if_else(variable=="MH admissions: self harm, aged <18", "NA", .x)))


# write table -------------------------------------------------------------

combined_models %>%
  write_csv(file = "C:/Users/40011625/OneDrive - Edinburgh Napier University/CHASE Publications/Results paper 1/BJPsych/BJPsych Open revision/table_supplementary_clogit_subcat_without_selfharm.csv")


# OR plot with models with and without SH ---------------------------------

ors_for_plot <-
  bind_rows(
    with_model_m %>% mutate(sex="Male", model = "incl. self-harm"),
    with_model_f %>% mutate(sex="Female", model = "incl. self-harm"),
    without_model_m %>% mutate(sex="Male", model = "excl. self-harm"), 
    without_model_f %>% mutate(sex = "Female", model = "excl. self-harm")
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
    sex = factor(sex, levels = c("Male","Female")),
    model = factor(model, levels = c("incl. self-harm", "excl. self-harm"))
  )

(
  fig_plot_subcategorised_adversities_with_or_without_sh <-
    ors_for_plot %>%
    ggplot(aes(y = label, x = or, shape = sex, colour = model)) +
    geom_vline(xintercept = 1, colour = "grey50", linetype = "dashed") +
    geom_point(position = position_dodge(width=0.5), size = 2.5) +
    geom_errorbar(aes(xmin = or_ci_low, xmax = or_ci_high), position = position_dodge(width=0.5), width = 0) +
    theme_minimal() +
    scale_colour_grey() +
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


fig_plot_subcategorised_adversities_with_or_without_sh %>%
  ggsave(
    filename = "C:/Users/40011625/OneDrive - Edinburgh Napier University/CHASE Publications/Results paper 1/BJPsych/BJPsych Open revision/or_plot_clogit_subcat_with_and_without_sh.png",
    bg = "white",
    dpi = 300,
    width = 10,
    height = 8
  )


fig_plot_subcategorised_adversities_with_or_without_sh %>%
  ggsave(
    filename = "C:/Users/40011625/OneDrive - Edinburgh Napier University/CHASE Publications/Results paper 1/BJPsych/BJPsych Open revision/or_plot_clogit_subcat_with_and_without_sh.pdf",
    bg = "white",
    dpi = 300,
    width = 10,
    height = 8
  )
