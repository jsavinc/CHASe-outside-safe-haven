## Population estimates over time by Urban-rural classification

library(curl)
library(tidyverse)
library(readxl)


# Define URL and download paths & download file ---------------------------

dir_pop_est <- "./population_estimates/"
url_pop_est_ur <- "https://www.nrscotland.gov.uk/files//statistics/population-estimates/special-area-2011-dz/urban-rural/urban-rural-21-tab-2.xlsx"
path_pop_est_ur <- file.path(dir_pop_est, basename(url_pop_est_ur))

## download the estimates
curl_download(url = url_pop_est_ur, destfile = path_pop_est_ur)


# Import data -------------------------------------------------------------

pop_est_raw <- map_dfr(
  .x = excel_sheets(path_pop_est_ur)[-c(1:2)],  # don't read the notes & ToC
  .f = function(x) {
    read_excel(
      path_pop_est_ur, 
      sheet = x, 
      range = "A4:C12") %>%  # this is for the totqal pop of all ages & both sexes
    mutate(year = as.integer(x)) %>%
    select(-Sex)
  }
) %>%
  janitor::clean_names()

# Transform 8-fold U-R classification to 2-fold ---------------------------

pop_est_ur_2 <-
  pop_est_raw  %>%
  mutate(
    # ur_2 = case_when(
    #   str_detect(urban_rural_class_name, pattern = regex("urban", ignore_case = TRUE)) ~ "Urban",
    #   TRUE ~ "Rural"
    #   ) %>% 
    ur_2 = if_else(
      condition = str_detect(urban_rural_class_name, pattern = regex("rural", ignore_case = TRUE)),
      true = "Rural",
      false = "Urban"
    ) %>%
      factor(., levels = c("Urban", "Rural"))
  ) %>%
  group_by(year, ur_2) %>%
  summarise(
    total = sum(total),
    .groups = "drop"
  ) %>%
  group_by(year) %>%
  mutate(
    percent = scales::percent(total / sum(total), accuracy = 0.1)
    ) %>%
  ungroup

write_csv(x = pop_est_ur_2, file = file.path(dir_pop_est, "pop_est_urban_rural_2fold_over_years.csv"))


# Population of 0-17 year-olds --------------------------------------------

pop_est_under18_raw <- map_dfr(
  .x = excel_sheets(path_pop_est_ur)[-c(1:2)],  # don't read the notes & ToC
  .f = function(x) {
    read_excel(
      path_pop_est_ur, 
      sheet = x, 
      range = "A4:U12") %>%  # this is for the 0-17 pop & both sexes
      mutate(year = as.integer(x)) %>%
      select(-c(Total, Sex))
  }
) %>%
  janitor::clean_names() %>%
  # pivot_longer(cols = matches("^age.*"), names_to = "age", names_transform = ~as.integer(str_sub(.x, 4, Inf)), values_to = "n")
  rowwise() %>% 
  mutate(total = sum(c_across(cols = matches("^age.*$")))) %>%
  ungroup %>%
  select(-matches("^age.*$"))
             
pop_est_ur_2_under18 <-
  pop_est_under18_raw  %>%
  mutate(
    # ur_2 = case_when(
    #   str_detect(urban_rural_class_name, pattern = regex("urban", ignore_case = TRUE)) ~ "Urban",
    #   TRUE ~ "Rural"
    #   ) %>% 
    ur_2 = if_else(
      condition = str_detect(urban_rural_class_name, pattern = regex("rural", ignore_case = TRUE)),
      true = "Rural",
      false = "Urban"
    ) %>%
      factor(., levels = c("Urban", "Rural"))
  ) %>%
  group_by(year, ur_2) %>%
  summarise(
    total = sum(total),
    .groups = "drop"
  ) %>%
  group_by(year) %>%
  mutate(
    percent = scales::percent(total / sum(total), accuracy = 0.1)
  ) %>%
  ungroup

write_csv(x = pop_est_ur_2_under18, file = file.path(dir_pop_est, "pop_est_under18s_urban_rural_2fold_over_years.csv"))
