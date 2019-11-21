## script to test ideas for generating table of counts of individuals by age by year in cohort

library(tidyverse)

set.seed(20191016)

years_of_birth <- 1981:1999
years_of_exit <- 1991:2017

sample_size <- 28000

test_df <- data.frame(
  year_birth = sample(years_of_birth, size = sample_size, replace = TRUE),
  year_exit = sample(years_of_exit, size = sample_size, replace = TRUE)
)

test_df_counted <- test_df %>% count(year_birth, year_exit)

map_dfr(
  .x = min(years_of_birth):max(years_of_exit), 
  .f = function(x) test_df_counted %>% filter(year_birth<=x & year_exit>x) %>% mutate(age=x-year_birth, year_study=x)
  ) %>%
  group_by(year_study, age) %>% summarise(n=sum(n))

## The map2 version below doesn't work because it iterates over the columns in the test_df rather than treating it as a single item!

# map2_dfr(
#   .x = test_df_counted, 
#   .y = min(test_df_counted$year_birth):max(test_df_counted$year_exit),
#   .f = ~(.x %>% filter(year_birth <= .y & year_exit > .y) %>% mutate(age=.x-year_birth, year_study = .y))
# )