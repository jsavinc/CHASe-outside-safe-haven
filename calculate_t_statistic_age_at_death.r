#### Calculate t-statistic & p-value for comparison of age at death
#### From data exported from safe haven


# Packages ----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(BSDA)  # for summarised t-test

# Load data ---------------------------------------------------------------

age_at_death <- 
  read_excel(
    path = "X:/R1369/CSO FULL grant/Safe Haven Exports/Descriptive summary of cohort & individuals with any records/Descriptive_summary_cohort_by_whether_they_had_any_records_prior_to_death.xlsx",
    sheet = "Descriptives", 
    range = "A1:F7"
    )


# Calculate t-statistic ---------------------------------------------------

## t-test for difference between cases & controls on age at death (both sexes)
age_at_death_both <- age_at_death %>% filter(sex=="both")

tsum.test(
  mean.x = age_at_death_both$mean_age[1], 
  s.x = age_at_death_both$sd_age[1], 
  n.x = age_at_death_both$N[1],
  mean.y = age_at_death_both$mean_age[2], 
  s.y = age_at_death_both$sd_age[2], 
  n.y = age_at_death_both$N[2],
  var.equal = TRUE
  )

# Standard Two-Sample t-Test
# 
# data:  Summarized x and y
# t = -6.4582, df = 2475, p-value = 1.271e-10
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -2.339264 -1.249569
# sample estimates:
#   mean of x mean of y 
# 21.85243  23.64684 
