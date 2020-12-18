library(dplyr)  # enables tidy processing functions
library(fuzzyjoin)  # for regex-based joining of tables

# Load data
admissions <- read_csv("../Collaborations/Sample Set of Simulated 3 Yrs of Encounters.csv") %>% 
  group_by(id) %>% 
  mutate(episode_id = 1:n()) %>%  # add episode id
  ungroup

# We convert the admissions into a format where each admission has a unique identifier, and each row represents one diagnosis code
# admissions_in_long_format <- NULL  # whatever processing needs doing to get to that stage
admissions_in_long_format <- 
  admissions %>% 
  pivot_longer(
    cols = starts_with("diagnosis"), names_to = "source", values_to = "code"
  )

# The algorithm outline is as follows:
# 1a Create a subset of all episodes that match any of the inclusions
# 1b Create a subset of all episodes that match any of the exclusions
# 2a Find the overlap between episodes with inclusions and exclusions that match one another (using episode id for matching episodes and inclusion id for finding matching inclusions & exclusions)
# 2b Remove the above overlap from the subset of episodes matching inclusions (i.e. keep only subset of inclusions with no matching exclusions)
# 3 Remove inclusions that don't meet the additional requirements

admissions_with_inclusions <-
  admissions_in_long_format %>%
  regex_inner_join(
    inclusion_codes, by = c("diagnosis_code"="prefix_for_matching")
  ) %>%
  filter(icd_version == icd_version_inclusions)

# TODO: additional inclusions needed!

admissions_with_exclusions <-
  admissions_in_long_format %>%
  regex_inner_join(
    exclusion_codes, by = c("diagnosis_code"="prefix_for_matching")
  ) %>%
  filter(icd_version == icd_version_exclusions)

admissions_with_inclusions_and_no_exclusions <-
  anti_join(
    x = admissions_with_inclusions,
    y = admissions_with_exclusions,
    by = c("episode_id","inclusion_id")
  )

## for checking, also see which inclusions matched their exclusions!
inclusions_with_exclusions <-
  semi_join(
    x = admissions_with_inclusions,
    y = admissions_with_exclusions,
    by = c("episode_id","inclusion_id")
  )