## Compile Consort-style diagram
## below reworked from:
## https://dannyjnwong.github.io/STROBE-CONSORT-Diagrams-in-R/

# install.packages("DiagrammeR")
# install.packages("DiagrammeRsvg")
# install.packages("rsvg")

library(DiagrammeR)
library(DiagrammeRsvg) #Needed if you want to export the image
library(rsvg) #Needed if you want to export the image
library(tidyverse)
library(devEMF)  # for saving EMF files

## Load data
consort_data <- 
  read_csv(file = "../Safe Haven Exports/CONSORT_diagram_data.csv") %>%
  mutate(
    note = case_when(
      note == "Excluded: cases with no hospital records prior to death before age 18" &
        N > 1000 ~ "Excluded: controls with no hospital records prior to death before age 18",
      TRUE ~ note
    )
  )  # correction: instead of 'cases' it should be 'controls' at the entry for exclusions with no records prior to death or age 18 with N>1000

## convert entries in data to 'meaningful' variables

remove_punctuation <- function(x) gsub(pattern = "[[:punct:]]", replacement = "", x = x)
remove_consecutive_spaces <- function(x) gsub(pattern = "\\s+", replacement = " ", x = x)
convert_space_to_underscore <- function(x) gsub(pattern = "\\s", replacement = "_", x = x)

numbers <- set_names(
  x = 
    consort_data$N %>%
    format(x=., big.mark = ",", trim = TRUE) %>%  # add thousands-separators
    as.list()
  , 
  nm = 
    consort_data$note %>%
    remove_punctuation() %>%
    remove_consecutive_spaces() %>%
    convert_space_to_underscore() %>%
    tolower
  )

## there's string wizardry happening underneath here: 
## basically grViz allows you to input a specification, as a string, of the nodes & edges and their relationships
## because it's a string, we can arbitrarily chop it up and paste the constitutent parts
## specifically, we can use glue() to insert arguments into the text where desired, however, because glue() uses curly braces to know where to insert arguments,
## we first chop up the specification into bits that don't use curly braces and bits that do, and onyl use glue() with the bits that don't need to use curly braces

consort_diagram <-
  grViz(paste0("
  digraph boxes_and_circles {
    ", glue::glue("
  
    # node definitions
    node [shape = box,
          fontname = Helvetica]
    
    cases_1[label = 'Deaths by suicide \\'cases\\'\\n1991 to 2017, aged 10 to 34\\nn={numbers$extracted_cases_individuals_who_died_by_suicide_aged_1034_1981_to_2017}']
    cases_1_excluded[label = 'Exclusions:\\nborn before 1981\\nn={numbers$excluded_cases_born_before_1981}']
    cases_1_blank[shape = point, label = '', width = 0.01, height = 0.01]
    
    cases_2[label = 'Born 1981 or later\\nn={numbers$cases_born_1981_or_later}']
    cases_2_excluded[label = 'No hospital records prior to death\\nn={numbers$excluded_cases_with_no_hospital_records_or_only_hospital_record_was_related_to_death}']
    cases_2_blank[shape = point, label = '', width = 0.01, height = 0.01]
    
    cases_3[label = 'Any hospital records prior to death\\nn={numbers$cases_with_hospital_records_prior_to_death}']
    cases_3_excluded[label = 'No hospital records prior to age 18 or prior to death\\nn={numbers$excluded_cases_with_no_hospital_records_prior_to_death_before_age_18}']
    cases_3_blank[shape = point, label = '', width = 0.01, height = 0.01]
    
    cases_4[label = 'Any hospital records prior to age 18 & prior to death\\nn={numbers$cases_with_hospital_records_prior_to_death_and_prior_to_age_18}']
    
    
    controls_1[label = 'Random sample non-suicide \\'controls\\'\\nn={numbers$random_sample_nonsuicide_controls_matched_to_cases}, ratio 1:10']
    controls_1_excluded[label = 'Exclusions:\\nlinked to cases born before 1981\\nn={numbers$excluded_controls_matched_to_cases_born_before_1981}']
    controls_1_blank[shape = point, label = '', width = 0.01, height = 0.01]
    
    controls_2[label = 'Linked to cases born 1981 or later\\nn={numbers$controls_matched_to_cases_born_1981_or_later}']
    controls_2_excluded[label = 'No hospital records prior to death\\nn={numbers$excluded_controls_with_no_hospital_records_or_only_hospital_record_was_related_to_death}']
    controls_2_blank[shape = point, label = '', width = 0.01, height = 0.01]
    
    controls_3[label = 'Any hospital records prior to death\\nn={numbers$controls_with_hospital_records_prior_to_death}']
    controls_3_excluded[label = 'No hospital records prior to age 18 or prior to death\\nn={numbers$excluded_controls_with_no_hospital_records_prior_to_death_before_age_18}']
    controls_3_blank[shape = point, label = '', width = 0.01, height = 0.01]
    
    controls_4[label = 'Any hospital records prior to age 18 & prior to death\\nn={numbers$controls_with_hospital_records_prior_to_death_and_prior_to_age_18}']
    
    "),"
    
    # rank definitions
    { rank = same; cases_1 controls_1}
    { rank = same; cases_1_blank cases_1_excluded controls_1_blank controls_1_excluded}
    { rank = same; cases_2 controls_2}
    { rank = same; cases_2_blank cases_2_excluded controls_2_blank controls_2_excluded}
    { rank = same; cases_3 controls_3}
    { rank = same; cases_3_blank cases_3_excluded controls_3_blank controls_3_excluded}
    { rank = same; cases_4 controls_4}
    
    # edge definitions
    cases_1->cases_1_blank[ dir = none ];  cases_1_blank->cases_1_excluded cases_1_blank->cases_2
    cases_2->cases_2_blank[ dir = none ];  cases_2_blank->cases_2_excluded cases_2_blank->cases_3
    cases_3->cases_3_blank[ dir = none ];  cases_3_blank->cases_3_excluded cases_3_blank->cases_4
    
    controls_1->controls_1_blank[ dir = none ];  controls_1_blank->controls_1_excluded controls_1_blank->controls_2
    controls_2->controls_2_blank[ dir = none ];  controls_2_blank->controls_2_excluded controls_2_blank->controls_3
    controls_3->controls_3_blank[ dir = none ];  controls_3_blank->controls_3_excluded controls_3_blank->controls_4
  }
  "))

consort_diagram %>% export_svg %>% charToRaw %>% rsvg_pdf("./CONSORT_diagram/CONSORT_diagram.pdf")
# consort_diagram %>% export_svg %>% charToRaw %>% rsvg_png("./CONSORT_diagram/CONSORT_diagram.png")  # Ugly
consort_diagram %>% export_svg %>% charToRaw %>% rsvg_svg("./CONSORT_diagram/CONSORT_diagram.svg")

## this doesn't work because we're not printing a vector image but rendering a bitmap image instead
# emf(file = "./CONSORT_diagram/CONSORT_diagram.emf")
# print(consort_diagram)
# dev.off()

## Vesrion without age 18 step


consort_diagram_without_step_age_18 <-
  grViz(paste0("
  digraph boxes_and_circles {
    ", glue::glue("
  
    # node definitions
    node [shape = box,
          fontname = Helvetica]
    
    cases_1[label = 'Deaths by suicide \\'cases\\'\\n1991 to 2017, aged 10 to 34\\nn={numbers$extracted_cases_individuals_who_died_by_suicide_aged_1034_1981_to_2017}']
    cases_1_excluded[label = 'Exclusions:\\nborn before 1981\\nn={numbers$excluded_cases_born_before_1981}']
    cases_1_blank[label = '', width = 0.01, height = 0.01]
    
    cases_2[label = 'Born 1981 or later\\nn={numbers$cases_born_1981_or_later}']
    cases_2_excluded[label = 'No hospital records prior to death\\nn={numbers$excluded_cases_with_no_hospital_records_or_only_hospital_record_was_related_to_death}']
    cases_2_blank[label = '', width = 0.01, height = 0.01]
    
    cases_3[label = 'Any hospital records prior to death\\nn={numbers$cases_with_hospital_records_prior_to_death}']
    
    controls_1[label = 'Random sample non-suicide \\'controls\\'\\nn={numbers$random_sample_nonsuicide_controls_matched_to_cases}, ratio 1:10']
    controls_1_excluded[label = 'Exclusions:\\nlinked to cases born before 1981\\nn={numbers$excluded_controls_matched_to_cases_born_before_1981}']
    controls_1_blank[label = '', width = 0.01, height = 0.01]
    
    controls_2[label = 'Linked to cases born 1981 or later\\nn={numbers$controls_matched_to_cases_born_1981_or_later}']
    controls_2_excluded[label = 'No hospital records prior to death\\nn={numbers$excluded_controls_with_no_hospital_records_or_only_hospital_record_was_related_to_death}']
    controls_2_blank[label = '', width = 0.01, height = 0.01]
    
    controls_3[label = 'Any hospital records prior to death\\nn={numbers$controls_with_hospital_records_prior_to_death}']
    
    "),"
    
    # rank definitions
    { rank = same; cases_1 controls_1}
    { rank = same; cases_1_blank cases_1_excluded controls_1_blank controls_1_excluded}
    { rank = same; cases_2 controls_2}
    { rank = same; cases_2_blank cases_2_excluded controls_2_blank controls_2_excluded}
    { rank = same; cases_3 controls_3}
    
    # edge definitions
    cases_1->cases_1_blank[ dir = none ];  cases_1_blank->cases_1_excluded cases_1_blank->cases_2
    cases_2->cases_2_blank[ dir = none ];  cases_2_blank->cases_2_excluded cases_2_blank->cases_3
    
    controls_1->controls_1_blank[ dir = none ];  controls_1_blank->controls_1_excluded controls_1_blank->controls_2
    controls_2->controls_2_blank[ dir = none ];  controls_2_blank->controls_2_excluded controls_2_blank->controls_3
  }
  "))

consort_diagram_without_step_age_18 %>% export_svg %>% charToRaw %>% rsvg_pdf("./CONSORT_diagram/CONSORT_diagram_excluding_under_18s_step.pdf")
# consort_diagram %>% export_svg %>% charToRaw %>% rsvg_png("./CONSORT_diagram/CONSORT_diagram.png")  # Ugly
consort_diagram_without_step_age_18 %>% export_svg %>% charToRaw %>% rsvg_svg("./CONSORT_diagram/CONSORT_diagram_excluding_under_18s_step.svg")