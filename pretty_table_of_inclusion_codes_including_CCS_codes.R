## Compiling pretty tables of ICD inclusion codes for CCS,
## and all other inclusion codes, actually


# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)


# load codes --------------------------------------------------------------

icd9 <- 
  read_csv("./processed_ICD_codes/master_icd9_code_list_UK(WHO).csv")
icd10 <- 
  read_csv("./processed_ICD_codes/master_icd10_code_list_UK(WHO).csv")

ccs_raw <- read_csv("./processed_ICD_codes/CCS_codes_updated.csv")

mvr_codes <- read_csv("./processed_ICD_codes/MVR_codes_merged_condensed_for_appendix.csv") %>%
  rename(mvr_category = category_mvr, mvr_codes_icd9 = codes_icd_9, mvr_codes_icd10 = codes_icd_10)

## Schnitzer codes use a combination of inclusions & exclusions
## Note - I'll copy these manually
# schnitzer_codes <- read_csv("./processed_ICD")


# wrangle icd codes -------------------------------------------------------

icd9 <-
  icd9 %>%
  filter(!is.na(code_decimal)) %>%
  group_by(parent_id) %>%
  mutate(number_of_codes = if_else(selectable=="Y", n(), NA_integer_)) %>%
  ungroup


icd10 <-
  icd10 %>%
  filter(!is.na(code_decimal)) %>%
  group_by(parent_id) %>%
  mutate(number_of_codes = if_else(selectable=="Y", n(), NA_integer_)) %>%
  ungroup


# overview of wrangling ---------------------------------------------------

## In the ICD hierarchy, most CCS categories capture an entire range of codes at
## 3 digit (ICD-9) and 3 character (ICD-10) level; There are a few code ranges
## where codes belonging to the range fit into different CCS categories, as well
## as a few codes scattered across different ICD categories that were also
## included

## I first compute the number of codes included in a single ICD 'category' this
## is the non-selectable code F05, for example, and its selectable subdivisions
## F051, F052, etc., are counted as 'category members'. If CCS includes an ICD
## category that includes all of its members, we can include the category,
## rather than enumerating all the individual codes. These are dealt with in the
## 'simple range' object. The remaining codes are then enumerated.

# wrangle icd-9 codes -----------------------------------------------------

ccs_icd9 <- 
  ccs_raw %>%
  filter(icd_version == 9) %>%  # split into icd 9 and 10 codes
  arrange(code) # %>%

## some of the ICD-9 codes don't appear in my reference list
ccs_icd9 %>%
  anti_join(icd9, by = "code") %>%
  nrow
## check if these codes, when the last character is removed, still appear in the CCS list
ccs_icd9 %>%
  anti_join(icd9, by = "code") %>%
  mutate(
    code_original = code,
    code = str_sub(code, 1, nchar(code)-1)  # shorten code by one character
  ) %>%
  anti_join(ccs_icd9, by = c("code","ccs_category"))  # see if any shortened codes DON'T appear in the original list, under the same CCS category
## all of the shortened codes appear in the original list, so I can simply remove them!

ccs_icd9_deduplicated <-
  ccs_icd9 %>%
  inner_join(icd9 %>% select(code, meaning, selectable, number_of_codes), by = "code") %>%
  mutate(icd9_category = str_extract(code, pattern = "^\\d{3}|^E\\d{3}|^V\\d{2,3}"))

ccs_icd9_without_suicide_or_undet_intent <-
  ccs_icd9_deduplicated %>%
  filter(!ccs_category_description %in% c("Suicide and intentional self-inflicted injury","Events of undetermined intent")) %>%
  group_by(icd9_category) %>%
  mutate(
    multiple_ccs_categories = n_distinct(ccs_category)>1,
    entire_icd_category = (number_of_codes==n() & !multiple_ccs_categories) | code==icd9_category
  ) %>%
  ungroup

ccs_icd9_ranges_simple <-
  ccs_icd9_without_suicide_or_undet_intent %>%
  filter(entire_icd_category) %>%
  select(code = icd9_category, ccs_category, ccs_category_description) %>%
  distinct %>%
  left_join(icd9 %>% select(code, meaning), by = "code") %>%
  select(meaning, ccs_category, ccs_category_description)

ccs_icd9_complex <-
  ccs_icd9_without_suicide_or_undet_intent %>%
  filter(!entire_icd_category) %>%
  select(meaning, ccs_category, ccs_category_description)

ccs_icd9_suicide_and_undet_intent <-
  tribble(
    ~code, ~ccs_category, ~ccs_category_description, ~meaning,
    "E950-E959", 662, "Suicide and intentional self-inflicted injury", "Self-harm",
    "E980-E989", 671, "Events of undetermined intent", "Sequelae of events of undetermined intent",
  )

ccs_icd9_pretty <-
  bind_rows(
    ccs_icd9_ranges_simple,
    ccs_icd9_complex,
    ccs_icd9_suicide_and_undet_intent %>%
      transmute(
        meaning = paste0(code, " ", meaning), ccs_category, ccs_category_description
      )
  ) %>%
  arrange(meaning) %>%
  mutate(ccs_category = paste0(ccs_category, " ", ccs_category_description)) %>%
  select(-ccs_category_description)


# wrangle icd-10 codes ----------------------------------------------------

ccs_icd10 <- 
  ccs_raw %>%
  filter(icd_version == 10) %>%  # split into icd 9 and 10 codes
  arrange(code) %>%
  mutate(
    code = str_remove(code, pattern="X$")
  ) %>%
  distinct

## some of the ICD-10 codes don't appear in my reference list
ccs_icd10 %>%
  anti_join(icd10, by = "code") %>%
  nrow
## check if these codes, when the last character is removed, still appear in the CCS list
ccs_icd10 %>%
  anti_join(icd10, by = "code") %>%
  mutate(
    code_original = code,
    code = str_sub(code, 1, nchar(code)-1)  # shorten code by one character
  ) %>%
  anti_join(ccs_icd10, by = c("code","ccs_category"))  # see if any shortened codes DON'T appear in the original list, under the same CCS category
## all of the shortened codes appear in the original list, so I can simply remove them!

ccs_icd10_deduplicated <-
  ccs_icd10 %>%
  inner_join(icd10 %>% select(code, meaning, selectable, number_of_codes), by = "code") %>%
  mutate(icd10_category = str_sub(code,1,3))

ccs_icd10_without_suicide_or_undet_intent <-
  ccs_icd10_deduplicated %>%
  filter(!ccs_category_description %in% c("Suicide and intentional self-inflicted injury","Events of undetermined intent")) %>%
  # filter(!str_detect(code, pattern = paste(c(paste0("X",c(40:49,60:84)),paste0("Y",10:34)), collapse = "|")))
  group_by(icd10_category) %>%
  mutate(
    multiple_ccs_categories = n_distinct(ccs_category)>1,
    entire_icd_category = (number_of_codes==n() & !multiple_ccs_categories) | code==icd10_category
    ) %>%
  ungroup

ccs_icd10_ranges_simple <-
  ccs_icd10_without_suicide_or_undet_intent %>%
  filter(entire_icd_category) %>%
  select(code = icd10_category, ccs_category, ccs_category_description) %>%
  distinct %>%
  left_join(icd10 %>% select(code, meaning), by = "code") %>%
  select(meaning, ccs_category, ccs_category_description)

ccs_icd10_complex <-
  ccs_icd10_without_suicide_or_undet_intent %>%
  filter(!entire_icd_category) %>%
  select(meaning, ccs_category, ccs_category_description)

ccs_icd10_suicide_and_undet_intent <-
  tribble(
    ~code, ~ccs_category, ~ccs_category_description, ~meaning,
    # Note: I'm not sure whether accidental poisoning was included as undetermined events or not
    # in either case, it's being reported separately
    # "X40-X49", 671, "Events of undetermined intent", "Accidental poisoning",
    "X60-X84", 662, "Suicide and intentional self-inflicted injury", "Self-harm",
    "Y870", 662, "Suicide and intentional self-inflicted injury", "Sequelae of intentional self-harm",
    "Y872", 671, "Events of undetermined intent", "Sequelae of events of undetermined intent",
  )

ccs_icd10_pretty <-
  bind_rows(
    ccs_icd10_ranges_simple,
    ccs_icd10_complex,
    ccs_icd10_suicide_and_undet_intent %>%
      transmute(
        meaning = paste0(code, " ", meaning), ccs_category, ccs_category_description
      )
  ) %>%
  arrange(meaning) %>%
  mutate(ccs_category = paste0(ccs_category, " ", ccs_category_description)) %>%
  select(-ccs_category_description)


# MVR codes ---------------------------------------------------------------




# save pretty table -------------------------------------------------------

# TODO: include Schnitzer codes also, load them and include them in the table

notes_tbl <-
  tibble(
    Codes = "CCS",
    Notes = c(
      "CCS codes representing widely-defined Mental (ill-) Health were produced by a cross-mapping exercise using the AHRQ Clinical Classifications Software (CCS) codes using ICD-10-CM, available from https://hcup-us.ahrq.gov/toolssoftware/ccsr/DXCCSR2019_1.zip. ICD-10-CM were cross-mapped to ICD-10 and ICD-9 codes used in the UK.",
      "Mental health-related codes were extracted using the 'MULTI CCS LVL 1 LABEL' column, for entries labelled 'Mental Illness', and the further categorisation using the column 'MULTI CCS LVL 2 LABEL' was retained.",
      "In addition to codes extracted from CCS, codes for Events of undetermined event (ICD-9: E980-E989; ICD-10: Y10-Y34, Y87.2) were included under an additional category '671 Events of undetermined intent'."
    )
  ) %>%
  add_row(
    Codes = "MVR", 
    Notes = "Maltreatment or violence-related (MVR) codes were compiled from Gilbert et al. (2012, https://doi.org/10.1016/S0140-6736(11)61087-8) and González-Izquierdo (personal communication, 2019, mostly available at: https://doi.org/10.1136/adc.2009.180216), and were previously published in the protocol paper, Dougall et al., (2020, https://doi.org/10.23889/ijpds.v5i1.1338)."
    ) %>%
  add_row(
    Codes = "Schnitzer et al 2011", 
    Notes = "Codes suggestive of maltreatment or neglect were compiled from Schnitzer, et al. (2011, https://doi.org/10.1016/j.chiabu.2010.06.008), and cross-mapped from the original ICD-9-CM to ICD-9 and ICD-10. Some of the inclusions have an associated set of exclusion codes also, which are listed in a separate sheet."
    )

wb <- createWorkbook()
addWorksheet(wb, "Notes")
addWorksheet(wb, "MVR codes")
addWorksheet(wb, "CCS, ICD-9")
addWorksheet(wb, "CCS, ICD-10")
addWorksheet(wb, "Schnitzer inclusions")
addWorksheet(wb, "Schnitzer exclusions")

writeData(
  wb,
  sheet = "Notes",
  x = notes_tbl
  )

writeData(
  wb,
  sheet = "MVR codes",
  x = mvr_codes
)

writeData(
  wb,
  sheet = "CCS, ICD-9",
  x = ccs_icd9_pretty %>% rename("ICD-9 code" = meaning, "CCS Category" = ccs_category)
)
writeData(
  wb,
  sheet = "CCS, ICD-10",
  x = ccs_icd10_pretty %>% rename("ICD-10 code" = meaning, "CCS Category" = ccs_category)
)

# TODO: copy SChnitzer criteria from the pre-processed file "

merging_index_icd9 <-
  ccs_icd9_pretty %>%
  mutate(
    row_number = 1:n(),
    index = cumsum(ccs_category!=lag(ccs_category,1,default=""))
    ) %>%
  group_by(index) %>%
  summarise(start_row = min(row_number)+1, end_row=max(row_number)+1)

pwalk(
  .l = merging_index_icd9,
  .f = function(index, start_row, end_row) {
    mergeCells(wb, sheet = "CCS, ICD-9", cols = 2, rows = start_row:end_row)
  }
)

merging_index_icd10 <-
  ccs_icd10_pretty %>%
  arrange(meaning) %>%
  mutate(
    row_number = 1:n(),
    index = cumsum(ccs_category!=lag(ccs_category,1,default=""))
  ) %>%
  group_by(index) %>%
  summarise(start_row = min(row_number)+1, end_row=max(row_number)+1)

pwalk(
  .l = merging_index_icd10,
  .f = function(index, start_row, end_row) {
    mergeCells(wb, sheet = "CCS, ICD-10", cols = 2, rows = start_row:end_row)
  }
)

saveWorkbook(wb, "./processed_ICD_codes/inclusion_codes_for_appendix.xlsx", overwrite = TRUE)