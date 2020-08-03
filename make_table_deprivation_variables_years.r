## Generate tables of year - deprivation variable pairings for SIMD and Carstairs
## These are copied from ISD Scotland's Deprivation guide for analysts:
## https://www.isdscotland.org/Products-and-Services/GPD-Support/Deprivation/_docs/PHS-Deprivation-Guidance-version-3-4.pdf

library(tidyverse)

years_in_data <- 1981:2017

carstairs_years <-
  tribble(
    ~carstairs, ~year,
    1981, "1981-1985",
    1991, "1986-1995",
    2001, "1996-2005",
    2011, "2006-2017"
  )

simd_years <-
  tribble(
    ~simd, ~year, ~note,
    2004, "1981-1995", "The earliest SIMD available is for 2004 which is only advised for use with data from 1996 onwards; fewer than 0.5% of death records were prior to 1996, so the 2004 version was used for those also.",
    2004, "1996-2003", NA_character_,
    2006, "2004-2006", NA_character_,
    2009, "2007-2009", NA_character_,
    2012, "2010-2013", NA_character_,
    2016, "2014-2017", NA_character_
  )

if (!dir.exists("./deprivation_years")) dir.create("./deprivation_years")

write_csv(carstairs_years, path = "./deprivation_years/carstairs_years_table.csv")
write_csv(simd_years, path = "./deprivation_years/simd_years_table.csv")

