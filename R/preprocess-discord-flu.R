## ----setup--------------------------------------------------------------------------------------------------------------------
# Load packages
library(dplyr)
library(readr)
library(here)
# Source the data processing file,
# lightly modified from the NLSY Investigator
source(here("R/nls-process-data.R"))

## ----manipulate-nlsy-data----------------------------------------------------------------------------------------------------------------------
# Restructure NLSY data with flu information to
# include a total across years, irrespective of encoded genders.
nlsy_flu_data <- categories %>%
  rowwise() %>%
  mutate(FLU_total = sum(across(starts_with("FLU_")), na.rm = TRUE),
         FLU_2008 = sum(across(ends_with("2008")), na.rm = TRUE),
         FLU_2010 = sum(across(ends_with("2010")), na.rm = TRUE),
         FLU_2012 = sum(across(ends_with("2012")), na.rm = TRUE),
         FLU_2014 = sum(across(ends_with("2014")), na.rm = TRUE),
         FLU_2016 = sum(across(ends_with("2016")), na.rm = TRUE)) %>%
  ungroup() %>%
  # If both encoded genders did not get a flu shot, set the entry in the year total equal to NA
  # This is necessary since we removed NAs from our sum calculation above
  mutate(FLU_2008 = ifelse(is.na(FLU_M_2008) & is.na(FLU_F_2008), NA, FLU_2008),
         FLU_2010 = ifelse(is.na(FLU_M_2010) & is.na(FLU_F_2010), NA, FLU_2010),
         FLU_2012 = ifelse(is.na(FLU_M_2012) & is.na(FLU_F_2012), NA, FLU_2012),
         FLU_2014 = ifelse(is.na(FLU_M_2014) & is.na(FLU_F_2014), NA, FLU_2014),
         FLU_2016 = ifelse(is.na(FLU_M_2016) & is.na(FLU_F_2016), NA, FLU_2016))



## ----read-demographic-data----------------------------------------------------------------------------------------------------
# Read demographic data from internal SES measures
demographic_data <- read_csv(here("data/nlsy-ses.csv"))


## ----merge-nlsy-demographic-data---------------------------------------------------------------------------------------------------------------

# Combine the demographic data with the flu shot data
flu_ses_data <- inner_join(nlsy_flu_data, demographic_data,
                      by = "CASEID") %>%
  mutate(RACE = case_when(RACE == "NON-BLACK, NON-HISPANIC" ~ 0,
                          RACE == "HISPANIC" | RACE == "BLACK" ~ 1),
         SEX = case_when(SEX == "FEMALE" ~ 0,
                         SEX == "MALE" ~ 1))


