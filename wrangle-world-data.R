# ==============================================================================
# Our World in Data Graphics (Downloadable CSV files):
# https://ourworldindata.org/grapher/prevalence-of-depression-by-age
# https://ourworldindata.org/grapher/prevalence-of-depression-males-vs-females
# https://ourworldindata.org/grapher/number-with-depression-by-country?tab=chart
#
# Authors: Ellie DeCarlo, Miranda Law
#
# Data dictionary (prev_number):
# `entity`: country name
# `code`: country code
# `year`: year of interest
# `number`: number of people diagnosed with depression
# ------------------------------------------------------------------------------
# Data dictionary (prev_age):
# `entity`: country name
# `code`: country code
# `year`: year of interest
# `20_24`: percent of people ages 20-24 diagnosed with depression
# `10_14`: percent of people ages 10-14 diagnosed with depression
# `all_ages`: total percent of people diagnosed with depression
# `above70`: percent of people ages 70+ diagnosed with depression
# `30_34`: percent of people ages 30-34 diagnosed with depression
# `15_19`: percent of people ages 15-19 diagnosed with depression
# `25_29`: percent of people ages 25-29 diagnosed with depression
# `50_69`: percent of people ages 50-69 diagnosed with depression
# `age_standardized`: % people diagnosed with depression (standardized)
# `15_49`: percent of people ages 15-49 diagnosed with depression
# ------------------------------------------------------------------------------
# Data dictionary (prev_sex):
# `entity`: country name
# `code`: country code
# `year`: year of interest
# `male`: percent of males diagnosed with depression
# `female`: percent of females diagnosed with depression
# ==============================================================================

# Load packages
library(tidyverse)
library(janitor)
library(dplyr)

# Read in CSV files
prev_number <- read_csv("raw-data/number-with-depression-by-country.csv")
prev_age <- read_csv("raw-data/prevalence-of-depression-by-age.csv")
prev_sex <- read_csv("raw-data/prevalence-of-depression-males-vs-females.csv")

# Wrangle Prevalence of Depression by Country ==================================

prev_number <- prev_number %>%
  clean_names() %>%
  # 1990 <= year <= 2019
  filter(year >= 1990 & year <= 2019) %>%
  rename(number = prevalence_depressive_disorders_sex_both_age_all_ages_number)

# clean up for map visualization
prev_number <- prev_number %>%
  # drop all non-countries
  drop_na() %>%
  filter(entity != "World")

# Wrangle Prevalence of Depression by Sex ======================================

prev_sex <- prev_sex %>%
  clean_names() %>%
  # 1990 <= year <= 2019
  filter(year >= 1990 & year <= 2019) %>%
  rename(male = prevalence_depressive_disorders_sex_male_age_age_standardized_percent,
         female = prevalence_depressive_disorders_sex_female_age_age_standardized_percent)

prev_sex <- prev_sex %>%
  # remove unnecessary columns
  select(-population_historical_estimates,
         -continent) %>%
  # drop all non-countries
  drop_na()

# Wrangle Prevalence of Depression by Age ======================================

prev_age <- prev_age %>%
  clean_names() %>%
  # 1990 <= year <= 2019
  filter(year >= 1990 & year <= 2019) %>%
  rename(age20_24 = prevalence_depressive_disorders_sex_both_age_20_to_24_percent,
         age10_14 = prevalence_depressive_disorders_sex_both_age_10_to_14_percent,
         all_ages = prevalence_depressive_disorders_sex_both_age_all_ages_percent,
         above70 = prevalence_depressive_disorders_sex_both_age_70_years_percent,
         age30_34 = prevalence_depressive_disorders_sex_both_age_30_to_34_percent,
         age15_19 = prevalence_depressive_disorders_sex_both_age_15_to_19_percent,
         age25_29 = prevalence_depressive_disorders_sex_both_age_25_to_29_percent,
         age50_69 = prevalence_depressive_disorders_sex_both_age_50_69_years_percent,
         age_standardized = prevalence_depressive_disorders_sex_both_age_age_standardized_percent,
         age15_49 = prevalence_depressive_disorders_sex_both_age_15_49_years_percent) %>%
  # drop all non-countries
  drop_na()

# Save datasets ================================================================

# Check if subfolders exist; if not, create them
if (!dir.exists("data")) {
  dir.create("data")
}

# Save all cleaned data frames in a single RData file in *data* folder
save(prev_number,
     prev_age,
     prev_sex,
     file = "data/world-data.RData")



