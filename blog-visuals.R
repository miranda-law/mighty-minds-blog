# ==============================================================================
# This R script contains visuals for all visuals used in `index.Rmd`
# 
# Visuals:
#
# Author: Miranda Law
# `prev_number_map`: choropleth of world map on prevalence of depression
# `prev_number_map_df`: data frame for `prev_number_map`
# `prev_male_map`: choropleth of world map on % of males with depression
# `prev_female_map`: choropleth of world map on % of females with depression
# `relevant_sex_bar`: bar graph of prevalence of depression by sex for relevant
#     countries
# `relevant_age_bar`: bar graph of prevalence of depression by age for relevant
#     countries
#
# Author: Ellie DeCarlo
# `relevant_dygraph`: dygraph of prevalence of depression for Brazil, China, 
#     USA, India, Pakistan
# `relevant`: data frame for `relevant_dygraph`
# ==============================================================================

# Load packages ================================================================
library(tidyverse)
library(plotly)
library(ggplot2)
library(dygraphs)

# Load wrangled data ===========================================================
load("data/world-data.RData")

# Global Prevalence Choropleths ================================================

# specify map projection/options
map_options <- list(
  showframe = FALSE,
  showcoastlines = FALSE
  #projection = list(type = 'Mercator')
)

# Join prevalence in % for global choropleth
prev_number_map_df <- prev_number %>%
  left_join(y = prev_age %>% dplyr::select(entity,
                                           code,
                                           year,
                                           all_ages),
            by =  c("entity", "code", "year")) %>%
  rename(percent = all_ages)

# Choropleth of number of people with depression 
prev_number_map <- plot_geo(prev_number_map_df) %>%
  add_trace(z = ~percent, 
            color = ~percent, 
            colors = 'Blues',
            # labels for country
            text = ~paste('Diagnosed: ', number,
                          '</br>', entity),
            locations = ~code,
            # add slider by year
            frame = ~year) %>% 
  # rename legend
  colorbar(title = 'Percentage',
           tickprefix = '%') %>% 
  layout(title = '% of people with depression 1990 - 2019',
         geo = map_options)

# Choropleth of % of males with depression 
prev_male_map <- plot_geo(prev_sex) %>%
  add_trace(z = ~male, 
            color = ~male, 
            colors = 'Blues',
            # labels for country
            text = ~entity, 
            locations = ~code,
            # add slider by year
            frame = ~year) %>% 
  # rename legend
  colorbar(title = 'Percentage',
           tickprefix = '%') %>% 
  layout(title = '% of males with depression 1990 - 2019',
         geo = map_options)


# Choropleth of % of females with depression 
prev_female_map <- plot_geo(prev_sex) %>%
  add_trace(z = ~female, 
            color = ~female, 
            colors = 'Blues',
            # labels for country
            text = ~entity, 
            locations = ~code,
            # add slider by year
            frame = ~year) %>% 
  # rename legend
  colorbar(title = 'Percentage',
           tickprefix = '%') %>% 
  layout(title = '% of females with depression 1990 - 2019',
         geo = map_options)


# Relevant countries ===========================================================

countries <- c("BRA", "CHN", "USA", "IND", "PAK")
  
# Wrangle data for dygraph
relevant <- prev_number %>%
  filter(code %in% countries) %>%
  select(year, entity, number) %>%
  mutate(number = number/1000000) %>%
  rename("country" = entity,
         "number of people (millions)" = number) %>%
  pivot_wider(names_from = country, 
              values_from = "number of people (millions)")


# create dygraph
relevant_dygraph <- relevant %>% 
  dygraph(
    main = "Top 5 Countries with Mental Illness", 
    ylab = "Population with Mental Illness (in Millions)") %>%
  dyRangeSelector(dateWindow = c("1990", "2019")) %>%
  dyHighlight(highlightCircleSize = 4,
              highlightSeriesBackgroundAlpha = 1) %>%
  dyLegend(show = "auto", width = 600)

# create bar graph for sex
relevant_sex <- prev_sex %>%
  filter(code %in% countries)

# create bar graph for sex
relevant_sex_bar <- plot_ly(relevant_sex,
                        type = 'bar') %>% 
  add_trace(x = ~entity, y = ~male, name = 'Male',
            # add slider by year
            frame = ~year) %>%
  add_trace(x = ~entity, y = ~female, name = 'Female',
            # add slider by year
            frame = ~year) %>%
  layout(title = 'Prevalence by sex', 
         xaxis = list(title = 'Country'), 
         yaxis = list(title = 'Prevalence (%)'), 
         legend = list(title=list(text='<b> Sex </b>')))

# Wrangle data for age bar graph
relevant_age <- prev_age %>%
  filter(code %in% countries) %>%
  select(year, 
         entity,
         age10_14,
         age15_49,
         age50_69,
         above70,
         age_standardized) 

# create bar graph for age
relevant_age_bar <- plot_ly(relevant_age,
               type = 'bar') %>% 
  add_trace(x = ~entity, y = ~age10_14, name = 'Ages 10-14',
            # add slider by year
            frame = ~year) %>% 
  add_trace(x = ~entity, y = ~age15_49, name = 'Ages 15-49',
            # add slider by year
            frame = ~year) %>% 
  add_trace(x = ~entity, y = ~age50_69, name = 'Ages 50-69',
            # add slider by year
            frame = ~year) %>% 
  add_trace(x = ~entity, y = ~above70, name = 'Ages 70+',
            # add slider by year
            frame = ~year) %>% 
  add_trace(x = ~entity, y = ~age_standardized, name = 'Age standardized',
            # add slider by year
            frame = ~year) %>% 
  layout(title = 'Prevalence by age', 
         xaxis = list(title = 'Country'), 
         yaxis = list(title = 'Prevalence (%)'), 
         legend = list(title=list(text='<b> Age group </b>')),
         barmode = 'group')

  
# Save datasets ================================================================

# Check if subfolders exist; if not, create them
if (!dir.exists("data")) {
  dir.create("data")
}

# Save all blog visuals in a single RData file in *data* folder
save(prev_number_map_df,
     prev_number_map,
     prev_male_map,
     prev_female_map,
     relevant,
     relevant_dygraph,
     relevant_age_bar,
     relevant_sex_bar,
     file = "data/blog-visuals.RData")
