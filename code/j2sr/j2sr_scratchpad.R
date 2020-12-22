library(tidyverse)
library(fs)
library(readxl)

# load add_group_index()
setwd("C:/Users/Stephen/Desktop/R/assorted_helper_scripts")
source("add_group_index.R")

# setwd
setwd("C:/Users/Stephen/Desktop/R/mcp")

# load j2sr data
# https://selfreliance.usaid.gov/country/albania#tab-methodology
dir_ls("data")
j2sr <- read_xlsx(path = "data/fy_2021_j2sr_dataset_all.xlsx", sheet = "FY21 Trends")
j2sr
j2sr %>% glimpse()

# inspect
j2sr %>% distinct(country)
j2sr %>% distinct(metric)
j2sr %>% distinct(region)
j2sr %>% filter(region == "Europe and Eurasia") %>% distinct(country)


#/////////////////////////////////////////////////////////////////////////////////////////////


# add capacity and commitment flags
j2sr <- j2sr %>% mutate(capacity_flag = case_when(metric %in% c("Government Effectiveness", "Tax System Effectiveness",
                                                        "Safety & Security", "Civil Society & Media Effectiveness",
                                                        "Poverty Rate ($5/Day)", "Education Quality", "Child Health",
                                                        "GDP Per Capita (PPP)", 
                                                        "Information & Communication Technology (ICT) Adoption",
                                                        "Export Sophistication") ~ 1, TRUE ~ 0),
                        commitment_flag = case_when(metric %in% c("Liberal Democracy", "Open Government",
                                                                  "Social Group Equality", "Economic Gender Gap",
                                                                  "Business & Investment Environment",
                                                                  "Trade Freedom", "Environmental Policy") ~ 1, TRUE ~ 0))

# inspect
j2sr %>% count(capacity_flag, commitment_flag, metric)


#////////////////////////////


# inspect

# note that not all country/year combos have the full 17 metrics available
j2sr %>% distinct(metric)
j2sr %>% filter(raw_observation_year == 2018) %>% group_by(country) %>% count() %>% arrange(desc(n))
j2sr %>% filter(raw_observation_year == 2018) %>% distinct(metric) %>%
        anti_join(j2sr %>% distinct(metric), ., by = "metric")


#////////////////////////////


# add most_recent_flag for each country/metric
j2sr <- j2sr %>% group_by(country, metric) %>% arrange(desc(raw_observation_year)) %>% 
        add_group_index(group_vars = raw_observation_year, group_name = "raw_observation_year_index") %>% 
        ungroup()


#////////////////////////////


# inspect 
# note that 85 countries still lack all 17 indicators with raw_observation_year_index = 1
j2sr %>% filter(raw_observation_year_index == 1) %>% group_by(country) %>% count() %>% 
        ungroup() %>% arrange(desc(n)) %>% filter(n != 17) %>% print(n = nrow(.))
# for instance, angola never has a record for the tax system effectiveness metric
j2sr %>% filter(country == "Angola", raw_observation_year_index == 1) %>%
        anti_join(j2sr %>% distinct(metric), ., by = "metric")
j2sr %>% filter(country == "Angola") %>% count(metric) 
# marshall islands has the least metrics, with only 3 metrics ever available
j2sr %>% filter(country == "Marshall Islands") %>% count(metric) 


#////////////////////////////


# compare country to j2sr dashboard to confirm metrics match
# result: they do match exactly the j2sr dashboard for albania 
# note there may be some quirks to calculating some countries; 
# the methodology documents some data edits, but i think those are all made upstream of this dataset for download
# note the marshall islands in this dataset, which has the least metrics available, matches the dashboard exactly
j2sr %>% filter(country == "Albania", raw_observation_year_index == 1)
j2sr %>% filter(country == "Albania", raw_observation_year_index == 1) %>%
        filter(capacity_flag == 1) %>% summarize(capacity_mean = mean(fy21_roadmap_value))
j2sr %>% filter(country == "Albania", raw_observation_year_index == 1) %>%
        filter(commitment_flag == 1) %>% summarize(commitment_mean = mean(fy21_roadmap_value))

j2sr %>% filter(country == "Marshall Islands", raw_observation_year_index == 1)




