extrafont::loadfonts(device="win") 
library(tidyverse)
library(lubridate)
library(readxl)
library(skimr)
library(rlang)
library(haven)
library(fs)
library(officer)
library(devEMF)
library(ggrepel)
library(rvest)
library(scales)
library(testthat)
library(patchwork)
library(vdemdata)
library(naniar)
library(corrr)
library(GGally)
library(Polychrome)
library(viridis)
library(bazar)
library(treemapify)
library(openxlsx)


# setwd
setwd("C:/Users/sdevine/Desktop/usaid/mcp/mcp_data")
options(scipen = 999)


#//////////////////////////////////////////////////////////////////////////////////////////////////////


# create custom color_palette ####
color_palette <- tibble(hex = c("#083D7F", "#2474B6", "#8BBFD0",
                                "#CBCBCB", "#7D7D7D",
                                "#99ba78", "#35B779FF", "#006629", 
                                "#E4DC68", "#FDA159", "#EF6712", "#CE1B1E",
                                "#8B008B", "#DA70D6"))
color_palette
color_palette %>% pull(hex) %>% show_col()

# color_palette supports 11 colors, plus possible extensions via fill/line type
show_col(color_palette %>% slice(1, 3) %>% pull(hex)) # 2 colors
show_col(color_palette %>% slice(1, 2, 3) %>% pull(hex)) # 3 colors
show_col(color_palette %>% slice(1, 2, 3, 4) %>% pull(hex)) # 4 colors
show_col(color_palette %>% slice(1, 2, 3, 4, 5) %>% pull(hex)) # 5 colors
show_col(color_palette %>% slice(1, 2, 3, 4, 5, 6) %>% pull(hex)) # 6 colors
show_col(color_palette %>% slice(1, 2, 3, 4, 5, 6, 7) %>% pull(hex)) # 7 colors
show_col(color_palette %>% slice(1, 2, 3, 4, 5, 6, 7, 8) %>% pull(hex)) # 8 colors
show_col(color_palette %>% slice(1, 2, 3, 4, 5, 6, 7, 8, 9) %>% pull(hex)) # 9 colors
show_col(color_palette %>% slice(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) %>% pull(hex)) # 10 colors
show_col(color_palette %>% slice(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11) %>% pull(hex)) # 11 colors


#//////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////


# read country_crosswalk and get country_crosswalk_expanded ####
current_wd <- getwd()
setwd("C:/Users/sdevine/Desktop/usaid/mcp/useful_info/country_crosswalk")
country_crosswalk <- read_csv("country_crosswalk.csv")
setwd(current_wd)

# get country_crosswalk_expanded to have a country record for each year
country_crosswalk_expanded <- expand_grid(country = country_crosswalk %>% pull(country), 
                                          year = seq(from = 2001, to = 2021, by = 1)) %>%
        left_join(., country_crosswalk, by = "country")


#/////////////////


# inspect
country_crosswalk_expanded
country_crosswalk_expanded %>% glimpse()
country_crosswalk_expanded %>% count(country) %>% distinct(n)
country_crosswalk_expanded %>% count(year) %>% print(n = nrow(.))

country_crosswalk %>% glimpse()
country_crosswalk %>% print(n = nrow(.))
country_crosswalk %>% skim()
country_crosswalk %>% count(mcp_grouping)


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# load fdi ####

# data downloaded via IMF website login bulk download
# https://data.imf.org/?sk=40313609-F037-48C1-84B1-E1F1CE54D6D5&sId=1390288795525

# note that fdi data is a stock or total position, not a flow
# https://data.imf.org/?sk=40313609-F037-48C1-84B1-E1F1CE54D6D5&sId=1390288795525
# "2. Are CDIS data position or flow data?
# CDIS includes only direct investment position (stock) data"

# note data is only available from IMF server up to 2019; can request up to 2050, but only up to 2019 downloads

# note that reported values are used, not mirror/derived values from counterparts
# note see fmir_data.R for more inspection of mirror/derived data differences
# so inward fdi from russia is taken as reported by receiving country
# picking one official perspective is how IMF reports a country's official exports/imports 
# to counterparties in their DOT statistics yearbook
# even though exports/imports have the same asymmetries with mirror/derived values reported by counterparts
# for exports/imports, atlas applies statistical cleaning to get better measure, and maybe the midpoint is at least a bit better
# but for simplicity and to keep with official reporting, will use the official non-derived values reported by 
# ee presence/CARS/graudates countries
# DOT yearbook: https://data.imf.org/?sk=9d6028d4-f14a-464c-a2f2-59b2cd424b85&sId=1488236767350
# https://data.imf.org/?sk=40313609-F037-48C1-84B1-E1F1CE54D6D5&sId=1410469360660

# note the default assumption would be values are reported in current dollars, 
# since that the docs don't seem to mention any adjustments
# note that values of C for censored are converted to NA
fdi <- read_csv(file = "data/imf/cdis/CDIS_01-16-2023 03-26-55-99_timeSeries.csv") %>% 
        rename(country_name = `Country Name`, counterpart = `Counterpart Country Name`, 
               category = `Indicator Name`) %>%
        select(-`...21`) %>%
        filter(Attribute == "Value") %>%
        pivot_longer(cols = -c(country_name, `Country Code`, category, `Indicator Code`,
                               counterpart, `Counterpart Country Code`,
                               `Attribute`), names_to = "year", values_to = "values") %>%
        select(country_name, counterpart, year, category, values) %>%
        mutate(values = as.numeric(values),
               year = as.numeric(year),
               country_name = case_when(country_name == "Armenia, Rep. of" ~ "Armenia",
                                        country_name == "Azerbaijan, Rep. of" ~ "Azerbaijan",
                                        country_name == "Belarus, Rep. of" ~ "Belarus",
                                        country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "Croatia, Rep. of" ~ "Croatia",
                                        country_name == "Czech Rep." ~ "Czechia",
                                        country_name == "Estonia, Rep. of" ~ "Estonia",
                                        country_name == "Kazakhstan, Rep. of" ~ "Kazakhstan",
                                        country_name == "Kosovo, Rep. of" ~ "Kosovo",
                                        country_name == "Kyrgyz Rep." ~ "Kyrgyzstan",
                                        country_name == "Moldova, Rep. of" ~ "Moldova",
                                        country_name == "Netherlands, The" ~ "Netherlands",
                                        country_name == "North Macedonia, Republic of" ~ "N. Macedonia",
                                        country_name == "Poland, Rep. of" ~ "Poland",
                                        country_name == "Russian Federation" ~ "Russia",
                                        country_name == "Serbia, Rep. of" ~ "Serbia",
                                        country_name == "Slovak Rep." ~ "Slovakia",
                                        country_name == "Slovenia, Rep. of" ~ "Slovenia",
                                        country_name == "Tajikistan, Rep. of" ~ "Tajikistan",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States" ~ "U.S.",
                                        country_name == "Uzbekistan, Rep. of" ~ "Uzbekistan",
                                        country_name == "China, P.R.: Mainland" ~ "China",
                                        TRUE ~ country_name),
               counterpart = case_when(counterpart == "Armenia, Rep. of" ~ "Armenia",
                                       counterpart == "Azerbaijan, Rep. of" ~ "Azerbaijan",
                                       counterpart == "Belarus, Rep. of" ~ "Belarus",
                                       counterpart == "Bosnia and Herzegovina" ~ "BiH",
                                       counterpart == "Croatia, Rep. of" ~ "Croatia",
                                       counterpart == "Czech Rep." ~ "Czechia",
                                       counterpart == "Estonia, Rep. of" ~ "Estonia",
                                       counterpart == "Kazakhstan, Rep. of" ~ "Kazakhstan",
                                       counterpart == "Kosovo, Rep. of" ~ "Kosovo",
                                       counterpart == "Kyrgyz Rep." ~ "Kyrgyzstan",
                                       counterpart == "Moldova, Rep. of" ~ "Moldova",
                                       counterpart == "Netherlands, The" ~ "Netherlands",
                                       counterpart == "North Macedonia, Republic of" ~ "N. Macedonia",
                                       counterpart == "Poland, Rep. of" ~ "Poland",
                                       counterpart == "Russian Federation" ~ "Russia",
                                       counterpart == "Serbia, Rep. of" ~ "Serbia",
                                       counterpart == "Slovak Rep." ~ "Slovakia",
                                       counterpart == "Slovenia, Rep. of" ~ "Slovenia",
                                       counterpart == "Tajikistan, Rep. of" ~ "Tajikistan",
                                       counterpart == "United Kingdom" ~ "U.K.",
                                       counterpart == "United States" ~ "U.S.",
                                       counterpart == "Uzbekistan, Rep. of" ~ "Uzbekistan",
                                       counterpart == "China, P.R.: Mainland" ~ "China",
                                       TRUE ~ counterpart)) %>%
        filter(!str_detect(string = counterpart, pattern = regex("emerging", ignore_case = TRUE)),
               !str_detect(string = counterpart, pattern = regex("not specified", ignore_case = TRUE)),
               !str_detect(string = counterpart, pattern = regex("middle east", ignore_case = TRUE)),
               !str_detect(string = counterpart, pattern = regex("hemisphere", ignore_case = TRUE)),
               !(counterpart %in% c("Advanced Economies",
                                    "Africa",
                                    "CIS",
                                    "Euro Area",
                                    "European Union",
                                    "Other Countries not included elsewhere",
                                    "Special Categories",
                                    "Sub-Saharan Africa",
                                    "British Indian Ocean Territory",
                                    "Central and South Asia",
                                    "Economies of Persian Gulf",
                                    "Europe",
                                    "French Southern Territories",
                                    "French Polynesia",
                                    "Other Near and Middle East Economies",
                                    "South America",
                                    "US Pacific Islands",
                                    "North and Central America")),
               !str_detect(string = country_name, pattern = regex("emerging", ignore_case = TRUE)),
               !str_detect(string = country_name, pattern = regex("not specified", ignore_case = TRUE)),
               !str_detect(string = country_name, pattern = regex("middle east", ignore_case = TRUE)),
               !str_detect(string = country_name, pattern = regex("hemisphere", ignore_case = TRUE)),
               !(country_name %in% c("Advanced Economies",
                                     "Africa",
                                     "CIS",
                                     "Euro Area",
                                     "European Union",
                                     "Other Countries not included elsewhere",
                                     "Special Categories",
                                     "Sub-Saharan Africa",
                                     "British Indian Ocean Territory",
                                     "Central and South Asia",
                                     "Economies of Persian Gulf",
                                     "Europe",
                                     "French Southern Territories",
                                     "French Polynesia",
                                     "North Africa",
                                     "North and Central America",
                                     "North Atlantic and Caribbean",
                                     "Other Near and Middle East Economies",
                                     "South America",
                                     "US Pacific Islands",
                                     "North and Central America")))


#//////////////////////


# inspect
fdi
fdi %>% glimpse()
fdi %>% nrow() # 1149655
fdi %>% ncol() # 5


fdi %>% arrange(desc(values)) %>% distinct(country_name)
fdi %>% skim()
fdi %>% count(country_name) %>% print(n = nrow(.))
fdi %>% count(counterpart) %>% print(n = nrow(.))

# note fdi data only runs from 2009-2021 as of 20230115
fdi %>% count(year)

# inspect country names
fdi %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
        distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
        anti_join(., fdi, by = c("country" = "country_name")) %>% select(country)

fdi %>% 
        filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
        distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////


# inspect
fdi %>% distinct(category)

# inspect specific countries
fdi %>% 
        filter(category == "Inward Direct Investment Positions, US Dollars",
               country_name == "Albania", year == 2009, counterpart == "Australia")
fdi %>% 
        filter(category == "Inward Direct Investment Positions, US Dollars",
               !is.na(values)) %>%
        count(country_name, year, counterpart) %>% distinct(n)
fdi %>% 
        filter(category == "Inward Direct Investment Positions, US Dollars",
               country_name == "Albania", year == 2009) %>% distinct(values)

# inspect serbia fdi_shares
fdi %>%
        filter(category == "Inward Direct Investment Positions, US Dollars",
                country_name == "Serbia", year == 2019, counterpart == "World")
fdi %>%
        filter(category == "Inward Direct Investment Positions, US Dollars",
                country_name == "Serbia", year == 2019) %>%
        mutate(fdi_from_world = 43844973018,
               share_of_fdi = values / fdi_from_world) %>%
        arrange(desc(share_of_fdi)) %>% print(n = 20)
fdi %>%
        filter(category == "Inward Direct Investment Positions, US Dollars",
               country_name == "Serbia", year == 2019,
               !(counterpart %in% c("Europe", "World", "East Asia", "North and Central America",
                                    "Economies of Persian Gulf"))) %>%
        mutate(fdi_from_world = 43844973018,
               share_of_fdi = values / fdi_from_world) %>%
        arrange(desc(share_of_fdi)) %>%
        mutate(cumsum_share_of_fdi = cumsum(replace_na(share_of_fdi, replace = 0))) %>%
        print(n = 20)

# treemap of serbia share_of_fdi
# https://cran.r-project.org/web/packages/treemapify/vignettes/introduction-to-treemapify.html
# https://r-charts.com/part-whole/treemapify/
fdi %>%
        filter(category == "Inward Direct Investment Positions, US Dollars",
               country_name == "Serbia", year == 2019,
               !(counterpart %in% c("Europe", "World", "East Asia", "North and Central America",
                                    "Economies of Persian Gulf"))) %>%
        mutate(fdi_from_world = 43844973018,
               share_of_fdi = values / fdi_from_world) %>%
        arrange(desc(share_of_fdi)) %>% slice(1:20) %>%
        mutate(counterpart = case_when(counterpart == "United Arab Emirates" ~ "UAE",
                                       counterpart == "China, P.R.: Hong Kong" ~ "HK",
                                       TRUE ~ counterpart)) %>%
        ggplot(data = ., mapping = aes(area = share_of_fdi, fill = share_of_fdi, label = counterpart)) +
        geom_treemap() + 
        geom_treemap_text(colour = "white",
                          place = "centre",
                          size = 5, 
                          grow = TRUE)


#/////////////////////////////////////////////////////////////////////////////////////////////


# load gdp_current ####
# https://data.worldbank.org/indicator/NY.GDP.MKTP.CD

gdp_current <- read_excel(path = "data/world_bank/API_NY.GDP.MKTP.CD_DS2_en_excel_v2_4334130.xls", sheet = "Data", skip = 3) %>%
        rename(country_name = `Country Name`, iso3 = `Country Code`) %>%
        select(-c(`Indicator Name`, `Indicator Code`)) %>%
        pivot_longer(cols = -c(country_name, iso3), names_to = "year", values_to = "gdp_current") %>%
        mutate(year = as.numeric(year))


#/////////////////


# inspect
gdp_current
gdp_current %>% glimpse()
gdp_current %>% nrow() # 16492
gdp_current %>% ncol() # 4

gdp_current %>% count(year) %>% arrange(desc(year)) %>% print(n = nrow(.))
gdp_current %>% filter(year == 2021) %>% distinct(gdp_current)
gdp_current %>% filter(is.na(gdp_current), year >= 2010) %>% count(country_name) %>% arrange(desc(n)) %>% print(n = 20)


#/////////////////////////////////////////////////////////////////////////////////////////////


# get inward_fdi_from_all_counterparts ####

# note inward_fdi_from_all_counterparts includes all bilateral records in the raw IMF data
# below is a subset called inward_fdi_from_russia_and_china that only includes russia/china as counterparts
# inward_fdi_from_all_counterparts is needed for charts showing all counterparts
# inward_fdi_from_russia_and_china is easier to work with for fmi-type charts

# join to country_crosswalk_expanded, filter to fdi data range (2009-2020), join inward_fdi_from_world, 
# get inward_fdi_as_share_of_total_fdi and inward_fdi_as_share_of_gdp
# note that world bank also has fdi_inflows_from_world_as_share_of_gdp
# https://data.worldbank.org/indicator/BX.KLT.DINV.WD.GD.ZS
inward_fdi_from_all_counterparts <- country_crosswalk_expanded %>% 
        filter(ee_region_flag == 1 | country == "U.S.") %>%
        left_join(., fdi %>% select(country_name, year, category, counterpart, values) %>% 
                          rename(inward_fdi_from_counterpart = values) %>%
                          filter(category == "Inward Direct Investment Positions, US Dollars"),
                  by = c("country" = "country_name", "year")) %>%
        left_join(., fdi %>% select(country_name, year, category, counterpart, values) %>% 
                          filter(counterpart == "World", category == "Inward Direct Investment Positions, US Dollars") %>% 
                          select(-counterpart) %>% rename(inward_fdi_from_world = values),
                  by = c("country" = "country_name", "year", "category")) %>%
        left_join(., gdp_current %>% select(-country_name), by = c("iso3", "year")) %>%
        filter(year >= 2009, year <= 2021) %>%
        mutate(inward_fdi_as_share_of_total_fdi = inward_fdi_from_counterpart / inward_fdi_from_world,
               inward_fdi_as_share_of_gdp = inward_fdi_from_counterpart / gdp_current,
               inward_fdi_from_world_as_share_of_gdp = inward_fdi_from_world / gdp_current)


#////////////////////////


# inspect
inward_fdi_from_all_counterparts
inward_fdi_from_all_counterparts %>% glimpse()
inward_fdi_from_all_counterparts %>% nrow() # 121420
inward_fdi_from_all_counterparts %>% ncol() # 23

inward_fdi_to_all_counterparts %>% count(country) %>% print(n = nrow(.))
inward_fdi_to_all_counterparts %>% count(year) %>% print(n = nrow(.))
inward_fdi_to_all_counterparts %>% count(counterpart) %>% print(n = nrow(.))

# inspect
inward_fdi_from_all_counterparts %>% 
        # filter(counterpart == "Russia") %>%
        filter(counterpart == "China") %>%
        select(country, ee_region_flag, year, counterpart, inward_fdi_from_counterpart, 
                                            inward_fdi_from_world, inward_fdi_as_share_of_total_fdi, inward_fdi_as_share_of_gdp,
                                            inward_fdi_from_world_as_share_of_gdp)

# unique at country/year level; each country/year/counterpart has only one record
inward_fdi_from_all_counterparts %>% count(country, year, counterpart) %>% arrange(desc(n))
inward_fdi_from_all_counterparts %>% filter(country == "Serbia", year == 2011) %>%
        select(country, ee_region_flag, year, counterpart, inward_fdi_from_counterpart, 
               inward_fdi_from_world, inward_fdi_as_share_of_total_fdi, inward_fdi_as_share_of_gdp,
               inward_fdi_from_world_as_share_of_gdp)


# check most recent year of data availability
# for russia, kosovo's most recent is 2017, serbia is 2019, the rest of e&E presence have 2020 records
# for china, kosovo's most recent is 2012, serbia is 2019, the rest of e&E presence have 2020 records
inward_fdi_from_all_counterparts %>% 
        pivot_longer(cols = c(inward_fdi_as_share_of_gdp), names_to = "var", values_to = "values") %>%
        # filter(counterpart == "Russia") %>%
        filter(counterpart == "China") %>%
        unite(col = var, var, counterpart, remove = FALSE) %>%
        select(country, mcp_grouping, year, var, values) %>%
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        group_by(country, var) %>%
        arrange(desc(year)) %>%
        mutate(non_na_year = case_when(!is.na(values) ~ year)) %>%
        filter(non_na_year == max(non_na_year, na.rm = TRUE)) %>%
        ungroup() %>%
        arrange(country, desc(non_na_year)) %>%
        distinct(country, year) %>%
        left_join(country_crosswalk %>% filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>% select(country), .,
                  by = c("country" = "country")) %>%
        arrange(year)


# plot missing data
inward_fdi_from_all_counterparts %>% 
        pivot_longer(cols = c(inward_fdi_as_share_of_gdp), names_to = "var", values_to = "values") %>%
        select(country, mcp_grouping, year, counterpart, var, values) %>%
        filter(counterpart %in% c("China", "Russia")) %>%
        unite(col = var, var, counterpart) %>%
        # filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        select(country, year, var, values) %>%
        group_by(country, year, var) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", var, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )

# plot
inward_fdi_from_all_counterparts %>% 
        pivot_longer(cols = c(inward_fdi_as_share_of_gdp), names_to = "var", values_to = "values") %>%
        select(country, mcp_grouping, year, counterpart, var, values) %>%
        filter(counterpart %in% c("China", "Russia")) %>%
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        # filter(mcp_grouping == "E&E Balkans") %>%
        # filter(mcp_grouping == "E&E Eurasia") %>%
        filter(counterpart == "Russia") %>%
        # filter(counterpart == "China") %>%
        filter(year >= 2010) %>%
        filter(!is.na(values)) %>%
        ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line()


#///////////////////


# test values
test_values_inward_fdi_from_all_counterparts <- function() {
        
        # 1
        expect_equal(object = inward_fdi_from_all_counterparts %>% filter(country == "Albania", counterpart == "Germany", year == 2018) %>%
                             pull(inward_fdi_as_share_of_total_fdi),
                     expected = 134146701.609575 / 7661392484.30699)
        
        # 2
        expect_equal(object = inward_fdi_from_all_counterparts %>% filter(country == "Albania", counterpart == "Germany", year == 2018) %>%
                             pull(inward_fdi_as_share_of_gdp),
                     expected = 134146701.609575 / 15156432309.8977)
        
        # 3
        expect_equal(object = inward_fdi_from_all_counterparts %>% filter(country == "Albania", counterpart == "Germany", year == 2018) %>%
                             pull(inward_fdi_from_world_as_share_of_gdp),
                     expected = 7661392484.30699 / 15156432309.8977)
        
        # 4
        expect_equal(object = inward_fdi_from_all_counterparts %>% filter(country == "Ukraine", counterpart == "Belarus", year == 2019) %>%
                             pull(inward_fdi_as_share_of_total_fdi),
                     expected = 118673024.678252 / 54209571897.9821)
        
        # 5
        expect_equal(object = inward_fdi_from_all_counterparts %>% filter(country == "Ukraine", counterpart == "Belarus", year == 2019) %>%
                             pull(inward_fdi_as_share_of_gdp),
                     expected = 118673024.678252 / 153882982016.281)
        
        # 6
        expect_equal(object = inward_fdi_from_all_counterparts %>% filter(country == "Ukraine", counterpart == "Belarus", year == 2019) %>%
                             pull(inward_fdi_from_world_as_share_of_gdp),
                     expected = 54209571897.9821 / 153882982016)
}
test_values_inward_fdi_from_all_counterparts()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# inward_fdi_from_all_counterparts %>%
#         write_csv(file = "data/imf/cdis/inward_fdi_from_all_counterparts_20230115.csv")
inward_fdi_from_all_counterparts <- read_csv(file = "data/imf/cdis/inward_fdi_from_all_counterparts_20220805.csv")

# inspect
inward_fdi_from_all_counterparts
inward_fdi_from_all_counterparts %>% glimpse()
inward_fdi_from_all_counterparts %>% nrow() # 121420
inward_fdi_from_all_counterparts %>% ncol() # 23

inward_fdi_from_all_counterparts %>% select(country, ee_region_flag, year, counterpart, inward_fdi_from_counterpart, 
                              inward_fdi_from_world, inward_fdi_as_share_of_total_fdi, inward_fdi_as_share_of_gdp,
                              inward_fdi_from_world_as_share_of_gdp)


#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# get outward_fdi_to_all_counterparts ####

# note outward_fdi_to_all_counterparts includes all bilateral records in the raw IMF data
# below is a subset called outward_fdi_from_russia_and_china that only includes russia/china as counterparts
# outward_fdi_to_all_counterparts is needed for charts showing all counterparts
# outward_fdi_from_russia_and_china is easier to work with for fmi-type charts

# join to country_crosswalk_expanded, filter to fdi data range (2009-2020), join outward_fdi_from_world, 
# get outward_fdi_as_share_of_total_fdi and outward_fdi_as_share_of_gdp
# note that world bank also has fdi_inflows_from_world_as_share_of_gdp
# https://data.worldbank.org/indicator/BX.KLT.DINV.WD.GD.ZS
outward_fdi_to_all_counterparts <- country_crosswalk_expanded %>% 
        filter(ee_region_flag == 1 | country == "U.S.") %>%
        left_join(., fdi %>% select(country_name, year, category, counterpart, values) %>% 
                          rename(outward_fdi_from_counterpart = values) %>%
                          filter(category == "Outward Direct Investment Positions, US Dollars"),
                  by = c("country" = "country_name", "year")) %>%
        left_join(., fdi %>% select(country_name, year, category, counterpart, values) %>% 
                          filter(counterpart == "World", category == "Outward Direct Investment Positions, US Dollars") %>% 
                          select(-counterpart) %>% rename(outward_fdi_from_world = values),
                  by = c("country" = "country_name", "year", "category")) %>%
        left_join(., gdp_current %>% select(-country_name), by = c("iso3", "year")) %>%
        filter(year >= 2009, year <= 2021) %>%
        mutate(outward_fdi_as_share_of_total_fdi = outward_fdi_from_counterpart / outward_fdi_from_world,
               outward_fdi_as_share_of_gdp = outward_fdi_from_counterpart / gdp_current,
               outward_fdi_from_world_as_share_of_gdp = outward_fdi_from_world / gdp_current)


#////////////////////////


# inspect
outward_fdi_to_all_counterparts
outward_fdi_to_all_counterparts %>% glimpse()
outward_fdi_to_all_counterparts %>% nrow() # 101205
outward_fdi_to_all_counterparts %>% ncol() # 23

outward_fdi_to_all_counterparts %>% count(country) %>% print(n = nrow(.))
outward_fdi_to_all_counterparts %>% count(year) %>% print(n = nrow(.))
outward_fdi_to_all_counterparts %>% count(counterpart) %>% print(n = nrow(.))


# inspect
outward_fdi_to_all_counterparts %>% 
        # filter(counterpart == "Russia") %>%
        filter(counterpart == "China") %>%
        select(country, ee_region_flag, year, counterpart, outward_fdi_from_counterpart, 
               outward_fdi_from_world, outward_fdi_as_share_of_total_fdi, outward_fdi_as_share_of_gdp,
               outward_fdi_from_world_as_share_of_gdp)

# unique at country/year level; each country/year/counterpart has only one record
outward_fdi_to_all_counterparts %>% count(country, year, counterpart) %>% arrange(desc(n))
outward_fdi_to_all_counterparts %>% filter(country == "Serbia", year == 2011) %>%
        select(country, ee_region_flag, year, counterpart, outward_fdi_from_counterpart, 
               outward_fdi_from_world, outward_fdi_as_share_of_total_fdi, outward_fdi_as_share_of_gdp,
               outward_fdi_from_world_as_share_of_gdp)


# check most recent year of data availability

# for china
# albania, armenia, georgia, molodova, and ukraine have no records in timeframe
# bih and serbia have 2019 as most recent, azerbaijan, belarus, kosovo, and macedonia have 2020

# for russia
# georgia, moldova, ukraine have no records
# armenia most recent is 2011, serbia 2019, and the rest 2020
outward_fdi_to_all_counterparts %>% 
        pivot_longer(cols = c(outward_fdi_as_share_of_gdp), names_to = "var", values_to = "values") %>%
        filter(counterpart == "Russia") %>%
        # filter(counterpart == "China") %>%
        unite(col = var, var, counterpart, remove = FALSE) %>%
        select(country, mcp_grouping, year, var, values) %>%
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        group_by(country, var) %>%
        arrange(desc(year)) %>%
        mutate(non_na_year = case_when(!is.na(values) ~ year)) %>%
        filter(non_na_year == max(non_na_year, na.rm = TRUE)) %>%
        ungroup() %>%
        arrange(country, desc(non_na_year)) %>%
        distinct(country, year) %>%
        left_join(country_crosswalk %>% filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>% select(country), .,
                  by = c("country" = "country")) %>%
        arrange(year)


# plot missing data
outward_fdi_to_all_counterparts %>% 
        pivot_longer(cols = c(outward_fdi_as_share_of_gdp), names_to = "var", values_to = "values") %>%
        select(country, mcp_grouping, year, counterpart, var, values) %>%
        filter(counterpart %in% c("China", "Russia")) %>%
        unite(col = var, var, counterpart) %>%
        # filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        select(country, year, var, values) %>%
        group_by(country, year, var) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", var, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )

# plot
outward_fdi_to_all_counterparts %>% 
        pivot_longer(cols = c(outward_fdi_as_share_of_gdp), names_to = "var", values_to = "values") %>%
        select(country, mcp_grouping, year, counterpart, var, values) %>%
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        # filter(mcp_grouping == "E&E Balkans") %>%
        # filter(mcp_grouping == "E&E Eurasia") %>%
        # filter(counterpart == "Russia") %>%
        filter(counterpart == "China") %>%
        filter(year >= 2009) %>%
        filter(!is.na(values)) %>%
        ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line()


#///////////////////


# test values
test_values_outward_fdi_to_all_counterparts <- function() {
        
        # 1
        expect_equal(object = outward_fdi_to_all_counterparts %>% filter(country == "BiH", counterpart == "Russia", year == 2013) %>%
                             select(country, ee_region_flag, year, counterpart, outward_fdi_from_counterpart, 
                                    outward_fdi_from_world, outward_fdi_as_share_of_total_fdi, gdp_current, outward_fdi_as_share_of_gdp,
                                    outward_fdi_from_world_as_share_of_gdp) %>%
                             pull(outward_fdi_as_share_of_gdp),
                     expected = 319658.496543666 / 18172335776.3301)
        
        # 2
        expect_equal(object = outward_fdi_to_all_counterparts %>% filter(country == "BiH", counterpart == "Russia", year == 2014) %>%
                             pull(outward_fdi_as_share_of_gdp),
                     expected = NA_real_)
        
        # 3
        expect_equal(object = outward_fdi_to_all_counterparts %>% filter(country == "BiH", counterpart == "China", year == 2019) %>%
                             pull(outward_fdi_as_share_of_gdp),
                     expected = 73798.8803165228 / 20201323282.5451)
        
        # 4
        expect_equal(object = outward_fdi_to_all_counterparts %>% filter(country == "Azerbaijan", counterpart == "Russia", year == 2020) %>%
                             pull(outward_fdi_as_share_of_gdp),
                     expected = 895553591.941624 / 42693000000)
        
        # 5
        expect_equal(object = outward_fdi_to_all_counterparts %>% filter(country == "Armenia", counterpart == "China", year == 2019) %>%
                             pull(outward_fdi_as_share_of_gdp),
                     expected = numeric(0))
     
}
test_values_outward_fdi_to_all_counterparts()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# outward_fdi_to_all_counterparts %>%
#         write_csv(file = "data/imf/cdis/outward_fdi_to_all_counterparts_20230115.csv")
outward_fdi_to_all_counterparts <- read_csv(file = "data/imf/cdis/outward_fdi_to_all_counterparts_20220805.csv")

# inspect
outward_fdi_to_all_counterparts
outward_fdi_to_all_counterparts %>% glimpse()
outward_fdi_to_all_counterparts %>% nrow() # 101205
outward_fdi_to_all_counterparts %>% ncol() # 23

outward_fdi_to_all_counterparts %>% select(country, ee_region_flag, year, counterpart, outward_fdi_from_counterpart, 
                                            outward_fdi_from_world, outward_fdi_as_share_of_total_fdi, outward_fdi_as_share_of_gdp,
                                            outward_fdi_from_world_as_share_of_gdp)


#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# load exports ####

# data is available up to 2020 from IMF server
# https://data.imf.org/?sk=9d6028d4-f14a-464c-a2f2-59b2cd424b85&sId=1409151240976

# use free on board for exports, and CIF for imports
# note the IMF BPM6 recommendation is to use free on board valuation for exports and exports, but the UN IMTS 2010 recommendaiton
# is to use CIF for imports
# in the DOTS data, only fob is available for exports, but fob for imports only has like 12 countries with records, all the rest use cif
# https://unstats.un.org/unsd/trade/eg-imts/IMTS%202010%20(English).pdf
# https://www.imf.org/external/pubs/ft/bop/2014/pdf/guide.pdf
# https://ourworldindata.org/trade-data-sources-discrepancies

exports <- read_csv(file = "data/imf/dots/DOT_08-05-2022 16-10-55-19_timeSeries.csv") %>%
        rename(country_name = `Country Name`,
               counterpart = `Counterpart Country Name`,
               category = `Indicator Name`) %>%
        filter(category == "Goods, Value of Exports, Free on board (FOB), US Dollars", Attribute == "Value") %>%
        select(-c(`Country Code`, `Counterpart Country Code`, `Indicator Code`, Attribute, "...30")) %>%
        pivot_longer(cols = -c(country_name, counterpart, category), names_to = "year", values_to = "exports_to_counterpart") %>%
        mutate(country_name = case_when(country_name == "Armenia, Rep. of" ~ "Armenia",
                                        country_name == "Azerbaijan, Rep. of" ~ "Azerbaijan",
                                        country_name == "Belarus, Rep. of" ~ "Belarus",
                                        country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "Croatia, Rep. of" ~ "Croatia",
                                        country_name == "Czech Rep." ~ "Czechia",
                                        country_name == "Estonia, Rep. of" ~ "Estonia",
                                        country_name == "Kazakhstan, Rep. of" ~ "Kazakhstan",
                                        country_name == "Kosovo, Rep. of" ~ "Kosovo",
                                        country_name == "Kyrgyz Rep." ~ "Kyrgyzstan",
                                        country_name == "Moldova, Rep. of" ~ "Moldova",
                                        country_name == "Netherlands, The" ~ "Netherlands",
                                        country_name == "North Macedonia, Republic of" ~ "N. Macedonia",
                                        country_name == "Poland, Rep. of" ~ "Poland",
                                        country_name == "Russian Federation" ~ "Russia",
                                        country_name == "Serbia, Rep. of" ~ "Serbia",
                                        country_name == "Slovak Rep." ~ "Slovakia",
                                        country_name == "Slovenia, Rep. of" ~ "Slovenia",
                                        country_name == "Tajikistan, Rep. of" ~ "Tajikistan",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States" ~ "U.S.",
                                        country_name == "Uzbekistan, Rep. of" ~ "Uzbekistan",
                                        country_name == "China, P.R.: Mainland" ~ "China",
                                        TRUE ~ country_name),
               counterpart = case_when(counterpart == "Armenia, Rep. of" ~ "Armenia",
                                       counterpart == "Azerbaijan, Rep. of" ~ "Azerbaijan",
                                       counterpart == "Belarus, Rep. of" ~ "Belarus",
                                       counterpart == "Bosnia and Herzegovina" ~ "BiH",
                                       counterpart == "Croatia, Rep. of" ~ "Croatia",
                                       counterpart == "Czech Rep." ~ "Czechia",
                                       counterpart == "Estonia, Rep. of" ~ "Estonia",
                                       counterpart == "Kazakhstan, Rep. of" ~ "Kazakhstan",
                                       counterpart == "Kosovo, Rep. of" ~ "Kosovo",
                                       counterpart == "Kyrgyz Rep." ~ "Kyrgyzstan",
                                       counterpart == "Moldova, Rep. of" ~ "Moldova",
                                       counterpart == "Netherlands, The" ~ "Netherlands",
                                       counterpart == "North Macedonia, Republic of" ~ "N. Macedonia",
                                       counterpart == "Poland, Rep. of" ~ "Poland",
                                       counterpart == "Russian Federation" ~ "Russia",
                                       counterpart == "Serbia, Rep. of" ~ "Serbia",
                                       counterpart == "Slovak Rep." ~ "Slovakia",
                                       counterpart == "Slovenia, Rep. of" ~ "Slovenia",
                                       counterpart == "Tajikistan, Rep. of" ~ "Tajikistan",
                                       counterpart == "United Kingdom" ~ "U.K.",
                                       counterpart == "United States" ~ "U.S.",
                                       counterpart == "Uzbekistan, Rep. of" ~ "Uzbekistan",
                                       counterpart == "China, P.R.: Mainland" ~ "China",
                                       TRUE ~ counterpart),
               exports_to_counterpart = as.numeric(exports_to_counterpart),
               year = as.numeric(year)) %>%
        filter(!str_detect(string = counterpart, pattern = regex("emerging", ignore_case = TRUE)),
               !str_detect(string = counterpart, pattern = regex("not specified", ignore_case = TRUE)),
               !str_detect(string = counterpart, pattern = regex("middle east", ignore_case = TRUE)),
               !str_detect(string = counterpart, pattern = regex("hemisphere", ignore_case = TRUE)),
               !(counterpart %in% c("Advanced Economies",
                                    "Africa",
                                    "CIS",
                                    "Euro Area",
                                    "European Union",
                                    "Other Countries not included elsewhere",
                                    "Special Categories",
                                    "Sub-Saharan Africa")),
               !str_detect(string = country_name, pattern = regex("emerging", ignore_case = TRUE)),
               !str_detect(string = country_name, pattern = regex("not specified", ignore_case = TRUE)),
               !str_detect(string = country_name, pattern = regex("middle east", ignore_case = TRUE)),
               !str_detect(string = country_name, pattern = regex("hemisphere", ignore_case = TRUE)),
               !(country_name %in% c("Advanced Economies",
                                    "Africa",
                                    "CIS",
                                    "Euro Area",
                                    "European Union",
                                    "Other Countries not included elsewhere",
                                    "Special Categories",
                                    "Sub-Saharan Africa")))


#/////////////////////


# inspect
exports
exports %>% glimpse()
exports %>% nrow() # 827002
exports %>% ncol() # 5

# check
exports %>% count(category)
exports %>% count(year) %>% print(n = nrow(.))
exports %>% count(country_name) %>% print(n = nrow(.))
exports %>% count(counterpart) %>% print(n = nrow(.))


# check values

# inspect country names
exports %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>%
        distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>%
        anti_join(., exports, by = c("country" = "country_name")) %>% select(country)

exports %>%
        filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
        distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////////////////////////////////////////////////////////////////////////////////


# load gdp_current ####
# https://data.worldbank.org/indicator/NY.GDP.MKTP.CD

gdp_current <- read_excel(path = "data/world_bank/API_NY.GDP.MKTP.CD_DS2_en_excel_v2_4334130.xls", sheet = "Data", skip = 3) %>%
        rename(country_name = `Country Name`, iso3 = `Country Code`) %>%
        select(-c(`Indicator Name`, `Indicator Code`)) %>%
        pivot_longer(cols = -c(country_name, iso3), names_to = "year", values_to = "gdp_current") %>%
        mutate(year = as.numeric(year))


#/////////////////


# inspect
gdp_current
gdp_current %>% glimpse()
gdp_current %>% nrow() # 16492
gdp_current %>% ncol() # 4

gdp_current %>% count(year) %>% arrange(desc(year))
gdp_current %>% filter(year == 2021) %>% distinct(gdp_current)
gdp_current %>% filter(is.na(gdp_current), year >= 2010) %>% count(country_name) %>% arrange(desc(n)) %>% print(n = 20)


#/////////////////////////////////////////////////////////////////////////////////////////////


# get exports_to_all_counterparts

# add exports_to_world and gdp_current, then get exports_as_share_of_total_exports and exports_as_share_of_gdp
# note that goods_and_services_exports_to_world_as_share_of_gdp is a separate world bank series
# https://data.worldbank.org/indicator/NE.EXP.GNFS.ZS?locations=MK
# but the imf bilateral export data is only for goods
exports_to_all_counterparts <- country_crosswalk_expanded %>%
        filter(ee_region_flag == 1 | country == "U.S.") %>%
        left_join(., exports %>% 
                          select(country_name, year, counterpart, exports_to_counterpart),
                  by = c("country" = "country_name", "year")) %>%
        left_join(., exports %>% filter(counterpart == "World") %>%
                          select(country_name, year, exports_to_counterpart) %>% rename(exports_to_world = exports_to_counterpart),
                  by = c("country" = "country_name", "year")) %>%
        left_join(., gdp_current %>% select(-country_name), by = c("iso3", "year")) %>%
        mutate(exports_as_share_of_total_exports = exports_to_counterpart / exports_to_world,
               exports_as_share_of_gdp = exports_to_counterpart / gdp_current) %>%
        filter(counterpart != "World")


#///////////////////////////


# inspect
exports_to_all_counterparts
exports_to_all_counterparts %>% glimpse()
exports_to_all_counterparts %>% nrow() # 183540
exports_to_all_counterparts %>% ncol() # 20

# check country/year
exports_to_all_counterparts %>% distinct(country) %>% nrow() # 45
exports_to_all_counterparts %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))
exports_to_all_counterparts %>% count(year) %>% print(n = nrow(.))
exports_to_all_counterparts %>% count(country) %>% print(n = nrow(.))


# inspect
exports_to_all_counterparts %>%
        select(country, year, counterpart, exports_to_counterpart, exports_to_world, gdp_current,
               exports_as_share_of_total_exports, exports_as_share_of_gdp)
exports_to_all_counterparts %>%
        # filter(counterpart == "Russia") %>%
        filter(counterpart == "China") %>%
        select(country, year, counterpart, exports_to_counterpart, exports_to_world, gdp_current,
               exports_as_share_of_total_exports, exports_as_share_of_gdp)


#/////////////////


# check missing
exports_to_all_counterparts %>% 
        filter(year > 2000) %>%
        select(country, year, exports_to_world) %>%
        group_by(country, year) %>%
        slice(1) %>%
        ungroup() %>%
        group_by(country, year) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", variable, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )

exports_to_all_counterparts %>% 
        filter(year > 2000) %>%
        filter(counterpart == "Russia") %>%
        select(country, year, counterpart, exports_to_counterpart) %>%
        group_by(country, counterpart, year) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", variable, counterpart, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )


#/////////////////


# skim
exports_to_all_counterparts %>% 
        filter(year > 2010) %>%
        group_by(country) %>%
        skim(exports_as_share_of_total_exports, exports_as_share_of_gdp) %>%
        ungroup() %>%
        as_tibble() %>% select(-numeric.hist) %>%
        arrange(complete_rate) %>%
        print(n = nrow(.))

exports_to_all_counterparts %>% 
        filter(country == "Serbia") %>% 
        select(country, year, counterpart, exports_to_counterpart, exports_to_world, exports_as_share_of_total_exports, 
               exports_as_share_of_gdp) %>% 
        # skim(exports_as_share_of_gdp)
        mutate(q75 = quantile(x = exports_as_share_of_gdp, probs = .75, na.rm = TRUE)) %>%
        filter(exports_as_share_of_gdp >= q75) %>% 
        arrange(desc(exports_as_share_of_gdp)) %>%
        skim(exports_as_share_of_gdp)
        

#////////////////////


# plot
exports_to_all_counterparts %>% 
        # filter(country == "Serbia") %>%
        filter(country == "N. Macedonia") %>%
        select(country, year, counterpart, exports_to_counterpart, exports_to_world, exports_as_share_of_total_exports,
               exports_as_share_of_gdp) %>%
        filter(exports_as_share_of_gdp >= .01) %>%
        filter(counterpart != "World") %>%
        select(country, counterpart) %>%
        left_join(., exports_to_all_counterparts %>% 
                          # filter(country == "Serbia"),
                          filter(country == "N. Macedonia"), 
                  by = c("country", "counterpart")) %>%
        select(country, year, counterpart, exports_to_counterpart, exports_to_world, exports_as_share_of_total_exports,
               exports_as_share_of_gdp) %>%
        ggplot(data = ., mapping = aes(x = year, y = exports_as_share_of_gdp, color = counterpart)) + geom_line()
        

#/////////////////////////////////////////////////////////////////////////////////////////////


# test values

test_values_exports_to_all_counterparts <- function() {
        
        # 1
        expect_equal(object = exports_to_all_counterparts %>% filter(country == "Kosovo", year == 2007, counterpart == "Albania") %>%
                             select(country, year, counterpart, exports_to_counterpart, exports_to_world, 
                                    exports_as_share_of_total_exports, exports_as_share_of_gdp) %>%
                             pull(exports_as_share_of_total_exports),
                     expected = NA_real_)
        
        # 2
        expect_equal(object = exports_to_all_counterparts %>% filter(country == "Kosovo", year == 2008, counterpart == "Albania") %>%
                             select(country, year, counterpart, exports_to_counterpart, exports_to_world, 
                                    exports_as_share_of_total_exports, exports_as_share_of_gdp) %>%
                             pull(exports_as_share_of_total_exports),
                     expected = 27293410 / 332810877)
        
        # 3
        expect_equal(object = exports_to_all_counterparts %>% filter(country == "Kosovo", year == 2008, counterpart == "Albania") %>%
                             select(country, year, counterpart, exports_to_counterpart, exports_to_world, 
                                    exports_as_share_of_total_exports, exports_as_share_of_gdp) %>%
                             pull(exports_as_share_of_gdp),
                     expected = 27293410 / 5181776768.71247)
        
        # 4
        expect_equal(object = exports_to_all_counterparts %>% filter(country == "Belarus", year == 2018, counterpart == "Ukraine") %>%
                             select(country, year, counterpart, exports_to_counterpart, exports_to_world, 
                                    exports_as_share_of_total_exports, exports_as_share_of_gdp) %>%
                             pull(exports_as_share_of_total_exports),
                     expected = 4062193500 / 33906946200)
        
        # 5
        expect_equal(object = exports_to_all_counterparts %>% filter(country == "Belarus", year == 2018, counterpart == "Ukraine") %>%
                             select(country, year, counterpart, exports_to_counterpart, exports_to_world, 
                                    exports_as_share_of_total_exports, exports_as_share_of_gdp) %>%
                             pull(exports_as_share_of_gdp),
                     expected = 4062193500 / 60031262269.3365)
}

test_values_exports_to_all_counterparts()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# exports_to_all_counterparts %>% write_csv(file = "data/imf/dots/exports_to_all_counterparts_20220805.csv")
exports_to_all_counterparts <- read_csv(file = "data/imf/dots/exports_to_all_counterparts_20220805.csv")

# inspect
exports_to_all_counterparts
exports_to_all_counterparts %>% glimpse()
exports_to_all_counterparts %>% nrow() # 183540
exports_to_all_counterparts %>% ncol() # 20


#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# load imports ####

# data is available up to 2020 from IMF server
# https://data.imf.org/?sk=9d6028d4-f14a-464c-a2f2-59b2cd424b85&sId=1409151240976

# use free on board for exports, and CIF for imports
# note the IMF BPM6 recommendation is to use free on board valuation for exports and exports, but the UN IMTS 2010 recommendaiton
# is to use CIF for imports
# in the DOTS data, only fob is available for exports, but fob for imports only has like 12 countries with records, all the rest use cif
# https://unstats.un.org/unsd/trade/eg-imts/IMTS%202010%20(English).pdf
# https://www.imf.org/external/pubs/ft/bop/2014/pdf/guide.pdf
# https://ourworldindata.org/trade-data-sources-discrepancies


imports <- read_csv(file = "data/imf/dots/DOT_08-05-2022 16-10-55-19_timeSeries.csv") %>%
        rename(country_name = `Country Name`,
               counterpart = `Counterpart Country Name`,
               category = `Indicator Name`) %>%
        filter(category == "Goods, Value of Imports, Cost, Insurance, Freight (CIF), US Dollars", Attribute == "Value") %>%
        select(-c(`Country Code`, `Counterpart Country Code`, `Indicator Code`, Attribute, "...30")) %>%
        pivot_longer(cols = -c(country_name, counterpart, category), names_to = "year", values_to = "imports_from_counterpart") %>%
        mutate(country_name = case_when(country_name == "Armenia, Rep. of" ~ "Armenia",
                                        country_name == "Azerbaijan, Rep. of" ~ "Azerbaijan",
                                        country_name == "Belarus, Rep. of" ~ "Belarus",
                                        country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "Croatia, Rep. of" ~ "Croatia",
                                        country_name == "Czech Rep." ~ "Czechia",
                                        country_name == "Estonia, Rep. of" ~ "Estonia",
                                        country_name == "Kazakhstan, Rep. of" ~ "Kazakhstan",
                                        country_name == "Kosovo, Rep. of" ~ "Kosovo",
                                        country_name == "Kyrgyz Rep." ~ "Kyrgyzstan",
                                        country_name == "Moldova, Rep. of" ~ "Moldova",
                                        country_name == "Netherlands, The" ~ "Netherlands",
                                        country_name == "North Macedonia, Republic of" ~ "N. Macedonia",
                                        country_name == "Poland, Rep. of" ~ "Poland",
                                        country_name == "Russian Federation" ~ "Russia",
                                        country_name == "Serbia, Rep. of" ~ "Serbia",
                                        country_name == "Slovak Rep." ~ "Slovakia",
                                        country_name == "Slovenia, Rep. of" ~ "Slovenia",
                                        country_name == "Tajikistan, Rep. of" ~ "Tajikistan",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States" ~ "U.S.",
                                        country_name == "Uzbekistan, Rep. of" ~ "Uzbekistan",
                                        country_name == "China, P.R.: Mainland" ~ "China",
                                        TRUE ~ country_name),
               counterpart = case_when(counterpart == "Armenia, Rep. of" ~ "Armenia",
                                       counterpart == "Azerbaijan, Rep. of" ~ "Azerbaijan",
                                       counterpart == "Belarus, Rep. of" ~ "Belarus",
                                       counterpart == "Bosnia and Herzegovina" ~ "BiH",
                                       counterpart == "Croatia, Rep. of" ~ "Croatia",
                                       counterpart == "Czech Rep." ~ "Czechia",
                                       counterpart == "Estonia, Rep. of" ~ "Estonia",
                                       counterpart == "Kazakhstan, Rep. of" ~ "Kazakhstan",
                                       counterpart == "Kosovo, Rep. of" ~ "Kosovo",
                                       counterpart == "Kyrgyz Rep." ~ "Kyrgyzstan",
                                       counterpart == "Moldova, Rep. of" ~ "Moldova",
                                       counterpart == "Netherlands, The" ~ "Netherlands",
                                       counterpart == "North Macedonia, Republic of" ~ "N. Macedonia",
                                       counterpart == "Poland, Rep. of" ~ "Poland",
                                       counterpart == "Russian Federation" ~ "Russia",
                                       counterpart == "Serbia, Rep. of" ~ "Serbia",
                                       counterpart == "Slovak Rep." ~ "Slovakia",
                                       counterpart == "Slovenia, Rep. of" ~ "Slovenia",
                                       counterpart == "Tajikistan, Rep. of" ~ "Tajikistan",
                                       counterpart == "United Kingdom" ~ "U.K.",
                                       counterpart == "United States" ~ "U.S.",
                                       counterpart == "Uzbekistan, Rep. of" ~ "Uzbekistan",
                                       counterpart == "China, P.R.: Mainland" ~ "China",
                                       TRUE ~ counterpart),
               imports_from_counterpart = as.numeric(imports_from_counterpart),
               year = as.numeric(year)) %>%
        filter(!str_detect(string = counterpart, pattern = regex("emerging", ignore_case = TRUE)),
               !str_detect(string = counterpart, pattern = regex("not specified", ignore_case = TRUE)),
               !str_detect(string = counterpart, pattern = regex("middle east", ignore_case = TRUE)),
               !str_detect(string = counterpart, pattern = regex("hemisphere", ignore_case = TRUE)),
               !(counterpart %in% c("Advanced Economies",
                                    "Africa",
                                    "CIS",
                                    "Euro Area",
                                    "European Union",
                                    "Other Countries not included elsewhere",
                                    "Special Categories",
                                    "Sub-Saharan Africa")),
               !str_detect(string = country_name, pattern = regex("emerging", ignore_case = TRUE)),
               !str_detect(string = country_name, pattern = regex("not specified", ignore_case = TRUE)),
               !str_detect(string = country_name, pattern = regex("middle east", ignore_case = TRUE)),
               !str_detect(string = country_name, pattern = regex("hemisphere", ignore_case = TRUE)),
               !(country_name %in% c("Advanced Economies",
                                     "Africa",
                                     "CIS",
                                     "Euro Area",
                                     "European Union",
                                     "Other Countries not included elsewhere",
                                     "Special Categories",
                                     "Sub-Saharan Africa")))


#/////////////////////


# inspect
imports
imports %>% glimpse()
imports %>% nrow() # 862356
imports %>% ncol() # 5

# check
imports %>% count(category)
imports %>% count(year) %>% print(n = nrow(.))
imports %>% count(country_name) %>% print(n = nrow(.))
imports %>% count(counterpart) %>% print(n = nrow(.))


# check values

# inspect country names
imports %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>%
        distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>%
        anti_join(., imports, by = c("country" = "country_name")) %>% select(country)

imports %>%
        filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
        distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////////////////////////////////////////////////////////////////////////////////


# load gdp_current ####
# https://data.worldbank.org/indicator/NY.GDP.MKTP.CD

gdp_current <- read_excel(path = "data/world_bank/API_NY.GDP.MKTP.CD_DS2_en_excel_v2_4334130.xls", sheet = "Data", skip = 3) %>%
        rename(country_name = `Country Name`, iso3 = `Country Code`) %>%
        select(-c(`Indicator Name`, `Indicator Code`)) %>%
        pivot_longer(cols = -c(country_name, iso3), names_to = "year", values_to = "gdp_current") %>%
        mutate(year = as.numeric(year))


#/////////////////


# inspect
gdp_current
gdp_current %>% glimpse()
gdp_current %>% nrow() # 16492
gdp_current %>% ncol() # 4

gdp_current %>% count(year) %>% arrange(desc(year))
gdp_current %>% filter(year == 2021) %>% distinct(gdp_current)
gdp_current %>% filter(is.na(gdp_current), year >= 2010) %>% count(country_name) %>% arrange(desc(n)) %>% print(n = 20)


#/////////////////////////////////////////////////////////////////////////////////////////////


# get imports_from_all_counterparts

# add imports_from_world and gdp_current, then get imports_as_share_of_total_imports and imports_as_share_of_gdp
# note that goods_and_services_imports_from_world_as_share_of_gdp is a separate world bank series
# https://data.worldbank.org/indicator/NE.EXP.GNFS.ZS?locations=MK
# but the imf bilateral export data is only for goods
imports_from_all_counterparts <- country_crosswalk_expanded %>%
        filter(ee_region_flag == 1 | country == "U.S.") %>%
        left_join(., imports %>% 
                          select(country_name, year, counterpart, imports_from_counterpart),
                  by = c("country" = "country_name", "year")) %>%
        left_join(., imports %>% filter(counterpart == "World") %>%
                          select(country_name, year, imports_from_counterpart) %>% rename(imports_from_world = imports_from_counterpart),
                  by = c("country" = "country_name", "year")) %>%
        left_join(., gdp_current %>% select(-country_name), by = c("iso3", "year")) %>%
        mutate(imports_as_share_of_total_imports = imports_from_counterpart / imports_from_world,
               imports_as_share_of_gdp = imports_from_counterpart / gdp_current) %>%
        filter(counterpart != "World")


#///////////////////////////


# inspect
imports_from_all_counterparts
imports_from_all_counterparts %>% glimpse()
imports_from_all_counterparts %>% nrow() # 191478
imports_from_all_counterparts %>% ncol() # 20

# check country/year
imports_from_all_counterparts %>% distinct(country) %>% nrow() # 45
imports_from_all_counterparts %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))
imports_from_all_counterparts %>% count(year) %>% print(n = nrow(.))
imports_from_all_counterparts %>% count(country) %>% print(n = nrow(.))


# inspect
imports_from_all_counterparts %>%
        select(country, year, counterpart, imports_from_counterpart, imports_from_world, gdp_current,
               imports_as_share_of_total_imports, imports_as_share_of_gdp)
imports_from_all_counterparts %>%
        # filter(counterpart == "Russia") %>%
        filter(counterpart == "China") %>%
        select(country, year, counterpart, imports_from_counterpart, imports_from_world, gdp_current,
               imports_as_share_of_total_imports, imports_as_share_of_gdp)


#/////////////////


# check missing

# imports_from_world
imports_from_all_counterparts %>% 
        filter(year > 2000) %>%
        select(country, year, imports_from_world) %>%
        group_by(country, year) %>%
        slice(1) %>%
        ungroup() %>%
        group_by(country, year) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", variable, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )

# imports from specific country
imports_from_all_counterparts %>% 
        filter(year > 2000) %>%
        filter(counterpart == "Russia") %>%
        select(country, year, counterpart, imports_from_counterpart) %>%
        group_by(country, counterpart, year) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", variable, counterpart, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )


#/////////////////


# skim
imports_from_all_counterparts %>% 
        filter(year > 2010) %>%
        group_by(country) %>%
        skim(imports_as_share_of_total_imports, imports_as_share_of_gdp) %>%
        ungroup() %>%
        as_tibble() %>% select(-numeric.hist) %>%
        arrange(complete_rate) %>%
        print(n = nrow(.))

imports_from_all_counterparts %>% 
        filter(country == "Serbia") %>% 
        select(country, year, counterpart, imports_from_counterpart, imports_from_world, imports_as_share_of_total_imports, 
               imports_as_share_of_gdp) %>% 
        # skim(imports_as_share_of_gdp)
        mutate(q75 = quantile(x = imports_as_share_of_gdp, probs = .75, na.rm = TRUE)) %>%
        filter(imports_as_share_of_gdp >= q75) %>% 
        arrange(desc(imports_as_share_of_gdp)) %>%
        skim(imports_as_share_of_gdp)


#////////////////////


# plot
imports_from_all_counterparts %>% 
        # filter(country == "Serbia") %>%
        # filter(country == "N. Macedonia") %>%
        filter(country == "Azerbaijan") %>%
        select(country, year, counterpart, imports_from_counterpart, imports_from_world, imports_as_share_of_total_imports,
               imports_as_share_of_gdp) %>%
        filter(imports_as_share_of_gdp >= .01) %>%
        filter(counterpart != "World") %>%
        distinct(country, counterpart) %>%
        left_join(., imports_from_all_counterparts %>% 
                          # filter(country == "Serbia"),
                          # filter(country == "N. Macedonia"), 
                          filter(country == "Azerbaijan"), 
                  by = c("country", "counterpart")) %>%
        select(country, year, counterpart, imports_from_counterpart, imports_from_world, imports_as_share_of_total_imports,
               imports_as_share_of_gdp) %>%
        ggplot(data = ., mapping = aes(x = year, y = imports_as_share_of_gdp, color = counterpart)) + geom_line()


#/////////////////////////////////////////////////////////////////////////////////////////////


# test values

test_values_imports_from_all_counterparts <- function() {
        
        # 1
        expect_equal(object = imports_from_all_counterparts %>% filter(country == "Kosovo", year == 2007, counterpart == "N. Macedonia") %>%
                             select(country, year, counterpart, imports_from_counterpart, imports_from_world, 
                                    imports_as_share_of_total_imports, imports_as_share_of_gdp) %>%
                             pull(imports_as_share_of_total_imports),
                     expected = NA_real_)
        
        # 2
        expect_equal(object = imports_from_all_counterparts %>% filter(country == "Kosovo", year == 2008, counterpart == "N. Macedonia") %>%
                             select(country, year, counterpart, imports_from_counterpart, imports_from_world, 
                                    imports_as_share_of_total_imports, imports_as_share_of_gdp) %>%
                             pull(imports_as_share_of_total_imports),
                     expected = 105731003 / 2254853713)
        
        # 3
        expect_equal(object = imports_from_all_counterparts %>% filter(country == "Kosovo", year == 2008, counterpart == "N. Macedonia") %>%
                             select(country, year, counterpart, imports_from_counterpart, imports_from_world, 
                                    imports_as_share_of_total_imports, imports_as_share_of_gdp) %>%
                             pull(imports_as_share_of_gdp),
                     expected = 105731003 / 5181776768.71247)
        
        # 4
        expect_equal(object = imports_from_all_counterparts %>% filter(country == "Belarus", year == 2018, counterpart == "Ukraine") %>%
                             select(country, year, counterpart, imports_from_counterpart, imports_from_world, 
                                    imports_as_share_of_total_imports, imports_as_share_of_gdp) %>%
                             pull(imports_as_share_of_total_imports),
                     expected = 1402922100 / 38441071300)
        
        # 5
        expect_equal(object = imports_from_all_counterparts %>% filter(country == "Belarus", year == 2018, counterpart == "Ukraine") %>%
                             select(country, year, counterpart, imports_from_counterpart, imports_from_world, 
                                    imports_as_share_of_total_imports, imports_as_share_of_gdp) %>%
                             pull(imports_as_share_of_gdp),
                     expected = 1402922100 / 60031262269.3365)
}

test_values_imports_from_all_counterparts()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# imports_from_all_counterparts %>% write_csv(file = "data/imf/dots/imports_from_all_counterparts_20220805.csv")
imports_from_all_counterparts <- read_csv(file = "data/imf/dots/imports_from_all_counterparts_20220805.csv")

# inspect
imports_from_all_counterparts
imports_from_all_counterparts %>% glimpse()
imports_from_all_counterparts %>% nrow() # 191478
imports_from_all_counterparts %>% ncol() # 20


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# ext_debt_owed_to_russia_and_china ####

# https://www.worldbank.org/en/programs/debt-statistics/ids
# https://datahelpdesk.worldbank.org/knowledgebase/topics/19287-external-debt

# detailed definition of "external debt" here: https://data.worldbank.org/indicator/DT.DOD.DECT.CD
# Total external debt is debt owed to nonresidents repayable in currency, goods, or services. 
# Total external debt is the sum of public, publicly guaranteed, and private nonguaranteed long-term debt, 
# use of IMF credit, and short-term debt. Short-term debt includes all debt having an original maturity of one year 
# or less and interest in arrears on long-term debt. Data are in current U.S. dollars.

# from debt statistics faq: https://pubdocs.worldbank.org/en/342211633111359690/Debt-Statistics-FAQ.pdf
# "DRS classifies official creditors as bilateral agencies and multilateral institutions. Bilateral creditors
# comprise these agencies that make loans on behalf of the government:
# . Development aid agencies e.g. European Bank for Reconstruction and Development
# . Official Export credit agencies e.g. Export Import Banks
# . Central banks and state monetary authorities
# . Other official bilateral agencies

# DRS classifies a creditor entity as private based on legal status the entity. The categories of these
# creditors are:
#         . Exporter: supplier of goods, such as manufacturer or trading company who extends credits
# for the purchase of the goods directly
# . Private bank: an institution engaged in commercial banking activities, whether the
# ownership of the bank is public or private
# . Bond holder: a security with a promise to pay a specified amount of money at a fixed date
# and income at periodic dates until maturity. It includes publicly placed bonds and privately
# placed bonds
# . Other financial institutions: any private entity other than exporters, private banks or
# bondholders

# remember to manually delete the "Data from database: International Debt Statistics" and "Last Updated: mm/dd/yyyy" records on
# the downloaded file from databank

# note that stock of ext_debt_to_russia_as_share_of_total_ext_debt is better indicator for index than flow,
# because flow could have negative values, which would get a very low score when standardized, and
# would lead to more jumpy shifts, whereas stocks are more stable over time, which is what we want for the index

# note that as of 20220814 download, there were no values for 2021-2022
ext_debt_owed_to_russia_and_china <- read_excel(path = "data/world_bank/Data_Extract_From_International_Debt_Statistics_ext_debt_owed_to_russia_and_china.xlsx", 
                                                                           na = "..") %>%
        rename(country_name = `Country Name`, 
               counterpart = `Counterpart-Area Name`,
               variable = `Series Name`) %>%
        select(-c(`Country Code`, `Counterpart-Area Code`, `Series Code`)) %>%
        pivot_longer(cols = -c(country_name, counterpart, variable), names_to = "year", values_to = "values") %>%
        mutate(year = as.numeric(str_sub(string = year, start = 1, end = 4)),
               high_value_is_good_outcome_flag = 0,
               indicator_name = "external_debt_to_",
               country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        country_name == "Russian Federation" ~ "Russia",
                                        country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                        TRUE ~ country_name),
               counterpart = case_when(counterpart == "Russian Federation" ~ "Russia",
                                       TRUE ~ counterpart)) %>%
        filter(year <= 2020,
               variable == "External debt stocks, total (DOD, current US$)",
               counterpart %in% c("Russia", "China", "World"),
               !(country_name %in% c("Data from database: International Debt Statistics")),
               !str_detect(string = country_name, pattern = regex("Last Updated: ", ignore_case = TRUE))) %>%
        arrange(country_name, year) %>%
        group_by(country_name, year) %>%
        mutate(ext_debt_to_world = case_when(counterpart == "World" ~ values,
                                             TRUE ~ NA_real_)) %>%
        fill(ext_debt_to_world, .direction = "updown") %>%
        ungroup() %>%
        rename(ext_debt_owed_to_counterpart = values) %>%
        mutate(ext_debt_owed_to_counterpart_as_share_of_ext_debt_to_world = ext_debt_owed_to_counterpart / ext_debt_to_world) %>%
        filter(counterpart != "World")
        

#/////////////////////


# inspect
ext_debt_owed_to_russia_and_china
ext_debt_owed_to_russia_and_china %>% glimpse()
ext_debt_owed_to_russia_and_china %>% nrow() # 5712
ext_debt_owed_to_russia_and_china %>% ncol() # 9

# check
ext_debt_owed_to_russia_and_china %>% count(variable)
ext_debt_owed_to_russia_and_china %>% count(year) %>% print(n = nrow(.))
ext_debt_owed_to_russia_and_china %>% count(country_name) %>% print(n = nrow(.))
ext_debt_owed_to_russia_and_china %>% count(counterpart) %>% print(n = nrow(.))

# check values

# inspect country names
# note that many EU-15 and graduate countries don't have external_debt record
ext_debt_owed_to_russia_and_china %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>%
        distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>%
        anti_join(., ext_debt_owed_to_russia_and_china, by = c("country" = "country_name")) %>% select(country, mcp_grouping) %>% print(n = nrow(.))

ext_debt_owed_to_russia_and_china %>%
        filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
        distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////////////////////////////////////////////////////////////////////////////////


# load gdp_current ####
# https://data.worldbank.org/indicator/NY.GDP.MKTP.CD

gdp_current <- read_excel(path = "data/world_bank/API_NY.GDP.MKTP.CD_DS2_en_excel_v2_4334130.xls", sheet = "Data", skip = 3) %>%
        rename(country_name = `Country Name`, iso3 = `Country Code`) %>%
        select(-c(`Indicator Name`, `Indicator Code`)) %>%
        pivot_longer(cols = -c(country_name, iso3), names_to = "year", values_to = "gdp_current") %>%
        mutate(year = as.numeric(year))


#/////////////////


# inspect
gdp_current
gdp_current %>% glimpse()
gdp_current %>% nrow() # 16492
gdp_current %>% ncol() # 4

gdp_current %>% count(year) %>% arrange(desc(year))
gdp_current %>% filter(year == 2021) %>% distinct(gdp_current)
gdp_current %>% filter(is.na(gdp_current), year >= 2010) %>% count(country_name) %>% arrange(desc(n)) %>% print(n = 20)


#/////////////////////////////////////////////////////////////////////////////////////////////


ext_debt_owed_to_russia_and_china <- ext_debt_owed_to_russia_and_china %>% select(country_name, year, counterpart, ext_debt_owed_to_counterpart, ext_debt_to_world,
                                 ext_debt_owed_to_counterpart_as_share_of_ext_debt_to_world) %>%
        filter(country_name %in% (country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% pull(country))) %>%
        left_join(., country_crosswalk, by = c("country_name" = "country")) %>%
        left_join(., gdp_current %>% select(-country_name), by = c("iso3", "year")) %>%
        mutate(ext_debt_owed_to_counterpart_as_share_of_gdp = ext_debt_owed_to_counterpart / gdp_current) %>%
        rename(country = country_name) %>%
        relocate(counterpart:ext_debt_owed_to_counterpart_as_share_of_ext_debt_to_world, .after = ee_region_flag) 


#//////////////////////


# inspect
ext_debt_owed_to_russia_and_china
ext_debt_owed_to_russia_and_china %>% glimpse()
ext_debt_owed_to_russia_and_china %>% nrow() # 840
ext_debt_owed_to_russia_and_china %>% ncol() # 20

# check
ext_debt_owed_to_russia_and_china %>% count(year) %>% print(n = nrow(.))
ext_debt_owed_to_russia_and_china %>% count(country) %>% print(n = nrow(.))
ext_debt_owed_to_russia_and_china %>% count(counterpart) %>% print(n = nrow(.))

# note that bih and kosovo have all NA records
ext_debt_owed_to_russia_and_china %>%
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        group_by(country) %>% skim(ext_debt_owed_to_counterpart)

ext_debt_owed_to_russia_and_china %>% 
        filter(country == "Kosovo") %>%
        select(country, year, ext_debt_owed_to_counterpart, 
               ext_debt_owed_to_counterpart_as_share_of_ext_debt_to_world, 
               ext_debt_owed_to_counterpart_as_share_of_gdp) %>%
        skim()


# inspect missing for e&E

# check most recent year of data availability
# note that bih and kosovo have all NA records
ext_debt_owed_to_russia_and_china %>% 
        pivot_longer(cols = c(ext_debt_owed_to_counterpart, 
                              ext_debt_owed_to_counterpart_as_share_of_ext_debt_to_world, 
                              ext_debt_owed_to_counterpart_as_share_of_gdp), names_to = "var", values_to = "values") %>%
        unite(col = var, var, counterpart, remove = FALSE) %>%
        select(country, mcp_grouping, year, counterpart, var, values) %>%
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        group_by(country, var) %>%
        arrange(desc(year)) %>%
        mutate(row_number = row_number(),
               most_recent_non_na_year_flag = case_when(row_number == 1 & !is.na(values) ~ 1,
                                                        TRUE ~ 0)) %>%
        ungroup() %>%
        filter(most_recent_non_na_year_flag == 1) %>%
        distinct(country, year) %>%
        left_join(country_crosswalk %>% filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>% select(country), .,
                  by = "country") %>%
        arrange(year)


# plot missing data
ext_debt_owed_to_russia_and_china %>% 
        pivot_longer(cols = c(ext_debt_owed_to_counterpart, 
                              ext_debt_owed_to_counterpart_as_share_of_ext_debt_to_world, 
                              ext_debt_owed_to_counterpart_as_share_of_gdp), names_to = "var", values_to = "values") %>%
        unite(col = var, var, counterpart, remove = FALSE) %>%
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        select(country, year, var, values) %>%
        group_by(country, year, var) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", var, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )

# plot
ext_debt_owed_to_russia_and_china %>%
        # filter(mcp_grouping == "E&E Balkans") %>%
        # filter(mcp_grouping == "E&E Eurasia") %>%
        filter(year >= 2010) %>%
        pivot_longer(cols = c(ext_debt_owed_to_counterpart, 
                              ext_debt_owed_to_counterpart_as_share_of_ext_debt_to_world, 
                              ext_debt_owed_to_counterpart_as_share_of_gdp), names_to = "var", values_to = "values") %>%
        unite(col = var, var, counterpart, remove = FALSE) %>%
        select(country, year, var, values) %>%
        # filter(var == "ext_debt_owed_to_counterpart_as_share_of_gdp_Russia") %>%
        filter(var == "ext_debt_owed_to_counterpart_as_share_of_gdp_China") %>%
        ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line()


#/////////////////////////////////////////////////////////////////////////////////////////////


# test_values

test_values_ext_debt_owed_to_russia_and_china <- function() {
        
        # 1
        expect_equal(object = ext_debt_owed_to_russia_and_china %>% 
                             filter(country == "Armenia", year == 2015, counterpart == "Russia") %>%
                             pull(ext_debt_owed_to_counterpart_as_share_of_gdp),
                     expected = 7766000 / 10553337672.9872)
        
        # 2
        expect_equal(object = ext_debt_owed_to_russia_and_china %>% 
                             filter(country == "Moldova", year == 2020, counterpart == "Russia") %>%
                             pull(ext_debt_owed_to_counterpart_as_share_of_gdp),
                     expected = 25409000 / 11859730543.5525)
        
        # 3
        expect_equal(object = ext_debt_owed_to_russia_and_china %>% 
                             filter(country == "N. Macedonia", year == 2017, counterpart == "China") %>%
                             pull(ext_debt_owed_to_counterpart_as_share_of_gdp),
                     expected = 411414000 / 11307058382.3435)
        
        # 4
        expect_equal(object = ext_debt_owed_to_russia_and_china %>% 
                             filter(country == "Kazakhstan", year == 2019, counterpart == "China") %>%
                             pull(ext_debt_owed_to_counterpart_as_share_of_gdp),
                     expected = 1180559000 / 181667190075.541)
}

test_values_ext_debt_owed_to_russia_and_china()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# ext_debt_owed_to_russia_and_china %>% write_csv(file = "data/world_bank/ext_debt_owed_to_russia_and_china_20220814.csv")
ext_debt_owed_to_russia_and_china <- read_csv(file = "data/world_bank/ext_debt_owed_to_russia_and_china_20220814.csv")

# inspect
ext_debt_owed_to_russia_and_china
ext_debt_owed_to_russia_and_china %>% glimpse()
ext_debt_owed_to_russia_and_china %>% nrow() # 840
ext_debt_owed_to_russia_and_china %>% ncol() # 20


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# ext_debt_owed_from_russia_and_china ####

# https://www.worldbank.org/en/programs/debt-statistics/ids
# https://datahelpdesk.worldbank.org/knowledgebase/topics/19287-external-debt

# detailed definition of "external debt" here: https://data.worldbank.org/indicator/DT.DOD.DECT.CD
# Total external debt is debt owed to nonresidents repayable in currency, goods, or services. 
# Total external debt is the sum of public, publicly guaranteed, and private nonguaranteed long-term debt, 
# use of IMF credit, and short-term debt. Short-term debt includes all debt having an original maturity of one year 
# or less and interest in arrears on long-term debt. Data are in current U.S. dollars.

# from debt statistics faq: https://pubdocs.worldbank.org/en/342211633111359690/Debt-Statistics-FAQ.pdf
# "DRS classifies official creditors as bilateral agencies and multilateral institutions. Bilateral creditors
# comprise these agencies that make loans on behalf of the government:
# . Development aid agencies e.g. European Bank for Reconstruction and Development
# . Official Export credit agencies e.g. Export Import Banks
# . Central banks and state monetary authorities
# . Other official bilateral agencies

# DRS classifies a creditor entity as private based on legal status the entity. The categories of these
# creditors are:
#         . Exporter: supplier of goods, such as manufacturer or trading company who extends credits
# for the purchase of the goods directly
# . Private bank: an institution engaged in commercial banking activities, whether the
# ownership of the bank is public or private
# . Bond holder: a security with a promise to pay a specified amount of money at a fixed date
# and income at periodic dates until maturity. It includes publicly placed bonds and privately
# placed bonds
# . Other financial institutions: any private entity other than exporters, private banks or
# bondholders

# remember to manually delete the "Data from database: International Debt Statistics" and "Last Updated: mm/dd/yyyy" records on
# the downloaded file from databank

# note that stock of ext_debt_to_russia_as_share_of_total_ext_debt is better indicator for index than flow,
# because flow could have negative values, which would get a very low score when standardized, and
# would lead to more jumpy shifts, whereas stocks are more stable over time, which is what we want for the index

# note that as of 20220814 download, there were no values for 2021-2022
ext_debt_owed_from_russia_and_china <- read_excel(path = "data/world_bank/Data_Extract_From_International_Debt_Statistics_ext_debt_owed_from_russia_and_china.xlsx", 
                                       na = "..") %>%
        rename(country_name = `Country Name`, 
               counterpart = `Counterpart-Area Name`,
               variable = `Series Name`) %>%
        select(-c(`Country Code`, `Counterpart-Area Code`, `Series Code`)) %>%
        pivot_longer(cols = -c(country_name, counterpart, variable), names_to = "year", values_to = "values") %>%
        mutate(year = as.numeric(str_sub(string = year, start = 1, end = 4)),
               high_value_is_good_outcome_flag = 0,
               indicator_name = "external_debt_from_",
               country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        country_name == "Russian Federation" ~ "Russia",
                                        country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                        country_name == "Bosnia-Herzegovina" ~ "BiH",
                                        country_name == "Czech Republic" ~ "Czechia",
                                        country_name == "Slovak Republic" ~ "Slovakia",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States" ~ "U.S.",
                                        TRUE ~ country_name),
               counterpart = case_when(counterpart == "Bosnia and Herzegovina" ~ "BiH",
                                       counterpart == "North Macedonia" ~ "N. Macedonia",
                                       counterpart == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                        counterpart == "Russian Federation" ~ "Russia",
                                       counterpart == "Bosnia-Herzegovina" ~ "BiH",
                                       counterpart == "Czech Republic" ~ "Czechia",
                                       counterpart == "Slovak Republic" ~ "Slovakia",
                                       counterpart == "United Kingdom" ~ "U.K.",
                                       counterpart == "United States" ~ "U.S.",
                                       TRUE ~ counterpart)) %>%
        filter(year <= 2020,
               variable == "External debt stocks, total (DOD, current US$)",
               counterpart %in% c(country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% pull(country)),
               !(country_name %in% c("Data from database: International Debt Statistics")),
               !str_detect(string = country_name, pattern = regex("Last Updated: ", ignore_case = TRUE))) %>%
        arrange(country_name, year) %>%
        # group_by(country_name, year) %>%
        # mutate(ext_debt_to_world = case_when(counterpart == "World" ~ values,
        #                                      TRUE ~ NA_real_)) %>%
        # fill(ext_debt_to_world, .direction = "updown") %>%
        # ungroup() %>%
        rename(ext_debt_owed_from_counterpart = values)
        # mutate(ext_debt_from_counterpart_as_share_of_ext_debt_to_world = ext_debt_owed_from_counterpart / ext_debt_to_world) %>%
        # filter(counterpart != "World")


#/////////////////////


# inspect
ext_debt_owed_from_russia_and_china
ext_debt_owed_from_russia_and_china %>% glimpse()
ext_debt_owed_from_russia_and_china %>% nrow() # 1848
ext_debt_owed_from_russia_and_china %>% ncol() # 7

# check
ext_debt_owed_from_russia_and_china %>% count(variable)
ext_debt_owed_from_russia_and_china %>% count(year) %>% print(n = nrow(.))
ext_debt_owed_from_russia_and_china %>% count(country_name) %>% print(n = nrow(.))
ext_debt_owed_from_russia_and_china %>% count(counterpart) %>% print(n = nrow(.))

# check values

# inspect country names
# note that many EU-15 and graduate countries don't have external_debt record
ext_debt_owed_from_russia_and_china %>% anti_join(., country_crosswalk, by = c("counterpart" = "country")) %>%
        distinct(counterpart) %>% arrange(counterpart) %>% print(n = nrow(.))
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>%
        anti_join(., ext_debt_owed_from_russia_and_china, by = c("country" = "counterpart")) %>% 
        select(country, mcp_grouping) %>% print(n = nrow(.))

ext_debt_owed_from_russia_and_china %>%
        filter(str_detect(string = counterpart, pattern = regex("united", ignore_case = TRUE))) %>%
        distinct(counterpart)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////////////////////////////////////////////////////////////////////////////////


# load gdp_current ####
# https://data.worldbank.org/indicator/NY.GDP.MKTP.CD

gdp_current <- read_excel(path = "data/world_bank/API_NY.GDP.MKTP.CD_DS2_en_excel_v2_4334130.xls", sheet = "Data", skip = 3) %>%
        rename(country_name = `Country Name`, iso3 = `Country Code`) %>%
        select(-c(`Indicator Name`, `Indicator Code`)) %>%
        pivot_longer(cols = -c(country_name, iso3), names_to = "year", values_to = "gdp_current") %>%
        mutate(year = as.numeric(year))


#/////////////////


# inspect
gdp_current
gdp_current %>% glimpse()
gdp_current %>% nrow() # 16492
gdp_current %>% ncol() # 4

gdp_current %>% count(year) %>% arrange(desc(year))
gdp_current %>% filter(year == 2021) %>% distinct(gdp_current)
gdp_current %>% filter(is.na(gdp_current), year >= 2010) %>% count(country_name) %>% arrange(desc(n)) %>% print(n = 20)


#/////////////////////////////////////////////////////////////////////////////////////////////


ext_debt_owed_from_russia_and_china <- ext_debt_owed_from_russia_and_china %>% 
        select(country_name, year, counterpart, ext_debt_owed_from_counterpart) %>%
        left_join(., country_crosswalk, by = c("counterpart" = "country")) %>%
        left_join(., gdp_current %>% select(-country_name), by = c("iso3", "year")) %>%
        mutate(ext_debt_owed_from_counterpart_as_share_of_gdp = ext_debt_owed_from_counterpart / gdp_current) %>%
        rename(country = counterpart,
               counterpart = country_name) %>%
        relocate(country, .before = everything()) %>%
        relocate(counterpart, .after = ee_region_flag) %>%
        relocate(ext_debt_owed_from_counterpart, .after = counterpart) 


#//////////////////////


# inspect
ext_debt_owed_from_russia_and_china
ext_debt_owed_from_russia_and_china %>% glimpse()
ext_debt_owed_from_russia_and_china %>% nrow() # 1848
ext_debt_owed_from_russia_and_china %>% ncol() # 18

# check
ext_debt_owed_from_russia_and_china %>% count(year) %>% print(n = nrow(.))
ext_debt_owed_from_russia_and_china %>% count(country) %>% print(n = nrow(.))
ext_debt_owed_from_russia_and_china %>% count(counterpart) %>% print(n = nrow(.))

# note that only macedonia and serbia have records (debt_owed_from_russia), but it's all less than 0.6% of gdp
ext_debt_owed_from_russia_and_china %>%
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        select(counterpart, country, year, ext_debt_owed_from_counterpart, 
               ext_debt_owed_from_counterpart_as_share_of_gdp) %>%
        # arrange(desc(ext_debt_owed_from_counterpart)) %>%
        arrange(desc(ext_debt_owed_from_counterpart_as_share_of_gdp)) %>%
        group_by(counterpart, country) %>% skim(ext_debt_owed_from_counterpart)

ext_debt_owed_from_russia_and_china %>% 
        filter(country == "Kosovo") %>%
        select(country, year, ext_debt_owed_from_counterpart, 
               ext_debt_owed_from_counterpart_as_share_of_gdp) %>%
        skim()


# inspect missing for e&E

# check most recent year of data availability
# for E&E presence countries, only serbia and macedonia have non-NA values for russia (none have records for china)
# records are also old (serbis is 2013 and macedonia is 2016)
ext_debt_owed_from_russia_and_china %>% 
        pivot_longer(cols = c(ext_debt_owed_from_counterpart, 
                              ext_debt_owed_from_counterpart_as_share_of_gdp), names_to = "var", values_to = "values") %>%
        # unite(col = var, var, counterpart, remove = FALSE) %>%
        select(country, mcp_grouping, year, counterpart, var, values) %>%
        unite(col = var, var, counterpart) %>%
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        group_by(country, var) %>%
        arrange(desc(year)) %>%
        mutate(non_na_year = case_when(!is.na(values) ~ year)) %>%
        filter(non_na_year == max(non_na_year, na.rm = TRUE)) %>%
        ungroup() %>%
        arrange(country, desc(non_na_year)) %>%
        distinct(country, year) %>%
        left_join(country_crosswalk %>% filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>% select(country), .,
                  by = "country") %>%
        arrange(year)


# plot missing data
ext_debt_owed_from_russia_and_china %>% 
        pivot_longer(cols = c(ext_debt_owed_from_counterpart, 
                              ext_debt_owed_from_counterpart_as_share_of_gdp), names_to = "var", values_to = "values") %>%
        select(country, mcp_grouping, year, counterpart, var, values) %>%
        unite(col = var, var, counterpart) %>%
        # filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        select(country, year, var, values) %>%
        group_by(country, year, var) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", var, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )

# plot
ext_debt_owed_from_russia_and_china %>%
        # filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        # filter(mcp_grouping == "E&E Balkans") %>%
        # filter(mcp_grouping == "E&E Eurasia") %>%
        filter(year >= 2010) %>%
        pivot_longer(cols = c(ext_debt_owed_from_counterpart, 
                              ext_debt_owed_from_counterpart_as_share_of_gdp), names_to = "var", values_to = "values") %>%
        select(country, mcp_grouping, year, counterpart, var, values) %>%
        unite(col = var, var, counterpart, remove = FALSE) %>%
        select(country, year, var, values) %>%
        filter(var == "ext_debt_owed_from_counterpart_as_share_of_gdp_Russia") %>%
        # filter(var == "ext_debt_owed_from_counterpart_as_share_of_gdp_China") %>%
        filter(!is.na(values)) %>%
        ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line()


#/////////////////////////////////////////////////////////////////////////////////////////////


# test_values

test_values_ext_debt_owed_from_russia_and_china <- function() {
        
        # 1
        expect_equal(object = ext_debt_owed_from_russia_and_china %>% 
                             filter(counterpart == "Russia", year == 2013, country == "Armenia") %>%
                             pull(ext_debt_owed_from_counterpart_as_share_of_gdp),
                     expected = NA_real_)
        
        # 2
        expect_equal(object = ext_debt_owed_from_russia_and_china %>% 
                             filter(counterpart == "Russia", year == 2012, country == "Serbia") %>%
                             pull(ext_debt_owed_from_counterpart_as_share_of_gdp),
                     expected = 13850000 / 43309252921.0567)
        
        # 3
        expect_equal(object = ext_debt_owed_from_russia_and_china %>% 
                             filter(counterpart == "Russia", year == 2015, country == "N. Macedonia") %>%
                             pull(ext_debt_owed_from_counterpart_as_share_of_gdp),
                     expected = 18300000 / 10064515432.0265)
        
        # 4
        expect_equal(object = ext_debt_owed_from_russia_and_china %>% 
                             filter(counterpart == "China", year == 2019, country == "Tajikistan") %>%
                             pull(ext_debt_owed_from_counterpart_as_share_of_gdp),
                     expected = NA_real_)
}

test_values_ext_debt_owed_from_russia_and_china()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# ext_debt_owed_from_russia_and_china %>% write_csv(file = "data/world_bank/ext_debt_owed_from_russia_and_china_20220814.csv")
ext_debt_owed_from_russia_and_china <- read_csv(file = "data/world_bank/ext_debt_owed_from_russia_and_china_20220814.csv")

# inspect
ext_debt_owed_from_russia_and_china
ext_debt_owed_from_russia_and_china %>% glimpse()
ext_debt_owed_from_russia_and_china %>% nrow() # 1848
ext_debt_owed_from_russia_and_china %>% ncol() # 18


#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////


# load weo data ####

# https://www.imf.org/en/Publications/SPROLLs/world-economic-outlook-databases#sort=%40imfdate%20descending
# https://www.imf.org/en/Publications/WEO

weo <- read_excel(path = "data/imf/weo/WEOOct2022all.xlsx", sheet = "WEOOct2022all", na = "n/a") %>% 
        # read_excel(path = "data/imf/weo/WEOOct2021all.xlsx", sheet = "WEOOct2021all", na = "n/a") %>% 
        rename(country_name = Country, 
               indicator_name = `WEO Subject Code`,
               indicator_desc = `Subject Descriptor`,
               indicator_notes = `Subject Notes`,
               units = Units,
               scale = Scale,
               estimates_start_after = `Estimates Start After`) %>% 
        mutate(across(.cols = `1980`:`2026`, .fns = ~ as.numeric(.x)),
               country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        country_name == "Czech Republic" ~ "Czechia",
                                        country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                        country_name == "Slovak Republic" ~ "Slovakia",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States" ~ "U.S.",
                                        TRUE ~ country_name)) %>% 
        pivot_longer(cols = `1980`:`2026`, names_to = "year", values_to = "values") %>%
        mutate(year = as.numeric(year),
               values_w_est = values,
               values = case_when(year > estimates_start_after ~ NA_real_,
                                  TRUE ~ values)) %>%
        select(country_name, indicator_name, indicator_desc, indicator_notes, units, scale, 
               estimates_start_after, year, values, values_w_est)


#////////////////////


# inspect
weo
weo %>% glimpse()
weo %>% nrow() # 405422
weo %>% ncol() # 10

weo %>% count(country_name) %>% print(n = nrow(.)) # 197

# check names
# note that only E&E presence countries names are cleaned up for joining
weo %>% 
        mutate(country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        TRUE ~ country_name)) %>%
        distinct(country_name) %>% 
        anti_join(., country_crosswalk,  
                  by = c("country_name" = "country")) %>% print(n = nrow(.))

weo %>% 
        mutate(country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        TRUE ~ country_name)) %>%
        distinct(country_name) %>% 
        anti_join(country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S."), .,
                  by = c("country" = "country_name"))


weo %>% filter(str_detect(string = country_name, pattern = "Macedo")) %>% distinct(country_name) 


#//////////////////////////////////////////////////////////////////////////////////////////////////


# join weo to country_crosswalk for ee_presence countries
weo <- weo %>% left_join(., country_crosswalk, by = c("country_name" = "country")) %>%
        rename(country = country_name) %>%
         filter(ee_region_flag == 1 | country == "U.S.")


#////////////////////


# inspect
weo
weo %>% glimpse()
weo %>% nrow() # 93060
weo %>% ncol() # 22

weo %>% count(country) %>% print(n = nrow(.)) # 45
weo %>% distinct(country, mcp_grouping) %>% count(mcp_grouping) 

weo %>% count(year) %>% arrange(desc(year))
weo %>% count(indicator_name)
weo %>% count(indicator_name, indicator_desc, indicator_notes, units, scale) %>% print(n = nrow(.))

# inspect indicators
weo %>% filter(indicator_name %in% c("PPPGDP", "NGDPD", "NGDPDPC", "PPPPC", "LUR", "GGXWDG_NGDP", "PCPIPCH", "GGR_NGDP", "GGXCNL_NGDP")) %>% 
        count(indicator_name, indicator_desc, 
              # indicator_notes, 
              units, scale)
weo %>% filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(indicator_name %in% c("PPPGDP", "NGDPD", "NGDPDPC", "PPPPC", "LUR", "GGXWDG_NGDP", "PCPIPCH", "GGR_NGDP", "GGXCNL_NGDP")) %>%
        count(estimates_start_after)


#//////////////////////////////////////////////////////////////////////////////////////////////////


# analyze trends

# plot values
weo %>% 
        # filter(country %in% c("Kosovo", "Serbia", "BiH")) %>%
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(year >= 2010, year <= 2022) %>%
        # filter(values < 100) %>%
        filter(indicator_name == "PPPGDP") %>% # gdp at ppp
        # filter(indicator_name == "NGDPD") %>% # gdp at current USD
        # filter(indicator_name == "LUR") %>% # unemployment rate
        # filter(indicator_name == "GGXWDG_NGDP") %>% # general gov gross debt as % gdp
        # filter(indicator_name == "PCPIPCH") %>% # annual inflation pct change
        # filter(indicator_name == "GGR_NGDP") %>% # general gov revenue as % gdp
        # filter(indicator_name == "GGXCNL_NGDP") %>% general gov net lending/borrowing as % gdp
        select(country, mcp_grouping, year, indicator_name, indicator_desc,  
               units, scale, estimates_start_after, values, values_w_est) %>%
        # count(estimates_start_after)
        ggplot(data = ., mapping = aes(x = year, y = values, 
                                       color = fct_reorder2(country, .x = year, .y = values, .desc = TRUE))) + geom_line(size = 1) +
        # scale_x_continuous(breaks = seq(from = 2010, to = 2025, by = 1)) +
        scale_x_continuous(breaks = seq(from = 2010, to = 2022, by = 1)) +
        labs(color = NULL)

# plot values_w_est
# note that ukraine does not have estimates
weo %>% 
        # filter(country %in% c("Kosovo", "Serbia", "BiH")) %>%
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(year >= 2010, year <= 2023) %>%
        # filter(values < 100) %>%
        filter(indicator_name == "PPPGDP") %>% # gdp at ppp
        # filter(indicator_name == "NGDPD") %>% # gdp at current USD
        # filter(indicator_name == "LUR") %>% # unemployment rate
        # filter(indicator_name == "GGXWDG_NGDP") %>% # general gov gross debt as % gdp
        # filter(indicator_name == "PCPIPCH") %>% # annual inflation pct change
        # filter(indicator_name == "GGR_NGDP") %>% # general gov revenue as % gdp
        # filter(indicator_name == "GGXCNL_NGDP") %>% general gov net lending/borrowing as % gdp
        select(country, mcp_grouping, year, indicator_name, indicator_desc, 
               units, scale, estimates_start_after, values, values_w_est) %>%
        # count(estimates_start_after)
        ggplot(data = ., mapping = aes(x = year, y = values_w_est, 
                                       color = fct_reorder2(country, .x = year, .y = values_w_est, .desc = TRUE))) + geom_line(size = 1) +
        # scale_x_continuous(breaks = seq(from = 2010, to = 2025, by = 1)) +
        scale_x_continuous(breaks = seq(from = 2010, to = 2023, by = 1)) +
        labs(color = NULL)


#//////////////////////


# inspect values_w_est
weo %>% filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        # filter(indicator_name == "PPPGDP") %>% # gdp at ppp
        # filter(indicator_name == "NGDPD") %>% # gdp at current USD
        filter(indicator_name == "LUR") %>% # unemployment rate
        # filter(indicator_name == "GGXWDG_NGDP") %>% # general gov gross debt as % gdp
        # filter(indicator_name == "PCPIPCH") %>% # annual inflation pct change
        # filter(indicator_name == "GGR_NGDP") %>% # general gov revenue as % gdp
        # filter(indicator_name == "GGXCNL_NGDP") %>% general gov net lending/borrowing as % gdp
        # filter(year %in% c(2020, 2022)) %>%
        filter(year %in% c(2019, 2022)) %>%
        select(country, mcp_grouping, year, indicator_name, indicator_desc, indicator_notes, units, scale, 
               estimates_start_after,  values, values_w_est) %>%
        arrange(country, year) %>%
        group_by(country) %>%
        mutate(diff = values_w_est - lag(values_w_est, n = 1)) %>%
        ungroup() %>%
        # filter(diff > 0) %>%
        # filter(year == 2022) %>%
        print(n = nrow(.))


#//////////////////


# pppgdp
weo %>% filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(indicator_name == "PPPGDP") %>% # gdp at ppp
        # filter(year %in% c(2021, 2022)) %>%
        filter(year >= 2010, year <= 2024) %>%
        select(country, mcp_grouping, year, indicator_name, indicator_desc, indicator_notes, units, scale, 
               estimates_start_after, values, values_w_est) %>%
        arrange(country, year) %>%
        group_by(country) %>%
        mutate(gdp_growth = (values_w_est - lag(values_w_est, n = 1)) / lag(values_w_est, n = 1)) %>%
        ungroup() %>%
        
        distinct(country, estimates_start_after) 

        
        # filter(year == 2022) %>%
        # arrange(desc(values_w_est)) %>%
        # mutate(avg_gdp = mean(values_w_est, na.rm = TRUE),
        #        avg_gdp_growth = mean(gdp_growth, na.rm = TRUE)) %>%
        
        ggplot(data = ., mapping = aes(x = year, y = gdp_growth, color = country)) + geom_line(size = 1) +
        scale_x_continuous(breaks = seq(from = 2010, to = 2024, by = 1)) 
        
        # print(n = nrow(.))
        

#//////////////////


# ppppc
weo %>% filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(indicator_name == "PPPPC") %>% # gdp per capita at ppp
        # filter(year %in% c(2021, 2022)) %>%
        filter(year >= 2010, year <= 2024) %>%
        select(country, mcp_grouping, year, indicator_name, indicator_desc, indicator_notes, units, scale, 
               estimates_start_after, values, values_w_est) %>%

        distinct(country, estimates_start_after) 
        
        # filter(year == 2022) %>%
        # arrange(desc(values_w_est)) %>%
        # mutate(avg_gdp = mean(values_w_est, na.rm = TRUE),
        #        avg_gdp_growth = mean(gdp_growth, na.rm = TRUE)) %>%
        
        ggplot(data = ., mapping = aes(x = year, y = values_w_est, color = country)) + geom_line(size = 1) +
        scale_x_continuous(breaks = seq(from = 2010, to = 2024, by = 1)) 

# print(n = nrow(.))
        


#//////////////////


# unemployment
weo %>% filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(indicator_name == "LUR") %>% # unemployment rate
        # filter(year %in% c(2019, 2022)) %>%
        filter(year >= 2010, year <= 2024) %>%
        select(country, mcp_grouping, year, indicator_name, indicator_desc, indicator_notes, 
               units, scale, values, values_w_est, estimates_start_after) %>%
        arrange(country, year) %>%
        group_by(country) %>%
        mutate(diff = values_w_est - lag(values_w_est, n = 1)) %>%
        ungroup() %>%
        
        distinct(country, estimates_start_after)

        
        # filter(year == 2022) %>%
        # arrange(desc(values_w_est)) %>%
        # mutate(avg_unemployment = mean(values_w_est, na.rm = TRUE)) %>%
        
        ggplot(data = ., mapping = aes(x = year, y = diff, color = country)) + geom_line(size = 1) +
        scale_x_continuous(breaks = seq(from = 2010, to = 2024, by = 1)) 
        
        print(n = nrow(.))


#//////////////////


# inflation
weo %>% filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(indicator_name == "PCPIPCH") %>% # annual inflation pct change
        filter(year %in% c(2019, 2022)) %>%
        select(country, mcp_grouping, year, indicator_name, indicator_desc, indicator_notes, 
               units, scale, values, values_w_est, estimates_start_after) %>%
        
        distinct(country, estimates_start_after) 

        
        arrange(country, year) %>%
        group_by(country) %>%
        mutate(diff = values_w_est - lag(values_w_est, n = 1)) %>%
        ungroup() %>%
        filter(year == 2022) %>%
        arrange(desc(values_w_est)) %>%
        
        mutate(avg_inflation = mean(values_w_est, na.rm = TRUE),
               avg_inflation_change = mean(diff, na.rm = TRUE)) %>%
        
        print(n = nrow(.))


#//////////////////


# general gov debt as % of gdp
weo %>% filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(indicator_name == "GGXWDG_NGDP") %>% # general gov gross debt as % gdp
        filter(year %in% c(2019, 2022)) %>%
        select(country, mcp_grouping, year, indicator_name, indicator_desc, indicator_notes, 
               units, scale, values, values_w_est, estimates_start_after) %>%
        
        distinct(country, estimates_start_after) 

        
        arrange(country, year) %>%
        group_by(country) %>%
        mutate(diff = values_w_est - lag(values_w_est, n = 1)) %>%
        ungroup() %>%
        
        filter(year == 2022) %>%
        arrange(desc(values_w_est)) %>%
        mutate(avg_gov_debt = mean(values_w_est, na.rm = TRUE),
               avg_gov_debt_change = mean(diff, na.rm = TRUE)) %>%

        print(n = nrow(.))


#///////////////////////////////////////////////////////////////////////////////////////////////////////


# test values
test_weo_apr_2022 <- function() {
        
        # albania unemployment in 2022
        expect_equal(object = weo %>% filter(country == "Albania", year == 2022, indicator_name == "LUR") %>% pull(values_w_est),
                     expected = 10.3)
        
        # armenia inflation in 2022
        expect_equal(object = weo %>% filter(country == "Armenia", year == 2022, indicator_name == "PCPIPCH") %>% pull(values_w_est),
                     expected = 7.609)
        
        # georgia gov debt in 2019
        expect_equal(object = weo %>% filter(country == "Georgia", year == 2019, indicator_name == "GGXWDG_NGDP") %>% pull(values_w_est),
                     expected = 40.434)
}
test_weo_apr_2022()


#////////////////////


test_weo_oct_2021 <- function() {
        
        # albania unemployment in 2021
        expect_equal(object = weo %>% filter(country == "Albania", year == 2021, indicator_name == "LUR") %>% pull(values_w_est),
                     expected = 12)
        
        # armenia inflation in 2022
        expect_equal(object = weo %>% filter(country == "Armenia", year == 2022, indicator_name == "PCPIPCH") %>% pull(values_w_est),
                     expected = 5.778)
        
        # georgia gov debt in 2019
        expect_equal(object = weo %>% filter(country == "Georgia", year == 2019, indicator_name == "GGXWDG_NGDP") %>% pull(values_w_est),
                     expected = 40.436)
        
        # belarus gdp per capita PPP in 2018
        expect_equal(object = weo %>% filter(country == "Belarus", year == 2018, indicator_name == "PPPPC") %>% pull(values_w_est),
                     expected = 19406.91)
}
test_weo_oct_2021()


#////////////////////


test_weo_oct_2022 <- function() {
        
        # albania unemployment in 2021
        expect_equal(object = weo %>% filter(country == "Albania", year == 2021, indicator_name == "LUR") %>% pull(values_w_est),
                     expected = 10.6)
        
        # armenia inflation in 2022
        expect_equal(object = weo %>% filter(country == "Armenia", year == 2022, indicator_name == "PCPIPCH") %>% pull(values_w_est),
                     expected = 8.54)
        
        # georgia gov debt in 2019
        expect_equal(object = weo %>% filter(country == "Georgia", year == 2019, indicator_name == "GGXWDG_NGDP") %>% pull(values_w_est),
                     expected = 40.434)
        
        # belarus gdp per capita PPP in 2018
        expect_equal(object = weo %>% filter(country == "Belarus", year == 2018, indicator_name == "PPPPC") %>% pull(values_w_est),
                     expected = 19406.91)
}
test_weo_oct_2022()


#///////////////////////////////////////////////////////////////////////////////////////////////////////


# read/write weo data ####

# weo %>% write.xlsx(file = "data/imf/weo/weo_oct_2021_cleaned_20221013.xlsx")
# weo %>% write.xlsx(file = "data/imf/weo/weo_oct_2022_cleaned_20221013.xlsx")
weo_oct_2021 <- read_excel(path = "data/imf/weo/weo_oct_2021_cleaned_20221013.xlsx")
weo_oct_2022 <- read_excel(path = "data/imf/weo/weo_oct_2022_cleaned_20221013.xlsx")

# inspect
weo
weo %>% glimpse()
weo %>% nrow() # 93060
weo %>% ncol() # 22


#///////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////


# economic_reform_index ####
read_excel(path = "data/economic_reform_index/MCP Economic Reform Index Data--20220324.xlsx") %>%
        rename(country = Country,
               eri = ERI,
               ee_filter = EEfilter,
               ee_area = EEArea) %>%
        select(country, year, ee_filter, ee_area, eri) %>% 
        # filter(ee_area == "Balkans") %>% 
        filter(ee_area == "Eurasia") %>%
        # count(country)
        ggplot(data = ., mapping = aes(x = year, y = eri, color = country)) + geom_line(size = 1)


#///////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////



# russian_remittances ####

# go to russian central bank statistics site :https://www.cbr.ru/eng/statistics/
# then search for "remittances", and then option for "Cross-border Remittances via Payment Systems in Breakdown by Countries" with
# individual sub-links for each year
# https://www.cbr.ru/eng/statistics/?CF.Search=remittances&CF.TagId=&CF.Date.Time=Any&CF.Date.DateFrom=&CF.Date.DateTo=

# note the original raw data has a weird space character that does not get recognized as a traditional space
# eg for 2011 spreadsheet, it lists "United States", but a regex str_detect or other case_when will not match as it appears
# but it will match "United.States" regex, because once the non-space character is wildcarded over
# or the dollar amount will have a space instead of a comma, but also have a decimal point, so i clean as string dropping 
# non-numbers and non-decimal, then convert back into numeric

# note that in 2009, georgia has two different entries on raw spreadsheet
# in the code below, i keep the record that fits closest with the subsequent year's values, and drop the extra 2009 record

# current_path <- "data/russian_remittances/36e-rem_09.xlsx"

load_russian_remittances_data <- function(current_path) {
        
        # get current_year
        current_year <- str_sub(string = current_path, start = -7, end = -1) %>%
                str_replace(string = ., pattern = "\\.xlsx", replacement = "") %>%
                str_c("20", .)
        
        print(current_year)
        
        # get current_data
        current_data <- read_excel(path = current_path, skip = 4, sheet = current_year)
        
        # clean data when current_year <= 2012
        if(current_year <= 2012) {
                
                current_data <- current_data %>% 
                               rename(country = 1,
                                      remittance_to_counterpart = 2,
                                      remittance_to_counterpart_avg = 3,
                                      remittance_from_counterpart = 4,
                                      remittance_from_counterpart_avg = 5) %>%
                               select(country, remittance_to_counterpart, remittance_to_counterpart_avg,
                                      remittance_from_counterpart, remittance_from_counterpart_avg) %>%
                               filter(!(str_detect(string = country, pattern = regex("Total|Non-CIS countries|of which|CIS countries", 
                                                                                     ignore_case = TRUE))),
                                      !(str_detect(string = remittance_to_counterpart, pattern = regex("total amount", ignore_case = TRUE)))) %>%
                               mutate(remittance_to_counterpart = as.numeric(str_replace(string = remittance_to_counterpart, pattern = "[^0-9|\\.]", 
                                                                                    replacement = "")) * 1000000,
                                      remittance_to_counterpart_avg = as.numeric(str_replace(string = remittance_to_counterpart_avg, pattern = "[^0-9|\\.]", 
                                                                                        replacement = "")),
                                      remittance_from_counterpart = as.numeric(str_replace(string = remittance_from_counterpart, pattern = "[^0-9|\\.]", 
                                                                                      replacement = "")) * 1000000,
                                      remittance_from_counterpart_avg = as.numeric(str_replace(string = remittance_from_counterpart_avg, pattern = "[^0-9|\\.]", 
                                                                                          replacement = "")),
                                      year = as.numeric(current_year),
                                      unit = "Dollars (it's assumed to be current dollars)",
                                      counterpart = "Russia",
                                      country = str_to_title(string = country),
                                      country = case_when(str_detect(string = country, pattern = regex("United.States")) ~ "U.S.",
                                                          country == "United States Of America" ~ "U.S.",
                                                          str_detect(string = country, pattern = regex("United.Kingdom")) ~ "U.K.",
                                                          country == "Czech Republic" ~ "Czechia",
                                                          country == "Armenia, Republic Of" ~ "Armenia",
                                                          country == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                                          country == "Azerbaijan, Republic Of" ~ "Azerbaijan",
                                                          country == "Moldova, Republic Of" ~ "Moldova",
                                                          country == "Serbia, Republic Of" ~ "Serbia",
                                                          country == "Poland, Republic Of" ~ "Poland",
                                                          str_detect(string = country, pattern = regex("China")) ~ "China",
                                                          TRUE ~ country)) %>%
                               relocate(year, .after = country) %>%
                        filter(!(country == "Georgia" & year == 2009 & remittance_to_counterpart == 12000000))
                       
                       return(current_data)
        }
        
        # clean data when current_year >= 2013 (extra column A has been added to format)
        if(current_year >= 2013) {
                
                current_data <- current_data %>% 
                        select(-1) %>%
                        rename(country = 1,
                               remittance_to_counterpart = 2,
                               remittance_to_counterpart_avg = 3,
                               remittance_from_counterpart = 4,
                               remittance_from_counterpart_avg = 5) %>%
                                select(country, remittance_to_counterpart, remittance_to_counterpart_avg,
                                       remittance_from_counterpart, remittance_from_counterpart_avg) %>%
                                filter(!(str_detect(string = country, pattern = regex("Total|Non-CIS countries|of which|CIS countries", 
                                                                                      ignore_case = TRUE))),
                                       !(str_detect(string = remittance_to_counterpart, pattern = regex("total amount", ignore_case = TRUE)))) %>%
                                mutate(remittance_to_counterpart = as.numeric(str_replace(string = remittance_to_counterpart, pattern = "[^0-9|\\.]", 
                                                                                     replacement = "")) * 1000000,
                                       remittance_to_counterpart_avg = as.numeric(str_replace(string = remittance_to_counterpart_avg, pattern = "[^0-9|\\.]", 
                                                                                         replacement = "")),
                                       remittance_from_counterpart = as.numeric(str_replace(string = remittance_from_counterpart, pattern = "[^0-9|\\.]", 
                                                                                       replacement = "")) * 1000000,
                                       remittance_from_counterpart_avg = as.numeric(str_replace(string = remittance_from_counterpart_avg, pattern = "[^0-9|\\.]", 
                                                                                           replacement = "")),
                                       year = as.numeric(current_year),
                                       unit = "Dollars (it's assumed to be current dollars)",
                                       counterpart = "Russia",
                                       country = str_to_title(string = country),
                                       country = case_when(str_detect(string = country, pattern = regex("United.States")) ~ "U.S.",
                                                           country == "United States Of America" ~ "U.S.",
                                                           str_detect(string = country, pattern = regex("United.Kingdom")) ~ "U.K.",
                                                           country == "Czech Republic" ~ "Czechia",
                                                           country == "Armenia, Republic Of" ~ "Armenia",
                                                           country == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                                           country == "Azerbaijan, Republic Of" ~ "Azerbaijan",
                                                           country == "Moldova, Republic Of" ~ "Moldova",
                                                           country == "Serbia, Republic Of" ~ "Serbia",
                                                           country == "Poland, Republic Of" ~ "Poland",
                                                           str_detect(string = country, pattern = regex("China")) ~ "China",
                                                           TRUE ~ country)) %>%
                                relocate(year, .after = country)
                
                return(current_data)
        }
}

# get russian_remittances
# note skip 2006 since the spreadsheet format is different (lacks a tabbed named "2006", and also it's not needed)
russian_remittances <- map(.x = dir_ls("data/russian_remittances") %>% tibble(path = .) %>%
                                   filter(str_detect(string = path, pattern = regex("[0-9]{2}.xlsx"))) %>% 
                                   filter(path != "data/russian_remittances/36e-rem_06.xlsx") %>%
                                   pull(path), 
                           .f = ~ load_russian_remittances_data(current_path = .x)) %>% bind_rows()
        

#///////////////////////


# inspect
russian_remittances
russian_remittances %>% glimpse()
russian_remittances %>% nrow() # 452
russian_remittances %>% ncol() # 8

russian_remittances %>% count(country) %>% print(n = nrow(.))
russian_remittances %>% count(year) # 2007-2021
russian_remittances %>% count(counterpart)

# check country names 
russian_remittances %>% anti_join(., country_crosswalk, by = "country") %>% count(country) %>% print(n = nrow(.))
russian_remittances %>% filter(str_detect(string = country, pattern = regex("serbia", ignore_case = TRUE)))


#/////////////////////////////////////////////////////////////////////////////////////////////


# load gdp_current ####
# https://data.worldbank.org/indicator/NY.GDP.MKTP.CD

gdp_current <- read_excel(path = "data/world_bank/API_NY.GDP.MKTP.CD_DS2_en_excel_v2_4334130.xls", sheet = "Data", skip = 3) %>%
        rename(country_name = `Country Name`, iso3 = `Country Code`) %>%
        select(-c(`Indicator Name`, `Indicator Code`)) %>%
        pivot_longer(cols = -c(country_name, iso3), names_to = "year", values_to = "gdp_current") %>%
        mutate(year = as.numeric(year))


#/////////////////


# inspect
gdp_current
gdp_current %>% glimpse()
gdp_current %>% nrow() # 16492
gdp_current %>% ncol() # 4

gdp_current %>% count(year) %>% arrange(desc(year))
gdp_current %>% filter(year == 2021) %>% distinct(gdp_current)
gdp_current %>% filter(is.na(gdp_current), year >= 2010) %>% count(country_name) %>% arrange(desc(n)) %>% print(n = 20)


#///////////////////////////////////////////////////////////////////////////////////////////////////////


# add country_crosswalk, gdp_current, and remittances_to/from_russia_as_share_of_gdp

russian_remittances <- russian_remittances %>% 
        left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., by = c("country", "year")) %>%
        left_join(., gdp_current %>% select(-country_name), by = c("iso3", "year")) %>%
        mutate(remittance_from_counterpart_as_share_of_gdp = remittance_from_counterpart / gdp_current,
               remittance_to_counterpart_as_share_of_gdp = remittance_to_counterpart / gdp_current) %>%
        relocate(remittance_to_counterpart:remittance_from_counterpart_avg, .after = ee_region_flag) %>%
        filter(year >= 2007)
        

#///////////////////////


# inspect
russian_remittances
russian_remittances %>% glimpse()
russian_remittances %>% nrow() # 675
russian_remittances %>% ncol() # 23

russian_remittances %>% count(country) %>% print(n = nrow(.))
russian_remittances %>% distinct(mcp_grouping, country) %>% count(mcp_grouping) 
russian_remittances %>% count(year)
russian_remittances %>%
        filter(country == "Serbia") %>% 
        select(country, mcp_grouping, year, remittance_to_counterpart, remittance_from_counterpart, gdp_current,
               remittance_to_counterpart_as_share_of_gdp, remittance_from_counterpart_as_share_of_gdp)

russian_remittances %>% select(starts_with("remittance")) %>% skim() %>% as_tibble() %>%
        select(-c(skim_type, numeric.hist))
russian_remittances %>% select(country, mcp_grouping, year, remittance_to_counterpart, remittance_from_counterpart, gdp_current,
                               remittance_to_counterpart_as_share_of_gdp, remittance_from_counterpart_as_share_of_gdp) %>%
        arrange(desc(remittance_from_counterpart_as_share_of_gdp))

# check missing
# note missing 4 balkans, 7 graduates, 1 CAR, and 7 eu-15 countries 
# (albania, bih, kosovo, macedonia; also serbia only has 2008)
# 3 graduates have only partial data
russian_remittances %>% filter(is.na(remittance_from_counterpart_as_share_of_gdp)) %>%
        count(country, mcp_grouping) %>% arrange(mcp_grouping) %>% print(n = nrow(.))
russian_remittances %>% filter(is.na(remittance_from_counterpart_as_share_of_gdp)) %>%
        count(country, year) %>% print(n = nrow(.))
russian_remittances %>% filter(is.na(remittance_from_counterpart_as_share_of_gdp)) %>%
        count(country, mcp_grouping) %>%
        filter(n < 15) %>%
        left_join(., russian_remittances %>% filter(!is.na(remittance_from_counterpart_as_share_of_gdp)) %>%
                          count(country, year),
                  by = "country") %>% print(n = nrow(.))
russian_remittances %>% filter(!is.na(remittance_from_counterpart_as_share_of_gdp)) %>%
        count(country, year) %>% print(n = nrow(.))

# check most recent year of data availability
russian_remittances %>% 
        pivot_longer(cols = c(remittance_from_counterpart_as_share_of_gdp, 
                              remittance_to_counterpart_as_share_of_gdp), names_to = "var", values_to = "values") %>%
        select(country, mcp_grouping, year, var, values) %>%
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        group_by(country, var) %>%
        arrange(desc(year)) %>%
        mutate(row_number = row_number(),
               most_recent_non_na_year_flag = case_when(row_number == 1 & !is.na(values) ~ 1,
                                                        TRUE ~ 0)) %>%
        ungroup() %>%
        filter(most_recent_non_na_year_flag == 1) %>%
        distinct(country, year) %>%
        left_join(country_crosswalk %>% filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>% select(country), .,
                  by = "country") %>%
        arrange(year)


# plot missing data
russian_remittances %>% 
        pivot_longer(cols = c(remittance_from_counterpart_as_share_of_gdp, 
                              remittance_to_counterpart_as_share_of_gdp), names_to = "var", values_to = "values") %>%
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        select(country, mcp_grouping, year, var, values) %>%
        group_by(country, mcp_grouping, year, var) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", var, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )

# plot
russian_remittances %>%
        # filter(mcp_grouping == "E&E Balkans") %>%
        # filter(mcp_grouping == "E&E Eurasia") %>%
        # filter(mcp_grouping == "E&E graduates") %>%
        filter(mcp_grouping == "EU-15") %>%
        
        # filter(year >= 2007) %>%
        pivot_longer(cols = c(remittance_from_counterpart_as_share_of_gdp, 
                              remittance_to_counterpart_as_share_of_gdp), names_to = "var", values_to = "values") %>%
        select(country, year, var, values) %>%
        filter(var == "remittance_from_counterpart_as_share_of_gdp") %>%
        # filter(var == "remittance_to_counterpart_as_share_of_gdp") %>%
        filter(!is.na(values)) %>%
        arrange(desc(values)) %>%
        ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1)


#///////////////////////////////////////////////////////////////////////////////////////////////////////


# test values
test_values_russian_remittances <- function() {
        
        # 1
        expect_equal(object = russian_remittances %>% filter(country == "Tajikistan", year == 2021) %>%
                             select(country, year, remittance_from_counterpart, gdp_current, remittance_from_counterpart_as_share_of_gdp) %>%
                             pull(remittance_from_counterpart_as_share_of_gdp),
                     expected = (1794.8 * 1000000) / 8746270636.40142)
        
        # 2
        expect_equal(object = russian_remittances %>% filter(country == "Tajikistan", year == 2008) %>%
                             select(country, year, remittance_from_counterpart, gdp_current, remittance_from_counterpart_as_share_of_gdp) %>%
                             pull(remittance_from_counterpart_as_share_of_gdp),
                     expected = (2516 * 1000000) / 5161337336.40365)
        
        # 3
        expect_equal(object = russian_remittances %>% filter(country == "Ukraine", year == 2015) %>%
                             select(country, year, remittance_from_counterpart, gdp_current, remittance_from_counterpart_as_share_of_gdp) %>%
                             pull(remittance_from_counterpart_as_share_of_gdp),
                     expected = (988 * 1000000) / 91030959454.6961)
}

test_values_russian_remittances()


#///////////////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# russian_remittances %>% write_csv(file = "data/russian_remittances/russian_remittances_20220814.csv")
russian_remittances <- read_csv(file = "data/russian_remittances/russian_remittances_20220814.csv")

# inspect
russian_remittances
russian_remittances %>% glimpse()
russian_remittances %>% nrow() # 676
russian_remittances %>% ncol() # 23


#///////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////


# compile econ_links to china and russia ####

econ_links <- country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S.") %>% mutate(counterpart = "Russia") %>%
        bind_rows(., country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S.") %>% mutate(counterpart = "China")) %>%
        left_join(., inward_fdi_from_all_counterparts %>% select(country, year, counterpart, inward_fdi_as_share_of_gdp),
                  by = c("country", "year", "counterpart")) %>%
        left_join(., outward_fdi_to_all_counterparts %>% select(country, year, counterpart, outward_fdi_as_share_of_gdp),
                  by = c("country", "year", "counterpart")) %>%
        left_join(., exports_to_all_counterparts %>% select(country, year, counterpart, exports_as_share_of_gdp),
                  by = c("country", "year", "counterpart")) %>%
        left_join(., imports_from_all_counterparts %>% select(country, year, counterpart, imports_as_share_of_gdp),
                  by = c("country", "year", "counterpart")) %>%
        left_join(., ext_debt_owed_to_russia_and_china %>% select(country, year, counterpart, ext_debt_owed_to_counterpart_as_share_of_gdp),
                  by = c("country", "year", "counterpart")) %>%
        left_join(., ext_debt_owed_from_russia_and_china %>% select(country, year, counterpart, ext_debt_owed_from_counterpart_as_share_of_gdp),
                  by = c("country", "year", "counterpart")) %>%
        left_join(., russian_remittances %>% select(country, year, counterpart, 
                                                    remittance_from_counterpart_as_share_of_gdp, remittance_to_counterpart_as_share_of_gdp),
                  by = c("country", "year", "counterpart")) %>%
        filter(year >= 2010, year <= 2021) %>%
        pivot_longer(cols = c(inward_fdi_as_share_of_gdp,
                              outward_fdi_as_share_of_gdp,
                              exports_as_share_of_gdp,
                              imports_as_share_of_gdp,
                              ext_debt_owed_to_counterpart_as_share_of_gdp,
                              ext_debt_owed_from_counterpart_as_share_of_gdp,
                              remittance_from_counterpart_as_share_of_gdp,
                              remittance_to_counterpart_as_share_of_gdp),
                     names_to = "var", values_to = "values") %>%
        filter(country != "Russia")



#///////////////


# inspect
econ_links
econ_links %>% glimpse()
econ_links %>% nrow() # 8448 (44 countries * 12 years * 8 indicators * 2 counterparts)
econ_links %>% ncol() # 17

econ_links %>% count(country) # 44
econ_links %>% count(year) # 12
econ_links %>% count(counterpart) # 2
econ_links %>% count(var) # 8
econ_links %>% count(country, year, counterpart, var) %>% arrange(desc(n))

econ_links %>%
        select(country, year, counterpart, var, values) %>%
        count(counterpart, var)


#///////////////////////


# inspect indicators
econ_links %>% 
        select(country, year, counterpart, var, values) %>%
        pivot_wider(id_cols = c(country, year, counterpart), names_from = var, values_from = values) %>%
         skim() %>% as_tibble() %>% select(-numeric.hist) %>% select(skim_variable, complete_rate, starts_with("numeric"))

# inspect negative values
econ_links %>% 
        select(country, year, counterpart, var, values) %>%
        filter(values < 0) %>%
        # count(var)
        group_by(var) %>% 
        arrange(values) %>%
        slice(1) %>%
        ungroup()

econ_links %>%
        select(country, year, counterpart, var, values) %>%
        filter(var == "inward_fdi_as_share_of_gdp") %>% # significant negative values (only significant for luxembourg though)
        # filter(var == "outward_fdi_as_share_of_gdp") %>% # insignificant negative values
        # filter(var == "exports_as_share_of_gdp") %>%
        # filter(var == "imports_as_share_of_gdp") %>%
        # filter(var == "ext_debt_owed_to_counterpart_as_share_of_gdp") %>%
        # filter(var == "ext_debt_owed_from_counterpart_as_share_of_gdp") %>%
        # filter(var == "remittance_from_russia_as_share_of_gdp") %>%
        # filter(var == "remittance_to_russia_as_share_of_gdp") %>%
        arrange(values) %>%
        print(n = 100)


#///////////////////////


# inspect missing values

# as of 20220814
# fdi has data up to 2020
# imports/exports has data up to 2021
# debt has data up to 2020
# remittances have data up to 2021

# there are several gaps for different countries/indicators, so the best choice is to 
# impute 2021 if 2020 is non-NA, but otherwise convert NA values to 0
# this is the minimum manipulation of raw data, 
# while still balancing appropriateness of not having NA for all countries for indicators only published up to 2020

econ_links %>%
        select(country, year, counterpart, var, values) %>%
        unite(col = var, var, counterpart, year) %>%
        group_by(country, var) %>%
        miss_var_summary() %>%
        ungroup() %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )



#///////////////////////////////////////////////////////////////////////////////////////////////////////


# impute econ_links values ####
econ_links <- econ_links %>% 
        
        
        # get first_year_in_data
        group_by(country, counterpart, var) %>% arrange(year) %>%
        mutate(non_missing_flag = case_when(!is.na(values) ~ 1, 
                                            TRUE ~ 0),
               old_to_new_cumsum_non_missing_flag = cumsum(non_missing_flag),
               first_year_in_data = case_when(old_to_new_cumsum_non_missing_flag == 1 &
                                                      non_missing_flag == 1 ~ 
                                                      year),
               first_values_in_data = case_when(old_to_new_cumsum_non_missing_flag == 1 &
                                                     non_missing_flag == 1 ~ 
                                                     values)) %>%
        fill(first_year_in_data, .direction = "downup") %>%
        fill(first_values_in_data, .direction = "downup") %>%
        ungroup() %>%
        
        # arrange(country, counterpart, var, year) %>%
        # select(country, year, counterpart, var, values, first_year_in_data, first_values_in_data) %>%
        # print(n = 200)
        
        
        #/////////////////
        
        
        # get last_year_in_data
        group_by(country, counterpart, var) %>% arrange(desc(year)) %>%
        mutate(non_missing_flag = case_when(!is.na(values) ~ 1, 
                                            TRUE ~ 0),
               new_to_old_cumsum_non_missing_flag = cumsum(non_missing_flag),
               last_year_in_data = case_when(new_to_old_cumsum_non_missing_flag == 1 &
                                                     non_missing_flag == 1 ~ 
                                                     year),
               last_values_in_data = case_when(new_to_old_cumsum_non_missing_flag == 1 &
                                                    non_missing_flag == 1 ~ 
                                                    values)) %>%
        fill(last_year_in_data, .direction = "downup") %>%
        fill(last_values_in_data, .direction = "downup") %>%
        ungroup() %>%
        
        # arrange(country, counterpart, var, year) %>%
        # select(country, year, counterpart, var, values, last_year_in_data, last_values_in_data) %>%
        # print(n = 200)
        
        
        #/////////////////
        
        
        # get country_values_avg
        # note that NaN is converted to NA_real_ because when mean() is called on all NAs it produces NaN
        group_by(country, counterpart, var) %>%
        mutate(country_values_avg = mean(values, na.rm = TRUE),
               country_values_avg = case_when(is.nan(country_values_avg) ~ NA_real_,
                                           TRUE ~ country_values_avg)) %>%
        ungroup() %>%
        
        # arrange(country, counterpart, var, year) %>%
        # select(country, year, counterpart, var, values, country_values_avg) %>%
        # print(n = 200)
        
        
        #/////////////////
        
        
        # apply imputation criteria 

        mutate(values_imputed = case_when(
                
                # impute missing_first
                is.na(values) &
                        year < first_year_in_data &
                        !is.na(first_values_in_data) ~ first_values_in_data,
                
                # impute missing_last
                is.na(values) &
                        year > last_year_in_data &
                        !is.na(last_values_in_data) ~ last_values_in_data,
                
                # impute missing_middle
                is.na(values) &
                        year > first_year_in_data &
                        year < last_year_in_data &
                        !is.na(country_values_avg) ~ country_values_avg,
                
                # note that those missing_all values will be left with all NA values
                
                TRUE ~ values),
               
               # log imputation_condition
               imputation_condition = case_when(is.na(values) &
                                                        year < first_year_in_data &
                                                        !is.na(first_values_in_data) ~ "missing_first",
                                                is.na(values) &
                                                        year > last_year_in_data &
                                                        !is.na(last_values_in_data) ~ "missing_last",
                                                is.na(values) &
                                                        year > first_year_in_data &
                                                        year < last_year_in_data &
                                                        !is.na(country_values_avg) ~ "missing_middle",
                                                is.na(values) &
                                                        is.na(country_values_avg)  ~ "missing_all_but_not_imputed",
                                                TRUE ~ "not_imputed"),
               imputed_flag = case_when(imputation_condition %in% c("not_imputed", "missing_all_but_not_imputed") ~ 0,
                                        TRUE ~ 1))


#///////////////////////////////////////////////////////////////////////////////////////////////////////        


# inspect
econ_links
econ_links %>% glimpse()
econ_links %>% nrow() # 8448 (44 countries * 12 years * 8 indicators * 2 counterparts)
econ_links %>% ncol() # 28

econ_links %>% count(country) # 44
econ_links %>% count(year) # 12
econ_links %>% count(counterpart) # 2
econ_links %>% count(var) # 8
econ_links %>% count(country, year, counterpart, var) %>% arrange(desc(n))

econ_links %>%
        select(country, year, counterpart, var, values) %>%
        count(counterpart, var)


# ///////////////////////


# inspect imputation

# note that 577 values were imputed (182 + 327 + 68)
econ_links %>%
        select(country, year, counterpart, var, values, values_imputed, imputation_condition, imputed_flag) %>%
        # count(imputation_condition)
        # count(imputed_flag)
        # count(counterpart, var, imputed_flag) %>% arrange(desc(imputed_flag), desc(n)) %>% print(n = 20)
        # filter(imputed_flag == 1) %>% count(year)
        # filter(imputed_flag == 1) %>%
        # filter(is.na(values_imputed)) %>%
        filter(imputed_flag == 0, values_imputed != values)
        # print(n = 200)


#////////////////////////


# inspect missing_first
econ_links %>%
        select(country, year, counterpart, var, values, values_imputed, imputation_condition, imputed_flag,
               first_year_in_data, first_values_in_data) %>%
        arrange(country, counterpart, var, year) %>%
        filter(imputation_condition == "missing_first") %>%
        distinct(country, counterpart, var) %>% 
        left_join(., econ_links, by = c("country", "counterpart", "var")) %>%
        select(country, year, counterpart, var, values, values_imputed, imputation_condition, imputed_flag,
               first_year_in_data, first_values_in_data) %>%
        print(n = 200)


#////////////////////////


# inspect missing_last
econ_links %>%
        select(country, year, counterpart, var, values, values_imputed, imputation_condition, imputed_flag,
               last_year_in_data, last_values_in_data) %>%
        arrange(country, counterpart, var, year) %>%
        filter(imputation_condition == "missing_last") %>%
        distinct(country, counterpart, var) %>% 
        left_join(., econ_links, by = c("country", "counterpart", "var")) %>%
        select(country, year, counterpart, var, values, values_imputed, imputation_condition, imputed_flag,
               last_year_in_data, last_values_in_data) %>%
        print(n = 200)


#////////////////////////


# inspect missing_middle
econ_links %>%
        select(country, year, counterpart, var, values, values_imputed, imputation_condition, imputed_flag,
               country_values_avg) %>%
        arrange(country, counterpart, var, year) %>%
        filter(imputation_condition == "missing_middle") %>%
        distinct(country, counterpart, var) %>% 
        left_join(., econ_links, by = c("country", "counterpart", "var")) %>%
        select(country, year, counterpart, var, values, values_imputed, imputation_condition, imputed_flag,
               country_values_avg) %>%
        print(n = 200)


#////////////////////////


# inspect values_imputed

# confirm that values_imputed has values for all records except missing_all_but_not_imputed
econ_links %>% group_by(imputation_condition) %>% skim(values, values_imputed) %>%
        as_tibble() %>%
        select(-numeric.hist) %>% select(imputation_condition, skim_variable, complete_rate, starts_with("numeric")) %>% 
        print(n = nrow(.))
        
# skim values and values_imputed by var
econ_links %>% 
        select(country, year, counterpart, var, values, values_imputed) %>%
        group_by(counterpart, var) %>% skim(values, values_imputed) %>% as_tibble() %>%
        select(-numeric.hist) %>% select(var, counterpart, skim_variable, complete_rate, starts_with("numeric")) %>% 
        arrange(var, counterpart, skim_variable) %>%
        print(n = nrow(.))

econ_links %>% 
        select(country, year, counterpart, var, values, values_imputed, imputed_flag) %>%
        filter(var == "exports_as_share_of_gdp") %>%
        # filter(values == max(values, na.rm = TRUE))
        # filter(values_imputed == max(values_imputed, na.rm = TRUE))
        filter(country == "Belarus") %>% 
        print(n = nrow(.))

# all negative values are for inward/outward fdi 
econ_links %>% filter(values_imputed < 0) %>% count(country, var) %>% arrange(desc(n))

# luxembourg is only country with meaningfully negative values; 
econ_links %>% 
        select(country, year, counterpart, var, values, values_imputed) %>%
        
        # skim(total_econ_exposure_as_share_of_gdp)
        # group_by(counterpart) %>% skim(total_econ_exposure_as_share_of_gdp) %>% as_tibble() %>%
        # select(-numeric.hist) %>% select(skim_variable, counterpart, complete_rate, starts_with("numeric"))
        
        arrange(values_imputed) %>%
        # arrange(desc(values_imputed)) %>%
        print(n = 200)


#///////////////////////////////////////////////////////////////////////////////////////////////////////        


# add total_econ_exposure_as_share_of_gdp

econ_links <- econ_links %>% 
        group_by(country, year, counterpart) %>%
        mutate(total_econ_exposure_as_share_of_gdp = sumNA(values_imputed, na.rm = TRUE)) %>%
        ungroup()


# ///////////////////////


# inspect
econ_links
econ_links %>% glimpse()
econ_links %>% nrow() # 8448 (44 countries * 12 years * 8 indicators * 2 counterparts)
econ_links %>% ncol() # 29

econ_links %>% count(country) # 44
econ_links %>% count(year) # 12
econ_links %>% count(counterpart) # 2
econ_links %>% count(var) # 8
econ_links %>% count(country, year, counterpart, var) %>% arrange(desc(n))

econ_links %>%
        select(country, year, counterpart, var, values) %>%
        count(counterpart, var)

# check total_econ_exposure_as_share_of_gdp
econ_links %>%
        select(country, year, counterpart, var, values, values_imputed, total_econ_exposure_as_share_of_gdp) %>%
        arrange(country, year, counterpart, var) %>%
        print(n = 50)

# check values_imputed avg by counterpart and var
# see which vars offer most exposure to each counterpart
econ_links %>% group_by(counterpart, var) %>% skim(values_imputed) %>% as_tibble() %>% 
        select(-numeric.hist) %>% select(counterpart, var, skim_variable, complete_rate, starts_with("numeric")) %>% 
        arrange(counterpart, desc(numeric.mean))
 

# ///////////////////////


# plot 

# plot total_econ_exposure_as_share_of_gdp for several countries
econ_links %>% 
        select(country, mcp_grouping, year, counterpart, var, values, values_imputed, 
               imputed_flag, imputation_condition, total_econ_exposure_as_share_of_gdp) %>%
        # filter(mcp_grouping == "E&E Balkans") %>%
        filter(mcp_grouping == "E&E Eurasia") %>%
        filter(counterpart == "Russia") %>%
        # filter(counterpart == "China") %>%
        ggplot(data = ., mapping = aes(x = year, y = total_econ_exposure_as_share_of_gdp, color = country)) +
                geom_line(size = 1)


# plot values_imputed for single country
econ_links %>% 
        select(country, mcp_grouping, year, counterpart, var, values, values_imputed, 
               imputed_flag, imputation_condition, total_econ_exposure_as_share_of_gdp) %>%
        # filter(country == "Serbia") %>%
        filter(country == "Belarus") %>%
        filter(counterpart == "Russia") %>%
        # filter(counterpart == "China") %>%
        
        # arrange(country, counterpart, var, year) %>% print(n = 100)
        
        ggplot(data = ., mapping = aes(x = year, y = values_imputed, color = var)) +
        geom_line(size = 1)


#///////////////////////////////////////////////////////////////////////////////////////////////////////        


# read/write econ_links ####
# econ_links %>% write_csv(file = "data/economic_exposure_to_russia_and_china/economic_exposure_to_russia_and_china_20220814.csv")
econ_links <- read_csv(file = "data/economic_exposure_to_russia_and_china/economic_exposure_to_russia_and_china_20220814.csv")

# inspect
econ_links
econ_links %>% glimpse()
econ_links %>% nrow() # 8448
econ_links %>% ncol() # 29



#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# dots_exports_to_eu_27_as_share_of_total_exports ####

# data is available up to 2020 from IMF server
# note that EU-27 countries are used, because since 2010 only croatia joined (2013), and uk left (2020)
# other current EU countries joined after 2001, so using them could create discontinuous jumps in indicator when they turn on
# https://www.cs.mcgill.ca/~rwest/wikispeedia/wpcd/wp/l/List_of_European_Union_member_states_by_accession.htm
# https://europa.eu/european-union/about-eu/countries_en#tab-0-1
# https://en.wikipedia.org/wiki/Enlargement_of_the_European_Union

dots_exports_to_eu_27_as_share_of_total_exports <- read_csv(file = "data/imf/dots/DOT_10-14-2022 04-44-51-18_timeSeries.csv") %>%
        rename(country_name = `Country Name`,
               counterpart = `Counterpart Country Name`,
               category = `Indicator Name`) %>%
        filter(category == "Goods, Value of Exports, Free on board (FOB), US Dollars",
               Attribute == "Value",
               counterpart %in% c("Austria", "Belgium", "Bulgaria", "Croatia, Rep. of", "Cyprus", "Czech Rep.", 
                                  "Denmark", "Estonia, Rep. of", "Finland", "France", "Germany",
                                  "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", 
                                  "Malta", "Netherlands, The", "Poland, Rep. of", 
                                  "Portugal", "Romania", "Slovak Rep.", "Slovenia, Rep. of", "Spain", "Sweden", 
                                  "World")) %>%
        select(-c(`Country Code`, `Counterpart Country Code`, `Indicator Code`, Attribute, ...29)) %>%
        pivot_longer(cols = -c(country_name, counterpart, category), names_to = "year", values_to = "official_exports") %>%
        mutate(country_name = case_when(country_name == "Armenia, Rep. of" ~ "Armenia",
                                        country_name == "Azerbaijan, Rep. of" ~ "Azerbaijan",
                                        country_name == "Belarus, Rep. of" ~ "Belarus",
                                        country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "Croatia, Rep. of" ~ "Croatia",
                                        country_name == "Czech Rep." ~ "Czechia",
                                        country_name == "Estonia, Rep. of" ~ "Estonia",
                                        country_name == "Kazakhstan, Rep. of" ~ "Kazakhstan",
                                        country_name == "Kosovo, Rep. of" ~ "Kosovo",
                                        country_name == "Kyrgyz Rep." ~ "Kyrgyzstan",
                                        country_name == "Moldova, Rep. of" ~ "Moldova",
                                        country_name == "Netherlands, The" ~ "Netherlands",
                                        country_name == "North Macedonia, Republic of" ~ "N. Macedonia",
                                        country_name == "Poland, Rep. of" ~ "Poland",
                                        country_name == "Russian Federation" ~ "Russia",
                                        country_name == "Serbia, Rep. of" ~ "Serbia",
                                        country_name == "Slovak Rep." ~ "Slovakia",
                                        country_name == "Slovenia, Rep. of" ~ "Slovenia",
                                        country_name == "Tajikistan, Rep. of" ~ "Tajikistan",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States" ~ "U.S.",
                                        country_name == "Uzbekistan, Rep. of" ~ "Uzbekistan",
                                        TRUE ~ country_name),
               counterpart = case_when(counterpart == "Armenia, Rep. of" ~ "Armenia",
                                       counterpart == "Azerbaijan, Rep. of" ~ "Azerbaijan",
                                       counterpart == "Belarus, Rep. of" ~ "Belarus",
                                       counterpart == "Bosnia and Herzegovina" ~ "BiH",
                                       counterpart == "Croatia, Rep. of" ~ "Croatia",
                                       counterpart == "Czech Rep." ~ "Czechia",
                                       counterpart == "Estonia, Rep. of" ~ "Estonia",
                                       counterpart == "Kazakhstan, Rep. of" ~ "Kazakhstan",
                                       counterpart == "Kosovo, Rep. of" ~ "Kosovo",
                                       counterpart == "Kyrgyz Rep." ~ "Kyrgyzstan",
                                       counterpart == "Moldova, Rep. of" ~ "Moldova",
                                       counterpart == "Netherlands, The" ~ "Netherlands",
                                       counterpart == "North Macedonia, Republic of" ~ "N. Macedonia",
                                       counterpart == "Poland, Rep. of" ~ "Poland",
                                       counterpart == "Russian Federation" ~ "Russia",
                                       counterpart == "Serbia, Rep. of" ~ "Serbia",
                                       counterpart == "Slovak Rep." ~ "Slovakia",
                                       counterpart == "Slovenia, Rep. of" ~ "Slovenia",
                                       counterpart == "Tajikistan, Rep. of" ~ "Tajikistan",
                                       counterpart == "United Kingdom" ~ "U.K.",
                                       counterpart == "United States" ~ "U.S.",
                                       counterpart == "Uzbekistan, Rep. of" ~ "Uzbekistan",
                                       TRUE ~ counterpart),
               official_exports = as.numeric(official_exports),
               year = as.numeric(year)) 


#/////////////////////


# inspect
dots_exports_to_eu_27_as_share_of_total_exports
dots_exports_to_eu_27_as_share_of_total_exports %>% glimpse()
dots_exports_to_eu_27_as_share_of_total_exports %>% nrow() # 129129
dots_exports_to_eu_27_as_share_of_total_exports %>% ncol() # 5

# exports to eu-27 countries and the world
dots_exports_to_eu_27_as_share_of_total_exports %>% count(counterpart) %>% print(n = nrow(.))
dots_exports_to_eu_27_as_share_of_total_exports %>% count(country_name) %>% print(n = nrow(.))


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# sum up exports to eu-15, and pivot to get one record per country/year, having columns for exports_to_eu and exports_to_world
# get values for exports_to_eu_as_share_of_total_exports
dots_exports_to_eu_27_as_share_of_total_exports <- dots_exports_to_eu_27_as_share_of_total_exports %>%
        mutate(counterpart = case_when(counterpart %in% c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", 
                                                    "Denmark", "Estonia", "Finland", "France", "Germany",
                                                    "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", 
                                                    "Malta", "Netherlands", "Poland", 
                                                    "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden") ~ "EU-27",
                                       counterpart == "World" ~ counterpart)) %>%
        group_by(country_name, year, category, counterpart) %>%
        mutate(official_exports_sum = sumNA(official_exports, na.rm = TRUE)) %>%
        ungroup() %>% 
        distinct(country_name, year, category, counterpart, official_exports_sum) %>%
        pivot_wider(id_cols = c(country_name, category, year), names_from = counterpart, values_from = official_exports_sum) %>%
        rename(official_exports_to_eu_27 = `EU-27`, official_exports_to_world = World) %>%
        mutate(official_exports_to_eu_27_as_share_of_total_exports = official_exports_to_eu_27 / official_exports_to_world,
                values = official_exports_to_eu_27_as_share_of_total_exports)


#/////////////////////


# inspect
dots_exports_to_eu_27_as_share_of_total_exports
dots_exports_to_eu_27_as_share_of_total_exports %>% glimpse()
dots_exports_to_eu_27_as_share_of_total_exports %>% nrow() # 4767
dots_exports_to_eu_27_as_share_of_total_exports %>% ncol() # 7

# check
dots_exports_to_eu_27_as_share_of_total_exports %>% count(category)
dots_exports_to_eu_27_as_share_of_total_exports %>% count(year) %>% print(n = nrow(.))
dots_exports_to_eu_27_as_share_of_total_exports %>% count(country_name) %>% print(n = nrow(.))

# inspect country names
dots_exports_to_eu_27_as_share_of_total_exports %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>%
        distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>%
        anti_join(., dots_exports_to_eu_27_as_share_of_total_exports, by = c("country" = "country_name")) %>% select(country)

dots_exports_to_eu_27_as_share_of_total_exports %>%
        filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
        distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////////////////////////////////////////////////////////////////////////////////


# join to country_crosswalk 
# need to update indicator_name and high_value_is_good_outcome_flag for records with NA values from country/year join
dots_exports_to_eu_27_as_share_of_total_exports <- dots_exports_to_eu_27_as_share_of_total_exports %>%
        left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), .,
                  by = c("country" = "country_name", "year" = "year")) %>%
        mutate(indicator_name = "dots_exports_to_eu_27_as_share_of_total_exports",
               high_value_is_good_outcome_flag = 1)


#///////////////////////////


# inspect
dots_exports_to_eu_27_as_share_of_total_exports
dots_exports_to_eu_27_as_share_of_total_exports %>% glimpse()
dots_exports_to_eu_27_as_share_of_total_exports %>% nrow() # 945 (45 countries x 21 years)
dots_exports_to_eu_27_as_share_of_total_exports %>% ncol() # 21

# check country/year
dots_exports_to_eu_27_as_share_of_total_exports %>% distinct(country) %>% nrow() # 45
dots_exports_to_eu_27_as_share_of_total_exports %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))
dots_exports_to_eu_27_as_share_of_total_exports %>% count(year) %>% print(n = nrow(.))
dots_exports_to_eu_27_as_share_of_total_exports %>% count(country) %>% print(n = nrow(.))

# check values
dots_exports_to_eu_27_as_share_of_total_exports %>% count(category)
dots_exports_to_eu_27_as_share_of_total_exports %>% filter(is.na(category)) %>%
        select(country, year, category, official_exports_to_eu)

# check missing
# note all the missing are from 2001-2006, so wont show up in final dataset ranging only from 2010-2020
dots_exports_to_eu_27_as_share_of_total_exports %>% skim(official_exports_to_eu_27)
dots_exports_to_eu_27_as_share_of_total_exports %>% skim(official_exports_to_world)
dots_exports_to_eu_27_as_share_of_total_exports %>% skim(values)
dots_exports_to_eu_27_as_share_of_total_exports %>% group_by(year) %>% skim(values)

# note that kosovo is missing 2001-2006 
# montenegro is missing 2001-2005, serbia is missing 2001-2005,
dots_exports_to_eu_27_as_share_of_total_exports %>% filter(is.na(official_exports_to_eu_27)) %>%
        select(country, year, category, official_exports_to_eu_27) %>% print(n = nrow(.))
dots_exports_to_eu_27_as_share_of_total_exports %>% filter(is.na(official_exports_to_world)) %>%
        select(country, year, category, official_exports_to_world) %>% print(n = nrow(.))

dots_exports_to_eu_27_as_share_of_total_exports %>% filter(country == "Kosovo", year > 2016) %>%
        select(country, year, official_exports_to_eu, official_exports_to_world, values)
dots_exports_to_eu_27_as_share_of_total_exports %>% filter(country == "Turkmenistan", year > 2016) %>%
        select(country, year, official_exports_to_eu, official_exports_to_world, values)


#///////////////////////


# plot
dots_exports_to_eu_27_as_share_of_total_exports %>% 
        # filter(year >= 2009) %>%
        # filter(mcp_grouping == "E&E Balkans") %>%
        # filter(mcp_grouping == "E&E Eurasia") %>%
        # filter(mcp_grouping == "E&E graduates") %>%
        # filter(mcp_grouping == "CARs") %>%
        filter(mcp_grouping == "EU-15") %>%
        ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line()


#/////////////////


# test values
test_values_dots_exports_to_eu_27_as_share_of_total_exports <- function() {
        
        # 1
        expect_equal(object = dots_exports_to_eu_27_as_share_of_total_exports %>% 
                             filter(country == "Albania", year == 2017) %>%
                             select(country, year, official_exports_to_eu_27, 
                                    official_exports_to_world, official_exports_to_eu_27_as_share_of_total_exports, values) %>%
                             pull(values),
                     expected = (4359804427 - 2463201513) / 2463201513)
        
        # 2
        expect_equal(object = dots_exports_to_eu_27_as_share_of_total_exports %>% 
                             filter(country == "Kosovo", year == 2006) %>%
                             select(country, year, official_exports_to_eu_27, 
                                    official_exports_to_world, official_exports_to_eu_27_as_share_of_total_exports, values) %>%
                             pull(values),
                     expected = NA_real_)
        
        # 3
        expect_equal(object = dots_exports_to_eu_27_as_share_of_total_exports %>% 
                             filter(country == "Kosovo", year == 2018) %>%
                             select(country, year, official_exports_to_eu_27, 
                                    official_exports_to_world, official_exports_to_eu_27_as_share_of_total_exports, values) %>%
                             pull(values),
                     expected = (551000499 - 432183268) / 432183268)
        
        # 4
        expect_equal(object = dots_exports_to_eu_27_as_share_of_total_exports %>% 
                             filter(country == "Portugal", year == 2018) %>%
                             select(country, year, official_exports_to_eu_27, 
                                    official_exports_to_world, official_exports_to_eu_27_as_share_of_total_exports, values) %>%
                             pull(values),
                     expected = (115396976433 - 68361133471) / 68361133471)
}
test_values_dots_exports_to_eu_27_as_share_of_total_exports()


#/////////////////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# dots_exports_to_eu_27_as_share_of_total_exports %>% write.xlsx(file = "data/imf/dots/dots_exports_to_eu_27_as_share_of_total_exports.xlsx")
dots_exports_to_eu_27_as_share_of_total_exports <- read_excel(path = "data/imf/dots/dots_exports_to_eu_27_as_share_of_total_exports.xlsx") 

# inspect
dots_exports_to_eu_27_as_share_of_total_exports
dots_exports_to_eu_27_as_share_of_total_exports %>% glimpse()
dots_exports_to_eu_27_as_share_of_total_exports %>% nrow() # 945
dots_exports_to_eu_27_as_share_of_total_exports %>% ncol() # 21


#/////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////


# atlas_2digit_ee_region ####


# https://dataverse.harvard.edu/dataverse/atlas


#/////////////////////////////////////////////////////////////////////////////////////////////


# read atlas hs codes w/ descriptions 
hs <- read_stata("data/atlas_of_economic_complexity/hs_product.dta")
hs


# read atlas location codes
location <- read_stata("data/atlas_of_economic_complexity/location.dta")
location %>% filter(location_code == "ANS")
location


#//////////////////////////


# inspect location
# note that atlas has no data for kosovo
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% select(country, iso3) %>% 
        anti_join(., location, by = c("iso3" = "location_code"))


# inspect hs
hs %>% print(n = 100)
hs %>% count(parent_id) %>% print(n = 100)
hs %>% count(hs_product_code, parent_id)
hs %>% count(hs_product_code)
hs %>% count(hs_product_name_short_en)
hs %>% count(level)

# inspect specific hs codes
hs %>% filter(str_detect(string = hs_product_code, pattern = regex("^2711", ignore_case = TRUE)))
hs %>% filter(str_detect(string = hs_product_code, pattern = regex("^2701", ignore_case = TRUE)))
hs %>% filter(str_detect(string = hs_product_code, pattern = regex("^2709", ignore_case = TRUE)))
hs %>% filter(str_detect(string = hs_product_code, pattern = regex("^2710", ignore_case = TRUE))) %>% print(n = nrow(.))
hs %>% filter(str_detect(string = hs_product_code, pattern = regex("^2716", ignore_case = TRUE)))


#/////////////////////////////////////////////////////////////////////////////////////////////


# read in atlas_2digit, and get 1digit classification 
# note the 1digit classificaiton is product_id 0 to 10
# the hs file has crosswalk of hs_product_code to parent_id, where parent_id 0 to 10 corresponds to product_id 0 to 10
# atlas uses both 1digit and 2digit (see hs codes above and atlas explorer)
atlas_2digit <- read_dta(file = "data/atlas_of_economic_complexity/country_partner_hsproduct2digit_year.dta") 
# rm(atlas_2digit)


atlas_2digit <- atlas_2digit %>% 
        filter(location_code %in% (country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% pull(iso3))) %>%
        left_join(., hs %>% select(hs_product_code, hs_product_name_short_en, parent_id), by = "hs_product_code") %>%
        rename(hs_product_name_2digit = hs_product_name_short_en,
               hs_product_code_1digit = parent_id) %>%
        left_join(., hs %>% 
                          filter(hs_product_code %in% c(seq(from = 0, to = 10, by = 1))) %>%
                          select(hs_product_code, hs_product_name_short_en) %>%
                          mutate(hs_product_code = as.numeric(hs_product_code)), 
                  by = c("hs_product_code_1digit" = "hs_product_code")) %>%
        rename(hs_product_name_1digit = hs_product_name_short_en) %>%
        left_join(., location %>% select(location_code, location_name_short_en) %>%
                          rename(location_name = location_name_short_en), 
                  by = "location_code") %>%
        left_join(., location %>% select(location_code, location_name_short_en) %>%
                          rename(partner_code = location_code, partner_name = location_name_short_en), 
                  by = "partner_code") %>%
        rename(hs_product_code_2digit = hs_product_code) %>%
        select(location_name, location_code, location_id, 
               partner_name, partner_code, partner_id,
               year, product_id, hs_product_code_2digit, hs_product_name_2digit, hs_product_code_1digit, hs_product_name_1digit,
               export_value, import_value, hs_eci, hs_coi)


#//////////////////////////


# inspect
atlas_2digit 
atlas_2digit %>% glimpse()
atlas_2digit %>% nrow() # 8083009
atlas_2digit %>% ncol() # 16

atlas_2digit %>% count(location_code)
atlas_2digit %>% count(hs_product_code_2digit, hs_product_name_2digit, hs_product_code_1digit, hs_product_name_1digit) %>% print(n = nrow(.))
atlas_2digit %>% filter(hs_product_name_1digit == "Agriculture") %>% count(hs_product_name_2digit) %>% print(n = nrow(.))
atlas_2digit %>% count(year) %>% arrange(desc(year)) %>% print(n = nrow(.))

# notice that ANS is partner_code for undeclared countries (there is no ANS value in location_code)
atlas_2digit %>% select(partner_code, partner_name) %>% filter(partner_code == "ANS")
atlas_2digit %>% select(location_code, location_name) %>% filter(location_code == "ANS")

# check location_code
# location_code matches country_crosswalk for all but kosovo
country_crosswalk %>% 
        filter(ee_region_flag == 1 | country == "U.S.") %>%
        anti_join(., atlas_2digit %>% select(location_code), by = c("iso3" = "location_code"))
atlas_2digit %>% distinct(location_code) %>%
        anti_join(., country_crosswalk %>% select(iso3), by = c("location_code" = "iso3"))


atlas_2digit %>%
        # filter(location_code %in% (country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% pull(iso3))) %>%
        count(location_code) %>% print(n = nrow(.))

# check partner_code
country_crosswalk %>% 
        anti_join(., atlas_2digit %>% select(partner_code), by = c("iso3" = "partner_code"))
atlas_2digit %>% distinct(partner_code, partner_name) %>%
        anti_join(., country_crosswalk %>% select(iso3), by = c("partner_code" = "iso3")) %>%
        print(n = nrow(.))

atlas_2digit %>%
        # filter(location_code %in% (country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% pull(iso3))) %>%
        count(partner_code) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# add country_crosswalk to get atlas_2digit_ee_region, and drop atlas_2digit
atlas_2digit_ee_region <- atlas_2digit %>% 
        left_join(., country_crosswalk %>% select(country, iso3, mcp_grouping, eu_27_flag, ee_region_flag), 
                  by = c("location_code" = "iso3")) %>%
        left_join(., country_crosswalk %>% select(country, iso3, mcp_grouping, eu_27_flag, ee_region_flag) %>%
                          rename(partner_country = country, 
                                 partner_mcp_grouping = mcp_grouping, 
                                 partner_eu_27_flag = eu_27_flag, 
                                 partner_ee_region_flag = ee_region_flag), 
                  by = c("partner_code" = "iso3")) %>%
        relocate(country, mcp_grouping, eu_27_flag, ee_region_flag, .after = location_id) %>%
        relocate(partner_country, partner_mcp_grouping, partner_eu_27_flag, partner_ee_region_flag, .after = partner_id) %>%
        mutate(partner_country_from_crosswalk = partner_country,
               partner_country = case_when(is.na(partner_country) ~ partner_name,
                                           TRUE ~ partner_country)) %>%
        relocate(partner_country_from_crosswalk, .after = partner_country)

# drop atlas_2digit
rm(atlas_2digit)


#//////////////////////////


# inspect
atlas_2digit_ee_region 
atlas_2digit_ee_region %>% glimpse()
atlas_2digit_ee_region %>% nrow() # 8083009
atlas_2digit_ee_region %>% ncol() # 25

# check countries
atlas_2digit_ee_region %>% count(location_code) %>% print(n = nrow(.))
atlas_2digit_ee_region %>% count(location_name) %>% print(n = nrow(.)) # 44
atlas_2digit_ee_region %>% count(partner_name) %>% print(n = nrow(.)) # 242

# compare country/partner_country names from country_crosswalk with atlas names in location_name/partner_name
# all location_names match country_crosswalk

# prior to cleaning, there are several partner_names that have different spellings in country_crosswalk
# and there are several partner_names in atlas for small territories/historic/other that have no atlas code / iso3 match
# note that the code above keeps country from crosswalk for locations,
# it also keeps partner_country from crosswalk where there are matched values,
# and where there are no matched values for partner_country from crosswalk (small territories/historic/other), will import partner_name from atlas
atlas_2digit_ee_region %>% filter(country != location_name) %>% distinct(country, location_name)
atlas_2digit_ee_region %>% filter(partner_country_from_crosswalk != partner_name) %>% 
        distinct(partner_country, partner_country_from_crosswalk, partner_name, partner_code) %>% print(n = nrow(.))
atlas_2digit_ee_region %>% filter(is.na(country)) %>% distinct(country, location_name)
atlas_2digit_ee_region %>% filter(is.na(partner_country_from_crosswalk)) %>% 
        distinct(partner_country, partner_country_from_crosswalk, partner_name, partner_code) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# get atlas_2digit_ee_region
# chose not to filter down partners to just ee_region, also including china, eu27, us, and ANS-undeclared
# this leaves a large 8 mil record dataset, but gains the option to show top import/export partners for ee countries at 2digit level
atlas_2digit_ee_region <- atlas_2digit_ee_region %>%
        group_by(location_code, year) %>% 
        mutate(imports_from_world_all_products = sumNA(import_value, na.rm = TRUE),
               exports_to_world_all_products = sumNA(export_value, na.rm = TRUE)) %>%
        ungroup() %>%
        group_by(location_code, year, hs_product_code_1digit) %>% 
        mutate(imports_from_world_hs_1digit = sumNA(import_value, na.rm = TRUE),
               exports_to_world_hs_1digit = sumNA(export_value, na.rm = TRUE)) %>%
        ungroup() %>%
        group_by(location_code, year, hs_product_code_2digit) %>% 
        mutate(imports_from_world_hs_2digit = sumNA(import_value, na.rm = TRUE),
               exports_to_world_hs_2digit = sumNA(export_value, na.rm = TRUE)) %>%
        ungroup() %>%
        # filter(partner_code %in% 
        #                (c(country_crosswalk %>% 
        #                           filter(ee_region_flag == 1 | country == "U.S." | country %in% c("China", "Cyprus", "Malta")) %>% 
        #                           pull(iso3), "ANS"))) %>%
        group_by(location_code, partner_code, year, hs_product_code_1digit) %>%
        mutate(imports_from_partner_hs_1digit = sumNA(import_value, na.rm = TRUE),
               exports_to_partner_hs_1digit = sumNA(export_value, na.rm = TRUE)) %>%
        ungroup() %>%
        rename(imports_from_partner_hs_2digit = import_value,
               exports_to_partner_hs_2digit = export_value) %>%
        arrange(location_name, year, partner_name, hs_product_code_1digit, hs_product_code_2digit) %>%
        relocate(hs_eci, hs_coi, .after = last_col())


#/////////////////////////


# inspect
atlas_2digit_ee_region
atlas_2digit_ee_region %>% glimpse()
atlas_2digit_ee_region %>% nrow() # 8083009
atlas_2digit_ee_region %>% ncol() # 33

# check countries
atlas_2digit_ee_region %>% count(location_name) %>% print(n = nrow(.)) # 44
atlas_2digit_ee_region %>% count(partner_name) %>% print(n = nrow(.)) # 242

# check
atlas_2digit_ee_region %>% 
        filter(country == "Albania", year == 2011) %>% 
        filter(partner_country == "China") %>%
        filter(hs_product_code_1digit == 8) %>%
        
        select(country, partner_country, year, 
               hs_product_name_1digit, hs_product_name_2digit,
               imports_from_partner_hs_2digit, imports_from_partner_hs_1digit, imports_from_world_hs_1digit, imports_from_world_all_products) %>%
        arrange(country, year, partner_country, hs_product_name_1digit, hs_product_name_2digit, desc(imports_from_partner_hs_2digit)) %>%
        # select(country, partner_country, year,
        #        hs_product_name_1digit, hs_product_name_2digit,
        #        exports_to_partner_hs_2digit, exports_to_partner_hs_1digit, exports_to_world_hs_1digit) %>%
        # arrange(country, year, partner_country, hs_product_code_1digit, hs_product_code_2digit, desc(exports_to_partner_hs_2digit)) %>%
        
        
        # distinct(country, partner_country, year, hs_product_name_1digit,
        #        imports_from_partner_hs_1digit, imports_from_world_hs_1digit, imports_from_world_all_products) %>%
        # distinct(country, partner_country, year, hs_product_name_1digit,
        #        exports_to_partner_hs_1digit, exports_to_world_hs_1digit) %>%
        

print(n = 20)

# check hs 27 - fuels/oils
atlas_2digit_ee_region %>% count(hs_product_code_2digit, hs_product_name_2digit, hs_product_code_1digit, hs_product_name_1digit) %>% print(n = nrow(.))
atlas_2digit_ee_region %>% filter(hs_product_code_2digit == 27) %>% count(location_name, partner_name)


#/////////////////////////////////////////////////////////////////////////////////////////////


# add gdp_current

# load gdp_current ####
# https://data.worldbank.org/indicator/NY.GDP.MKTP.CD

gdp_current <- read_excel(path = "data/world_bank/API_NY.GDP.MKTP.CD_DS2_en_excel_v2_4334130.xls", sheet = "Data", skip = 3) %>%
        rename(country_name = `Country Name`, iso3 = `Country Code`) %>%
        select(-c(`Indicator Name`, `Indicator Code`)) %>%
        pivot_longer(cols = -c(country_name, iso3), names_to = "year", values_to = "gdp_current") %>%
        mutate(year = as.numeric(year))


#/////////////////


# inspect
gdp_current
gdp_current %>% glimpse()
gdp_current %>% nrow() # 16492
gdp_current %>% ncol() # 4

gdp_current %>% count(year) %>% arrange(desc(year))
gdp_current %>% filter(year == 2021) %>% distinct(gdp_current)
gdp_current %>% filter(is.na(gdp_current), year >= 2010) %>% count(country_name) %>% arrange(desc(n)) %>% print(n = 20)

gdp_current %>% anti_join(., atlas_2digit_ee_region %>% select(location_code), by = c("iso3" = "location_code")) %>% 
        distinct(country_name) %>% print(n = nrow(.))
atlas_2digit_ee_region %>% anti_join(., gdp_current %>% select(iso3), by = c("location_code" = "iso3")) %>% 
        distinct(country) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# add gdp_current
atlas_2digit_ee_region <- atlas_2digit_ee_region %>% 
        left_join(., gdp_current %>% select(iso3, year, gdp_current), by = c("location_code" = "iso3", "year"))


#/////////////////////////


# inspect
atlas_2digit_ee_region
atlas_2digit_ee_region %>% glimpse()
atlas_2digit_ee_region %>% nrow() # 8083009
atlas_2digit_ee_region %>% ncol() # 34

atlas_2digit_ee_region %>% count(year) %>% arrange(desc(year)) %>% print(n = nrow(.))

atlas_2digit_ee_region %>% distinct(country, year, gdp_current) %>% sample_n(10)
atlas_2digit_ee_region %>% mutate(imports_from_partner_as_share_of_gdp = imports_from_partner_hs_2digit / gdp_current,
                                  imports_from_partner_as_share_of_imports_from_world = imports_from_partner_hs_2digit / imports_from_world_hs_2digit) %>%
        filter(hs_product_code_2digit == 27,
               year == 2020) %>%
        # filter(mcp_grouping == "E&E Eurasia") %>%
        select(location_name, partner_name, year, hs_product_code_2digit, hs_product_name_2digit, 
               imports_from_partner_hs_2digit, imports_from_world_hs_2digit, gdp_current, 
               imports_from_partner_as_share_of_gdp, imports_from_partner_as_share_of_imports_from_world) %>%
        arrange(location_name, desc(year), desc(imports_from_partner_as_share_of_gdp)) %>%
        group_by(location_name) %>% slice(1) %>%
        ungroup() %>%
        arrange(desc(imports_from_partner_as_share_of_gdp)) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# test values
# note values are manually pulled from atlas explorer website
# https://atlas.cid.harvard.edu/explore

test_values_atlas_2digit_ee_region <- function() {
        
        # imports
        
        # albania imports of 1digit machinery from china in 2011
        expect_equal(object = atlas_2digit_ee_region %>%
                             filter(country == "Albania", year == 2011) %>% 
                             filter(partner_country == "China") %>%
                             filter(hs_product_code_1digit == 7) %>%
                             
                             select(country, partner_country, year, 
                                    hs_product_name_1digit, hs_product_name_2digit, hs_product_code_2digit,
                                    imports_from_partner_hs_2digit, imports_from_partner_hs_1digit, 
                                    imports_from_world_hs_1digit, imports_from_world_all_products) %>%
                             arrange(country, year, partner_country, hs_product_name_1digit, 
                                     hs_product_name_2digit, desc(imports_from_partner_hs_2digit)) %>%
                             slice(1) %>% 
                             mutate(imports_from_partner_hs_1digit = as.numeric(round(imports_from_partner_hs_1digit / 1000000, digits = 1))) %>%
                             pull(imports_from_partner_hs_1digit),
                     expected = 49.6) 
        
        
        #//////////////////////
        
        
        # serbia imports of 2digit minerals - hs 27 mineral fuels, etc from russia in 2020
        expect_equal(object = atlas_2digit_ee_region %>%
                             filter(country == "Serbia", year == 2020) %>% 
                             filter(partner_country == "Russia") %>%
                             filter(hs_product_code_1digit == 3) %>%
                             
                             select(country, partner_country, year, 
                                    hs_product_name_1digit, hs_product_name_2digit, hs_product_code_2digit,
                                    imports_from_partner_hs_2digit, imports_from_partner_hs_1digit, 
                                    imports_from_world_hs_1digit, imports_from_world_all_products) %>%
                             arrange(country, year, partner_country, hs_product_name_1digit, 
                                     hs_product_name_2digit, desc(imports_from_partner_hs_2digit)) %>%
                             filter(hs_product_code_2digit == 27) %>%
                             mutate(imports_from_partner_hs_2digit = as.numeric(round(imports_from_partner_hs_2digit / 1000000, digits = 1))) %>%
                             pull(imports_from_partner_hs_2digit),
                     expected = 347) 
        
        
        #//////////////////////
        
        
        # belarus imports of 1digit agriculture from world in 2016
        expect_equal(object = atlas_2digit_ee_region %>%
                             filter(country == "Belarus", year == 2016) %>% 
                             filter(partner_country == "Russia") %>%
                             filter(hs_product_code_1digit == 1) %>%
                             
                             select(country, partner_country, year, 
                                    hs_product_name_1digit, hs_product_name_2digit, hs_product_code_2digit,
                                    imports_from_partner_hs_2digit, imports_from_partner_hs_1digit, 
                                    imports_from_world_hs_1digit, imports_from_world_all_products) %>%
                             arrange(country, year, partner_country, hs_product_name_1digit, 
                                     hs_product_name_2digit, desc(imports_from_partner_hs_2digit)) %>%
                             slice(1) %>% 
                             mutate(imports_from_world_hs_1digit = as.numeric(round(imports_from_world_hs_1digit / 1000000000, digits = 1))) %>%
                             pull(imports_from_world_hs_1digit),
                     expected = 3.6) 
        
        
        #//////////////////////
        
        
        # georgia imports of 2digit vehicle = HS 86 trains from world in 2015
        expect_equal(object = atlas_2digit_ee_region %>%
                             filter(country == "Georgia", year == 2015) %>% 
                             filter(partner_country == "Russia") %>%
                             filter(hs_product_code_1digit == 6) %>%
                             
                             select(country, partner_country, year, 
                                    hs_product_name_1digit, hs_product_name_2digit, hs_product_code_2digit,
                                    imports_from_partner_hs_2digit, imports_from_partner_hs_1digit, 
                                    imports_from_world_hs_2digit, imports_from_world_hs_1digit, imports_from_world_all_products) %>%
                             arrange(country, year, partner_country, hs_product_name_1digit, 
                                     hs_product_name_2digit, desc(imports_from_partner_hs_2digit)) %>%
                             filter(hs_product_code_2digit == 86) %>% 
                             mutate(imports_from_world_hs_2digit = as.numeric(round(imports_from_world_hs_2digit / 1000000, digits = 1))) %>%
                             pull(imports_from_world_hs_2digit),
                     expected = 12.4) 
        
        
        #///////////////////////////////////////////////////////////////////////////////////////////// 
        
        # exports
        
        # moldova exports of 1digit machinery from china in 2011
        expect_equal(object = atlas_2digit_ee_region %>%
                             filter(country == "Moldova", year == 2011) %>% 
                             filter(partner_country == "Ukraine") %>%
                             filter(hs_product_code_1digit == 7) %>%
                             
                             select(country, partner_country, year, 
                                    hs_product_name_1digit, hs_product_name_2digit, hs_product_code_2digit,
                                    exports_to_partner_hs_2digit, exports_to_partner_hs_1digit, 
                                    exports_to_world_hs_1digit, exports_to_world_all_products) %>%
                             arrange(country, year, partner_country, hs_product_name_1digit, 
                                     hs_product_name_2digit, desc(exports_to_partner_hs_2digit)) %>%
                             slice(1) %>% 
                             mutate(exports_to_partner_hs_1digit = as.numeric(round(exports_to_partner_hs_1digit / 1000000, digits = 1))) %>%
                             pull(exports_to_partner_hs_1digit),
                     expected = 7.9) 
        
        
        #//////////////////////
        
        
        # bih exports of 2digit minerals - HS 27 mineral fuels, etc to serbia in 2018
        expect_equal(object = atlas_2digit_ee_region %>%
                             filter(country == "BiH", year == 2018) %>% 
                             filter(partner_country == "Serbia") %>%
                             filter(hs_product_code_1digit == 3) %>%
                             
                             select(country, partner_country, year, 
                                    hs_product_name_1digit, hs_product_name_2digit, hs_product_code_2digit,
                                    exports_to_partner_hs_2digit, exports_to_partner_hs_1digit, 
                                    exports_to_world_hs_1digit, exports_to_world_all_products) %>%
                             arrange(country, year, partner_country, hs_product_name_1digit, 
                                     hs_product_name_2digit, desc(exports_to_partner_hs_2digit)) %>%
                             slice(1) %>% 
                             mutate(exports_to_partner_hs_2digit = as.numeric(round(exports_to_partner_hs_2digit / 1000000, digits = 1))) %>%
                             pull(exports_to_partner_hs_2digit),
                     expected = 246.4) 
        
        
        #//////////////////////
        
        
        # azerbaijan exports of 1digit minerals to world in 2019
        expect_equal(object = atlas_2digit_ee_region %>%
                             filter(country == "Azerbaijan", year == 2019) %>% 
                             filter(partner_country == "Serbia") %>%
                             filter(hs_product_code_1digit == 3) %>%
                             
                             select(country, partner_country, year, 
                                    hs_product_name_1digit, hs_product_name_2digit, hs_product_code_2digit,
                                    exports_to_partner_hs_2digit, exports_to_partner_hs_1digit, 
                                    exports_to_world_hs_1digit, exports_to_world_all_products) %>%
                             arrange(country, year, partner_country, hs_product_name_1digit, 
                                     hs_product_name_2digit, desc(exports_to_partner_hs_2digit)) %>%
                             slice(1) %>% 
                             mutate(exports_to_world_hs_1digit = as.numeric(round(exports_to_world_hs_1digit / 1000000000, digits = 1))) %>%
                             pull(exports_to_world_hs_1digit),
                     expected = 14.7) 
        
        
        #//////////////////////
        
        
        # ukraine exports of 2digit agriculture - HS 10 cereals to world in 2020
        expect_equal(object = atlas_2digit_ee_region %>%
                             filter(country == "Ukraine", year == 2020) %>% 
                             filter(partner_country == "Serbia") %>%
                             filter(hs_product_code_1digit == 1) %>%
                             
                             select(country, partner_country, year, 
                                    hs_product_name_1digit, hs_product_name_2digit, hs_product_code_2digit,
                                    exports_to_partner_hs_2digit,
                                    # exports_to_partner_hs_1digit, 
                                    exports_to_world_hs_2digit, exports_to_world_hs_1digit, 
                                    # exports_to_world_all_products
                             ) %>%
                             arrange(country, year, partner_country, hs_product_name_1digit, 
                                     hs_product_name_2digit, desc(exports_to_partner_hs_2digit)) %>%
                             filter(hs_product_code_2digit == 10) %>%
                             mutate(exports_to_world_hs_2digit = as.numeric(round(exports_to_world_hs_2digit / 1000000000, digits = 1))) %>%
                             pull(exports_to_world_hs_2digit),
                     expected = 9.2) 
        
}
test_values_atlas_2digit_ee_region()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write

# atlas_2digit_ee_region %>% write_csv(file = "data/atlas_of_economic_complexity/atlas_2digit_ee_region.csv")
# atlas_2digit_ee_region <- read_csv(file = "data/atlas_of_economic_complexity/atlas_2digit_ee_region.csv")

# inspect
atlas_2digit_ee_region
atlas_2digit_ee_region %>% glimpse()
atlas_2digit_ee_region %>% nrow() # 8083009
atlas_2digit_ee_region %>% ncol() # 34


# inspect hs_products
atlas_2digit_ee_region %>% 
        count(hs_product_code_1digit,
              hs_product_name_1digit)

atlas_2digit_ee_region %>% 
        count(hs_product_code_1digit,
              hs_product_name_1digit,
              hs_product_code_2digit,
              hs_product_name_2digit) %>%
        print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# atlas_1digit_ee_region ####

# get atlas_1digit_ee_region
atlas_1digit_ee_region <- atlas_2digit_ee_region %>% select(-c(matches(match = "2digit"), product_id, hs_eci, hs_coi)) %>% distinct()


#/////////////////////////


# inspect
atlas_1digit_ee_region
atlas_1digit_ee_region %>% glimpse()
atlas_1digit_ee_region %>% nrow() # 1381409
atlas_1digit_ee_region %>% ncol() # 25


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# atlas_1digit_ee_region %>% write_csv(file = "data/atlas_of_economic_complexity/atlas_1digit_ee_region.csv")
atlas_1digit_ee_region <- read_csv(file = "data/atlas_of_economic_complexity/atlas_1digit_ee_region.csv")

# inspect
atlas_1digit_ee_region
atlas_1digit_ee_region %>% glimpse()
atlas_1digit_ee_region %>% nrow() # 1381409
atlas_1digit_ee_region %>% ncol() # 25


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////



# atlas_2digit_non_bilateral ####


# https://dataverse.harvard.edu/dataverse/atlas


#/////////////////////////////////////////////////////////////////////////////////////////////


# read atlas hs codes w/ descriptions 
hs <- read_stata("data/atlas_of_economic_complexity/hs_product.dta")
hs
hs %>% glimpse()


# read atlas location codes
location <- read_stata("data/atlas_of_economic_complexity/location.dta")
location %>% filter(location_code == "ANS")
location


#//////////////////////////


# inspect location
# note that atlas has no data for kosovo
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% select(country, iso3) %>% 
        anti_join(., location, by = c("iso3" = "location_code"))


# inspect hs
hs %>% print(n = 100)
hs %>% count(parent_id) %>% print(n = 100)
hs %>% count(hs_product_code, parent_id)
hs %>% count(hs_product_code)
hs %>% count(hs_product_name_short_en)
hs %>% count(level)

# inspect specific hs codes
hs %>% filter(str_detect(string = hs_product_code, pattern = regex("^2711", ignore_case = TRUE)))
hs %>% filter(str_detect(string = hs_product_code, pattern = regex("^2701", ignore_case = TRUE)))
hs %>% filter(str_detect(string = hs_product_code, pattern = regex("^2709", ignore_case = TRUE)))
hs %>% filter(str_detect(string = hs_product_code, pattern = regex("^2710", ignore_case = TRUE))) %>% print(n = nrow(.))
hs %>% filter(str_detect(string = hs_product_code, pattern = regex("^2716", ignore_case = TRUE)))

# note services
hs %>% filter(parent_id >= 400)


#/////////////////////////////////////////////////////////////////////////////////////////////


# read in atlas_2digit, and get 1digit classification 
# note the 1digit classificaiton is product_id 0 to 10
# the hs file has crosswalk of hs_product_code to parent_id, where parent_id 0 to 10 corresponds to product_id 0 to 10
# atlas uses both 1digit and 2digit (see hs codes above and atlas explorer)
atlas_2digit_non_bilateral <- read_dta(file = "data/atlas_of_economic_complexity/country_hsproduct2digit_year.dta") 


#////////////////////////////


# inspect
atlas_2digit_non_bilateral
atlas_2digit_non_bilateral %>% glimpse()
atlas_2digit_non_bilateral %>% nrow() # 628881
atlas_2digit_non_bilateral %>% ncol() # 10

# note that all three levels of services codes (eg 10, 4xx, and 4xxx) all have the exact same record count
# and below confirms they have the same values
atlas_2digit_non_bilateral %>% 
        left_join(., hs %>% select(hs_product_code, hs_product_name_short_en, parent_id), by = "hs_product_code") %>% 
        # count(hs_product_code, hs_product_name_short_en) %>% print(n = nrow(.))
        count(hs_product_code, hs_product_name_short_en, parent_id) %>% print(n = nrow(.))

# inspect services parent_id
# note that services weirdly have three duplicate records: parent_id = 10, parent_id = 4xx, and parent_id 4xxx
# for all other records, parent_id is the hs_1digit, as expected
atlas_2digit_non_bilateral %>% 
        left_join(., hs %>% select(hs_product_code, hs_product_name_short_en, parent_id), by = "hs_product_code") %>% 
        filter(hs_product_name_short_en == "ICT") %>%
        filter(location_code == "ABW", year == 2020)

# confirm that once parent_id is dropped, the services records are exact duplicates, and calling distinct
# gets them to be unique at the country/year/hs_product_code level
atlas_2digit_non_bilateral %>% 
        left_join(., hs %>% select(hs_product_code, hs_product_name_short_en, parent_id), by = "hs_product_code") %>% 
        filter(parent_id >= 10) %>%
        select(-parent_id) %>%
        # nrow(.) # 83025
        distinct() %>% # 27675
        count(location_id, year, hs_product_code) # 27675


atlas_2digit_non_bilateral %>% 
        left_join(., hs %>% select(hs_product_code, hs_product_name_short_en, parent_id), by = "hs_product_code") %>% 
        filter(parent_id >= 10) %>%
        select(-parent_id) %>%
        # nrow(.) # 83025
        distinct() %>% 
        mutate(parent_id = 10)

#/////////////////////////////////////////////////////////////////////////////////////////////



# need to row_bind services seperately because of weird issue inspected above where they have three levels of parent_id for duplicate records
atlas_2digit_non_bilateral <- atlas_2digit_non_bilateral %>% 
        left_join(., hs %>% select(hs_product_code, hs_product_name_short_en, parent_id), by = "hs_product_code") %>%
        filter(parent_id < 10) %>%
        bind_rows(., 
                  atlas_2digit_non_bilateral %>% 
                          left_join(., hs %>% select(hs_product_code, hs_product_name_short_en, parent_id), by = "hs_product_code") %>% 
                          filter(parent_id >= 10) %>%
                          select(-parent_id) %>%
                          distinct() %>% 
                          mutate(parent_id = 10)) %>%
        mutate(parent_id = as.character(parent_id)) %>%
        rename(hs_product_name_2digit = hs_product_name_short_en,
               hs_product_code_1digit = parent_id) %>%
        left_join(., hs %>%
                          filter(hs_product_code %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
                                                        "unspecified", "travel", "transport", "ict", "financial")) %>%
                          select(hs_product_code, hs_product_name_short_en) %>%
                          mutate(
                                  hs_product_code = case_when(hs_product_code %in%
                                                                      c("unspecified", "travel", "transport", "ict", "financial") ~ "10",
                                                              TRUE ~ hs_product_code),
                                  hs_product_name_short_en = case_when(hs_product_name_short_en %in% 
                                                                               c("Unspecified", "Travel and tourism", "Transport", "ICT", "Insurance and finance") ~ "Services",
                                                                       TRUE ~ hs_product_name_short_en)) %>%
                          distinct(),
                  by = c("hs_product_code_1digit" = "hs_product_code")) %>%
        rename(hs_product_name_1digit = hs_product_name_short_en) %>%
        rename(hs_product_code_2digit = hs_product_code) %>%
        left_join(., location %>% select(location_code, location_name_short_en) %>%
                          rename(location_name = location_name_short_en), 
                  by = "location_code") %>%
        rename(country_name = location_name) %>%
        rename(exports_2digit = export_value, 
               imports_2digit = import_value) %>%
        select(country_name, location_code, location_id, 
               year, product_id, hs_product_code_2digit, hs_product_name_2digit, hs_product_code_1digit, hs_product_name_1digit,
               exports_2digit, imports_2digit, hs_eci, hs_coi) %>%
        mutate(hs_product_name_2digit = case_when(hs_product_name_2digit == "Unspecified" ~ "Unspecified services",
                                                  TRUE ~ hs_product_name_2digit),
                hs_product_name_1digit = case_when(hs_product_name_2digit %in% c("Insurance and finance",
                                                                                "ICT",
                                                                                "Transport",
                                                                                "Travel and tourism",
                                                                                "Unspecified services") ~ hs_product_name_2digit,
                                                  TRUE ~ hs_product_name_1digit)) %>%
        group_by(country_name, year, hs_product_name_1digit) %>%
        mutate(exports_1digit = sumNA(exports_2digit, na.rm = TRUE),
               imports_1digit = sumNA(imports_2digit, na.rm = TRUE)) %>%
        ungroup()
                

#///////////////////


# inspect
atlas_2digit_non_bilateral
atlas_2digit_non_bilateral %>% glimpse()
atlas_2digit_non_bilateral %>% nrow() # 628881
atlas_2digit_non_bilateral %>% ncol() # 15

atlas_2digit_non_bilateral %>% count(country_name) # 243
atlas_2digit_non_bilateral %>% count(year) %>% print(n = nrow(.)) # 27 (up to 2020)


# inspect hs_products
atlas_2digit_non_bilateral %>% 
        count(hs_product_code_1digit,
              hs_product_name_1digit)

atlas_2digit_non_bilateral %>% 
        count(hs_product_code_1digit,
              hs_product_name_1digit,
              hs_product_code_2digit,
              hs_product_name_2digit) %>%
        print(n = nrow(.))


# check specific country
atlas_2digit_non_bilateral %>% 
        filter(country_name == "Armenia", year == 2020) %>%
        select(country_name, year, 
               hs_product_name_2digit, hs_product_code_2digit, exports_2digit, 
               hs_product_name_1digit, hs_product_code_1digit, exports_1digit) %>%
        # arrange(desc(exports_1digit))
        arrange(desc(exports_2digit))

# check country
# note that atlas doesn't have kosovo
country_crosswalk %>% 
        # filter(ee_region_flag == 1 | country == "U.S.") %>% 
        select(country, iso3) %>%
        anti_join(., atlas_2digit_non_bilateral %>% select(country_name, location_code),
                  by = c("iso3" = "location_code"))


#/////////////////////////////////////////////////////////////////////////////////////////////


# add country_crosswalk
atlas_2digit_non_bilateral <- atlas_2digit_non_bilateral %>%
        left_join(., country_crosswalk %>% select(country, iso3, mcp_grouping, eu_27_flag, ee_region_flag), 
                  by = c("location_code" = "iso3")) %>%
        select(-country_name) %>%
        relocate(country, .before = everything())


#///////////////////


# inspect
atlas_2digit_non_bilateral
atlas_2digit_non_bilateral %>% glimpse()
atlas_2digit_non_bilateral %>% nrow() # 628881
atlas_2digit_non_bilateral %>% ncol() # 18

atlas_2digit_non_bilateral %>% count(country) # 212
atlas_2digit_non_bilateral %>% count(year) %>% print(n = nrow(.)) # 27 (up to 2020)


#/////////////////////////////////////////////////////////////////////////////////////////////


# add gdp_current

# load gdp_current ####
# https://data.worldbank.org/indicator/NY.GDP.MKTP.CD

gdp_current <- read_excel(path = "data/world_bank/API_NY.GDP.MKTP.CD_DS2_en_excel_v2_4334130.xls", sheet = "Data", skip = 3) %>%
        rename(country_name = `Country Name`, iso3 = `Country Code`) %>%
        select(-c(`Indicator Name`, `Indicator Code`)) %>%
        pivot_longer(cols = -c(country_name, iso3), names_to = "year", values_to = "gdp_current") %>%
        mutate(year = as.numeric(year))


#/////////////////


# inspect
gdp_current
gdp_current %>% glimpse()
gdp_current %>% nrow() # 16492
gdp_current %>% ncol() # 4

gdp_current %>% count(year) %>% arrange(desc(year))
gdp_current %>% filter(year == 2021) %>% distinct(gdp_current)
gdp_current %>% filter(is.na(gdp_current), year >= 2010) %>% count(country_name) %>% arrange(desc(n)) %>% print(n = 20)

gdp_current %>% anti_join(., atlas_2digit_ee_region %>% select(location_code), by = c("iso3" = "location_code")) %>% 
        distinct(country_name) %>% print(n = nrow(.))
atlas_2digit_ee_region %>% anti_join(., gdp_current %>% select(iso3), by = c("location_code" = "iso3")) %>% 
        distinct(country) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# add gdp_current
atlas_2digit_non_bilateral  <- atlas_2digit_non_bilateral %>% 
        left_join(., gdp_current %>% select(iso3, year, gdp_current), by = c("location_code" = "iso3", "year")) %>%
        mutate(exports_2digit_as_share_of_gdp = exports_2digit / gdp_current,
               imports_2digit_as_share_of_gdp = imports_2digit / gdp_current,
               exports_1digit_as_share_of_gdp = exports_1digit / gdp_current,
               imports_1digit_as_share_of_gdp = imports_1digit / gdp_current)


#/////////////////////////


# inspect
atlas_2digit_non_bilateral 
atlas_2digit_non_bilateral  %>% glimpse()
atlas_2digit_non_bilateral  %>% nrow() # 628881
atlas_2digit_non_bilateral  %>% ncol() # 23


#//////////////////////////


# test_values
# note that atlas explorer doesn't show 2digit, just 1 digit aggregate and then 4 digit breakdown
# but can test 1digit, which is a manual roll-up of 2_digit, so if 1 digit matches then two digit should be good
test_values_atlas_2digit_non_bilateral <- function() {
        
        # verified on atlas explorer website
        expect_equal(object = atlas_2digit_non_bilateral %>%
                        filter(country == "Armenia") %>%
                        filter(year == 2020) %>%
                        distinct(country, year, hs_product_name_1digit, exports_1digit, gdp_current, exports_1digit_as_share_of_gdp) %>%
                        select(country, year, hs_product_name_1digit, exports_1digit, gdp_current, exports_1digit_as_share_of_gdp) %>%
                        filter(hs_product_name_1digit == "ICT") %>% pull(exports_1digit_as_share_of_gdp) %>% 
                             signif(., digits = 3),
                     expected = signif(532000000 / 12641209802.112, digits = 3))
        
        # verified on atlas explorer website
        expect_equal(object = atlas_2digit_non_bilateral %>%
                             filter(country == "Serbia") %>%
                             filter(year == 2019) %>%
                             distinct(country, year, hs_product_name_1digit, exports_1digit, gdp_current, exports_1digit_as_share_of_gdp) %>%
                             select(country, year, hs_product_name_1digit, exports_1digit, gdp_current, exports_1digit_as_share_of_gdp) %>%
                             filter(hs_product_name_1digit == "Agriculture") %>% pull(exports_1digit_as_share_of_gdp) %>% 
                             signif(., digits = 3),
                     expected = signif(4560000000 / 51514222381.8428, digits = 3))
        
}
test_values_atlas_2digit_non_bilateral()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write

# atlas_2digit_non_bilateral %>% write_csv(file = "data/atlas_of_economic_complexity/atlas_2digit_non_bilateral.csv")
atlas_2digit_non_bilateral <- read_csv(file = "data/atlas_of_economic_complexity/atlas_2digit_non_bilateral.csv")

# inspect
atlas_2digit_non_bilateral
atlas_2digit_non_bilateral %>% glimpse()
atlas_2digit_non_bilateral %>% nrow() # 628881
atlas_2digit_non_bilateral %>% ncol() # 23


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# fao food_price_index ####

# https://www.fao.org/worldfoodsituation/foodpricesindex/en/
# https://news.un.org/en/story/2022/10/1130022


# get food_price_index_nominal
food_price_index_nominal <- read_excel(path = "data/fao/Food_price_indices_data_jan665.xls", sheet = "Indices_Monthly", skip = 2) %>%
        # read_excel(path = "data/fao/Food_price_indices_data_oct173.xls", sheet = "Indices_Monthly", skip = 2) %>%
        slice(-1) %>% 
        rename(date = Date, 
               food_price_index = `Food Price Index`,
               meat_price_index = Meat,
               dairy_price_index = Dairy,
               cereals_price_index = Cereals,
               oils_price_index = Oils,
               sugar_price_index = Sugar) %>%
        mutate(year = year(date),
               month = month(date),
               index_type = "nominal") %>%
        select(date, year, month, index_type,
               food_price_index, meat_price_index, dairy_price_index, cereals_price_index, oils_price_index, sugar_price_index)


#///////////////


# inspect
food_price_index_nominal
food_price_index_nominal %>% glimpse()
food_price_index_nominal %>% nrow() # 396
food_price_index_nominal %>% ncol() # 10

food_price_index_nominal %>% arrange(desc(date))
food_price_index_nominal %>% count(year) %>% print(n = nrow(.))
food_price_index_nominal %>% skim()


#/////////////////////////////////////////////////////////////////////////////////////////////


# get food_price_index_real
# note that read_excel reads in three extra rows at end with all NA values, will drop them
food_price_index_real <- read_excel(path = "data/fao/Food_price_indices_data_jan665.xls", sheet = "Indices_Monthly_Real", skip = 2) %>%
        # read_excel(path = "data/fao/Food_price_indices_data_oct173.xls", sheet = "Indices_Monthly_Real", skip = 2) %>%
        slice(-1) %>% 
        rename(date = Month, 
               food_price_index = `Food Price Index`,
               meat_price_index = `Meat Price Index`,
               dairy_price_index = `Dairy Price Index`,
               cereals_price_index = `Cereals Price Index`,
               oils_price_index = `Oils Price Index`,
               sugar_price_index = `Sugar Price Index`) %>%
        mutate(year = year(date),
               month = month(date),
               index_type = "real") %>%
        select(date, year, month, index_type,
               food_price_index, meat_price_index, dairy_price_index, cereals_price_index, oils_price_index, sugar_price_index) %>%
        filter(!is.na(date))


#///////////////


# inspect
food_price_index_real
food_price_index_real %>% glimpse()
food_price_index_real %>% nrow() # 396
food_price_index_real %>% ncol() # 10

food_price_index_real %>% tail()
food_price_index_real %>% arrange(desc(date))
food_price_index_real %>% count(year) %>% print(n = nrow(.))
food_price_index_real %>% skim()
food_price_index_real %>% mutate(row_number = row_number()) %>% filter(is.na(year)) %>% select(-sugar_price_index)


#/////////////////////////////////////////////////////////////////////////////////////////////


# combine to get food_price_index

food_price_index <- food_price_index_nominal %>% bind_rows(., food_price_index_real) %>%
        mutate(date = ymd(date))


#///////////////


# inspect
food_price_index
food_price_index %>% glimpse()
food_price_index %>% nrow() # 792
food_price_index %>% ncol() # 10

food_price_index %>% arrange(desc(date))
food_price_index %>% count(year) %>% print(n = nrow(.))
food_price_index %>% skim()


#/////////////////////////////////////////////////////////////////////////////////////////////


# plot

food_price_index %>% 
        ggplot(data = ., mapping = aes(x = date, y = food_price_index, color = index_type)) + geom_line(size = 1)


food_price_index %>% 
        filter(index_type == "real") %>%
        select(-food_price_index) %>%
        pivot_longer(cols = -c(date, year, month, index_type), names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = date, y = values, color = var)) + geom_line(size = 1)
        

#/////////////////////////////////////////////////////////////////////////////////////////////


# test values

test_values_food_price_index <- function(){
        
        # food_price_index_nominal = 2022-09-01
        expect_equal(object = food_price_index %>% filter(date == "2022-09-01", index_type == "nominal") %>% pull(food_price_index),
                     expected = 136.041941317831)
        
        # food_price_index_real = 2022-09-01
        expect_equal(object = food_price_index %>% filter(date == "2022-09-01", index_type == "real") %>% pull(food_price_index),
                     expected = 133.105786960728)
        
        # meat_price_index_nominal = 2020-06-01
        expect_equal(object = food_price_index %>% filter(date == "2020-06-01", index_type == "nominal") %>% pull(meat_price_index),
                     expected = 94.8164103705907)
        
        # dairy_price_index_real = 2018-02-01
        expect_equal(object = food_price_index %>% filter(date == "2018-02-01", index_type == "real") %>% pull(dairy_price_index),
                     expected = 106.825474283693)
}
test_values_food_price_index()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# food_price_index %>% write_csv(file = "data/fao/food_price_index_20230115.csv")
food_price_index <- read_csv(file = "data/fao/food_price_index_20230115.csv")

# inspect
food_price_index
food_price_index %>% glimpse()
food_price_index %>% nrow() # 792
food_price_index %>% ncol() # 10



#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# load iea energy data ####

# https://www.iea.org/data-and-statistics/data-browser?country=ALBANIA

# energy data is loaded from mcp/malign_influence/data/iea
# iea_energy_balances_cleaned_20210902.csv
# iea_fossil_fuel_imports_from_russia_20210921.csv
# which is cleaned with mcp/malign_influence/code/fmir_data.R

# note that natural_gas_by_origin data is the only one where IEA doesn't use ktoe units, but instead uses TJ
# so will convert TJ to ktoe (not because it's necessary to get fuel_type_import_share_from_russia, but just in case to avoid
# possible confusion from re-using this cleaned data down the road, i don't want to accidentally treat 
# natural gas TJ imports_from_russia as ktoe)
# Unit of measurement of energy consumption : 1 TOE = 0.041868 TJ.
# https://stats.oecd.org/glossary/detail.asp?ID=4109
# Armenia TES in 2019
# convert ktoe to TJ:
# 0.041868 * (3400.449 ktoe * 1000) = 142370 TJ
# 0.041868 * (3400.449 * 1000)
# convert TJ to ktoe:
# (142370 TJ / .041868) / 1000
# (142370 / .041868) / 1000

energy <- read_csv(file = "data/iea/iea_energy_balances_cleaned_20210902.csv") %>%
        select(country_name, year, unit, starts_with(match = "TES")) %>% 
        pivot_longer(cols = -c(country_name, year, unit), names_to = "fuel_type", values_to = "fuel_type_tes") %>% 
        mutate(total_tes = case_when(fuel_type == "TES_total" ~ fuel_type_tes, 
                                     TRUE ~ NA_real_)) %>%
        group_by(country_name, year) %>%
        fill(total_tes, .direction = "updown") %>%
        ungroup() %>%
        mutate(fuel_type_tes_as_share_of_total_tes = fuel_type_tes / total_tes) %>%
        filter(fuel_type != "TES_total") %>%
        left_join(., country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S."), by = c("country_name" = "country")) %>%
        rename(country = "country_name")


#////////////////////


# inspect

energy
energy %>% glimpse()
energy %>% nrow() # 9000
energy %>% ncol() # 19

energy %>% count(year)
energy %>% count(country) # 45

# check values
# macedonia crude TES in 2001
# convert ktoe to TJ:
# 0.041868 * (652 ktoe * 1000) = 27297.94 TJ
# 0.041868 * (652 * 1000)
# convert TJ to ktoe:
# (27297.94 TJ / .041868) / 1000
# (27297.94 / .041868) / 1000

# plot
energy %>% 
        filter(country == "N. Macedonia") %>%
        # filter(fuel_type %in% c("TES_crude_oil", "TES_natural_gas")) %>%
        ggplot(data = ., mapping = aes(x = year, y = fuel_type_tes_as_share_of_total_tes, color = fuel_type)) + 
        geom_line()


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# freedom house nit ####

# https://freedomhouse.org/report/nations-transit

# note that higher scores are better

# note that democracy_percentage is used by mcp as "democratic reforms"
# it is calculated by min-max on democracy score, and democracy score is the average of the individual indicators
# eg macedonia in 2004 (report_year 2005)
# (4.10714285714286 - 1) / (7 - 1)

# note that mcp converts indicator scores with min-max (see macedonia country brief - july 2022)
nit <- read_excel(path = "data/freedom_house/All_Data_Nations_in_Transit_NIT_2005-2022_For_website.xlsx", sheet = "NIT 2005-2022") %>%
        rename(country_name = Country,
               year = Year) %>%
        select(-Region) %>%
        mutate(report_year = year,
               year = report_year - 1,
               country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "Czech Republic" ~ "Czechia",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        TRUE ~ country_name)) %>%
        pivot_longer(cols = -c(country_name, year, report_year, "Regime Classification"), names_to = "var", values_to = "values") %>%
        mutate(values_std = case_when(var %in% c("Democracy Percentage", "Democracy Percentage (Rounded)") ~ NA_real_,
                                      TRUE ~ (values - 1) / (7 - 1)),
               indicator_flag = case_when(var %in% c("Democracy Score", "Democracy Percentage", "Democracy Percentage (Rounded)") ~ 0,
                                          TRUE ~ 1)) %>%
        left_join(., country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S."), by = c("country_name" = "country")) %>%
        rename(country = "country_name", regime_classification = "Regime Classification") %>%
        relocate(regime_classification:values_std, .after = everything())


#///////////////


# inspect

nit
nit %>% glimpse()
nit %>% nrow() # 5220
nit %>% ncol() # 19

nit %>% count(country) # 29
nit %>% count(year)
nit %>% count(var)
nit %>% count(var, indicator_flag)
nit %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% distinct(country_name) %>% print(n = nrow(.))

# plot democratic reforms
nit %>% 
        filter(mcp_grouping %in% c("E&E Balkans")) %>%
        select(country, year, var, values, values_std) %>%
        filter(var == "Democracy Score") %>%
        ggplot(data = ., mapping = aes(x = year, y = values_std, color = country)) + geom_line() +
        scale_y_continuous(limits = c(0, 1))

# plot individual indicators
nit %>% 
        filter(country == "N. Macedonia") %>%
        filter(year >= 2010) %>%
        filter(indicator_flag == 1) %>%
        select(country, year, var, values, values_std) %>%
        ggplot(data = ., mapping = aes(x = year, y = values_std, color = var)) + geom_line() +
        scale_y_continuous(limits = c(0, 1))


#/////////////////////////////////////////////////////////////////////////////////////////////


# test values

test_nit_values <- function() {
        
        # 1
        expect_equal(object = nit %>% filter(country == "N. Macedonia", year == 2018, var == "Electoral Process") %>%
                             select(country, year, var, values, values_std) %>%
                             pull(values),
                     expected = 4)
        
        # 2
        expect_equal(object = nit %>% filter(country == "Albania", year == 2015, var == "Corruption") %>%
                             select(country, year, var, values, values_std) %>%
                             pull(values),
                     expected = 2.75)
        
        # 3
        expect_equal(object = nit %>% filter(country == "Kosovo", year == 2019, var == "Democracy Score") %>%
                             select(country, year, var, values, values_std) %>%
                             mutate(values_std = values_std * 100) %>%
                             pull(values_std),
                     expected = 36.3095238095238)
}

test_nit_values()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# nit %>% write_csv(file = "data/freedom_house/nit_20220805.csv")
nit <- read_csv(file = "data/freedom_house/nit_20220805.csv")

# inspect
nit
nit %>% glimpse()
nit %>% nrow() # 5220
nit %>% ncol() # 20


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# load un migrant data ####


# https://www.un.org/development/desa/pd/content/international-migrant-stock

migrants <- read_excel(path = "data/un/UN_MigrantStockByOriginAndDestination_2019.xlsx", sheet = "Table 1", skip = 15, na = c("-", "..")) %>%
        rename(year = "...1",
               destination_country = "...3") %>%
        select(-c(...2, ...4, ...5, ...6)) %>%
        pivot_longer(cols = -c(year, destination_country), names_to = "origin_country", values_to = "migrant_stock") %>%
        mutate(migrant_stock = as.numeric(migrant_stock),
               origin_country = case_when(origin_country == "Bosnia and Herzegovina" ~ "BiH",
                                          origin_country == "North Macedonia" ~ "N. Macedonia",
                                          origin_country == "Russian Federation" ~ "Russia",
                                          origin_country == "United Kingdom" ~ "U.K.",
                                          origin_country == "United States of America" ~ "U.S.",
                                          origin_country == "Republic of Moldova" ~ "Moldova",
                                          origin_country == "Bahamas" ~ "Bahamas, The",
                                          origin_country == "Bolivia (Plurinational State of)" ~ "Bolivia",
                                          origin_country == "Brunei Darussalam" ~ "Brunei",
                                          origin_country == "Congo" ~ "Congo (Brazzaville)",    
                                          origin_country == "Côte d'Ivoire" ~ "Cote d'Ivoire",
                                          origin_country == "Curaçao" ~  "Curacao",
                                          origin_country == "Dem. People's Republic of Korea" ~ "Korea, North",
                                          origin_country == "Democratic Republic of the Congo" ~ "Congo (Kinshasa)",
                                          origin_country == "Gambia" ~ "Gambia, The",
                                          origin_country == "Iran (Islamic Republic of)" ~ "Iran",
                                          origin_country == "Lao People's Democratic Republic" ~ "Laos",
                                          origin_country == "Micronesia (Fed. States of)" ~ "Micronesia, Federated States of",
                                          origin_country == "Myanmar" ~ "Burma",
                                          origin_country == "Republic of Korea" ~ "Korea, South",
                                          origin_country == "State of Palestine" ~ "West Bank and Gaza",
                                          origin_country == "Syrian Arab Republic" ~ "Syria",
                                          origin_country == "United Republic of Tanzania" ~ "Tanzania",
                                          origin_country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
                                          origin_country == "Viet Nam" ~ "Vietnam",
                                          TRUE ~ origin_country)) %>%
        filter(!str_detect(string = destination_country, pattern = regex("western sahara", ignore_case = TRUE)),
               !str_detect(string = destination_country, pattern = regex("world bank", ignore_case = TRUE)),
               !str_detect(string = destination_country, pattern = regex("europe", ignore_case = TRUE)),
               !str_detect(string = destination_country, pattern = regex("western asia", ignore_case = TRUE)),
               !str_detect(string = destination_country, pattern = regex("western africa", ignore_case = TRUE)),
               !str_detect(string = destination_country, pattern = regex("countries", ignore_case = TRUE)),
               !str_detect(string = destination_country, pattern = regex("develop", ignore_case = TRUE)),
               !str_detect(string = destination_country, pattern = regex("sub-sahara", ignore_case = TRUE)),
               !str_detect(string = destination_country, pattern = regex("southern asia", ignore_case = TRUE)),
               !str_detect(string = destination_country, pattern = regex("southern africa", ignore_case = TRUE)),
               !str_detect(string = destination_country, pattern = regex("south america", ignore_case = TRUE)),
               !str_detect(string = destination_country, pattern = regex("oceania", ignore_case = TRUE)),
               !str_detect(string = destination_country, pattern = regex("northern america", ignore_case = TRUE)),
               !str_detect(string = destination_country, pattern = regex("northern africa", ignore_case = TRUE)),
               !str_detect(string = destination_country, pattern = regex("eastern africa", ignore_case = TRUE)),
               !str_detect(string = destination_country, pattern = regex("geographic", ignore_case = TRUE)),
               !str_detect(string = destination_country, pattern = regex("income", ignore_case = TRUE)),
               !str_detect(string = destination_country, pattern = regex("middle africa", ignore_case = TRUE)),
               !str_detect(string = destination_country, pattern = regex("latin america", ignore_case = TRUE)),
               !str_detect(string = destination_country, pattern = regex("eastern asia", ignore_case = TRUE)),
               !str_detect(string = destination_country, pattern = regex("southern asia", ignore_case = TRUE)),
               !str_detect(string = destination_country, pattern = regex("central asia", ignore_case = TRUE)),
               !str_detect(string = destination_country, pattern = regex("central america", ignore_case = TRUE)),
               !(destination_country %in% c("Africa", 
                                            "Asia"))) %>%
        mutate(migrant_stock_to_world = case_when(destination_country == "WORLD" ~ migrant_stock,
                                                  TRUE ~ NA_real_)) %>% arrange(origin_country, year) %>%
        group_by(origin_country, year) %>%
        fill(migrant_stock_to_world, .direction = "updown") %>%
        ungroup()


#//////////////////


# inspect

migrants
migrants %>% glimpse()
migrants %>% nrow() # 389865
migrants %>% ncol() # 5


# check country
migrants %>% count(origin_country) %>% print(n = nrow(.)) # 235
migrants %>% count(destination_country) %>% print(n = nrow(.))

# note the UN does not have data for kosovo
migrants %>% anti_join(., country_crosswalk,
                       by = c("origin_country" = "country")) %>% count(origin_country) %>% print(n = nrow(.))
country_crosswalk %>%
        anti_join(., migrants, by = c("country" = "origin_country"))

country_crosswalk %>% filter(str_detect(string = country, pattern = regex("tai", ignore_case = TRUE))) %>% count(country)


#/////////////////////////////////////////////////////////////////////////////////////////////


# get population ####

# https://data.worldbank.org/indicator/SP.POP.TOTL

population <- read_excel(path = "data/un/API_SP.POP.TOTL_DS2_en_excel_v2_4353768.xls", sheet = "Data", skip = 3) %>%
        rename(country_name = `Country Name`, iso3 = `Country Code`) %>%
        select(-c(`Indicator Name`, `Indicator Code`)) %>%
        pivot_longer(cols = -c(country_name, iso3), names_to = "year", values_to = "population") %>%
        mutate(year = as.numeric(year),
               country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "Brunei Darussalam" ~ "Brunei",
                                        country_name == "Myanmar" ~ "Burma",
                                        country_name == "Congo, Dem. Rep." ~ "Congo (Kinshasa)",
                                        country_name == "Congo, Rep." ~ "Congo (Brazzaville)",
                                        country_name == "Czech Republic" ~ "Czechia",
                                        country_name == "Egypt, Arab Rep." ~ "Egypt",
                                        country_name == "Iran, Islamic Rep." ~ "Iran",
                                        country_name == "Korea, Dem. People's Rep." ~ "Korea, North",
                                        country_name == "Korea, Rep." ~ "Korea, South",
                                        country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                        country_name == "Lao PDR" ~ "Laos",
                                        country_name == "Micronesia, Fed. Sts." ~ "Micronesia, Federated States of",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        country_name == "Russian Federation" ~ "Russia",
                                        country_name == "St. Kitts and Nevis" ~ "Saint Kitts and Nevis",
                                        country_name == "St. Vincent and the Grenadines" ~ "Saint Vincent and the Grenadines",
                                        country_name == "St. Lucia" ~ "Saint Lucia",
                                        country_name == "Slovak Republic" ~ "Slovakia",
                                        country_name == "Syrian Arab Republic" ~ "Syria",
                                        country_name == "Turkiye" ~ "Turkey",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States" ~ "U.S.",
                                        country_name == "Venezuela, RB" ~ "Venezuela",
                                        country_name == "Yemen, Rep." ~ "Yemen",
                                        TRUE ~ country_name))


#/////////////


# inspect
population
population %>% glimpse()
population %>% nrow() # 16492
population %>% ncol() # 4

population %>% count(year) %>% print(n = nrow(.))
population %>% count(country_name)

# check names
country_crosswalk %>% anti_join(., population, by = c("country" = "country_name")) %>% count(country) %>% print(n = nrow(.))
population %>% filter(str_detect(string = country_name, pattern = regex("lucia", ignore_case = TRUE))) %>% distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("baham", ignore_case = TRUE)))


migrants %>% anti_join(., population, by = c("origin_country" = "country_name")) %>% count(origin_country) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# add population and country_crosswalk
migrants <- migrants %>%
        left_join(., country_crosswalk, by = c("origin_country" = "country")) %>%
        left_join(., population %>% select(-iso3), by = c("origin_country" = "country_name", "year")) %>%
        mutate(migrant_stock_as_share_of_migrant_stock_to_world = migrant_stock / migrant_stock_to_world,
               migrant_stock_as_share_of_population = migrant_stock / population) %>%
        rename(country = origin_country) %>%
        relocate(country, .before = everything()) %>%
        relocate(c(migrant_stock, migrant_stock_to_world), .after = ee_region_flag)


#//////////////////


# inspect
migrants
migrants %>% glimpse()
migrants %>% nrow() # 389865
migrants %>% ncol() # 20


# note no kosovo data
# also names for several small territories were not adjusted to join, but that's fine
migrants %>% count(country) # 235
migrants %>% count(year)
migrants %>% anti_join(., country_crosswalk, by = c("country")) %>% count(country) %>% print(n = nrow(.))

# inspect
migrants %>%
        #         filter(is.na(year)) %>%
        filter(country == "Moldova") %>%
        select(country, year, destination_country, migrant_stock, migrant_stock_to_world, 
               migrant_stock_as_share_of_migrant_stock_to_world, migrant_stock_as_share_of_population)

# plot
migrants %>% 
        filter(year == 2019) %>%
        filter(destination_country == "WORLD") %>%
        # filter(str_detect(string = country, pattern = "West Bank and Gaza")) %>%
        filter(population >= 1000000) %>%
        # distinct(country, year, destination_country, migrant_stock_as_share_of_population) %>%
        select(country, mcp_grouping, year, destination_country, migrant_stock, migrant_stock_to_world, population,
               migrant_stock_as_share_of_migrant_stock_to_world, migrant_stock_as_share_of_population) %>%
        arrange(desc(migrant_stock_as_share_of_population)) %>%
        # mutate(row_number = row_number()) %>% 
        # filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        slice(1:50) %>%
        ggplot(data = ., mapping = aes(x = fct_reorder(.f = country, .x = migrant_stock_as_share_of_population, .desc = FALSE), 
                                       y = migrant_stock_as_share_of_population)) + geom_col() +
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )


#/////////////////////////////////////////////////////////////////////////////////////////////


# test values

test_values_migrants <- function() {
        
        #1 
        expect_equal(object = migrants %>% filter(country == "Albania", year == 2019, destination_country == "WORLD") %>% 
                             select(country, year, destination_country, migrant_stock, migrant_stock_to_world, 
                                    migrant_stock_as_share_of_migrant_stock_to_world, migrant_stock_as_share_of_population) %>%
                             pull(migrant_stock_as_share_of_population),
                     expected = 1207032 / 2854191)
        
        #2
        expect_equal(object = migrants %>% filter(country == "Albania", year == 2015, destination_country == "WORLD") %>% 
                             select(country, year, destination_country, migrant_stock, migrant_stock_to_world, 
                                    migrant_stock_as_share_of_migrant_stock_to_world, migrant_stock_as_share_of_population) %>%
                             pull(migrant_stock_as_share_of_population),
                     expected = 1129044 / 2880703)
        
        #3
        expect_equal(object = migrants %>% filter(country == "Armenia", year == 2019, destination_country == "WORLD") %>% 
                             select(country, year, destination_country, migrant_stock, migrant_stock_to_world, 
                                    migrant_stock_as_share_of_migrant_stock_to_world, migrant_stock_as_share_of_population) %>%
                             pull(migrant_stock_as_share_of_population),
                     expected = 964848 / 2957728)
        
        #3
        expect_equal(object = migrants %>% filter(country == "Armenia", year == 2015, destination_country == "WORLD") %>% 
                             select(country, year, destination_country, migrant_stock, migrant_stock_to_world, 
                                    migrant_stock_as_share_of_migrant_stock_to_world, migrant_stock_as_share_of_population) %>%
                             pull(migrant_stock_as_share_of_population),
                     expected = 946978 / 2925559)
}

test_values_migrants()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# migrants %>% write_csv(file = "data/un/migrants_20220805.csv")
migrants <- read_csv(file = "data/un/migrants_20220805.csv")

# inspect
migrants
migrants %>% glimpse()
migrants %>% nrow() # 389865
migrants %>% ncol() # 20


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# load un_annual_net_migration_per_100_population ####


# https://population.un.org/wpp/Download/Standard/MostUsed/

un_annual_net_migration_per_100_population <- read_excel(path = "data/un/WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx", 
                                                                sheet = "Estimates", 
                       skip = 16, na = c("-", "..", "...")) %>%
        rename(country_name = `Region, subregion, country or area *`,
               iso3 = `ISO3 Alpha-code`,
               year = Year,
               values = `Net Migration Rate (per 1,000 population)`,
               ) %>%
        mutate(year = as.numeric(year),
               indicator_name = "annual_net_migration_per_100_population",
               values = as.numeric(values)) %>%
        filter(Type == "Country/Area") %>%
        select(country_name, iso3, year, indicator_name, values) %>%
        pivot_longer(cols = -c(country_name, iso3, year, indicator_name), names_to = "var", values_to = "values") %>%
        mutate(country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                          country_name == "North Macedonia" ~ "N. Macedonia",
                                          country_name == "Russian Federation" ~ "Russia",
                                          country_name == "United Kingdom" ~ "U.K.",
                                          country_name == "United States of America" ~ "U.S.",
                                          country_name == "Republic of Moldova" ~ "Moldova",
                                          country_name == "Bahamas" ~ "Bahamas, The",
                                          country_name == "Bolivia (Plurinational State of)" ~ "Bolivia",
                                          country_name == "Brunei Darussalam" ~ "Brunei",
                                          country_name == "Congo" ~ "Congo (Brazzaville)",    
                                          country_name == "Côte d'Ivoire" ~ "Cote d'Ivoire",
                                          country_name == "Curaçao" ~  "Curacao",
                                          country_name == "Dem. People's Republic of Korea" ~ "Korea, North",
                                          country_name == "Democratic Republic of the Congo" ~ "Congo (Kinshasa)",
                                          country_name == "Gambia" ~ "Gambia, The",
                                          country_name == "Iran (Islamic Republic of)" ~ "Iran",
                                          country_name == "Lao People's Democratic Republic" ~ "Laos",
                                          country_name == "Micronesia (Fed. States of)" ~ "Micronesia, Federated States of",
                                          country_name == "Myanmar" ~ "Burma",
                                          country_name == "Republic of Korea" ~ "Korea, South",
                                          country_name == "State of Palestine" ~ "West Bank and Gaza",
                                          country_name == "Syrian Arab Republic" ~ "Syria",
                                          country_name == "United Republic of Tanzania" ~ "Tanzania",
                                          country_name == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
                                          country_name == "Viet Nam" ~ "Vietnam",
                                          TRUE ~ country_name))


#//////////////////


# inspect

un_annual_net_migration_per_100_population
un_annual_net_migration_per_100_population %>% glimpse()
un_annual_net_migration_per_100_population %>% nrow() # 17064
un_annual_net_migration_per_100_population %>% ncol() # 6


# check country
un_annual_net_migration_per_100_population %>% count(year) %>% arrange(desc(year))
un_annual_net_migration_per_100_population %>% count(country_name) %>% print(n = nrow(.)) # 235


# check missing values
# note that "..." is missing data placeholder
un_annual_net_migration_per_100_population %>%
        filter(is.na(annual_net_migration_per_100_population_num) & !is.na(annual_net_migration_per_100_population)) %>%
        count(annual_net_migration_per_100_population)

# note the UN does not have data for kosovo
un_annual_net_migration_per_100_population %>% anti_join(., country_crosswalk,
                       by = c("country_name" = "country")) %>% count(country_name) %>% print(n = nrow(.))
country_crosswalk %>%
        anti_join(., un_annual_net_migration_per_100_population, by = c("country" = "country_name"))
country_crosswalk %>%
        anti_join(., un_annual_net_migration_per_100_population, by = "iso3")

country_crosswalk %>% filter(str_detect(string = country, pattern = regex("tai", ignore_case = TRUE))) %>% count(country)


#/////////////////////////////////////////////////////////////////////////////////////////////


un_annual_net_migration_per_100_population <- un_annual_net_migration_per_100_population %>% 
        select(-country_name) %>%
        left_join(., country_crosswalk,
                  by = "iso3") %>% 
        relocate(c(var, values), .after = everything())


#/////////////


# inspect
un_annual_net_migration_per_100_population
un_annual_net_migration_per_100_population %>% glimpse()
un_annual_net_migration_per_100_population %>% nrow() # 17064
un_annual_net_migration_per_100_population %>% ncol() # 18

un_annual_net_migration_per_100_population %>% count(year) %>% print(n = nrow(.))
un_annual_net_migration_per_100_population %>% count(country_name)

# inspect missing for e&E
# note that bih and azerbaijan are missing recent data; others are out of date

# check most recent year of data availability
un_annual_net_migration_per_100_population %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(!is.na(values)) %>%
        group_by(country) %>%
        arrange(desc(year)) %>%
        slice(1) %>%
        ungroup() %>%
        select(country, year) %>% arrange(year)

# plot missing data
un_annual_net_migration_per_100_population %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        select(country, year, values) %>%
        group_by(country, year) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", variable, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )


# plot
un_annual_net_migration_per_100_population %>%
        # filter(mcp_grouping == "E&E Balkans") %>%
        filter(mcp_grouping == "E&E Eurasia") %>%
        filter(year >= 2010) %>%
        select(country, year, values) %>%
        ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + 
        geom_line(size = 1)


#/////////////////////////////////////////////////////////////////////////////////////////////


# test values

test_un_annual_net_migration_per_100_population_values <- function() {
        
        # 1
        expect_equal(object = un_annual_net_migration_per_100_population %>% 
                             filter(country == "Serbia", year == 2021) %>% 
                             pull(values),
                     expected = 0.033)
        
        # 2
        expect_equal(object = un_annual_net_migration_per_100_population %>% 
                             filter(country == "Kosovo", year == 2020) %>% 
                             pull(values),
                     expected = -8.968)
        
        # 3
        expect_equal(object = un_annual_net_migration_per_100_population %>% 
                             filter(country == "Ukraine", year == 2019) %>% 
                             pull(values),
                     expected = -0.177)
}

test_un_annual_net_migration_per_100_population_values()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# un_annual_net_migration_per_100_population %>% write_csv(file = "data/un/un_annual_net_migration_per_100_population_20230115.csv")
un_annual_net_migration_per_100_population <- read_csv(file = "data/un/un_annual_net_migration_per_100_population_20230115.csv")

# inspect
un_annual_net_migration_per_100_population
un_annual_net_migration_per_100_population %>% glimpse()
un_annual_net_migration_per_100_population %>% nrow() # 17064
un_annual_net_migration_per_100_population %>% ncol() # 18


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# world bank poverty headcount rate at $3.20 (2011 PPP) ####

# https://data.worldbank.org/indicator/SI.POV.LMIC

poverty <- read_excel(path = "data/world_bank/API_SI.POV.LMIC_DS2_en_excel_v2_4359324.xls", sheet = "Data", skip = 3) %>%
        rename(country_name = `Country Name`, iso3 = `Country Code`) %>%
        select(-c(`Indicator Name`, `Indicator Code`)) %>%
        pivot_longer(cols = -c(country_name, iso3), names_to = "year", values_to = "poverty_rate") %>%
        mutate(year = as.numeric(year),
               country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "Brunei Darussalam" ~ "Brunei",
                                        country_name == "Myanmar" ~ "Burma",
                                        country_name == "Congo, Dem. Rep." ~ "Congo (Kinshasa)",
                                        country_name == "Congo, Rep." ~ "Congo (Brazzaville)",
                                        country_name == "Czech Republic" ~ "Czechia",
                                        country_name == "Egypt, Arab Rep." ~ "Egypt",
                                        country_name == "Iran, Islamic Rep." ~ "Iran",
                                        country_name == "Korea, Dem. People's Rep." ~ "Korea, North",
                                        country_name == "Korea, Rep." ~ "Korea, South",
                                        country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                        country_name == "Lao PDR" ~ "Laos",
                                        country_name == "Micronesia, Fed. Sts." ~ "Micronesia, Federated States of",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        country_name == "Russian Federation" ~ "Russia",
                                        country_name == "St. Kitts and Nevis" ~ "Saint Kitts and Nevis",
                                        country_name == "St. Vincent and the Grenadines" ~ "Saint Vincent and the Grenadines",
                                        country_name == "St. Lucia" ~ "Saint Lucia",
                                        country_name == "Slovak Republic" ~ "Slovakia",
                                        country_name == "Syrian Arab Republic" ~ "Syria",
                                        country_name == "Turkiye" ~ "Turkey",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States" ~ "U.S.",
                                        country_name == "Venezuela, RB" ~ "Venezuela",
                                        country_name == "Yemen, Rep." ~ "Yemen",
                                        TRUE ~ country_name)) %>%
        filter(year >= 2000) 


#/////////////


# inspect
poverty
poverty %>% glimpse()
poverty %>% nrow() # 5852
poverty %>% ncol() # 4

poverty %>% count(year) %>% print(n = nrow(.))
poverty %>% count(country_name)

# check names
country_crosswalk %>% anti_join(., poverty, by = c("country" = "country_name")) %>% count(country) %>% print(n = nrow(.))
poverty %>% filter(str_detect(string = country_name, pattern = regex("lucia", ignore_case = TRUE))) %>% distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("baham", ignore_case = TRUE)))


migrants %>% anti_join(., poverty, by = c("origin_country" = "country_name")) %>% count(origin_country) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# add country_crosswalk
poverty <- poverty %>% left_join(., country_crosswalk %>% select(-country), by = "iso3") %>% relocate(poverty_rate, .after = everything()) %>%
        rename(country = country_name)


#/////////////


# inspect
poverty
poverty %>% glimpse()
poverty %>% nrow() # 5852
poverty %>% ncol() # 15

poverty %>% count(year) %>% print(n = nrow(.))
poverty %>% count(country_name)

# inspect missing for e&E
# note that bih and azerbaijan are missing recent data; others are out of date

# check most recent year of data availability
poverty %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(!is.na(poverty_rate)) %>%
        group_by(country) %>%
        arrange(desc(year)) %>%
        slice(1) %>%
        ungroup() %>%
        select(country, year) %>% arrange(year)

# plot missing data
poverty %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        select(country, year, poverty_rate) %>%
        group_by(country, year) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", variable, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )


# plot
poverty %>%
        filter(mcp_grouping == "E&E Balkans") %>%
        filter(year >= 2010) %>%
        select(country, year, poverty_rate) %>%
        ggplot(data = ., mapping = aes(x = year, y = poverty_rate, color = country)) + geom_line()


#/////////////////////////////////////////////////////////////////////////////////////////////


# test values

test_poverty_values <- function() {
        
        # 1
        expect_equal(object = poverty %>% filter(country == "Serbia", year == 2018) %>% pull(poverty_rate),
                     expected = 5.9)
        
        # 2
        expect_equal(object = poverty %>% filter(country == "Kosovo", year == 2017) %>% pull(poverty_rate),
                     expected = 3.6)
        
        # 3
        expect_equal(object = poverty %>% filter(country == "Kosovo", year == 2018) %>% pull(poverty_rate),
                     expected = NA_real_)
}

test_poverty_values()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# poverty %>% write_csv(file = "data/world_bank/poverty_20220805.csv")
poverty <- read_csv(file = "data/world_bank/poverty_20220805.csv")

# inspect
poverty
poverty %>% glimpse()
poverty %>% nrow() # 5852
poverty %>% ncol() # 15


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# world bank goods_and_services_exports ####

# https://data.worldbank.org/indicator/NE.EXP.GNFS.ZS

goods_and_services_exports <- read_excel(path = "data/world_bank/API_NE.EXP.GNFS.ZS_DS2_en_excel_v2_4355515.xls", sheet = "Data", skip = 3) %>%
        rename(country_name = `Country Name`, iso3 = `Country Code`) %>%
        select(-c(`Indicator Name`, `Indicator Code`)) %>%
        pivot_longer(cols = -c(country_name, iso3), names_to = "year", values_to = "goods_and_services_exports_as_share_of_gdp") %>%
        mutate(year = as.numeric(year),
               country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "Brunei Darussalam" ~ "Brunei",
                                        country_name == "Myanmar" ~ "Burma",
                                        country_name == "Congo, Dem. Rep." ~ "Congo (Kinshasa)",
                                        country_name == "Congo, Rep." ~ "Congo (Brazzaville)",
                                        country_name == "Czech Republic" ~ "Czechia",
                                        country_name == "Egypt, Arab Rep." ~ "Egypt",
                                        country_name == "Iran, Islamic Rep." ~ "Iran",
                                        country_name == "Korea, Dem. People's Rep." ~ "Korea, North",
                                        country_name == "Korea, Rep." ~ "Korea, South",
                                        country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                        country_name == "Lao PDR" ~ "Laos",
                                        country_name == "Micronesia, Fed. Sts." ~ "Micronesia, Federated States of",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        country_name == "Russian Federation" ~ "Russia",
                                        country_name == "St. Kitts and Nevis" ~ "Saint Kitts and Nevis",
                                        country_name == "St. Vincent and the Grenadines" ~ "Saint Vincent and the Grenadines",
                                        country_name == "St. Lucia" ~ "Saint Lucia",
                                        country_name == "Slovak Republic" ~ "Slovakia",
                                        country_name == "Syrian Arab Republic" ~ "Syria",
                                        country_name == "Turkiye" ~ "Turkey",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States" ~ "U.S.",
                                        country_name == "Venezuela, RB" ~ "Venezuela",
                                        country_name == "Yemen, Rep." ~ "Yemen",
                                        TRUE ~ country_name)) %>%
        filter(year >= 2000) 


#/////////////


# inspect
goods_and_services_exports
goods_and_services_exports %>% glimpse()
goods_and_services_exports %>% nrow() # 5852
goods_and_services_exports %>% ncol() # 4

goods_and_services_exports %>% count(year) %>% print(n = nrow(.))
goods_and_services_exports %>% count(country_name)

# check names
country_crosswalk %>% anti_join(., goods_and_services_exports, by = c("country" = "country_name")) %>% count(country) %>% print(n = nrow(.))
goods_and_services_exports %>% filter(str_detect(string = country_name, pattern = regex("lucia", ignore_case = TRUE))) %>% distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("baham", ignore_case = TRUE)))


migrants %>% anti_join(., goods_and_services_exports, by = c("origin_country" = "country_name")) %>% count(origin_country) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# add country_crosswalk
goods_and_services_exports <- goods_and_services_exports %>% left_join(., country_crosswalk %>% select(-country), by = "iso3") %>% 
        relocate(goods_and_services_exports_as_share_of_gdp, .after = everything()) %>%
        rename(country = country_name)


#/////////////


# inspect
goods_and_services_exports
goods_and_services_exports %>% glimpse()
goods_and_services_exports %>% nrow() # 5852
goods_and_services_exports %>% ncol() # 15

goods_and_services_exports %>% count(year) %>% print(n = nrow(.))
goods_and_services_exports %>% count(country_name)

# inspect missing for e&E

# check most recent year of data availability
goods_and_services_exports %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(!is.na(goods_and_services_exports_as_share_of_gdp)) %>%
        group_by(country) %>%
        arrange(desc(year)) %>%
        slice(1) %>%
        ungroup() %>%
        select(country, year) %>% arrange(year)

# plot missing data
goods_and_services_exports %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        select(country, year, goods_and_services_exports_as_share_of_gdp) %>%
        group_by(country, year) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", variable, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )


# plot
goods_and_services_exports %>%
        filter(mcp_grouping == "E&E Balkans") %>%
        filter(year >= 2010) %>%
        select(country, year, goods_and_services_exports_as_share_of_gdp) %>%
        ggplot(data = ., mapping = aes(x = year, y = goods_and_services_exports_as_share_of_gdp, color = country)) + geom_line()


#/////////////////////////////////////////////////////////////////////////////////////////////


# test values

test_goods_and_services_exports_values <- function() {
        
        # 1
        expect_equal(object = goods_and_services_exports %>% filter(country == "Albania", year == 2014) %>% 
                             pull(goods_and_services_exports_as_share_of_gdp),
                     expected = 28.2130014584625)
        
        # 2
        expect_equal(object = goods_and_services_exports %>% filter(country == "Ukraine", year == 2021) %>% 
                             pull(goods_and_services_exports_as_share_of_gdp),
                     expected = 40.748673797626)
        
        # 3
        expect_equal(object = goods_and_services_exports %>% filter(country == "Belarus", year == 2017) %>% 
                             pull(goods_and_services_exports_as_share_of_gdp),
                     expected = 66.7896002012327)
}

test_goods_and_services_exports_values()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# goods_and_services_exports %>% write_csv(file = "data/world_bank/goods_and_services_exports_20220805.csv")
goods_and_services_exports <- read_csv(file = "data/world_bank/goods_and_services_exports_20220805.csv")

# inspect
goods_and_services_exports
goods_and_services_exports %>% glimpse()
goods_and_services_exports %>% nrow() # 5852
goods_and_services_exports %>% ncol() # 15


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# world bank goods_and_services_imports ####

# https://data.worldbank.org/indicator/NE.IMP.GNFS.ZS

goods_and_services_imports <- read_excel(path = "data/world_bank/API_NE.IMP.GNFS.ZS_DS2_en_excel_v2_4353198.xls", sheet = "Data", skip = 3) %>%
        rename(country_name = `Country Name`, iso3 = `Country Code`) %>%
        select(-c(`Indicator Name`, `Indicator Code`)) %>%
        pivot_longer(cols = -c(country_name, iso3), names_to = "year", values_to = "goods_and_services_imports_as_share_of_gdp") %>%
        mutate(year = as.numeric(year),
               country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "Brunei Darussalam" ~ "Brunei",
                                        country_name == "Myanmar" ~ "Burma",
                                        country_name == "Congo, Dem. Rep." ~ "Congo (Kinshasa)",
                                        country_name == "Congo, Rep." ~ "Congo (Brazzaville)",
                                        country_name == "Czech Republic" ~ "Czechia",
                                        country_name == "Egypt, Arab Rep." ~ "Egypt",
                                        country_name == "Iran, Islamic Rep." ~ "Iran",
                                        country_name == "Korea, Dem. People's Rep." ~ "Korea, North",
                                        country_name == "Korea, Rep." ~ "Korea, South",
                                        country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                        country_name == "Lao PDR" ~ "Laos",
                                        country_name == "Micronesia, Fed. Sts." ~ "Micronesia, Federated States of",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        country_name == "Russian Federation" ~ "Russia",
                                        country_name == "St. Kitts and Nevis" ~ "Saint Kitts and Nevis",
                                        country_name == "St. Vincent and the Grenadines" ~ "Saint Vincent and the Grenadines",
                                        country_name == "St. Lucia" ~ "Saint Lucia",
                                        country_name == "Slovak Republic" ~ "Slovakia",
                                        country_name == "Syrian Arab Republic" ~ "Syria",
                                        country_name == "Turkiye" ~ "Turkey",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States" ~ "U.S.",
                                        country_name == "Venezuela, RB" ~ "Venezuela",
                                        country_name == "Yemen, Rep." ~ "Yemen",
                                        TRUE ~ country_name)) %>%
        filter(year >= 2000) 


#/////////////


# inspect
goods_and_services_imports
goods_and_services_imports %>% glimpse()
goods_and_services_imports %>% nrow() # 5852
goods_and_services_imports %>% ncol() # 4

goods_and_services_imports %>% count(year) %>% print(n = nrow(.))
goods_and_services_imports %>% count(country_name)

# check names
country_crosswalk %>% anti_join(., goods_and_services_imports, by = c("country" = "country_name")) %>% count(country) %>% print(n = nrow(.))
goods_and_services_imports %>% filter(str_detect(string = country_name, pattern = regex("lucia", ignore_case = TRUE))) %>% distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("baham", ignore_case = TRUE)))


migrants %>% anti_join(., goods_and_services_imports, by = c("origin_country" = "country_name")) %>% count(origin_country) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# add country_crosswalk
goods_and_services_imports <- goods_and_services_imports %>% left_join(., country_crosswalk %>% select(-country), by = "iso3") %>% 
        relocate(goods_and_services_imports_as_share_of_gdp, .after = everything()) %>%
        rename(country = country_name)


#/////////////


# inspect
goods_and_services_imports
goods_and_services_imports %>% glimpse()
goods_and_services_imports %>% nrow() # 5852
goods_and_services_imports %>% ncol() # 15

goods_and_services_imports %>% count(year) %>% print(n = nrow(.))
goods_and_services_imports %>% count(country_name)

# inspect missing for e&E

# check most recent year of data availability
goods_and_services_imports %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(!is.na(goods_and_services_imports_as_share_of_gdp)) %>%
        group_by(country) %>%
        arrange(desc(year)) %>%
        slice(1) %>%
        ungroup() %>%
        select(country, year) %>% arrange(year)

# plot missing data
goods_and_services_imports %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        select(country, year, goods_and_services_imports_as_share_of_gdp) %>%
        group_by(country, year) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", variable, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )


# plot
goods_and_services_imports %>%
        filter(mcp_grouping == "E&E Balkans") %>%
        filter(year >= 2010) %>%
        select(country, year, goods_and_services_imports_as_share_of_gdp) %>%
        ggplot(data = ., mapping = aes(x = year, y = goods_and_services_imports_as_share_of_gdp, color = country)) + geom_line()


#/////////////////////////////////////////////////////////////////////////////////////////////


# test values

test_goods_and_services_imports_values <- function() {
        
        # 1
        expect_equal(object = goods_and_services_imports %>% filter(country == "Albania", year == 2014) %>% 
                             pull(goods_and_services_imports_as_share_of_gdp),
                     expected = 47.19484413802)
        
        # 2
        expect_equal(object = goods_and_services_imports %>% filter(country == "Ukraine", year == 2021) %>% 
                             pull(goods_and_services_imports_as_share_of_gdp),
                     expected = 41.8726259594613)
        
        # 3
        expect_equal(object = goods_and_services_imports %>% filter(country == "Belarus", year == 2017) %>% 
                             pull(goods_and_services_imports_as_share_of_gdp),
                     expected = 66.5781545217791)
}

test_goods_and_services_imports_values()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# goods_and_services_imports %>% write_csv(file = "data/world_bank/goods_and_services_imports_20220805.csv")
goods_and_services_imports <- read_csv(file = "data/world_bank/goods_and_services_imports_20220805.csv")

# inspect
goods_and_services_imports
goods_and_services_imports %>% glimpse()
goods_and_services_imports %>% nrow() # 5852
goods_and_services_imports %>% ncol() # 15



#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# world bank gdp_per_capita ####

# https://data.worldbank.org/indicator/NE.IMP.GNFS.ZS

gdp_per_capita <- read_excel(path = "data/world_bank/API_NY.GDP.PCAP.PP.CD_DS2_en_excel_v2_4355406.xls", sheet = "Data", skip = 3) %>%
        rename(country_name = `Country Name`, iso3 = `Country Code`) %>%
        select(-c(`Indicator Name`, `Indicator Code`)) %>%
        pivot_longer(cols = -c(country_name, iso3), names_to = "year", values_to = "gdp_per_capita_ppp") %>%
        mutate(year = as.numeric(year),
               country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "Brunei Darussalam" ~ "Brunei",
                                        country_name == "Myanmar" ~ "Burma",
                                        country_name == "Congo, Dem. Rep." ~ "Congo (Kinshasa)",
                                        country_name == "Congo, Rep." ~ "Congo (Brazzaville)",
                                        country_name == "Czech Republic" ~ "Czechia",
                                        country_name == "Egypt, Arab Rep." ~ "Egypt",
                                        country_name == "Iran, Islamic Rep." ~ "Iran",
                                        country_name == "Korea, Dem. People's Rep." ~ "Korea, North",
                                        country_name == "Korea, Rep." ~ "Korea, South",
                                        country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                        country_name == "Lao PDR" ~ "Laos",
                                        country_name == "Micronesia, Fed. Sts." ~ "Micronesia, Federated States of",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        country_name == "Russian Federation" ~ "Russia",
                                        country_name == "St. Kitts and Nevis" ~ "Saint Kitts and Nevis",
                                        country_name == "St. Vincent and the Grenadines" ~ "Saint Vincent and the Grenadines",
                                        country_name == "St. Lucia" ~ "Saint Lucia",
                                        country_name == "Slovak Republic" ~ "Slovakia",
                                        country_name == "Syrian Arab Republic" ~ "Syria",
                                        country_name == "Turkiye" ~ "Turkey",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States" ~ "U.S.",
                                        country_name == "Venezuela, RB" ~ "Venezuela",
                                        country_name == "Yemen, Rep." ~ "Yemen",
                                        TRUE ~ country_name)) %>%
        filter(year >= 2000) 


#/////////////


# inspect
gdp_per_capita
gdp_per_capita %>% glimpse()
gdp_per_capita %>% nrow() # 5852
gdp_per_capita %>% ncol() # 4

gdp_per_capita %>% count(year) %>% print(n = nrow(.))
gdp_per_capita %>% count(country_name)

# check names
country_crosswalk %>% anti_join(., gdp_per_capita, by = c("country" = "country_name")) %>% count(country) %>% print(n = nrow(.))
gdp_per_capita %>% filter(str_detect(string = country_name, pattern = regex("lucia", ignore_case = TRUE))) %>% distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("baham", ignore_case = TRUE)))


migrants %>% anti_join(., gdp_per_capita, by = c("origin_country" = "country_name")) %>% count(origin_country) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# add country_crosswalk
gdp_per_capita <- gdp_per_capita %>% left_join(., country_crosswalk %>% select(-country), by = "iso3") %>% 
        relocate(gdp_per_capita_ppp, .after = everything()) %>%
        rename(country = country_name)


#/////////////


# inspect
gdp_per_capita
gdp_per_capita %>% glimpse()
gdp_per_capita %>% nrow() # 5852
gdp_per_capita %>% ncol() # 15

gdp_per_capita %>% count(year) %>% print(n = nrow(.))
gdp_per_capita %>% count(country_name)

# inspect missing for e&E

# check most recent year of data availability
gdp_per_capita %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(!is.na(gdp_per_capita_ppp)) %>%
        group_by(country) %>%
        arrange(desc(year)) %>%
        slice(1) %>%
        ungroup() %>%
        select(country, year) %>% arrange(year)

# plot missing data
gdp_per_capita %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        select(country, year, gdp_per_capita_ppp) %>%
        group_by(country, year) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", variable, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )


# plot
gdp_per_capita %>%
        filter(mcp_grouping == "E&E Balkans") %>%
        filter(year >= 2010) %>%
        select(country, year, gdp_per_capita_ppp) %>%
        ggplot(data = ., mapping = aes(x = year, y = gdp_per_capita_ppp, color = country)) + geom_line()


#/////////////////////////////////////////////////////////////////////////////////////////////


# test values

test_gdp_per_capita_values <- function() {
        
        # 1
        expect_equal(object = gdp_per_capita %>% filter(country == "Albania", year == 2015) %>% 
                             pull(gdp_per_capita_ppp),
                     expected = 11658.9055171112)
        
        # 2
        expect_equal(object = gdp_per_capita %>% filter(country == "Georgia", year == 2021) %>% 
                             pull(gdp_per_capita_ppp),
                     expected = 16997.1489389764)
        
        # 3
        expect_equal(object = gdp_per_capita %>% filter(country == "Belarus", year == 2018) %>% 
                             pull(gdp_per_capita_ppp),
                     expected = 19428.0074589115)
}

test_gdp_per_capita_values()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# gdp_per_capita %>% write_csv(file = "data/world_bank/gdp_per_capita_20220805.csv")
gdp_per_capita <- read_csv(file = "data/world_bank/gdp_per_capita_20220805.csv")

# inspect
gdp_per_capita
gdp_per_capita %>% glimpse()
gdp_per_capita %>% nrow() # 5852
gdp_per_capita %>% ncol() # 15


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# world bank female_unemployment ####

# https://data.worldbank.org/indicator/SL.UEM.TOTL.FE.NE.ZS

# note that this is national estimate of unemployment, not ilo modeled estimate
# for national estimate, there may be some differences in country definitions of labor force and unemployment
# but the within-country trend is arguably more important for country briefs
# also the ilo estimate, while possibly more consistent across countries, completely omits kosovo
# the two series look very similar for e&E countries
# https://data.worldbank.org/indicator/SL.UEM.TOTL.FE.ZS?locations=XK

female_unemployment <- read_excel(path = "data/world_bank/API_SL.UEM.TOTL.FE.NE.ZS_DS2_en_excel_v2_4357850.xls", sheet = "Data", skip = 3) %>%
        rename(country_name = `Country Name`, iso3 = `Country Code`) %>%
        select(-c(`Indicator Name`, `Indicator Code`)) %>%
        pivot_longer(cols = -c(country_name, iso3), names_to = "year", values_to = "female_unemployment_rate") %>%
        mutate(year = as.numeric(year),
               country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "Brunei Darussalam" ~ "Brunei",
                                        country_name == "Myanmar" ~ "Burma",
                                        country_name == "Congo, Dem. Rep." ~ "Congo (Kinshasa)",
                                        country_name == "Congo, Rep." ~ "Congo (Brazzaville)",
                                        country_name == "Czech Republic" ~ "Czechia",
                                        country_name == "Egypt, Arab Rep." ~ "Egypt",
                                        country_name == "Iran, Islamic Rep." ~ "Iran",
                                        country_name == "Korea, Dem. People's Rep." ~ "Korea, North",
                                        country_name == "Korea, Rep." ~ "Korea, South",
                                        country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                        country_name == "Lao PDR" ~ "Laos",
                                        country_name == "Micronesia, Fed. Sts." ~ "Micronesia, Federated States of",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        country_name == "Russian Federation" ~ "Russia",
                                        country_name == "St. Kitts and Nevis" ~ "Saint Kitts and Nevis",
                                        country_name == "St. Vincent and the Grenadines" ~ "Saint Vincent and the Grenadines",
                                        country_name == "St. Lucia" ~ "Saint Lucia",
                                        country_name == "Slovak Republic" ~ "Slovakia",
                                        country_name == "Syrian Arab Republic" ~ "Syria",
                                        country_name == "Turkiye" ~ "Turkey",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States" ~ "U.S.",
                                        country_name == "Venezuela, RB" ~ "Venezuela",
                                        country_name == "Yemen, Rep." ~ "Yemen",
                                        TRUE ~ country_name)) %>%
        filter(year >= 2000) 


#/////////////


# inspect
female_unemployment
female_unemployment %>% glimpse()
female_unemployment %>% nrow() # 5852
female_unemployment %>% ncol() # 4

female_unemployment %>% count(year) %>% print(n = nrow(.))
female_unemployment %>% count(country_name)

# check names
country_crosswalk %>% anti_join(., female_unemployment, by = c("country" = "country_name")) %>% count(country) %>% print(n = nrow(.))
female_unemployment %>% filter(str_detect(string = country_name, pattern = regex("lucia", ignore_case = TRUE))) %>% distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("baham", ignore_case = TRUE)))


migrants %>% anti_join(., female_unemployment, by = c("origin_country" = "country_name")) %>% count(origin_country) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# add country_crosswalk
female_unemployment <- female_unemployment %>% left_join(., country_crosswalk %>% select(-country), by = "iso3") %>% 
        relocate(female_unemployment_rate, .after = everything()) %>%
        rename(country = country_name)


#/////////////


# inspect
female_unemployment
female_unemployment %>% glimpse()
female_unemployment %>% nrow() # 5852
female_unemployment %>% ncol() # 15

female_unemployment %>% count(year) %>% print(n = nrow(.))
female_unemployment %>% count(country_name)

# inspect missing for e&E

# check most recent year of data availability
# note that some are a few years old
female_unemployment %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(!is.na(female_unemployment_rate)) %>%
        group_by(country) %>%
        arrange(desc(year)) %>%
        slice(1) %>%
        ungroup() %>%
        select(country, year) %>% arrange(year)

# plot missing data
female_unemployment %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        select(country, year, female_unemployment_rate) %>%
        group_by(country, year) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", variable, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )


# plot
female_unemployment %>%
        filter(mcp_grouping == "E&E Balkans") %>%
        filter(year >= 2010) %>%
        select(country, year, female_unemployment_rate) %>%
        ggplot(data = ., mapping = aes(x = year, y = female_unemployment_rate, color = country)) + geom_line()


#/////////////////////////////////////////////////////////////////////////////////////////////


# test values

test_female_unemployment_values <- function() {
        
        # 1
        expect_equal(object = female_unemployment %>% filter(country == "Serbia", year == 2013) %>% 
                             pull(female_unemployment_rate),
                     expected = 23.8400001525879)
        
        # 2
        expect_equal(object = female_unemployment %>% filter(country == "Moldova", year == 2021) %>% 
                             pull(female_unemployment_rate),
                     expected = 2.53999996185303)
        
        # 3
        expect_equal(object = female_unemployment %>% filter(country == "Albania", year == 2015) %>% 
                             pull(female_unemployment_rate),
                     expected = 17.1200008392333)
}

test_female_unemployment_values()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# female_unemployment %>% write_csv(file = "data/world_bank/female_unemployment_20220805.csv")
female_unemployment <- read_csv(file = "data/world_bank/female_unemployment_20220805.csv")

# inspect
female_unemployment
female_unemployment %>% glimpse()
female_unemployment %>% nrow() # 5852
female_unemployment %>% ncol() # 15


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# world bank male_unemployment ####

# https://data.worldbank.org/indicator/SL.UEM.TOTL.FE.NE.ZS

# note that this is national estimate of unemployment, not ilo modeled estimate
# for national estimate, there may be some differences in country definitions of labor force and unemployment
# but the within-country trend is arguably more important for country briefs
# also the ilo estimate, while possibly more consistent across countries, completely omits kosovo
# the two series look very similar for e&E countries
# https://data.worldbank.org/indicator/SL.UEM.TOTL.FE.ZS?locations=XK

male_unemployment <- read_excel(path = "data/world_bank/API_SL.UEM.TOTL.MA.NE.ZS_DS2_en_excel_v2_4359609.xls", sheet = "Data", skip = 3) %>%
        rename(country_name = `Country Name`, iso3 = `Country Code`) %>%
        select(-c(`Indicator Name`, `Indicator Code`)) %>%
        pivot_longer(cols = -c(country_name, iso3), names_to = "year", values_to = "male_unemployment_rate") %>%
        mutate(year = as.numeric(year),
               country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "Brunei Darussalam" ~ "Brunei",
                                        country_name == "Myanmar" ~ "Burma",
                                        country_name == "Congo, Dem. Rep." ~ "Congo (Kinshasa)",
                                        country_name == "Congo, Rep." ~ "Congo (Brazzaville)",
                                        country_name == "Czech Republic" ~ "Czechia",
                                        country_name == "Egypt, Arab Rep." ~ "Egypt",
                                        country_name == "Iran, Islamic Rep." ~ "Iran",
                                        country_name == "Korea, Dem. People's Rep." ~ "Korea, North",
                                        country_name == "Korea, Rep." ~ "Korea, South",
                                        country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                        country_name == "Lao PDR" ~ "Laos",
                                        country_name == "Micronesia, Fed. Sts." ~ "Micronesia, Federated States of",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        country_name == "Russian Federation" ~ "Russia",
                                        country_name == "St. Kitts and Nevis" ~ "Saint Kitts and Nevis",
                                        country_name == "St. Vincent and the Grenadines" ~ "Saint Vincent and the Grenadines",
                                        country_name == "St. Lucia" ~ "Saint Lucia",
                                        country_name == "Slovak Republic" ~ "Slovakia",
                                        country_name == "Syrian Arab Republic" ~ "Syria",
                                        country_name == "Turkiye" ~ "Turkey",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States" ~ "U.S.",
                                        country_name == "Venezuela, RB" ~ "Venezuela",
                                        country_name == "Yemen, Rep." ~ "Yemen",
                                        TRUE ~ country_name)) %>%
        filter(year >= 2000) 


#/////////////


# inspect
male_unemployment
male_unemployment %>% glimpse()
male_unemployment %>% nrow() # 5852
male_unemployment %>% ncol() # 4

male_unemployment %>% count(year) %>% print(n = nrow(.))
male_unemployment %>% count(country_name)

# check names
country_crosswalk %>% anti_join(., male_unemployment, by = c("country" = "country_name")) %>% count(country) %>% print(n = nrow(.))
male_unemployment %>% filter(str_detect(string = country_name, pattern = regex("lucia", ignore_case = TRUE))) %>% distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("baham", ignore_case = TRUE)))


migrants %>% anti_join(., male_unemployment, by = c("origin_country" = "country_name")) %>% count(origin_country) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# add country_crosswalk
male_unemployment <- male_unemployment %>% left_join(., country_crosswalk %>% select(-country), by = "iso3") %>% 
        relocate(male_unemployment_rate, .after = everything()) %>%
        rename(country = country_name)


#/////////////


# inspect
male_unemployment
male_unemployment %>% glimpse()
male_unemployment %>% nrow() # 5852
male_unemployment %>% ncol() # 15

male_unemployment %>% count(year) %>% print(n = nrow(.))
male_unemployment %>% count(country_name)

# inspect missing for e&E

# check most recent year of data availability
# note that some are a few years old
male_unemployment %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(!is.na(male_unemployment_rate)) %>%
        group_by(country) %>%
        arrange(desc(year)) %>%
        slice(1) %>%
        ungroup() %>%
        select(country, year) %>% arrange(year)

# plot missing data
male_unemployment %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        select(country, year, male_unemployment_rate) %>%
        group_by(country, year) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", variable, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )


# plot
male_unemployment %>%
        filter(mcp_grouping == "E&E Balkans") %>%
        filter(year >= 2010) %>%
        select(country, year, male_unemployment_rate) %>%
        ggplot(data = ., mapping = aes(x = year, y = male_unemployment_rate, color = country)) + geom_line()


#/////////////////////////////////////////////////////////////////////////////////////////////


# test values

test_male_unemployment_values <- function() {
        
        # 1
        expect_equal(object = male_unemployment %>% filter(country == "Serbia", year == 2013) %>% 
                             pull(male_unemployment_rate),
                     expected = 20.8700008392333)
        
        # 2
        expect_equal(object = male_unemployment %>% filter(country == "Moldova", year == 2021) %>% 
                             pull(male_unemployment_rate),
                     expected = 3.84999990463257)
        
        # 3
        expect_equal(object = male_unemployment %>% filter(country == "Albania", year == 2015) %>% 
                             pull(male_unemployment_rate),
                     expected = 17.25)
}

test_male_unemployment_values()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# male_unemployment %>% write_csv(file = "data/world_bank/male_unemployment_20220805.csv")
male_unemployment <- read_csv(file = "data/world_bank/male_unemployment_20220805.csv")

# inspect
male_unemployment
male_unemployment %>% glimpse()
male_unemployment %>% nrow() # 5852
male_unemployment %>% ncol() # 15


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# world bank unemployment ####

# https://data.worldbank.org/indicator/SL.UEM.TOTL.NE.ZS

# note that this is national estimate of unemployment, not ilo modeled estimate
# for national estimate, there may be some differences in country definitions of labor force and unemployment
# but the within-country trend is arguably more important for country briefs
# also the ilo estimate, while possibly more consistent across countries, completely omits kosovo
# the two series look very similar for e&E countries
# https://data.worldbank.org/indicator/SL.UEM.TOTL.FE.ZS?locations=XK

unemployment <- read_excel(path = "data/world_bank/API_SL.UEM.TOTL.NE.ZS_DS2_en_excel_v2_4357642.xls", sheet = "Data", skip = 3) %>%
        rename(country_name = `Country Name`, iso3 = `Country Code`) %>%
        select(-c(`Indicator Name`, `Indicator Code`)) %>%
        pivot_longer(cols = -c(country_name, iso3), names_to = "year", values_to = "unemployment_rate") %>%
        mutate(year = as.numeric(year),
               country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "Brunei Darussalam" ~ "Brunei",
                                        country_name == "Myanmar" ~ "Burma",
                                        country_name == "Congo, Dem. Rep." ~ "Congo (Kinshasa)",
                                        country_name == "Congo, Rep." ~ "Congo (Brazzaville)",
                                        country_name == "Czech Republic" ~ "Czechia",
                                        country_name == "Egypt, Arab Rep." ~ "Egypt",
                                        country_name == "Iran, Islamic Rep." ~ "Iran",
                                        country_name == "Korea, Dem. People's Rep." ~ "Korea, North",
                                        country_name == "Korea, Rep." ~ "Korea, South",
                                        country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                        country_name == "Lao PDR" ~ "Laos",
                                        country_name == "Micronesia, Fed. Sts." ~ "Micronesia, Federated States of",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        country_name == "Russian Federation" ~ "Russia",
                                        country_name == "St. Kitts and Nevis" ~ "Saint Kitts and Nevis",
                                        country_name == "St. Vincent and the Grenadines" ~ "Saint Vincent and the Grenadines",
                                        country_name == "St. Lucia" ~ "Saint Lucia",
                                        country_name == "Slovak Republic" ~ "Slovakia",
                                        country_name == "Syrian Arab Republic" ~ "Syria",
                                        country_name == "Turkiye" ~ "Turkey",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States" ~ "U.S.",
                                        country_name == "Venezuela, RB" ~ "Venezuela",
                                        country_name == "Yemen, Rep." ~ "Yemen",
                                        TRUE ~ country_name)) %>%
        filter(year >= 2000) 


#/////////////


# inspect
unemployment
unemployment %>% glimpse()
unemployment %>% nrow() # 5852
unemployment %>% ncol() # 4

unemployment %>% count(year) %>% print(n = nrow(.))
unemployment %>% count(country_name)

# check names
country_crosswalk %>% anti_join(., unemployment, by = c("country" = "country_name")) %>% count(country) %>% print(n = nrow(.))
unemployment %>% filter(str_detect(string = country_name, pattern = regex("lucia", ignore_case = TRUE))) %>% distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("baham", ignore_case = TRUE)))


migrants %>% anti_join(., unemployment, by = c("origin_country" = "country_name")) %>% count(origin_country) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# add country_crosswalk
unemployment <- unemployment %>% left_join(., country_crosswalk %>% select(-country), by = "iso3") %>% 
        relocate(unemployment_rate, .after = everything()) %>%
        rename(country = country_name)


#/////////////


# inspect
unemployment
unemployment %>% glimpse()
unemployment %>% nrow() # 5852
unemployment %>% ncol() # 15

unemployment %>% count(year) %>% print(n = nrow(.))
unemployment %>% count(country_name)

# inspect missing for e&E

# check most recent year of data availability
# note that some are a few years old
unemployment %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(!is.na(unemployment_rate)) %>%
        group_by(country) %>%
        arrange(desc(year)) %>%
        slice(1) %>%
        ungroup() %>%
        select(country, year) %>% arrange(year)

# plot missing data
unemployment %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        select(country, year, unemployment_rate) %>%
        group_by(country, year) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", variable, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )


# plot
unemployment %>%
        filter(mcp_grouping == "E&E Balkans") %>%
        filter(year >= 2010) %>%
        select(country, year, unemployment_rate) %>%
        ggplot(data = ., mapping = aes(x = year, y = unemployment_rate, color = country)) + geom_line()


#/////////////////////////////////////////////////////////////////////////////////////////////


# test values

test_unemployment_values <- function() {
        
        # 1
        expect_equal(object = unemployment %>% filter(country == "Serbia", year == 2013) %>% 
                             pull(unemployment_rate),
                     expected = 22.1499996185303)
        
        # 2
        expect_equal(object = unemployment %>% filter(country == "Moldova", year == 2021) %>% 
                             pull(unemployment_rate),
                     expected = 3.23000001907349)
        
        # 3
        expect_equal(object = unemployment %>% filter(country == "Albania", year == 2015) %>% 
                             pull(unemployment_rate),
                     expected = 17.1900005340576)
}

test_unemployment_values()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# unemployment %>% write_csv(file = "data/world_bank/unemployment_20220805.csv")
unemployment <- read_csv(file = "data/world_bank/unemployment_20220805.csv")

# inspect
unemployment
unemployment %>% glimpse()
unemployment %>% nrow() # 5852
unemployment %>% ncol() # 15


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# world bank youth_unemployment ####

# https://data.worldbank.org/indicator/SL.UEM.TOTL.NE.ZS

# note that this is national estimate of youth_unemployment, not ilo modeled estimate
# for national estimate, there may be some differences in country definitions of labor force and youth_unemployment
# but the within-country trend is arguably more important for country briefs
# also the ilo estimate, while possibly more consistent across countries, completely omits kosovo
# the two series look very similar for e&E countries
# https://data.worldbank.org/indicator/SL.UEM.TOTL.FE.ZS?locations=XK

youth_unemployment <- read_excel(path = "data/world_bank/API_SL.UEM.1524.ZS_DS2_en_excel_v2_4770620.xls", sheet = "Data", skip = 3) %>%
        rename(country_name = `Country Name`, iso3 = `Country Code`) %>%
        select(-c(`Indicator Name`, `Indicator Code`)) %>%
        pivot_longer(cols = -c(country_name, iso3), names_to = "year", values_to = "youth_unemployment_rate") %>%
        mutate(year = as.numeric(year),
               country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "Brunei Darussalam" ~ "Brunei",
                                        country_name == "Myanmar" ~ "Burma",
                                        country_name == "Congo, Dem. Rep." ~ "Congo (Kinshasa)",
                                        country_name == "Congo, Rep." ~ "Congo (Brazzaville)",
                                        country_name == "Czech Republic" ~ "Czechia",
                                        country_name == "Egypt, Arab Rep." ~ "Egypt",
                                        country_name == "Iran, Islamic Rep." ~ "Iran",
                                        country_name == "Korea, Dem. People's Rep." ~ "Korea, North",
                                        country_name == "Korea, Rep." ~ "Korea, South",
                                        country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                        country_name == "Lao PDR" ~ "Laos",
                                        country_name == "Micronesia, Fed. Sts." ~ "Micronesia, Federated States of",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        country_name == "Russian Federation" ~ "Russia",
                                        country_name == "St. Kitts and Nevis" ~ "Saint Kitts and Nevis",
                                        country_name == "St. Vincent and the Grenadines" ~ "Saint Vincent and the Grenadines",
                                        country_name == "St. Lucia" ~ "Saint Lucia",
                                        country_name == "Slovak Republic" ~ "Slovakia",
                                        country_name == "Syrian Arab Republic" ~ "Syria",
                                        country_name == "Turkiye" ~ "Turkey",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States" ~ "U.S.",
                                        country_name == "Venezuela, RB" ~ "Venezuela",
                                        country_name == "Yemen, Rep." ~ "Yemen",
                                        TRUE ~ country_name)) %>%
        filter(year >= 2000) 


#/////////////


# inspect
youth_unemployment
youth_unemployment %>% glimpse()
youth_unemployment %>% nrow() # 5852
youth_unemployment %>% ncol() # 4

youth_unemployment %>% count(year) %>% print(n = nrow(.))
youth_unemployment %>% count(country_name)

# check names
country_crosswalk %>% anti_join(., youth_unemployment, by = c("country" = "country_name")) %>% count(country) %>% print(n = nrow(.))
youth_unemployment %>% filter(str_detect(string = country_name, pattern = regex("lucia", ignore_case = TRUE))) %>% distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("baham", ignore_case = TRUE)))


migrants %>% anti_join(., youth_unemployment, by = c("origin_country" = "country_name")) %>% count(origin_country) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# add country_crosswalk
youth_unemployment <- youth_unemployment %>% left_join(., country_crosswalk %>% select(-country), by = "iso3") %>% 
        relocate(youth_unemployment_rate, .after = everything()) %>%
        rename(country = country_name)


#/////////////


# inspect
youth_unemployment
youth_unemployment %>% glimpse()
youth_unemployment %>% nrow() # 5852
youth_unemployment %>% ncol() # 16

youth_unemployment %>% count(year) %>% print(n = nrow(.))
youth_unemployment %>% count(country_name)

# inspect missing for e&E

# check most recent year of data availability
# note that some are a few years old
youth_unemployment %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(!is.na(youth_unemployment_rate)) %>%
        group_by(country) %>%
        arrange(desc(year)) %>%
        slice(1) %>%
        ungroup() %>%
        select(country, year) %>% arrange(year)

# plot missing data
youth_unemployment %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        select(country, year, youth_unemployment_rate) %>%
        group_by(country, year) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", variable, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )


# plot
youth_unemployment %>%
        filter(mcp_grouping == "E&E Balkans") %>%
        filter(year >= 2010) %>%
        select(country, year, youth_unemployment_rate) %>%
        ggplot(data = ., mapping = aes(x = year, y = youth_unemployment_rate, color = country)) + geom_line()


#/////////////////////////////////////////////////////////////////////////////////////////////


# test values

test_youth_unemployment_values <- function() {
        
        # 1
        expect_equal(object = youth_unemployment %>% filter(country == "Albania", year == 2015) %>% 
                             pull(youth_unemployment_rate),
                     expected = 39.7799987792969)
        
        # 2
        expect_equal(object = youth_unemployment %>% filter(country == "Moldova", year == 2021) %>% 
                             pull(youth_unemployment_rate),
                     expected = 8.60099983215332)
        
        # 3
        expect_equal(object = youth_unemployment %>% filter(country == "Belarus", year == 2019) %>% 
                             pull(youth_unemployment_rate),
                     expected = 10.2080001831055)
}
test_youth_unemployment_values()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# youth_unemployment %>% write_csv(file = "data/world_bank/youth_unemployment_20230115.csv")
youth_unemployment <- read_csv(file = "data/world_bank/youth_unemployment_20220805.csv")

# inspect
youth_unemployment
youth_unemployment %>% glimpse()
youth_unemployment %>% nrow() # 5852
youth_unemployment %>% ncol() # 16


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# world bank wgi_gov_effectiveness ####

# note all variables must first be converted to character, since some have "#N/A" and some dont, 
# and pivot wont combine numeric and character columns
# note that wgi data skips 2001, so here 2001 is imputed as midpoint between 2000 and 2002 values

wgi_gov_effectiveness <- read_excel(path = "data/world_bank/wgidataset_20230115.xlsx", sheet = "GovernmentEffectiveness", skip = 14) %>%
        rename(country_name = `Country/Territory`) %>%
        select(-Code) %>% mutate_all(.funs = as.character) %>%
        pivot_longer(cols = -country_name, names_to = "var", values_to = "values") %>%
        mutate(var = case_when(str_detect(string = var, pattern = "^Estimate") ~ "estimate",
                               TRUE ~ var)) %>%
        filter(var == "estimate") %>% 
        mutate(year = rep(c(1996, 1998, 2000, seq(from = 2002, to = 2021, by = 1)), times = 214),
               values = case_when(values == "#N/A" ~ NA_character_, 
                                  TRUE ~ values),
               values = as.numeric(values),
               country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "Czech Republic" ~ "Czechia",
                                        country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        country_name == "Russian Federation" ~ "Russia",
                                        country_name == "Slovak Republic" ~ "Slovakia",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States" ~ "U.S.",
                                        TRUE ~ country_name),
               high_value_is_good_outcome_flag = 1,
               indicator_name = "wgi_gov_effectiveness") %>%
        pivot_wider(names_from = year, values_from = values) %>%
        mutate(`2001` = ((`2002` - `2000`) / 2) + `2000`) %>%
        pivot_longer(cols = -c(country_name, var, high_value_is_good_outcome_flag, indicator_name), 
                     names_to = "year", values_to = "values") %>%
        mutate(year = as.numeric(year)) %>%
        filter(year >= 2001)


#/////////////


# inspect
wgi_gov_effectiveness
wgi_gov_effectiveness %>% glimpse()
wgi_gov_effectiveness %>% nrow() # 4494
wgi_gov_effectiveness %>% ncol() # 6

wgi_gov_effectiveness %>% count(year) %>% print(n = nrow(.))
wgi_gov_effectiveness %>% count(country_name)

# check names
country_crosswalk %>% anti_join(., wgi_gov_effectiveness, by = c("country" = "country_name")) %>% count(country) %>% print(n = nrow(.))
wgi_gov_effectiveness %>% filter(str_detect(string = country_name, pattern = regex("lucia", ignore_case = TRUE))) %>% distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("baham", ignore_case = TRUE)))


migrants %>% anti_join(., wgi_gov_effectiveness, by = c("origin_country" = "country_name")) %>% count(origin_country) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# add country_crosswalk
wgi_gov_effectiveness <- wgi_gov_effectiveness %>% 
        left_join(., country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S."),
                  by = c("country_name" = "country")) %>% 
        relocate(var, indicator_name, values, .after = everything()) %>%
        rename(country = country_name)


#/////////////


# inspect
wgi_gov_effectiveness
wgi_gov_effectiveness %>% glimpse()
wgi_gov_effectiveness %>% nrow() # 4494
wgi_gov_effectiveness %>% ncol() # 19

wgi_gov_effectiveness %>% count(year) %>% print(n = nrow(.))
wgi_gov_effectiveness %>% count(country_name)

# inspect missing for e&E

# check most recent year of data availability
wgi_gov_effectiveness %>% 
        select(country, mcp_grouping, year, indicator_name, values) %>%
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(!is.na(var)) %>%
        group_by(country) %>%
        arrange(desc(year)) %>%
        slice(1) %>%
        ungroup() %>%
        select(country, year) %>% arrange(year)

# plot missing data
wgi_gov_effectiveness %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        select(country, year, values) %>%
        group_by(country, year) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", variable, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )


# plot
wgi_gov_effectiveness %>%
        filter(mcp_grouping == "E&E Balkans") %>%
        filter(year >= 2010) %>%
        select(country, year, values) %>%
        ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line()


#/////////////////////////////////////////////////////////////////////////////////////////////


# test values

test_wgi_gov_effectiveness_values <- function() {
        
        # 1
        expect_equal(object = wgi_gov_effectiveness %>% filter(country == "Serbia", year == 2020) %>% 
                             pull(values),
                     expected = -0.0102546475827694)
        
        # 2
        expect_equal(object = wgi_gov_effectiveness %>% filter(country == "Albania", year == 2018) %>% 
                             pull(values),
                     expected = 0.110513411462307)
        
        # 3
        expect_equal(object = wgi_gov_effectiveness %>% filter(country == "Kosovo", year == 2017) %>% 
                             pull(values),
                     expected = -0.387631714344025)
}

test_wgi_gov_effectiveness_values()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# wgi_gov_effectiveness %>% write_csv(file = "data/world_bank/wgi_gov_effectiveness_20230115.csv")
wgi_gov_effectiveness <- read_csv(file = "data/world_bank/wgi_gov_effectiveness_20230115.csv")

# inspect
wgi_gov_effectiveness
wgi_gov_effectiveness %>% glimpse()
wgi_gov_effectiveness %>% nrow() # 4494
wgi_gov_effectiveness %>% ncol() # 19


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# world bank wgi_reg_quality ####

# note all variables must first be converted to character, since some have "#N/A" and some dont, 
# and pivot wont combine numeric and character columns
# note that wgi data skips 2001, so here 2001 is imputed as midpoint between 2000 and 2002 values

wgi_reg_quality <- read_excel(path = "data/world_bank/wgidataset_20230115.xlsx", sheet = "RegulatoryQuality", skip = 14) %>%
        rename(country_name = `Country/Territory`) %>%
        select(-Code) %>% mutate_all(.funs = as.character) %>%
        pivot_longer(cols = -country_name, names_to = "var", values_to = "values") %>%
        mutate(var = case_when(str_detect(string = var, pattern = "^Estimate") ~ "estimate",
                               TRUE ~ var)) %>%
        filter(var == "estimate") %>% 
        mutate(year = rep(c(1996, 1998, 2000, seq(from = 2002, to = 2021, by = 1)), times = 214),
               values = case_when(values == "#N/A" ~ NA_character_, 
                                  TRUE ~ values),
               values = as.numeric(values),
               country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "Czech Republic" ~ "Czechia",
                                        country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        country_name == "Russian Federation" ~ "Russia",
                                        country_name == "Slovak Republic" ~ "Slovakia",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States" ~ "U.S.",
                                        TRUE ~ country_name),
               high_value_is_good_outcome_flag = 1,
               indicator_name = "wgi_reg_quality") %>%
        pivot_wider(names_from = year, values_from = values) %>%
        mutate(`2001` = ((`2002` - `2000`) / 2) + `2000`) %>%
        pivot_longer(cols = -c(country_name, var, high_value_is_good_outcome_flag, indicator_name), 
                     names_to = "year", values_to = "values") %>%
        mutate(year = as.numeric(year)) %>%
        filter(year >= 2001)


#/////////////


# inspect
wgi_reg_quality
wgi_reg_quality %>% glimpse()
wgi_reg_quality %>% nrow() # 4494
wgi_reg_quality %>% ncol() # 6

wgi_reg_quality %>% count(year) %>% print(n = nrow(.))
wgi_reg_quality %>% count(country_name)

# check names
country_crosswalk %>% anti_join(., wgi_reg_quality, by = c("country" = "country_name")) %>% count(country) %>% print(n = nrow(.))
wgi_reg_quality %>% filter(str_detect(string = country_name, pattern = regex("lucia", ignore_case = TRUE))) %>% distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("baham", ignore_case = TRUE)))


migrants %>% anti_join(., wgi_reg_quality, by = c("origin_country" = "country_name")) %>% count(origin_country) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# add country_crosswalk
wgi_reg_quality <- wgi_reg_quality %>% 
        left_join(., country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S."),
                  by = c("country_name" = "country")) %>% 
        relocate(var, indicator_name, values, .after = everything()) %>%
        rename(country = country_name)


#/////////////


# inspect
wgi_reg_quality
wgi_reg_quality %>% glimpse()
wgi_reg_quality %>% nrow() # 4280
wgi_reg_quality %>% ncol() # 18

wgi_reg_quality %>% count(year) %>% print(n = nrow(.))
wgi_reg_quality %>% count(country_name)

# inspect missing for e&E

# check most recent year of data availability
wgi_reg_quality %>% 
        select(country, mcp_grouping, year, indicator_name, values) %>%
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(!is.na(var)) %>%
        group_by(country) %>%
        arrange(desc(year)) %>%
        slice(1) %>%
        ungroup() %>%
        select(country, year) %>% arrange(year)

# plot missing data
wgi_reg_quality %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        select(country, year, values) %>%
        group_by(country, year) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", variable, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )


# plot
wgi_reg_quality %>%
        filter(mcp_grouping == "E&E Balkans") %>%
        filter(year >= 2010) %>%
        select(country, year, values) %>%
        ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line()


#/////////////////////////////////////////////////////////////////////////////////////////////


# test values

test_wgi_reg_quality_values <- function() {
        
        # 1
        expect_equal(object = wgi_reg_quality %>% filter(country == "Serbia", year == 2020) %>% 
                             pull(values),
                     expected = 0.0948551073670387)
        
        # 2
        expect_equal(object = wgi_reg_quality %>% filter(country == "Albania", year == 2018) %>% 
                             pull(values),
                     expected = 0.266459137201309)
        
        # 3
        expect_equal(object = wgi_reg_quality %>% filter(country == "Kosovo", year == 2017) %>% 
                             pull(values),
                     expected = -0.136985152959824)
}

test_wgi_reg_quality_values()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# wgi_reg_quality %>% write_csv(file = "data/world_bank/wgi_reg_quality_20230115.csv")
wgi_reg_quality <- read_csv(file = "data/world_bank/wgi_reg_quality_20230115.csv")

# inspect
wgi_reg_quality
wgi_reg_quality %>% glimpse()
wgi_reg_quality %>% nrow() # 4494
wgi_reg_quality %>% ncol() # 19



#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# world bank wgi_rule_of_law ####

# note all variables must first be converted to character, since some have "#N/A" and some dont, 
# and pivot wont combine numeric and character columns
# note that wgi data skips 2001, so here 2001 is imputed as midpoint between 2000 and 2002 values

wgi_rule_of_law <- read_excel(path = "data/world_bank/wgidataset_20230115.xlsx", sheet = "RuleofLaw", skip = 14) %>%
        rename(country_name = `Country/Territory`) %>%
        select(-Code) %>% mutate_all(.funs = as.character) %>%
        pivot_longer(cols = -country_name, names_to = "var", values_to = "values") %>%
        mutate(var = case_when(str_detect(string = var, pattern = "^Estimate") ~ "estimate",
                               TRUE ~ var)) %>%
        filter(var == "estimate") %>% 
        mutate(year = rep(c(1996, 1998, 2000, seq(from = 2002, to = 2021, by = 1)), times = 214),
               values = case_when(values == "#N/A" ~ NA_character_, 
                                  TRUE ~ values),
               values = as.numeric(values),
               country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "Czech Republic" ~ "Czechia",
                                        country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        country_name == "Russian Federation" ~ "Russia",
                                        country_name == "Slovak Republic" ~ "Slovakia",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States" ~ "U.S.",
                                        TRUE ~ country_name),
               high_value_is_good_outcome_flag = 1,
               indicator_name = "wgi_rule_of_law") %>%
        pivot_wider(names_from = year, values_from = values) %>%
        mutate(`2001` = ((`2002` - `2000`) / 2) + `2000`) %>%
        pivot_longer(cols = -c(country_name, var, high_value_is_good_outcome_flag, indicator_name), 
                     names_to = "year", values_to = "values") %>%
        mutate(year = as.numeric(year)) %>%
        filter(year >= 2001)


#/////////////


# inspect
wgi_rule_of_law
wgi_rule_of_law %>% glimpse()
wgi_rule_of_law %>% nrow() # 4494
wgi_rule_of_law %>% ncol() # 6

wgi_rule_of_law %>% count(year) %>% print(n = nrow(.))
wgi_rule_of_law %>% count(country_name)

# check names
country_crosswalk %>% anti_join(., wgi_rule_of_law, by = c("country" = "country_name")) %>% count(country) %>% print(n = nrow(.))
wgi_rule_of_law %>% filter(str_detect(string = country_name, pattern = regex("lucia", ignore_case = TRUE))) %>% distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("baham", ignore_case = TRUE)))


migrants %>% anti_join(., wgi_rule_of_law, by = c("origin_country" = "country_name")) %>% count(origin_country) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# add country_crosswalk
wgi_rule_of_law <- wgi_rule_of_law %>% 
        left_join(., country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S."),
                  by = c("country_name" = "country")) %>% 
        relocate(var, indicator_name, values, .after = everything()) %>%
        rename(country = country_name)


#/////////////


# inspect
wgi_rule_of_law
wgi_rule_of_law %>% glimpse()
wgi_rule_of_law %>% nrow() # 4494
wgi_rule_of_law %>% ncol() # 19

wgi_rule_of_law %>% count(year) %>% print(n = nrow(.))
wgi_rule_of_law %>% count(country_name)

# inspect missing for e&E

# check most recent year of data availability
wgi_rule_of_law %>% 
        select(country, mcp_grouping, year, indicator_name, values) %>%
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(!is.na(var)) %>%
        group_by(country) %>%
        arrange(desc(year)) %>%
        slice(1) %>%
        ungroup() %>%
        select(country, year) %>% arrange(year)

# plot missing data
wgi_rule_of_law %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        select(country, year, values) %>%
        group_by(country, year) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", variable, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )


# plot
wgi_rule_of_law %>%
        filter(mcp_grouping == "E&E Balkans") %>%
        filter(year >= 2010) %>%
        select(country, year, values) %>%
        ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line()


#/////////////////////////////////////////////////////////////////////////////////////////////


# test values

test_wgi_rule_of_law_values <- function() {
        
        # 1
        expect_equal(object = wgi_rule_of_law %>% filter(country == "Serbia", year == 2020) %>% 
                             pull(values),
                     expected = -0.100547984242439)
        
        # 2
        expect_equal(object = wgi_rule_of_law %>% filter(country == "Albania", year == 2018) %>% 
                             pull(values),
                     expected = -0.395619302988052)
        
        # 3
        expect_equal(object = wgi_rule_of_law %>% filter(country == "Kosovo", year == 2017) %>% 
                             pull(values),
                     expected = -0.416558086872101)
}

test_wgi_rule_of_law_values()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# wgi_rule_of_law %>% write_csv(file = "data/world_bank/wgi_rule_of_law_20230115.csv")
wgi_rule_of_law <- read_csv(file = "data/world_bank/wgi_rule_of_law_20230115.csv")

# inspect
wgi_rule_of_law
wgi_rule_of_law %>% glimpse()
wgi_rule_of_law %>% nrow() # 4494
wgi_rule_of_law %>% ncol() # 19


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# world bank wgi_corruption ####

# note all variables must first be converted to character, since some have "#N/A" and some dont, 
# and pivot wont combine numeric and character columns
# note that wgi data skips 2001, so here 2001 is imputed as midpoint between 2000 and 2002 values

wgi_corruption <- read_excel(path = "data/world_bank/wgidataset_20230115.xlsx", sheet = "ControlofCorruption", skip = 14) %>%
        rename(country_name = `Country/Territory`) %>%
        select(-Code) %>% mutate_all(.funs = as.character) %>%
        pivot_longer(cols = -country_name, names_to = "var", values_to = "values") %>%
        mutate(var = case_when(str_detect(string = var, pattern = "^Estimate") ~ "estimate",
                               TRUE ~ var)) %>%
        filter(var == "estimate") %>% 
        mutate(year = rep(c(1996, 1998, 2000, seq(from = 2002, to = 2021, by = 1)), times = 214),
               values = case_when(values == "#N/A" ~ NA_character_, 
                                  TRUE ~ values),
               values = as.numeric(values),
               country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "Czech Republic" ~ "Czechia",
                                        country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        country_name == "Russian Federation" ~ "Russia",
                                        country_name == "Slovak Republic" ~ "Slovakia",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States" ~ "U.S.",
                                        TRUE ~ country_name),
               high_value_is_good_outcome_flag = 1,
               indicator_name = "wgi_corruption") %>%
        pivot_wider(names_from = year, values_from = values) %>%
        mutate(`2001` = ((`2002` - `2000`) / 2) + `2000`) %>%
        pivot_longer(cols = -c(country_name, var, high_value_is_good_outcome_flag, indicator_name), 
                     names_to = "year", values_to = "values") %>%
        mutate(year = as.numeric(year)) %>%
        filter(year >= 2001)


#/////////////


# inspect
wgi_corruption
wgi_corruption %>% glimpse()
wgi_corruption %>% nrow() # 4494
wgi_corruption %>% ncol() # 6

wgi_corruption %>% count(year) %>% print(n = nrow(.))
wgi_corruption %>% count(country_name)

# check names
country_crosswalk %>% anti_join(., wgi_corruption, by = c("country" = "country_name")) %>% count(country) %>% print(n = nrow(.))
wgi_corruption %>% filter(str_detect(string = country_name, pattern = regex("lucia", ignore_case = TRUE))) %>% distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("baham", ignore_case = TRUE)))


migrants %>% anti_join(., wgi_corruption, by = c("origin_country" = "country_name")) %>% count(origin_country) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# add country_crosswalk
wgi_corruption <- wgi_corruption %>% 
        left_join(., country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S."),
                  by = c("country_name" = "country")) %>% 
        relocate(var, indicator_name, values, .after = everything()) %>%
        rename(country = country_name)


#/////////////


# inspect
wgi_corruption
wgi_corruption %>% glimpse()
wgi_corruption %>% nrow() # 4494
wgi_corruption %>% ncol() # 19

wgi_corruption %>% count(year) %>% print(n = nrow(.))
wgi_corruption %>% count(country_name)

# inspect missing for e&E

# check most recent year of data availability
wgi_corruption %>% 
        select(country, mcp_grouping, year, indicator_name, values) %>%
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(!is.na(var)) %>%
        group_by(country) %>%
        arrange(desc(year)) %>%
        slice(1) %>%
        ungroup() %>%
        select(country, year) %>% arrange(year)

# plot missing data
wgi_corruption %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        select(country, year, values) %>%
        group_by(country, year) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", variable, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )


# plot
wgi_corruption %>%
        filter(mcp_grouping == "E&E Balkans") %>%
        filter(year >= 2010) %>%
        select(country, year, values) %>%
        ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line()


#/////////////////////////////////////////////////////////////////////////////////////////////


# test values

test_wgi_corruption_values <- function() {
        
        # 1
        expect_equal(object = wgi_corruption %>% filter(country == "Serbia", year == 2020) %>% 
                             pull(values),
                     expected = -0.429459095001221)
        
        # 2
        expect_equal(object = wgi_corruption %>% filter(country == "Albania", year == 2018) %>% 
                             pull(values),
                     expected = -0.524145126342773)
        
        # 3
        expect_equal(object = wgi_corruption %>% filter(country == "Kosovo", year == 2017) %>% 
                             pull(values),
                     expected = -0.502136826515197)
}

test_wgi_corruption_values()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# wgi_corruption %>% write_csv(file = "data/world_bank/wgi_corruption_20230115.csv")
wgi_corruption <- read_csv(file = "data/world_bank/wgi_corruption_20230115.csv")

# inspect
wgi_corruption
wgi_corruption %>% glimpse()
wgi_corruption %>% nrow() # 4494
wgi_corruption %>% ncol() # 19



#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# world bank wgi_political_stability ####

# note all variables must first be converted to character, since some have "#N/A" and some dont, 
# and pivot wont combine numeric and character columns
# note that wgi data skips 2001, so here 2001 is imputed as midpoint between 2000 and 2002 values

wgi_political_stability <- read_excel(path = "data/world_bank/wgidataset_20230115.xlsx", sheet = "Political StabilityNoViolence", skip = 14) %>%
        rename(country_name = `Country/Territory`) %>%
        select(-Code) %>% mutate_all(.funs = as.character) %>%
        pivot_longer(cols = -country_name, names_to = "var", values_to = "values") %>%
        mutate(var = case_when(str_detect(string = var, pattern = "^Estimate") ~ "estimate",
                               TRUE ~ var)) %>%
        filter(var == "estimate") %>% 
        mutate(year = rep(c(1996, 1998, 2000, seq(from = 2002, to = 2021, by = 1)), times = 214),
               values = case_when(values == "#N/A" ~ NA_character_, 
                                  TRUE ~ values),
               values = as.numeric(values),
               country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "Czech Republic" ~ "Czechia",
                                        country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        country_name == "Russian Federation" ~ "Russia",
                                        country_name == "Slovak Republic" ~ "Slovakia",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States" ~ "U.S.",
                                        TRUE ~ country_name),
               high_value_is_good_outcome_flag = 1,
               indicator_name = "wgi_political_stability") %>%
        pivot_wider(names_from = year, values_from = values) %>%
        mutate(`2001` = ((`2002` - `2000`) / 2) + `2000`) %>%
        pivot_longer(cols = -c(country_name, var, high_value_is_good_outcome_flag, indicator_name), 
                     names_to = "year", values_to = "values") %>%
        mutate(year = as.numeric(year)) %>%
        filter(year >= 2001)


#/////////////


# inspect
wgi_political_stability
wgi_political_stability %>% glimpse()
wgi_political_stability %>% nrow() # 4494
wgi_political_stability %>% ncol() # 6

wgi_political_stability %>% count(year) %>% print(n = nrow(.))
wgi_political_stability %>% count(country_name)

# check names
country_crosswalk %>% anti_join(., wgi_political_stability, by = c("country" = "country_name")) %>% count(country) %>% print(n = nrow(.))
wgi_political_stability %>% filter(str_detect(string = country_name, pattern = regex("lucia", ignore_case = TRUE))) %>% distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("baham", ignore_case = TRUE)))


migrants %>% anti_join(., wgi_political_stability, by = c("origin_country" = "country_name")) %>% count(origin_country) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# add country_crosswalk
wgi_political_stability <- wgi_political_stability %>% 
        left_join(., country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S."),
                  by = c("country_name" = "country")) %>% 
        relocate(var, indicator_name, values, .after = everything()) %>%
        rename(country = country_name)


#/////////////


# inspect
wgi_political_stability
wgi_political_stability %>% glimpse()
wgi_political_stability %>% nrow() # 4494
wgi_political_stability %>% ncol() # 19

wgi_political_stability %>% count(year) %>% print(n = nrow(.))
wgi_political_stability %>% count(country_name)

# inspect missing for e&E

# check most recent year of data availability
wgi_political_stability %>% 
        select(country, mcp_grouping, year, indicator_name, values) %>%
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(!is.na(var)) %>%
        group_by(country) %>%
        arrange(desc(year)) %>%
        slice(1) %>%
        ungroup() %>%
        select(country, year) %>% arrange(year)

# plot missing data
wgi_political_stability %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        select(country, year, values) %>%
        group_by(country, year) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", variable, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )


# plot
wgi_political_stability %>%
        filter(mcp_grouping == "E&E Balkans") %>%
        filter(year >= 2010) %>%
        select(country, year, values) %>%
        ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line()


#/////////////////////////////////////////////////////////////////////////////////////////////


# test values

test_wgi_political_stability_values <- function() {
        
        # 1
        expect_equal(object = wgi_political_stability %>% filter(country == "Serbia", year == 2020) %>% 
                             pull(values),
                     expected = -0.163741484284401)
        
        # 2
        expect_equal(object = wgi_political_stability %>% filter(country == "Albania", year == 2018) %>% 
                             pull(values),
                     expected = 0.370515644550323)
        
        # 3
        expect_equal(object = wgi_political_stability %>% filter(country == "Kosovo", year == 2017) %>% 
                             pull(values),
                     expected = -0.246099919080734)
}

test_wgi_political_stability_values()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# wgi_political_stability %>% write_csv(file = "data/world_bank/wgi_political_stability_20230115.csv")
wgi_political_stability <- read_csv(file = "data/world_bank/wgi_political_stability_20230115.csv")

# inspect
wgi_political_stability
wgi_political_stability %>% glimpse()
wgi_political_stability %>% nrow() # 4494
wgi_political_stability %>% ncol() # 19



#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# world bank wgi_voice_accountability ####

# note all variables must first be converted to character, since some have "#N/A" and some dont, 
# and pivot wont combine numeric and character columns
# note that wgi data skips 2001, so here 2001 is imputed as midpoint between 2000 and 2002 values

wgi_voice_accountability <- read_excel(path = "data/world_bank/wgidataset_20230115.xlsx", sheet = "VoiceandAccountability", skip = 14) %>%
        rename(country_name = `Country/Territory`) %>%
        select(-Code) %>% mutate_all(.funs = as.character) %>%
        pivot_longer(cols = -country_name, names_to = "var", values_to = "values") %>%
        mutate(var = case_when(str_detect(string = var, pattern = "^Estimate") ~ "estimate",
                               TRUE ~ var)) %>%
        filter(var == "estimate") %>% 
        mutate(year = rep(c(1996, 1998, 2000, seq(from = 2002, to = 2021, by = 1)), times = 214),
               values = case_when(values == "#N/A" ~ NA_character_, 
                                  TRUE ~ values),
               values = as.numeric(values),
               country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "Czech Republic" ~ "Czechia",
                                        country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        country_name == "Russian Federation" ~ "Russia",
                                        country_name == "Slovak Republic" ~ "Slovakia",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States" ~ "U.S.",
                                        TRUE ~ country_name),
               high_value_is_good_outcome_flag = 1,
               indicator_name = "wgi_voice_accountability") %>%
        pivot_wider(names_from = year, values_from = values) %>%
        mutate(`2001` = ((`2002` - `2000`) / 2) + `2000`) %>%
        pivot_longer(cols = -c(country_name, var, high_value_is_good_outcome_flag, indicator_name), 
                     names_to = "year", values_to = "values") %>%
        mutate(year = as.numeric(year)) %>%
        filter(year >= 2001)


#/////////////


# inspect
wgi_voice_accountability
wgi_voice_accountability %>% glimpse()
wgi_voice_accountability %>% nrow() # 4494
wgi_voice_accountability %>% ncol() # 6

wgi_voice_accountability %>% count(year) %>% print(n = nrow(.))
wgi_voice_accountability %>% count(country_name)

# check names
country_crosswalk %>% anti_join(., wgi_voice_accountability, by = c("country" = "country_name")) %>% count(country) %>% print(n = nrow(.))
wgi_voice_accountability %>% filter(str_detect(string = country_name, pattern = regex("lucia", ignore_case = TRUE))) %>% distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("baham", ignore_case = TRUE)))


migrants %>% anti_join(., wgi_voice_accountability, by = c("origin_country" = "country_name")) %>% count(origin_country) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# add country_crosswalk
wgi_voice_accountability <- wgi_voice_accountability %>% 
        left_join(., country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S."),
                  by = c("country_name" = "country")) %>% 
        relocate(var, indicator_name, values, .after = everything()) %>%
        rename(country = country_name)


#/////////////


# inspect
wgi_voice_accountability
wgi_voice_accountability %>% glimpse()
wgi_voice_accountability %>% nrow() # 4494
wgi_voice_accountability %>% ncol() # 19

wgi_voice_accountability %>% count(year) %>% print(n = nrow(.))
wgi_voice_accountability %>% count(country_name)

# inspect missing for e&E

# check most recent year of data availability
wgi_voice_accountability %>% 
        select(country, mcp_grouping, year, indicator_name, values) %>%
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(!is.na(var)) %>%
        group_by(country) %>%
        arrange(desc(year)) %>%
        slice(1) %>%
        ungroup() %>%
        select(country, year) %>% arrange(year)

# plot missing data
wgi_voice_accountability %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        select(country, year, values) %>%
        group_by(country, year) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", variable, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )


# plot
wgi_voice_accountability %>%
        filter(mcp_grouping == "E&E Balkans") %>%
        filter(year >= 2010) %>%
        select(country, year, values) %>%
        ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line()


#/////////////////////////////////////////////////////////////////////////////////////////////


# test values

test_wgi_voice_accountability_values <- function() {
        
        # 1
        expect_equal(object = wgi_voice_accountability %>% filter(country == "Serbia", year == 2020) %>% 
                             pull(values),
                     expected = -0.119043312966824)
        
        # 2
        expect_equal(object = wgi_voice_accountability %>% filter(country == "Albania", year == 2018) %>% 
                             pull(values),
                     expected = 0.180729448795319)
        
        # 3
        expect_equal(object = wgi_voice_accountability %>% filter(country == "Kosovo", year == 2017) %>% 
                             pull(values),
                     expected = -0.115478247404099)
}

test_wgi_voice_accountability_values()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# wgi_voice_accountability %>% write_csv(file = "data/world_bank/wgi_voice_accountability_20230115.csv")
wgi_voice_accountability <- read_csv(file = "data/world_bank/wgi_voice_accountability_20230115.csv")

# inspect
wgi_voice_accountability
wgi_voice_accountability %>% glimpse()
wgi_voice_accountability %>% nrow() # 4494
wgi_voice_accountability %>% ncol() # 19


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# load vdem_electoral_democracy_index ####
vdem_electoral_democracy_index <- vdem %>% select(country_name, year, v2x_polyarchy) %>%
        filter(year >= 2001) %>% 
        rename(sub_obj_1_3_vdem_electoral_democracy = "v2x_polyarchy") %>%
        pivot_longer(cols = sub_obj_1_3_vdem_electoral_democracy, names_to = "indicator_name", values_to = "values") %>%
        mutate(high_value_is_good_outcome_flag = 1) %>%
        mutate(country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "Burma/Myanmar" ~ "Burma",
                                        country_name == "Cape Verde" ~ "Cabo Verde",
                                        country_name == "Czech Republic" ~ "Czechia",
                                        country_name == "Democratic Republic of the Congo" ~ "Congo (Kinshasa)",
                                        country_name == "Hong Kong" ~ "Hong Kong SAR, China",
                                        country_name == "Ivory Coast" ~ "Cote d'Ivoire",
                                        country_name == "North Korea" ~ "Korea, North",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        country_name == "Palestine/West Bank" ~ "West Bank and Gaza",
                                        country_name == "Papal States" ~ "Holy See",
                                        country_name == "Republic of the Congo" ~ "Congo (Brazzaville)",
                                        country_name == "Republic of Vietnam" ~ "Vietnam",
                                        country_name == "South Korea" ~ "Korea, South",
                                        country_name == "The Gambia" ~ "Gambia, The",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States of America" ~ "U.S.",
                                        TRUE ~ country_name))


#/////////////


# inspect
vdem_electoral_democracy_index
vdem_electoral_democracy_index %>% glimpse()
vdem_electoral_democracy_index %>% nrow() # 3743
vdem_electoral_democracy_index %>% ncol() # 5

vdem_electoral_democracy_index %>% count(year) %>% print(n = nrow(.))
vdem_electoral_democracy_index %>% count(country_name) 

# check names
country_crosswalk %>% anti_join(., vdem_electoral_democracy_index, by = c("country" = "country_name")) %>% count(country) %>% print(n = nrow(.))
vdem_electoral_democracy_index %>% filter(str_detect(string = country_name, pattern = regex("lucia", ignore_case = TRUE))) %>% distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("baham", ignore_case = TRUE)))


#/////////////////////////////////////////////////////////////////////////////////////////////


# add country_crosswalk
vdem_electoral_democracy_index <- vdem_electoral_democracy_index %>% 
        left_join(., country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S."),
                  by = c("country_name" = "country")) %>% 
        relocate(indicator_name, values, .after = everything()) %>%
        rename(country = country_name)


#/////////////


# inspect
vdem_electoral_democracy_index
vdem_electoral_democracy_index %>% glimpse()
vdem_electoral_democracy_index %>% nrow() # 3743
vdem_electoral_democracy_index %>% ncol() # 17

vdem_electoral_democracy_index %>% count(year) %>% print(n = nrow(.))
vdem_electoral_democracy_index %>% count(country_name)

# inspect missing for e&E

# check most recent year of data availability
vdem_electoral_democracy_index %>% 
        select(country, mcp_grouping, year, indicator_name, values) %>%
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(!is.na(var)) %>%
        group_by(country) %>%
        arrange(desc(year)) %>%
        slice(1) %>%
        ungroup() %>%
        select(country, year) %>% arrange(year)

# plot missing data
vdem_electoral_democracy_index %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        select(country, year, values) %>%
        group_by(country, year) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", variable, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )


# plot
vdem_electoral_democracy_index %>%
        filter(mcp_grouping == "E&E Balkans") %>%
        filter(year >= 2010) %>%
        select(country, year, values) %>%
        ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# vdem_electoral_democracy_index %>% write_csv(file = "data/vdem/vdem_electoral_democracy_index_20220805.csv")
vdem_electoral_democracy_index <- read_csv(file = "data/vdem/vdem_electoral_democracy_index_20220805.csv")

# inspect
vdem_electoral_democracy_index
vdem_electoral_democracy_index %>% glimpse()
vdem_electoral_democracy_index %>% nrow() # 3743
vdem_electoral_democracy_index %>% ncol() # 17


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# load vdem_liberal_democracy_index ####
vdem_liberal_democracy_index <- vdem %>% select(country_name, year, v2x_libdem) %>%
        filter(year >= 2001) %>% 
        rename(vdem_liberal_democracy = "v2x_libdem") %>%
        pivot_longer(cols = vdem_liberal_democracy, names_to = "indicator_name", values_to = "values") %>%
        mutate(high_value_is_good_outcome_flag = 1) %>%
        mutate(country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "Burma/Myanmar" ~ "Burma",
                                        country_name == "Cape Verde" ~ "Cabo Verde",
                                        country_name == "Czech Republic" ~ "Czechia",
                                        country_name == "Democratic Republic of the Congo" ~ "Congo (Kinshasa)",
                                        country_name == "Hong Kong" ~ "Hong Kong SAR, China",
                                        country_name == "Ivory Coast" ~ "Cote d'Ivoire",
                                        country_name == "North Korea" ~ "Korea, North",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        country_name == "Palestine/West Bank" ~ "West Bank and Gaza",
                                        country_name == "Papal States" ~ "Holy See",
                                        country_name == "Republic of the Congo" ~ "Congo (Brazzaville)",
                                        country_name == "Republic of Vietnam" ~ "Vietnam",
                                        country_name == "South Korea" ~ "Korea, South",
                                        country_name == "The Gambia" ~ "Gambia, The",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States of America" ~ "U.S.",
                                        TRUE ~ country_name))


#/////////////


# inspect
vdem_liberal_democracy_index
vdem_liberal_democracy_index %>% glimpse()
vdem_liberal_democracy_index %>% nrow() # 3743
vdem_liberal_democracy_index %>% ncol() # 5

vdem_liberal_democracy_index %>% count(year) %>% print(n = nrow(.))
vdem_liberal_democracy_index %>% count(country_name) 

# check names
country_crosswalk %>% anti_join(., vdem_liberal_democracy_index, by = c("country" = "country_name")) %>% count(country) %>% print(n = nrow(.))
vdem_liberal_democracy_index %>% filter(str_detect(string = country_name, pattern = regex("lucia", ignore_case = TRUE))) %>% distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("baham", ignore_case = TRUE)))


#/////////////////////////////////////////////////////////////////////////////////////////////


# add country_crosswalk
vdem_liberal_democracy_index <- vdem_liberal_democracy_index %>% 
        left_join(., country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S."),
                  by = c("country_name" = "country")) %>% 
        relocate(indicator_name, values, .after = everything()) %>%
        rename(country = country_name)


#/////////////


# inspect
vdem_liberal_democracy_index
vdem_liberal_democracy_index %>% glimpse()
vdem_liberal_democracy_index %>% nrow() # 3743
vdem_liberal_democracy_index %>% ncol() # 18

vdem_liberal_democracy_index %>% count(year) %>% print(n = nrow(.))
vdem_liberal_democracy_index %>% count(country_name)

# inspect missing for e&E

# check most recent year of data availability
vdem_liberal_democracy_index %>% 
        select(country, mcp_grouping, year, indicator_name, values) %>%
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(!is.na(var)) %>%
        group_by(country) %>%
        arrange(desc(year)) %>%
        slice(1) %>%
        ungroup() %>%
        select(country, year) %>% arrange(year)

# plot missing data
vdem_liberal_democracy_index %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        select(country, year, values) %>%
        group_by(country, year) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", variable, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )


# plot
vdem_liberal_democracy_index %>%
        filter(mcp_grouping == "E&E Balkans") %>%
        filter(year >= 2010) %>%
        select(country, year, values) %>%
        ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# vdem_liberal_democracy_index %>% write_csv(file = "data/vdem/vdem_liberal_democracy_index_20230115.csv")
vdem_liberal_democracy_index <- read_csv(file = "data/vdem/vdem_liberal_democracy_index_20230115.csv")

# inspect
vdem_liberal_democracy_index
vdem_liberal_democracy_index %>% glimpse()
vdem_liberal_democracy_index %>% nrow() # 3743
vdem_liberal_democracy_index %>% ncol() # 18


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# load vdem_civil_society_index ####
vdem_civil_society_index <- vdem %>% select(country_name, year, v2xcs_ccsi) %>%
        filter(year >= 2001) %>% 
        rename(sub_obj_1_2_vdem_civil_society = "v2xcs_ccsi") %>%
        pivot_longer(cols = sub_obj_1_2_vdem_civil_society, names_to = "indicator_name", values_to = "values") %>%
        mutate(high_value_is_good_outcome_flag = 1) %>%
        mutate(country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "Burma/Myanmar" ~ "Burma",
                                        country_name == "Cape Verde" ~ "Cabo Verde",
                                        country_name == "Czech Republic" ~ "Czechia",
                                        country_name == "Democratic Republic of the Congo" ~ "Congo (Kinshasa)",
                                        country_name == "Hong Kong" ~ "Hong Kong SAR, China",
                                        country_name == "Ivory Coast" ~ "Cote d'Ivoire",
                                        country_name == "North Korea" ~ "Korea, North",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        country_name == "Palestine/West Bank" ~ "West Bank and Gaza",
                                        country_name == "Papal States" ~ "Holy See",
                                        country_name == "Republic of the Congo" ~ "Congo (Brazzaville)",
                                        country_name == "Republic of Vietnam" ~ "Vietnam",
                                        country_name == "South Korea" ~ "Korea, South",
                                        country_name == "The Gambia" ~ "Gambia, The",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States of America" ~ "U.S.",
                                        TRUE ~ country_name))


#/////////////


# inspect
vdem_civil_society_index
vdem_civil_society_index %>% glimpse()
vdem_civil_society_index %>% nrow() # 3743
vdem_civil_society_index %>% ncol() # 5

vdem_civil_society_index %>% count(year) %>% print(n = nrow(.))
vdem_civil_society_index %>% count(country_name) 

# check names
country_crosswalk %>% anti_join(., vdem_civil_society_index, by = c("country" = "country_name")) %>% count(country) %>% print(n = nrow(.))
vdem_civil_society_index %>% filter(str_detect(string = country_name, pattern = regex("lucia", ignore_case = TRUE))) %>% distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("baham", ignore_case = TRUE)))


#/////////////////////////////////////////////////////////////////////////////////////////////


# add country_crosswalk
vdem_civil_society_index <- vdem_civil_society_index %>% 
        left_join(., country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S."),
                  by = c("country_name" = "country")) %>% 
        relocate(indicator_name, values, .after = everything()) %>%
        rename(country = country_name)


#/////////////


# inspect
vdem_civil_society_index
vdem_civil_society_index %>% glimpse()
vdem_civil_society_index %>% nrow() # 3743
vdem_civil_society_index %>% ncol() # 17

vdem_civil_society_index %>% count(year) %>% print(n = nrow(.))
vdem_civil_society_index %>% count(country_name)

# inspect missing for e&E

# check most recent year of data availability
vdem_civil_society_index %>% 
        select(country, mcp_grouping, year, indicator_name, values) %>%
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(!is.na(var)) %>%
        group_by(country) %>%
        arrange(desc(year)) %>%
        slice(1) %>%
        ungroup() %>%
        select(country, year) %>% arrange(year)

# plot missing data
vdem_civil_society_index %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        select(country, year, values) %>%
        group_by(country, year) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", variable, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )


# plot
vdem_civil_society_index %>%
        filter(mcp_grouping == "E&E Balkans") %>%
        filter(year >= 2010) %>%
        select(country, year, values) %>%
        ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# vdem_civil_society_index %>% write_csv(file = "data/vdem/vdem_civil_society_index_20220805.csv")
vdem_civil_society_index <- read_csv(file = "data/vdem/vdem_civil_society_index_20220805.csv")

# inspect
vdem_civil_society_index
vdem_civil_society_index %>% glimpse()
vdem_civil_society_index %>% nrow() # 3743
vdem_civil_society_index %>% ncol() # 17


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# load irex_vibe ####

# for reference, the new VIBE dataset will replace MSI, but VIBE only has 2020-2021 condition_year data
# below is the crosswalk of old MSI dimensions to new VICE dimensions from VIBE's report (also in methodology section on dashboard)
# https://vibe.irex.org/
# https://www.irex.org/sites/default/files/pdf/vibrant-information-barometer-2021.pdf
# MSI Overall scores - VIBE Overall scores
# Freedom of Speech - VIBE Principle 2 (Multiple Channels - How Info Flows)
# Professional Journalism - VIBE Principle 1 (Information Quality)
# Plurality of News - VIBE Indicator 4 (The body of content overall is inclusive and diverse.)
# Business Management - VIBE Indicator 5 (Content production is sufficiently resourced.)
# Supporting Institutions - no direct mapping (Supporting Institutions indicators are spread across VIBE Indicators)
# VIBE Principal 3 (Info Consumption and Engagement) has no MSI corollary
# VIBE Principal 4 (Transformative Action) has no MSI corollary

# note that shannon mcguire confirmed that the year variable listed in MSI and VIBE data is "report year", 
# and data refer to prior year; see email from shannon Feb 14, 2022, 4:25 PM

# note that MSI scoring ranges from 0 to 4; VIBE scoring ranges from 0 to 40
# will multiply MSI 0-4 scores by 10 to convert to VIBE 0-40 scale 
# https://www.irex.org/sites/default/files/pdf/media-sustainability-index-europe-eurasia-2019-full.pdf
# https://vibe.irex.org/

# note that MSI had 21 countries, VIBE had 13 in condition_year 2020, and 18 in condition_year 2021

# note that raw irex_msi data was copy/pasted together
msi_overall <- read_excel(path = "data/irex/irex_msi_data_cleaned.xlsx", sheet = "Sheet1") %>%
        mutate(report_year = year,
               year = report_year - 1) %>%
        filter(year >= 2001) %>%
        rename(country_name = COUNTRY, 
               freedom_of_speech = `Obj. 1`,
               professional_journalism = `Obj. 2`,
               plurality_of_news = `Obj. 3`,
               business_management = `Obj. 4`,
               supporting_institutions = `Obj. 5`,
               msi_overall = "Average") %>% 
        select(country_name, year, report_year, msi_overall) %>% 
        pivot_longer(cols = -c(country_name, year, report_year), names_to = "var", values_to = "values") %>%
        mutate(values = values * 10,
               high_value_is_good_outcome_flag = 1,
               indicator_name = "msi_overall",
               country_name = case_when(country_name == "Bosnia & Herzegovina" ~ "BiH",
                                        country_name == "Macedonia" ~ "N. Macedonia",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        TRUE ~ country_name)) %>%
        select(country_name, year, report_year, indicator_name, values, high_value_is_good_outcome_flag)

# get vibe
vibe_overall <- read_excel(path = "data/irex/vibe-scores-2021 - 2022 final.xlsx", sheet = "VIBE 2021", skip = 1) %>%
        rename(country_name = "...1", 
               values = Overall) %>%
        filter(country_name != "Country Name") %>%
        select(country_name, values) %>%
        mutate(report_year = 2021,
               year = 2020,
               values = as.numeric(values),
               indicator_name = "vibe_overall",
               high_value_is_good_outcome_flag = 1,
               country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "Macedonia" ~ "N. Macedonia",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        TRUE ~ country_name)) %>%
        select(country_name, year, report_year, indicator_name, values, high_value_is_good_outcome_flag) %>%
        bind_rows(.,
                  read_excel(path = "data/irex/vibe-scores-2021 - 2022 final.xlsx", sheet = "VIBE 2022", skip = 1) %>%
                          rename(country_name = "...1", 
                                 values = Overall) %>%
                          filter(country_name != "Country Name") %>%
                          select(country_name, values) %>%
                          mutate(report_year = 2022,
                                 year = 2021,
                                 values = as.numeric(values),
                                 indicator_name = "vibe_overall",
                                 high_value_is_good_outcome_flag = 1,
                                 country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                                          country_name == "Macedonia" ~ "N. Macedonia",
                                                          country_name == "North Macedonia" ~ "N. Macedonia",
                                                          TRUE ~ country_name)) %>%
                          select(country_name, year, report_year, indicator_name, values, high_value_is_good_outcome_flag))


# combine msi and vibe
irex_msi_vibe_overall <- msi_overall %>% 
        bind_rows(., vibe_overall)


#/////////////


# inspect
irex_msi_vibe_overall
irex_msi_vibe_overall %>% glimpse()
irex_msi_vibe_overall %>% nrow() # 403
irex_msi_vibe_overall %>% ncol() # 6

irex_msi_vibe_overall %>% count(year) %>% print(n = nrow(.))
irex_msi_vibe_overall %>% count(country_name) %>% print(n = nrow(.))

# check names
country_crosswalk %>% anti_join(., irex_msi_vibe_overall, by = c("country" = "country_name")) %>% count(country) %>% print(n = nrow(.))
irex_msi_vibe_overall %>% filter(str_detect(string = country_name, pattern = regex("lucia", ignore_case = TRUE))) %>% distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("baham", ignore_case = TRUE)))


#/////////////////////////////////////////////////////////////////////////////////////////////


# get country_crosswalk_expanded to have a country record for each year
country_crosswalk_expanded <- expand_grid(country = country_crosswalk %>% pull(country), 
                                          year = seq(from = 2001, to = 2021, by = 1)) %>%
        left_join(., country_crosswalk, by = "country")

# add country_crosswalk
irex_msi_vibe_overall <- irex_msi_vibe_overall %>% 
        left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
                  by = c("country" = "country_name", "year" = "year")) %>%
        relocate(indicator_name, values, .after = everything()) 


#/////////////


# inspect
irex_msi_vibe_overall
irex_msi_vibe_overall %>% glimpse()
irex_msi_vibe_overall %>% nrow() # 945
irex_msi_vibe_overall %>% ncol() # 19

irex_msi_vibe_overall %>% count(year) %>% print(n = nrow(.))
irex_msi_vibe_overall %>% count(year, indicator_name) %>% print(n = nrow(.))
irex_msi_vibe_overall %>% count(country)

irex_msi_vibe_overall %>% select(country, year, indicator_name, values) %>%
        filter(is.na(indicator_name))


#/////////////////////////////////////////////////////////////////////////////////////////////


# impute 2019 values as midpoint of 2018 MSI and 2020 VIBE
# manually assign the CARs the eurasia avg, since this will avoid it's earlier MSI values from being extended during imputation 
# to fill all the missing VIBE values 
irex_msi_vibe_overall <- irex_msi_vibe_overall %>%
        mutate(values_2018 = case_when(year == 2018 ~ values, 
                                       TRUE ~ NA_real_),
               values_2020 = case_when(year == 2020 ~ values, 
                                       TRUE ~ NA_real_)) %>%
        group_by(country) %>% 
        fill(values_2018, .direction = "updown") %>%
        fill(values_2020, .direction = "updown") %>%
        ungroup() %>%
        mutate(values = case_when(year == 2019 ~ ((values_2020 - values_2018) / 2) + values_2018,
                                  TRUE ~ values)) %>%
        group_by(mcp_grouping, year) %>%
        mutate(regional_annual_avg = mean(values, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(eurasia_annual_avg = case_when(mcp_grouping == "E&E Eurasia" ~ regional_annual_avg,
                                              TRUE ~ NA_real_)) %>%
        group_by(year) %>% 
        fill(eurasia_annual_avg, .direction = "updown") %>%
        ungroup() %>%
        mutate(values = case_when(mcp_grouping == "CARs" & year >= 2019 & is.na(values) ~ eurasia_annual_avg,
                                  TRUE ~ values))


#/////////////////////////////////////////////////////////////////////////////////////////////


# inspect
irex_msi_vibe_overall
irex_msi_vibe_overall %>% glimpse()
irex_msi_vibe_overall %>% nrow() # 945
irex_msi_vibe_overall %>% ncol() # 23

irex_msi_vibe_overall %>% count(year) %>% print(n = nrow(.))
irex_msi_vibe_overall %>% count(year, indicator_name) %>% print(n = nrow(.))
irex_msi_vibe_overall %>% count(country)

# inspect missing for e&E

# check most recent year of data availability
irex_msi_vibe_overall %>% 
        select(country, mcp_grouping, year, indicator_name, values) %>%
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(!is.na(var)) %>%
        group_by(country) %>%
        arrange(desc(year)) %>%
        slice(1) %>%
        ungroup() %>%
        select(country, year) %>% arrange(year)

# plot missing data
irex_msi_vibe_overall %>% 
        # filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        select(country, year, values) %>%
        group_by(country, year) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", variable, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )

# plot
irex_msi_vibe_overall %>%
        filter(mcp_grouping == "E&E Balkans") %>%
        filter(year >= 2010) %>%
        select(country, year, values) %>%
        ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line()


#/////////////////////////////////////////////////////////////////////////////////////////////


# test values

test_irex_msi_vibe_overall_values <- function() {
        
        # 1 
        expect_equal(object = irex_msi_vibe_overall %>% filter(country == "Serbia", year == 2020) %>%
                             pull(values),
                     expected = 15)
        
        # 1 
        expect_equal(object = irex_msi_vibe_overall %>% filter(country == "Albania", year == 2019) %>%
                             pull(values),
                     expected = ((22 - (2.49 * 10)) / 2) + (2.49 * 10))
        
        # 1 
        expect_equal(object = irex_msi_vibe_overall %>% filter(country == "Belarus", year == 2018) %>%
                             pull(values),
                     expected = 1.46 * 10)
}

test_irex_msi_vibe_overall_values()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# irex_msi_vibe_overall %>% write_csv(file = "data/irex/irex_msi_vibe_overall_20230115.csv")
irex_msi_vibe_overall <- read_csv(file = "data/irex/irex_msi_vibe_overall_20230115.csv")

# inspect
irex_msi_vibe_overall
irex_msi_vibe_overall %>% glimpse()
irex_msi_vibe_overall %>% nrow() # 945
irex_msi_vibe_overall %>% ncol() # 23



#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# world bank gross_capital_formation_as_share_of_gdp ####

# https://data.worldbank.org/indicator/NE.IMP.GNFS.ZS

gross_capital_formation_as_share_of_gdp <- read_excel(path = "data/world_bank/API_NE.GDI.TOTL.ZS_DS2_en_excel_v2_4770889.xls", sheet = "Data", skip = 3) %>%
        rename(country_name = `Country Name`, iso3 = `Country Code`) %>%
        select(-c(`Indicator Name`, `Indicator Code`)) %>%
        pivot_longer(cols = -c(country_name, iso3), names_to = "year", values_to = "gross_capital_formation_as_share_of_gdp_ppp") %>%
        mutate(year = as.numeric(year),
               values = gross_capital_formation_as_share_of_gdp_ppp,
               country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "Brunei Darussalam" ~ "Brunei",
                                        country_name == "Myanmar" ~ "Burma",
                                        country_name == "Congo, Dem. Rep." ~ "Congo (Kinshasa)",
                                        country_name == "Congo, Rep." ~ "Congo (Brazzaville)",
                                        country_name == "Czech Republic" ~ "Czechia",
                                        country_name == "Egypt, Arab Rep." ~ "Egypt",
                                        country_name == "Iran, Islamic Rep." ~ "Iran",
                                        country_name == "Korea, Dem. People's Rep." ~ "Korea, North",
                                        country_name == "Korea, Rep." ~ "Korea, South",
                                        country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                        country_name == "Lao PDR" ~ "Laos",
                                        country_name == "Micronesia, Fed. Sts." ~ "Micronesia, Federated States of",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        country_name == "Russian Federation" ~ "Russia",
                                        country_name == "St. Kitts and Nevis" ~ "Saint Kitts and Nevis",
                                        country_name == "St. Vincent and the Grenadines" ~ "Saint Vincent and the Grenadines",
                                        country_name == "St. Lucia" ~ "Saint Lucia",
                                        country_name == "Slovak Republic" ~ "Slovakia",
                                        country_name == "Syrian Arab Republic" ~ "Syria",
                                        country_name == "Turkiye" ~ "Turkey",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States" ~ "U.S.",
                                        country_name == "Venezuela, RB" ~ "Venezuela",
                                        country_name == "Yemen, Rep." ~ "Yemen",
                                        TRUE ~ country_name)) %>%
        filter(year >= 2000) 


#/////////////


# inspect
gross_capital_formation_as_share_of_gdp
gross_capital_formation_as_share_of_gdp %>% glimpse()
gross_capital_formation_as_share_of_gdp %>% nrow() # 5852
gross_capital_formation_as_share_of_gdp %>% ncol() # 5

gross_capital_formation_as_share_of_gdp %>% count(year) %>% print(n = nrow(.))
gross_capital_formation_as_share_of_gdp %>% count(country_name)

# check names
country_crosswalk %>% anti_join(., gross_capital_formation_as_share_of_gdp, by = c("country" = "country_name")) %>% count(country) %>% print(n = nrow(.))
gross_capital_formation_as_share_of_gdp %>% filter(str_detect(string = country_name, pattern = regex("lucia", ignore_case = TRUE))) %>% distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("baham", ignore_case = TRUE)))


migrants %>% anti_join(., gross_capital_formation_as_share_of_gdp, by = c("origin_country" = "country_name")) %>% count(origin_country) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# add country_crosswalk
gross_capital_formation_as_share_of_gdp <- gross_capital_formation_as_share_of_gdp %>% 
        left_join(., country_crosswalk %>% select(-country), by = "iso3") %>% 
        relocate(gross_capital_formation_as_share_of_gdp_ppp, .after = everything()) %>%
        rename(country = country_name)


#/////////////


# inspect
gross_capital_formation_as_share_of_gdp
gross_capital_formation_as_share_of_gdp %>% glimpse()
gross_capital_formation_as_share_of_gdp %>% nrow() # 5852
gross_capital_formation_as_share_of_gdp %>% ncol() # 16

gross_capital_formation_as_share_of_gdp %>% count(year) %>% print(n = nrow(.))
gross_capital_formation_as_share_of_gdp %>% count(country_name)

# inspect missing for e&E

# check most recent year of data availability
gross_capital_formation_as_share_of_gdp %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(!is.na(gross_capital_formation_as_share_of_gdp_ppp)) %>%
        group_by(country) %>%
        arrange(desc(year)) %>%
        slice(1) %>%
        ungroup() %>%
        select(country, year) %>% arrange(year)

# plot missing data
gross_capital_formation_as_share_of_gdp %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        select(country, year, gross_capital_formation_as_share_of_gdp_ppp) %>%
        group_by(country, year) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", variable, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )


# plot
gross_capital_formation_as_share_of_gdp %>%
        filter(mcp_grouping == "E&E Balkans") %>%
        filter(year >= 2010) %>%
        select(country, year, gross_capital_formation_as_share_of_gdp_ppp) %>%
        ggplot(data = ., mapping = aes(x = year, y = gross_capital_formation_as_share_of_gdp_ppp, color = country)) + geom_line()


#/////////////////////////////////////////////////////////////////////////////////////////////


# test values

test_gross_capital_formation_as_share_of_gdp_values <- function() {
        
        # 1
        expect_equal(object = gross_capital_formation_as_share_of_gdp %>% filter(country == "Albania", year == 2015) %>% 
                             pull(gross_capital_formation_as_share_of_gdp_ppp),
                     expected = 25.8190485021686)
        
        # 2
        expect_equal(object = gross_capital_formation_as_share_of_gdp %>% filter(country == "Georgia", year == 2021) %>% 
                             pull(gross_capital_formation_as_share_of_gdp_ppp),
                     expected = 18.9716422355346)
        
        # 3
        expect_equal(object = gross_capital_formation_as_share_of_gdp %>% filter(country == "Belarus", year == 2018) %>% 
                             pull(gross_capital_formation_as_share_of_gdp_ppp),
                     expected = 28.0788785453202)
}

test_gross_capital_formation_as_share_of_gdp_values()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# gross_capital_formation_as_share_of_gdp %>% write_csv(file = "data/world_bank/gross_capital_formation_as_share_of_gdp_20230115.csv")
gross_capital_formation_as_share_of_gdp <- read_csv(file = "data/world_bank/gross_capital_formation_as_share_of_gdp_20230115.csv")

# inspect
gross_capital_formation_as_share_of_gdp
gross_capital_formation_as_share_of_gdp %>% glimpse()
gross_capital_formation_as_share_of_gdp %>% nrow() # 5852
gross_capital_formation_as_share_of_gdp %>% ncol() # 16




#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# load sub_obj_4_1_atlas_eci ####

# https://atlas.cid.harvard.edu/rankings
# https://atlas.cid.harvard.edu/glossary

sub_obj_4_1_atlas_eci <- read_csv(file = "data/atlas_of_economic_complexity/Country Complexity Rankings 1995 - 2020.csv") %>%
        rename(country_name = Country) %>%
        pivot_longer(cols = -country_name, names_to = "var", values_to = "values") %>%
        filter(!str_detect(string = var, pattern = regex("Rank|COI"))) %>%
        mutate(indicator_name = "sub_obj_4_1_atlas_eci",
               year = as.numeric(str_extract(string = var, pattern = regex("[0-9]{4}"))),
               high_value_is_good_outcome_flag = 1,
               country_name = case_when(country_name == "North Macedonia" ~ "N. Macedonia",
                                        country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States of America" ~ "U.S.",
                                        TRUE ~ country_name)) %>%
        select(-var)


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# inspect
sub_obj_4_1_atlas_eci
sub_obj_4_1_atlas_eci %>% glimpse()
sub_obj_4_1_atlas_eci %>% nrow() # 3458
sub_obj_4_1_atlas_eci %>% ncol() # 5

# eci only goes up to 2020
sub_obj_4_1_atlas_eci %>% arrange(desc(values)) %>% distinct(country_name)
sub_obj_4_1_atlas_eci %>% arrange(values) %>% distinct(country_name)
sub_obj_4_1_atlas_eci %>% skim()
sub_obj_4_1_atlas_eci %>% count(year) %>% print(n = nrow(.))

# inspect country names
# note that atlas does not have kosovo, montenegro, or luxembourg (only has 134 countries total)
sub_obj_4_1_atlas_eci %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
        distinct(country_name) %>% arrange(country_name)
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
        anti_join(., sub_obj_4_1_atlas_eci, by = c("country" = "country_name")) %>% 
        distinct(country) %>% arrange(country)

sub_obj_4_1_atlas_eci %>% 
        filter(str_detect(string = country_name, pattern = regex("mont", ignore_case = TRUE))) %>%
        distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# join country_crosswalk and fmir_framework
sub_obj_4_1_atlas_eci <- sub_obj_4_1_atlas_eci %>% 
        left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
                  by = c("country" = "country_name", "year" = "year")) %>%
        mutate(indicator_name = "sub_obj_4_1_atlas_eci",
               high_value_is_good_outcome_flag = 1) %>%
        filter(year <= 2020)


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# inspect
sub_obj_4_1_atlas_eci
sub_obj_4_1_atlas_eci %>% glimpse()
sub_obj_4_1_atlas_eci %>% nrow() # 900
sub_obj_4_1_atlas_eci %>% ncol() # 18

# check country/year
sub_obj_4_1_atlas_eci %>% distinct(country) %>% nrow() # 45
sub_obj_4_1_atlas_eci %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))
sub_obj_4_1_atlas_eci %>% count(indicator_name) # 945 (21 years * 45 countries = 900)
sub_obj_4_1_atlas_eci %>% count(year)

# check values
# missing all values (2001-2020) for kosovo, luxembourg, montenegro
# missing serbia (2001-2004)
sub_obj_4_1_atlas_eci %>% filter(indicator_name == "sub_obj_4_1_atlas_eci") %>% skim(values)
sub_obj_4_1_atlas_eci  %>% filter(is.na(values)) %>% count(country) %>% 
        arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_4_1_atlas_eci  %>% filter(is.na(values)) %>% count(country, year) %>% 
        arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_4_1_atlas_eci  %>% filter(is.na(values), year <= 2019) %>%
        count(country, year) %>% print(n = nrow(.))

sub_obj_4_1_atlas_eci  %>% skim(values)
sub_obj_4_1_atlas_eci  %>% group_by(year) %>% skim(values)
sub_obj_4_1_atlas_eci  %>% filter(year <= 2019) %>% skim(values)
sub_obj_4_1_atlas_eci  %>% filter(year <= 2019) %>% group_by(country) %>% skim(values)

# plot
sub_obj_4_1_atlas_eci %>% 
        # filter(mcp_grouping == "E&E Balkans") %>%
        # filter(mcp_grouping == "E&E Eurasia") %>%
        # filter(mcp_grouping == "E&E graduates") %>%
        # filter(mcp_grouping == "CARs") %>%
        filter(mcp_grouping == "EU-15") %>%
        ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line() 

# inspect summary stats on indicators
sub_obj_4_1_atlas_eci %>% group_by(indicator_name) %>% 
        mutate(year_start = min(year, na.rm = TRUE), year_end = max(year, na.rm = TRUE),
               missing_flag = case_when(is.na(values) ~ 1, TRUE ~ 0),
               missing_pct = mean(missing_flag, na.rm = TRUE)) %>% 
        distinct(indicator_name, year_start, year_end, missing_pct)


#///////////////////////


# test values
test_values_sub_obj_4_1_atlas_eci <- function() {
        
        # 1
        expect_equal(object = sub_obj_4_1_atlas_eci %>% 
                             filter(country == "Albania", year == 2017) %>%
                             pull(values),
                     expected = -0.351054)
        
        # 2
        expect_equal(object = sub_obj_4_1_atlas_eci %>% 
                             filter(country == "Belarus", year == 2017) %>%
                             pull(values),
                     expected = 0.918878)
        
        # 3
        expect_equal(object = sub_obj_4_1_atlas_eci %>% 
                             filter(country == "Belarus", year == 2018) %>%
                             pull(values),
                     expected = 0.86016)
        
        # 4
        expect_equal(object = sub_obj_4_1_atlas_eci %>% 
                             filter(country == "Serbia", year == 2011) %>%
                             pull(values),
                     expected = 0.624627)
}
test_values_sub_obj_4_1_atlas_eci()


#//////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# sub_obj_4_1_atlas_eci %>% write_csv(file = "data/atlas_of_economic_complexity/sub_obj_4_1_atlas_eci.csv")
sub_obj_4_1_atlas_eci <- read_csv(file = "data/atlas_of_economic_complexity/sub_obj_4_1_atlas_eci.csv")

# inspect
sub_obj_4_1_atlas_eci
sub_obj_4_1_atlas_eci %>% glimpse()
sub_obj_4_1_atlas_eci %>% nrow() # 900
sub_obj_4_1_atlas_eci %>% ncol() # 18


#//////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////


# https://gain.nd.edu/our-work/country-index/rankings/

# confirmed that for "vulnerability" scores, lower is better (though it's reversed for "readiness" scores)
# https://gain.nd.edu/assets/254377/nd_gain_technical_document_2015.pdf
# The parameter of "direction" has two values, 0 when calculating score of vulnerability
# indicator; 1 when calculating score of readiness indicators, so that a higher
# vulnerability score means higher vulnerability ("worse") and a higher readiness score
# means higher readiness ("better"). 

# nd_gain_water_vulnerability ####
nd_gain_water_vulnerability <- read_csv(file = "data/nd_gain/resources/vulnerability/water.csv") %>%
        rename(iso3 = ISO3, country_name = Name) %>%
        pivot_longer(cols = -c(country_name, iso3), names_to = "year", values_to = "values") %>%
        mutate(high_value_is_good_outcome_flag = 0,
               indicator_name = "nd_gain_water_vulnerability",
               year = as.numeric(year))


#////////////////////


# inspect
nd_gain_water_vulnerability 
nd_gain_water_vulnerability %>% glimpse()
nd_gain_water_vulnerability %>% nrow() # 4992
nd_gain_water_vulnerability %>% ncol() # 6

# data up to 2020
nd_gain_water_vulnerability %>% count(year) %>% print(n = nrow(.))
nd_gain_water_vulnerability %>% count(country_name) # 192

# check country
# only country missing for ee region is kosovo
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
        select(country, iso3) %>%
        anti_join(., nd_gain_water_vulnerability %>% select(country_name, iso3),
                  by = c("iso3"))


#//////////////////////////////////////////////////////////////////////////////////////////////


# add country_crosswalk
# drop 2021
nd_gain_water_vulnerability <- nd_gain_water_vulnerability %>%
        left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
                  by = c("iso3", "year")) %>%
        select(-country_name) %>%
        filter(year <= 2020)


#/////////////////////


# inspect
nd_gain_water_vulnerability 
nd_gain_water_vulnerability %>% glimpse()
nd_gain_water_vulnerability %>% nrow() # 945
nd_gain_water_vulnerability %>% ncol() # 18

# data up to 2020
nd_gain_water_vulnerability %>% count(year) %>% print(n = nrow(.))
nd_gain_water_vulnerability %>% count(country) # 45


# check missing
# only kosovo is missing (nd_gain has no 2021 values, so they were dropped from country_crosswalk_expanded)
nd_gain_water_vulnerability %>% 
        filter(ee_region_flag == 1) %>%
        filter(is.na(values)) %>% 
        count(country) %>% print(n = nrow(.))


#//////////////////////////


# test values
test_nd_gain_water_vulnerability <- function() {
        
        expect_equal(object = nd_gain_water_vulnerability %>%
                             filter(country == "Armenia", year == 2020) %>% 
                             pull(values),
                     expected = 0.302175653104603)
        
        expect_equal(object = nd_gain_water_vulnerability %>%
                             filter(country == "Ukraine", year == 2019) %>% 
                             pull(values),
                     expected = 0.375234205915825)
        
        expect_equal(object = nd_gain_water_vulnerability %>%
                             filter(country == "Georgia", year == 2018) %>% 
                             pull(values),
                     expected = 0.293794438054454)
}
test_nd_gain_water_vulnerability()


#//////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# nd_gain_water_vulnerability %>% write_csv(file = "data/nd_gain/nd_gain_water_vulnerability.csv")
nd_gain_water_vulnerability <- read_csv(file = "data/nd_gain/nd_gain_water_vulnerability.csv")

# inspect
nd_gain_water_vulnerability
nd_gain_water_vulnerability %>% glimpse()
nd_gain_water_vulnerability %>% nrow() # 900
nd_gain_water_vulnerability %>% ncol() # 18



#//////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////


# https://gain.nd.edu/our-work/country-index/rankings/

# nd_gain_food_vulnerability ####
nd_gain_food_vulnerability <- read_csv(file = "data/nd_gain/resources/vulnerability/food.csv") %>%
        rename(iso3 = ISO3, country_name = Name) %>%
        pivot_longer(cols = -c(country_name, iso3), names_to = "year", values_to = "values") %>%
        mutate(high_value_is_good_outcome_flag = 0,
               indicator_name = "nd_gain_food_vulnerability",
               year = as.numeric(year))


#////////////////////


# inspect
nd_gain_food_vulnerability 
nd_gain_food_vulnerability %>% glimpse()
nd_gain_food_vulnerability %>% nrow() # 4992
nd_gain_food_vulnerability %>% ncol() # 6

# data up to 2020
nd_gain_food_vulnerability %>% count(year) %>% print(n = nrow(.))
nd_gain_food_vulnerability %>% count(country_name) # 192

# check country
# only country missing for ee region is kosovo
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
        select(country, iso3) %>%
        anti_join(., nd_gain_food_vulnerability %>% select(country_name, iso3),
                  by = c("iso3"))


#//////////////////////////////////////////////////////////////////////////////////////////////


# add country_crosswalk
# drop 2021
nd_gain_food_vulnerability <- nd_gain_food_vulnerability %>%
        left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
                  by = c("iso3", "year")) %>%
        select(-country_name) %>%
        filter(year <= 2020)


#/////////////////////


# inspect
nd_gain_food_vulnerability 
nd_gain_food_vulnerability %>% glimpse()
nd_gain_food_vulnerability %>% nrow() # 945
nd_gain_food_vulnerability %>% ncol() # 18

# data up to 2020
nd_gain_food_vulnerability %>% count(year) %>% print(n = nrow(.))
nd_gain_food_vulnerability %>% count(country) # 45


# check missing
# only kosovo is missing (nd_gain has no 2021 values, so they were dropped from country_crosswalk_expanded)
nd_gain_food_vulnerability %>% 
        filter(ee_region_flag == 1) %>%
        filter(is.na(values)) %>% 
        count(country) %>% print(n = nrow(.))


#//////////////////////////


# test values
test_nd_gain_food_vulnerability <- function() {
        
        expect_equal(object = nd_gain_food_vulnerability %>%
                             filter(country == "Armenia", year == 2020) %>% 
                             pull(values),
                     expected = 0.392396714217035)
        
        expect_equal(object = nd_gain_food_vulnerability %>%
                             filter(country == "Ukraine", year == 2019) %>% 
                             pull(values),
                     expected = 0.417336251160365)
        
        expect_equal(object = nd_gain_food_vulnerability %>%
                             filter(country == "Georgia", year == 2018) %>% 
                             pull(values),
                     expected = 0.349786905784522)
}
test_nd_gain_food_vulnerability()


#//////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# nd_gain_food_vulnerability %>% write_csv(file = "data/nd_gain/nd_gain_food_vulnerability.csv")
nd_gain_food_vulnerability<- read_csv(file = "data/nd_gain/nd_gain_food_vulnerability.csv")

# inspect
nd_gain_food_vulnerability
nd_gain_food_vulnerability %>% glimpse()
nd_gain_food_vulnerability %>% nrow() # 900
nd_gain_food_vulnerability %>% ncol() # 18


#//////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////


# https://gain.nd.edu/our-work/country-index/rankings/

# nd_gain_human_habitat_vulnerability ####
nd_gain_human_habitat_vulnerability <- read_csv(file = "data/nd_gain/resources/vulnerability/habitat.csv") %>%
        rename(iso3 = ISO3, country_name = Name) %>%
        pivot_longer(cols = -c(country_name, iso3), names_to = "year", values_to = "values") %>%
        mutate(high_value_is_good_outcome_flag = 0,
               indicator_name = "nd_gain_human_habitat_vulnerability",
               year = as.numeric(year))


#////////////////////


# inspect
nd_gain_human_habitat_vulnerability 
nd_gain_human_habitat_vulnerability %>% glimpse()
nd_gain_human_habitat_vulnerability %>% nrow() # 4992
nd_gain_human_habitat_vulnerability %>% ncol() # 6

# data up to 2020
nd_gain_human_habitat_vulnerability %>% count(year) %>% print(n = nrow(.))
nd_gain_human_habitat_vulnerability %>% count(country_name) # 192

# check country
# only country missing for ee region is kosovo
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
        select(country, iso3) %>%
        anti_join(., nd_gain_human_habitat_vulnerability %>% select(country_name, iso3),
                  by = c("iso3"))


#//////////////////////////////////////////////////////////////////////////////////////////////


# add country_crosswalk
# drop 2021
nd_gain_human_habitat_vulnerability <- nd_gain_human_habitat_vulnerability %>%
        left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
                  by = c("iso3", "year")) %>%
        select(-country_name) %>%
        filter(year <= 2020)


#/////////////////////


# inspect
nd_gain_human_habitat_vulnerability 
nd_gain_human_habitat_vulnerability %>% glimpse()
nd_gain_human_habitat_vulnerability %>% nrow() # 945
nd_gain_human_habitat_vulnerability %>% ncol() # 18

# data up to 2020
nd_gain_human_habitat_vulnerability %>% count(year) %>% print(n = nrow(.))
nd_gain_human_habitat_vulnerability %>% count(country) # 45


# check missing
# only kosovo is missing (nd_gain has no 2021 values, so they were dropped from country_crosswalk_expanded)
nd_gain_human_habitat_vulnerability %>% 
        filter(ee_region_flag == 1) %>%
        filter(is.na(values)) %>% 
        count(country) %>% print(n = nrow(.))


#//////////////////////////


# test values
test_nd_gain_human_habitat_vulnerability <- function() {
        
        expect_equal(object = nd_gain_human_habitat_vulnerability %>%
                             filter(country == "Armenia", year == 2020) %>% 
                             pull(values),
                     expected = 0.411418492759932)
        
        expect_equal(object = nd_gain_human_habitat_vulnerability %>%
                             filter(country == "Ukraine", year == 2019) %>% 
                             pull(values),
                     expected = 0.467521451612943)
        
        expect_equal(object = nd_gain_human_habitat_vulnerability %>%
                             filter(country == "Georgia", year == 2018) %>% 
                             pull(values),
                     expected = 0.527048868049843)
}
test_nd_gain_human_habitat_vulnerability()


#//////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# nd_gain_human_habitat_vulnerability %>% write_csv(file = "data/nd_gain/nd_gain_human_habitat_vulnerability.csv")
nd_gain_human_habitat_vulnerability<- read_csv(file = "data/nd_gain/nd_gain_human_habitat_vulnerability.csv")

# inspect
nd_gain_human_habitat_vulnerability
nd_gain_human_habitat_vulnerability %>% glimpse()
nd_gain_human_habitat_vulnerability %>% nrow() # 900
nd_gain_human_habitat_vulnerability %>% ncol() # 18


#//////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////


# https://gain.nd.edu/our-work/country-index/rankings/

# nd_gain_ecosystems_vulnerability ####
nd_gain_ecosystems_vulnerability <- read_csv(file = "data/nd_gain/resources/vulnerability/ecosystems.csv") %>%
        rename(iso3 = ISO3, country_name = Name) %>%
        pivot_longer(cols = -c(country_name, iso3), names_to = "year", values_to = "values") %>%
        mutate(high_value_is_good_outcome_flag = 1,
               indicator_name = "nd_gain_ecosystems_vulnerability",
               year = as.numeric(year))


#////////////////////


# inspect
nd_gain_ecosystems_vulnerability 
nd_gain_ecosystems_vulnerability %>% glimpse()
nd_gain_ecosystems_vulnerability %>% nrow() # 4992
nd_gain_ecosystems_vulnerability %>% ncol() # 6

# data up to 2020
nd_gain_ecosystems_vulnerability %>% count(year) %>% print(n = nrow(.))
nd_gain_ecosystems_vulnerability %>% count(country_name) # 192

# check country
# only country missing for ee region is kosovo
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
        select(country, iso3) %>%
        anti_join(., nd_gain_ecosystems_vulnerability %>% select(country_name, iso3),
                  by = c("iso3"))


#//////////////////////////////////////////////////////////////////////////////////////////////


# add country_crosswalk
# drop 2021
nd_gain_ecosystems_vulnerability <- nd_gain_ecosystems_vulnerability %>%
        left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
                  by = c("iso3", "year")) %>%
        select(-country_name) %>%
        filter(year <= 2020)


#/////////////////////


# inspect
nd_gain_ecosystems_vulnerability 
nd_gain_ecosystems_vulnerability %>% glimpse()
nd_gain_ecosystems_vulnerability %>% nrow() # 900
nd_gain_ecosystems_vulnerability %>% ncol() # 18

# data up to 2020
nd_gain_ecosystems_vulnerability %>% count(year) %>% print(n = nrow(.))
nd_gain_ecosystems_vulnerability %>% count(country) # 45


# check missing
# only kosovo is missing (nd_gain has no 2021 values, so they were dropped from country_crosswalk_expanded)
nd_gain_ecosystems_vulnerability %>% 
        filter(ee_region_flag == 1) %>%
        filter(is.na(values)) %>% 
        count(country) %>% print(n = nrow(.))


#//////////////////////////


# test values
test_nd_gain_ecosystems_vulnerability <- function() {
        
        expect_equal(object = nd_gain_ecosystems_vulnerability %>%
                             filter(country == "Armenia", year == 2020) %>% 
                             pull(values),
                     expected = 0.377484361984063)
        
        expect_equal(object = nd_gain_ecosystems_vulnerability %>%
                             filter(country == "Ukraine", year == 2019) %>% 
                             pull(values),
                     expected = 0.42637620640529)
        
        expect_equal(object = nd_gain_ecosystems_vulnerability %>%
                             filter(country == "Georgia", year == 2018) %>% 
                             pull(values),
                     expected = 0.488293913219082)
}
test_nd_gain_ecosystems_vulnerability()


#//////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# nd_gain_ecosystems_vulnerability %>% write_csv(file = "data/nd_gain/nd_gain_ecosystems_vulnerability.csv")
nd_gain_ecosystems_vulnerability<- read_csv(file = "data/nd_gain/nd_gain_ecosystems_vulnerability.csv")

# inspect
nd_gain_ecosystems_vulnerability
nd_gain_ecosystems_vulnerability %>% glimpse()
nd_gain_ecosystems_vulnerability %>% nrow() # 900
nd_gain_ecosystems_vulnerability %>% ncol() # 18


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# world bank agriculture_value_added_as_share_of_gdp ####

# https://data.worldbank.org/indicator/NV.AGR.TOTL.ZS

agriculture_value_added_as_share_of_gdp <- read_excel(path = "data/world_bank/API_NV.AGR.TOTL.ZS_DS2_en_excel_v2_4770726.xls", sheet = "Data", skip = 3) %>%
        rename(country_name = `Country Name`, iso3 = `Country Code`) %>%
        select(-c(`Indicator Name`, `Indicator Code`)) %>%
        pivot_longer(cols = -c(country_name, iso3), names_to = "year", values_to = "agriculture_value_added_as_share_of_gdp") %>%
        mutate(year = as.numeric(year),
               country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "Brunei Darussalam" ~ "Brunei",
                                        country_name == "Myanmar" ~ "Burma",
                                        country_name == "Congo, Dem. Rep." ~ "Congo (Kinshasa)",
                                        country_name == "Congo, Rep." ~ "Congo (Brazzaville)",
                                        country_name == "Czech Republic" ~ "Czechia",
                                        country_name == "Egypt, Arab Rep." ~ "Egypt",
                                        country_name == "Iran, Islamic Rep." ~ "Iran",
                                        country_name == "Korea, Dem. People's Rep." ~ "Korea, North",
                                        country_name == "Korea, Rep." ~ "Korea, South",
                                        country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                        country_name == "Lao PDR" ~ "Laos",
                                        country_name == "Micronesia, Fed. Sts." ~ "Micronesia, Federated States of",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        country_name == "Russian Federation" ~ "Russia",
                                        country_name == "St. Kitts and Nevis" ~ "Saint Kitts and Nevis",
                                        country_name == "St. Vincent and the Grenadines" ~ "Saint Vincent and the Grenadines",
                                        country_name == "St. Lucia" ~ "Saint Lucia",
                                        country_name == "Slovak Republic" ~ "Slovakia",
                                        country_name == "Syrian Arab Republic" ~ "Syria",
                                        country_name == "Turkiye" ~ "Turkey",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States" ~ "U.S.",
                                        country_name == "Venezuela, RB" ~ "Venezuela",
                                        country_name == "Yemen, Rep." ~ "Yemen",
                                        TRUE ~ country_name)) %>%
        filter(year >= 2000) 


#/////////////


# inspect
agriculture_value_added_as_share_of_gdp
agriculture_value_added_as_share_of_gdp %>% glimpse()
agriculture_value_added_as_share_of_gdp %>% nrow() # 5852
agriculture_value_added_as_share_of_gdp %>% ncol() # 4

agriculture_value_added_as_share_of_gdp %>% count(year) %>% print(n = nrow(.))
agriculture_value_added_as_share_of_gdp %>% count(country_name)

# check names
country_crosswalk %>% anti_join(., agriculture_value_added_as_share_of_gdp, by = c("country" = "country_name")) %>% count(country) %>% print(n = nrow(.))
agriculture_value_added_as_share_of_gdp %>% filter(str_detect(string = country_name, pattern = regex("lucia", ignore_case = TRUE))) %>% distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("baham", ignore_case = TRUE)))


migrants %>% anti_join(., agriculture_value_added_as_share_of_gdp, by = c("origin_country" = "country_name")) %>% count(origin_country) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# add country_crosswalk
agriculture_value_added_as_share_of_gdp <- agriculture_value_added_as_share_of_gdp %>% left_join(., country_crosswalk %>% select(-country), by = "iso3") %>% 
        relocate(agriculture_value_added_as_share_of_gdp, .after = everything()) %>%
        rename(country = country_name)


#/////////////


# inspect
agriculture_value_added_as_share_of_gdp
agriculture_value_added_as_share_of_gdp %>% glimpse()
agriculture_value_added_as_share_of_gdp %>% nrow() # 5852
agriculture_value_added_as_share_of_gdp %>% ncol() # 16

agriculture_value_added_as_share_of_gdp %>% count(year) %>% print(n = nrow(.))
agriculture_value_added_as_share_of_gdp %>% count(country_name)

# inspect missing for e&E

# check most recent year of data availability
agriculture_value_added_as_share_of_gdp %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(!is.na(agriculture_value_added_as_share_of_gdp)) %>%
        group_by(country) %>%
        arrange(desc(year)) %>%
        slice(1) %>%
        ungroup() %>%
        select(country, year) %>% arrange(year)

# plot missing data
agriculture_value_added_as_share_of_gdp %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        select(country, year, agriculture_value_added_as_share_of_gdp) %>%
        group_by(country, year) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", variable, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )


# plot
agriculture_value_added_as_share_of_gdp %>%
        filter(mcp_grouping == "E&E Balkans") %>%
        filter(year >= 2010) %>%
        select(country, year, agriculture_value_added_as_share_of_gdp) %>%
        ggplot(data = ., mapping = aes(x = year, y = agriculture_value_added_as_share_of_gdp, color = country)) + geom_line()


#/////////////////////////////////////////////////////////////////////////////////////////////


# test values

test_agriculture_value_added_as_share_of_gdp_values <- function() {
        
        # 1
        expect_equal(object = agriculture_value_added_as_share_of_gdp %>% filter(country == "Albania", year == 2015) %>% 
                             pull(agriculture_value_added_as_share_of_gdp),
                     expected = 19.7802144171366)
        
        # 2
        expect_equal(object = agriculture_value_added_as_share_of_gdp %>% filter(country == "Georgia", year == 2021) %>% 
                             pull(agriculture_value_added_as_share_of_gdp),
                     expected = 6.46632850320505)
        
        # 3
        expect_equal(object = agriculture_value_added_as_share_of_gdp %>% filter(country == "Belarus", year == 2018) %>% 
                             pull(agriculture_value_added_as_share_of_gdp),
                     expected = 6.59010772590188)
}

test_agriculture_value_added_as_share_of_gdp_values()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# agriculture_value_added_as_share_of_gdp %>% write_csv(file = "data/world_bank/agriculture_value_added_as_share_of_gdp_20230115.csv")
agriculture_value_added_as_share_of_gdp <- read_csv(file = "data/world_bank/agriculture_value_added_as_share_of_gdp_20230115.csv")

# inspect
agriculture_value_added_as_share_of_gdp
agriculture_value_added_as_share_of_gdp %>% glimpse()
agriculture_value_added_as_share_of_gdp %>% nrow() # 5852
agriculture_value_added_as_share_of_gdp %>% ncol() # 16


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# world bank services_value_added_as_share_of_gdp ####

# https://data.worldbank.org/indicator/NV.SRV.TOTL.ZS

services_value_added_as_share_of_gdp <- read_excel(path = "data/world_bank/API_NV.SRV.TOTL.ZS_DS2_en_excel_v2_4771369.xls", sheet = "Data", skip = 3) %>%
        rename(country_name = `Country Name`, iso3 = `Country Code`) %>%
        select(-c(`Indicator Name`, `Indicator Code`)) %>%
        pivot_longer(cols = -c(country_name, iso3), names_to = "year", values_to = "services_value_added_as_share_of_gdp") %>%
        mutate(year = as.numeric(year),
               country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "Brunei Darussalam" ~ "Brunei",
                                        country_name == "Myanmar" ~ "Burma",
                                        country_name == "Congo, Dem. Rep." ~ "Congo (Kinshasa)",
                                        country_name == "Congo, Rep." ~ "Congo (Brazzaville)",
                                        country_name == "Czech Republic" ~ "Czechia",
                                        country_name == "Egypt, Arab Rep." ~ "Egypt",
                                        country_name == "Iran, Islamic Rep." ~ "Iran",
                                        country_name == "Korea, Dem. People's Rep." ~ "Korea, North",
                                        country_name == "Korea, Rep." ~ "Korea, South",
                                        country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                        country_name == "Lao PDR" ~ "Laos",
                                        country_name == "Micronesia, Fed. Sts." ~ "Micronesia, Federated States of",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        country_name == "Russian Federation" ~ "Russia",
                                        country_name == "St. Kitts and Nevis" ~ "Saint Kitts and Nevis",
                                        country_name == "St. Vincent and the Grenadines" ~ "Saint Vincent and the Grenadines",
                                        country_name == "St. Lucia" ~ "Saint Lucia",
                                        country_name == "Slovak Republic" ~ "Slovakia",
                                        country_name == "Syrian Arab Republic" ~ "Syria",
                                        country_name == "Turkiye" ~ "Turkey",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States" ~ "U.S.",
                                        country_name == "Venezuela, RB" ~ "Venezuela",
                                        country_name == "Yemen, Rep." ~ "Yemen",
                                        TRUE ~ country_name)) %>%
        filter(year >= 2000) 


#/////////////


# inspect
services_value_added_as_share_of_gdp
services_value_added_as_share_of_gdp %>% glimpse()
services_value_added_as_share_of_gdp %>% nrow() # 5852
services_value_added_as_share_of_gdp %>% ncol() # 4

services_value_added_as_share_of_gdp %>% count(year) %>% print(n = nrow(.))
services_value_added_as_share_of_gdp %>% count(country_name)

# check names
country_crosswalk %>% anti_join(., services_value_added_as_share_of_gdp, by = c("country" = "country_name")) %>% count(country) %>% print(n = nrow(.))
services_value_added_as_share_of_gdp %>% filter(str_detect(string = country_name, pattern = regex("lucia", ignore_case = TRUE))) %>% distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("baham", ignore_case = TRUE)))


migrants %>% anti_join(., services_value_added_as_share_of_gdp, by = c("origin_country" = "country_name")) %>% count(origin_country) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# add country_crosswalk
services_value_added_as_share_of_gdp <- services_value_added_as_share_of_gdp %>% left_join(., country_crosswalk %>% select(-country), by = "iso3") %>% 
        relocate(services_value_added_as_share_of_gdp, .after = everything()) %>%
        rename(country = country_name)


#/////////////


# inspect
services_value_added_as_share_of_gdp
services_value_added_as_share_of_gdp %>% glimpse()
services_value_added_as_share_of_gdp %>% nrow() # 5852
services_value_added_as_share_of_gdp %>% ncol() # 16

services_value_added_as_share_of_gdp %>% count(year) %>% print(n = nrow(.))
services_value_added_as_share_of_gdp %>% count(country_name)

# inspect missing for e&E

# check most recent year of data availability
services_value_added_as_share_of_gdp %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(!is.na(services_value_added_as_share_of_gdp)) %>%
        group_by(country) %>%
        arrange(desc(year)) %>%
        slice(1) %>%
        ungroup() %>%
        select(country, year) %>% arrange(year)

# plot missing data
services_value_added_as_share_of_gdp %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        select(country, year, services_value_added_as_share_of_gdp) %>%
        group_by(country, year) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", variable, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )


# plot
services_value_added_as_share_of_gdp %>%
        filter(mcp_grouping == "E&E Balkans") %>%
        filter(year >= 2010) %>%
        select(country, year, services_value_added_as_share_of_gdp) %>%
        ggplot(data = ., mapping = aes(x = year, y = services_value_added_as_share_of_gdp, color = country)) + geom_line()


#/////////////////////////////////////////////////////////////////////////////////////////////


# test values

test_services_value_added_as_share_of_gdp_values <- function() {
        
        # 1
        expect_equal(object = services_value_added_as_share_of_gdp %>% filter(country == "Albania", year == 2015) %>% 
                             pull(services_value_added_as_share_of_gdp),
                     expected = 46.2844425914396)
        
        # 2
        expect_equal(object = services_value_added_as_share_of_gdp %>% filter(country == "Georgia", year == 2021) %>% 
                             pull(services_value_added_as_share_of_gdp),
                     expected = 59.5147081599135)
        
        # 3
        expect_equal(object = services_value_added_as_share_of_gdp %>% filter(country == "Belarus", year == 2018) %>% 
                             pull(services_value_added_as_share_of_gdp),
                     expected = 47.8104508104582)
}
test_services_value_added_as_share_of_gdp_values()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# services_value_added_as_share_of_gdp %>% write_csv(file = "data/world_bank/services_value_added_as_share_of_gdp_20230115.csv")
services_value_added_as_share_of_gdp <- read_csv(file = "data/world_bank/services_value_added_as_share_of_gdp_20230115.csv")

# inspect
services_value_added_as_share_of_gdp
services_value_added_as_share_of_gdp %>% glimpse()
services_value_added_as_share_of_gdp %>% nrow() # 5852
services_value_added_as_share_of_gdp %>% ncol() # 16


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# world bank industry_value_added_as_share_of_gdp ####

# https://data.worldbank.org/indicator/NV.IND.TOTL.ZS

industry_value_added_as_share_of_gdp <- read_excel(path = "data/world_bank/API_NV.IND.TOTL.ZS_DS2_en_excel_v2_4770575.xls", sheet = "Data", skip = 3) %>%
        rename(country_name = `Country Name`, iso3 = `Country Code`) %>%
        select(-c(`Indicator Name`, `Indicator Code`)) %>%
        pivot_longer(cols = -c(country_name, iso3), names_to = "year", values_to = "industry_value_added_as_share_of_gdp") %>%
        mutate(year = as.numeric(year),
               country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "Brunei Darussalam" ~ "Brunei",
                                        country_name == "Myanmar" ~ "Burma",
                                        country_name == "Congo, Dem. Rep." ~ "Congo (Kinshasa)",
                                        country_name == "Congo, Rep." ~ "Congo (Brazzaville)",
                                        country_name == "Czech Republic" ~ "Czechia",
                                        country_name == "Egypt, Arab Rep." ~ "Egypt",
                                        country_name == "Iran, Islamic Rep." ~ "Iran",
                                        country_name == "Korea, Dem. People's Rep." ~ "Korea, North",
                                        country_name == "Korea, Rep." ~ "Korea, South",
                                        country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                        country_name == "Lao PDR" ~ "Laos",
                                        country_name == "Micronesia, Fed. Sts." ~ "Micronesia, Federated States of",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        country_name == "Russian Federation" ~ "Russia",
                                        country_name == "St. Kitts and Nevis" ~ "Saint Kitts and Nevis",
                                        country_name == "St. Vincent and the Grenadines" ~ "Saint Vincent and the Grenadines",
                                        country_name == "St. Lucia" ~ "Saint Lucia",
                                        country_name == "Slovak Republic" ~ "Slovakia",
                                        country_name == "Syrian Arab Republic" ~ "Syria",
                                        country_name == "Turkiye" ~ "Turkey",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States" ~ "U.S.",
                                        country_name == "Venezuela, RB" ~ "Venezuela",
                                        country_name == "Yemen, Rep." ~ "Yemen",
                                        TRUE ~ country_name)) %>%
        filter(year >= 2000) 


#/////////////


# inspect
industry_value_added_as_share_of_gdp
industry_value_added_as_share_of_gdp %>% glimpse()
industry_value_added_as_share_of_gdp %>% nrow() # 5852
industry_value_added_as_share_of_gdp %>% ncol() # 4

industry_value_added_as_share_of_gdp %>% count(year) %>% print(n = nrow(.))
industry_value_added_as_share_of_gdp %>% count(country_name)

# check names
country_crosswalk %>% anti_join(., industry_value_added_as_share_of_gdp, by = c("country" = "country_name")) %>% count(country) %>% print(n = nrow(.))
industry_value_added_as_share_of_gdp %>% filter(str_detect(string = country_name, pattern = regex("lucia", ignore_case = TRUE))) %>% distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("baham", ignore_case = TRUE)))


migrants %>% anti_join(., industry_value_added_as_share_of_gdp, by = c("origin_country" = "country_name")) %>% count(origin_country) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# add country_crosswalk
industry_value_added_as_share_of_gdp <- industry_value_added_as_share_of_gdp %>% left_join(., country_crosswalk %>% select(-country), by = "iso3") %>% 
        relocate(industry_value_added_as_share_of_gdp, .after = everything()) %>%
        rename(country = country_name)


#/////////////


# inspect
industry_value_added_as_share_of_gdp
industry_value_added_as_share_of_gdp %>% glimpse()
industry_value_added_as_share_of_gdp %>% nrow() # 5852
industry_value_added_as_share_of_gdp %>% ncol() # 16

industry_value_added_as_share_of_gdp %>% count(year) %>% print(n = nrow(.))
industry_value_added_as_share_of_gdp %>% count(country_name)

# inspect missing for e&E

# check most recent year of data availability
industry_value_added_as_share_of_gdp %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(!is.na(industry_value_added_as_share_of_gdp)) %>%
        group_by(country) %>%
        arrange(desc(year)) %>%
        slice(1) %>%
        ungroup() %>%
        select(country, year) %>% arrange(year)

# plot missing data
industry_value_added_as_share_of_gdp %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        select(country, year, industry_value_added_as_share_of_gdp) %>%
        group_by(country, year) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", variable, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )


# plot
industry_value_added_as_share_of_gdp %>%
        filter(mcp_grouping == "E&E Balkans") %>%
        filter(year >= 2010) %>%
        select(country, year, industry_value_added_as_share_of_gdp) %>%
        ggplot(data = ., mapping = aes(x = year, y = industry_value_added_as_share_of_gdp, color = country)) + geom_line()


#/////////////////////////////////////////////////////////////////////////////////////////////


# test values

test_industry_value_added_as_share_of_gdp_values <- function() {
        
        # 1
        expect_equal(object = industry_value_added_as_share_of_gdp %>% filter(country == "Albania", year == 2015) %>% 
                             pull(industry_value_added_as_share_of_gdp),
                     expected = 21.7636809971645)
        
        # 2
        expect_equal(object = industry_value_added_as_share_of_gdp %>% filter(country == "Georgia", year == 2021) %>% 
                             pull(industry_value_added_as_share_of_gdp),
                     expected = 21.3680837567167)
        
        # 3
        expect_equal(object = industry_value_added_as_share_of_gdp %>% filter(country == "Belarus", year == 2018) %>% 
                             pull(industry_value_added_as_share_of_gdp),
                     expected = 31.2656097096379)
}
test_industry_value_added_as_share_of_gdp_values()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# industry_value_added_as_share_of_gdp %>% write_csv(file = "data/world_bank/industry_value_added_as_share_of_gdp_20230115.csv")
industry_value_added_as_share_of_gdp <- read_csv(file = "data/world_bank/industry_value_added_as_share_of_gdp_20230115.csv")

# inspect
industry_value_added_as_share_of_gdp
industry_value_added_as_share_of_gdp %>% glimpse()
industry_value_added_as_share_of_gdp %>% nrow() # 5852
industry_value_added_as_share_of_gdp %>% ncol() # 16



#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# world bank agriculture_employment_as_share_of_total_employment ####

# https://data.worldbank.org/indicator/SL.AGR.EMPL.ZS

agriculture_employment_as_share_of_total_employment <- read_excel(path = "data/world_bank/API_SL.AGR.EMPL.ZS_DS2_en_excel_v2_4770986.xls", sheet = "Data", skip = 3) %>%
        rename(country_name = `Country Name`, iso3 = `Country Code`) %>%
        select(-c(`Indicator Name`, `Indicator Code`)) %>%
        pivot_longer(cols = -c(country_name, iso3), names_to = "year", values_to = "agriculture_employment_as_share_of_total_employment") %>%
        mutate(year = as.numeric(year),
               country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "Brunei Darussalam" ~ "Brunei",
                                        country_name == "Myanmar" ~ "Burma",
                                        country_name == "Congo, Dem. Rep." ~ "Congo (Kinshasa)",
                                        country_name == "Congo, Rep." ~ "Congo (Brazzaville)",
                                        country_name == "Czech Republic" ~ "Czechia",
                                        country_name == "Egypt, Arab Rep." ~ "Egypt",
                                        country_name == "Iran, Islamic Rep." ~ "Iran",
                                        country_name == "Korea, Dem. People's Rep." ~ "Korea, North",
                                        country_name == "Korea, Rep." ~ "Korea, South",
                                        country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                        country_name == "Lao PDR" ~ "Laos",
                                        country_name == "Micronesia, Fed. Sts." ~ "Micronesia, Federated States of",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        country_name == "Russian Federation" ~ "Russia",
                                        country_name == "St. Kitts and Nevis" ~ "Saint Kitts and Nevis",
                                        country_name == "St. Vincent and the Grenadines" ~ "Saint Vincent and the Grenadines",
                                        country_name == "St. Lucia" ~ "Saint Lucia",
                                        country_name == "Slovak Republic" ~ "Slovakia",
                                        country_name == "Syrian Arab Republic" ~ "Syria",
                                        country_name == "Turkiye" ~ "Turkey",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States" ~ "U.S.",
                                        country_name == "Venezuela, RB" ~ "Venezuela",
                                        country_name == "Yemen, Rep." ~ "Yemen",
                                        TRUE ~ country_name)) %>%
        filter(year >= 2000) 


#/////////////


# inspect
agriculture_employment_as_share_of_total_employment
agriculture_employment_as_share_of_total_employment %>% glimpse()
agriculture_employment_as_share_of_total_employment %>% nrow() # 5852
agriculture_employment_as_share_of_total_employment %>% ncol() # 4

agriculture_employment_as_share_of_total_employment %>% count(year) %>% print(n = nrow(.))
agriculture_employment_as_share_of_total_employment %>% count(country_name)

# check names
country_crosswalk %>% anti_join(., agriculture_employment_as_share_of_total_employment, by = c("country" = "country_name")) %>% count(country) %>% print(n = nrow(.))
agriculture_employment_as_share_of_total_employment %>% filter(str_detect(string = country_name, pattern = regex("lucia", ignore_case = TRUE))) %>% distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("baham", ignore_case = TRUE)))


migrants %>% anti_join(., agriculture_employment_as_share_of_total_employment, by = c("origin_country" = "country_name")) %>% count(origin_country) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# add country_crosswalk
agriculture_employment_as_share_of_total_employment <- agriculture_employment_as_share_of_total_employment %>% left_join(., country_crosswalk %>% select(-country), by = "iso3") %>% 
        relocate(agriculture_employment_as_share_of_total_employment, .after = everything()) %>%
        rename(country = country_name)


#/////////////


# inspect
agriculture_employment_as_share_of_total_employment
agriculture_employment_as_share_of_total_employment %>% glimpse()
agriculture_employment_as_share_of_total_employment %>% nrow() # 5852
agriculture_employment_as_share_of_total_employment %>% ncol() # 16

agriculture_employment_as_share_of_total_employment %>% count(year) %>% print(n = nrow(.))
agriculture_employment_as_share_of_total_employment %>% count(country_name)

# inspect missing for e&E

# check most recent year of data availability
agriculture_employment_as_share_of_total_employment %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(!is.na(agriculture_employment_as_share_of_total_employment)) %>%
        group_by(country) %>%
        arrange(desc(year)) %>%
        slice(1) %>%
        ungroup() %>%
        select(country, year) %>% arrange(year)

# plot missing data
agriculture_employment_as_share_of_total_employment %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        select(country, year, agriculture_employment_as_share_of_total_employment) %>%
        group_by(country, year) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", variable, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )


# plot
agriculture_employment_as_share_of_total_employment %>%
        filter(mcp_grouping == "E&E Eurasia") %>%
        filter(year >= 2010) %>%
        select(country, year, agriculture_employment_as_share_of_total_employment) %>%
        ggplot(data = ., mapping = aes(x = year, y = agriculture_employment_as_share_of_total_employment, color = country)) + geom_line()


#/////////////////////////////////////////////////////////////////////////////////////////////


# test values

test_agriculture_employment_as_share_of_total_employment_values <- function() {
        
        # 1
        expect_equal(object = agriculture_employment_as_share_of_total_employment %>% filter(country == "Albania", year == 2015) %>% 
                             pull(agriculture_employment_as_share_of_total_employment),
                     expected = 41.2799987792969)
        
        # 2
        expect_equal(object = agriculture_employment_as_share_of_total_employment %>% filter(country == "Georgia", year == 2019) %>% 
                             pull(agriculture_employment_as_share_of_total_employment),
                     expected = 38.1500015258789)
        
        # 3
        expect_equal(object = agriculture_employment_as_share_of_total_employment %>% filter(country == "Belarus", year == 2018) %>% 
                             pull(agriculture_employment_as_share_of_total_employment),
                     expected = 11.3100004196166)
}
test_agriculture_employment_as_share_of_total_employment_values()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# agriculture_employment_as_share_of_total_employment %>%
#         write_csv(file = "data/world_bank/agriculture_employment_as_share_of_total_employment_20230115.csv")
agriculture_employment_as_share_of_total_employment <- read_csv(file = "data/world_bank/agriculture_employment_as_share_of_total_employment_20230115.csv")

# inspect
agriculture_employment_as_share_of_total_employment
agriculture_employment_as_share_of_total_employment %>% glimpse()
agriculture_employment_as_share_of_total_employment %>% nrow() # 5852
agriculture_employment_as_share_of_total_employment %>% ncol() # 16


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# world bank services_employment_as_share_of_total_employment ####

# https://data.worldbank.org/indicator/SL.SRV.EMPL.ZS

services_employment_as_share_of_total_employment <- read_excel(path = "data/world_bank/API_SL.SRV.EMPL.ZS_DS2_en_excel_v2_4771387.xls", sheet = "Data", skip = 3) %>%
        rename(country_name = `Country Name`, iso3 = `Country Code`) %>%
        select(-c(`Indicator Name`, `Indicator Code`)) %>%
        pivot_longer(cols = -c(country_name, iso3), names_to = "year", values_to = "services_employment_as_share_of_total_employment") %>%
        mutate(year = as.numeric(year),
               country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "Brunei Darussalam" ~ "Brunei",
                                        country_name == "Myanmar" ~ "Burma",
                                        country_name == "Congo, Dem. Rep." ~ "Congo (Kinshasa)",
                                        country_name == "Congo, Rep." ~ "Congo (Brazzaville)",
                                        country_name == "Czech Republic" ~ "Czechia",
                                        country_name == "Egypt, Arab Rep." ~ "Egypt",
                                        country_name == "Iran, Islamic Rep." ~ "Iran",
                                        country_name == "Korea, Dem. People's Rep." ~ "Korea, North",
                                        country_name == "Korea, Rep." ~ "Korea, South",
                                        country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                        country_name == "Lao PDR" ~ "Laos",
                                        country_name == "Micronesia, Fed. Sts." ~ "Micronesia, Federated States of",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        country_name == "Russian Federation" ~ "Russia",
                                        country_name == "St. Kitts and Nevis" ~ "Saint Kitts and Nevis",
                                        country_name == "St. Vincent and the Grenadines" ~ "Saint Vincent and the Grenadines",
                                        country_name == "St. Lucia" ~ "Saint Lucia",
                                        country_name == "Slovak Republic" ~ "Slovakia",
                                        country_name == "Syrian Arab Republic" ~ "Syria",
                                        country_name == "Turkiye" ~ "Turkey",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States" ~ "U.S.",
                                        country_name == "Venezuela, RB" ~ "Venezuela",
                                        country_name == "Yemen, Rep." ~ "Yemen",
                                        TRUE ~ country_name)) %>%
        filter(year >= 2000) 


#/////////////


# inspect
services_employment_as_share_of_total_employment
services_employment_as_share_of_total_employment %>% glimpse()
services_employment_as_share_of_total_employment %>% nrow() # 5852
services_employment_as_share_of_total_employment %>% ncol() # 4

services_employment_as_share_of_total_employment %>% count(year) %>% print(n = nrow(.))
services_employment_as_share_of_total_employment %>% count(country_name)

# check names
country_crosswalk %>% anti_join(., services_employment_as_share_of_total_employment, by = c("country" = "country_name")) %>% count(country) %>% print(n = nrow(.))
services_employment_as_share_of_total_employment %>% filter(str_detect(string = country_name, pattern = regex("lucia", ignore_case = TRUE))) %>% distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("baham", ignore_case = TRUE)))


migrants %>% anti_join(., services_employment_as_share_of_total_employment, by = c("origin_country" = "country_name")) %>% count(origin_country) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# add country_crosswalk
services_employment_as_share_of_total_employment <- services_employment_as_share_of_total_employment %>% left_join(., country_crosswalk %>% select(-country), by = "iso3") %>% 
        relocate(services_employment_as_share_of_total_employment, .after = everything()) %>%
        rename(country = country_name)


#/////////////


# inspect
services_employment_as_share_of_total_employment
services_employment_as_share_of_total_employment %>% glimpse()
services_employment_as_share_of_total_employment %>% nrow() # 5852
services_employment_as_share_of_total_employment %>% ncol() # 16

services_employment_as_share_of_total_employment %>% count(year) %>% print(n = nrow(.))
services_employment_as_share_of_total_employment %>% count(country_name)

# inspect missing for e&E

# check most recent year of data availability
services_employment_as_share_of_total_employment %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(!is.na(services_employment_as_share_of_total_employment)) %>%
        group_by(country) %>%
        arrange(desc(year)) %>%
        slice(1) %>%
        ungroup() %>%
        select(country, year) %>% arrange(year)

# plot missing data
services_employment_as_share_of_total_employment %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        select(country, year, services_employment_as_share_of_total_employment) %>%
        group_by(country, year) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", variable, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )


# plot
services_employment_as_share_of_total_employment %>%
        filter(mcp_grouping == "E&E Balkans") %>%
        filter(year >= 2010) %>%
        select(country, year, services_employment_as_share_of_total_employment) %>%
        ggplot(data = ., mapping = aes(x = year, y = services_employment_as_share_of_total_employment, color = country)) + geom_line()


#/////////////////////////////////////////////////////////////////////////////////////////////


# test values

test_services_employment_as_share_of_total_employment_values <- function() {
        
        # 1
        expect_equal(object = services_employment_as_share_of_total_employment %>% filter(country == "Albania", year == 2015) %>% 
                             pull(services_employment_as_share_of_total_employment),
                     expected = 40.0499992370605)
        
        # 2
        expect_equal(object = services_employment_as_share_of_total_employment %>% filter(country == "Georgia", year == 2019) %>% 
                             pull(services_employment_as_share_of_total_employment),
                     expected = 47.5900001525879)
        
        # 3
        expect_equal(object = services_employment_as_share_of_total_employment %>% filter(country == "Belarus", year == 2018) %>% 
                             pull(services_employment_as_share_of_total_employment),
                     expected = 58.060001373291)
}
test_services_employment_as_share_of_total_employment_values()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# services_employment_as_share_of_total_employment %>%
#         write_csv(file = "data/world_bank/services_employment_as_share_of_total_employment_20230115.csv")
services_employment_as_share_of_total_employment <- read_csv(file = "data/world_bank/services_employment_as_share_of_total_employment_20230115.csv")

# inspect
services_employment_as_share_of_total_employment
services_employment_as_share_of_total_employment %>% glimpse()
services_employment_as_share_of_total_employment %>% nrow() # 5852
services_employment_as_share_of_total_employment %>% ncol() # 16


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# world bank industry_employment_as_share_of_total_employment ####

# https://data.worldbank.org/indicator/SL.IND.EMPL.ZS

industry_employment_as_share_of_total_employment <- read_excel(path = "data/world_bank/API_SL.IND.EMPL.ZS_DS2_en_excel_v2_4771382.xls", sheet = "Data", skip = 3) %>%
        rename(country_name = `Country Name`, iso3 = `Country Code`) %>%
        select(-c(`Indicator Name`, `Indicator Code`)) %>%
        pivot_longer(cols = -c(country_name, iso3), names_to = "year", values_to = "industry_employment_as_share_of_total_employment") %>%
        mutate(year = as.numeric(year),
               country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "Brunei Darussalam" ~ "Brunei",
                                        country_name == "Myanmar" ~ "Burma",
                                        country_name == "Congo, Dem. Rep." ~ "Congo (Kinshasa)",
                                        country_name == "Congo, Rep." ~ "Congo (Brazzaville)",
                                        country_name == "Czech Republic" ~ "Czechia",
                                        country_name == "Egypt, Arab Rep." ~ "Egypt",
                                        country_name == "Iran, Islamic Rep." ~ "Iran",
                                        country_name == "Korea, Dem. People's Rep." ~ "Korea, North",
                                        country_name == "Korea, Rep." ~ "Korea, South",
                                        country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                        country_name == "Lao PDR" ~ "Laos",
                                        country_name == "Micronesia, Fed. Sts." ~ "Micronesia, Federated States of",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        country_name == "Russian Federation" ~ "Russia",
                                        country_name == "St. Kitts and Nevis" ~ "Saint Kitts and Nevis",
                                        country_name == "St. Vincent and the Grenadines" ~ "Saint Vincent and the Grenadines",
                                        country_name == "St. Lucia" ~ "Saint Lucia",
                                        country_name == "Slovak Republic" ~ "Slovakia",
                                        country_name == "Syrian Arab Republic" ~ "Syria",
                                        country_name == "Turkiye" ~ "Turkey",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States" ~ "U.S.",
                                        country_name == "Venezuela, RB" ~ "Venezuela",
                                        country_name == "Yemen, Rep." ~ "Yemen",
                                        TRUE ~ country_name)) %>%
        filter(year >= 2000) 


#/////////////


# inspect
industry_employment_as_share_of_total_employment
industry_employment_as_share_of_total_employment %>% glimpse()
industry_employment_as_share_of_total_employment %>% nrow() # 5852
industry_employment_as_share_of_total_employment %>% ncol() # 4

industry_employment_as_share_of_total_employment %>% count(year) %>% print(n = nrow(.))
industry_employment_as_share_of_total_employment %>% count(country_name)

# check names
country_crosswalk %>% anti_join(., industry_employment_as_share_of_total_employment, by = c("country" = "country_name")) %>% count(country) %>% print(n = nrow(.))
industry_employment_as_share_of_total_employment %>% filter(str_detect(string = country_name, pattern = regex("lucia", ignore_case = TRUE))) %>% distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("baham", ignore_case = TRUE)))


migrants %>% anti_join(., industry_employment_as_share_of_total_employment, by = c("origin_country" = "country_name")) %>% count(origin_country) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# add country_crosswalk
industry_employment_as_share_of_total_employment <- industry_employment_as_share_of_total_employment %>% left_join(., country_crosswalk %>% select(-country), by = "iso3") %>% 
        relocate(industry_employment_as_share_of_total_employment, .after = everything()) %>%
        rename(country = country_name)


#/////////////


# inspect
industry_employment_as_share_of_total_employment
industry_employment_as_share_of_total_employment %>% glimpse()
industry_employment_as_share_of_total_employment %>% nrow() # 5852
industry_employment_as_share_of_total_employment %>% ncol() # 16

industry_employment_as_share_of_total_employment %>% count(year) %>% print(n = nrow(.))
industry_employment_as_share_of_total_employment %>% count(country_name)

# inspect missing for e&E

# check most recent year of data availability
industry_employment_as_share_of_total_employment %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(!is.na(industry_employment_as_share_of_total_employment)) %>%
        group_by(country) %>%
        arrange(desc(year)) %>%
        slice(1) %>%
        ungroup() %>%
        select(country, year) %>% arrange(year)

# plot missing data
industry_employment_as_share_of_total_employment %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        select(country, year, industry_employment_as_share_of_total_employment) %>%
        group_by(country, year) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", variable, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )


# plot
industry_employment_as_share_of_total_employment %>%
        filter(mcp_grouping == "E&E Balkans") %>%
        filter(year >= 2010) %>%
        select(country, year, industry_employment_as_share_of_total_employment) %>%
        ggplot(data = ., mapping = aes(x = year, y = industry_employment_as_share_of_total_employment, color = country)) + geom_line()


#/////////////////////////////////////////////////////////////////////////////////////////////


# test values

test_industry_employment_as_share_of_total_employment_values <- function() {
        
        # 1
        expect_equal(object = industry_employment_as_share_of_total_employment %>% filter(country == "Albania", year == 2015) %>% 
                             pull(industry_employment_as_share_of_total_employment),
                     expected = 18.6700000762939)
        
        # 2
        expect_equal(object = industry_employment_as_share_of_total_employment %>% filter(country == "Georgia", year == 2019) %>% 
                             pull(industry_employment_as_share_of_total_employment),
                     expected = 14.2600002288818)
        
        # 3
        expect_equal(object = industry_employment_as_share_of_total_employment %>% filter(country == "Belarus", year == 2018) %>% 
                             pull(industry_employment_as_share_of_total_employment),
                     expected = 30.6299991607666)
}
test_industry_employment_as_share_of_total_employment_values()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# industry_employment_as_share_of_total_employment %>%
#         write_csv(file = "data/world_bank/industry_employment_as_share_of_total_employment_20230115.csv")
industry_employment_as_share_of_total_employment <- read_csv(file = "data/world_bank/industry_employment_as_share_of_total_employment_20230115.csv")

# inspect
industry_employment_as_share_of_total_employment
industry_employment_as_share_of_total_employment %>% glimpse()
industry_employment_as_share_of_total_employment %>% nrow() # 5852
industry_employment_as_share_of_total_employment %>% ncol() # 16



#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# world bank income_share_of_top_10_pct ####

# https://data.worldbank.org/indicator/SI.DST.10TH.10

income_share_of_top_10_pct <- read_excel(path = "data/world_bank/API_SI.DST.10TH.10_DS2_en_excel_v2_4773088.xls", sheet = "Data", skip = 3) %>%
        rename(country_name = `Country Name`, iso3 = `Country Code`) %>%
        select(-c(`Indicator Name`, `Indicator Code`)) %>%
        pivot_longer(cols = -c(country_name, iso3), names_to = "year", values_to = "income_share_of_top_10_pct") %>%
        mutate(year = as.numeric(year),
               country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "Brunei Darussalam" ~ "Brunei",
                                        country_name == "Myanmar" ~ "Burma",
                                        country_name == "Congo, Dem. Rep." ~ "Congo (Kinshasa)",
                                        country_name == "Congo, Rep." ~ "Congo (Brazzaville)",
                                        country_name == "Czech Republic" ~ "Czechia",
                                        country_name == "Egypt, Arab Rep." ~ "Egypt",
                                        country_name == "Iran, Islamic Rep." ~ "Iran",
                                        country_name == "Korea, Dem. People's Rep." ~ "Korea, North",
                                        country_name == "Korea, Rep." ~ "Korea, South",
                                        country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                        country_name == "Lao PDR" ~ "Laos",
                                        country_name == "Micronesia, Fed. Sts." ~ "Micronesia, Federated States of",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        country_name == "Russian Federation" ~ "Russia",
                                        country_name == "St. Kitts and Nevis" ~ "Saint Kitts and Nevis",
                                        country_name == "St. Vincent and the Grenadines" ~ "Saint Vincent and the Grenadines",
                                        country_name == "St. Lucia" ~ "Saint Lucia",
                                        country_name == "Slovak Republic" ~ "Slovakia",
                                        country_name == "Syrian Arab Republic" ~ "Syria",
                                        country_name == "Turkiye" ~ "Turkey",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States" ~ "U.S.",
                                        country_name == "Venezuela, RB" ~ "Venezuela",
                                        country_name == "Yemen, Rep." ~ "Yemen",
                                        TRUE ~ country_name)) %>%
        filter(year >= 2000) 


#/////////////


# inspect
income_share_of_top_10_pct
income_share_of_top_10_pct %>% glimpse()
income_share_of_top_10_pct %>% nrow() # 5852
income_share_of_top_10_pct %>% ncol() # 4

income_share_of_top_10_pct %>% count(year) %>% print(n = nrow(.))
income_share_of_top_10_pct %>% count(country_name)

# check names
country_crosswalk %>% anti_join(., income_share_of_top_10_pct, by = c("country" = "country_name")) %>% count(country) %>% print(n = nrow(.))
income_share_of_top_10_pct %>% filter(str_detect(string = country_name, pattern = regex("lucia", ignore_case = TRUE))) %>% distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("baham", ignore_case = TRUE)))


migrants %>% anti_join(., income_share_of_top_10_pct, by = c("origin_country" = "country_name")) %>% count(origin_country) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# add country_crosswalk
income_share_of_top_10_pct <- income_share_of_top_10_pct %>% left_join(., country_crosswalk %>% select(-country), by = "iso3") %>% 
        relocate(income_share_of_top_10_pct, .after = everything()) %>%
        rename(country = country_name)


#/////////////


# inspect
income_share_of_top_10_pct
income_share_of_top_10_pct %>% glimpse()
income_share_of_top_10_pct %>% nrow() # 5852
income_share_of_top_10_pct %>% ncol() # 16

income_share_of_top_10_pct %>% count(year) %>% print(n = nrow(.))
income_share_of_top_10_pct %>% count(country_name)

# inspect missing for e&E

# check most recent year of data availability
income_share_of_top_10_pct %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(!is.na(income_share_of_top_10_pct)) %>%
        group_by(country) %>%
        arrange(desc(year)) %>%
        slice(1) %>%
        ungroup() %>%
        select(country, year) %>% arrange(year)

# plot missing data
income_share_of_top_10_pct %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        select(country, year, income_share_of_top_10_pct) %>%
        group_by(country, year) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", variable, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )


# plot
income_share_of_top_10_pct %>%
        filter(mcp_grouping == "E&E Balkans") %>%
        filter(year >= 2010) %>%
        select(country, year, income_share_of_top_10_pct) %>%
        ggplot(data = ., mapping = aes(x = year, y = income_share_of_top_10_pct, color = country)) + geom_line()


#/////////////////////////////////////////////////////////////////////////////////////////////


# test values

test_income_share_of_top_10_pct_values <- function() {
        
        # 1
        expect_equal(object = income_share_of_top_10_pct %>% filter(country == "Albania", year == 2015) %>% 
                             pull(income_share_of_top_10_pct),
                     expected = 24.8)
        
        # 2
        expect_equal(object = income_share_of_top_10_pct %>% filter(country == "Georgia", year == 2019) %>% 
                             pull(income_share_of_top_10_pct),
                     expected = 27.6)
        
        # 3
        expect_equal(object = income_share_of_top_10_pct %>% filter(country == "Belarus", year == 2018) %>% 
                             pull(income_share_of_top_10_pct),
                     expected = 21.4)
}
test_income_share_of_top_10_pct_values()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# income_share_of_top_10_pct %>%
#         write_csv(file = "data/world_bank/income_share_of_top_10_pct_20230115.csv")
income_share_of_top_10_pct <- read_csv(file = "data/world_bank/income_share_of_top_10_pct_20230115.csv")

# inspect
income_share_of_top_10_pct
income_share_of_top_10_pct %>% glimpse()
income_share_of_top_10_pct %>% nrow() # 5852
income_share_of_top_10_pct %>% ncol() # 16


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# world bank food_insecurity_rate ####

# https://data.worldbank.org/indicator/SN.ITK.MSFI.ZS

food_insecurity_rate <- read_excel(path = "data/world_bank/API_SN.ITK.MSFI.ZS_DS2_en_excel_v2_4770401.xls", sheet = "Data", skip = 3) %>%
        rename(country_name = `Country Name`, iso3 = `Country Code`) %>%
        select(-c(`Indicator Name`, `Indicator Code`)) %>%
        pivot_longer(cols = -c(country_name, iso3), names_to = "year", values_to = "food_insecurity_rate") %>%
        mutate(year = as.numeric(year),
               country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "Brunei Darussalam" ~ "Brunei",
                                        country_name == "Myanmar" ~ "Burma",
                                        country_name == "Congo, Dem. Rep." ~ "Congo (Kinshasa)",
                                        country_name == "Congo, Rep." ~ "Congo (Brazzaville)",
                                        country_name == "Czech Republic" ~ "Czechia",
                                        country_name == "Egypt, Arab Rep." ~ "Egypt",
                                        country_name == "Iran, Islamic Rep." ~ "Iran",
                                        country_name == "Korea, Dem. People's Rep." ~ "Korea, North",
                                        country_name == "Korea, Rep." ~ "Korea, South",
                                        country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                        country_name == "Lao PDR" ~ "Laos",
                                        country_name == "Micronesia, Fed. Sts." ~ "Micronesia, Federated States of",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        country_name == "Russian Federation" ~ "Russia",
                                        country_name == "St. Kitts and Nevis" ~ "Saint Kitts and Nevis",
                                        country_name == "St. Vincent and the Grenadines" ~ "Saint Vincent and the Grenadines",
                                        country_name == "St. Lucia" ~ "Saint Lucia",
                                        country_name == "Slovak Republic" ~ "Slovakia",
                                        country_name == "Syrian Arab Republic" ~ "Syria",
                                        country_name == "Turkiye" ~ "Turkey",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States" ~ "U.S.",
                                        country_name == "Venezuela, RB" ~ "Venezuela",
                                        country_name == "Yemen, Rep." ~ "Yemen",
                                        TRUE ~ country_name)) %>%
        filter(year >= 2000) 


#/////////////


# inspect
food_insecurity_rate
food_insecurity_rate %>% glimpse()
food_insecurity_rate %>% nrow() # 5852
food_insecurity_rate %>% ncol() # 4

food_insecurity_rate %>% count(year) %>% print(n = nrow(.))
food_insecurity_rate %>% count(country_name)

# check names
country_crosswalk %>% anti_join(., food_insecurity_rate, by = c("country" = "country_name")) %>% count(country) %>% print(n = nrow(.))
food_insecurity_rate %>% filter(str_detect(string = country_name, pattern = regex("lucia", ignore_case = TRUE))) %>% distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("baham", ignore_case = TRUE)))


migrants %>% anti_join(., food_insecurity_rate, by = c("origin_country" = "country_name")) %>% count(origin_country) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# add country_crosswalk
food_insecurity_rate <- food_insecurity_rate %>% left_join(., country_crosswalk %>% select(-country), by = "iso3") %>% 
        relocate(food_insecurity_rate, .after = everything()) %>%
        rename(country = country_name)


#/////////////


# inspect
food_insecurity_rate
food_insecurity_rate %>% glimpse()
food_insecurity_rate %>% nrow() # 5852
food_insecurity_rate %>% ncol() # 16

food_insecurity_rate %>% count(year) %>% print(n = nrow(.))
food_insecurity_rate %>% count(country_name)

# inspect missing for e&E

# check most recent year of data availability
food_insecurity_rate %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(!is.na(food_insecurity_rate)) %>%
        group_by(country) %>%
        arrange(desc(year)) %>%
        slice(1) %>%
        ungroup() %>%
        select(country, year) %>% arrange(year)

# plot missing data
food_insecurity_rate %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        select(country, year, food_insecurity_rate) %>%
        group_by(country, year) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", variable, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )


# plot
food_insecurity_rate %>%
        # filter(mcp_grouping == "E&E Balkans") %>%
        filter(mcp_grouping == "E&E Eurasia") %>%
        filter(year >= 2010) %>%
        select(country, year, food_insecurity_rate) %>%
        ggplot(data = ., mapping = aes(x = year, y = food_insecurity_rate, color = country)) + geom_line(size = 1)


#/////////////////////////////////////////////////////////////////////////////////////////////


# test values

test_food_insecurity_rate_values <- function() {
        
        # 1
        expect_equal(object = food_insecurity_rate %>% filter(country == "Albania", year == 2015) %>% 
                             pull(food_insecurity_rate),
                     expected = 38.8)
        
        # 2
        expect_equal(object = food_insecurity_rate %>% filter(country == "Georgia", year == 2019) %>% 
                             pull(food_insecurity_rate),
                     expected = 39.7)
        
        # 3
        expect_equal(object = food_insecurity_rate %>% filter(country == "Azerbaijan", year == 2020) %>% 
                             pull(food_insecurity_rate),
                     expected = 9.5)
}
test_food_insecurity_rate_values()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# food_insecurity_rate %>%
#         write_csv(file = "data/world_bank/food_insecurity_rate_20230115.csv")
food_insecurity_rate <- read_csv(file = "data/world_bank/food_insecurity_rate_20230115.csv")

# inspect
food_insecurity_rate
food_insecurity_rate %>% glimpse()
food_insecurity_rate %>% nrow() # 5852
food_insecurity_rate %>% ncol() # 16


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# world bank income_share_of_bottom_20_pct ####

# https://data.worldbank.org/indicator/SI.DST.FRST.20?view=chart

income_share_of_bottom_20_pct <- read_excel(path = "data/world_bank/API_SI.DST.FRST.20_DS2_en_excel_v2_4770748.xls", sheet = "Data", skip = 3) %>%
        rename(country_name = `Country Name`, iso3 = `Country Code`) %>%
        select(-c(`Indicator Name`, `Indicator Code`)) %>%
        pivot_longer(cols = -c(country_name, iso3), names_to = "year", values_to = "income_share_of_bottom_20_pct") %>%
        mutate(year = as.numeric(year),
               country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "Brunei Darussalam" ~ "Brunei",
                                        country_name == "Myanmar" ~ "Burma",
                                        country_name == "Congo, Dem. Rep." ~ "Congo (Kinshasa)",
                                        country_name == "Congo, Rep." ~ "Congo (Brazzaville)",
                                        country_name == "Czech Republic" ~ "Czechia",
                                        country_name == "Egypt, Arab Rep." ~ "Egypt",
                                        country_name == "Iran, Islamic Rep." ~ "Iran",
                                        country_name == "Korea, Dem. People's Rep." ~ "Korea, North",
                                        country_name == "Korea, Rep." ~ "Korea, South",
                                        country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                        country_name == "Lao PDR" ~ "Laos",
                                        country_name == "Micronesia, Fed. Sts." ~ "Micronesia, Federated States of",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        country_name == "Russian Federation" ~ "Russia",
                                        country_name == "St. Kitts and Nevis" ~ "Saint Kitts and Nevis",
                                        country_name == "St. Vincent and the Grenadines" ~ "Saint Vincent and the Grenadines",
                                        country_name == "St. Lucia" ~ "Saint Lucia",
                                        country_name == "Slovak Republic" ~ "Slovakia",
                                        country_name == "Syrian Arab Republic" ~ "Syria",
                                        country_name == "Turkiye" ~ "Turkey",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States" ~ "U.S.",
                                        country_name == "Venezuela, RB" ~ "Venezuela",
                                        country_name == "Yemen, Rep." ~ "Yemen",
                                        TRUE ~ country_name)) %>%
        filter(year >= 2000) 


#/////////////


# inspect
income_share_of_bottom_20_pct
income_share_of_bottom_20_pct %>% glimpse()
income_share_of_bottom_20_pct %>% nrow() # 5852
income_share_of_bottom_20_pct %>% ncol() # 4

income_share_of_bottom_20_pct %>% count(year) %>% print(n = nrow(.))
income_share_of_bottom_20_pct %>% count(country_name)

# check names
country_crosswalk %>% anti_join(., income_share_of_bottom_20_pct, by = c("country" = "country_name")) %>% count(country) %>% print(n = nrow(.))
income_share_of_bottom_20_pct %>% filter(str_detect(string = country_name, pattern = regex("lucia", ignore_case = TRUE))) %>% distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("baham", ignore_case = TRUE)))


migrants %>% anti_join(., income_share_of_bottom_20_pct, by = c("origin_country" = "country_name")) %>% count(origin_country) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# add country_crosswalk
income_share_of_bottom_20_pct <- income_share_of_bottom_20_pct %>% left_join(., country_crosswalk %>% select(-country), by = "iso3") %>% 
        relocate(income_share_of_bottom_20_pct, .after = everything()) %>%
        rename(country = country_name)


#/////////////


# inspect
income_share_of_bottom_20_pct
income_share_of_bottom_20_pct %>% glimpse()
income_share_of_bottom_20_pct %>% nrow() # 5852
income_share_of_bottom_20_pct %>% ncol() # 16

income_share_of_bottom_20_pct %>% count(year) %>% print(n = nrow(.))
income_share_of_bottom_20_pct %>% count(country_name)

# inspect missing for e&E

# check most recent year of data availability
income_share_of_bottom_20_pct %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(!is.na(income_share_of_bottom_20_pct)) %>%
        group_by(country) %>%
        arrange(desc(year)) %>%
        slice(1) %>%
        ungroup() %>%
        select(country, year) %>% arrange(year)

# plot missing data
income_share_of_bottom_20_pct %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        select(country, year, income_share_of_bottom_20_pct) %>%
        group_by(country, year) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", variable, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )


# plot
income_share_of_bottom_20_pct %>%
        # filter(mcp_grouping == "E&E Balkans") %>%
        filter(mcp_grouping == "E&E Eurasia") %>%
        filter(year >= 2010) %>%
        select(country, year, income_share_of_bottom_20_pct) %>%
        ggplot(data = ., mapping = aes(x = year, y = income_share_of_bottom_20_pct, color = country)) + geom_line(size = 1)


#/////////////////////////////////////////////////////////////////////////////////////////////


# test values

test_income_share_of_bottom_20_pct_values <- function() {
        
        # 1
        expect_equal(object = income_share_of_bottom_20_pct %>% filter(country == "Albania", year == 2015) %>% 
                             pull(income_share_of_bottom_20_pct),
                     expected = 7.7)
        
        # 2
        expect_equal(object = income_share_of_bottom_20_pct %>% filter(country == "Georgia", year == 2019) %>% 
                             pull(income_share_of_bottom_20_pct),
                     expected = 6.8)
        
        # 3
        expect_equal(object = income_share_of_bottom_20_pct %>% filter(country == "Armenia", year == 2020) %>% 
                             pull(income_share_of_bottom_20_pct),
                     expected = 10.2)
}
test_income_share_of_bottom_20_pct_values()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# income_share_of_bottom_20_pct %>%
#         write_csv(file = "data/world_bank/income_share_of_bottom_20_pct_20230115.csv")
income_share_of_bottom_20_pct <- read_csv(file = "data/world_bank/income_share_of_bottom_20_pct_20230115.csv")

# inspect
income_share_of_bottom_20_pct
income_share_of_bottom_20_pct %>% glimpse()
income_share_of_bottom_20_pct %>% nrow() # 5852
income_share_of_bottom_20_pct %>% ncol() # 16


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# world bank external_debt_stocks_as_share_of_gni ####

# https://data.worldbank.org/indicator/DT.DOD.DECT.GN.ZS?view=chart

external_debt_stocks_as_share_of_gni <- read_excel(path = "data/world_bank/API_DT.DOD.DECT.GN.ZS_DS2_en_excel_v2_4773510.xls", sheet = "Data", skip = 3) %>%
        rename(country_name = `Country Name`, iso3 = `Country Code`) %>%
        select(-c(`Indicator Name`, `Indicator Code`)) %>%
        pivot_longer(cols = -c(country_name, iso3), names_to = "year", values_to = "external_debt_stocks_as_share_of_gni") %>%
        mutate(year = as.numeric(year),
               country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "Brunei Darussalam" ~ "Brunei",
                                        country_name == "Myanmar" ~ "Burma",
                                        country_name == "Congo, Dem. Rep." ~ "Congo (Kinshasa)",
                                        country_name == "Congo, Rep." ~ "Congo (Brazzaville)",
                                        country_name == "Czech Republic" ~ "Czechia",
                                        country_name == "Egypt, Arab Rep." ~ "Egypt",
                                        country_name == "Iran, Islamic Rep." ~ "Iran",
                                        country_name == "Korea, Dem. People's Rep." ~ "Korea, North",
                                        country_name == "Korea, Rep." ~ "Korea, South",
                                        country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                        country_name == "Lao PDR" ~ "Laos",
                                        country_name == "Micronesia, Fed. Sts." ~ "Micronesia, Federated States of",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        country_name == "Russian Federation" ~ "Russia",
                                        country_name == "St. Kitts and Nevis" ~ "Saint Kitts and Nevis",
                                        country_name == "St. Vincent and the Grenadines" ~ "Saint Vincent and the Grenadines",
                                        country_name == "St. Lucia" ~ "Saint Lucia",
                                        country_name == "Slovak Republic" ~ "Slovakia",
                                        country_name == "Syrian Arab Republic" ~ "Syria",
                                        country_name == "Turkiye" ~ "Turkey",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States" ~ "U.S.",
                                        country_name == "Venezuela, RB" ~ "Venezuela",
                                        country_name == "Yemen, Rep." ~ "Yemen",
                                        TRUE ~ country_name)) %>%
        filter(year >= 2000) 


#/////////////


# inspect
external_debt_stocks_as_share_of_gni
external_debt_stocks_as_share_of_gni %>% glimpse()
external_debt_stocks_as_share_of_gni %>% nrow() # 5852
external_debt_stocks_as_share_of_gni %>% ncol() # 4

external_debt_stocks_as_share_of_gni %>% count(year) %>% print(n = nrow(.))
external_debt_stocks_as_share_of_gni %>% count(country_name)

# check names
country_crosswalk %>% anti_join(., external_debt_stocks_as_share_of_gni, by = c("country" = "country_name")) %>% count(country) %>% print(n = nrow(.))
external_debt_stocks_as_share_of_gni %>% filter(str_detect(string = country_name, pattern = regex("lucia", ignore_case = TRUE))) %>% distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("baham", ignore_case = TRUE)))


migrants %>% anti_join(., external_debt_stocks_as_share_of_gni, by = c("origin_country" = "country_name")) %>% count(origin_country) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# add country_crosswalk
external_debt_stocks_as_share_of_gni <- external_debt_stocks_as_share_of_gni %>% left_join(., country_crosswalk %>% select(-country), by = "iso3") %>% 
        relocate(external_debt_stocks_as_share_of_gni, .after = everything()) %>%
        rename(country = country_name)


#/////////////


# inspect
external_debt_stocks_as_share_of_gni
external_debt_stocks_as_share_of_gni %>% glimpse()
external_debt_stocks_as_share_of_gni %>% nrow() # 5852
external_debt_stocks_as_share_of_gni %>% ncol() # 16

external_debt_stocks_as_share_of_gni %>% count(year) %>% print(n = nrow(.))
external_debt_stocks_as_share_of_gni %>% count(country_name)

# inspect missing for e&E

# check most recent year of data availability
external_debt_stocks_as_share_of_gni %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(!is.na(external_debt_stocks_as_share_of_gni)) %>%
        group_by(country) %>%
        arrange(desc(year)) %>%
        slice(1) %>%
        ungroup() %>%
        select(country, year) %>% arrange(year)

# plot missing data
external_debt_stocks_as_share_of_gni %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        select(country, year, external_debt_stocks_as_share_of_gni) %>%
        group_by(country, year) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", variable, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )


# plot
external_debt_stocks_as_share_of_gni %>%
        # filter(mcp_grouping == "E&E Balkans") %>%
        filter(mcp_grouping == "E&E Eurasia") %>%
        filter(year >= 2010) %>%
        select(country, year, external_debt_stocks_as_share_of_gni) %>%
        ggplot(data = ., mapping = aes(x = year, y = external_debt_stocks_as_share_of_gni, color = country)) + geom_line(size = 1)


#/////////////////////////////////////////////////////////////////////////////////////////////


# test values

test_external_debt_stocks_as_share_of_gni_values <- function() {
        
        # 1
        expect_equal(object = external_debt_stocks_as_share_of_gni %>% filter(country == "Albania", year == 2015) %>% 
                             pull(external_debt_stocks_as_share_of_gni),
                     expected = 73.313551445325)
        
        # 2
        expect_equal(object = external_debt_stocks_as_share_of_gni %>% filter(country == "Georgia", year == 2019) %>% 
                             pull(external_debt_stocks_as_share_of_gni),
                     expected = 112.365510760398)
        
        # 3
        expect_equal(object = external_debt_stocks_as_share_of_gni %>% filter(country == "Armenia", year == 2020) %>% 
                             pull(external_debt_stocks_as_share_of_gni),
                     expected = 101.244573393431)
}
test_external_debt_stocks_as_share_of_gni_values()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# external_debt_stocks_as_share_of_gni %>%
#         write_csv(file = "data/world_bank/external_debt_stocks_as_share_of_gni_20230115.csv")
external_debt_stocks_as_share_of_gni <- read_csv(file = "data/world_bank/external_debt_stocks_as_share_of_gni_20230115.csv")

# inspect
external_debt_stocks_as_share_of_gni
external_debt_stocks_as_share_of_gni %>% glimpse()
external_debt_stocks_as_share_of_gni %>% nrow() # 5852
external_debt_stocks_as_share_of_gni %>% ncol() # 16


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# world bank total_debt_service_as_share_of_exports ####

# https://data.worldbank.org/indicator/DT.TDS.DECT.EX.ZS?view=chart

total_debt_service_as_share_of_exports <- read_excel(path = "data/world_bank/API_DT.TDS.DECT.EX.ZS_DS2_en_excel_v2_4773644.xls", sheet = "Data", skip = 3) %>%
        rename(country_name = `Country Name`, iso3 = `Country Code`) %>%
        select(-c(`Indicator Name`, `Indicator Code`)) %>%
        pivot_longer(cols = -c(country_name, iso3), names_to = "year", values_to = "total_debt_service_as_share_of_exports") %>%
        mutate(year = as.numeric(year),
               country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "Brunei Darussalam" ~ "Brunei",
                                        country_name == "Myanmar" ~ "Burma",
                                        country_name == "Congo, Dem. Rep." ~ "Congo (Kinshasa)",
                                        country_name == "Congo, Rep." ~ "Congo (Brazzaville)",
                                        country_name == "Czech Republic" ~ "Czechia",
                                        country_name == "Egypt, Arab Rep." ~ "Egypt",
                                        country_name == "Iran, Islamic Rep." ~ "Iran",
                                        country_name == "Korea, Dem. People's Rep." ~ "Korea, North",
                                        country_name == "Korea, Rep." ~ "Korea, South",
                                        country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                        country_name == "Lao PDR" ~ "Laos",
                                        country_name == "Micronesia, Fed. Sts." ~ "Micronesia, Federated States of",
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        country_name == "Russian Federation" ~ "Russia",
                                        country_name == "St. Kitts and Nevis" ~ "Saint Kitts and Nevis",
                                        country_name == "St. Vincent and the Grenadines" ~ "Saint Vincent and the Grenadines",
                                        country_name == "St. Lucia" ~ "Saint Lucia",
                                        country_name == "Slovak Republic" ~ "Slovakia",
                                        country_name == "Syrian Arab Republic" ~ "Syria",
                                        country_name == "Turkiye" ~ "Turkey",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States" ~ "U.S.",
                                        country_name == "Venezuela, RB" ~ "Venezuela",
                                        country_name == "Yemen, Rep." ~ "Yemen",
                                        TRUE ~ country_name)) %>%
        filter(year >= 2000) 


#/////////////


# inspect
total_debt_service_as_share_of_exports
total_debt_service_as_share_of_exports %>% glimpse()
total_debt_service_as_share_of_exports %>% nrow() # 5852
total_debt_service_as_share_of_exports %>% ncol() # 4

total_debt_service_as_share_of_exports %>% count(year) %>% print(n = nrow(.))
total_debt_service_as_share_of_exports %>% count(country_name)

# check names
country_crosswalk %>% anti_join(., total_debt_service_as_share_of_exports, by = c("country" = "country_name")) %>% count(country) %>% print(n = nrow(.))
total_debt_service_as_share_of_exports %>% filter(str_detect(string = country_name, pattern = regex("lucia", ignore_case = TRUE))) %>% distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("baham", ignore_case = TRUE)))


migrants %>% anti_join(., total_debt_service_as_share_of_exports, by = c("origin_country" = "country_name")) %>% count(origin_country) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# add country_crosswalk
total_debt_service_as_share_of_exports <- total_debt_service_as_share_of_exports %>% left_join(., country_crosswalk %>% select(-country), by = "iso3") %>% 
        relocate(total_debt_service_as_share_of_exports, .after = everything()) %>%
        rename(country = country_name)


#/////////////


# inspect
total_debt_service_as_share_of_exports
total_debt_service_as_share_of_exports %>% glimpse()
total_debt_service_as_share_of_exports %>% nrow() # 5852
total_debt_service_as_share_of_exports %>% ncol() # 16

total_debt_service_as_share_of_exports %>% count(year) %>% print(n = nrow(.))
total_debt_service_as_share_of_exports %>% count(country_name)

# inspect missing for e&E

# check most recent year of data availability
total_debt_service_as_share_of_exports %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        filter(!is.na(total_debt_service_as_share_of_exports)) %>%
        group_by(country) %>%
        arrange(desc(year)) %>%
        slice(1) %>%
        ungroup() %>%
        select(country, year) %>% arrange(year)

# plot missing data
total_debt_service_as_share_of_exports %>% 
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
        select(country, year, total_debt_service_as_share_of_exports) %>%
        group_by(country, year) %>%
        miss_var_summary() %>%
        ungroup() %>%
        arrange(desc(n_miss)) %>%
        unite(col = "var", variable, year) %>%
        ggplot(data = ., mapping = aes(x = var, y = country, fill = pct_miss)) +
        geom_tile() + 
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 6, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1)
        )


# plot
total_debt_service_as_share_of_exports %>%
        # filter(mcp_grouping == "E&E Balkans") %>%
        filter(mcp_grouping == "E&E Eurasia") %>%
        filter(year >= 2010) %>%
        select(country, year, total_debt_service_as_share_of_exports) %>%
        ggplot(data = ., mapping = aes(x = year, y = total_debt_service_as_share_of_exports, color = country)) + geom_line(size = 1)


#/////////////////////////////////////////////////////////////////////////////////////////////


# test values

test_total_debt_service_as_share_of_exports_values <- function() {
        
        # 1
        expect_equal(object = total_debt_service_as_share_of_exports %>% filter(country == "Albania", year == 2015) %>% 
                             pull(total_debt_service_as_share_of_exports),
                     expected = 33.5242160000422)
        
        # 2
        expect_equal(object = total_debt_service_as_share_of_exports %>% filter(country == "Georgia", year == 2019) %>% 
                             pull(total_debt_service_as_share_of_exports),
                     expected = 22.5470245967007)
        
        # 3
        expect_equal(object = total_debt_service_as_share_of_exports %>% filter(country == "Armenia", year == 2020) %>% 
                             pull(total_debt_service_as_share_of_exports),
                     expected = 45.951991828642)
}
test_total_debt_service_as_share_of_exports_values()


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
# total_debt_service_as_share_of_exports %>%
#         write_csv(file = "data/world_bank/total_debt_service_as_share_of_exports_20230115.csv")
total_debt_service_as_share_of_exports <- read_csv(file = "data/world_bank/total_debt_service_as_share_of_exports_20230115.csv")

# inspect
total_debt_service_as_share_of_exports
total_debt_service_as_share_of_exports %>% glimpse()
total_debt_service_as_share_of_exports %>% nrow() # 5852
total_debt_service_as_share_of_exports %>% ncol() # 16






























