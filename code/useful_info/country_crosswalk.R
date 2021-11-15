library(tidyverse)
library(data360r)
library(rvest)

# setwd
setwd("C:/Users/sdevine/Desktop/usaid/mcp/useful_info/country_crosswalk")


#///////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////


# scrape state dept - independent states of the world table
# https://www.state.gov/independent-states-in-the-world/
html_page <- read_html("https://www.state.gov/independent-states-in-the-world/")
html_page

# extract main table
table <- html_page %>% html_nodes(css = "table") %>% .[1] %>% html_table() %>% .[[1]] %>% as_tibble()
table
table %>% glimpse()

# clean table
names(table) <- c("state_short_name", "state_long_name", "state_genc_2a_code", "state_genc_3a_code", "capital")
state <- table %>% 
        slice(-1) %>%
        select(state_short_name, state_long_name, state_genc_2a_code, state_genc_3a_code) %>%
        mutate(state_short_name = str_replace(string = state_short_name, pattern = "\\*", replacement = ""),
                 state_short_name = str_replace(string = state_short_name, pattern = "\\+", replacement = ""),
                 state_short_name = str_replace(string = state_short_name, pattern = "\\n", replacement = " "),
                 state_short_name = str_replace(string = state_short_name, pattern = "\\(see note [0-9]\\)", replacement = ""),
                 state_short_name = case_when(state_genc_3a_code == "CIV" ~ "Cote d'Ivoire", 
                                              TRUE ~ state_short_name),
                 state_long_name = case_when(state_genc_3a_code == "CIV" ~ "Cote d'Ivoire", 
                                            TRUE ~ state_long_name),
                 state_short_name = str_trim(string = state_short_name, side = "both"),
               state_long_name = str_replace(string = state_short_name, pattern = "\\n", replacement = " "),
               state_independent_flag = 1)


#///////////////////////////////


# inspect table
# 195 independent countries as of 4/19/2021
state
state %>% glimpse()
state %>% nrow() # 195
state %>% distinct(state_short_name) %>% nrow() # 195
state %>% print(n = nrow(.))


#///////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////


# get world bank country metadata using world bank official r package 
# https://github.com/mrpsonglao/data360r
# https://blogs.worldbank.org/opendata/introducing-data360r-data-power-r
# note that the data360r package was last updated in april 2020, so doesn't reflect changes since then (e.g. romania is now HIC)

world_bank <- get_metadata360(metadata_type = 'countries') %>% as_tibble()
world_bank %>% glimpse()
world_bank %>% nrow()


#///////////////////////


# inspect world_bank
state %>% anti_join(., world_bank, by = c("state_short_name" = "name")) %>% print(n = nrow(.))
world_bank %>% filter(str_detect(string = name, pattern = regex("yemen", ignore_case = TRUE)))


#///////////////////////


# clean world_bank to make them match state for independent countries
# clean kosovo's iso3 to match july 2020 world_bank_income_level download ("XKX"), instead of "KSV" in data360r
# countryconverter r package also updated to XKX: https://github.com/konstantinstadler/country_converter/issues/29
world_bank <- world_bank %>% rename(world_bank_name = name) %>%
        select(world_bank_name, iso2, iso3) %>%
        mutate(world_bank_name = case_when(world_bank_name == "Brunei Darussalam" ~ "Brunei",
                                   world_bank_name == "Myanmar" ~ "Burma",
                                   world_bank_name == "Congo, Dem. Rep." ~ "Congo (Kinshasa)",
                                   world_bank_name == "Congo, Rep." ~ "Congo (Brazzaville)",
                                   world_bank_name == "Congo, Rep." ~ "Congo (Brazzaville)",
                                   world_bank_name == "Czech Republic" ~ "Czechia",
                                   world_bank_name == "Egypt, Arab Rep." ~ "Egypt",
                                   world_bank_name == "Swaziland" ~ "Eswatini",
                                   world_bank_name == "Swaziland" ~ "Eswatini",
                                   world_bank_name == "Iran, Islamic Rep." ~ "Iran",
                                   world_bank_name == "Korea, Dem. People’s Rep." ~ "Korea, North",
                                   world_bank_name == "Korea, Rep." ~ "Korea, South",
                                   world_bank_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                   world_bank_name == "Lao PDR" ~ "Laos",
                                   world_bank_name == "Micronesia, Fed. Sts." ~ "Micronesia, Federated States of",
                                   world_bank_name == "Macedonia, FYR" ~ "North Macedonia",
                                   world_bank_name == "Russian Federation" ~ "Russia",
                                   world_bank_name == "St. Kitts and Nevis" ~ "Saint Kitts and Nevis",
                                   world_bank_name == "St. Lucia" ~ "Saint Lucia",
                                   world_bank_name == "St. Vincent and the Grenadines" ~ "Saint Vincent and the Grenadines",
                                   world_bank_name == "Slovak Republic" ~ "Slovakia",
                                   world_bank_name == "Syrian Arab Republic" ~ "Syria",
                                   world_bank_name == "Venezuela, RB" ~ "Venezuela",
                                   world_bank_name == "Yemen, Rep." ~ "Yemen",
                                   TRUE ~ world_bank_name),
               iso3 = case_when(world_bank_name == "Kosovo" ~ "XKX", 
                                TRUE ~ iso3))


#///////////////////////


# inspect world_bank
state %>% anti_join(., world_bank, by = c("state_short_name" = "world_bank_name")) %>% print(n = nrow(.))


#///////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////


# combine state and world_bank
country_crosswalk <- world_bank %>% left_join(., state, by = c("world_bank_name" = "state_short_name"), keep = TRUE) %>%
        mutate(state_independent_flag = case_when(is.na(state_independent_flag) ~ 0, TRUE ~ state_independent_flag))

# add holy see record
holy_see <- tibble(world_bank_name = "Holy See", iso2 = NA_character_, iso3 = NA_character_, 
                   state_short_name = "Holy See",
                   state_long_name = "Holy See", state_genc_2a_code = "VA", state_genc_3a_code = "VAT",
                   state_independent_flag = 1)
holy_see

# add holy see record to country_crosswalk
country_crosswalk <- country_crosswalk %>% bind_rows(., holy_see)


#/////////////////////////


# inspect
country_crosswalk
country_crosswalk %>% glimpse()
country_crosswalk %>% nrow() # 219
country_crosswalk %>% filter(state_independent_flag == 1) %>% nrow() # 195


#/////////////////////////////


# get updated world_bank_income_level - updated every year in july - the data360r package was last updated 4/2020
# https://blogs.worldbank.org/opendata/new-world-bank-country-classifications-income-level-2020-2021
# https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups
world_bank_income_level <- read_excel(path = "world_bank_income_classifications_july_2020.xls", 
                                      sheet = "List of economies",
                                      range = "C7:H224", col_names = FALSE) %>% 
        select(c(2, 4, 5))
names(world_bank_income_level) <- c("iso3", "world_bank_region", "world_bank_income_level")


#/////////////////////////////


# inspect
world_bank_income_level


#/////////////////////////////


# combine world_bank_income_level with country_crosswalk
country_crosswalk <- country_crosswalk %>% left_join(., world_bank_income_level, by = "iso3")


#/////////////////////////////


# inspect
country_crosswalk
country_crosswalk %>% glimpse()
country_crosswalk %>% nrow() # 219
country_crosswalk %>% count(state_independent_flag)
country_crosswalk %>% count(world_bank_income_level)
country_crosswalk %>% count(world_bank_region)
country_crosswalk %>% filter(str_detect(string = world_bank_name, pattern = regex("bosnia", ignore_case = TRUE)))


#/////////////////////////////


# add e&e variable
# eu-15: https://stats.oecd.org/glossary/detail.asp?ID=6805
# note that ee_region_flag omits greenland, because from practical perspective we don't cover them in analysis
# note also that the EU currently has 27 members, which include all 12 E&E graduates except Montenegro,
# and includes all EU-15 except UK (11 grads + 14 EU-15 = 25 + malta + cyprus = EU-27)
# note that the EU-27 also includes Malta and Cyprus, which are not included in either E&E graduates or EU-15, so wont be in E&E analysis
# https://european-union.europa.eu/principles-countries-history/country-profiles_en
country_crosswalk <- country_crosswalk %>% mutate(country = case_when(world_bank_name == "Bosnia and Herzegovina" ~ "BiH",
                                                 world_bank_name == "North Macedonia" ~ "N. Macedonia",
                                                 world_bank_name == "United States" ~ "U.S.", 
                                                 world_bank_name == "United Kingdom" ~ "U.K.",
                                                 world_bank_name == "Taiwan, China" ~ "Taiwan",
                                                 TRUE ~ world_bank_name),
                             mcp_grouping = case_when(country %in% c("Bulgaria", "Croatia", "Czechia", "Estonia",
                                                                    "Hungary", "Latvia", "Lithuania", "Montenegro",
                                                                    "Poland", "Romania", "Slovakia", "Slovenia") ~ "E&E graduates",
                                        country %in% c("Albania", "BiH", "Kosovo", "N. Macedonia",
                                                                  "Serbia") ~ "E&E Balkans",
                                        country %in% c("Armenia", "Azerbaijan", "Belarus", 
                                                                     "Georgia", "Moldova", "Ukraine") ~ "E&E Eurasia",
                                        country == "Greenland" ~ "E&E Europe",
                                        country == "Russia" ~ "Russia",
                                        country %in% c("Austria", "Belgium", "Denmark", "Finland", "France", "Germany", "Greece",
                                                       "Ireland", "Italy", "Luxembourg", "Netherlands",
                                                       "Portugal", "Spain", "Sweden", "U.K.") ~ "EU-15",
                                        country %in% c("Kazakhstan", "Kyrgyzstan", "Turkmenistan", 
                                                       "Tajikistan", "Uzbekistan") ~ "CARs",
                                        country == "U.S." ~ "U.S.",
                                        TRUE ~ NA_character_),
                             ee_region_flag = case_when(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates",
                                                                            "CARs", "EU-15", "Russia") ~ 1, 
                                                        TRUE ~ 0)) %>%
                        relocate(country) %>% relocate(mcp_grouping, .after = country) %>%
                        relocate(world_bank_region, world_bank_income_level, .after = world_bank_name)


#///////////////////////


# inspect
country_crosswalk
country_crosswalk %>% glimpse()
country_crosswalk %>% nrow() # 219
country_crosswalk %>% filter(str_detect(string = country, pattern = "United")) %>% select(country)
country_crosswalk %>% filter(country %in% c("U.S.", "U.K.")) %>% select(country)
country_crosswalk %>% count(mcp_grouping)
country_crosswalk %>% count(world_bank_region)
country_crosswalk %>% count(ee_region_flag)
country_crosswalk %>% filter(ee_region_flag == 1) %>% count(mcp_grouping)
country_crosswalk %>% filter(world_bank_region == "Europe & Central Asia", is.na(mcp_grouping)) %>% 
        select(country, mcp_grouping, state_independent_flag) %>% print(n = nrow(.))
country_crosswalk %>% count(world_bank_income_level)
country_crosswalk %>% filter(country == "Romania") %>% select(country, world_bank_income_level)
country_crosswalk %>% filter(is.na(world_bank_income_level))


#///////////////////////


# write country_crosswalk
# country_crosswalk %>% write_csv(file = "country_crosswalk.csv")

# read country_crosswalk
current_wd <- getwd()
setwd("C:/Users/sdevine/Desktop/usaid/mcp/useful_info/country_crosswalk")
country_crosswalk <- read_csv("country_crosswalk.csv")
setwd(current_wd)

# inspect
country_crosswalk %>% glimpse()
country_crosswalk %>% print(n = nrow(.))
country_crosswalk %>% skim()








