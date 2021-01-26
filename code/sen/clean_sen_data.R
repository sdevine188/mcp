library(tidyverse)
library(lubridate)
library(fs)
library(skimr)
library(priceR)
library(readxl)
library(httr)
library(tidyjson)

# load add_group_index
setwd("C:/Users/Stephen/Desktop/usaid/r/helper_scripts")
source("add_group_index.R")

# https://www.census.gov/topics/income-poverty/income/guidance/current-vs-constant-dollars.html
# https://github.com/stevecondylios/priceR
# under the hood, priceR gets exchange rate info from exchangerate.host
# https://exchangerate.host/#/docs
# https://api.exchangerate.host/timeseries?start_date=2020-01-01&end_date=2020-01-04&base=UAH&symbols=USD

# treasury has historical quarterly exchange rates: 
# https://fiscaldata.treasury.gov/datasets/treasury-reporting-rates-exchange/treasury-reporting-rates-of-exchange

# irs uses average yearly exchange rate: https://www.irs.gov/individuals/international-taxpayers/yearly-average-currency-exchange-rates

# setwd
setwd("C:/Users/Stephen/Desktop/usaid/mcp/tso_portfolio_reviews/media/data")
options(scipen=999)


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# read in sen_media_data file with metrics 2a, 1.1b, 1.2b, 1.3a, 1.3b ####
sen_metrics <- read_excel("sen_program_data/sen_media_data.xlsx")
sen_metrics
sen_metrics %>% glimpse()

# clean and wrangle sen_metrics
sen_metrics <- sen_metrics %>% mutate(date_reported = ymd(date_to)) %>% 
        select(-c(date_from, date_to)) %>%
        pivot_longer(cols = contains("partner"), names_to = "partner", values_to = "values") %>%
        # drop records where partner = all_partners for all indicators except 1.3a, since that is only indicator for all_partners
        filter(!(partner == "all_partners" & indicator_number != "1.3a")) %>%
        mutate(partner = case_when(partner == "partner_1" ~ "Partner 1",
                                   partner == "partner_2" ~ "Partner 2",
                                   partner == "partner_3" ~ "Partner 3",
                                   partner == "partner_4" ~ "Partner 4",
                                   partner == "partner_5" ~ "Partner 5",
                                   partner == "partner_6" ~ "Partner 6",
                                   partner == "partner_7" ~ "Partner 7",
                                   partner == "all_partners" ~ "All partners",
                                   TRUE ~ NA_character_),
               notes = case_when(indicator_number == "1.1b" & values == 2 ~ "Cloning",
                                 indicator_number == "1.1b" & values == 3 ~ "Coopetition",
                                 indicator_number == "1.1b" & values == 4 ~ "Content sharing",
                                 indicator_number == "1.1b" & values == 5 ~ "Convergence",
                                 TRUE ~ notes),
               note_flag = case_when(!is.na(notes) ~ 1, TRUE ~ 0)) %>% 
        select(indicator_number, indicator_name, level_2_indicator_name, level_3_indicator_name,
               indicator_desc, indicator_units, partner, date_reported, baseline_flag,
               values, note_flag, notes) 

# get sen_metrics_dates
sen_metrics_dates <- sen_metrics %>% group_by(indicator_name, level_2_indicator_name, level_3_indicator_name) %>% 
        distinct(date_reported) %>%
        arrange(date_reported) %>% add_group_index(group_vars = date_reported, group_name = "date_reported_index") %>%
        ungroup() %>% arrange(indicator_name, level_2_indicator_name, level_3_indicator_name, date_reported) %>%
        select(indicator_name, level_2_indicator_name, level_3_indicator_name, date_reported, date_reported_index) 
sen_metrics_dates

# add date_reported_index and filter NA values
sen_metrics <- sen_metrics %>% left_join(., sen_metrics_dates, by = c("indicator_name", "level_2_indicator_name", 
                                                       "level_3_indicator_name", "date_reported")) %>%
        filter(!is.na(values))

# inspect
sen_metrics
sen_metrics %>% glimpse()
sen_metrics %>% nrow() # 252
sen_metrics %>% ncol() # 13
sen_metrics %>% count(indicator_name)
sen_metrics %>% count(level_2_indicator_name)
sen_metrics %>% count(level_3_indicator_name)
sen_metrics %>% count(indicator_desc)
sen_metrics %>% count(indicator_units)
sen_metrics %>% count(partner)
sen_metrics %>% filter(is.na(values)) %>% select(indicator_name, partner, date_reported, values)
sen_metrics %>% group_by(indicator_name) %>% skim(values)


#/////////////////////////////////////////////////////////////////////////////////////////////


# read in metric 2b ####
metric_2b <- read_excel("sen_program_data/sen_media_metric_2b_data.xlsx")
metric_2b
metric_2b %>% glimpse()
metric_2b %>% nrow() # 69
metric_2b %>% ncol() # 18

# clean and wrangle
metric_2b <- metric_2b %>% mutate(date_reported = ymd(date_to)) %>% 
        select(-c(date_from, date_to)) %>%
        pivot_longer(cols = contains("partner"), names_to = "partner", values_to = "values") %>%
        mutate(partner = case_when(partner == "partner_1" ~ "Partner 1",
                                   partner == "partner_2" ~ "Partner 2",
                                   partner == "partner_3" ~ "Partner 3",
                                   partner == "partner_4" ~ "Partner 4",
                                   partner == "partner_5" ~ "Partner 5",
                                   partner == "partner_6" ~ "Partner 6",
                                   partner == "partner_7" ~ "Partner 7",
                                   TRUE ~ NA_character_)) %>% 
        mutate(currency = str_sub(string = values, start = 1, end = 3),
               values = str_sub(string = values, start = 5),
               values = as.numeric(str_replace_all(string = values, pattern = ",", replacement = ""))) %>%
        select(indicator_number, indicator_name, level_2_indicator_name, level_3_indicator_name,
               indicator_desc, indicator_units, partner, date_reported, baseline_flag,
               values, currency, note_flag, notes) 

# get metric_2b_dates
metric_2b_dates <- metric_2b %>% group_by(indicator_name, level_2_indicator_name, level_3_indicator_name) %>% distinct(date_reported) %>%
        arrange(date_reported) %>% add_group_index(group_vars = date_reported, group_name = "date_reported_index") %>%
        ungroup() %>% arrange(indicator_name, level_2_indicator_name, level_3_indicator_name, date_reported) %>%
        select(indicator_name, level_2_indicator_name, level_3_indicator_name, date_reported, date_reported_index) 
metric_2b_dates

# add date_reported_index and filter NA values
metric_2b <- metric_2b %>% left_join(., metric_2b_dates, by = c("indicator_name", "level_2_indicator_name", 
                                                                      "level_3_indicator_name", "date_reported")) %>%
        filter(!is.na(values))
        
# inspect
metric_2b
metric_2b %>% glimpse()
metric_2b %>% nrow() # 56
metric_2b %>% ncol() # 14
metric_2b %>% count(indicator_name)
metric_2b %>% count(level_2_indicator_name)
metric_2b %>% count(level_3_indicator_name)
metric_2b %>% count(indicator_desc)
metric_2b %>% count(indicator_units)
metric_2b %>% count(partner)
metric_2b %>% group_by(indicator_name) %>% skim(values)
metric_2b %>% count(currency)
metric_2b %>% filter(is.na(values)) %>% select(indicator_name, partner, date_reported, values)
metric_2b %>% group_by(currency) %>% skim(values)


#///////////////////////////


# convert historical currency amounts to dollars using treasury quarterly exchange rates
# download csv https://fiscaldata.treasury.gov/datasets/treasury-reporting-rates-exchange/treasury-reporting-rates-of-exchange
# also can hit api: https://www.transparency.treasury.gov/services/api/fiscal_service/v1/accounting/od/rates_of_exchange

# note that world bank has the international comparison price project calculating PPP conversion rates for countries
# but their FAQ says that due to its calculations taking into account multiple sectors, it should only be used for whole GDP 
# or whole sector comparisons, not for converting non-aggregate trade flows or investments or aid, etc
# so the sen program data income is not nearly sector-aggregate enough to use the PPP, 
# for cases like this the world bank recommends just converting using market exchange rates, despite the known price level issue
# https://www.worldbank.org/en/programs/icp/brief/faq#5-1

# load rates data
rates <- read_csv(file = "treasury_exchange_rate_data/RprtRateXchg_5_years.csv")
rates
rates %>% glimpse()

# clean rates
rates <- rates %>% mutate(Country = str_to_title(string = Country)) %>% 
        mutate(currency = case_when(Country == "Euro Zone" ~ "EUR",
                                    Country == "Moldova" ~ "MDL",
                                    Country == "Russia" ~ "RUB",
                                    Country == "Ukraine" ~ "UAH",
                                    Country == "Georgia" ~ "GEL",
                                    TRUE ~ NA_character_),
               year = year(`Record Date`), quarter = quarter(`Record Date`)) %>%
        rename(exchange_rate = "Exchange Rate") %>%
        select(year, quarter, currency, exchange_rate)

# inspect
rates
rates %>% glimpse()
rates %>% nrow() # 3994
rates %>% ncol() # 4
rates %>% filter(!is.na(currency))


#///////////////////////////


# join rates to metric_2b        
metric_2b <- metric_2b %>% mutate(year = year(date_reported), quarter = quarter(date_reported)) %>%
        left_join(., rates, by = c("currency" = "currency", "year" = "year", "quarter" = "quarter"), na_matches = "never") %>% 
        mutate(dollar_value = case_when(!is.na(exchange_rate) ~ values / exchange_rate,
                                        currency == "USD" ~ values, TRUE ~ NA_real_))
        
# inspect
metric_2b
metric_2b %>% glimpse()
metric_2b %>% nrow() # 56
metric_2b %>% ncol() # 18
metric_2b %>% filter(currency != "USD") %>% 
        select(partner, date_reported, level_2_indicator_name, year, quarter, currency, exchange_rate, values, dollar_value) %>%
        arrange(partner) %>%
        print(n = nrow(.))


#///////////////////////////


# check quarterly exchange_rate against daily rate for date_reported - this is just a check, not part of final sen data output
# https://exchangerate.host/#/
# https://www1.oanda.com/currency/converter/

# create get_historic_exchange_rates()
get_historic_exchange_rates <- function(date_from, date_to, from_currency, to_currency, ...) {
        
        # create api call
        api_call <- str_c("https://api.exchangerate.host/timeseries?start_date=", date_from, "&end_date=",
                          date_to, "&base=", from_currency, "&symbols=", to_currency)
        print(api_call)
        
        content(GET(api_call), as = "text") %>% spread_all() %>% select(base, starts_with("rates")) %>% as_tibble() %>%
                pivot_longer(cols = -base, names_to = "var", values_to = "exchange_rate") %>%
                separate(col = var, into = c("extra", "date_currency"), sep = "\\.", extra = "merge", remove = FALSE) %>%
                separate(col = date_currency, into = c("date", "to_currency"), sep = "\\.", extra = "merge", remove = FALSE) %>%
                mutate(date = ymd(date), corrupt_multi_record_response_flag = case_when(nrow(.) > 1 ~ 1, TRUE ~ 0)) %>% 
                rename(from_currency = base) %>%
                select(date, from_currency, to_currency, exchange_rate, corrupt_multi_record_response_flag) %>% return()
}        

# get historic_exchange_rates
historic_exchange_rates <- metric_2b %>% filter(currency != "USD") %>% select(date_reported, currency, exchange_rate) %>%
        mutate(date_from = date_reported, date_to = date_reported, from_currency = "USD", to_currency = currency) %>%
        pmap(.l = ., .f = ~ get_historic_exchange_rates(date_from, date_to, from_currency, to_currency, ...)) %>%
        bind_rows()

historic_exchange_rates
historic_exchange_rates %>% glimpse()
# map(.x = historic_exchange_rates, .f = ~ nrow(.x))
historic_exchange_rates %>% count(to_currency)
historic_exchange_rates %>% count(date)
metric_2b %>% glimpse()

# note that for some reason the api is not finding georgian lari (GEL) for 20200401, but does find it for 20200801
# but GEL is supposed to be a supported symbol for the api - maybe they only have historic data back to mid-2020?, 
# or a period without data?
url <- 'https://api.exchangerate.host/symbols'
data <- fromJSON(url)
print(data)

# treasury quarterly spreadsheet had GEL though, 
# so does treasury daily api, but tresury api is less current, only going up to 2019 q3, where spreadsheet goes to 2020 q3
api_call <- "https://www.transparency.treasury.gov/services/api/fiscal_service/v1/accounting/od/exchange_rates?fields=country_currency_desc,exchange_rate,data_date&filter=country_currency_desc:in:(GEORGIA-LARI),data_date:gte:2018-01-01"
content(GET(api_call), as = "text") %>% spread_all() %>% glimpse()
content(GET(api_call), as = "text") %>% gather_object() %>% json_types()
content(GET(api_call), as = "text") %>% enter_object(data) %>% gather_array() %>% spread_all()
metric_2b %>% filter(currency == "GEL") %>% select(date_reported, currency, exchange_rate, dollar_value)


# compare historic exchange rates
# result, the quarterly treasury and daily exhchangerate.host rates are very very close (max diff pct is 5.6%, median is 2%)
# with exception of how exchangerate.host didn't have full history for GEL??, later GEL rates were very close though
historic_exchange_rates %>% filter(corrupt_multi_record_response_flag == 0) %>%
        distinct() %>%
        rename(historic_exchange_rate = exchange_rate) %>%
        left_join(metric_2b %>% mutate(row_number = row_number()), ., 
                  by = c("date_reported" = "date", "currency" = "to_currency"), na_matches = "never") %>%
        select(date_reported, currency, values, exchange_rate, historic_exchange_rate) %>%
        filter(!is.na(currency), currency != "USD") %>% print(n = nrow(.)) %>%
        mutate(abs_exchange_rate_diff = abs(exchange_rate - historic_exchange_rate),
               abs_pct_exchange_rate_diff = abs(exchange_rate - historic_exchange_rate) / exchange_rate) %>%
        arrange(desc(abs_pct_exchange_rate_diff)) %>% print(n = nrow(.))
        # skim(abs_exchange_rate_diff, abs_pct_exchange_rate_diff)


#///////////////////////////


# inflate dollar_value to constant_sep_2020_dollars

# download non-seasonally adjusted cpi for all urban consumers (CPI-U) from BLS 
# https://www.bls.gov/cpi/
# bls data portal: https://data.bls.gov/PDQWeb/cu
# click area = US city average; items = all items; not seasonally adjusted; click get data, 
# then when data table returns, click "include annual averages" checkbox (the annual averages are just the simple monthly mean)
# bls handbook on cpi: https://www.bls.gov/opub/hom/pdf/cpihom.pdf
# bls handbook says CPI-U is broadest geographic coverage for most categories of spending
# "For the CPI-U population areas—the broadest geographic coverage—detailed item indexes for most categories of
# consumer spending are published every month."
# bls handbook showing coversion calcuation: https://www.bls.gov/cpi/factsheets/cpi-math-calculations.pdf

# bls inflation calculator: www.bls.gov/data/inflation_calculator.htm
# bls calculator inside: https://www.bls.gov/data/inflation_calculator_inside.htm
# note that bls calculator will inflate any month/year to any month/year, and does so using non-seasonally adjusted monthly cpi-u
# manual calculation matched excatly the calculator ouput; apr-2019 to sep-2020 (260.280 / 255.548) * 110756.00 = 112806.9

# bls faq on seasonal adjustment confirms that non-seasonal adjustment should be used to measure actual change in price of goods
# also this vignette for blsscrapeR calculates price escalation using non-seasonally adjusted cpi
# https://www.bls.gov/cpi/seasonal-adjustment/questions-and-answers.htm

# census definition of constant dollars, which are adjusted for price level, vs current dollars, which is amount when reported
# https://www.census.gov/topics/income-poverty/income/guidance/current-vs-constant-dollars.html#:~:text=Constant%2Ddollar%20value%20(also%20called,series%20reported%20in%20dollar%20terms.

# note that census uses a modified "research" version of urban cpi annual averages; also shows conversion calculation, same as bls
# https://www.census.gov/topics/income-poverty/income/guidance/current-vs-constant-dollars.html

cpi <- read_excel("bls_cpi_data/SeriesReport-20201106120259_788a93.xlsx", skip = 11)
cpi
cpi %>% glimpse()
cpi %>% nrow() # 11
cpi %>% ncol() # 16

# clean
cpi <- cpi %>% select(-c(Annual, HALF1, HALF2)) %>% rename(year = Year) %>%
        pivot_longer(cols = -year, names_to = "month", values_to = "index") %>%
        mutate(month = case_when(month == "Jan" ~ 1,
                                 month == "Feb" ~ 2,
                                 month == "Mar" ~ 3,
                                 month == "Apr" ~ 4,
                                 month == "May" ~ 5,
                                 month == "Jun" ~ 6,
                                 month == "Jul" ~ 7,
                                 month == "Aug" ~ 8,
                                 month == "Sep" ~ 9,
                                 month == "Oct" ~ 10,
                                 month == "Nov" ~ 11,
                                 month == "Dec" ~ 12, 
                                 TRUE ~ NA_real_))
cpi
cpi %>% arrange(desc(year), desc(month))

cpi <- cpi %>% mutate(cpi_sep_2020 = cpi %>% filter(year == 2020, month == 9) %>% pull(index))
cpi

# join cpi to metric_2b
metric_2b %>% glimpse()
metric_2b <- metric_2b %>% mutate(month = month(date_reported)) %>%
        left_join(., cpi, by = c("year" = "year", "month" = "month")) %>% 
        mutate(constant_sep_2020_dollar_value = (cpi_sep_2020 / index) * dollar_value)


#////////////////////


# inspect
metric_2b
metric_2b %>% glimpse()
metric_2b %>% nrow() # 56
metric_2b %>% ncol() # 22
 
# check usd
metric_2b %>%
        select(values, currency, date_reported, exchange_rate, dollar_value, index, 
               cpi_sep_2020, constant_sep_2020_dollar_value)
metric_2b %>% slice(3) %>% pull(dollar_value) # 125626
metric_2b %>% slice(3) %>% pull(cpi_sep_2020) # 260.28
metric_2b %>% slice(3) %>% pull(index) # 255.548
metric_2b %>% slice(3) %>% pull(constant_sep_2020_dollar_value) # 125626
(260.28 / 255.548) * 123342.1

# check foreign
metric_2b %>% filter(currency != "USD") %>%
        select(values, currency, date_reported, exchange_rate, dollar_value, index, 
               cpi_sep_2020, constant_sep_2020_dollar_value)
metric_2b %>% filter(currency != "USD") %>% slice(4) %>% pull(dollar_value) # 468.75
metric_2b %>% filter(currency != "USD") %>% slice(4) %>% pull(cpi_sep_2020) # 260.28
metric_2b %>% filter(currency != "USD") %>% slice(4) %>% pull(index) # 255.548
metric_2b %>% filter(currency != "USD") %>% slice(4) %>% pull(constant_sep_2020_dollar_value) # 477.4299
(260.28 / 255.548) * 468.75


# check collapsing of level_2_indicator_name
# note the categorization of level_2_indicator_name comes from nick doty's email on 1/4/2021
# the only change i made to nick's suggestion was putting "Events" into "Other" since only 1 partner had an Event
metric_2b %>% count(indicator_number, indicator_name, level_2_indicator_name) %>%
        print(n = nrow(.))
metric_2b %>% count(indicator_number, indicator_name, level_2_indicator_name, partner) %>%
        print(n = nrow(.))
metric_2b_inspection <- metric_2b %>% 
        mutate(level_2_indicator_name_v2 = case_when(
                level_2_indicator_name == "top_line" ~ "Total non-grant income",
                level_2_indicator_name %in% c("Advertising", "Advertising networks",
                                              "Google advertisement", "Native advertising", "Online ads",
                                              "Promotional material/ advertisements", "Standard advertising") ~ "Advertising",
                level_2_indicator_name %in% c("Commercial partner projects", "Royalty",
                                              "Commerical partnerships", "Content production",
                                              "Media production services") ~ "Commercial partnerships",
                level_2_indicator_name %in% c("Subscription", "Subscription online", "Subscription Online", 
                                              "Subscription print",
                                              "Crowdfunding", "Donations", 
                                              "Voluntary donations") ~ "Subscriptions/crowdfunding",
                level_2_indicator_name %in% c("Barter", "Renting out part of office", "Events",
                                              "Other (prizes, etc.)") ~ "Other"))
metric_2b_inspection %>% count(level_2_indicator_name_v2, level_2_indicator_name) %>% print(n = nrow(.))
metric_2b_inspection %>% count(level_2_indicator_name_v2) %>% print(n = nrow(.))
metric_2b_inspection %>% count(date_reported)


#///////////////////////////


# clean metric_2b ####

# note the categorization of level_2_indicator_name comes from nick doty's email on 1/4/2021
# the only change i made to nick's suggestion was putting "Events" into "Other" since only 1 partner had an Event

# note that per nick doty's 1/4/2021 email, the aug 2020 measurement was an unscheduled covid-related update 
# and should be excluded from analysis due to some vague/confusing comparability/interpretation/coverage issues 
metric_2b <- metric_2b %>% mutate(indicator_units = "Income of each media outlet, in constant Sept. 2020 dollars",
                     values = constant_sep_2020_dollar_value,
                     level_2_indicator_name = case_when(
                             level_2_indicator_name == "top_line" ~ "Total non-grant income",
                             level_2_indicator_name %in% c("Advertising", "Advertising networks",
                                                           "Google advertisement", "Native advertising", "Online ads",
                                                           "Promotional material/ advertisements", "Standard advertising") ~ "Advertising",
                             level_2_indicator_name %in% c("Commercial partner projects", "Royalty",
                                                           "Commerical partnerships", "Content production",
                                                           "Media production services") ~ "Commercial partnerships",
                             level_2_indicator_name %in% c("Subscription", "Subscription online", "Subscription Online", 
                                                           "Subscription print",
                                                           "Crowdfunding", "Donations", 
                                                           "Voluntary donations") ~ "Subscriptions/crowdfunding",
                             level_2_indicator_name %in% c("Barter", "Renting out part of office", "Events",
                                                           "Other (prizes, etc.)") ~ "Other")) %>%
        select(-c(year, quarter, currency, exchange_rate, dollar_value, month, index, cpi_sep_2020, 
                  constant_sep_2020_dollar_value)) %>%
        filter(!(indicator_number == "2b" & date_reported == "2020-08-01")) %>%
        group_by(indicator_name, indicator_number, level_2_indicator_name, partner, date_reported) %>%
        mutate(values_sum = sum(values)) %>% 
        ungroup() %>% 
        arrange(partner, level_2_indicator_name, date_reported, values, values_sum) %>%
        select(-values) %>% rename(values = values_sum) %>% 
        relocate(values, .before = note_flag) %>% distinct()


#/////////////////////////////


# inspect
metric_2b
metric_2b %>% glimpse()
metric_2b %>% nrow() # 28
metric_2b %>% ncol() # 13
metric_2b %>% count(indicator_name)
metric_2b %>% count(level_2_indicator_name) %>% print(n = nrow(.))
metric_2b %>% count(level_3_indicator_name)
metric_2b %>% count(indicator_desc)
metric_2b %>% count(indicator_units)
metric_2b %>% count(partner)
metric_2b %>% group_by(indicator_name) %>% skim(values)


# confirm that total_non grant income equals sum of business model income
metric_2b %>% filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income") %>%
        group_by(partner, date_reported) %>% mutate(values_sum = sum(values)) %>%
        ungroup() %>% select(level_2_indicator_name, partner, date_reported, values, values_sum) %>%
        arrange(partner, date_reported)

# the difference in total_non grant income vs sum of business model income is very small 
# no more than 6 pct, probably due to mix of currencies listed in original data and then exchange rate conversions
metric_2b %>% filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income") %>%
        group_by(partner, date_reported) %>% mutate(values_sum = sum(values)) %>%
        ungroup() %>% select(level_2_indicator_name, partner, date_reported, values, values_sum) %>%
        left_join(., metric_2b %>% filter(indicator_number == "2b", level_2_indicator_name == "Total non-grant income") %>%
                          select(partner, date_reported, values) %>% 
                          rename(total_non_grant_income = values),
                  by = c("partner", "date_reported")) %>% 
        mutate(diff = round(values_sum - total_non_grant_income, digits = 2),
               diff_pct = round((values_sum - total_non_grant_income) / values_sum, digits = 2)) %>%
        arrange(desc(diff))

# will not drop total_non_grant_income though, because partner 7 has only a total_non_grant_income value for period 2
# but no underlying breakdown of revenue category, so if total is dropped, there are no period 2 values for partner 7
metric_2b %>% filter(indicator_number == "2b", level_2_indicator_name == "Total non-grant income") %>% 
        distinct(partner, date_reported) %>%
        count(partner) %>% arrange(partner)
metric_2b %>% filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income") %>% 
        distinct(partner, date_reported) %>%
        count(partner) %>% arrange(partner)
metric_2b %>% filter(indicator_number == "2b", partner == "Partner 7") %>%
        select(level_2_indicator_name, partner, date_reported, values)


#/////////////////////////////////////////////////////////////////////////////////////////////


# read in metric 1.3c ####
metric_1_3c <- read_excel("sen_program_data/sen_media_metric_1_3c_data.xlsx")
metric_1_3c
metric_1_3c %>% glimpse()
metric_1_3c %>% nrow() # 86
metric_1_3c %>% ncol() # 20

# clean and wrangle
metric_1_3c <- metric_1_3c %>% mutate(baseline_period = ymd(baseline_period)) %>%
        pivot_longer(cols = starts_with("date_"), names_to = "date_reported", values_to = "values") %>%
        mutate(date_reported = ymd(str_replace(string = date_reported, pattern = regex("date_", ignore_case = TRUE), 
                                               replacement = "")),
               baseline_flag = case_when(baseline_period == date_reported ~ 1, TRUE ~ 0),
               note_flag = 0, notes = NA_character_) %>%
        select(indicator_number, indicator_name, level_2_indicator_name, level_3_indicator_name,
               indicator_desc, indicator_units, partner, date_reported, baseline_flag,
               values, note_flag, notes) 

# get metric_1_3c_dates
metric_1_3c_dates <- metric_1_3c %>% group_by(indicator_name, level_2_indicator_name, level_3_indicator_name) %>% distinct(date_reported) %>%
        arrange(date_reported) %>% add_group_index(group_vars = date_reported, group_name = "date_reported_index") %>%
        ungroup() %>% arrange(indicator_name, level_2_indicator_name, level_3_indicator_name, date_reported) %>%
        select(indicator_name, level_2_indicator_name, level_3_indicator_name, date_reported, date_reported_index) 
metric_1_3c_dates

# add date_reported_index and filter NA values
metric_1_3c <- metric_1_3c %>% left_join(., metric_1_3c_dates, by = c("indicator_name", "level_2_indicator_name", 
                                                                      "level_3_indicator_name", "date_reported")) %>%
        filter(!is.na(values))

# inspect
metric_1_3c
metric_1_3c %>% glimpse()
metric_1_3c %>% nrow() # 694
metric_1_3c %>% ncol() # 13
metric_1_3c %>% count(indicator_name)
metric_1_3c %>% count(level_2_indicator_name)
metric_1_3c %>% count(level_3_indicator_name)
metric_1_3c %>% count(indicator_desc)
metric_1_3c %>% count(indicator_units)
metric_1_3c %>% count(partner)
metric_1_3c %>% group_by(indicator_name) %>% skim(values)


#/////////////////////////////////////////////////////////////////////////////////////////////


# read in metric 1.3.2a ####
metric_1_3_2a <- read_excel("sen_program_data/sen_media_metric_1_3_2a_data.xlsx")
metric_1_3_2a
metric_1_3_2a %>% glimpse()
metric_1_3_2a %>% nrow() # 7
metric_1_3_2a %>% ncol() # 16

# clean and wrangle
metric_1_3_2a <- metric_1_3_2a %>% 
        pivot_longer(cols = starts_with("date_"), names_to = "date_reported", values_to = "values") %>%
        mutate(date_reported = ymd(str_replace(string = date_reported, pattern = regex("date_", ignore_case = TRUE), 
                                               replacement = "")),
               baseline_flag = case_when(baseline_period == date_reported ~ 1, TRUE ~ 0),
               note_flag = 0, notes = NA_character_) %>%
        select(indicator_number, indicator_name, level_2_indicator_name, level_3_indicator_name,
               indicator_desc, indicator_units, partner, date_reported, baseline_flag,
               values, note_flag, notes) 

# get metric_1_3_2a_dates
metric_1_3_2a_dates <- metric_1_3_2a %>% group_by(indicator_name, level_2_indicator_name, level_3_indicator_name) %>% distinct(date_reported) %>%
        arrange(date_reported) %>% add_group_index(group_vars = date_reported, group_name = "date_reported_index") %>%
        ungroup() %>% arrange(indicator_name, level_2_indicator_name, level_3_indicator_name, date_reported) %>%
        select(indicator_name, level_2_indicator_name, level_3_indicator_name, date_reported, date_reported_index) 
metric_1_3_2a_dates

# add date_reported_index and filter NA values
metric_1_3_2a <- metric_1_3_2a %>% left_join(., metric_1_3_2a_dates, by = c("indicator_name", "level_2_indicator_name", 
                                                                      "level_3_indicator_name", "date_reported")) %>%
        filter(!is.na(values))

# inspect
metric_1_3_2a
metric_1_3_2a %>% glimpse()
metric_1_3_2a %>% nrow() # 56
metric_1_3_2a %>% ncol() # 13
metric_1_3_2a %>% count(indicator_name)
metric_1_3_2a %>% count(level_2_indicator_name)
metric_1_3_2a %>% count(level_3_indicator_name)
metric_1_3_2a %>% count(indicator_desc)
metric_1_3_2a %>% count(indicator_units)
metric_1_3_2a %>% count(partner)
metric_1_3_2a %>% group_by(indicator_name) %>% skim(values)


#/////////////////////////////////////////////////////////////////////////////////////////////


# read in metric 1.3.2b ####
metric_1_3_2b <- read_excel("sen_program_data/sen_media_metric_1_3_2b_data.xlsx")
metric_1_3_2b
metric_1_3_2b %>% glimpse()
metric_1_3_2b %>% nrow() # 7
metric_1_3_2b %>% ncol() # 20

# clean and wrangle
metric_1_3_2b <- metric_1_3_2b %>% 
        pivot_longer(cols = starts_with("date_"), names_to = "date_reported", values_to = "values") %>%
        mutate(date_reported = ymd(str_replace(string = date_reported, pattern = regex("date_", ignore_case = TRUE), 
                                               replacement = "")),
               baseline_flag = case_when(baseline_period == date_reported ~ 1, TRUE ~ 0),
               note_flag = 0, notes = NA_character_) %>%
        select(indicator_number, indicator_name, level_2_indicator_name, level_3_indicator_name,
               indicator_desc, indicator_units, partner, date_reported, baseline_flag,
               values, note_flag, notes) 

# get metric_1_3_2b_dates
metric_1_3_2b_dates <- metric_1_3_2b %>% group_by(indicator_name, level_2_indicator_name, level_3_indicator_name) %>% distinct(date_reported) %>%
        arrange(date_reported) %>% add_group_index(group_vars = date_reported, group_name = "date_reported_index") %>%
        ungroup() %>% arrange(indicator_name, level_2_indicator_name, level_3_indicator_name, date_reported) %>%
        select(indicator_name, level_2_indicator_name, level_3_indicator_name, date_reported, date_reported_index) 
metric_1_3_2b_dates

# add date_reported_index and filter NA values
metric_1_3_2b <- metric_1_3_2b %>% left_join(., metric_1_3_2b_dates, by = c("indicator_name", "level_2_indicator_name", 
                                                                      "level_3_indicator_name", "date_reported")) %>%
        filter(!is.na(values))

# inspect
metric_1_3_2b
metric_1_3_2b %>% glimpse()
metric_1_3_2b %>% nrow() # 69
metric_1_3_2b %>% ncol() # 13
metric_1_3_2b %>% count(indicator_name)
metric_1_3_2b %>% count(level_2_indicator_name)
metric_1_3_2b %>% count(level_3_indicator_name)
metric_1_3_2b %>% count(indicator_desc)
metric_1_3_2b %>% count(indicator_units)
metric_1_3_2b %>% count(partner)
metric_1_3_2b %>% group_by(indicator_name) %>% skim(values)


#/////////////////////////////////////////////////////////////////////////////////////////////


# read in metric 2.2a ####
metric_2_2a <- read_excel("sen_program_data/sen_media_metric_2_2a_data.xlsx")
metric_2_2a
metric_2_2a %>% glimpse()
metric_2_2a %>% nrow() # 14
metric_2_2a %>% ncol() # 20

# clean and wrangle
metric_2_2a <- metric_2_2a %>% 
        pivot_longer(cols = starts_with("date_"), names_to = "date_reported", values_to = "values") %>%
        mutate(date_reported = ymd(str_replace(string = date_reported, pattern = regex("date_", ignore_case = TRUE), 
                                               replacement = "")),
               baseline_flag = case_when(baseline_period == date_reported ~ 1, TRUE ~ 0),
               note_flag = 0, notes = NA_character_) %>%
        select(indicator_number, indicator_name, level_2_indicator_name, level_3_indicator_name,
               indicator_desc, indicator_units, partner, date_reported, baseline_flag,
               values, note_flag, notes) 

# get metric_2_2a_dates
metric_2_2a_dates <- metric_2_2a %>% group_by(indicator_name, level_2_indicator_name, level_3_indicator_name) %>% distinct(date_reported) %>%
        arrange(date_reported) %>% add_group_index(group_vars = date_reported, group_name = "date_reported_index") %>%
        ungroup() %>% arrange(indicator_name, level_2_indicator_name, level_3_indicator_name, date_reported) %>%
        select(indicator_name, level_2_indicator_name, level_3_indicator_name, date_reported, date_reported_index) 
metric_2_2a_dates

# add date_reported_index and filter NA values
metric_2_2a <- metric_2_2a %>% left_join(., metric_2_2a_dates, by = c("indicator_name", "level_2_indicator_name", 
                                                                      "level_3_indicator_name", "date_reported")) %>%
        filter(!is.na(values))

# inspect
metric_2_2a
metric_2_2a %>% glimpse()
metric_2_2a %>% nrow() # 144
metric_2_2a %>% ncol() # 13
metric_2_2a %>% count(indicator_name)
metric_2_2a %>% count(level_2_indicator_name)
metric_2_2a %>% count(level_3_indicator_name)
metric_2_2a %>% count(indicator_desc)
metric_2_2a %>% count(indicator_units)
metric_2_2a %>% count(partner)
metric_2_2a %>% group_by(indicator_name) %>% skim(values)


#/////////////////////////////////////////////////////////////////////////////////////////////


# read in metric 2.2b ####
metric_2_2b <- read_excel("sen_program_data/sen_media_metric_2_2b_data.xlsx")
metric_2_2b
metric_2_2b %>% glimpse()
metric_2_2b %>% nrow() # 14
metric_2_2b %>% ncol() # 14

# clean and wrangle
metric_2_2b <- metric_2_2b %>% 
        pivot_longer(cols = starts_with("date_"), names_to = "date_reported", values_to = "values") %>%
        mutate(date_reported = ymd(str_replace(string = date_reported, pattern = regex("date_", ignore_case = TRUE), 
                                               replacement = "")),
               baseline_flag = case_when(baseline_period == date_reported ~ 1, TRUE ~ 0),
               note_flag = 0, notes = NA_character_) %>%
        select(indicator_number, indicator_name, level_2_indicator_name, level_3_indicator_name,
               indicator_desc, indicator_units, partner, date_reported, baseline_flag,
               values, note_flag, notes) 

# get metric_2_2b_dates
metric_2_2b_dates <- metric_2_2b %>% group_by(indicator_name, level_2_indicator_name, level_3_indicator_name) %>% distinct(date_reported) %>%
        arrange(date_reported) %>% add_group_index(group_vars = date_reported, group_name = "date_reported_index") %>%
        ungroup() %>% arrange(indicator_name, level_2_indicator_name, level_3_indicator_name, date_reported) %>%
        select(indicator_name, level_2_indicator_name, level_3_indicator_name, date_reported, date_reported_index) 
metric_2_2b_dates

# add date_reported_index and filter NA values
metric_2_2b <- metric_2_2b %>% left_join(., metric_2_2b_dates, by = c("indicator_name", "level_2_indicator_name", 
                                                                      "level_3_indicator_name", "date_reported")) %>%
        filter(!is.na(values))

# inspect
metric_2_2b
metric_2_2b %>% glimpse()
metric_2_2b %>% nrow() # 70
metric_2_2b %>% ncol() # 13
metric_2_2b %>% count(indicator_name)
metric_2_2b %>% count(level_2_indicator_name)
metric_2_2b %>% count(level_3_indicator_name)
metric_2_2b %>% count(indicator_desc)
metric_2_2b %>% count(indicator_units)
# note that partner 7 is missing values for 4/2020 period because of discrepancy of 2.2b with 1.3.2a for this period
# required "shifting" 2.2b values across partners for 4/2020 to align wtih 1.3.2a, but that left partner 7 with no values
# see original spreadsheet 
metric_2_2b %>% count(partner)
metric_2_2b %>% group_by(indicator_name) %>% skim(values)


#/////////////////////////////////////////////////////////////////////////////////////////////


# read in metric 2.1.2a ####
metric_2_1_2a <- read_excel("sen_program_data/sen_media_metric_2_1_2a_data.xlsx")
metric_2_1_2a
metric_2_1_2a %>% glimpse()
metric_2_1_2a %>% nrow() # 14
metric_2_1_2a %>% ncol() # 10

# clean and wrangle
metric_2_1_2a <- metric_2_1_2a %>% 
        pivot_longer(cols = starts_with("date_"), names_to = "date_reported", values_to = "values") %>%
        mutate(date_reported = ymd(str_replace(string = date_reported, pattern = regex("date_", ignore_case = TRUE), 
                                               replacement = "")),
               baseline_flag = case_when(baseline_period == date_reported ~ 1, TRUE ~ 0),
               note_flag = 0, notes = NA_character_) %>%
        select(indicator_number, indicator_name, level_2_indicator_name, level_3_indicator_name,
               indicator_desc, indicator_units, partner, date_reported, baseline_flag,
               values, note_flag, notes) 

# get metric_2_1_2a_dates
metric_2_1_2a_dates <- metric_2_1_2a %>% group_by(indicator_name, level_2_indicator_name, level_3_indicator_name) %>% distinct(date_reported) %>%
        arrange(date_reported) %>% add_group_index(group_vars = date_reported, group_name = "date_reported_index") %>%
        ungroup() %>% arrange(indicator_name, level_2_indicator_name, level_3_indicator_name, date_reported) %>%
        select(indicator_name, level_2_indicator_name, level_3_indicator_name, date_reported, date_reported_index) 
metric_2_1_2a_dates

# add date_reported_index and filter NA values
metric_2_1_2a <- metric_2_1_2a %>% left_join(., metric_2_1_2a_dates, by = c("indicator_name", "level_2_indicator_name", 
                                                                      "level_3_indicator_name", "date_reported")) %>%
        filter(!is.na(values))

# inspect
metric_2_1_2a
metric_2_1_2a %>% glimpse()
metric_2_1_2a %>% nrow() # 26
metric_2_1_2a %>% ncol() # 13
metric_2_1_2a %>% count(indicator_name)
metric_2_1_2a %>% count(level_2_indicator_name)
metric_2_1_2a %>% count(level_3_indicator_name)
metric_2_1_2a %>% count(indicator_desc)
metric_2_1_2a %>% count(indicator_units)
metric_2_1_2a %>% count(partner)
metric_2_1_2a %>% group_by(indicator_name) %>% skim(values)


#/////////////////////////////////////////////////////////////////////////////////////////////


# compile sen_final_data ####

sen_data <- sen_metrics %>% 
        bind_rows(., metric_2b) %>%
        bind_rows(., metric_1_3c) %>%
        bind_rows(., metric_1_3_2a) %>%
        bind_rows(., metric_1_3_2b) %>%
        bind_rows(., metric_2_2a) %>%
        bind_rows(., metric_2_2b) %>%
        bind_rows(., metric_2_1_2a) %>%
        group_by(indicator_name) %>% mutate(date_reported_count = max(date_reported_index)) %>%
        ungroup() %>%
        relocate(c(date_reported_index, date_reported_count), .after = date_reported)
        

#///////////////////////


# update metric and recompile sen_data, if needed ####
sen_data <- metric_2_2b %>% group_by(indicator_name) %>% mutate(date_reported_count = max(date_reported_index)) %>%
        ungroup() %>%
        relocate(c(date_reported_index, date_reported_count), .after = date_reported) %>%
        bind_rows(sen_data %>% filter(indicator_number != "2.2b"), .)


#///////////////////////


# inspect
sen_data
sen_data %>% glimpse()
sen_data %>% nrow() # 1339
sen_data %>% ncol() # 14
sen_data %>% count(indicator_number) # 12
sen_data %>% count(indicator_name) # 14
sen_data %>% count(indicator_number, indicator_name) # 14, 2.2a and 2.2b each have two indicator_name
sen_data %>% count(indicator_number, indicator_name, level_2_indicator_name, level_3_indicator_name) %>% print(n = nrow(.))
sen_data %>% count(indicator_desc)
sen_data %>% count(indicator_units)
sen_data %>% count(partner)
sen_data %>% count(date_reported)
sen_data %>% filter(baseline_flag == 1) %>% count(date_reported)
sen_data %>% filter(note_flag == 1) %>% distinct(indicator_number, indicator_name, notes)
sen_data %>% filter(note_flag == 1) %>% distinct(indicator_number, indicator_name, notes) %>% pull(notes)

# check individual indicators
sen_data %>% filter(indicator_number == "1.3c") %>% 
        select(indicator_number, indicator_name, level_2_indicator_name, level_3_indicator_name, 
               partner, date_reported, values) %>%
        arrange(partner) %>% 
        print(n = nrow(.))

# check count of partners w/ data for indicators
sen_data %>% group_by(indicator_number, indicator_name, level_2_indicator_name, level_3_indicator_name) %>%
        mutate(partner_n_distinct = n_distinct(partner)) %>%
        ungroup() %>%
        distinct(indicator_number, indicator_name, level_2_indicator_name, level_3_indicator_name, 
                 partner, partner_n_distinct) %>% 
        filter(indicator_number == "1.3c") %>%
        print(n = 50)

# check dates for each indicator
sen_data %>% distinct(indicator_number, indicator_name, date_reported_count) %>% arrange(indicator_number)
sen_data %>% distinct(indicator_number, indicator_name, date_reported) %>% print(n = nrow(.))

# skim indicators
sen_data %>% group_by(indicator_name) %>% skim(values)
sen_data %>% group_by(indicator_name) %>% skim(values) %>% as_tibble() %>%
        select(indicator_name, numeric.p0, numeric.p25, numeric.p50, numeric.p75, numeric.p100)
# note that partner 1 only has non-NA values for 5 indicators
sen_data %>% filter(partner == "Partner 1", !is.na(values)) %>% distinct(indicator_number, indicator_name)


#////////////////


# inspect 1.3c ####
sen_data %>% filter(indicator_number == "1.3c") %>% 
        count(indicator_number, indicator_name, level_2_indicator_name, level_3_indicator_name, 
               partner, date_reported, values) %>%
        arrange(partner) %>% 
        print(n = nrow(.))

# check count of partners
sen_data %>% group_by(indicator_number, indicator_name, level_2_indicator_name, level_3_indicator_name) %>%
        mutate(partner_n_distinct = n_distinct(partner)) %>%
        ungroup() %>%
        distinct(indicator_number, indicator_name, level_2_indicator_name, level_3_indicator_name, partner_n_distinct) %>% 
        filter(indicator_number == "1.3c") %>% 
        arrange(indicator_number, indicator_name, level_2_indicator_name, level_3_indicator_name) %>%
        print(n = 50)
        

sen_data %>%  filter(indicator_number == "1.3c", level_2_indicator_name == "Vkontakte") %>%
        distinct(partner)


#///////////////////


# inspect 1.3a ####
sen_data %>% filter(indicator_number == "1.3a") %>% 
        distinct(indicator_number, indicator_name, level_2_indicator_name, level_3_indicator_name, partner)

# check dates
sen_data %>% filter(indicator_number == "1.3a") %>% distinct(date_reported)


#///////////////////


# inspect 1.3b ####
sen_data %>% filter(indicator_number == "1.3b") %>% 
        distinct(indicator_number, indicator_name, level_2_indicator_name, level_3_indicator_name, partner)

sen_data %>% filter(indicator_number == "1.3b", level_2_indicator_name == "Google Analytics") %>%
        ggplot(data = ., mapping = aes(x = date_reported, y = values, color = partner)) + 
        geom_line()

sen_data %>% filter(indicator_number == "1.3b", level_2_indicator_name == "IO Analytics") %>%
        ggplot(data = ., mapping = aes(x = date_reported, y = values, color = partner)) + 
        geom_line()


#///////////////////


# inspect 2b ####
sen_data %>% filter(indicator_number == "2b") %>% 
        count(indicator_number, indicator_name, level_2_indicator_name, level_3_indicator_name)

# note that partners 5 and 6 have only 1 measurement period for total income
sen_data %>% filter(indicator_number == "2b", level_2_indicator_name == "Total non-grant income") %>% 
        distinct(partner, date_reported) %>%
        count(partner) %>% arrange(partner)

sen_data %>% filter(indicator_number == "2b") %>% 
        select(level_2_indicator_name, partner, date_reported, values) %>% arrange(partner)

sen_data %>% filter(indicator_number == "2b", level_2_indicator_name == "Total non-grant income",
                    !(partner %in% c("Partner 5", "Partner 6"))) %>%
        ggplot(data = ., mapping = aes(x = date_reported, y = values, color = partner)) + 
        geom_line()

# note that there are several non-total partner/level_2_indicator combos with only one measurement period
sen_data %>% filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income") %>%
        select(level_2_indicator_name, partner, date_reported) %>%
        count(level_2_indicator_name, partner) %>%
        print(n = nrow(.))

# inspect share of income
sen_data %>% filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income") %>%
        select(level_2_indicator_name, partner, date_reported, values) %>%
        left_join(., metric_2b %>% filter(indicator_number == "2b", level_2_indicator_name == "Total non-grant income") %>%
                          select(partner, date_reported, values) %>% 
                          rename(total_non_grant_income = values),
                  by = c("partner", "date_reported")) %>%
        mutate(income_share = values / total_non_grant_income)

#///////////////////


# inspect 2.1.2a ####
sen_data %>% filter(indicator_number == "2.1.2a") %>% 
        distinct(indicator_number, indicator_name, level_2_indicator_name, level_3_indicator_name)

# confirm there is two measurement periods for each partner/level_2_indicator combo
sen_data %>% filter(indicator_number == "2.1.2a") %>% 
        distinct(level_2_indicator_name, partner, date_reported) %>%
        count(level_2_indicator_name, partner) %>%
        print(n = nrow(.))

sen_data %>% filter(indicator_number == "2.1.2a") %>% 
        select(level_2_indicator_name, partner, date_reported, values)


#////////////////////


# inspect 2.2b ####
sen_data %>% filter(indicator_number == "2.2b") %>% 
        distinct(indicator_number, indicator_name, level_2_indicator_name, level_3_indicator_name)

sen_data %>% filter(indicator_number == "2.2b", indicator_name == "count_of_pieces_shared_on_social_media_by_partners") %>% 
        count(indicator_number, indicator_name, level_2_indicator_name, level_3_indicator_name, date_reported)

sen_data %>% filter(indicator_number == "2.2b") %>% 
        select(indicator_number, indicator_name, date_reported, partner, values)

# check dates
sen_data %>% filter(indicator_number == "2.2b") %>% distinct(date_reported)


#///////////////////


# inspect 1.2b ####
sen_data %>% filter(indicator_number == "1.2b") %>% 
        distinct(indicator_number, indicator_name, level_2_indicator_name, level_3_indicator_name)

sen_data %>% filter(indicator_number == "1.2b") %>% 
        ggplot(data = ., mapping = aes(x = date_reported, y = values, color = partner)) +
        geom_line()


#///////////////////

# inspect 1.3.2a ####
sen_data %>% filter(indicator_number == "1.3.2a") %>% 
        count(indicator_number, indicator_name, level_2_indicator_name, level_3_indicator_name, date_reported) 

# not clear how to interpret the difference between monthly_stories_taken_by_partner_for_exchange and
# count_of_pieces_shared_on_social_media_by_partners
# sometimes it's positive, sometimes negative
sen_data %>% filter(indicator_number == "1.3.2a" | 
        (indicator_number == "2.2b" & indicator_name == "count_of_pieces_shared_on_social_media_by_partners")) %>%
        select(partner, indicator_name, date_reported, values) %>%
        pivot_wider(id_cols = c(partner, date_reported), names_from = indicator_name, values_from = "values") %>%
        mutate(diff = monthly_stories_taken_by_partner_for_exchange - count_of_pieces_shared_on_social_media_by_partners) %>%
        arrange(desc(diff)) %>%
        print(n = nrow(.))

sen_data %>% filter(indicator_number == "1.3.2a" | 
                            (indicator_number == "2.2b" & indicator_name == "count_of_pieces_shared_on_social_media_by_partners")) %>%
        select(partner, indicator_name, date_reported, values) %>%
        pivot_wider(id_cols = c(partner, date_reported), names_from = indicator_name, values_from = "values") %>%
        mutate(diff = monthly_stories_taken_by_partner_for_exchange - count_of_pieces_shared_on_social_media_by_partners) %>%
        ggplot(data = ., mapping = aes(x = date_reported, y = diff, color = partner)) + 
        geom_line()

sen_data %>% filter(indicator_number == "1.3.2a") %>%
        ggplot(data = ., mapping = aes(x = date_reported, y = values, color = partner)) + 
        geom_line()          


#/////////////////////////////////////////////////////////////////////////////////////////////


# write to file
# sen_data %>% write_csv(path = "sen_data_20200105.csv")
sen_data <- read_csv("sen_data_20200105.csv")
sen_data
sen_data %>% glimpse()
sen_data %>% nrow() # 1339
sen_data %>% ncol() # 14


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////



