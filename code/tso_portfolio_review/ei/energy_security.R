extrafont::loadfonts(device="win") 
library(tidyverse)
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



# setwd
setwd("C:/Users/sdevine/Desktop/usaid/mcp/tso_portfolio_reviews/energy_and_infrastructure")
options(scipen=999)


#//////////////////////////////////////////////////////////////////////////////////////////////////////


# create custom color_palette ####
color_palette <- tibble(hex = c("#08306B", "#2474B6", "#8BBFD0", 
                                "#CBCBCB", "#7D7D7D", "#424242",
                                "#99ba78", "#35B779FF", "#355e3b", 
                                "#E4DC68", "#7A378B"))
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


# load ee_country_crosswalk ####
current_directory <- getwd()
# setwd("C:/Users/Stephen/Desktop/usaid/mcp/useful_info")
setwd("C:/Users/sdevine/Desktop/usaid/mcp/useful_info")
ee_country_crosswalk <- read_excel(path = "ee_country_crosswalk.xlsx", sheet = "ee_country_crosswalk", na = "NA")
setwd(current_directory)

ee_country_crosswalk %>% print(n = nrow(.))
ee_country_crosswalk %>% skim()


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# write/read final atlas data ####
atlas <- read_csv(file = "data/atlas_un_comtrade_trade_flows/atlas_20201218.csv")

# inspect
atlas
atlas %>% glimpse()
atlas %>% nrow() # 1240
atlas %>% ncol() # 61


#/////////////////////////////////////////////////////////////////////////////////////////////


# write/read final iea data ####
iea <- read_csv(file = "data/iea/iea_20201218.csv")

# inspect
iea
iea %>% glimpse()
iea %>% nrow() # 350
iea %>% ncol() # 163


#/////////////////////////////////////////////////////////////////////////////////////////////


# write/read final iea_supplement ####
iea_supplement <- read_csv(file = "data/iea/iea_supplement_20201218.csv")

# inspect
iea_supplement
iea_supplement %>% glimpse()
iea_supplement %>% nrow() # 450
iea_supplement %>% ncol() # 141


#/////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////


# atlas_iea_combined_fossil_imports_from_russia_as_share_of_tes_graduates_line_chart ####


# inspect 
atlas %>% filter(country == "Moldova", year == 2018) %>%
        select(country, year, fuel_type,
               # russia_fuel_type_import_adj, global_fuel_type_import_sum_adj,
               # russia_fuel_type_import_as_share_of_global_fuel_type_import_sum_adj,
               # natural_gas_ktoe_imports_Russia, natural_gas_ktoe_imports_total,
               russia_fuel_type_import_as_share_of_global_fuel_type_import_sum_adj_w_iea_nat_gas,
               fuel_type_imports_as_share_of_tes,
               estimated_russia_fuel_type_imports_as_share_of_tes_w_iea_nat_gas,
               estimated_russia_fossil_imports_as_share_of_tes_w_iea_nat_gas)

# inspect filter for countries
atlas %>% 
        filter(!(country %in% c("E&E graduates")), 
               mcp_grouping %in% c("E&E graduates")) %>%
        count(mcp_grouping, country)

# inspect 
atlas %>% 
        filter(!(country %in% c("E&E graduates")), 
               mcp_grouping %in% c("E&E graduates")) %>%
        group_by(country, year) %>% 
        slice(1) %>%
        ungroup() %>% rename(values = estimated_russia_fossil_imports_as_share_of_tes_w_iea_nat_gas) %>%
        select(country, year, location_code, values) %>% arrange(desc(values)) %>% print(n = nrow(.))

# check ANS unspecified import country
atlas %>% group_by(country, fuel_type) %>% 
        summarize(avg_ans_fuel_type_import_as_share_of_global_import_value = 
                          mean(ans_fuel_type_import_as_share_of_global_import_value_sum_adj)) %>%
        ungroup() %>% arrange(desc(avg_ans_fuel_type_import_as_share_of_global_import_value)) %>% print(n = nrow(.))
atlas %>% arrange(desc(ans_fuel_type_import)) %>% select(country, year, fuel_type, ans_fuel_type_import) %>% print(n = 50)
atlas %>% group_by(country) %>%
        summarize(ans_imports = sum(ans_fuel_type_import)) %>%
        ungroup() %>%
        arrange(desc(ans_imports)) %>% print(n = 50)

# inspect chart
atlas %>% 
        filter(!(country %in% c("E&E graduates")), 
               mcp_grouping %in% c("E&E graduates")) %>%
        group_by(country, year) %>% 
        slice(1) %>%
        ungroup() %>% rename(values = estimated_russia_fossil_imports_as_share_of_tes_w_iea_nat_gas) %>%
        select(country, year, location_code, values) %>% 
        ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line()


# inspect lithuania
# note the spike from 2009 to 2010 is driven primarily by spike in iea fuel_type_imports_as_share_of_tes for crude
# and the drop from 2013 to 2014 is also driven by iea fuel_type_imports_as_share_of_tes for crude
atlas %>% filter(country == "Lithuania") %>%
        select(country, year, fuel_type,
               # russia_fuel_type_import_adj, global_fuel_type_import_sum_adj,
               # russia_fuel_type_import_as_share_of_global_fuel_type_import_sum_adj,
               # natural_gas_ktoe_imports_Russia, natural_gas_ktoe_imports_total,
               russia_fuel_type_import_as_share_of_global_fuel_type_import_sum_adj_w_iea_nat_gas,
               fuel_type_imports_as_share_of_tes,
               estimated_russia_fuel_type_imports_as_share_of_tes_w_iea_nat_gas,
               estimated_russia_fossil_imports_as_share_of_tes_w_iea_nat_gas) %>% print(n = nrow(.))

# inspect for report
chart_data %>% group_by(country) %>% summarize(mean = mean(values)) %>% arrange(mean)
chart_data %>% group_by(year) %>% summarize(mean = mean(values)) %>% 
        ungroup() %>% arrange(year) %>%
        mutate(overal_mean = mean(mean))


#//////////////////


# add color_bin and color
chart_data <- atlas %>% 
        filter(!(country %in% c("E&E graduates")), 
               mcp_grouping %in% c("E&E graduates")) %>%
        group_by(country, year) %>% 
        mutate(values = estimated_russia_fossil_imports_as_share_of_tes_w_iea_nat_gas) %>%
        slice(1) %>%
        ungroup() %>%
        select(country, year, location_code, values) %>%
        mutate(color_bin = country,
               color = case_when(color_bin == "Bulgaria" ~ color_palette %>% slice(1) %>% pull(hex),
                                 color_bin == "Croatia" ~ color_palette %>% slice(2) %>% pull(hex),
                                 color_bin == "Czechia" ~ color_palette %>% slice(3) %>% pull(hex),
                                 color_bin == "Estonia" ~ color_palette %>% slice(4) %>% pull(hex),
                                 color_bin == "Hungary" ~ color_palette %>% slice(5) %>% pull(hex),
                                 color_bin == "Latvia" ~ color_palette %>% slice(6) %>% pull(hex),
                                 color_bin == "Lithuania" ~ color_palette %>% slice(7) %>% pull(hex),
                                 color_bin == "Montenegro" ~ color_palette %>% slice(8) %>% pull(hex),
                                 color_bin == "Poland" ~ color_palette %>% slice(9) %>% pull(hex),
                                 color_bin == "Romania" ~ color_palette %>% slice(10) %>% pull(hex),
                                 color_bin == "Slovakia" ~ color_palette %>% slice(11) %>% pull(hex),
                                 color_bin == "Slovenia" ~ color_palette %>% slice(1) %>% pull(hex)),
               linetype_bin = country,
               linetype = case_when(country == "Slovenia" ~ "dotted",
                       TRUE ~ "solid"))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list

# create linetype_list for to pass to scale_linetype_manual
chart_data_linetype_list <- chart_data %>% count(linetype_bin, linetype) %>% pull(linetype)
names(chart_data_linetype_list) <- chart_data %>% count(linetype_bin, linetype) %>% pull(linetype_bin)
chart_data_linetype_list

# title
"Estimated fossil fuel imports from Russia as a share of total primary energy use in E&E graduates"


#/////////////////////


# create chart
atlas_iea_combined_fossil_imports_from_russia_as_share_of_tes_graduates_line_chart <- chart_data %>%
        ggplot(data = ., aes(x = year, 
                             y = values, 
                             color = factor(color_bin, levels = c("Bulgaria", "Croatia", "Czechia", "Estonia", "Hungary",
                                                                  "Latvia", "Lithuania", "Montenegro", "Poland",
                                                                  "Romania", "Slovakia", "Slovenia")),
                             linetype = factor(color_bin, levels = c("Bulgaria", "Croatia", "Czechia", "Estonia", "Hungary",
                                                                     "Latvia", "Lithuania", "Montenegro", "Poland",
                                                                     "Romania", "Slovakia", "Slovenia")))) + 
        geom_line(size = 1) +
        # geom_point(size = 2) +
        geom_text(data = chart_data %>% filter(year == max(year), country == "Bulgaria"),
                  mapping = aes(x = year + .1, y = values, label = color_bin),
                  fontface = "bold", hjust = 0, size = 2.2, family = "Calibri") +
        geom_text(data = chart_data %>% filter(year == max(year), country == "Croatia"),
                  mapping = aes(x = year + .1, y = values - .04, label = color_bin),
                  fontface = "bold", hjust = 0, size = 2.2, family = "Calibri") +
        geom_text(data = chart_data %>% filter(year == max(year), country == "Czechia"),
                  mapping = aes(x = year + .1, y = values + .05, label = color_bin),
                  fontface = "bold", hjust = 0, size = 2.2, family = "Calibri") +
        geom_text(data = chart_data %>% filter(year == max(year), country == "Estonia"),
                  mapping = aes(x = year + .1, y = values + .02, label = color_bin),
                  fontface = "bold", hjust = 0, size = 2.2, family = "Calibri") +
        geom_text(data = chart_data %>% filter(year == max(year), country == "Hungary"),
                  mapping = aes(x = year + .1, y = values, label = color_bin),
                  fontface = "bold", hjust = 0, size = 2.2, family = "Calibri") +
        geom_text(data = chart_data %>% filter(year == max(year), country == "Latvia"),
                  mapping = aes(x = year + .1, y = values + .04, label = color_bin),
                  fontface = "bold", hjust = 0, size = 2.2, family = "Calibri") +
        geom_text(data = chart_data %>% filter(year == max(year), country == "Lithuania"),
                  mapping = aes(x = year + .1, y = values + .03, label = color_bin),
                  fontface = "bold", hjust = 0, size = 2.2, family = "Calibri") +
        geom_text(data = chart_data %>% filter(year == max(year), country == "Montenegro"),
                  mapping = aes(x = year + .1, y = values + .03, label = color_bin),
                  fontface = "bold", hjust = 0, size = 2.2, family = "Calibri") +
        geom_text(data = chart_data %>% filter(year == max(year), country == "Poland"),
                  mapping = aes(x = year + .1, y = values, label = color_bin),
                  fontface = "bold", hjust = 0, size = 2.2, family = "Calibri") +
        geom_text(data = chart_data %>% filter(year == max(year), country == "Romania"),
                  mapping = aes(x = year + .1, y = values, label = color_bin),
                  fontface = "bold", hjust = 0, size = 2.2, family = "Calibri") +
        geom_text(data = chart_data %>% filter(year == max(year), country == "Slovakia"),
                  mapping = aes(x = year + .1, y = values, label = color_bin),
                  fontface = "bold", hjust = 0, size = 2.2, family = "Calibri") +
        geom_text(data = chart_data %>% filter(year == max(year), country == "Slovenia"),
                  mapping = aes(x = year + .1, y = values, label = color_bin),
                  fontface = "bold", hjust = 0, size = 2.2, family = "Calibri") +
        scale_color_manual(values = chart_data_color_list, guide = FALSE,
                           labels = c("Bulgaria", "Croatia", "Czechia", "Estonia", "Hungary",
                                      "Latvia", "Lithuania", "Montenegro", "Poland",
                                      "Romania", "Slovakia", "Slovenia")) +
        scale_linetype_manual(values = chart_data_linetype_list, guide = FALSE,
                              labels = c("Bulgaria", "Croatia", "Czechia", "Estonia", "Hungary",
                                         "Latvia", "Lithuania", "Montenegro", "Poland",
                                         "Romania", "Slovakia", "Slovenia")) +
        scale_y_continuous(breaks = seq(from = 0, to = 1.75, by = .25), limits = c(0, 1.8), expand = c(0, 0),
                           labels = percent_format(accuracy = 1)) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 1)) +
        labs(x = NULL, y = "Share", 
             title = NULL,
             caption = NULL, color = "", linetype = "") +
        coord_fixed(ratio = 2.5 / 1, clip = "off") +
        # geom_segment(data = segment_tbl, mapping = aes(x = x, xend = xend, y = y, yend = yend)) +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 5, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 9, face = "plain", family = "Calibri", 
                                            color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333", 
                                           margin = margin(t = 5, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333", 
                                           margin = margin(t = 0, r = 2, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333", 
                                            margin = margin(t = 0, r = 5, b = 0, l = 2)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 9, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 9, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) 

# inspect
atlas_iea_combined_fossil_imports_from_russia_as_share_of_tes_graduates_line_chart


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(atlas_iea_combined_fossil_imports_from_russia_as_share_of_tes_graduates_line_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/energy_security/atlas_iea_combined_fossil_imports_from_russia_as_share_of_tes_graduates_line_chart.docx")


#/////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////


# create fossil_imports_by_type_from_russia_as_share_of_tes_graduates_facet_line_chart ####

# inspect 
atlas %>% filter(country != "E&E graduates" & mcp_grouping == "E&E graduates") %>% 
        select(country, year, fuel_type,
               # russia_fuel_type_import_adj, global_fuel_type_import_sum_adj,
               # russia_fuel_type_import_as_share_of_global_fuel_type_import_sum_adj,
               # natural_gas_ktoe_imports_Russia, natural_gas_ktoe_imports_total,
               russia_fuel_type_import_as_share_of_global_fuel_type_import_sum_adj_w_iea_nat_gas,
               fuel_type_imports_as_share_of_tes,
               estimated_russia_fuel_type_imports_as_share_of_tes_w_iea_nat_gas,
               estimated_russia_fossil_imports_as_share_of_tes_w_iea_nat_gas)


# inspect chart
atlas %>% filter(country != "E&E graduates" & mcp_grouping == "E&E graduates") %>%
        select(country, year, fuel_type,
               estimated_russia_fuel_type_imports_as_share_of_tes_w_iea_nat_gas) %>%
        rename(var = fuel_type,
               values = estimated_russia_fuel_type_imports_as_share_of_tes_w_iea_nat_gas) %>%
        ggplot(data = ., mapping = aes(x = year, y = values, color = var)) + geom_line() + 
        facet_wrap(facets = vars(country))

# inspect latvia
# note the rise in oil products imports from russia is reflected in both the atlas and iea data, so looks legit
atlas %>% filter(country == "Latvia") %>%
        select(country, year, fuel_type,
               # russia_fuel_type_import_adj, global_fuel_type_import_sum_adj,
               # russia_fuel_type_import_as_share_of_global_fuel_type_import_sum_adj,
               # natural_gas_ktoe_imports_Russia, natural_gas_ktoe_imports_total,
               russia_fuel_type_import_as_share_of_global_fuel_type_import_sum_adj_w_iea_nat_gas,
               fuel_type_imports_as_share_of_tes,
               estimated_russia_fuel_type_imports_as_share_of_tes_w_iea_nat_gas,
               estimated_russia_fossil_imports_as_share_of_tes_w_iea_nat_gas) %>% print(n = nrow(.))

# inspect for report
chart_data %>% group_by(country, var) %>% 
        mutate(mean = mean(values)) %>% slice(1) %>%
        ungroup() %>% arrange(desc(mean)) %>% print(n = nrow(.))


#//////////////////


# add color_bin and color
chart_data <- atlas %>% filter(country != "E&E graduates" & mcp_grouping == "E&E graduates") %>%
        select(country, year, fuel_type,
               estimated_russia_fuel_type_imports_as_share_of_tes_w_iea_nat_gas) %>%
        rename(var = fuel_type,
               values = estimated_russia_fuel_type_imports_as_share_of_tes_w_iea_nat_gas) %>%
        mutate(color_bin = var,
               color = case_when(color_bin == "Natural gas" ~ color_palette %>% slice(1) %>% pull(hex),
                                 color_bin == "Coal" ~ color_palette %>% slice(2) %>% pull(hex),
                                 color_bin == "Crude oil" ~ color_palette %>% slice(3) %>% pull(hex),
                                 color_bin == "Oil products" ~ color_palette %>% slice(4) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#/////////////////////


# create chart
fossil_imports_by_type_from_russia_as_share_of_tes_graduates_facet_line_chart <- chart_data %>% 
        ggplot(data = ., mapping = aes(x = year, 
                                       y = values, 
                                       color = factor(color_bin, 
                                                      levels = c("Oil products",
                                                                 "Crude oil",
                                                                 "Coal",
                                                                 "Natural gas")))) + 
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 3), expand = c(0,0)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1.5, by = .25), 
                           limits = c(0, 1.5), 
                           expand = c(0, 0),
                           labels = percent_format(accuracy = 1)) +
        labs(x = NULL, y = "Share", 
             title = NULL,
             caption = NULL, color = "") +
        geom_line(size = 1) + facet_wrap(facets = vars(country)) +
        geom_segment(data = chart_data,
                     mapping = aes(x = chart_data %>% filter(year == min(year)) %>% slice(1) %>% pull(year) - .5,
                                   xend = chart_data %>% filter(year == max(year)) %>% slice(1) %>% pull(year) + .5,
                                   y = 0, yend = 0), color = "#333333", size = .5) + 
        coord_fixed(ratio = 4/1, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 0, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD", size = .25),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                panel.spacing.x = unit(1, "lines"),
                panel.spacing.y = unit(1, "lines"),
                strip.background = element_blank(),
                strip.text = element_text(family = "Calibri", face = "plain", size = 7, color = "#333333"),
                axis.ticks.y = element_blank(),
                # axis.ticks.y = element_line(color = "#DDDDDD"),
                # axis.ticks.x = element_blank(),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.1, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 7, color = "#333333",
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                # axis.text.x = element_blank(),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 7, color = "#333333", 
                                           margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 7, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 7, color = "#333333", 
                                            margin = margin(t = 0, r = 5, b = 0, l = 2)),
                plot.title = element_text(size = 7, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "right",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 7, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 7, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.x = unit(1.0, 'cm')
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
                # legend.key.height = unit(2,"line")
        ) +
        # guides(fill = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333"))
        guides(color = guide_legend(override.aes = list(size = 6)))

# inspect
fossil_imports_by_type_from_russia_as_share_of_tes_graduates_facet_line_chart


#///////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(fossil_imports_by_type_from_russia_as_share_of_tes_graduates_facet_line_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/energy_security/fossil_imports_by_type_from_russia_as_share_of_tes_graduates_facet_line_chart.docx")


#/////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////


# create fossil_imports_by_type_from_russia_as_share_of_tes_graduates_stacked_bar_chart ####

# inspect 
atlas %>% filter(country != "E&E graduates" & mcp_grouping == "E&E graduates") %>% 
        select(country, year, fuel_type,
               # russia_fuel_type_import_adj, global_fuel_type_import_sum_adj,
               # russia_fuel_type_import_as_share_of_global_fuel_type_import_sum_adj,
               # natural_gas_ktoe_imports_Russia, natural_gas_ktoe_imports_total,
               russia_fuel_type_import_as_share_of_global_fuel_type_import_sum_adj_w_iea_nat_gas,
               fuel_type_imports_as_share_of_tes,
               estimated_russia_fuel_type_imports_as_share_of_tes_w_iea_nat_gas,
               estimated_russia_fossil_imports_as_share_of_tes_w_iea_nat_gas)


# inspect chart
atlas %>% filter(country != "E&E graduates" & mcp_grouping == "E&E graduates",
                 year == 2018) %>% 
        select(country, year, fuel_type,
               estimated_russia_fuel_type_imports_as_share_of_tes_w_iea_nat_gas) %>%
        rename(var = fuel_type,
               values = estimated_russia_fuel_type_imports_as_share_of_tes_w_iea_nat_gas) %>%
        ggplot(data = ., mapping = aes(x = country, y = values, fill = var)) + 
        geom_bar(position = position_stack(), stat = "identity", width = .8)


#//////////////////


# add color_bin and color
chart_data <- atlas %>% filter(country != "E&E graduates" & mcp_grouping == "E&E graduates",
                               year == 2018) %>%
        select(country, year, fuel_type,
               estimated_russia_fuel_type_imports_as_share_of_tes_w_iea_nat_gas) %>%
        ungroup() %>%
        rename(var = fuel_type,
               values = estimated_russia_fuel_type_imports_as_share_of_tes_w_iea_nat_gas) %>%
        group_by(country) %>% 
        mutate(values_sum = sum(values)) %>%
        ungroup() %>%
        mutate(color_bin = var,
               color = case_when(color_bin == "Natural gas" ~ color_palette %>% slice(1) %>% pull(hex),
                                 color_bin == "Coal" ~ color_palette %>% slice(2) %>% pull(hex),
                                 color_bin == "Crude oil" ~ color_palette %>% slice(3) %>% pull(hex),
                                 color_bin == "Oil products" ~ color_palette %>% slice(4) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list

# title
title <- "Estimated fossil fuel imports from Russia by type, as a share of primary energy use (2018)"


#/////////////////////


# create chart
fossil_imports_by_type_from_russia_as_share_of_tes_graduates_stacked_bar_chart <- chart_data %>% 
        ggplot(data = ., aes(x = fct_reorder(.f = country, .x = values_sum, .desc = FALSE), 
                             y = values, 
                             fill = factor(color_bin, 
                                           levels = c("Oil products",
                                                      "Crude oil", "Coal",
                                                      "Natural gas")))) + 
        geom_bar(position = position_stack(), stat = "identity", width = .8) + # stack exact values
        # geom_bar(position = "fill", stat = "identity", width = .8) + # auto-convert to shares summing to 1
        scale_fill_manual(values = chart_data_color_list) +
        scale_x_discrete(expand = c(0, 0)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1.5, by = .25), 
                           limits = c(0, 1.5), 
                           expand = c(0, 0),
                           labels = percent_format(accuracy = 1)) +
        labs(x = NULL, y = "Share", 
             title = NULL,
             caption = NULL, fill = "") +
        coord_fixed(ratio = 3.5/1, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 0, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                axis.ticks.y = element_line(color = "#DDDDDD"),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = 0, l = 0), angle = 45, hjust = 1),
                # axis.text.x = element_blank(),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333", 
                                           margin = margin(t = 0, r = 5, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333", 
                                            margin = margin(t = 0, r = 5, b = 0, l = 2)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "right",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 9, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 9, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) 
# guides(fill = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333"))

# inspect
fossil_imports_by_type_from_russia_as_share_of_tes_graduates_stacked_bar_chart


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(fossil_imports_by_type_from_russia_as_share_of_tes_graduates_stacked_bar_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/energy_security/fossil_imports_by_type_from_russia_as_share_of_tes_graduates_stacked_bar_chart.docx")


#/////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////


# create fossil_imports_by_type_from_russia_as_share_of_tes_stacked_bar_chart ####

# inspect 
atlas %>% filter((country != "E&E Balkans" & mcp_grouping == "E&E Balkans") | 
                         (country != "E&E Eurasia" & mcp_grouping == "E&E Eurasia")) %>% 
        select(country, year, fuel_type,
               # russia_fuel_type_import_adj, global_fuel_type_import_sum_adj,
               # russia_fuel_type_import_as_share_of_global_fuel_type_import_sum_adj,
               # natural_gas_ktoe_imports_Russia, natural_gas_ktoe_imports_total,
               russia_fuel_type_import_as_share_of_global_fuel_type_import_sum_adj_w_iea_nat_gas,
               fuel_type_imports_as_share_of_tes,
               estimated_russia_fuel_type_imports_as_share_of_tes_w_iea_nat_gas,
               estimated_russia_fossil_imports_as_share_of_tes_w_iea_nat_gas)


# inspect chart
atlas %>% filter((country != "E&E Balkans" & mcp_grouping == "E&E Balkans") | 
                         (country != "E&E Eurasia" & mcp_grouping == "E&E Eurasia"),
                 year == 2018) %>% 
        select(country, year, fuel_type,
               estimated_russia_fuel_type_imports_as_share_of_tes_w_iea_nat_gas) %>%
        rename(var = fuel_type,
               values = estimated_russia_fuel_type_imports_as_share_of_tes_w_iea_nat_gas) %>%
        ggplot(data = ., mapping = aes(x = country, y = values, fill = var)) + 
        geom_bar(position = position_stack(), stat = "identity", width = .8)


#//////////////////


# add color_bin and color
chart_data <- atlas %>% filter((country != "E&E Balkans" & mcp_grouping == "E&E Balkans") | 
                                       (country != "E&E Eurasia" & mcp_grouping == "E&E Eurasia"),
                               year == 2018) %>% 
        select(country, year, fuel_type,
               estimated_russia_fuel_type_imports_as_share_of_tes_w_iea_nat_gas) %>%
        ungroup() %>%
        rename(var = fuel_type,
               values = estimated_russia_fuel_type_imports_as_share_of_tes_w_iea_nat_gas) %>%
        group_by(country) %>% 
        mutate(values_sum = sum(values)) %>%
        ungroup() %>%
        mutate(color_bin = var,
               color = case_when(color_bin == "Natural gas" ~ color_palette %>% slice(1) %>% pull(hex),
                                 color_bin == "Coal" ~ color_palette %>% slice(2) %>% pull(hex),
                                 color_bin == "Crude oil" ~ color_palette %>% slice(3) %>% pull(hex),
                                 color_bin == "Oil products" ~ color_palette %>% slice(4) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list

# title
title <- "Estimated fossil fuel imports from Russia by type, as a share of primary energy use (2018)"


#/////////////////////


# create chart
fossil_imports_by_type_from_russia_as_share_of_tes_stacked_bar_chart <- chart_data %>% 
        ggplot(data = ., aes(x = fct_reorder(.f = country, .x = values_sum, .desc = FALSE), 
                             y = values, 
                             fill = factor(color_bin, 
                                           levels = c("Oil products",
                                                      "Crude oil", "Coal",
                                                      "Natural gas")))) + 
        geom_bar(position = position_stack(), stat = "identity", width = .8) + # stack exact values
        # geom_bar(position = "fill", stat = "identity", width = .8) + # auto-convert to shares summing to 1
        scale_fill_manual(values = chart_data_color_list) +
        scale_x_discrete(expand = c(0, 0)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1.5, by = .25), 
                           limits = c(0, 1.5), 
                           expand = c(0, 0),
                           labels = percent_format(accuracy = 1)) +
        labs(x = NULL, y = "Share", 
             title = NULL,
             caption = NULL, fill = "") +
        coord_fixed(ratio = 3/1, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 0, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                axis.ticks.y = element_line(color = "#DDDDDD"),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = 0, l = 0), angle = 45, hjust = 1),
                # axis.text.x = element_blank(),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333", 
                                           margin = margin(t = 0, r = 5, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333", 
                                            margin = margin(t = 0, r = 5, b = 0, l = 2)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "right",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 9, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 9, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) 
# guides(fill = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333"))

# inspect
fossil_imports_by_type_from_russia_as_share_of_tes_stacked_bar_chart


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(fossil_imports_by_type_from_russia_as_share_of_tes_stacked_bar_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/energy_security/fossil_imports_by_type_from_russia_as_share_of_tes_stacked_bar_chart.docx")


#/////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////


# atlas_fossil_imports_from_russia_as_share_of_global_fossil_import_sum_graduates_line_chart ####


# inspect
atlas %>% filter(country != "E&E graduates", mcp_grouping == "E&E graduates") %>%
        select(country, year, fuel_type, russia_fuel_type_import_adj, global_fuel_type_import_sum_adj,
               russia_fossil_import_sum, global_fossil_import_sum, 
               russia_fossil_import_sum_as_share_of_global_fossil_import_sum) 

atlas %>% filter(country != "E&E graduates", mcp_grouping == "E&E graduates") %>%
        select(country, year, russia_fossil_import_sum_as_share_of_global_fossil_import_sum) %>% 
        group_by(country, year, russia_fossil_import_sum_as_share_of_global_fossil_import_sum) %>% slice(1) %>%
        ungroup() %>% print(n = nrow(.))

atlas %>% filter(country != "E&E graduates", mcp_grouping == "E&E graduates") %>%
        select(country, year, russia_fossil_import_sum_as_share_of_global_fossil_import_sum) %>% 
        group_by(country, year, russia_fossil_import_sum_as_share_of_global_fossil_import_sum) %>% slice(1) %>%
        ungroup() %>% ggplot(data = ., mapping = aes(x = year, y = russia_fossil_import_sum_as_share_of_global_fossil_import_sum)) + 
        geom_line() + facet_wrap(facets = vars(country))

# inspect for report
chart_data %>% group_by(country) %>% summarize(mean = mean(values)) %>% arrange(mean)


#//////////////////


# add color_bin and color
chart_data <- atlas %>% 
        filter(!(country %in% c("E&E graduates")), 
               mcp_grouping %in% c("E&E graduates")) %>%
        group_by(country, year) %>% 
        mutate(values = estimated_russia_fossil_imports_as_share_of_tes_w_iea_nat_gas) %>%
        slice(1) %>%
        ungroup() %>%
        select(country, year, location_code, values) %>%
        mutate(color_bin = country,
               color = case_when(color_bin == "Bulgaria" ~ color_palette %>% slice(1) %>% pull(hex),
                                 color_bin == "Croatia" ~ color_palette %>% slice(2) %>% pull(hex),
                                 color_bin == "Czechia" ~ color_palette %>% slice(3) %>% pull(hex),
                                 color_bin == "Estonia" ~ color_palette %>% slice(4) %>% pull(hex),
                                 color_bin == "Hungary" ~ color_palette %>% slice(5) %>% pull(hex),
                                 color_bin == "Latvia" ~ color_palette %>% slice(6) %>% pull(hex),
                                 color_bin == "Lithuania" ~ color_palette %>% slice(7) %>% pull(hex),
                                 color_bin == "Montenegro" ~ color_palette %>% slice(8) %>% pull(hex),
                                 color_bin == "Poland" ~ color_palette %>% slice(9) %>% pull(hex),
                                 color_bin == "Romania" ~ color_palette %>% slice(10) %>% pull(hex),
                                 color_bin == "Slovakia" ~ color_palette %>% slice(11) %>% pull(hex),
                                 color_bin == "Slovenia" ~ color_palette %>% slice(1) %>% pull(hex)),
               linetype_bin = country,
               linetype = case_when(country == "Slovenia" ~ "dotted",
                                    TRUE ~ "solid"))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list

# create linetype_list for to pass to scale_linetype_manual
chart_data_linetype_list <- chart_data %>% count(linetype_bin, linetype) %>% pull(linetype)
names(chart_data_linetype_list) <- chart_data %>% count(linetype_bin, linetype) %>% pull(linetype_bin)
chart_data_linetype_list

# title
"Estimated fossil fuel imports from Russia as a share of total primary energy use in E&E graduates"


#/////////////////////


# create chart
atlas_fossil_imports_from_russia_as_share_of_global_fossil_import_sum_graduates_line_chart <- chart_data %>%
        ggplot(data = ., aes(x = year, 
                             y = values, 
                             color = factor(color_bin, levels = c("Bulgaria", "Croatia", "Czechia", "Estonia", "Hungary",
                                                                  "Latvia", "Lithuania", "Montenegro", "Poland",
                                                                  "Romania", "Slovakia", "Slovenia")),
                             linetype = factor(color_bin, levels = c("Bulgaria", "Croatia", "Czechia", "Estonia", "Hungary",
                                                                     "Latvia", "Lithuania", "Montenegro", "Poland",
                                                                     "Romania", "Slovakia", "Slovenia")))) + 
        geom_line(size = 1) +
        # geom_point(size = 2) +
        geom_text(data = chart_data %>% filter(year == max(year), country == "Bulgaria"),
                  mapping = aes(x = year + .1, y = values, label = color_bin),
                  fontface = "bold", hjust = 0, size = 2.2, family = "Calibri") +
        geom_text(data = chart_data %>% filter(year == max(year), country == "Croatia"),
                  mapping = aes(x = year + .1, y = values - .04, label = color_bin),
                  fontface = "bold", hjust = 0, size = 2.2, family = "Calibri") +
        geom_text(data = chart_data %>% filter(year == max(year), country == "Czechia"),
                  mapping = aes(x = year + .1, y = values + .05, label = color_bin),
                  fontface = "bold", hjust = 0, size = 2.2, family = "Calibri") +
        geom_text(data = chart_data %>% filter(year == max(year), country == "Estonia"),
                  mapping = aes(x = year + .1, y = values + .02, label = color_bin),
                  fontface = "bold", hjust = 0, size = 2.2, family = "Calibri") +
        geom_text(data = chart_data %>% filter(year == max(year), country == "Hungary"),
                  mapping = aes(x = year + .1, y = values, label = color_bin),
                  fontface = "bold", hjust = 0, size = 2.2, family = "Calibri") +
        geom_text(data = chart_data %>% filter(year == max(year), country == "Latvia"),
                  mapping = aes(x = year + .1, y = values + .04, label = color_bin),
                  fontface = "bold", hjust = 0, size = 2.2, family = "Calibri") +
        geom_text(data = chart_data %>% filter(year == max(year), country == "Lithuania"),
                  mapping = aes(x = year + .1, y = values + .03, label = color_bin),
                  fontface = "bold", hjust = 0, size = 2.2, family = "Calibri") +
        geom_text(data = chart_data %>% filter(year == max(year), country == "Montenegro"),
                  mapping = aes(x = year + .1, y = values + .03, label = color_bin),
                  fontface = "bold", hjust = 0, size = 2.2, family = "Calibri") +
        geom_text(data = chart_data %>% filter(year == max(year), country == "Poland"),
                  mapping = aes(x = year + .1, y = values, label = color_bin),
                  fontface = "bold", hjust = 0, size = 2.2, family = "Calibri") +
        geom_text(data = chart_data %>% filter(year == max(year), country == "Romania"),
                  mapping = aes(x = year + .1, y = values, label = color_bin),
                  fontface = "bold", hjust = 0, size = 2.2, family = "Calibri") +
        geom_text(data = chart_data %>% filter(year == max(year), country == "Slovakia"),
                  mapping = aes(x = year + .1, y = values, label = color_bin),
                  fontface = "bold", hjust = 0, size = 2.2, family = "Calibri") +
        geom_text(data = chart_data %>% filter(year == max(year), country == "Slovenia"),
                  mapping = aes(x = year + .1, y = values, label = color_bin),
                  fontface = "bold", hjust = 0, size = 2.2, family = "Calibri") +
        scale_color_manual(values = chart_data_color_list, guide = FALSE,
                           labels = c("Bulgaria", "Croatia", "Czechia", "Estonia", "Hungary",
                                      "Latvia", "Lithuania", "Montenegro", "Poland",
                                      "Romania", "Slovakia", "Slovenia")) +
        scale_linetype_manual(values = chart_data_linetype_list, guide = FALSE,
                              labels = c("Bulgaria", "Croatia", "Czechia", "Estonia", "Hungary",
                                         "Latvia", "Lithuania", "Montenegro", "Poland",
                                         "Romania", "Slovakia", "Slovenia")) +
        scale_y_continuous(breaks = seq(from = 0, to = 1.75, by = .25), limits = c(0, 1.8), expand = c(0, 0),
                           labels = percent_format(accuracy = 1)) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 1)) +
        labs(x = NULL, y = "Share", 
             title = NULL,
             caption = NULL, color = "", linetype = "") +
        coord_fixed(ratio = 2.5 / 1, clip = "off") +
        # geom_segment(data = segment_tbl, mapping = aes(x = x, xend = xend, y = y, yend = yend)) +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 5, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 9, face = "plain", family = "Calibri", 
                                            color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333", 
                                           margin = margin(t = 5, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333", 
                                           margin = margin(t = 0, r = 2, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333", 
                                            margin = margin(t = 0, r = 5, b = 0, l = 2)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 9, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 9, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) 

# inspect
atlas_fossil_imports_from_russia_as_share_of_global_fossil_import_sum_graduates_line_chart


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(atlas_fossil_imports_from_russia_as_share_of_global_fossil_import_sum_graduates_line_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/energy_security/atlas_fossil_imports_from_russia_as_share_of_global_fossil_import_sum_graduates_line_chart.docx")


#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////


# create net_imports_by_type_as_share_of_tes_stacked_bar_chart ####


# check raw data
iea %>% filter(year == 2018, country %in% c("Belarus")) %>%
        select(country, year, matches("^net_.*_imports_as_share_of_tes")) %>%
        pivot_longer(cols = -c(country, year), names_to = "var", values_to = "values") %>%
        select(country, year, var, values)

# confirm that the sum of positive values only will rise above 100%
iea %>% filter(year == 2018, country %in% c("Belarus")) %>%
        select(country, year, matches("^net_.*_imports_as_share_of_tes")) %>%
        pivot_longer(cols = -c(country, year), names_to = "var", values_to = "values") %>%
        select(country, year, var, values) %>% 
        filter(var != "net_energy_imports_as_share_of_tes", values > 0) %>% 
        mutate(sum = sum(values))


# inspect energy types with zero share of production across all countries/years, to exclude from chart
# note heat, hydro, nuclear, and wind_solar always have zero imports, and will be excluded
iea %>% filter((country != "E&E Balkans" & mcp_grouping == "E&E Balkans") | 
                       (country != "E&E Eurasia" & mcp_grouping == "E&E Eurasia")) %>% 
        select(country, year, matches(match = "^Imports")) %>%
        pivot_longer(cols = -c(country, year), names_to = "var", values_to = "values") %>%
        group_by(var) %>% skim(values)
iea %>% filter(Production_electricity > 0) %>% nrow() # 0
iea %>% filter(Production_oil_products > 0) %>% nrow() # 0


# inspect chart
# note azerbaijan's crude oil net imports will be dropped because it is -228%
iea %>% filter((country != "E&E Balkans" & mcp_grouping == "E&E Balkans") | 
                       (country != "E&E Eurasia" & mcp_grouping == "E&E Eurasia")) %>%
        select(country, year, matches("^net_.*_imports_as_share_of_tes"), 
               -c(net_energy_imports_as_share_of_tes)) %>% 
        pivot_longer(cols = -c(country, year), names_to = "var", values_to = "values") %>%
        filter(!(var %in% c("net_heat_imports_as_share_of_tes",
                            "net_hydro_imports_as_share_of_tes",
                            "net_nuclear_imports_as_share_of_tes",
                            "net_wind_solar_etc_imports_as_share_of_tes")),
               !(country == "Azerbaijan" & var == "net_crude_oil_imports_as_share_of_tes"),
               year == 2018) %>%
        ggplot(data = ., mapping = aes(x = country, y = values, fill = var)) + 
        geom_bar(position = position_stack(), stat = "identity", width = .8)


#///////////////////////////


# add color_bin and color
chart_data <- iea %>% filter((country != "E&E Balkans" & mcp_grouping == "E&E Balkans") | 
                                     (country != "E&E Eurasia" & mcp_grouping == "E&E Eurasia")) %>%
        select(country, year, matches("^net_.*_imports_as_share_of_tes"), 
               -c(net_energy_imports_as_share_of_tes)) %>% 
        pivot_longer(cols = -c(country, year), names_to = "var", values_to = "values") %>%
        filter(!(var %in% c("net_heat_imports_as_share_of_tes",
                            "net_hydro_imports_as_share_of_tes",
                            "net_nuclear_imports_as_share_of_tes",
                            "net_wind_solar_etc_imports_as_share_of_tes")),
               !(country == "Azerbaijan" & var == "net_crude_oil_imports_as_share_of_tes"),
               year == 2018) %>%
        group_by(country) %>% mutate(values_sum = sum(values)) %>%
        ungroup() %>%
        mutate(var = case_when(var == "net_natural_gas_imports_as_share_of_tes" ~ "Natural gas",
                               var == "net_coal_imports_as_share_of_tes" ~ "Coal",
                               var == "net_crude_oil_imports_as_share_of_tes" ~ "Crude oil",
                               var == "net_oil_products_imports_as_share_of_tes" ~ "Oil products",
                               var == "net_electricity_imports_as_share_of_tes" ~ "Electricity",
                               var == "net_biofuels_and_waste_imports_as_share_of_tes" ~ "Biofuels and waste"
                               # var == "net_heat_imports_as_share_of_tes" ~ "Heat",
                               # var == "net_hydro_imports_as_share_of_tes" ~ "Hydro",
                               # var == "net_nuclear_imports_as_share_of_tes" ~ "Nuclear",
                               # var == "net_wind_solar_etc_imports_as_share_of_tes" ~ "Wind, solar, etc."
        )) %>%
        mutate(color_bin = var,
               color = case_when(color_bin == "Natural gas" ~ color_palette %>% slice(1) %>% pull(hex),
                                 color_bin == "Coal" ~ color_palette %>% slice(2) %>% pull(hex),
                                 color_bin == "Crude oil" ~ color_palette %>% slice(3) %>% pull(hex),
                                 color_bin == "Oil products" ~ color_palette %>% slice(4) %>% pull(hex),
                                 color_bin == "Electricity" ~ color_palette %>% slice(5) %>% pull(hex),
                                 color_bin == "Biofuels and waste" ~ color_palette %>% slice(6) %>% pull(hex)
                                 # color_bin == "Heat" ~ color_palette %>% slice(7) %>% pull(hex),
                                 # color_bin == "Hydro" ~ "#99ba78",
                                 # color_bin == "Nuclear" ~ "#24A99C",
                                 # color_bin == "Wind, solar, etc." ~ "#2E6657"
               ))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#/////////////////////


# create chart
net_imports_by_type_as_share_of_tes_stacked_bar_chart <- chart_data %>% 
        ggplot(data = ., aes(x = fct_reorder(.f = country, .x = values_sum, .desc = FALSE),
                             y = values, 
                             fill = factor(color_bin, 
                                           levels = c("Wind, solar, etc.",
                                                      "Nuclear", "Hydro",
                                                      "Heat", "Biofuels and waste",
                                                      "Electricity", "Oil products",
                                                      "Crude oil", "Coal",
                                                      "Natural gas")))) + 
        geom_bar(position = position_stack(), stat = "identity", width = .8) + # stack exact values
        # geom_bar(position = "fill", stat = "identity", width = .8) + # auto-convert to shares summing to 1
        scale_fill_manual(values = chart_data_color_list) +
        scale_x_discrete(expand = c(0, 0)) +
        scale_y_continuous(breaks = seq(from = -.5, to = 1.25, by = .25), 
                           limits = c(-.5, 1.3), 
                           expand = c(0, 0),
                           labels = percent_format(accuracy = 1)) +
        labs(x = NULL, y = "Share", 
             title = NULL,
             caption = NULL, fill = "") +
        coord_fixed(ratio = 3/1, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 0, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                axis.ticks.y = element_line(color = "#DDDDDD"),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = 0, l = 0), angle = 45, hjust = 1),
                # axis.text.x = element_blank(),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333", 
                                           margin = margin(t = 0, r = 5, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333", 
                                            margin = margin(t = 0, r = 5, b = 0, l = 2)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "right",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 9, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 9, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) 
# guides(fill = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333"))

# inspect
net_imports_by_type_as_share_of_tes_stacked_bar_chart 


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(net_imports_by_type_as_share_of_tes_stacked_bar_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/energy_security/net_imports_by_type_as_share_of_tes_stacked_bar_chart.docx")


#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////


# create iea_production_as_share_of_tes_stacked_bar_chart ####

# inspect energy types with zero share of production across all countries/years, to exclude from chart
# note electricity and oil products have zero share of production; 
# note electricity is already excluded, 
# oil products will be manually dropped for chart
iea %>% filter((country != "E&E Balkans" & mcp_grouping == "E&E Balkans") | 
                       (country != "E&E Eurasia" & mcp_grouping == "E&E Eurasia")) %>% 
        select(country, year, matches(match = "as_share_of_overall_production")) %>%
        pivot_longer(cols = -c(country, year), names_to = "var", values_to = "values") %>%
        group_by(var) %>% skim(values)
iea %>% filter(Production_electricity > 0) %>% nrow() # 0
iea %>% filter(Production_oil_products > 0) %>% nrow() # 0


# inspect azerbaijan's crude production is 328% of TES avg for the period; 271% of TES for 2018
iea %>% filter((country != "E&E Balkans" & mcp_grouping == "E&E Balkans") | 
                       (country != "E&E Eurasia" & mcp_grouping == "E&E Eurasia")) %>%
        select(country, year, matches("^Production_"), -c(Production_total, production_as_share_of_overall_production_sum), 
               TES_total) %>% 
        pivot_longer(cols = -c(country, year, TES_total), names_to = "var", values_to = "production_by_type") %>%
        filter(country == "Azerbaijan" & var == "Production_crude_oil") %>%
        mutate(production_as_share_of_tes = production_by_type / TES_total,
               production_as_share_of_tes_avg = mean(production_as_share_of_tes))

# inspect chart
iea %>% filter((country != "E&E Balkans" & mcp_grouping == "E&E Balkans") | 
                       (country != "E&E Eurasia" & mcp_grouping == "E&E Eurasia")) %>%
        select(country, year, matches("^Production_"), -c(Production_total, production_as_share_of_overall_production_sum), 
               TES_total) %>% 
        pivot_longer(cols = -c(country, year, TES_total), names_to = "var", values_to = "production_by_type") %>%
        filter(!(var %in% c("Production_oil_products", "Production_electricity")),
               !(country == "Azerbaijan" & var == "Production_crude_oil"),
               year == 2018) %>%
        mutate(production_as_share_of_tes = production_by_type / TES_total) %>%
        rename(values = production_as_share_of_tes) %>%
        ggplot(data = ., mapping = aes(x = country, y = values, fill = var)) + 
        geom_bar(position = position_stack(), stat = "identity", width = .8)


#///////////////////////////


# add color_bin and color
chart_data <- iea %>% filter((country != "E&E Balkans" & mcp_grouping == "E&E Balkans") | 
                                     (country != "E&E Eurasia" & mcp_grouping == "E&E Eurasia")) %>%
        select(country, year, matches("^Production_"), -c(Production_total, production_as_share_of_overall_production_sum), 
               TES_total) %>% 
        pivot_longer(cols = -c(country, year, TES_total), names_to = "var", values_to = "production_by_type") %>%
        filter(!(var %in% c("Production_oil_products", "Production_electricity")),
               !(country == "Azerbaijan" & var == "Production_crude_oil"),
               year == 2018) %>%
        mutate(production_as_share_of_tes = production_by_type / TES_total) %>%
        rename(values = production_as_share_of_tes) %>%
        group_by(country) %>% mutate(values_sum = sum(values)) %>%
        ungroup() %>%
        mutate(var = case_when(var == "Production_natural_gas" ~ "Natural gas",
                               var == "Production_coal" ~ "Coal",
                               var == "Production_crude_oil" ~ "Crude oil",
                               # var == "Production_oil_products" ~ "Oil products",
                               # var == "Production_electricity" ~ "Electricity",
                               var == "Production_biofuels_and_waste" ~ "Biofuels and waste",
                               var == "Production_heat" ~ "Heat",
                               var == "Production_hydro" ~ "Hydro",
                               var == "Production_nuclear" ~ "Nuclear",
                               var == "Production_wind_solar_etc" ~ "Wind, solar, etc.")) %>%
        mutate(color_bin = var,
               color = case_when(color_bin == "Natural gas" ~ color_palette %>% slice(1) %>% pull(hex),
                                 color_bin == "Coal" ~ color_palette %>% slice(2) %>% pull(hex),
                                 color_bin == "Crude oil" ~ color_palette %>% slice(3) %>% pull(hex),
                                 # color_bin == "Oil products" ~ color_palette %>% slice(4) %>% pull(hex),
                                 # color_bin == "Electricity" ~ color_palette %>% slice(5) %>% pull(hex),
                                 color_bin == "Biofuels and waste" ~ color_palette %>% slice(6) %>% pull(hex),
                                 color_bin == "Hydro" ~ color_palette %>% slice(7) %>% pull(hex),
                                 color_bin == "Nuclear" ~ color_palette %>% slice(8) %>% pull(hex),
                                 color_bin == "Wind, solar, etc." ~ color_palette %>% slice(9) %>% pull(hex),
                                 color_bin == "Heat" ~ color_palette %>% slice(10) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#/////////////////////


# create chart
iea_production_as_share_of_tes_stacked_bar_chart <- chart_data %>% 
        ggplot(data = ., aes(x = fct_reorder(.f = country, .x = values_sum, .desc = FALSE), 
                             y = values, 
                             fill = factor(color_bin, 
                                           levels = c("Heat", "Wind, solar, etc.",
                                                      "Nuclear", "Hydro",
                                                      "Biofuels and waste",
                                                      "Electricity", "Oil products",
                                                      "Crude oil", "Coal",
                                                      "Natural gas")))) + 
        geom_bar(position = position_stack(), stat = "identity", width = .8) + # stack exact values
        # geom_bar(position = "fill", stat = "identity", width = .8) + # auto-convert to shares summing to 1 
        scale_fill_manual(values = chart_data_color_list) +
        scale_x_discrete(expand = c(0, 0)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1.2, by = .2), 
                           limits = c(0, 1.2), 
                           expand = c(0, 0),
                           labels = percent_format(accuracy = 1)) +
        labs(x = NULL, y = "Share", 
             title = NULL,
             caption = NULL, fill = "") +
        coord_fixed(ratio = 6/1, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 0, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                axis.ticks.y = element_line(color = "#DDDDDD"),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = 0, l = 0), angle = 45, hjust = 1),
                # axis.text.x = element_blank(),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333", 
                                           margin = margin(t = 0, r = 5, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333", 
                                            margin = margin(t = 0, r = 5, b = 0, l = 2)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "right",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 9, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 9, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) 
# guides(fill = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333"))

# inspect
iea_production_as_share_of_tes_stacked_bar_chart


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(iea_production_as_share_of_tes_stacked_bar_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/energy_security/iea_production_as_share_of_tes_stacked_bar_chart.docx")


#/////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////


# create russia_fossil_import_sum_as_share_of_global_fossil_import_sum_graduates_line_chart ####

# inspect
atlas %>% filter(country != "E&E graduates", mcp_grouping == "E&E graduates") %>%
        select(country, year, fuel_type, russia_fuel_type_import_adj, global_fuel_type_import_sum_adj,
               russia_fossil_import_sum, global_fossil_import_sum, 
               russia_fossil_import_sum_as_share_of_global_fossil_import_sum) 
atlas %>% filter(country != "E&E graduates", mcp_grouping == "E&E graduates") %>%
        select(country, year, russia_fossil_import_sum_as_share_of_global_fossil_import_sum) %>% 
        group_by(country, year, russia_fossil_import_sum_as_share_of_global_fossil_import_sum) %>% slice(1) %>%
        ungroup() %>% print(n = nrow(.))
atlas %>% filter(country != "E&E graduates", mcp_grouping == "E&E graduates") %>%
        select(country, year, russia_fossil_import_sum_as_share_of_global_fossil_import_sum) %>% 
        group_by(country, year, russia_fossil_import_sum_as_share_of_global_fossil_import_sum) %>% slice(1) %>%
        ungroup() %>% ggplot(data = ., mapping = aes(x = year, y = russia_fossil_import_sum_as_share_of_global_fossil_import_sum)) + 
        geom_line() + facet_wrap(facets = vars(country))

# inspect for report
chart_data %>% group_by(country) %>% summarize(mean = mean(russia_fossil_import_sum_as_share_of_global_fossil_import_sum)) %>%
        arrange(mean)
chart_data %>% group_by(year) %>% summarize(mean = mean(russia_fossil_import_sum_as_share_of_global_fossil_import_sum)) %>%
        ungroup() %>% mutate(overall_mean = mean(mean))
chart_data %>% filter(year %in% c(2009, 2018)) %>% 
        group_by(country) %>%
        mutate(diff = russia_fossil_import_sum_as_share_of_global_fossil_import_sum - 
                       lag(russia_fossil_import_sum_as_share_of_global_fossil_import_sum, n = 1)) %>%
        # arrange(diff) %>%
        print(n = nrow(.))


#//////////////////


# add color_bin and color
chart_data <- atlas %>% filter(country != "E&E graduates", mcp_grouping == "E&E graduates") %>%
        select(country, year, russia_fossil_import_sum_as_share_of_global_fossil_import_sum) %>% 
        group_by(country, year, russia_fossil_import_sum_as_share_of_global_fossil_import_sum) %>% slice(1) %>%
        ungroup() %>% mutate(values = russia_fossil_import_sum_as_share_of_global_fossil_import_sum) %>%
        mutate(color_bin = country,
               color = case_when(color_bin == "Bulgaria" ~ color_palette %>% slice(1) %>% pull(hex),
                                 color_bin == "Croatia" ~ color_palette %>% slice(2) %>% pull(hex),
                                 color_bin == "Czechia" ~ color_palette %>% slice(3) %>% pull(hex),
                                 color_bin == "Estonia" ~ color_palette %>% slice(4) %>% pull(hex),
                                 color_bin == "Hungary" ~ color_palette %>% slice(5) %>% pull(hex),
                                 color_bin == "Latvia" ~ color_palette %>% slice(6) %>% pull(hex),
                                 color_bin == "Lithuania" ~ color_palette %>% slice(7) %>% pull(hex),
                                 color_bin == "Montenegro" ~ color_palette %>% slice(8) %>% pull(hex),
                                 color_bin == "Poland" ~ color_palette %>% slice(9) %>% pull(hex),
                                 color_bin == "Romania" ~ color_palette %>% slice(10) %>% pull(hex),
                                 color_bin == "Slovakia" ~ color_palette %>% slice(11) %>% pull(hex),
                                 color_bin == "Slovenia" ~ color_palette %>% slice(1) %>% pull(hex)),
               linetype_bin = country,
               linetype = case_when(country == "Slovenia" ~ "dotted",
                                    TRUE ~ "solid"))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list

# create linetype_list for to pass to scale_linetype_manual
chart_data_linetype_list <- chart_data %>% count(linetype_bin, linetype) %>% pull(linetype)
names(chart_data_linetype_list) <- chart_data %>% count(linetype_bin, linetype) %>% pull(linetype_bin)
chart_data_linetype_list

# title
title <- "Fossil fuel imports from Russia as a share of total fossil fuel imports, by value"


#/////////////////////


# create chart
russia_fossil_import_sum_as_share_of_global_fossil_import_sum_graduates_line_chart <- chart_data %>%
        ggplot(data = ., aes(x = year, 
                             y = values, 
                             color = factor(color_bin, levels = c("Bulgaria", "Croatia", "Czechia", "Estonia", "Hungary",
                                                                  "Latvia", "Lithuania", "Montenegro", "Poland",
                                                                  "Romania", "Slovakia", "Slovenia")),
                             linetype = factor(color_bin, levels = c("Bulgaria", "Croatia", "Czechia", "Estonia", "Hungary",
                                                                     "Latvia", "Lithuania", "Montenegro", "Poland",
                                                                     "Romania", "Slovakia", "Slovenia")))) + 
        geom_line(size = 1) +
        # geom_point(size = 2) +
        geom_text(data = chart_data %>% filter(year == max(year), country == "Bulgaria"),
                  mapping = aes(x = year + .1, y = values + .01, label = color_bin),
                  fontface = "bold", hjust = 0, size = 2.2, family = "Calibri") +
        geom_text(data = chart_data %>% filter(year == max(year), country == "Croatia"),
                  mapping = aes(x = year + .1, y = values, label = color_bin),
                  fontface = "bold", hjust = 0, size = 2.2, family = "Calibri") +
        geom_text(data = chart_data %>% filter(year == max(year), country == "Czechia"),
                  mapping = aes(x = year + .1, y = values, label = color_bin),
                  fontface = "bold", hjust = 0, size = 2.2, family = "Calibri") +
        geom_text(data = chart_data %>% filter(year == max(year), country == "Estonia"),
                  mapping = aes(x = year + .1, y = values - .02, label = color_bin),
                  fontface = "bold", hjust = 0, size = 2.2, family = "Calibri") +
        geom_text(data = chart_data %>% filter(year == max(year), country == "Hungary"),
                  mapping = aes(x = year + .1, y = values, label = color_bin),
                  fontface = "bold", hjust = 0, size = 2.2, family = "Calibri") +
        geom_text(data = chart_data %>% filter(year == max(year), country == "Latvia"),
                  mapping = aes(x = year + .1, y = values, label = color_bin),
                  fontface = "bold", hjust = 0, size = 2.2, family = "Calibri") +
        geom_text(data = chart_data %>% filter(year == max(year), country == "Lithuania"),
                  mapping = aes(x = year + .1, y = values, label = color_bin),
                  fontface = "bold", hjust = 0, size = 2.2, family = "Calibri") +
        geom_text(data = chart_data %>% filter(year == max(year), country == "Montenegro"),
                  mapping = aes(x = year + .1, y = values + .01, label = color_bin),
                  fontface = "bold", hjust = 0, size = 2.2, family = "Calibri") +
        geom_text(data = chart_data %>% filter(year == max(year), country == "Poland"),
                  mapping = aes(x = year + .1, y = values - .03, label = color_bin),
                  fontface = "bold", hjust = 0, size = 2.2, family = "Calibri") +
        geom_text(data = chart_data %>% filter(year == max(year), country == "Romania"),
                  mapping = aes(x = year + .1, y = values, label = color_bin),
                  fontface = "bold", hjust = 0, size = 2.2, family = "Calibri") +
        geom_text(data = chart_data %>% filter(year == max(year), country == "Slovakia"),
                  mapping = aes(x = year + .1, y = values + .03, label = color_bin),
                  fontface = "bold", hjust = 0, size = 2.2, family = "Calibri") +
        geom_text(data = chart_data %>% filter(year == max(year), country == "Slovenia"),
                  mapping = aes(x = year + .1, y = values, label = color_bin),
                  fontface = "bold", hjust = 0, size = 2.2, family = "Calibri") +
        scale_color_manual(values = chart_data_color_list, guide = FALSE,
                           labels = c("Bulgaria", "Croatia", "Czechia", "Estonia", "Hungary",
                                      "Latvia", "Lithuania", "Montenegro", "Poland",
                                      "Romania", "Slovakia", "Slovenia")) +
        scale_linetype_manual(values = chart_data_linetype_list, guide = FALSE,
                              labels = c("Bulgaria", "Croatia", "Czechia", "Estonia", "Hungary",
                                         "Latvia", "Lithuania", "Montenegro", "Poland",
                                         "Romania", "Slovakia", "Slovenia")) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .25), limits = c(0, 1), expand = c(0, 0),
                           labels = percent_format(accuracy = 1)) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 1)) +
        labs(x = NULL, y = "Share", 
             title = NULL,
             caption = NULL, color = "", linetype = "") +
        coord_fixed(ratio = 4.5 / 1, clip = "off") +
        # geom_segment(data = segment_tbl, mapping = aes(x = x, xend = xend, y = y, yend = yend)) +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 5, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 9, face = "plain", family = "Calibri", 
                                            color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333", 
                                           margin = margin(t = 5, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333", 
                                           margin = margin(t = 0, r = 2, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333", 
                                            margin = margin(t = 0, r = 5, b = 0, l = 2)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 9, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 9, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) 


# inspect
russia_fossil_import_sum_as_share_of_global_fossil_import_sum_graduates_line_chart


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(russia_fossil_import_sum_as_share_of_global_fossil_import_sum_graduates_line_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/energy_security/russia_fossil_import_sum_as_share_of_global_fossil_import_sum_graduates_line_chart.docx")

