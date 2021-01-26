extrafont::loadfonts(device="win") 
library(tidyverse)
library(lubridate)
library(scales)
library(fs)
library(skimr)
library(priceR)
library(readxl)
library(httr)
library(tidyjson)
library(officer)
library(devEMF)
library(testthat)
library(ggrepel)

options(scipen=999)

# load helper functions
setwd("C:/Users/Stephen/Desktop/R/assorted_helper_scripts")
source("add_number_label.R")
source("add_group_index.R")
source("add_dummies.R")


#//////////////////////////////////////////////////////////////////////////////////////////////////////////


# setwd
setwd("C:/Users/Stephen/Desktop/usaid/mcp/tso_portfolio_reviews/media")


#//////////////////////////////////////////////////////////////////////////////////////////////////////////


# load sen_data ####
sen_data <- read_csv("data/sen_data_20200105.csv")
sen_data
sen_data %>% glimpse()
sen_data %>% nrow() # 1351
sen_data %>% ncol() # 14


#//////////////////////////////////////////////////////////////////////////////////////////////////////


# create blue_grey custom palette ####
color_palette <- tibble(hex = c("#08306B", "#08519C", "#4292C6", "#9ECAE1", "#BDBDBD", "#737373", "#484848"))
color_palette
show_col(color_palette %>% pull(hex))

# blue_grey palette supports 7 colors, plus possible extensions via fill/line type
show_col(color_palette %>% slice(1, 4) %>% pull(hex)) # 2 colors
show_col(color_palette %>% slice(1, 2, 4) %>% pull(hex)) # 3 colors
show_col(color_palette %>% slice(1, 2, 3, 4) %>% pull(hex)) # 4 colors
show_col(color_palette %>% slice(1, 2, 4, 6, 7) %>% pull(hex)) # 5 colors
show_col(color_palette %>% slice(1, 2, 4, 5, 6, 7) %>% pull(hex)) # 6 colors
show_col(color_palette %>% slice(1, 2, 3, 4, 5, 6, 7) %>% pull(hex)) # 7 colors


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# create sen_1_1_b_line_chart ####

# note the line chart has several overlapping lines due to low variation, so is not really useful 
# will use grouped bar chart instead

# # check 
# sen_data %>% filter(indicator_number == "1.1b") %>% 
#         arrange(partner) %>%
#         select(indicator_number, indicator_name, 
#                # level_2_indicator_name, level_3_indicator_name, 
#                partner, date_reported, values) %>% print(n = nrow(.))
# 
# # add color_bin and color
# chart_data <- sen_data %>% 
#         filter(indicator_number == "1.1b") %>%
#         mutate(color_bin = partner,
#                color = case_when(color_bin == "Partner 1" ~ color_palette %>% slice(1) %>% pull(hex),
#                                  color_bin == "Partner 2" ~ color_palette %>% slice(2) %>% pull(hex),
#                                  color_bin == "Partner 3" ~ color_palette %>% slice(3) %>% pull(hex),
#                                  color_bin == "Partner 4" ~ color_palette %>% slice(4) %>% pull(hex),
#                                  color_bin == "Partner 5" ~ color_palette %>% slice(5) %>% pull(hex),
#                                  color_bin == "Partner 6" ~ color_palette %>% slice(6) %>% pull(hex),
#                                  color_bin == "Partner 7" ~ color_palette %>% slice(7) %>% pull(hex)),
#                linetype_bin = partner,
#                linetype = case_when(TRUE ~ "solid"))
# 
# # create color_list for to pass to scale_color_manual
# chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
# names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
# chart_data_color_list
# 
# # create linetype_list for to pass to scale_linetype_manual
# chart_data_linetype_list <- chart_data %>% count(linetype_bin, linetype) %>% pull(linetype)
# names(chart_data_linetype_list) <- chart_data %>% count(linetype_bin, linetype) %>% pull(linetype_bin)
# chart_data_linetype_list
# 
# 
# #/////////////////////
# 
# 
# # create chart
# sen_1_1_b_line_chart <- chart_data %>%
#         ggplot(data = ., aes(x = date_reported, 
#                              y = values, 
#                              color = factor(color_bin, levels = c("Partner 1", "Partner 2", "Partner 3",
#                                                                   "Partner 4", "Partner 5", "Partner 6",
#                                                                   "Partner 7")),
#                              linetype = factor(color_bin, levels = c("Partner 1", "Partner 2", "Partner 3",
#                                                                      "Partner 4", "Partner 5", "Partner 6",
#                                                                      "Partner 7")))) + 
#         geom_line(size = 2) + 
#         geom_point(size = 4) +
#         # geom_text(data = chart_data %>% filter(year == max(year), country == "Albania"), 
#         #           mapping = aes(x = year + 0.15, y = net_energy_imports_as_share_of_tes, label = color_bin), 
#         #           fontface = "bold", hjust = 0) + 
#         scale_color_manual(values = chart_data_color_list, guide = FALSE,
#                            labels = c("Partner 1", "Partner 2", "Partner 3",
#                                       "Partner 4", "Partner 5", "Partner 6",
#                                       "Partner 7")) +
#         scale_linetype_manual(values = chart_data_linetype_list, guide = FALSE,
#                               labels = c("Partner 1", "Partner 2", "Partner 3",
#                                          "Partner 4", "Partner 5", "Partner 6",
#                                          "Partner 7")) +
#         # scale_y_continuous(breaks = seq(from = 0, to = .6, by = .1), limits = c(0, .6), expand = c(0, 0),
#         #                    labels = percent_format(accuracy = 1)) +
#         # scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 1)) +
#         labs(x = NULL, y = "Depth of cooperation between participating media outlets", 
#              title = NULL,
#              caption = NULL, color = "", linetype = "") +
#         coord_fixed(ratio = 10 / .1, clip = "off") +
#         theme_bw() +
#         theme(
#                 # plot.background = element_rect(fill = "blue"),
#                 plot.margin = unit(c(0, 20, 0, 0), "mm"),
#                 plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
#                                             color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
#                 # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
#                 panel.grid.minor = element_blank(),
#                 panel.grid.major.x = element_blank(),
#                 panel.grid.major.y = element_line(color = "#DDDDDD"),
#                 # panel.grid.major.y = element_line(color = "#000000"),
#                 panel.border = element_blank(),
#                 # panel.grid = element_blank(),
#                 # line = element_blank(),
#                 # rect = element_blank(),
#                 axis.ticks.y = element_blank(),
#                 # axis.ticks.x = element_blank(),
#                 # axis.ticks.length.y.left = unit(.2, "cm"),
#                 axis.ticks.length.x.bottom = unit(.2, "cm"),
#                 axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
#                                            margin = margin(t = 5, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
#                 axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
#                                            margin = margin(t = 0, r = 5, b = 0, l = 0)),
#                 axis.line.x.bottom = element_line(color = "#333333"),
#                 axis.line.y.left = element_blank(),
#                 axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
#                                             margin = margin(t = 13, r = 0, b = 5, l = 0)),
#                 axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
#                                             margin = margin(t = 0, r = 13, b = 0, l = 0)),
#                 plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
#                                           margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
#                 legend.position = "bottom",
#                 # legend.key.size = unit(2, "mm"), 
#                 legend.title = element_text(size = 12, family = "Calibri", face = "plain", color = "#333333"),
#                 legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
#                                            hjust = .5, color = "#333333")
#                 # legend.spacing.y = unit(5.5, "cm"),
#                 # legend.key = element_rect(size = 5),
#                 # legend.key.size = unit(2, 'lines')
#         ) 
# # guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333", keywidth = 4),
# #        linetype = guide_legend(keywidth = 4))
# 
# 
# # inspect
# sen_1_1_b_line_chart
# 
# 
# #//////////////////////////////////
# 
# 
# # save chart as emf
# filename <- tempfile(fileext = ".emf")
# emf(file = filename)
# print(sen_1_1_b_line_chart)
# dev.off()
# 
# # add emf to word doc  
# read_docx() %>% 
#         body_add_img(src = filename, width = 6, height = 6) %>% 
#         print(target = "output/charts/sen_1_1_b_line_chart.docx")
# 

#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# create sen_1_1_b_grouped_bar_chart ####


# check 
sen_data %>% filter(indicator_number == "1.1b") %>% 
        arrange(partner) %>%
        select(indicator_number, indicator_name, 
               # level_2_indicator_name, level_3_indicator_name, 
               partner, date_reported, values, notes) %>% print(n = nrow(.))

sen_data %>% filter(indicator_number == "1.1b") %>% count(date_reported)

# add color_bin and color
chart_data <- sen_data %>% 
        filter(indicator_number == "1.1b") %>%
        mutate(color_bin = strftime(x = date_reported, format = "%m/%Y"),
               color = case_when(color_bin == "03/2018" ~ color_palette %>% slice(1) %>% pull(hex),
                                 color_bin == "09/2018" ~ color_palette %>% slice(2) %>% pull(hex),
                                 color_bin == "04/2019" ~ color_palette %>% slice(3) %>% pull(hex),
                                 color_bin == "04/2020" ~ color_palette %>% slice(4) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list

# get title
title <- "Depth of partner cooperation with other SEN-partnered media outlets"


#/////////////////////


# create chart
sen_1_1_b_grouped_bar_chart <- chart_data %>%
        ggplot(data = ., aes(x = partner, 
                             y = values, 
                             fill = factor(color_bin, levels = c("03/2018", "09/2018",
                                                                 "04/2019", "04/2020")))) + 
        geom_bar(stat = "identity", position = position_dodge(width = .7), width = .7) +
        scale_fill_manual(values = chart_data_color_list) +
        # scale_x_discrete(expand = c(0, 0)) +
        scale_y_continuous(breaks = seq(from = 1, to = 5, by = 1), 
                           limits = c(0, 5.1), 
                           expand = c(0, 0),
                           labels = c("Cross\npromotion", "Cloning", "Coopetition", "Content\nsharing", "Convergence")) +
        labs(x = NULL, y = NULL, 
             title = NULL,
             caption = NULL, fill = "Date reported") +
        coord_fixed(ratio = 1 / 1.5, clip = "off") +
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
                axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                           margin = margin(t = 5, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", hjust = .5,
                                           margin = margin(t = 0, r = 5, b = 0, l = 0)),
                # axis.text.y = element_blank(),
                axis.line.x.bottom = element_line(color = "#333333"),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 0, r = 13, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) 
# guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333", keywidth = 4),
#        linetype = guide_legend(keywidth = 4))


# inspect
sen_1_1_b_grouped_bar_chart


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(sen_1_1_b_grouped_bar_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/sen_1_1_b_grouped_bar_chart.docx")


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# create sen_2b_total_income_grouped_bar_chart ####


# check
sen_data %>% filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income") %>%
        arrange(partner) %>%
        select(indicator_number, indicator_name,
               level_2_indicator_name,
               # level_3_indicator_name,
               partner, date_reported, values) %>% print(n = nrow(.))

# inspect dates
sen_data %>% filter(indicator_number == "2b") %>% distinct(date_reported)

# inspect level_2_indicator/partner combos
sen_data %>% filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income") %>%
        select(level_2_indicator_name, date_reported, partner) %>%
        count(level_2_indicator_name, date_reported, partner)

# inspect level_2_indicator/partner combos with only 1 measurement period
sen_data %>% filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income") %>%
        select(level_2_indicator_name, date_reported, partner) %>%
        count(level_2_indicator_name, partner)

# get inspect_chart_data with total_income per partner/period
# note that partner 5 and 6 have no total or sub-category revenue data for period 2
# partner 7 has total revenue for period 1 and 2, but sub-category data only for period 1
# note that inspect_chart_data uses the sum of sub-categories as total revenue so that sub-category shares sum to 1
# except for partner 7 in period 2, where i used the total revenue as reported, since sub-cateogry revenue was not reported
inspect_chart_data <- sen_data %>%
        filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income",
               !(partner == "Partner 7" & date_reported == "2020-04-01")) %>%
        select(partner, date_reported, values) %>%
        group_by(partner, date_reported) %>% mutate(total_income = sum(values)) %>% slice(1) %>%
        ungroup() %>% arrange(partner, date_reported) %>% select(-values) %>%
        bind_rows(., sen_data %>%
                          filter(indicator_number == "2b", level_2_indicator_name == "Total non-grant income",
                                 partner == "Partner 7", date_reported == "2020-04-01") %>%
                          select(partner, date_reported, values) %>%
                          rename(total_income = values))

inspect_chart_data %>% print(n = nrow(.))


#////////////////////


# add color_bin and color
chart_data <- inspect_chart_data %>%
        rename(values = total_income) %>%
        mutate(color_bin = strftime(x = date_reported, format = "%m/%Y"),
               color = case_when(color_bin == "04/2019" ~ color_palette %>% slice(1) %>% pull(hex),
                                 color_bin == "04/2020" ~ color_palette %>% slice(4) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list

# get title
title <- "Total non-grant revenue from new business models"


#/////////////////////


# create chart
sen_2b_total_income_grouped_bar_chart <- chart_data %>% 
        ggplot(data = ., aes(y = factor(partner, levels = c("Partner 2", "Partner 3", "Partner 4", "Partner 5",
                                                            "Partner 6", "Partner 7")), 
                             x = values, 
                             fill = factor(color_bin, levels = c("04/2019", "04/2020")))) + 
        geom_bar(stat = "identity", position = position_dodge(width = .7, preserve = "single"), width = .7) +
        scale_fill_manual(values = chart_data_color_list) +
        # scale_x_discrete(expand = c(0, 0)) +
        scale_x_continuous(breaks = seq(from = 0, to = 800000, by = 200000), 
                           limits = c(0, 900000), 
                           expand = c(0, 0),
                           labels = dollar_format(accuracy = 1)) +
        labs(x = NULL, y = NULL, 
             title = NULL,
             caption = NULL, fill = "Date reported") +
        coord_fixed(ratio = 800 / .01, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 0, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                # panel.grid.major.x = element_blank(),
                panel.grid.major.x = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#DDDDDD"),
                panel.grid.major.y = element_blank(),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_blank(),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                # axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                           margin = margin(t = 5, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", hjust = .5,
                                           margin = margin(t = 0, r = 5, b = 0, l = 0)),
                # axis.text.y = element_blank(),
                # axis.line.x.bottom = element_line(color = "#333333"),
                axis.line.x.bottom = element_blank(),
                axis.line.y.left = element_line(color = "#333333"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 0, r = 13, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) 
# guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333", keywidth = 4),
#        linetype = guide_legend(keywidth = 4))


# inspect
sen_2b_total_income_grouped_bar_chart


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(sen_2b_total_income_grouped_bar_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/sen_2b_total_income_grouped_bar_chart.docx")


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# create sen_1_2b_line_chart ####


# check 
sen_data %>% filter(indicator_number == "1.2b") %>% 
        arrange(partner) %>%
        select(indicator_number, indicator_name, 
               # level_2_indicator_name, level_3_indicator_name, 
               partner, date_reported, values) %>% print(n = nrow(.))

# note that early measurements are multi-month, 
# but since it's an average there's no need to manually split up into monthly measurements for comparability 
sen_data %>% filter(indicator_number == "1.2b") %>% distinct(date_reported)

# add color_bin and color
chart_data <- sen_data %>% 
        filter(indicator_number == "1.2b") %>%
        mutate(color_bin = partner,
               color = case_when(color_bin == "Partner 1" ~ color_palette %>% slice(1) %>% pull(hex),
                                 color_bin == "Partner 2" ~ color_palette %>% slice(2) %>% pull(hex),
                                 color_bin == "Partner 3" ~ color_palette %>% slice(3) %>% pull(hex),
                                 color_bin == "Partner 4" ~ color_palette %>% slice(4) %>% pull(hex),
                                 color_bin == "Partner 5" ~ color_palette %>% slice(5) %>% pull(hex),
                                 color_bin == "Partner 6" ~ color_palette %>% slice(6) %>% pull(hex),
                                 color_bin == "Partner 7" ~ color_palette %>% slice(7) %>% pull(hex)),
               linetype_bin = partner,
               linetype = case_when(TRUE ~ "solid"))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list

# create linetype_list for to pass to scale_linetype_manual
chart_data_linetype_list <- chart_data %>% count(linetype_bin, linetype) %>% pull(linetype)
names(chart_data_linetype_list) <- chart_data %>% count(linetype_bin, linetype) %>% pull(linetype_bin)
chart_data_linetype_list

# get title
title <- "Average seconds spent per visit to SEN partner websites each period"


#/////////////////////


# create chart
sen_1_2b_line_chart <- chart_data %>%
        ggplot(data = ., aes(x = date_reported, 
                             y = values, 
                             color = factor(color_bin, levels = c("Partner 1", "Partner 2", "Partner 3",
                                                                  "Partner 4", "Partner 5", "Partner 6",
                                                                  "Partner 7")),
                             linetype = factor(color_bin, levels = c("Partner 1", "Partner 2", "Partner 3",
                                                                     "Partner 4", "Partner 5", "Partner 6",
                                                                     "Partner 7")))) + 
        geom_line(size = 2) + 
        geom_point(size = 4) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 1"),
                  mapping = aes(x = date_reported + 20, y = values, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 2"),
                  mapping = aes(x = date_reported + 20, y = values + 7, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 3"),
                  mapping = aes(x = date_reported + 20, y = values - 5, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 4"),
                  mapping = aes(x = date_reported + 20, y = values + 1, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 5"),
                  mapping = aes(x = date_reported + 20, y = values - 13, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 6"),
                  mapping = aes(x = date_reported + 20, y = values + 5, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 7"),
                  mapping = aes(x = date_reported + 20, y = values + 2, label = color_bin),
                  fontface = "bold", hjust = 0) +
        scale_color_manual(values = chart_data_color_list, 
                           guide = FALSE,
                           labels = c("Partner 1", "Partner 2", "Partner 3",
                                      "Partner 4", "Partner 5", "Partner 6",
                                      "Partner 7")) +
        scale_linetype_manual(values = chart_data_linetype_list, guide = FALSE,
                              labels = c("Partner 1", "Partner 2", "Partner 3",
                                         "Partner 4", "Partner 5", "Partner 6",
                                         "Partner 7")) +
        scale_y_continuous(breaks = seq(from = 0, to = 200, by = 50), limits = c(-5, 200), expand = c(0, 0),
                           labels = waiver()) +
        scale_x_date(breaks = seq(from = ymd("2018-01-01"), to = ymd("2020-12-01"), by = 180),
                     date_labels = "%m/%Y", expand = c(.1, 0)) +
        labs(x = NULL, y = "Seconds spent per visit", 
             title = NULL,
             caption = NULL, color = "partner", linetype = "") +
        coord_fixed(ratio = 2.5 / 1, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 10, 0, 0), "mm"),
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
                axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                           margin = margin(t = 5, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                           margin = margin(t = 0, r = 5, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 0, r = 13, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) 
# guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333", keywidth = 4),
#        linetype = guide_legend(keywidth = 4))


# inspect
sen_1_2b_line_chart


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(sen_1_2b_line_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/sen_1_2b_line_chart.docx")


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# create sen_1_3_2a_line_chart ####


# check 
sen_data %>% filter(indicator_number == "1.3.2a") %>% 
        arrange(partner) %>%
        select(indicator_number, indicator_name, 
               # level_2_indicator_name, level_3_indicator_name,
               partner, date_reported, values) %>% print(n = nrow(.))

# note that early measurement periods were multi-month, later periods were two months
# varying gaps for early measurement periods: 6, 7, 7, 8 months, so could divide values by 3, 3.5, 3.5, and 4, respectively
# but spreadsheet explicitly says bimonthly measurements, and they later adopt monthly measurements
# so it could be interpreted as being measurments for the past two months, they just didn't get on a steady reporting tempo
# looking at the data, this explanation seems most likely, since it avoids a uniform large jump across all partners after
# the downscaled multi-month periods; 
sen_data %>% filter(indicator_number == "1.3.2a") %>% distinct(date_reported)
sen_data %>% filter(indicator_number == "1.3.2a") %>% 
        ggplot(data = ., mapping = aes(x = date_reported, y = values, color = partner)) + geom_line()
sen_data %>% filter(indicator_number == "1.3.2a") %>%
        mutate(values_v2 = case_when(date_reported == "2018-03-01" ~ values / 3,
                                  date_reported == "2018-09-01" ~ values / 3.5,
                                  date_reported == "2019-04-01" ~ values / 3.5,
                                  date_reported == "2019-12-01" ~ values / 4,
                                  TRUE ~ values)) %>%
        ggplot(data = ., mapping = aes(x = date_reported, y = values_v2, color = partner)) + geom_line()
        distinct(date_reported, values, values_v2) %>% arrange(date_reported) %>% print(n = nrow(.))


# clean/prep, and add color_bin and color
chart_data <- sen_data %>% 
        filter(indicator_number == "1.3.2a") %>%
        mutate(color_bin = partner,
               color = case_when(color_bin == "Partner 1" ~ color_palette %>% slice(1) %>% pull(hex),
                                 color_bin == "Partner 2" ~ color_palette %>% slice(2) %>% pull(hex),
                                 color_bin == "Partner 3" ~ color_palette %>% slice(3) %>% pull(hex),
                                 color_bin == "Partner 4" ~ color_palette %>% slice(4) %>% pull(hex),
                                 color_bin == "Partner 5" ~ color_palette %>% slice(5) %>% pull(hex),
                                 color_bin == "Partner 6" ~ color_palette %>% slice(6) %>% pull(hex),
                                 color_bin == "Partner 7" ~ color_palette %>% slice(7) %>% pull(hex)),
               linetype_bin = partner,
               linetype = case_when(TRUE ~ "solid"))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list

# create linetype_list for to pass to scale_linetype_manual
chart_data_linetype_list <- chart_data %>% count(linetype_bin, linetype) %>% pull(linetype)
names(chart_data_linetype_list) <- chart_data %>% count(linetype_bin, linetype) %>% pull(linetype_bin)
chart_data_linetype_list

# get title
title <- "Number of SEN-exchanged stories taken\n by partner for exchange each period"


#/////////////////////


# create chart
sen_1_3_2a_line_chart <- chart_data %>%
        ggplot(data = ., aes(x = date_reported, 
                             y = values, 
                             color = factor(color_bin, levels = c("Partner 1", "Partner 2", "Partner 3",
                                                                  "Partner 4", "Partner 5", "Partner 6",
                                                                  "Partner 7")),
                             linetype = factor(color_bin, levels = c("Partner 1", "Partner 2", "Partner 3",
                                                                     "Partner 4", "Partner 5", "Partner 6",
                                                                     "Partner 7")))) + 
        geom_line(size = 2) + 
        geom_point(size = 4) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 1"),
                  mapping = aes(x = date_reported + 20, y = values + 4, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 2"),
                  mapping = aes(x = date_reported + 20, y = values, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 3"),
                  mapping = aes(x = date_reported + 20, y = values, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 4"),
                  mapping = aes(x = date_reported + 20, y = values - 4, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 5"),
                  mapping = aes(x = date_reported + 20, y = values, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 6"),
                  mapping = aes(x = date_reported + 20, y = values, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 7"),
                  mapping = aes(x = date_reported + 20, y = values - 3, label = color_bin),
                  fontface = "bold", hjust = 0) +
        scale_color_manual(values = chart_data_color_list, 
                           guide = FALSE,
                           labels = c("Partner 1", "Partner 2", "Partner 3",
                                      "Partner 4", "Partner 5", "Partner 6",
                                      "Partner 7")) +
        scale_linetype_manual(values = chart_data_linetype_list, guide = FALSE,
                              labels = c("Partner 1", "Partner 2", "Partner 3",
                                         "Partner 4", "Partner 5", "Partner 6",
                                         "Partner 7")) +
        scale_y_continuous(breaks = seq(from = 0, to = 60, by = 10), limits = c(-5, 60), expand = c(0, 0),
                           labels = waiver()) +
        scale_x_date(breaks = seq(from = ymd("2018-01-01"), to = ymd("2020-12-01"), by = 180),
                     date_labels = "%m/%Y", expand = c(.1, 0)) +
        labs(x = NULL, y = "Number of SEN-exchanged stories", 
             title = NULL,
             caption = NULL, color = "partner", linetype = "") +
        coord_fixed(ratio = 8 / 1, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 10, 0, 0), "mm"),
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
                axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                           margin = margin(t = 5, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                           margin = margin(t = 0, r = 5, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 0, r = 13, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) 
# guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333", keywidth = 4),
#        linetype = guide_legend(keywidth = 4))


# inspect
sen_1_3_2a_line_chart


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(sen_1_3_2a_line_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/sen_1_3_2a_line_chart.docx")


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////



# create sen_1_3_2b_line_chart ####


# check 
sen_data %>% filter(indicator_number == "1.3.2b") %>% 
        arrange(partner) %>%
        select(indicator_number, indicator_name, 
               level_2_indicator_name, level_3_indicator_name,
               partner, date_reported, values) %>% print(n = nrow(.))

# note that early measurement periods were multi-month, later periods were monthly
# but since it's a share, we can treat is an average that doesn't need to be down-scaled for comparibility
sen_data %>% filter(indicator_number == "1.3.2b") %>% distinct(date_reported)

# clean/prep, and add color_bin and color
chart_data <- sen_data %>% 
        filter(indicator_number == "1.3.2b") %>%
        mutate(color_bin = partner,
               color = case_when(color_bin == "Partner 1" ~ color_palette %>% slice(1) %>% pull(hex),
                                 color_bin == "Partner 2" ~ color_palette %>% slice(2) %>% pull(hex),
                                 color_bin == "Partner 3" ~ color_palette %>% slice(3) %>% pull(hex),
                                 color_bin == "Partner 4" ~ color_palette %>% slice(4) %>% pull(hex),
                                 color_bin == "Partner 5" ~ color_palette %>% slice(5) %>% pull(hex),
                                 color_bin == "Partner 6" ~ color_palette %>% slice(6) %>% pull(hex),
                                 color_bin == "Partner 7" ~ color_palette %>% slice(7) %>% pull(hex)),
               linetype_bin = partner,
               linetype = case_when(TRUE ~ "solid"))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list

# create linetype_list for to pass to scale_linetype_manual
chart_data_linetype_list <- chart_data %>% count(linetype_bin, linetype) %>% pull(linetype)
names(chart_data_linetype_list) <- chart_data %>% count(linetype_bin, linetype) %>% pull(linetype_bin)
chart_data_linetype_list

# get title
title <- "Finished-reading rate of stories on SEN partner websites"


#/////////////////////


# create chart
sen_1_3_2b_line_chart <- chart_data %>%
        ggplot(data = ., aes(x = date_reported, 
                             y = values, 
                             color = factor(color_bin, levels = c("Partner 1", "Partner 2", "Partner 3",
                                                                  "Partner 4", "Partner 5", "Partner 6",
                                                                  "Partner 7")),
                             linetype = factor(color_bin, levels = c("Partner 1", "Partner 2", "Partner 3",
                                                                     "Partner 4", "Partner 5", "Partner 6",
                                                                     "Partner 7")))) + 
        geom_line(size = 2) + 
        geom_point(size = 4) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 1"),
                  mapping = aes(x = date_reported + 20, y = values + .02, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 2"),
                  mapping = aes(x = date_reported + 20, y = values, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 3"),
                  mapping = aes(x = date_reported + 20, y = values + .02, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 4"),
                  mapping = aes(x = date_reported + 20, y = values - .01, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 5"),
                  mapping = aes(x = date_reported + 20, y = values, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 6"),
                  mapping = aes(x = date_reported + 20, y = values - .02, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 7"),
                  mapping = aes(x = date_reported + 20, y = values - .01, label = color_bin),
                  fontface = "bold", hjust = 0) +
        scale_color_manual(values = chart_data_color_list, 
                           guide = FALSE,
                           labels = c("Partner 1", "Partner 2", "Partner 3",
                                      "Partner 4", "Partner 5", "Partner 6",
                                      "Partner 7")) +
        scale_linetype_manual(values = chart_data_linetype_list, guide = FALSE,
                              labels = c("Partner 1", "Partner 2", "Partner 3",
                                         "Partner 4", "Partner 5", "Partner 6",
                                         "Partner 7")) +
        scale_y_continuous(breaks = seq(from = 0, to = .8, by = .2), limits = c(-.05, .85), expand = c(0, 0),
                           labels = percent_format(accuracy = 1)) +
        scale_x_date(breaks = seq(from = ymd("2018-01-01"), to = ymd("2020-12-01"), by = 180),
                     date_labels = "%m/%Y", expand = c(.1, 0)) +
        labs(x = NULL, y = "Finished-reading rate", 
             title = NULL,
             caption = NULL, color = "partner", linetype = "") +
        coord_fixed(ratio = 600 / 1, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 10, 0, 0), "mm"),
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
                axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                           margin = margin(t = 5, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                           margin = margin(t = 0, r = 5, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 0, r = 13, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) 
# guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333", keywidth = 4),
#        linetype = guide_legend(keywidth = 4))


# inspect
sen_1_3_2b_line_chart


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(sen_1_3_2b_line_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/sen_1_3_2b_line_chart.docx")


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////



# create sen_1_3b_google_line_chart ####


# check 
sen_data %>% filter(indicator_number == "1.3b", level_2_indicator_name == "Google Analytics") %>% 
        arrange(partner) %>%
        select(indicator_number, indicator_name, 
               level_2_indicator_name, level_3_indicator_name,
               partner, date_reported, values) %>% print(n = nrow(.))

sen_data %>% filter(indicator_number == "1.3b", 
                    level_2_indicator_name == "Google Analytics") %>% count(level_2_indicator_name, partner)

# note that early measurement periods were multi-month, later periods were monthly
# varying gaps for early measurement periods: 6, 7, 7, 8 months, so could divide values by 6, 7, 7, 8 respectively
# but that sen spreadsheet explicitly says this metric is collected bimonthly
# though that appears to have changed to monthly for more recent measurements
# so i could downscale the early measurements by dividing by 6, 7, 7, 8; or could divide them by 2
# or could leave it alone and not downscale, which actually results in the smoothest time series 
# maybe the data collectors already downscaled it? that would make sense and explain the clear pattern
# it's very suspicious that if downscaling were correct it would result it uniform sharp jumps in the post-downscale period
# result: will assume the early measurement periods are already downscaled, this looks most accurate, and will flag for nick
sen_data %>% filter(indicator_number == "1.3b", level_2_indicator_name == "Google Analytics") %>% distinct(date_reported)
sen_data %>% filter(indicator_number == "1.3b", level_2_indicator_name == "Google Analytics") %>%
        ggplot(data = ., mapping = aes(x = date_reported, y = values, color = partner)) + geom_line()
sen_data %>% filter(indicator_number == "1.3b", level_2_indicator_name == "Google Analytics") %>%
        mutate(values_v2 = case_when(date_reported == "2018-03-01" ~ values / 6,
                                     date_reported == "2018-09-01" ~ values / 7,
                                     date_reported == "2019-04-01" ~ values / 7,
                                     date_reported == "2019-12-01" ~ values / 8,
                                     TRUE ~ values)) %>%
        ggplot(data = ., mapping = aes(x = date_reported, y = values_v2, color = partner)) + geom_line()
        distinct(partner, date_reported, values, values_v2) %>% arrange(date_reported) %>% print(n = nrow(.))
sen_data %>% filter(indicator_number == "1.3b", level_2_indicator_name == "Google Analytics") %>%
        mutate(values_v2 = case_when(date_reported == "2018-03-01" ~ values / 2,
                                     date_reported == "2018-09-01" ~ values / 2,
                                     date_reported == "2019-04-01" ~ values / 2,
                                     date_reported == "2019-12-01" ~ values / 2,
                                     TRUE ~ values)) %>%
        ggplot(data = ., mapping = aes(x = date_reported, y = values_v2, color = partner)) + geom_line()
        distinct(partner, date_reported, values, values_v2) %>% arrange(date_reported) %>% print(n = nrow(.))

# clean/prep, and add color_bin and color
chart_data <- sen_data %>% 
        filter(indicator_number == "1.3b", level_2_indicator_name == "Google Analytics") %>%
        mutate(values = values / 1000000) %>%
        mutate(color_bin = partner,
               color = case_when(color_bin == "Partner 1" ~ color_palette %>% slice(1) %>% pull(hex),
                                 color_bin == "Partner 2" ~ color_palette %>% slice(2) %>% pull(hex),
                                 color_bin == "Partner 3" ~ color_palette %>% slice(3) %>% pull(hex),
                                 color_bin == "Partner 4" ~ color_palette %>% slice(4) %>% pull(hex),
                                 color_bin == "Partner 5" ~ color_palette %>% slice(5) %>% pull(hex),
                                 color_bin == "Partner 6" ~ color_palette %>% slice(6) %>% pull(hex),
                                 color_bin == "Partner 7" ~ color_palette %>% slice(7) %>% pull(hex)),
               linetype_bin = partner,
               linetype = case_when(TRUE ~ "solid"))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list

# create linetype_list for to pass to scale_linetype_manual
chart_data_linetype_list <- chart_data %>% count(linetype_bin, linetype) %>% pull(linetype)
names(chart_data_linetype_list) <- chart_data %>% count(linetype_bin, linetype) %>% pull(linetype_bin)
chart_data_linetype_list

# get title
title <- "Number of unique visitors to SEN partner websites (Google Analytics)"


#/////////////////////


# create chart
sen_1_3b_google_line_chart <- chart_data %>%
        ggplot(data = ., aes(x = date_reported, 
                             y = values, 
                             color = factor(color_bin, levels = c("Partner 1", "Partner 2", "Partner 3",
                                                                  "Partner 4", "Partner 5", "Partner 6",
                                                                  "Partner 7")),
                             linetype = factor(color_bin, levels = c("Partner 1", "Partner 2", "Partner 3",
                                                                     "Partner 4", "Partner 5", "Partner 6",
                                                                     "Partner 7")))) + 
        geom_line(size = 2) + 
        geom_point(size = 4) +
        # geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 1"),
        #           mapping = aes(x = date_reported + 20, y = values + .02, label = color_bin),
        #           fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 2"),
                  mapping = aes(x = date_reported + 20, y = values + .3, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 3"),
                  mapping = aes(x = date_reported + 20, y = values + .3, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 4"),
                  mapping = aes(x = date_reported + 20, y = values + .2, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 5"),
                  mapping = aes(x = date_reported + 20, y = values + .6, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 6"),
                  mapping = aes(x = date_reported + 20, y = values, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 7"),
                  mapping = aes(x = date_reported + 20, y = values + .5, label = color_bin),
                  fontface = "bold", hjust = 0) +
        scale_color_manual(values = chart_data_color_list, 
                           guide = FALSE,
                           labels = c("Partner 1", "Partner 2", "Partner 3",
                                      "Partner 4", "Partner 5", "Partner 6",
                                      "Partner 7")) +
        scale_linetype_manual(values = chart_data_linetype_list, guide = FALSE,
                              labels = c("Partner 1", "Partner 2", "Partner 3",
                                         "Partner 4", "Partner 5", "Partner 6",
                                         "Partner 7")) +
        scale_y_continuous(breaks = seq(from = 0, to = 14, by = 2), limits = c(0, 14), expand = c(0, 0),
                           labels = add_number_label(breaks = seq(from = 0, to = 14, by = 2), label = "m")) +
        scale_x_date(breaks = seq(from = ymd("2018-01-01"), to = ymd("2020-12-01"), by = 180),
                     date_labels = "%m/%Y", expand = c(.1, 0)) +
        labs(x = NULL, y = "Number of unique website visitors\n(millions)", 
             title = NULL,
             caption = NULL, color = "partner", linetype = "") +
        coord_fixed(ratio = 40 / 1, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 10, 0, 0), "mm"),
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
                axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                           margin = margin(t = 5, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                           margin = margin(t = 0, r = 5, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 0, r = 13, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) 
# guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333", keywidth = 4),
#        linetype = guide_legend(keywidth = 4))


# inspect
sen_1_3b_google_line_chart


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(sen_1_3b_google_line_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/sen_1_3b_google_line_chart.docx")



#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////



# create sen_1_3b_io_line_chart ####


# check 
sen_data %>% filter(indicator_number == "1.3b", level_2_indicator_name == "IO Analytics") %>% 
        arrange(partner) %>%
        select(indicator_number, indicator_name, 
               level_2_indicator_name, level_3_indicator_name,
               partner, date_reported, values) %>% print(n = nrow(.))

sen_data %>% filter(indicator_number == "1.3b", 
                    level_2_indicator_name == "IO Analytics") %>% count(level_2_indicator_name, partner)
sen_data %>% filter(indicator_number == "1.3b", 
                    level_2_indicator_name == "IO Analytics", partner %in% c("Partner 5","Partner 7")) %>%
        select(indicator_name, partner, date_reported, values) %>% arrange(partner, date_reported)

# note that early measurement periods were multi-month, later periods were monthly
# varying gaps for early measurement periods: 6, 7, 7, 8 months, so could divide values by 6, 7, 7, 8 respectively
# but that sen spreadsheet explicitly says this metric is collected bimonthly
# though that appears to have changed to monthly for more recent measurements
# so i could downscale the early measurements by dividing by 6, 7, 7, 8; or could divide them by 2
# or could leave it alone and not downscale, which actually results in the smoothest time series 
# maybe the data collectors already downscaled it? that would make sense and explain the clear pattern
# it's very suspicious that if downscaling were correct it would result it uniform sharp jumps in the post-downscale period
# result: will assume the early measurement periods are already downscaled, this looks most accurate, and will flag for nick
sen_data %>% filter(indicator_number == "1.3b", level_2_indicator_name == "IO Analytics") %>% distinct(date_reported)
sen_data %>% filter(indicator_number == "1.3b", level_2_indicator_name == "IO Analytics") %>%
        mutate(values_v2 = case_when(date_reported == "2018-03-01" ~ values / 6,
                                     date_reported == "2018-09-01" ~ values / 7,
                                     date_reported == "2019-04-01" ~ values / 7,
                                     date_reported == "2019-12-01" ~ values / 8,
                                     TRUE ~ values)) %>%
        distinct(partner, date_reported, values, values_v2) %>% arrange(date_reported) %>% print(n = nrow(.))
sen_data %>% filter(indicator_number == "1.3b", level_2_indicator_name == "IO Analytics") %>%
        mutate(values_v2 = case_when(date_reported == "2018-03-01" ~ values / 2,
                                     date_reported == "2018-09-01" ~ values / 2,
                                     date_reported == "2019-04-01" ~ values / 2,
                                     date_reported == "2019-12-01" ~ values / 2,
                                     TRUE ~ values)) %>%
        distinct(partner, date_reported, values, values_v2) %>% arrange(date_reported) %>% print(n = nrow(.))

# clean/prep, and add color_bin and color
chart_data <- sen_data %>% 
        filter(indicator_number == "1.3b", level_2_indicator_name == "IO Analytics") %>%
        mutate(values = values / 1000000) %>%
        mutate(color_bin = partner,
               color = case_when(color_bin == "Partner 1" ~ color_palette %>% slice(1) %>% pull(hex),
                                 color_bin == "Partner 2" ~ color_palette %>% slice(2) %>% pull(hex),
                                 color_bin == "Partner 3" ~ color_palette %>% slice(3) %>% pull(hex),
                                 color_bin == "Partner 4" ~ color_palette %>% slice(4) %>% pull(hex),
                                 color_bin == "Partner 5" ~ color_palette %>% slice(5) %>% pull(hex),
                                 color_bin == "Partner 6" ~ color_palette %>% slice(6) %>% pull(hex),
                                 color_bin == "Partner 7" ~ color_palette %>% slice(7) %>% pull(hex)),
               linetype_bin = partner,
               linetype = case_when(TRUE ~ "solid"))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list

# create linetype_list for to pass to scale_linetype_manual
chart_data_linetype_list <- chart_data %>% count(linetype_bin, linetype) %>% pull(linetype)
names(chart_data_linetype_list) <- chart_data %>% count(linetype_bin, linetype) %>% pull(linetype_bin)
chart_data_linetype_list

# get title
title <- "Number of unique visitors to SEN partner websites (IO Analytics)"


#/////////////////////


# create chart
sen_1_3b_io_line_chart <- chart_data %>%
        ggplot(data = ., aes(x = date_reported, 
                             y = values, 
                             color = factor(color_bin, levels = c("Partner 1", "Partner 2", "Partner 3",
                                                                  "Partner 4", "Partner 5", "Partner 6",
                                                                  "Partner 7")),
                             linetype = factor(color_bin, levels = c("Partner 1", "Partner 2", "Partner 3",
                                                                     "Partner 4", "Partner 5", "Partner 6",
                                                                     "Partner 7")))) + 
        geom_line(size = 2) + 
        geom_point(size = 4) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 1"),
                  mapping = aes(x = date_reported + 20, y = values + 1.5, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 2"),
                  mapping = aes(x = date_reported + 20, y = values + 2.5, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 3"),
                  mapping = aes(x = date_reported + 20, y = values + 3, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 4"),
                  mapping = aes(x = date_reported + 20, y = values + .75, label = color_bin),
                  fontface = "bold", hjust = 0) +
        # geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 5"),
        #           mapping = aes(x = date_reported + 20, y = values + .6, label = color_bin),
        #           fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 6"),
                  mapping = aes(x = date_reported + 20, y = values, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(partner == "Partner 7", date_reported == "2019-04-01"),
                  mapping = aes(x = date_reported + 70, y = values + 6, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_segment(data = chart_data %>% filter(partner == "Partner 7", date_reported == "2019-04-01"), 
                     inherit.aes = FALSE, size = 1,
                     mapping = aes(x = date_reported, y = values, xend = date_reported + 60, yend = values + 5)) +
        scale_color_manual(values = chart_data_color_list, 
                           guide = FALSE,
                           labels = c("Partner 1", "Partner 2", "Partner 3",
                                      "Partner 4", "Partner 5", "Partner 6",
                                      "Partner 7")) +
        scale_linetype_manual(values = chart_data_linetype_list, guide = FALSE,
                              labels = c("Partner 1", "Partner 2", "Partner 3",
                                         "Partner 4", "Partner 5", "Partner 6",
                                         "Partner 7")) +
        scale_y_continuous(breaks = seq(from = 0, to = 35, by = 5), limits = c(0, 35), expand = c(0, 0),
                           labels = add_number_label(breaks = seq(from = 0, to = 35, by = 5), label = "m")) +
        scale_x_date(breaks = seq(from = ymd("2018-01-01"), to = ymd("2020-12-01"), by = 180),
                     date_labels = "%m/%Y", expand = c(.1, 0)) +
        labs(x = NULL, y = "Number of unique website visitors\n(millions)", 
             title = NULL,
             caption = NULL, color = "partner", linetype = "") +
        coord_fixed(ratio = 15 / 1, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 10, 0, 0), "mm"),
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
                axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                           margin = margin(t = 5, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                           margin = margin(t = 0, r = 5, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 0, r = 13, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) 
# guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333", keywidth = 4),
#        linetype = guide_legend(keywidth = 4))


# inspect
sen_1_3b_io_line_chart


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(sen_1_3b_io_line_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/sen_1_3b_io_line_chart.docx")


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# # create sen_2b_partner_count_per_business_model_line_chart
# 
# 
# # note this chart is not used since partners 5, 6, and 7 don't have period 2 business models
# # so a large portion of the decrease in business model use over time is due to 
# # these 3 partners using zero business models in period 2
# 
# # check 
# sen_data %>% filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income") %>% 
#         arrange(partner) %>%
#         select(indicator_number, indicator_name, 
#                level_2_indicator_name, 
#                # level_3_indicator_name,
#                partner, date_reported, values) %>% print(n = nrow(.))
# 
# sen_data %>% filter(indicator_number == "2b") %>% distinct(date_reported)
# sen_data %>% filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income") %>% 
#         count(level_2_indicator_name, date_reported, partner)
# sen_data %>% filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income") %>% 
#         group_by(level_2_indicator_name, date_reported) %>% 
#         mutate(partner_n_distinct = n_distinct(partner)) %>% 
#         ungroup() %>% select(level_2_indicator_name, date_reported, partner, partner_n_distinct) %>%
#         arrange(level_2_indicator_name, date_reported)
# sen_data %>% filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income") %>% 
#         group_by(level_2_indicator_name, date_reported) %>% 
#         mutate(partner_n_distinct = n_distinct(partner)) %>% 
#         ungroup() %>% distinct(level_2_indicator_name, date_reported, partner_n_distinct) %>%
#         arrange(level_2_indicator_name, date_reported)
# 
# # inspect chart_data
# # note have to add an observation for Other category in period 2 showing zero partners using that business model
# sen_data %>% 
#         filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income") %>% 
#         group_by(level_2_indicator_name, date_reported) %>% 
#         mutate(partner_n_distinct = n_distinct(partner)) %>% 
#         ungroup() %>% distinct(level_2_indicator_name, date_reported, partner_n_distinct) %>%
#         rename(values = partner_n_distinct) %>%
#         bind_rows(tibble(level_2_indicator_name = "Other", date_reported = ymd("2020-04-01"), values = 0))
#         
# 
# # clean/prep, and add color_bin and color
# chart_data <- sen_data %>% 
#         filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income") %>% 
#         group_by(level_2_indicator_name, date_reported) %>% 
#         mutate(partner_n_distinct = n_distinct(partner),
#                level_2_indicator_name = case_when(level_2_indicator_name == "Subscriptions/crowdfunding" ~
#                                                           "Subscriptions/\ncrowdfunding",
#                                                   level_2_indicator_name == "Commercial partnerships" ~
#                                                           "Commercial\npartnerships",
#                                                   TRUE ~ level_2_indicator_name)) %>% 
#         ungroup() %>% distinct(level_2_indicator_name, date_reported, partner_n_distinct) %>%
#         rename(values = partner_n_distinct) %>%
#         bind_rows(tibble(level_2_indicator_name = "Other", date_reported = ymd("2020-04-01"), values = 0)) %>%
#         mutate(color_bin = level_2_indicator_name,
#                color = case_when(color_bin == "Advertising" ~ color_palette %>% slice(1) %>% pull(hex),
#                                  color_bin == "Commercial\npartnerships" ~ color_palette %>% slice(2) %>% pull(hex),
#                                  color_bin == "Other" ~ color_palette %>% slice(3) %>% pull(hex),
#                                  color_bin == "Subscriptions/\ncrowdfunding" ~ color_palette %>% slice(4) %>% pull(hex)),
#                linetype_bin = level_2_indicator_name,
#                linetype = case_when(TRUE ~ "solid"))
# 
# # create color_list for to pass to scale_color_manual
# chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
# names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
# chart_data_color_list
# 
# # create linetype_list for to pass to scale_linetype_manual
# chart_data_linetype_list <- chart_data %>% count(linetype_bin, linetype) %>% pull(linetype)
# names(chart_data_linetype_list) <- chart_data %>% count(linetype_bin, linetype) %>% pull(linetype_bin)
# chart_data_linetype_list
# 
# # get title
# title <- "Business models used by SEN partners"
# 
# 
# #/////////////////////
# 
# 
# # create chart
# sen_2b_partner_count_per_business_model_line_chart <- chart_data %>%
#         ggplot(data = ., aes(x = date_reported, 
#                              y = values, 
#                              color = factor(color_bin, levels = c("Advertising", "Commercial\npartnerships",
#                                                                   "Other", "Subscriptions/\ncrowdfunding")),
#                              linetype = factor(color_bin, levels = c("Advertising", "Commercial\npartnerships",
#                                                                      "Other", "Subscriptions/\ncrowdfunding")))) + 
#         geom_line(size = 2) + 
#         geom_point(size = 4) +
#         geom_text(data = chart_data %>% filter(date_reported == max(date_reported)),
#                   mapping = aes(x = date_reported + 10, y = values + .1, label = color_bin),
#                   fontface = "bold", hjust = 0) +
#         scale_color_manual(values = chart_data_color_list, 
#                            guide = FALSE,
#                            labels = c("Advertising", "Commercial\npartnerships",
#                                       "Other", "Subscriptions/\ncrowdfunding")) +
#         scale_linetype_manual(values = chart_data_linetype_list, guide = FALSE,
#                               labels = c("Advertising", "Commercial\npartnerships",
#                                          "Other", "Subscriptions/\ncrowdfunding")) +
#         scale_y_continuous(breaks = seq(from = 0, to = 5, by = 1), limits = c(0, 5.1), expand = c(0, 0),
#                            labels = waiver()) +
#         scale_x_date(breaks = seq(from = ymd("2019-04-01"), to = ymd("2020-04-01"), by = 365),
#                      date_labels = "%m/%Y", expand = c(0, 0)) +
#         labs(x = NULL, y = "Number of SEN partners\nusing business model", 
#              title = NULL,
#              caption = NULL, color = "Business model", linetype = "") +
#         coord_fixed(ratio = 40 / 1, clip = "off") +
#         theme_bw() +
#         theme(
#                 # plot.background = element_rect(fill = "blue"),
#                 plot.margin = unit(c(0, 30, 0, 0), "mm"),
#                 plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
#                                             color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
#                 # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
#                 panel.grid.minor = element_blank(),
#                 panel.grid.major.x = element_blank(),
#                 panel.grid.major.y = element_line(color = "#DDDDDD"),
#                 # panel.grid.major.y = element_line(color = "#000000"),
#                 panel.border = element_blank(),
#                 # panel.grid = element_blank(),
#                 # line = element_blank(),
#                 # rect = element_blank(),
#                 axis.ticks.y = element_blank(),
#                 # axis.ticks.x = element_blank(),
#                 # axis.ticks.length.y.left = unit(.2, "cm"),
#                 axis.ticks.length.x.bottom = unit(.2, "cm"),
#                 axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
#                                            margin = margin(t = 5, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
#                 axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
#                                            margin = margin(t = 0, r = 5, b = 0, l = 0)),
#                 axis.line.x.bottom = element_line(color = "#333333"),
#                 axis.line.y.left = element_blank(),
#                 axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
#                                             margin = margin(t = 13, r = 0, b = 5, l = 0)),
#                 axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
#                                             margin = margin(t = 0, r = 13, b = 0, l = 0)),
#                 plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
#                                           margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
#                 legend.position = "bottom",
#                 # legend.key.size = unit(2, "mm"), 
#                 legend.title = element_text(size = 12, family = "Calibri", face = "plain", color = "#333333"),
#                 legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
#                                            hjust = .5, color = "#333333")
#                 # legend.spacing.y = unit(5.5, "cm"),
#                 # legend.key = element_rect(size = 5),
#                 # legend.key.size = unit(2, 'lines')
#         ) 
# # guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333", keywidth = 4),
# #        linetype = guide_legend(keywidth = 4))
# 
# 
# # inspect
# sen_2b_partner_count_per_business_model_line_chart
# 
# 
# #//////////////////////////////////
# 
# 
# # save chart as emf
# filename <- tempfile(fileext = ".emf")
# emf(file = filename)
# print(sen_2b_partner_count_per_business_model_line_chart)
# dev.off()
# 
# # add emf to word doc  
# read_docx() %>% 
#         body_add_img(src = filename, width = 6, height = 6) %>% 
#         print(target = "output/charts/sen_2b_partner_count_per_business_model_line_chart.docx")


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


## create sen_income_share_per_business_model_line_chart ####
#
# # note that this chart won't be used, since it has too many lines to be clear, and the combination of partner labels on 
# # y axis, with line color mapped to business model, also takes extra effort to distinguish
# 
# 
# # check 
# sen_data %>% filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income") %>% 
#         arrange(partner) %>%
#         select(indicator_number, indicator_name, 
#                level_2_indicator_name, 
#                # level_3_indicator_name,
#                partner, date_reported, values) %>% print(n = nrow(.))
# 
# # inspect dates 
# sen_data %>% filter(indicator_number == "2b") %>% distinct(date_reported)
# 
# # inspect level_2_indicator/partner combos 
# sen_data %>% filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income") %>% 
#         select(level_2_indicator_name, date_reported, partner) %>%
#         count(level_2_indicator_name, date_reported, partner)
# 
# # inspect level_2_indicator/partner combos with only 1 measurement period
# sen_data %>% filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income") %>% 
#         select(level_2_indicator_name, date_reported, partner) %>%
#         count(level_2_indicator_name, partner)
# 
# # get inspect_chart_data with income_share per business model by partner/date combo
# # note that partner 5 and 6 have no total or sub-category revenue data for period 2
# # partner 7 has total revenue for period 1 and 2, but sub-category data only for period 1
# # note that in clean_sen_data.R when inspecting metric_2b, it shows that the sum of sub-categories doesn't always equal the
# # total income for some partners; this is due to currency conversions at slightly different times/rates
# # but the largest difference is only 6% of total revenue
# # note that inspect_chart_data uses the sum of sub-categories as total revenue so that sub-category shares sum to 1
# # except for partner 7 in period 2, where i used the total revenue as reported, since sub-cateogry revenue was not reported
# inspect_chart_data <- sen_data %>% 
#         filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income", 
#                !(partner == "Partner 7" & date_reported == "2020-04-01")) %>%
#         select(partner, date_reported, values) %>%
#         group_by(partner, date_reported) %>% mutate(total_income = sum(values)) %>% slice(1) %>%
#         ungroup() %>% arrange(partner, date_reported) %>% select(-values) %>%
#         bind_rows(., sen_data %>% 
#                           filter(indicator_number == "2b", level_2_indicator_name == "Total non-grant income", 
#                                  partner == "Partner 7", date_reported == "2020-04-01") %>%
#                           select(partner, date_reported, values) %>%
#                           rename(total_income = values)) %>%
#         left_join(sen_data %>% 
#                           filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income") %>%
#                           select(level_2_indicator_name, partner, date_reported, values), .,
#                   by = c("partner", "date_reported")) %>%
#         arrange(partner, date_reported, level_2_indicator_name) %>%
#         mutate(income_share = values / total_income) 
# 
# # inspect
# inspect_chart_data
# inspect_chart_data %>% group_by(partner, date_reported) %>% summarize(sum_income_share = sum(income_share))
# expect_equal(object = inspect_chart_data %>% group_by(partner, date_reported) %>% 
#                      summarize(sum_income_share = round(sum(income_share), digits = 2)) %>% 
#                      ungroup() %>% distinct(sum_income_share) %>% pull(sum_income_share),
#              expected = 1)
# 
# 
# # update inspect_chart_data with income_share of zero input for missing level_2_indicator/partner/date combos
# # drop period 2 for partner 7, since it has no sub-category data
# inspect_chart_data <- sen_data %>% 
#         filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income") %>%
#         distinct(level_2_indicator_name, partner, date_reported) %>% 
#         expand(nesting(level_2_indicator_name, partner), date_reported) %>%
#         left_join(., inspect_chart_data, by = c("level_2_indicator_name", "partner", "date_reported")) %>% 
#         mutate(values = case_when(is.na(values) ~ 0, TRUE ~ values),
#                income_share = case_when(values == 0 ~ 0, TRUE ~ income_share)) %>%
#         arrange(partner, date_reported, level_2_indicator_name) %>%
#         # drop partner 7 for period 2, because it only has total income, no info on category breakdown; caveat in footnote
#         filter(!(partner == "Partner 7" & date_reported == "2020-04-01")) %>%
#         add_group_index(group_vars = vars(level_2_indicator_name, partner), 
#                         group_name = "partner_business_model_group_index") %>%
#         arrange(partner_business_model_group_index) 
# 
# inspect_chart_data %>% glimpse()
# inspect_chart_data %>% print(n = nrow(.))
# inspect_chart_data %>% ggplot(data = ., mapping = aes(x = level_2_indicator_name, y = income_share)) + geom_boxplot()
# 
#
# #//////////////////////
# 
# 
# # clean/prep, and add color_bin and color
# chart_data <- inspect_chart_data %>%
#         mutate(values = income_share) %>%
#         mutate(color_bin = level_2_indicator_name,
#                color = case_when(color_bin == "Advertising" ~ color_palette %>% slice(1) %>% pull(hex),
#                                  color_bin == "Commercial partnerships" ~ color_palette %>% slice(2) %>% pull(hex),
#                                  color_bin == "Other" ~ color_palette %>% slice(3) %>% pull(hex),
#                                  color_bin == "Subscriptions/crowdfunding" ~ color_palette %>% slice(4) %>% pull(hex)),
#                linetype_bin = level_2_indicator_name,
#                linetype = case_when(TRUE ~ "solid"))
# 
# # create color_list for to pass to scale_color_manual
# chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
# names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
# chart_data_color_list
# 
# # create linetype_list for to pass to scale_linetype_manual
# chart_data_linetype_list <- chart_data %>% count(linetype_bin, linetype) %>% pull(linetype)
# names(chart_data_linetype_list) <- chart_data %>% count(linetype_bin, linetype) %>% pull(linetype_bin)
# chart_data_linetype_list
# 
# # get title
# title <- "SEN partners' share of income by business model"
# 
# 
# #/////////////////////
# 
# 
# # create chart
# sen_income_share_per_business_model_line_chart <- chart_data %>%
#         ggplot(data = ., aes(x = date_reported, 
#                              y = values, group = partner_business_model_group_index,
#                              color = factor(color_bin, levels = c("Advertising", "Commercial partnerships",
#                                                                   "Other", "Subscriptions/crowdfunding")),
#                              linetype = factor(color_bin, levels = c("Advertising", "Commercial partnerships",
#                                                                      "Other", "Subscriptions/crowdfunding")))) + 
#         geom_line(size = 2) + 
#         geom_point(size = 4) +
#         geom_text(data = chart_data %>% filter(date_reported == min(date_reported), values != 0),
#                   mapping = aes(x = date_reported - 5, y = values, label = partner),
#                   fontface = "bold", hjust = 1) +
#         scale_color_manual(values = chart_data_color_list, 
#                            # guide = FALSE,
#                            labels = c("Advertising", "Commercial partnerships",
#                                       "Other", "Subscriptions/crowdfunding")) +
#         scale_linetype_manual(values = chart_data_linetype_list, guide = FALSE,
#                               labels = c("Advertising", "Commercial partnerships",
#                                          "Other", "Subscriptions/crowdfunding")) +
#         scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), limits = c(0, 1), expand = c(0, 0),
#                            labels = percent_format(accuracy = 1)) +
#         scale_x_date(breaks = seq(from = ymd("2019-04-01"), to = ymd("2020-04-01"), by = 365),
#                      date_labels = "%m/%Y", expand = c(.05, .05)) +
#         labs(x = NULL, y = "Share of income", 
#              title = NULL,
#              caption = NULL, color = "Business model", linetype = "") +
#         coord_fixed(ratio = 200 / 1, clip = "off") +
#         theme_bw() +
#         theme(
#                 # plot.background = element_rect(fill = "blue"),
#                 plot.margin = unit(c(0, 10, 0, 0), "mm"),
#                 plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
#                                             color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
#                 # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
#                 panel.grid.minor = element_blank(),
#                 panel.grid.major.x = element_blank(),
#                 panel.grid.major.y = element_line(color = "#DDDDDD"),
#                 # panel.grid.major.y = element_line(color = "#000000"),
#                 panel.border = element_blank(),
#                 # panel.grid = element_blank(),
#                 # line = element_blank(),
#                 # rect = element_blank(),
#                 axis.ticks.y = element_blank(),
#                 # axis.ticks.x = element_blank(),
#                 # axis.ticks.length.y.left = unit(.2, "cm"),
#                 axis.ticks.length.x.bottom = unit(.2, "cm"),
#                 axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
#                                            margin = margin(t = 5, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
#                 axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
#                                            margin = margin(t = 0, r = 5, b = 0, l = 0)),
#                 axis.line.x.bottom = element_line(color = "#333333"),
#                 axis.line.y.left = element_blank(),
#                 axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
#                                             margin = margin(t = 13, r = 0, b = 5, l = 0)),
#                 axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
#                                             margin = margin(t = 0, r = 13, b = 0, l = 0)),
#                 plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
#                                           margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
#                 legend.position = "bottom",
#                 # legend.key.size = unit(2, "mm"), 
#                 legend.title = element_text(size = 12, family = "Calibri", face = "plain", color = "#333333"),
#                 legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
#                                            hjust = .5, color = "#333333")
#                 # legend.spacing.y = unit(5.5, "cm"),
#                 # legend.key = element_rect(size = 5),
#                 # legend.key.size = unit(2, 'lines')
#         ) 
# # guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333", keywidth = 4),
# #        linetype = guide_legend(keywidth = 4))
# 
# 
# # inspect
# sen_income_share_per_business_model_line_chart
# 
# 
# #//////////////////////////////////
# 
# 
# # save chart as emf
# filename <- tempfile(fileext = ".emf")
# emf(file = filename)
# print(sen_income_share_per_business_model_line_chart)
# dev.off()
# 
# # add emf to word doc  
# read_docx() %>% 
#         body_add_img(src = filename, width = 6, height = 6) %>% 
#         print(target = "output/charts/sen_income_share_per_business_model_line_chart.docx")


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# create sen_2b_partner_total_income_line_chart 

# # this chart won't be included because it's better represented as a grouped bar chart, which plots the data more clearly
# 
# # check
# sen_data %>% filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income") %>%
#         arrange(partner) %>%
#         select(indicator_number, indicator_name,
#                level_2_indicator_name,
#                # level_3_indicator_name,
#                partner, date_reported, values) %>% print(n = nrow(.))
# 
# # inspect dates
# sen_data %>% filter(indicator_number == "2b") %>% distinct(date_reported)
# 
# # inspect level_2_indicator/partner combos
# sen_data %>% filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income") %>%
#         select(level_2_indicator_name, date_reported, partner) %>%
#         count(level_2_indicator_name, date_reported, partner)
# 
# # inspect level_2_indicator/partner combos with only 1 measurement period
# sen_data %>% filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income") %>%
#         select(level_2_indicator_name, date_reported, partner) %>%
#         count(level_2_indicator_name, partner)
# 
# # get inspect_chart_data with total_income per partner/period
# # note that partner 5 and 6 have no total or sub-category revenue data for period 2
# # partner 7 has total revenue for period 1 and 2, but sub-category data only for period 1
# # note that inspect_chart_data uses the sum of sub-categories as total revenue so that sub-category shares sum to 1
# # except for partner 7 in period 2, where i used the total revenue as reported, since sub-cateogry revenue was not reported
# inspect_chart_data <- sen_data %>%
#         filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income",
#                !(partner == "Partner 7" & date_reported == "2020-04-01")) %>%
#         select(partner, date_reported, values) %>%
#         group_by(partner, date_reported) %>% mutate(total_income = sum(values)) %>% slice(1) %>%
#         ungroup() %>% arrange(partner, date_reported) %>% select(-values) %>%
#         bind_rows(., sen_data %>%
#                           filter(indicator_number == "2b", level_2_indicator_name == "Total non-grant income",
#                                  partner == "Partner 7", date_reported == "2020-04-01") %>%
#                           select(partner, date_reported, values) %>%
#                           rename(total_income = values))
# 
# inspect_chart_data %>% print(n = nrow(.))
# 
# 
# #/////////////////////
# 
# 
# # clean/prep, and add color_bin and color
# chart_data <- inspect_chart_data %>%
#         rename(values = total_income) %>% 
#         filter(partner != "Partner 6") %>%
#         mutate(color_bin = partner,
#                color = case_when(color_bin == "Partner 1" ~ color_palette %>% slice(7) %>% pull(hex),
#                                  color_bin == "Partner 2" ~ color_palette %>% slice(1) %>% pull(hex),
#                                  color_bin == "Partner 3" ~ color_palette %>% slice(2) %>% pull(hex),
#                                  color_bin == "Partner 4" ~ color_palette %>% slice(3) %>% pull(hex),
#                                  color_bin == "Partner 5" ~ color_palette %>% slice(4) %>% pull(hex),
#                                  color_bin == "Partner 6" ~ color_palette %>% slice(5) %>% pull(hex),
#                                  color_bin == "Partner 7" ~ color_palette %>% slice(6) %>% pull(hex)),
#                linetype_bin = partner,
#                linetype = case_when(TRUE ~ "solid"))
# 
# # create color_list for to pass to scale_color_manual
# chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
# names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
# chart_data_color_list
# 
# # create linetype_list for to pass to scale_linetype_manual
# chart_data_linetype_list <- chart_data %>% count(linetype_bin, linetype) %>% pull(linetype)
# names(chart_data_linetype_list) <- chart_data %>% count(linetype_bin, linetype) %>% pull(linetype_bin)
# chart_data_linetype_list
# 
# # get title
# title <- "SEN partners' total non-grant revenue from new business models/strategies"
# 
# 
# #/////////////////////
# 
# 
# # create chart
# sen_2b_partner_total_income_line_chart <- chart_data %>%
#         ggplot(data = ., aes(x = date_reported, 
#                              y = values, 
#                              color = factor(color_bin, levels = c("Partner 1", "Partner 2", "Partner 3",
#                                                                   "Partner 4", "Partner 5", "Partner 6", "Partner 7")),
#                              linetype = factor(color_bin, levels = c("Partner 1", "Partner 2", "Partner 3",
#                                                                      "Partner 4", "Partner 5", "Partner 6", "Partner 7")))) + 
#         geom_line(size = 2) + 
#         geom_point(size = 4) +
#         geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner != "Partner 4"),
#                   mapping = aes(x = date_reported + 10, y = values, label = color_bin),
#                   fontface = "bold", hjust = 0) +
#         geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 4"),
#                   mapping = aes(x = date_reported + 10, y = values + 5000, label = color_bin),
#                   fontface = "bold", hjust = 0) +
#         geom_text(data = chart_data %>% filter(date_reported == min(date_reported), partner == "Partner 5"),
#                   mapping = aes(x = date_reported + 10, y = values + 5000, label = color_bin),
#                   fontface = "bold", hjust = 0) +
#         scale_color_manual(values = chart_data_color_list, 
#                            guide = FALSE,
#                            labels = c("Partner 1", "Partner 2", "Partner 3",
#                                       "Partner 4", "Partner 5", "Partner 6", "Partner 7")) +
#         scale_linetype_manual(values = chart_data_linetype_list, guide = FALSE,
#                               labels = c("Partner 1", "Partner 2", "Partner 3",
#                                          "Partner 4", "Partner 5", "Partner 6", "Partner 7")) +
#         scale_y_continuous(breaks = seq(from = 0, to = 125000, by = 25000), limits = c(0, 130000), expand = c(0, 0),
#                            labels = dollar_format(accuracy = 1)) +
#         scale_x_date(breaks = c(ymd("2019-04-01"), ymd("2020-04-01")),
#                      date_labels = "%m/%Y", expand = c(0, 0)) +
#         labs(x = NULL, y = "U.S. dollars (constant 2020)", 
#              title = NULL,
#              caption = NULL, color = "Business model", linetype = "") +
#         coord_fixed(ratio = .01 / 6, clip = "off") +
#         theme_bw() +
#         theme(
#                 # plot.background = element_rect(fill = "blue"),
#                 plot.margin = unit(c(0, 20, 0, 0), "mm"),
#                 plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
#                                             color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
#                 # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
#                 panel.grid.minor = element_blank(),
#                 panel.grid.major.x = element_blank(),
#                 panel.grid.major.y = element_line(color = "#DDDDDD"),
#                 # panel.grid.major.y = element_line(color = "#000000"),
#                 panel.border = element_blank(),
#                 # panel.grid = element_blank(),
#                 # line = element_blank(),
#                 # rect = element_blank(),
#                 axis.ticks.y = element_blank(),
#                 # axis.ticks.x = element_blank(),
#                 # axis.ticks.length.y.left = unit(.2, "cm"),
#                 axis.ticks.length.x.bottom = unit(.2, "cm"),
#                 axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
#                                            margin = margin(t = 5, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
#                 axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
#                                            margin = margin(t = 0, r = 5, b = 0, l = 0)),
#                 axis.line.x.bottom = element_line(color = "#333333"),
#                 axis.line.y.left = element_blank(),
#                 axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
#                                             margin = margin(t = 13, r = 0, b = 5, l = 0)),
#                 axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
#                                             margin = margin(t = 0, r = 13, b = 0, l = 0)),
#                 plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
#                                           margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
#                 legend.position = "bottom",
#                 # legend.key.size = unit(2, "mm"), 
#                 legend.title = element_text(size = 12, family = "Calibri", face = "plain", color = "#333333"),
#                 legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
#                                            hjust = .5, color = "#333333")
#                 # legend.spacing.y = unit(5.5, "cm"),
#                 # legend.key = element_rect(size = 5),
#                 # legend.key.size = unit(2, 'lines')
#         ) 
# # guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333", keywidth = 4),
# #        linetype = guide_legend(keywidth = 4))
# 
# 
# # inspect
# sen_2b_partner_total_income_line_chart
# 
# 
# #//////////////////////////////////
# 
# 
# # save chart as emf
# filename <- tempfile(fileext = ".emf")
# emf(file = filename)
# print(sen_2b_partner_total_income_line_chart)
# dev.off()
# 
# # add emf to word doc  
# read_docx() %>% 
#         body_add_img(src = filename, width = 6, height = 6) %>% 
#         print(target = "output/charts/sen_2b_partner_total_income_line_chart.docx")


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


## create sen_business_model_count_per_partner_line_chart
# 
# 
# # check
# sen_data %>% filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income") %>%
#         arrange(partner) %>%
#         select(indicator_number, indicator_name,
#                level_2_indicator_name,
#                # level_3_indicator_name,
#                partner, date_reported, values) %>% print(n = nrow(.))
# 
# # inspect dates
# sen_data %>% filter(indicator_number == "2b") %>% distinct(date_reported)
# 
# # inspect level_2_indicator/partner combos
# sen_data %>% filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income") %>%
#         select(level_2_indicator_name, date_reported, partner) %>%
#         count(level_2_indicator_name, date_reported, partner)
# 
# # inspect level_2_indicator/partner combos with only 1 measurement period
# sen_data %>% filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income") %>%
#         select(level_2_indicator_name, date_reported, partner) %>%
#         count(level_2_indicator_name, partner)
# 
# # inspect business model count per partner
# sen_data %>% filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income") %>%
#         distinct(level_2_indicator_name, partner, date_reported) %>%
#         group_by(partner, date_reported) %>%
#         mutate(level_2_indicator_name_n_distinct = n_distinct(level_2_indicator_name)) %>%
#         ungroup() %>% arrange(partner, date_reported) %>%
#         select(-level_2_indicator_name) %>% distinct()
# 
# 
# #/////////////////////
# 
# 
# # clean/prep, and add color_bin and color
# chart_data <- sen_data %>% filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income") %>%
#         distinct(level_2_indicator_name, partner, date_reported) %>%
#         group_by(partner, date_reported) %>%
#         mutate(level_2_indicator_name_n_distinct = n_distinct(level_2_indicator_name)) %>%
#         ungroup() %>% arrange(partner, date_reported) %>%
#         select(-level_2_indicator_name) %>% distinct() %>%
#         rename(values = level_2_indicator_name_n_distinct) %>%
#         mutate(color_bin = partner,
#                color = case_when(color_bin == "Partner 1" ~ color_palette %>% slice(7) %>% pull(hex),
#                                  color_bin == "Partner 2" ~ color_palette %>% slice(1) %>% pull(hex),
#                                  color_bin == "Partner 3" ~ color_palette %>% slice(2) %>% pull(hex),
#                                  color_bin == "Partner 4" ~ color_palette %>% slice(3) %>% pull(hex),
#                                  color_bin == "Partner 5" ~ color_palette %>% slice(4) %>% pull(hex),
#                                  color_bin == "Partner 6" ~ color_palette %>% slice(5) %>% pull(hex),
#                                  color_bin == "Partner 7" ~ color_palette %>% slice(6) %>% pull(hex)),
#                linetype_bin = partner,
#                linetype = case_when(TRUE ~ "solid"))
# 
# # create color_list for to pass to scale_color_manual
# chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
# names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
# chart_data_color_list
# 
# # create linetype_list for to pass to scale_linetype_manual
# chart_data_linetype_list <- chart_data %>% count(linetype_bin, linetype) %>% pull(linetype)
# names(chart_data_linetype_list) <- chart_data %>% count(linetype_bin, linetype) %>% pull(linetype_bin)
# chart_data_linetype_list
# 
# # get title
# title <- "Number of business models used by SEN partners"
# 
# 
# #/////////////////////
# 
# 
# # create chart
# sen_business_model_count_per_partner_line_chart <- chart_data %>%
#         ggplot(data = ., aes(x = date_reported,
#                              y = values,
#                              color = factor(color_bin, levels = c("Partner 1", "Partner 2", "Partner 3",
#                                                                   "Partner 4", "Partner 5", "Partner 6", "Partner 7")),
#                              linetype = factor(color_bin, levels = c("Partner 1", "Partner 2", "Partner 3",
#                                                                      "Partner 4", "Partner 5", "Partner 6", "Partner 7")))) +
#         geom_line(size = 2) +
#         geom_point(size = 4) +
#         geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner != "Partner 4"),
#                   mapping = aes(x = date_reported + 10, y = values, label = color_bin),
#                   fontface = "bold", hjust = 0) +
#         geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 4"),
#                   mapping = aes(x = date_reported + 10, y = values + 5000, label = color_bin),
#                   fontface = "bold", hjust = 0) +
#         geom_text(data = chart_data %>% filter(date_reported == min(date_reported), partner == "Partner 5"),
#                   mapping = aes(x = date_reported + 10, y = values + 5000, label = color_bin),
#                   fontface = "bold", hjust = 0) +
#         scale_color_manual(values = chart_data_color_list,
#                            guide = FALSE,
#                            labels = c("Partner 1", "Partner 2", "Partner 3",
#                                       "Partner 4", "Partner 5", "Partner 6", "Partner 7")) +
#         scale_linetype_manual(values = chart_data_linetype_list, guide = FALSE,
#                               labels = c("Partner 1", "Partner 2", "Partner 3",
#                                          "Partner 4", "Partner 5", "Partner 6", "Partner 7")) +
#         scale_y_continuous(breaks = seq(from = 0, to = 125000, by = 25000), limits = c(0, 130000), expand = c(0, 0),
#                            labels = dollar_format(accuracy = 1)) +
#         scale_x_date(breaks = seq(from = ymd("2019-04-01"), to = ymd("2020-04-01"), by = 365),
#                      date_labels = "%m/%Y", expand = c(0, 0)) +
#         labs(x = NULL, y = "Number of business models\nused by SEN partners",
#              title = NULL,
#              caption = NULL, color = "Business model", linetype = "") +
#         coord_fixed(ratio = .01 / 6, clip = "off") +
#         theme_bw() +
#         theme(
#                 # plot.background = element_rect(fill = "blue"),
#                 plot.margin = unit(c(0, 20, 0, 0), "mm"),
#                 plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri",
#                                             color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
#                 # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
#                 panel.grid.minor = element_blank(),
#                 panel.grid.major.x = element_blank(),
#                 panel.grid.major.y = element_line(color = "#DDDDDD"),
#                 # panel.grid.major.y = element_line(color = "#000000"),
#                 panel.border = element_blank(),
#                 # panel.grid = element_blank(),
#                 # line = element_blank(),
#                 # rect = element_blank(),
#                 axis.ticks.y = element_blank(),
#                 # axis.ticks.x = element_blank(),
#                 # axis.ticks.length.y.left = unit(.2, "cm"),
#                 axis.ticks.length.x.bottom = unit(.2, "cm"),
#                 axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333",
#                                            margin = margin(t = 5, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
#                 axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333",
#                                            margin = margin(t = 0, r = 5, b = 0, l = 0)),
#                 axis.line.x.bottom = element_line(color = "#333333"),
#                 axis.line.y.left = element_blank(),
#                 axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333",
#                                             margin = margin(t = 13, r = 0, b = 5, l = 0)),
#                 axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333",
#                                             margin = margin(t = 0, r = 13, b = 0, l = 0)),
#                 plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#333333",
#                                           margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
#                 legend.position = "bottom",
#                 # legend.key.size = unit(2, "mm"),
#                 legend.title = element_text(size = 12, family = "Calibri", face = "plain", color = "#333333"),
#                 legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
#                                            hjust = .5, color = "#333333")
#                 # legend.spacing.y = unit(5.5, "cm"),
#                 # legend.key = element_rect(size = 5),
#                 # legend.key.size = unit(2, 'lines')
#         )
# # guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333", keywidth = 4),
# #        linetype = guide_legend(keywidth = 4))
# 
# 
# # inspect
# sen_business_model_count_per_partner_line_chart
# 
# 
# #//////////////////////////////////
# 
# 
# # save chart as emf
# filename <- tempfile(fileext = ".emf")
# emf(file = filename)
# print(sen_business_model_count_per_partner_line_chart)
# dev.off()
# 
# # add emf to word doc 
# read_docx() %>%
#         body_add_img(src = filename, width = 6, height = 6) %>%
#         print(target = "output/charts/sen_business_model_count_per_partner_line_chart.docx")


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# create sen_2b_business_model_income_share_by_partner_grouped_stacked_bar_chart ####


# check
sen_data %>% filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income") %>%
        arrange(partner) %>%
        select(indicator_number, indicator_name,
               level_2_indicator_name,
               # level_3_indicator_name,
               partner, date_reported, values) %>% print(n = nrow(.))

# inspect dates
sen_data %>% filter(indicator_number == "2b") %>% distinct(date_reported)

# inspect level_2_indicator/partner combos
sen_data %>% filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income") %>%
        select(level_2_indicator_name, date_reported, partner) %>%
        count(level_2_indicator_name, date_reported, partner)

# inspect level_2_indicator/partner combos with only 1 measurement period
sen_data %>% filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income") %>%
        select(level_2_indicator_name, date_reported, partner) %>%
        count(level_2_indicator_name, partner)

# get inspect_chart_data with income_share per business model by partner/date combo
# note that partner 5 and 6 have no total or sub-category revenue data for period 2
# partner 7 has total revenue for period 1 and 2, but sub-category data only for period 1
# note that in clean_sen_data.R when inspecting metric_2b, it shows that the sum of sub-categories doesn't always equal the
# total income for some partners; this is due to currency conversions at slightly different times/rates
# but the largest difference is only 6% of total revenue
# note that inspect_chart_data uses the sum of sub-categories as total revenue so that sub-category shares sum to 1
# except for partner 7 in period 2, where i used the total revenue as reported, since sub-cateogry revenue was not reported
inspect_chart_data <- sen_data %>%
        filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income",
               !(partner == "Partner 7" & date_reported == "2020-04-01")) %>%
        select(partner, date_reported, values) %>%
        group_by(partner, date_reported) %>% mutate(total_income = sum(values)) %>% slice(1) %>%
        ungroup() %>% arrange(partner, date_reported) %>% select(-values) %>%
        bind_rows(., sen_data %>%
                          filter(indicator_number == "2b", level_2_indicator_name == "Total non-grant income",
                                 partner == "Partner 7", date_reported == "2020-04-01") %>%
                          select(partner, date_reported, values) %>%
                          rename(total_income = values)) %>%
        left_join(sen_data %>%
                          filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income") %>%
                          select(level_2_indicator_name, partner, date_reported, values), .,
                  by = c("partner", "date_reported")) %>%
        arrange(partner, date_reported, level_2_indicator_name) %>%
        mutate(income_share = values / total_income)

# inspect
inspect_chart_data
inspect_chart_data %>% group_by(partner, date_reported) %>% summarize(sum_income_share = sum(income_share))
expect_equal(object = inspect_chart_data %>% group_by(partner, date_reported) %>%
                     summarize(sum_income_share = round(sum(income_share), digits = 2)) %>%
                     ungroup() %>% distinct(sum_income_share) %>% pull(sum_income_share),
             expected = 1)


#//////////////////////


# add color_bin and color
chart_data <- inspect_chart_data %>%
        mutate(partner_n_distinct = n_distinct(partner),
               level_2_indicator_name = case_when(level_2_indicator_name == "Subscriptions/crowdfunding" ~
                                                          "Subscriptions/\ncrowdfunding",
                                                  level_2_indicator_name == "Commercial partnerships" ~
                                                          "Commercial\npartnerships",
                                                  TRUE ~ level_2_indicator_name)) %>%
        select(-values) %>% rename(values = income_share) %>%
        mutate(color_bin = level_2_indicator_name,
               color = case_when(color_bin == "Advertising" ~ color_palette %>% slice(1) %>% pull(hex),
                                 color_bin == "Commercial\npartnerships" ~ color_palette %>% slice(2) %>% pull(hex),
                                 color_bin == "Other" ~ color_palette %>% slice(3) %>% pull(hex),
                                 color_bin == "Subscriptions/\ncrowdfunding" ~ color_palette %>% slice(4) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list

# get title
title <- "SEN partners' share of revenue generated per business model"


#/////////////////////


# create chart
sen_2b_business_model_income_share_by_partner_grouped_stacked_bar_chart <- chart_data %>%
        ggplot(data = ., aes(x = date_reported, 
                             y = values, 
                             fill = factor(color_bin, levels = c("Advertising", "Commercial\npartnerships",
                                                                  "Other", "Subscriptions/\ncrowdfunding")))) + 
        geom_bar(position = "fill", stat = "identity", width = 300) + 
        facet_wrap(facets = vars(partner), nrow = 1) +
        # geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 1"),
        #           mapping = aes(x = date_reported + 20, y = values, label = color_bin),
        #           fontface = "bold", hjust = 0) +
        scale_fill_manual(values = chart_data_color_list, 
                           # guide = FALSE,
                           labels = c("Advertising", "Commercial\npartnerships",
                                      "Other", "Subscriptions/\ncrowdfunding")) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), limits = c(0, 1), expand = c(0, 0),
                           labels = percent_format(accuracy = 1)) +
        scale_x_date(breaks = c(ymd("2019-04-01"), ymd("2020-04-01")),
                     date_labels = "%m/%Y", expand = c(.4, .4)) +
        labs(x = NULL, y = "Share of revenue", 
             title = NULL, fill = NULL,
             caption = NULL) +
        coord_fixed(ratio = 300 / .1, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 5, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                panel.spacing = unit(0, "lines"),
                # panel.grid = element_blank(),
                # strip.background = element_rect(fill = "#ffffff"),
                strip.background = element_blank(),
                strip.text = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333"),
                # line = element_blank(),
                # rect = element_blank(),
                axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                           margin = margin(t = 0, r = 0, b = 0, l = 0), angle = 320, hjust = 0),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                           margin = margin(t = 0, r = 5, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 0, r = 13, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) 
# guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333", keywidth = 4),
#        linetype = guide_legend(keywidth = 4))


# inspect
sen_2b_business_model_income_share_by_partner_grouped_stacked_bar_chart


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(sen_2b_business_model_income_share_by_partner_grouped_stacked_bar_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/sen_2b_business_model_income_share_by_partner_grouped_stacked_bar_chart.docx")


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# # create sen_2b_pct_income_change_per_business_model_grouped_bar_chart 
# 
# # note this chart isn't used because the pct_income_change is so varied (e.g. -96% to 561%)
# # and because there is only period 1 & 2 data for partner 2 (advertising), partner 3(ads, commercial), and partner 4(commercial)
# 
# # check 
# sen_data %>% filter(indicator_number == "2b") %>% 
#         arrange(partner) %>%
#         select(indicator_number, indicator_name, 
#                # level_2_indicator_name, level_3_indicator_name, 
#                partner, date_reported, values, notes) %>% print(n = nrow(.))
# 
# # check dates
# sen_data %>% filter(indicator_number == "2b") %>% count(date_reported)
# 
# # inspect level_2_indicator/partner combos
# sen_data %>% filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income") %>%
#         select(level_2_indicator_name, date_reported, partner) %>%
#         count(level_2_indicator_name, date_reported, partner)
# 
# # inspect level_2_indicator/partner combos with only 1 measurement period
# sen_data %>% filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income") %>%
#         select(level_2_indicator_name, date_reported, partner) %>%
#         count(level_2_indicator_name, partner)
# 
# # inspect pct_income_change
# sen_data %>% filter(indicator_number == "2b", level_2_indicator_name != "Total non-grant income") %>%
#         select(level_2_indicator_name, date_reported, partner, values) %>%
#         mutate(date = "date", 
#                date_reported = str_replace_all(string = date_reported, pattern = "-", replacement = "_")) %>% 
#         unite(col = "date_reported", date, date_reported, sep = "_") %>%
#         pivot_wider(id_cols = -date_reported, names_from = date_reported, values_from = values) %>%
#         mutate(pct_income_change = (date_2020_04_01 - date_2019_04_01) / date_2019_04_01)


#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////


# create sen_mobile_social_visitors_bubble_chart ####

# check 
sen_data %>% filter(indicator_number == "2.2a") %>%
        select(indicator_number, indicator_name,
               # level_2_indicator_name, level_3_indicator_name,
               partner, date_reported, values, notes) %>% print(n = nrow(.))

sen_data %>% filter(indicator_number == "1.3b") %>%
        select(indicator_number, indicator_name,
               level_2_indicator_name, 
               # level_3_indicator_name,
               partner, date_reported, values) %>% print(n = nrow(.))

# confirm that more partners have unique visitor data from google analytics than IO analytics
# so will use google analytics data in chart
sen_data %>% filter(indicator_number == "1.3b", level_2_indicator_name == "Google Analytics") %>%
        count(date_reported)
sen_data %>% filter(indicator_number == "1.3b", level_2_indicator_name == "IO Analytics") %>%
        count(date_reported)

# note that early measurement periods were multi-month, later periods were monthly
# varying gaps for early measurement periods: 6, 7, 7, 8 months, so could divide values by 6, 7, 7, 8 respectively
# but that sen spreadsheet explicitly says this metric is collected bimonthly
# though that appears to have changed to monthly for more recent measurements
# so i could downscale the early measurements by dividing by 6, 7, 7, 8; or could divide them by 2
# or could leave it alone and not downscale, which actually results in the smoothest time series 
# maybe the data collectors already downscaled it? that would make sense and explain the clear pattern
# it's very suspicious that if downscaling were correct it would result it uniform sharp jumps in the post-downscale period
# result: will assume the early measurement periods are already downscaled, this looks most accurate, and will flag for nick
sen_data %>% filter(indicator_number == "1.3b", level_2_indicator_name == "Google Analytics") %>% distinct(date_reported)
sen_data %>% filter(indicator_number == "1.3b", level_2_indicator_name == "Google Analytics") %>%
        select(partner, date_reported, values) %>% arrange(partner, date_reported) %>% print(n = nrow(.)) 
sen_data %>% filter(indicator_number == "1.3b", level_2_indicator_name == "Google Analytics") %>%
        ggplot(data = ., mapping = aes(x = date_reported, y = values, color = partner)) + geom_line()
sen_data %>% filter(indicator_number == "1.3b", level_2_indicator_name == "Google Analytics") %>%
        mutate(values_v2 = case_when(date_reported == "2018-03-01" ~ values / 6,
                                     date_reported == "2018-09-01" ~ values / 7,
                                     date_reported == "2019-04-01" ~ values / 7,
                                     date_reported == "2019-12-01" ~ values / 8,
                                     TRUE ~ values)) %>%
        ggplot(data = ., mapping = aes(x = date_reported, y = values_v2, color = partner)) + geom_line()
distinct(partner, date_reported, values, values_v2) %>% arrange(date_reported) %>% print(n = nrow(.))
sen_data %>% filter(indicator_number == "1.3b", level_2_indicator_name == "Google Analytics") %>%
        mutate(values_v2 = case_when(date_reported == "2018-03-01" ~ values / 2,
                                     date_reported == "2018-09-01" ~ values / 2,
                                     date_reported == "2019-04-01" ~ values / 2,
                                     date_reported == "2019-12-01" ~ values / 2,
                                     TRUE ~ values)) %>%
        ggplot(data = ., mapping = aes(x = date_reported, y = values_v2, color = partner)) + geom_line()
distinct(partner, date_reported, values, values_v2) %>% arrange(date_reported) %>% print(n = nrow(.))

# check 1.3b for outliers
# note that partner 6 does have a month with 12m google visitors/30 mil io visitors
# not sure if this is bad data, but the consistency across google/io, and residual effect in sequential months
# indicate it's not a random fat-finger error
# for reference though, home depot got 48 mil visitors in december 2019, so there may be an issue 
# https://www.statista.com/statistics/271450/monthly-unique-visitors-to-us-retail-websites/#:~:text=In%20the%20United%20States%2C%20Amazon,terms%20of%20revenue%20and%20reach.
sen_data %>% filter(indicator_number == "1.3b", level_2_indicator_name == "Google Analytics") %>% skim(values)
sen_data %>% filter(indicator_number == "1.3b", level_2_indicator_name == "Google Analytics") %>%
        ggplot(data = ., mapping = aes(x = partner, y = values)) + geom_boxplot()
sen_data %>% filter(indicator_number == "1.3b", level_2_indicator_name == "Google Analytics") %>%
        ggplot(data = ., mapping = aes(x = date_reported, y = values, color = partner)) + geom_line()


# inspect 2.2a
# interesting to note partner 6 has surge in social media driven audience share for 4/2020
# note that partner 6 visitor count surged the month prior 3/2020 (see 1.3b inspection above)
# but partner 6 did not have an abnormally high count of sen-exchanged articles either month
# maybe a SEN story(s) went viral, or some non-SEN journalism?
sen_data %>% filter(indicator_number == "2.2a") %>% distinct(date_reported)
sen_data %>% filter(indicator_number == "2.2a") %>%
        select(indicator_name, partner, date_reported, values) %>%
        pivot_wider(id_cols = c(partner, date_reported), names_from = indicator_name, values_from = values) %>%
        print(n = nrow(.))
sen_data %>% filter(indicator_number == "2.2a") %>%
        select(indicator_name, partner, date_reported, values) %>%
        pivot_wider(id_cols = c(partner, date_reported), names_from = indicator_name, values_from = values) %>%
        ggplot(data = ., mapping = aes(x = date_reported, y = share_of_total_visitors_driven_by_social_media,
                                       color = partner)) + geom_line()

# inspect chart_data
sen_data %>% filter(indicator_number == "2.2a") %>%
        select(indicator_name, partner, date_reported, values) %>%
        pivot_wider(id_cols = c(partner, date_reported), names_from = indicator_name, values_from = values) %>%
        left_join(., sen_data %>% filter(indicator_number == "1.3b", level_2_indicator_name == "Google Analytics") %>%
                          rename(unique_users_per_month = values) %>%
                          select(partner, date_reported, unique_users_per_month),
                  by = c("partner", "date_reported")) %>% 
        print(n = nrow(.))


#///////////////////////


# add color_bin and color
chart_data <- sen_data %>% filter(indicator_number == "2.2a") %>%
        select(indicator_name, partner, date_reported, values) %>%
        pivot_wider(id_cols = c(partner, date_reported), names_from = indicator_name, values_from = values) %>%
        left_join(., sen_data %>% filter(indicator_number == "1.3b", level_2_indicator_name == "Google Analytics") %>%
                          rename(unique_users_per_month = values) %>%
                          select(partner, date_reported, unique_users_per_month),
                  by = c("partner", "date_reported")) %>%
        mutate(unique_users_per_month = unique_users_per_month / 1000000) %>%
        mutate(color_bin = partner,
               color = case_when(color_bin == "Partner 1" ~ color_palette %>% slice(7) %>% pull(hex),
                                                   color_bin == "Partner 2" ~ color_palette %>% slice(1) %>% pull(hex),
                                                   color_bin == "Partner 3" ~ color_palette %>% slice(2) %>% pull(hex),
                                                   color_bin == "Partner 4" ~ color_palette %>% slice(3) %>% pull(hex),
                                                   color_bin == "Partner 5" ~ color_palette %>% slice(4) %>% pull(hex),
                                                   color_bin == "Partner 6" ~ color_palette %>% slice(5) %>% pull(hex),
                                                   color_bin == "Partner 7" ~ color_palette %>% slice(6) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


# title
title <- "Visitors to SEN partner websites"


#/////////////////////


# create chart
sen_mobile_social_visitors_bubble_chart <- chart_data %>% 
        ggplot(data = ., aes(x = share_of_total_visitors_driven_by_mobile, 
                             y = share_of_total_visitors_driven_by_social_media, 
                             size = unique_users_per_month,
                             label = color_bin,
                             color = factor(color_bin, levels = c("Partner 1", "Partner 2", "Partner 3",
                                                                  "Partner 4", "Partner 5", "Partner 6", "Partner 7")))) + 
        # geom_abline(intercept = 0, slope = 1, color = "#DDDDDD") +
        geom_point(alpha = .7) + 
        scale_size(range = c(2, 15), 
                   breaks = c(1, 6, 12),
                   labels = add_number_label(breaks = c(1, 6, 12), label = " mil")) +
        # geom_text_repel(fontface = "bold", point.padding = .3, size = 3.25, segment.alpha = 0) +
        geom_text(data = chart_data %>% 
                          filter(partner == "Partner 2") %>%
                filter(share_of_total_visitors_driven_by_social_media == max(share_of_total_visitors_driven_by_social_media)),
                  mapping = aes(x = share_of_total_visitors_driven_by_mobile - .14, 
                                y = share_of_total_visitors_driven_by_social_media, label = color_bin),
                  size = 4, fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% 
                          filter(partner == "Partner 3") %>%
                          filter(share_of_total_visitors_driven_by_mobile == min(share_of_total_visitors_driven_by_mobile)),
                  mapping = aes(x = share_of_total_visitors_driven_by_mobile, 
                                y = share_of_total_visitors_driven_by_social_media + .1, label = color_bin),
                  size = 4, fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% 
                          filter(partner == "Partner 4") %>%
                          filter(round(share_of_total_visitors_driven_by_mobile, digits = 3) == .788),
                  mapping = aes(x = share_of_total_visitors_driven_by_mobile + .02, 
                                y = share_of_total_visitors_driven_by_social_media - .03, label = color_bin),
                  size = 4, fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% 
                          filter(partner == "Partner 5") %>%
                          filter(round(share_of_total_visitors_driven_by_social_media, digits = 2) == .64),
                  mapping = aes(x = share_of_total_visitors_driven_by_mobile + .02, 
                                y = share_of_total_visitors_driven_by_social_media - .03, label = color_bin),
                  size = 4, fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% 
                          filter(partner == "Partner 6") %>%
                          filter(round(share_of_total_visitors_driven_by_mobile, digits = 3) == .732),
                  mapping = aes(x = share_of_total_visitors_driven_by_mobile + .05, 
                                y = share_of_total_visitors_driven_by_social_media, label = color_bin),
                  size = 4, fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% 
                          filter(partner == "Partner 7") %>%
                          filter(round(share_of_total_visitors_driven_by_social_media, digits = 3) == .805),
                  mapping = aes(x = share_of_total_visitors_driven_by_mobile + .05, 
                                y = share_of_total_visitors_driven_by_social_media, label = color_bin),
                  size = 4, fontface = "bold", hjust = 0) +
        scale_color_manual(values = chart_data_color_list, guide = FALSE) +
        # scale_linetype_manual(values = chart_data_linetype_list, guide = FALSE,
        #                       labels = c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs")) +
        # scale_fill_manual(values = chart_data_color_list) +
        # scale_x_discrete(expand = c(0, 0)) +
        # scale_y_continuous(breaks = seq(from = 0, to = 100, by = 10), limits = c(-10, 110), expand = c(-.05, 0)) +
        # scale_x_continuous(breaks = seq(from = 0, to = 100, by = 10), limits = c(-10, 110), expand = c(-.05, 0)) +
        scale_y_continuous(breaks = seq(from = 0, to = .9, by = .2), limits = c(-.05, .9), expand = c(0, 0),
                           labels = percent_format(accuracy = 1)) +
        scale_x_continuous(breaks = seq(from = .3, to = 1, by = .1), limits = c(.3, 1), expand = c(0, 0),
                           labels = percent_format(accuracy = 1)) +
        labs(x = "Share of website visitors driven by mobile devices", 
             y = "Share of website visitors\ndriven by social media", 
             title = NULL, size = "Unique visitors\n    to website",
             caption = NULL, color = "") +
        coord_fixed(ratio = 1/2, clip = "off") +
        # coord_fixed(ratio = 1/1.5, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 0, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_blank(),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                           margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                           margin = margin(t = 0, r = 5, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                axis.line.y.left = element_line(color = "#333333"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = -5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 0, r = 13, b = 0, l = 0)),
                plot.title = element_text(size = 18, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "right",
                # legend.key.size = unit(2, "mm"), 
                legend.box.margin=margin(0, 0, 0, 10),
                legend.title = element_text(size = 12, family = "Calibri", face = "plain", color = "#333333", hjust = 0),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) +
        guides(size = guide_legend(override.aes = list(color = "#333333")))
        # guides(color = guide_legend(nrow = 1, byrow = TRUE, label.hjust = 0, color = "#333333"))


# inspect
sen_mobile_social_visitors_bubble_chart


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(sen_mobile_social_visitors_bubble_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/sen_mobile_social_visitors_bubble_chart.docx")



#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////


# # create sen_mobile_social_seconds_scatterplot
# 
# # this plot wont be included because, upon inspection, it's not very interesting/insightful
# # avg seconds doesn't seem very related to mobile/social, but more so to partner, which is shown in the line chart above
# 
# # check 
# sen_data %>% filter(indicator_number == "2.2a") %>%
#         select(indicator_number, indicator_name,
#                # level_2_indicator_name, level_3_indicator_name,
#                partner, date_reported, values, notes) %>% print(n = nrow(.))
# 
# sen_data %>% filter(indicator_number == "1.2b") %>%
#         select(indicator_number, indicator_name,
#                level_2_indicator_name, 
#                # level_3_indicator_name,
#                partner, date_reported, values) %>% print(n = nrow(.))
# 
# 
# # inspect chart_data
# sen_data %>% filter(indicator_number == "2.2a") %>%
#         select(indicator_name, partner, date_reported, values) %>%
#         pivot_wider(id_cols = c(partner, date_reported), names_from = indicator_name, values_from = values) %>%
#         left_join(., sen_data %>% filter(indicator_number == "1.2b") %>%
#                           rename(avg_seconds_spent_per_site_visit = values) %>%
#                           select(partner, date_reported, avg_seconds_spent_per_site_visit),
#                   by = c("partner", "date_reported")) %>% 
#         print(n = nrow(.))
# 
# 
# #///////////////////////
# 
# 
# # add color_bin and color
# chart_data <- sen_data %>% filter(indicator_number == "2.2a") %>%
#         select(indicator_name, partner, date_reported, values) %>%
#         pivot_wider(id_cols = c(partner, date_reported), names_from = indicator_name, values_from = values) %>%
#         left_join(., sen_data %>% filter(indicator_number == "1.2b") %>%
#                           rename(avg_seconds_spent_per_site_visit = values) %>%
#                           select(partner, date_reported, avg_seconds_spent_per_site_visit),
#                   by = c("partner", "date_reported")) 
# 
# # title
# title <- "Visitors to SEN partner websites"
# 
# 
# #/////////////////////
# 
# 
# # create chart
# sen_mobile_social_seconds_scatterplot <- chart_data %>% 
#         ggplot(data = ., aes(x = share_of_total_visitors_driven_by_mobile, 
#                              y = share_of_total_visitors_driven_by_social_media, 
#                              color = avg_seconds_spent_per_site_visit)) + 
#         # geom_abline(intercept = 0, slope = 1, color = "#DDDDDD") +
#         geom_point(alpha = 1, size = 3) + 
#         # geom_text(data = chart_data %>% 
#         #                   filter(partner == "Partner 2") %>%
#         #                   filter(share_of_total_visitors_driven_by_social_media == max(share_of_total_visitors_driven_by_social_media)),
#         #           mapping = aes(x = share_of_total_visitors_driven_by_mobile - .14, 
#         #                         y = share_of_total_visitors_driven_by_social_media, label = color_bin),
#         #           size = 4, fontface = "bold", hjust = 0) +
#         # scale_color_manual(values = chart_data_color_list, guide = FALSE) +
#         # scale_linetype_manual(values = chart_data_linetype_list, guide = FALSE,
#         #                       labels = c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs")) +
#         # scale_fill_manual(values = chart_data_color_list) +
#         # scale_x_discrete(expand = c(0, 0)) +
#         # scale_y_continuous(breaks = seq(from = 0, to = 100, by = 10), limits = c(-10, 110), expand = c(-.05, 0)) +
#         # scale_x_continuous(breaks = seq(from = 0, to = 100, by = 10), limits = c(-10, 110), expand = c(-.05, 0)) +
#         scale_color_gradientn(colors = color_palette %>% slice(1:4) %>% pull(hex)) +
#         scale_y_continuous(breaks = seq(from = 0, to = .9, by = .2), limits = c(-.05, .9), expand = c(0, 0),
#                            labels = percent_format(accuracy = 1)) +
#         scale_x_continuous(breaks = seq(from = .3, to = 1, by = .1), limits = c(.3, 1), expand = c(0, 0),
#                            labels = percent_format(accuracy = 1)) +
#         labs(x = "Monthly share of website visitors driven by mobile devices", 
#              y = "Monthly share of website visitors\ndriven by social media", 
#              title = NULL, size = "      Monthly\nunique visitors\n    to website",
#              caption = NULL, color = "") +
#         coord_fixed(ratio = 1/2.5, clip = "off") +
#         # coord_fixed(ratio = 1/1.5, clip = "off") +
#         theme_bw() +
#         theme(
#                 # plot.background = element_rect(fill = "blue"),
#                 plot.margin = unit(c(0, 0, 0, 0), "mm"),
#                 plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
#                                             color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
#                 # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
#                 panel.grid.minor = element_blank(),
#                 panel.grid.major.x = element_blank(),
#                 panel.grid.major.y = element_blank(),
#                 # panel.grid.major.y = element_line(color = "#000000"),
#                 panel.border = element_blank(),
#                 # panel.grid = element_blank(),
#                 # line = element_blank(),
#                 # rect = element_blank(),
#                 # axis.ticks.y = element_blank(),
#                 # axis.ticks.x = element_blank(),
#                 axis.ticks.length.y.left = unit(.2, "cm"),
#                 axis.ticks.length.x.bottom = unit(.2, "cm"),
#                 axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
#                                            margin = margin(t = 5, r = 0, b = 0, l = 0)),
#                 axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
#                                            margin = margin(t = 0, r = 5, b = 0, l = 0)),
#                 axis.line.x.bottom = element_line(color = "#333333"),
#                 axis.line.y.left = element_line(color = "#333333"),
#                 axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
#                                             margin = margin(t = 13, r = 0, b = -5, l = 0)),
#                 axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
#                                             margin = margin(t = 0, r = 13, b = 0, l = 0)),
#                 plot.title = element_text(size = 18, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
#                                           margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
#                 legend.position = "right",
#                 # legend.key.size = unit(2, "mm"), 
#                 legend.box.margin=margin(0, 0, 0, 10),
#                 legend.title = element_text(size = 12, family = "Calibri", face = "plain", color = "#333333", hjust = 0),
#                 legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
#                                            hjust = .5, color = "#333333")
#                 # legend.spacing.y = unit(5.5, "cm"),
#                 # legend.key = element_rect(size = 5),
#                 # legend.key.size = unit(2, 'lines')
#         ) +
#         guides(size = guide_legend(override.aes = list(color = "#333333")))
# # guides(color = guide_legend(nrow = 1, byrow = TRUE, label.hjust = 0, color = "#333333"))
# 
# 
# # inspect
# sen_mobile_social_seconds_scatterplot
# 
# 
# #//////////////////////////////////
# 
# 
# # save chart as emf
# filename <- tempfile(fileext = ".emf")
# emf(file = filename)
# print(sen_mobile_social_seconds_scatterplot)
# dev.off()
# 
# # add emf to word doc  
# read_docx() %>% 
#         body_add_img(src = filename, width = 6, height = 6) %>% 
#         print(target = "output/charts/sen_mobile_social_seconds_scatterplot.docx")


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# create sen_2_2b_pct_social_media_pieces_further_shared_line_chart ####


# check
sen_data %>% filter(indicator_number == "2.2b") %>%
        arrange(partner) %>%
        select(indicator_number, indicator_name,
               level_2_indicator_name,
               # level_3_indicator_name,
               partner, date_reported, values) %>% print(n = nrow(.))

# inspect dates
# note the first two measurement periods are multi-month, then later it becomes bimonthly
# it does appear like the early periods have not yet been downscaled
# but it's not necessary since i'm dividing them, so any downscaling would affect numerator and denominator equally
# leaving the quotient unchanged
sen_data %>% filter(indicator_number == "2.2b") %>% distinct(date_reported)

# check indicator_name
sen_data %>% filter(indicator_number == "2.2b") %>% distinct(indicator_name)

# inspect chart_data
sen_data %>% filter(indicator_number == "2.2b") %>% select(indicator_name, partner, date_reported, values) %>%
        pivot_wider(id_cols = c(partner, date_reported), names_from = indicator_name, values_from = values) %>%
        mutate(pct_of_pieces_further_shared_by_others = count_of_pieces_shared_on_social_media_by_others / 
                       count_of_pieces_shared_on_social_media_by_partners) %>%
        print(n = nrow(.))

# inspect pct_of_pieces_further_shared_by_others
sen_data %>% filter(indicator_number == "2.2b") %>% select(indicator_name, partner, date_reported, values) %>%
        pivot_wider(id_cols = c(partner, date_reported), names_from = indicator_name, values_from = values) %>%
        mutate(pct_of_pieces_further_shared_by_others = count_of_pieces_shared_on_social_media_by_others / 
                       count_of_pieces_shared_on_social_media_by_partners) %>%
        ggplot(data = ., mapping = aes(x = date_reported, y = pct_of_pieces_further_shared_by_others, color = partner)) +
        geom_line()
sen_data %>% filter(indicator_number == "2.2b", indicator_name == "count_of_pieces_shared_on_social_media_by_others") %>% 
        ggplot(data = ., mapping = aes(x = date_reported, y = values, color = partner)) +
        geom_line()
sen_data %>% filter(indicator_number == "2.2b", indicator_name == "count_of_pieces_shared_on_social_media_by_partners") %>% 
        ggplot(data = ., mapping = aes(x = date_reported, y = values, color = partner)) +
        geom_line()


# note that the spreadsheet appears to have some data quality issues for 2.2b 
# (note this was fixed, as addressed below, so won't be visible in sen_data, but can be seen in original spreadsheet 
# or the inspect_1_3_2a_and_2_2b.xlsx extract)
# for the 4/1/2020 measurement period, the 2.2b - pieces shared by partners diverges from 1.3.2a pieces taken for exchange
# but the counts appear "shifted", since matching counts appear for subsequent partners - can't be a coincidence
# also, the 4/1/2019 measurement period for 2.2b is off by a large amount seemingly at random - 
# likely this is because 2.2b represents multiple months, while 1.3.2a has been pre-downscaled
# since this chart takes the quotient, the scale isn't actually an issue though
# will manually "shift" 2.2b values for 4/1/2020 period, eg old partner 3 values shift to become new partner 2 values
# this makes 2.2b denominators match 1.3.2a, but has downside of leaving partner 7 with no values though
sen_data %>% filter(indicator_number == "1.3.2a" | indicator_name == "count_of_pieces_shared_on_social_media_by_partners" |
                            indicator_name == "count_of_pieces_shared_on_social_media_by_others") %>%
        select(indicator_name, partner, date_reported, values) %>%
        pivot_wider(id_cols = c(partner, date_reported), names_from = indicator_name, values_from = values) %>%
        mutate(diff = monthly_stories_taken_by_partner_for_exchange - count_of_pieces_shared_on_social_media_by_partners) %>%
        relocate(count_of_pieces_shared_on_social_media_by_others, .after = last_col()) %>%
        arrange(date_reported) %>%
        print(n = nrow(.))


#/////////////////////


# clean/prep, and add color_bin and color
chart_data <- sen_data %>% filter(indicator_number == "2.2b") %>% select(indicator_name, partner, date_reported, values) %>%
        pivot_wider(id_cols = c(partner, date_reported), names_from = indicator_name, values_from = values) %>%
        mutate(pct_of_pieces_further_shared_by_others = count_of_pieces_shared_on_social_media_by_others / 
                       count_of_pieces_shared_on_social_media_by_partners) %>%
        rename(values = pct_of_pieces_further_shared_by_others) %>% 
        mutate(color_bin = partner,
               color = case_when(color_bin == "Partner 1" ~ color_palette %>% slice(7) %>% pull(hex),
                                 color_bin == "Partner 2" ~ color_palette %>% slice(1) %>% pull(hex),
                                 color_bin == "Partner 3" ~ color_palette %>% slice(2) %>% pull(hex),
                                 color_bin == "Partner 4" ~ color_palette %>% slice(3) %>% pull(hex),
                                 color_bin == "Partner 5" ~ color_palette %>% slice(4) %>% pull(hex),
                                 color_bin == "Partner 6" ~ color_palette %>% slice(5) %>% pull(hex),
                                 color_bin == "Partner 7" ~ color_palette %>% slice(6) %>% pull(hex)),
               linetype_bin = partner,
               linetype = case_when(TRUE ~ "solid"))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list

# create linetype_list for to pass to scale_linetype_manual
chart_data_linetype_list <- chart_data %>% count(linetype_bin, linetype) %>% pull(linetype)
names(chart_data_linetype_list) <- chart_data %>% count(linetype_bin, linetype) %>% pull(linetype_bin)
chart_data_linetype_list

# get title
title <- "Share of SEN-exchanged stories further shared by others on social media "


#/////////////////////


# create chart
sen_2_2b_pct_social_media_pieces_further_shared_line_chart <- chart_data %>%
        ggplot(data = ., aes(x = date_reported, 
                             y = values, 
                             color = factor(color_bin, levels = c("Partner 1", "Partner 2", "Partner 3",
                                                                  "Partner 4", "Partner 5", "Partner 6", "Partner 7")),
                             linetype = factor(color_bin, levels = c("Partner 1", "Partner 2", "Partner 3",
                                                                     "Partner 4", "Partner 5", "Partner 6", "Partner 7")))) + 
        geom_line(size = 2) + 
        geom_point(size = 4) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 2"),
                  mapping = aes(x = date_reported + 10, y = values, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 3"),
                  mapping = aes(x = date_reported + 10, y = values, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 4"),
                  mapping = aes(x = date_reported + 10, y = values - .05, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 5"),
                  mapping = aes(x = date_reported + 10, y = values - .02, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 6"),
                  mapping = aes(x = date_reported + 10, y = values, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 7"),
                  mapping = aes(x = date_reported + 10, y = values + .02, label = color_bin),
                  fontface = "bold", hjust = 0) +
        scale_color_manual(values = chart_data_color_list, 
                           guide = FALSE,
                           labels = c("Partner 1", "Partner 2", "Partner 3",
                                      "Partner 4", "Partner 5", "Partner 6", "Partner 7")) +
        scale_linetype_manual(values = chart_data_linetype_list, guide = FALSE,
                              labels = c("Partner 1", "Partner 2", "Partner 3",
                                         "Partner 4", "Partner 5", "Partner 6", "Partner 7")) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), limits = c(0, 1), expand = c(0, 0),
                           labels = percent_format(accuracy = 1)) +
        scale_x_date(breaks = c(ymd("2019-04-01"), ymd("2019-08-01"), ymd("2019-12-01"),
                                ymd("2020-04-01"), ymd("2020-08-01")),
                     date_labels = "%m/%Y", expand = c(0, 0)) +
        labs(x = NULL, y = "Share", 
             title = NULL,
             caption = NULL, color = NULL, linetype = "") +
        coord_fixed(ratio = 250 / 1, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 20, 0, 0), "mm"),
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
                axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                           margin = margin(t = 5, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                           margin = margin(t = 0, r = 5, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 0, r = 13, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) 
# guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333", keywidth = 4),
#        linetype = guide_legend(keywidth = 4))


# inspect
sen_2_2b_pct_social_media_pieces_further_shared_line_chart


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(sen_2_2b_pct_social_media_pieces_further_shared_line_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/sen_2_2b_pct_social_media_pieces_further_shared_line_chart.docx")


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# create sen_1_3c_exchanged_story_reach_by_platform_grouped_bar_chart ####

# check
sen_data %>% filter(indicator_number == "1.3c") %>%
        select(indicator_number, 
               # indicator_name,
               level_2_indicator_name, 
               level_3_indicator_name,
               partner, date_reported, values) %>% print(n = nrow(.))

# check dates
sen_data %>% filter(indicator_number == "1.3c") %>% distinct(date_reported)
sen_data %>% filter(indicator_number == "1.3c") %>%
        distinct(partner, level_2_indicator_name, level_3_indicator_name, date_reported) %>%
        group_by(partner, level_2_indicator_name, level_3_indicator_name) %>% add_count(name = "date_reported_count") %>% 
        slice(1) %>% select(-date_reported) %>%
        ungroup() %>%
        print(n = nrow(.))
# note will drop partners/platform combos with < 4 values, usually for earliest measurement periods, 
# the averages for these are really are not representive and will skew results
sen_data %>% filter(indicator_number == "1.3c", values != 0) %>%
        distinct(partner, level_2_indicator_name, level_3_indicator_name, date_reported) %>%
        group_by(partner, level_2_indicator_name, level_3_indicator_name) %>% add_count(name = "date_reported_count") %>% 
        slice(1) %>% select(-date_reported) %>%
        ungroup() %>%
        filter(date_reported_count > 4) %>%
        print(n = nrow(.))

# after dropping partners with < 4 dates, and dropping zero values, there are still enough with valid values to take average
sen_data %>% left_join(sen_data %>% filter(indicator_number == "1.3c", values != 0) %>%
                               distinct(partner, level_2_indicator_name, level_3_indicator_name, date_reported) %>%
                               group_by(partner, level_2_indicator_name, level_3_indicator_name) %>% 
                               add_count(name = "date_reported_count") %>% 
                               slice(1) %>% select(-date_reported) %>%
                               ungroup() %>%
                               filter(date_reported_count > 4), ., 
                       by = c("level_2_indicator_name", "level_3_indicator_name", "partner")) %>%
        filter(indicator_number == "1.3c", level_3_indicator_name == "Exchanged stories") %>% 
        select(level_2_indicator_name, partner, date_reported, values) %>%
        group_by(partner, level_2_indicator_name) %>% add_count(partner) %>% print(n = nrow(.))

# check partners
sen_data %>% filter(indicator_number == "1.3c") %>%
        distinct(partner, level_2_indicator_name, level_3_indicator_name) %>%
        group_by(level_2_indicator_name, level_3_indicator_name) %>%
        add_count(name = "partner_count") %>% arrange(level_2_indicator_name, level_3_indicator_name) %>%
        slice(1) %>% select(-partner) %>%
        ungroup() %>% print(n = nrow(.))

# check level_3_indicator_name
sen_data %>% filter(indicator_number == "1.3c") %>%
        distinct(level_2_indicator_name, level_3_indicator_name) %>%
        group_by(level_3_indicator_name) %>%
        add_count(name = "level_2_indicator_name_count") %>% arrange(level_3_indicator_name) %>%
        slice(1) %>% select(-level_2_indicator_name) %>%
        ungroup() %>% print(n = nrow(.))

# check values 
# note will drop zero values since they'll skew the average and almost certainly just missing data
# very unlikely the actual measured value is zero social media reach for random periods
sen_data %>% filter(indicator_number == "1.3c", level_3_indicator_name == "Exchanged stories") %>% 
        select(level_2_indicator_name, partner, date_reported, values) %>% print(n = nrow(.))
sen_data %>% filter(indicator_number == "1.3c", level_3_indicator_name == "Exchanged stories",
                    values == 0) %>% 
        select(level_2_indicator_name, partner, date_reported, values) %>% print(n = nrow(.))

sen_data %>% filter(indicator_number == "1.3c", level_3_indicator_name == "Exchanged stories",
                    values != 0) %>%
        ggplot(data = ., mapping = aes(x = level_2_indicator_name, y = values)) + geom_boxplot()
sen_data %>% filter(indicator_number == "1.3c", level_3_indicator_name == "Exchanged stories",
                    values != 0) %>%
        group_by(level_2_indicator_name) %>% summarize(values_mean = mean(values)) %>%
        ungroup() %>%
        ggplot(data = ., mapping = aes(x = level_2_indicator_name, y = values_mean)) + geom_col()

sen_data %>% filter(indicator_number == "1.3c", level_3_indicator_name == "Commissioned stories",
                    values != 0) %>%
        ggplot(data = ., mapping = aes(x = level_2_indicator_name, y = values)) + geom_boxplot()     
sen_data %>% filter(indicator_number == "1.3c", level_3_indicator_name == "Commissioned stories",
                    values != 0) %>%
        group_by(level_2_indicator_name) %>% summarize(values_mean = mean(values)) %>%
        ungroup() %>%
        ggplot(data = ., mapping = aes(x = level_2_indicator_name, y = values_mean)) + geom_col()

# inspect chart_data
# get non_zero_multi_month_observations
non_zero_multi_month_observations <- sen_data %>% left_join(sen_data %>% filter(indicator_number == "1.3c", values != 0) %>%
                                                                    distinct(partner, level_2_indicator_name, level_3_indicator_name, date_reported) %>%
                                                                    group_by(partner, level_2_indicator_name, level_3_indicator_name) %>% 
                                                                    add_count(name = "date_reported_count") %>% 
                                                                    slice(1) %>% select(-date_reported) %>%
                                                                    ungroup() %>%
                                                                    filter(date_reported_count > 4), ., 
                                                            by = c("level_2_indicator_name", "level_3_indicator_name", "partner")) %>%
        filter(indicator_number == "1.3c", level_3_indicator_name == "Exchanged stories") %>% 
        select(level_2_indicator_name, partner, date_reported, values)
non_zero_multi_month_observations %>% print(n = nrow(.))

# get means for each partner and overall; note i add placeholder zeroes so the x-axis groups have same width
non_zero_multi_month_observations %>%
        group_by(partner, level_2_indicator_name) %>%
        summarize(values_mean = mean(values)) %>%
        ungroup() %>% arrange(level_2_indicator_name) %>%
        left_join(sen_data %>% filter(indicator_number == "1.3c", level_3_indicator_name == "Exchanged stories") %>%
                          distinct(partner, level_2_indicator_name) %>% expand(nesting(partner), level_2_indicator_name), .,
                  by = c("partner", "level_2_indicator_name")) %>%
        mutate(values_mean = case_when(is.na(values_mean) ~ 0, TRUE ~ values_mean)) %>%
        bind_rows(., non_zero_multi_month_observations %>%
                          group_by(level_2_indicator_name) %>%
                          summarize(values_mean = mean(values)) %>%
                          ungroup() %>% mutate(partner = "All partners") %>% relocate(partner)) %>%
        print(n = nrow(.))






#/////////////////////////


# add color_bin and color
chart_data <- non_zero_multi_month_observations %>%
        group_by(partner, level_2_indicator_name) %>%
        summarize(values_mean = mean(values)) %>%
        ungroup() %>% arrange(level_2_indicator_name) %>%
        left_join(sen_data %>% filter(indicator_number == "1.3c", level_3_indicator_name == "Exchanged stories") %>%
                          distinct(partner, level_2_indicator_name) %>% expand(nesting(partner), level_2_indicator_name), .,
                  by = c("partner", "level_2_indicator_name")) %>%
        mutate(values_mean = case_when(is.na(values_mean) ~ 0, TRUE ~ values_mean)) %>%
        bind_rows(., non_zero_multi_month_observations %>%
                          group_by(level_2_indicator_name) %>%
                          summarize(values_mean = mean(values)) %>%
                          ungroup() %>% mutate(partner = "All partners") %>% relocate(partner)) %>%
        filter(partner != "Partner 1") %>%
        mutate(color_bin = partner,
               color = case_when(color_bin == "Partner 2" ~ color_palette %>% slice(1) %>% pull(hex),
                                 color_bin == "Partner 3" ~ color_palette %>% slice(2) %>% pull(hex),
                                 color_bin == "Partner 4" ~ color_palette %>% slice(3) %>% pull(hex),
                                 color_bin == "Partner 5" ~ color_palette %>% slice(4) %>% pull(hex),
                                 color_bin == "Partner 6" ~ color_palette %>% slice(5) %>% pull(hex),
                                 color_bin == "Partner 7" ~ color_palette %>% slice(6) %>% pull(hex),
                                 color_bin == "All partners" ~ color_palette %>% slice(7) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list

# get title
title <- "Average number of people reached by SEN-exchanged stories 
on social media platforms, by partner
"


#/////////////////////


# create chart
sen_1_3c_exchanged_story_reach_by_platform_grouped_bar_chart <- chart_data %>%
        ggplot(data = ., aes(x = factor(level_2_indicator_name, levels = c("Facebook", "Odnoklassniki",
                                                                           "Telegram", "Twitter", "Vkontakte")),
                             y = values_mean, 
                             fill = factor(color_bin, levels = c("Partner 2", "Partner 3",
                                                                 "Partner 4", "Partner 5", "Partner 6", "Partner 7",
                                                                 "All partners")))) + 
        geom_bar(stat = "identity", position = position_dodge(width = .7, preserve = "single"), width = .7) +
        scale_fill_manual(values = chart_data_color_list) +
        scale_x_discrete(expand = c(0, 0)) +
        scale_y_continuous(breaks = seq(from = 0, to = 175000, by = 25000), 
                           limits = c(0, 180000), 
                           expand = c(0, 0),
                           labels = comma_format()) +
        labs(x = NULL, y = "Avg. number of people\nreached on platform", 
             title = NULL,
             caption = NULL, fill = "") +
        coord_fixed(ratio = .01 / 800, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 10, 0, 0), "mm"),
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
                axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                           margin = margin(t = 5, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", hjust = .5,
                                           margin = margin(t = 0, r = 5, b = 0, l = 0)),
                # axis.text.y = element_blank(),
                axis.line.x.bottom = element_line(color = "#333333"),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 0, r = 13, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) +
        guides(fill = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333", keywidth = 1)
       # linetype = guide_legend(keywidth = 4)
       )


# inspect
sen_1_3c_exchanged_story_reach_by_platform_grouped_bar_chart


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(sen_1_3c_exchanged_story_reach_by_platform_grouped_bar_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/sen_1_3c_exchanged_story_reach_by_platform_grouped_bar_chart.docx")


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////

#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# create sen_1_3c_exchanged_story_pct_of_total_reach_by_platform_grouped_bar_chart ####

# check
sen_data %>% filter(indicator_number == "1.3c") %>%
        select(indicator_number, 
               # indicator_name,
               level_2_indicator_name, 
               level_3_indicator_name,
               partner, date_reported, values) %>% print(n = nrow(.))

# check dates
sen_data %>% filter(indicator_number == "1.3c") %>% distinct(date_reported)
sen_data %>% filter(indicator_number == "1.3c") %>%
        distinct(partner, level_2_indicator_name, level_3_indicator_name, date_reported) %>%
        group_by(partner, level_2_indicator_name, level_3_indicator_name) %>% add_count(name = "date_reported_count") %>% 
        slice(1) %>% select(-date_reported) %>%
        ungroup() %>%
        print(n = nrow(.))
# note will drop partners/platform combos with < 4 values, usually for earliest measurement periods, 
# the averages for these are really are not representive and will skew results
sen_data %>% filter(indicator_number == "1.3c", values != 0) %>%
        distinct(partner, level_2_indicator_name, level_3_indicator_name, date_reported) %>%
        group_by(partner, level_2_indicator_name, level_3_indicator_name) %>% add_count(name = "date_reported_count") %>% 
        slice(1) %>% select(-date_reported) %>%
        ungroup() %>%
        filter(date_reported_count > 4) %>%
        print(n = nrow(.))

# inspect exchanges stories
# after dropping partners with < 4 dates, and dropping zero values, there are still enough with valid values to take average
sen_data %>% left_join(sen_data %>% filter(indicator_number == "1.3c", values != 0) %>%
                               distinct(partner, level_2_indicator_name, level_3_indicator_name, date_reported) %>%
                               group_by(partner, level_2_indicator_name, level_3_indicator_name) %>% 
                               add_count(name = "date_reported_count") %>% 
                               slice(1) %>% select(-date_reported) %>%
                               ungroup() %>%
                               filter(date_reported_count > 4), ., 
                       by = c("level_2_indicator_name", "level_3_indicator_name", "partner")) %>%
        filter(indicator_number == "1.3c", level_3_indicator_name == "Exchanged stories") %>% 
        select(level_2_indicator_name, partner, date_reported, values) %>%
        group_by(partner, level_2_indicator_name) %>% add_count(partner) %>% print(n = nrow(.))

# inspect total reach metric, which varies by platform
sen_data %>% filter(indicator_number == "1.3c") %>% distinct(level_3_indicator_name)
sen_data %>% left_join(sen_data %>% filter(indicator_number == "1.3c", values != 0) %>%
                               distinct(partner, level_2_indicator_name, level_3_indicator_name, date_reported) %>%
                               group_by(partner, level_2_indicator_name, level_3_indicator_name) %>% 
                               add_count(name = "date_reported_count") %>% 
                               slice(1) %>% select(-date_reported) %>%
                               ungroup() %>%
                               filter(date_reported_count > 4), ., 
                       by = c("level_2_indicator_name", "level_3_indicator_name", "partner")) %>%
        filter(indicator_number == "1.3c", 
               level_3_indicator_name %in% c("Exchanged stories", "Total reach", "Total tweet impressions",
                                             "Total number of views", "Number of followers")) %>% 
        select(level_2_indicator_name, level_3_indicator_name, partner, date_reported, values) %>%
        group_by(partner, level_2_indicator_name) %>% add_count(partner, name = "partner_count") %>% 
        # print(n = nrow(.))
        distinct(level_2_indicator_name, level_3_indicator_name, partner, partner_count) %>%
        arrange(partner, level_2_indicator_name, level_3_indicator_name) %>%
        print(n = nrow(.))

# get non_zero_multi_month_observations
non_zero_multi_month_observations <- sen_data %>% left_join(sen_data %>% filter(indicator_number == "1.3c", values != 0) %>%
                    distinct(partner, level_2_indicator_name, level_3_indicator_name, date_reported) %>%
                    group_by(partner, level_2_indicator_name, level_3_indicator_name) %>% 
                    add_count(name = "date_reported_count") %>% 
                    slice(1) %>% select(-date_reported) %>%
                    ungroup() %>%
                    filter(date_reported_count > 4), ., 
            by = c("level_2_indicator_name", "level_3_indicator_name", "partner")) %>%
        filter(indicator_number == "1.3c", level_3_indicator_name == "Exchanged stories") %>% 
        select(level_2_indicator_name, partner, date_reported, values) %>%
        rename(exchanged_story_reach = values)
non_zero_multi_month_observations %>% print(n = nrow(.))
non_zero_multi_month_observations %>% distinct(partner, date_reported) %>% print(n = nrow(.))

# update to get non_zero_multi_month_observations_w_total_reach
# note there are 4 observations with non-sensical exchanged_story_reach_pct_of_total > 1, all partner 7, these are set to 1
non_zero_multi_month_observations_w_total_reach <- sen_data %>% filter(indicator_number == "1.3c", 
       level_3_indicator_name %in% c("Total reach", "Total tweet impressions",
                                     "Total number of views", "Number of followers")) %>%
        select(level_2_indicator_name, partner, date_reported, values) %>%
        rename(total_reach = values) %>%
        left_join(non_zero_multi_month_observations, ., by = c("level_2_indicator_name", "partner", "date_reported")) %>%
        mutate(exchanged_story_reach_pct_of_total = exchanged_story_reach / total_reach,
               exchanged_story_reach_pct_of_total = case_when(exchanged_story_reach_pct_of_total > 1 ~ 1,
                                                              TRUE ~ exchanged_story_reach_pct_of_total)) 
non_zero_multi_month_observations_w_total_reach %>% print(n = nrow(.))
non_zero_multi_month_observations_w_total_reach %>% arrange(desc(exchanged_story_reach_pct_of_total))
non_zero_multi_month_observations_w_total_reach %>% arrange(exchanged_story_reach_pct_of_total)
non_zero_multi_month_observations_w_total_reach %>% 
        ggplot(data = ., mapping = aes(x = date_reported, y = exchanged_story_reach_pct_of_total, color = partner)) +
        geom_line() + facet_wrap(facets = vars(level_2_indicator_name))

# inspect chart_data
# get means for each partner and overall; note i add placeholder zeroes so the x-axis groups have same width
non_zero_multi_month_observations_w_total_reach  %>%
        group_by(partner, level_2_indicator_name) %>%
        summarize(values_mean = mean(exchanged_story_reach_pct_of_total)) %>%
        ungroup() %>% arrange(level_2_indicator_name) %>%
        left_join(sen_data %>% filter(indicator_number == "1.3c", level_3_indicator_name == "Exchanged stories") %>%
                          distinct(partner, level_2_indicator_name) %>% expand(nesting(partner), level_2_indicator_name), .,
                  by = c("partner", "level_2_indicator_name")) %>%
        mutate(values_mean = case_when(is.na(values_mean) ~ 0, TRUE ~ values_mean)) %>%
        bind_rows(., non_zero_multi_month_observations_w_total_reach %>%
                          group_by(level_2_indicator_name) %>%
                          summarize(values_mean = mean(exchanged_story_reach_pct_of_total)) %>%
                          ungroup() %>% mutate(partner = "All partners") %>% relocate(partner)) %>%
        print(n = nrow(.))


#////////////////


# check partners
sen_data %>% filter(indicator_number == "1.3c") %>%
        distinct(partner, level_2_indicator_name, level_3_indicator_name) %>%
        group_by(level_2_indicator_name, level_3_indicator_name) %>%
        add_count(name = "partner_count") %>% arrange(level_2_indicator_name, level_3_indicator_name) %>%
        slice(1) %>% select(-partner) %>%
        ungroup() %>% print(n = nrow(.))

# check level_3_indicator_name
sen_data %>% filter(indicator_number == "1.3c") %>%
        distinct(level_2_indicator_name, level_3_indicator_name) %>%
        group_by(level_3_indicator_name) %>%
        add_count(name = "level_2_indicator_name_count") %>% arrange(level_3_indicator_name) %>%
        slice(1) %>% select(-level_2_indicator_name) %>%
        ungroup() %>% print(n = nrow(.))

# check values 
# note will drop zero values since they'll skew the average and almost certainly just missing data
# very unlikely the actual measured value is zero social media reach for random periods
sen_data %>% filter(indicator_number == "1.3c", level_3_indicator_name == "Exchanged stories") %>% 
        select(level_2_indicator_name, partner, date_reported, values) %>% print(n = nrow(.))
sen_data %>% filter(indicator_number == "1.3c", level_3_indicator_name == "Exchanged stories",
                    values == 0) %>% 
        select(level_2_indicator_name, partner, date_reported, values) %>% print(n = nrow(.))

sen_data %>% filter(indicator_number == "1.3c", level_3_indicator_name == "Exchanged stories",
                    values != 0) %>%
        ggplot(data = ., mapping = aes(x = level_2_indicator_name, y = values)) + geom_boxplot()
sen_data %>% filter(indicator_number == "1.3c", level_3_indicator_name == "Exchanged stories",
                    values != 0) %>%
        group_by(level_2_indicator_name) %>% summarize(values_mean = mean(values)) %>%
        ungroup() %>%
        ggplot(data = ., mapping = aes(x = level_2_indicator_name, y = values_mean)) + geom_col()


#/////////////////////////


# add color_bin and color
chart_data <- non_zero_multi_month_observations_w_total_reach  %>%
        group_by(partner, level_2_indicator_name) %>%
        summarize(values_mean = mean(exchanged_story_reach_pct_of_total)) %>%
        ungroup() %>% arrange(level_2_indicator_name) %>%
        left_join(sen_data %>% filter(indicator_number == "1.3c", level_3_indicator_name == "Exchanged stories") %>%
                          distinct(partner, level_2_indicator_name) %>% expand(nesting(partner), level_2_indicator_name), .,
                  by = c("partner", "level_2_indicator_name")) %>%
        mutate(values_mean = case_when(is.na(values_mean) ~ 0, TRUE ~ values_mean)) %>%
        bind_rows(., non_zero_multi_month_observations_w_total_reach %>%
                          group_by(level_2_indicator_name) %>%
                          summarize(values_mean = mean(exchanged_story_reach_pct_of_total)) %>%
                          ungroup() %>% mutate(partner = "All partners") %>% relocate(partner)) %>%
        filter(partner != "Partner 1") %>%
        mutate(color_bin = partner,
               color = case_when(color_bin == "Partner 2" ~ color_palette %>% slice(1) %>% pull(hex),
                                 color_bin == "Partner 3" ~ color_palette %>% slice(2) %>% pull(hex),
                                 color_bin == "Partner 4" ~ color_palette %>% slice(3) %>% pull(hex),
                                 color_bin == "Partner 5" ~ color_palette %>% slice(4) %>% pull(hex),
                                 color_bin == "Partner 6" ~ color_palette %>% slice(5) %>% pull(hex),
                                 color_bin == "Partner 7" ~ color_palette %>% slice(6) %>% pull(hex),
                                 color_bin == "All partners" ~ color_palette %>% slice(7) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list

# get title
title <- "Average number of people reached by SEN-exchanged stories 
on social media platforms, by partner
"


#/////////////////////


# create chart
sen_1_3c_exchanged_story_pct_of_total_reach_by_platform_grouped_bar_chart <- chart_data %>%
        ggplot(data = ., aes(x = factor(level_2_indicator_name, levels = c("Facebook", "Odnoklassniki",
                                                                           "Telegram", "Twitter", "Vkontakte")),
                             y = values_mean, 
                             fill = factor(color_bin, levels = c("Partner 2", "Partner 3",
                                                                 "Partner 4", "Partner 5", "Partner 6", "Partner 7",
                                                                 "All partners")))) + 
        geom_bar(stat = "identity", position = position_dodge(width = .7, preserve = "single"), width = .7) +
        scale_fill_manual(values = chart_data_color_list) +
        scale_x_discrete(expand = c(0, 0)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), 
                           limits = c(0, 1), 
                           expand = c(0, 0),
                           labels = percent_format(accuracy = 1)) +
        labs(x = NULL, y = "Share", 
             title = NULL,
             caption = NULL, fill = "") +
        coord_fixed(ratio = 2 / 1, clip = "off") +
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
                axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                           margin = margin(t = 5, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", hjust = .5,
                                           margin = margin(t = 0, r = 5, b = 0, l = 0)),
                # axis.text.y = element_blank(),
                axis.line.x.bottom = element_line(color = "#333333"),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 0, r = 13, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) +
        guides(fill = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333", keywidth = 1)
               # linetype = guide_legend(keywidth = 4)
        )


# inspect
sen_1_3c_exchanged_story_pct_of_total_reach_by_platform_grouped_bar_chart


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(sen_1_3c_exchanged_story_pct_of_total_reach_by_platform_grouped_bar_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/sen_1_3c_exchanged_story_pct_of_total_reach_by_platform_grouped_bar_chart.docx")


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////



# # dropped: create sen_1_3c_commissioned_story_reach_by_platform_grouped_bar_chart ####
# 
# # note this chart will not be used because there isn't enough non-zero observations for commissioned stories
# # see inspection below
# 
# # check
# sen_data %>% filter(indicator_number == "1.3c") %>%
#         select(indicator_number, 
#                # indicator_name,
#                level_2_indicator_name, 
#                level_3_indicator_name,
#                partner, date_reported, values) %>% print(n = nrow(.))
# 
# # check dates
# sen_data %>% filter(indicator_number == "1.3c") %>% distinct(date_reported)
# sen_data %>% filter(indicator_number == "1.3c") %>%
#         distinct(partner, level_2_indicator_name, level_3_indicator_name, date_reported) %>%
#         group_by(partner, level_2_indicator_name, level_3_indicator_name) %>% add_count(name = "date_reported_count") %>% 
#         slice(1) %>% select(-date_reported) %>%
#         ungroup() %>%
#         print(n = nrow(.))
# # note will drop partners/platform combos with < 4 values, usually for earliest measurement periods, 
# # the averages for these are really are not representive and will skew results
# sen_data %>% filter(indicator_number == "1.3c", values != 0) %>%
#         distinct(partner, level_2_indicator_name, level_3_indicator_name, date_reported) %>%
#         group_by(partner, level_2_indicator_name, level_3_indicator_name) %>% add_count(name = "date_reported_count") %>% 
#         slice(1) %>% select(-date_reported) %>%
#         ungroup() %>%
#         filter(date_reported_count > 4) %>%
#         print(n = nrow(.))
# 
# # even after dropping partners with < 4 dates, and dropping zero values, there are many with valid values
# sen_data %>% left_join(sen_data %>% filter(indicator_number == "1.3c", values != 0) %>%
#                                distinct(partner, level_2_indicator_name, level_3_indicator_name, date_reported) %>%
#                                group_by(partner, level_2_indicator_name, level_3_indicator_name) %>% 
#                                add_count(name = "date_reported_count") %>% 
#                                slice(1) %>% select(-date_reported) %>%
#                                ungroup() %>%
#                                filter(date_reported_count > 4), ., 
#                        by = c("level_2_indicator_name", "level_3_indicator_name", "partner")) %>%
#         filter(indicator_number == "1.3c", level_3_indicator_name == "Commissioned stories") %>% 
#         select(level_2_indicator_name, partner, date_reported, values) %>%
#         group_by(partner, level_2_indicator_name) %>% add_count(partner) %>% print(n = nrow(.))



#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////


# create sen_visitors_exchanged_stories_scatterplot ####

# check 
sen_data %>% filter(indicator_number == "1.3.2a") %>%
        select(indicator_number, indicator_name,
               # level_2_indicator_name, level_3_indicator_name,
               partner, date_reported, values, notes) %>% print(n = nrow(.))

sen_data %>% filter(indicator_number == "1.3b") %>%
        select(indicator_number, indicator_name,
               level_2_indicator_name, 
               # level_3_indicator_name,
               partner, date_reported, values) %>% print(n = nrow(.))

# check dates
# note that 1.3b unique users has monthly data, 1.3.2a had bimonthly
# will use bimonthly data; note i could take bimonthly average for 1.3b as a bonus, 
# but looking at line chart it's pretty steady so wouldn't be a big difference, and i'm short on time
sen_data %>% filter(indicator_number == "1.3.2a") %>% distinct(date_reported)
sen_data %>% filter(indicator_number == "1.3b") %>% distinct(date_reported)

# confirm that more partners have unique visitor data from google analytics than IO analytics
# so will use google analytics data in chart
sen_data %>% filter(indicator_number == "1.3b", level_2_indicator_name == "Google Analytics") %>%
        count(date_reported)
sen_data %>% filter(indicator_number == "1.3b", level_2_indicator_name == "IO Analytics") %>%
        count(date_reported)

# note that early measurement periods were multi-month, later periods were monthly
# varying gaps for early measurement periods: 6, 7, 7, 8 months, so could divide values by 6, 7, 7, 8 respectively
# but that sen spreadsheet explicitly says this metric is collected bimonthly
# though that appears to have changed to monthly for more recent measurements
# so i could downscale the early measurements by dividing by 6, 7, 7, 8; or could divide them by 2
# or could leave it alone and not downscale, which actually results in the smoothest time series 
# maybe the data collectors already downscaled it? that would make sense and explain the clear pattern
# it's very suspicious that if downscaling were correct it would result it uniform sharp jumps in the post-downscale period
# result: will assume the early measurement periods are already downscaled, this looks most accurate, and will flag for nick
sen_data %>% filter(indicator_number == "1.3b", level_2_indicator_name == "Google Analytics") %>% distinct(date_reported)
sen_data %>% filter(indicator_number == "1.3b", level_2_indicator_name == "Google Analytics") %>%
        select(partner, date_reported, values) %>% arrange(partner, date_reported) %>% print(n = nrow(.)) 
sen_data %>% filter(indicator_number == "1.3b", level_2_indicator_name == "Google Analytics") %>%
        ggplot(data = ., mapping = aes(x = date_reported, y = values, color = partner)) + geom_line()
sen_data %>% filter(indicator_number == "1.3b", level_2_indicator_name == "Google Analytics") %>%
        mutate(values_v2 = case_when(date_reported == "2018-03-01" ~ values / 6,
                                     date_reported == "2018-09-01" ~ values / 7,
                                     date_reported == "2019-04-01" ~ values / 7,
                                     date_reported == "2019-12-01" ~ values / 8,
                                     TRUE ~ values)) %>%
        ggplot(data = ., mapping = aes(x = date_reported, y = values_v2, color = partner)) + geom_line()
distinct(partner, date_reported, values, values_v2) %>% arrange(date_reported) %>% print(n = nrow(.))
sen_data %>% filter(indicator_number == "1.3b", level_2_indicator_name == "Google Analytics") %>%
        mutate(values_v2 = case_when(date_reported == "2018-03-01" ~ values / 2,
                                     date_reported == "2018-09-01" ~ values / 2,
                                     date_reported == "2019-04-01" ~ values / 2,
                                     date_reported == "2019-12-01" ~ values / 2,
                                     TRUE ~ values)) %>%
        ggplot(data = ., mapping = aes(x = date_reported, y = values_v2, color = partner)) + geom_line()
distinct(partner, date_reported, values, values_v2) %>% arrange(date_reported) %>% print(n = nrow(.))

# check 1.3b for outliers
# note that partner 6 does have a month with 12m google visitors/30 mil io visitors
# not sure if this is bad data, but the consistency across google/io, and residual effect in sequential months
# indicate it's not a random fat-finger error
# for reference though, home depot got 48 mil visitors in december 2019, so there may be an issue 
# https://www.statista.com/statistics/271450/monthly-unique-visitors-to-us-retail-websites/#:~:text=In%20the%20United%20States%2C%20Amazon,terms%20of%20revenue%20and%20reach.
sen_data %>% filter(indicator_number == "1.3b", level_2_indicator_name == "Google Analytics") %>% skim(values)
sen_data %>% filter(indicator_number == "1.3b", level_2_indicator_name == "Google Analytics") %>%
        ggplot(data = ., mapping = aes(x = partner, y = values)) + geom_boxplot()
sen_data %>% filter(indicator_number == "1.3b", level_2_indicator_name == "Google Analytics") %>%
        ggplot(data = ., mapping = aes(x = date_reported, y = values, color = partner)) + geom_line()


# note that early measurement periods were multi-month, later periods were two months
# varying gaps for early measurement periods: 6, 7, 7, 8 months, so could divide values by 3, 3.5, 3.5, and 4, respectively
# but spreadsheet explicitly says bimonthly measurements, and they later adopt monthly measurements
# so it could be interpreted as being measurments for the past two months, they just didn't get on a steady reporting tempo
# looking at the data, this explanation seems most likely, since it avoids a uniform large jump across all partners after
# the downscaled multi-month periods; 
sen_data %>% filter(indicator_number == "1.3.2a") %>% distinct(date_reported)
sen_data %>% filter(indicator_number == "1.3.2a") %>% 
        ggplot(data = ., mapping = aes(x = date_reported, y = values, color = partner)) + geom_line()
sen_data %>% filter(indicator_number == "1.3.2a") %>%
        mutate(values_v2 = case_when(date_reported == "2018-03-01" ~ values / 3,
                                     date_reported == "2018-09-01" ~ values / 3.5,
                                     date_reported == "2019-04-01" ~ values / 3.5,
                                     date_reported == "2019-12-01" ~ values / 4,
                                     TRUE ~ values)) %>%
        ggplot(data = ., mapping = aes(x = date_reported, y = values_v2, color = partner)) + geom_line()


# inspect chart_data
sen_data %>% filter(indicator_number == "1.3.2a") %>%
        select(indicator_name, partner, date_reported, values) %>%
        pivot_wider(id_cols = c(partner, date_reported), names_from = indicator_name, values_from = values) %>%
        left_join(., sen_data %>% filter(indicator_number == "1.3b", level_2_indicator_name == "Google Analytics") %>%
                          rename(unique_users_per_month = values) %>%
                          select(partner, date_reported, unique_users_per_month),
                  by = c("partner", "date_reported")) %>% 
        filter(partner != "Partner 1") %>%
        print(n = nrow(.))


#///////////////////////


# add color_bin and color
chart_data <- sen_data %>% filter(indicator_number == "1.3.2a") %>%
        select(indicator_name, partner, date_reported, values) %>%
        pivot_wider(id_cols = c(partner, date_reported), names_from = indicator_name, values_from = values) %>%
        left_join(., sen_data %>% filter(indicator_number == "1.3b", level_2_indicator_name == "Google Analytics") %>%
                          rename(unique_users_per_month = values) %>%
                          select(partner, date_reported, unique_users_per_month),
                  by = c("partner", "date_reported")) %>% 
        filter(partner != "Partner 1") %>% mutate(unique_users_per_month = unique_users_per_month / 1000000) %>%
        mutate(color_bin = partner,
               color = case_when(color_bin == "Partner 1" ~ color_palette %>% slice(7) %>% pull(hex),
                                 color_bin == "Partner 2" ~ color_palette %>% slice(1) %>% pull(hex),
                                 color_bin == "Partner 3" ~ color_palette %>% slice(2) %>% pull(hex),
                                 color_bin == "Partner 4" ~ color_palette %>% slice(3) %>% pull(hex),
                                 color_bin == "Partner 5" ~ color_palette %>% slice(4) %>% pull(hex),
                                 color_bin == "Partner 6" ~ color_palette %>% slice(5) %>% pull(hex),
                                 color_bin == "Partner 7" ~ color_palette %>% slice(6) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


# title
title <- "Number of SEN-exchanged stories taken by SEN partners, 
and unique visitors to SEN partner websites (bimonthly)
"


#/////////////////////


# create chart
sen_visitors_exchanged_stories_scatterplot <- chart_data %>% 
        ggplot(data = ., aes(x = monthly_stories_taken_by_partner_for_exchange, 
                             y = unique_users_per_month, 
                             color = factor(color_bin, levels = c("Partner 1", "Partner 2", "Partner 3",
                                                                  "Partner 4", "Partner 5", "Partner 6", "Partner 7")))) + 
        geom_point(alpha = .85, size = 5) + 
        # geom_text_repel(fontface = "bold", point.padding = .3, size = 3.25, segment.alpha = 0) +
        # geom_text(data = chart_data %>% 
        #                   filter(partner == "Partner 2") %>%
        #                   filter(share_of_total_visitors_driven_by_social_media == max(share_of_total_visitors_driven_by_social_media)),
        #           mapping = aes(x = share_of_total_visitors_driven_by_mobile - .14, 
        #                         y = share_of_total_visitors_driven_by_social_media, label = color_bin),
        #           size = 4, fontface = "bold", hjust = 0) +
        scale_color_manual(values = chart_data_color_list) +
        # scale_linetype_manual(values = chart_data_linetype_list, guide = FALSE,
        #                       labels = c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs")) +
        # scale_fill_manual(values = chart_data_color_list) +
        # scale_x_discrete(expand = c(0, 0)) +
        # scale_y_continuous(breaks = seq(from = 0, to = 100, by = 10), limits = c(-10, 110), expand = c(-.05, 0)) +
        # scale_x_continuous(breaks = seq(from = 0, to = 100, by = 10), limits = c(-10, 110), expand = c(-.05, 0)) +
        scale_y_continuous(breaks = seq(from = 0, to = 12, by = 2), limits = c(-.5, 13), expand = c(0, 0),
                           labels = add_number_label(breaks = seq(from = 0, to = 12, by = 2), label = "m")) +
        scale_x_continuous(breaks = seq(from = 0, to = 60, by = 10), limits = c(0, 60), expand = c(0, 0),
                           labels = waiver()) +
        labs(x = "Number of SEN-exchanged stories taken", 
             y = "Unique visitors\nto website (millions)", 
             title = NULL, 
             caption = NULL, color = "") +
        coord_fixed(ratio = 2 / 1, clip = "off") +
        # coord_fixed(ratio = 1/1.5, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 10, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                # panel.grid.major.x = element_line(color = "#333333"),
                panel.grid.major.y = element_blank(),
                # panel.grid.major.y = element_line(color = "#333333"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                           margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                           margin = margin(t = 0, r = 5, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                axis.line.y.left = element_line(color = "#333333"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = -5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 0, r = 13, b = 0, l = 0)),
                plot.title = element_text(size = 18, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.box.margin=margin(0, 0, 0, 10),
                legend.title = element_text(size = 12, family = "Calibri", face = "plain", color = "#333333", hjust = 0),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) +
        # guides(size = guide_legend(override.aes = list(color = "#333333")))
        guides(color = guide_legend(nrow = 1, byrow = TRUE, label.hjust = 0, color = "#333333"))


# inspect
sen_visitors_exchanged_stories_scatterplot


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(sen_visitors_exchanged_stories_scatterplot)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/sen_visitors_exchanged_stories_scatterplot.docx")



#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////



#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////


# create sen_platform_reach_exchanged_stories_scatterplot ####

# check 
sen_data %>% filter(indicator_number == "1.3.2a") %>%
        select(indicator_number, indicator_name,
               # level_2_indicator_name, level_3_indicator_name,
               partner, date_reported, values, notes) %>% print(n = nrow(.))

sen_data %>% filter(indicator_number == "1.3c") %>%
        select(indicator_number, indicator_name,
               level_2_indicator_name, 
               level_3_indicator_name,
               partner, date_reported, values) %>% print(n = nrow(.))

# check dates
# note that 1.3c social media has monthly data, 1.3.2a had bimonthly
# will use bimonthly data; note i could take bimonthly average for 1.3c as a bonus, 
# but probably wouldn't be a big difference, and i'm short on time
sen_data %>% filter(indicator_number == "1.3.2a") %>% distinct(date_reported)
sen_data %>% filter(indicator_number == "1.3c") %>% distinct(date_reported)


# note that early measurement periods were multi-month, later periods were two months
# varying gaps for early measurement periods: 6, 7, 7, 8 months, so could divide values by 3, 3.5, 3.5, and 4, respectively
# but spreadsheet explicitly says bimonthly measurements, and they later adopt monthly measurements
# so it could be interpreted as being measurments for the past two months, they just didn't get on a steady reporting tempo
# looking at the data, this explanation seems most likely, since it avoids a uniform large jump across all partners after
# the downscaled multi-month periods; 
sen_data %>% filter(indicator_number == "1.3.2a") %>% distinct(date_reported)
sen_data %>% filter(indicator_number == "1.3.2a") %>% 
        ggplot(data = ., mapping = aes(x = date_reported, y = values, color = partner)) + geom_line()
sen_data %>% filter(indicator_number == "1.3.2a") %>%
        mutate(values_v2 = case_when(date_reported == "2018-03-01" ~ values / 3,
                                     date_reported == "2018-09-01" ~ values / 3.5,
                                     date_reported == "2019-04-01" ~ values / 3.5,
                                     date_reported == "2019-12-01" ~ values / 4,
                                     TRUE ~ values)) %>%
        ggplot(data = ., mapping = aes(x = date_reported, y = values_v2, color = partner)) + geom_line()


# check partners
sen_data %>% filter(indicator_number == "1.3c") %>%
        distinct(partner, level_2_indicator_name, level_3_indicator_name) %>%
        group_by(level_2_indicator_name, level_3_indicator_name) %>%
        add_count(name = "partner_count") %>% arrange(level_2_indicator_name, level_3_indicator_name) %>%
        slice(1) %>% select(-partner) %>%
        ungroup() %>% print(n = nrow(.))

# check level_3_indicator_name
sen_data %>% filter(indicator_number == "1.3c") %>%
        distinct(level_2_indicator_name, level_3_indicator_name) %>%
        group_by(level_3_indicator_name) %>%
        add_count(name = "level_2_indicator_name_count") %>% arrange(level_3_indicator_name) %>%
        slice(1) %>% select(-level_2_indicator_name) %>%
        ungroup() %>% print(n = nrow(.))

# check values 
sen_data %>% filter(indicator_number == "1.3c", level_3_indicator_name == "Exchanged stories") %>%
        ggplot(data = ., mapping = aes(x = level_2_indicator_name, y = values)) + geom_boxplot()
sen_data %>% filter(indicator_number == "1.3c", level_3_indicator_name == "Exchanged stories") %>%
        group_by(level_2_indicator_name) %>% summarize(values_mean = mean(values)) %>%
        ungroup() %>%
        ggplot(data = ., mapping = aes(x = level_2_indicator_name, y = values_mean)) + geom_col()

# note will drop zero values for platform_reach_for_exchange_stories because for some it's clearly no data entry
# eg partner 6 for twitter; other partners only have zero for a few obs (eg partner 4 has zero for final 3 twitter periods)
# actually having exactly zero users reached seems implausible
sen_data %>% filter(indicator_number == "1.3c", level_3_indicator_name == "Exchanged stories") %>%
        select(level_2_indicator_name, partner, date_reported, values) %>%
        rename(platform_reach_for_exchange_stories = values) %>%
        left_join(., sen_data %>% filter(indicator_number == "1.3.2a") %>%
                          select(partner, date_reported, values) %>% rename(sen_exchanged_stories_taken = values),
                  by = c("partner", "date_reported")) %>%
        filter(partner != "Partner 1") %>%
        filter(platform_reach_for_exchange_stories == 0 | is.na(platform_reach_for_exchange_stories))

# inspect chart_data
sen_data %>% filter(indicator_number == "1.3c", level_3_indicator_name == "Exchanged stories") %>%
        select(level_2_indicator_name, partner, date_reported, values) %>%
        rename(platform_reach_for_exchange_stories = values) %>%
        left_join(., sen_data %>% filter(indicator_number == "1.3.2a") %>%
                          select(partner, date_reported, values) %>% rename(sen_exchanged_stories_taken = values),
                  by = c("partner", "date_reported")) %>%
        filter(partner != "Partner 1") %>%
        filter(platform_reach_for_exchange_stories != 0, !is.na(sen_exchanged_stories_taken)) %>%
        print(n = nrow(.))


#///////////////////////


# add color_bin and color
chart_data <- sen_data %>% filter(indicator_number == "1.3c", level_3_indicator_name == "Exchanged stories") %>%
        select(level_2_indicator_name, partner, date_reported, values) %>%
        rename(platform_reach_for_exchange_stories = values) %>%
        left_join(., sen_data %>% filter(indicator_number == "1.3.2a") %>%
                          select(partner, date_reported, values) %>% rename(sen_exchanged_stories_taken = values),
                  by = c("partner", "date_reported")) %>%
        filter(partner != "Partner 1") %>%
        filter(platform_reach_for_exchange_stories != 0, !is.na(sen_exchanged_stories_taken)) %>%
        mutate(platform_reach_for_exchange_stories = platform_reach_for_exchange_stories / 1000) %>%
        mutate(color_bin = partner,
               color = case_when(color_bin == "Partner 1" ~ color_palette %>% slice(7) %>% pull(hex),
                                 color_bin == "Partner 2" ~ color_palette %>% slice(1) %>% pull(hex),
                                 color_bin == "Partner 3" ~ color_palette %>% slice(2) %>% pull(hex),
                                 color_bin == "Partner 4" ~ color_palette %>% slice(3) %>% pull(hex),
                                 color_bin == "Partner 5" ~ color_palette %>% slice(4) %>% pull(hex),
                                 color_bin == "Partner 6" ~ color_palette %>% slice(5) %>% pull(hex),
                                 color_bin == "Partner 7" ~ color_palette %>% slice(6) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


# title
title <- "Visitors to SEN partner websites"


#/////////////////////


# create chart
sen_platform_reach_exchanged_stories_scatterplot <- chart_data %>% 
        ggplot(data = ., aes(x = sen_exchanged_stories_taken, 
                             y = platform_reach_for_exchange_stories, 
                             color = factor(color_bin, levels = c("Partner 1", "Partner 2", "Partner 3",
                                                                  "Partner 4", "Partner 5", "Partner 6", "Partner 7")))) + 
        geom_point(alpha = 1, size = 2) +
        facet_wrap(facets = vars(level_2_indicator_name), scales = "free") +
        # geom_text_repel(fontface = "bold", point.padding = .3, size = 3.25, segment.alpha = 0) +
        # geom_text(data = chart_data %>% 
        #                   filter(partner == "Partner 2") %>%
        #                   filter(share_of_total_visitors_driven_by_social_media == max(share_of_total_visitors_driven_by_social_media)),
        #           mapping = aes(x = share_of_total_visitors_driven_by_mobile - .14, 
        #                         y = share_of_total_visitors_driven_by_social_media, label = color_bin),
        #           size = 4, fontface = "bold", hjust = 0) +
        scale_color_manual(values = chart_data_color_list) +
        # scale_linetype_manual(values = chart_data_linetype_list, guide = FALSE,
        #                       labels = c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs")) +
        # scale_fill_manual(values = chart_data_color_list) +
        # scale_x_discrete(expand = c(0, 0)) +
        # scale_y_continuous(breaks = seq(from = 0, to = 100, by = 10), limits = c(-10, 110), expand = c(-.05, 0)) +
        # scale_x_continuous(breaks = seq(from = 0, to = 100, by = 10), limits = c(-10, 110), expand = c(-.05, 0)) +
        scale_y_continuous(
                # breaks = seq(from = 0, to = 400000, by = 1000000), 
                #            limits = c(-10000, 400000), 
                # expand = c(0, 0),
                           labels = label_number_si(unit = "k", sep = "")) +
        scale_x_continuous(breaks = seq(from = 0, to = 60, by = 20), limits = c(0, 60), expand = c(0, 0),
                           labels = waiver()) +
        labs(x = "Number of SEN-exchanged stories taken", 
             y = "Number of people reached\nby SEN-exchanged stories on platform", 
             title = NULL, 
             caption = NULL, color = "") +
        # coord_fixed(ratio = .1 / 1000, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 20, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                # panel.grid.major.x = element_line(color = "#333333"),
                panel.grid.major.y = element_blank(),
                # panel.grid.major.y = element_line(color = "#333333"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                strip.background = element_blank(),
                strip.text = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333"),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                           margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                           margin = margin(t = 0, r = 5, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                axis.line.y.left = element_line(color = "#333333"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = -5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 0, r = 13, b = 0, l = 0)),
                plot.title = element_text(size = 18, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.box.margin=margin(0, 0, 0, 10),
                legend.title = element_text(size = 12, family = "Calibri", face = "plain", color = "#333333", hjust = 0),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) +
        # guides(size = guide_legend(override.aes = list(color = "#333333")))
        guides(color = guide_legend(nrow = 1, byrow = TRUE, label.hjust = 0, color = "#333333"))  +
        theme(aspect.ratio = .9 / 1)


# inspect
sen_platform_reach_exchanged_stories_scatterplot


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(sen_platform_reach_exchanged_stories_scatterplot)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/sen_platform_reach_exchanged_stories_scatterplot.docx")



#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# create sen_1_3a_bar_chart ####


# check 
# will assume that 
sen_data %>% filter(indicator_number == "1.3a") %>% 
        arrange(partner) %>%
        select(indicator_number, indicator_name, 
               # level_2_indicator_name, level_3_indicator_name, 
               partner, date_reported, values) %>% print(n = nrow(.))

# will just show bar chart, since it's not clear how/if the multi-month measurements have been downscaled
sen_data %>% filter(indicator_number == "1.3a") %>% 
        ggplot(data = ., mapping = aes(x = factor(date_reported), y = values)) +
        geom_col()

# note that early measurements are multi-month, 
# but since it's an average there's no need to manually split up into monthly measurements for comparability 
sen_data %>% filter(indicator_number == "1.3a") %>% distinct(date_reported)

# add color_bin and color
chart_data <- sen_data %>% 
        filter(indicator_number == "1.3a") %>%
        mutate(date_reported = strftime(x = date_reported, format = "%m/%Y")) %>%
        mutate(color_bin = partner,
               color = case_when(color_bin == "All partners" ~ color_palette %>% slice(1) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list

# get title
title <- "Number of reprints of SEN-commissioned stories by other media platforms"


#/////////////////////


# create chart
sen_1_3a_bar_chart <- chart_data %>%
        ggplot(data = ., aes(x = date_reported, 
                             y = values, 
                             fill = factor(color_bin, levels = c("All partners")))) + 
        geom_col(width = .7) +
        # geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 1"),
        #           mapping = aes(x = date_reported + 20, y = values, label = color_bin),
        #           fontface = "bold", hjust = 0) +
        scale_fill_manual(values = chart_data_color_list, 
                           guide = FALSE,
                           labels = c("All partners")) +
        scale_y_continuous(breaks = seq(from = 0, to = 60, by = 20), limits = c(0, 60), expand = c(0, 0),
                           labels = waiver()) +
        # scale_x_date(breaks = seq(from = ymd("2018-01-01"), to = ymd("2020-12-01"), by = 180),
        #              date_labels = "%m/%Y", expand = c(.1, 0)) +
        labs(x = NULL, y = "Number of reprints\nof SEN-commissioned stories", 
             title = NULL,
             caption = NULL, color = NULL, linetype = NULL) +
        coord_fixed(ratio = 1 / 20, clip = "off") +
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
                axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                           margin = margin(t = 5, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                           margin = margin(t = 0, r = 5, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 0, r = 13, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) 
# guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333", keywidth = 4),
#        linetype = guide_legend(keywidth = 4))


# inspect
sen_1_3a_bar_chart


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(sen_1_3a_bar_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/sen_1_3a_bar_chart.docx")


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////

#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# create sen_2_2b_shared_on_social_media_by_others_line_chart ####


# check 
sen_data %>% filter(indicator_number == "2.2b") %>% 
        arrange(partner) %>%
        select(indicator_number, indicator_name, 
               # level_2_indicator_name, level_3_indicator_name,
               partner, date_reported, values) %>% print(n = nrow(.))

# check indicator_name
sen_data %>% filter(indicator_number == "2.2b", 
                    indicator_name == "count_of_pieces_shared_on_social_media_by_others") %>% distinct(indicator_name)

# check partners
sen_data %>% filter(indicator_number == "2.2b", 
                    indicator_name == "count_of_pieces_shared_on_social_media_by_others") %>% distinct(partner)

# inspect dates
# note the first two measurement periods are multi-month, then later it becomes bimonthly
# it does appear like the early periods have not yet been downscaled
# but even when attempting to downscale, the first measurement period still looks abnormally high
# and the second measurement looks too low
# result: will just drop the first measurement period, and leave the second measurement period and all other periods unchanged
sen_data %>% filter(indicator_number == "2.2b", 
                    indicator_name == "count_of_pieces_shared_on_social_media_by_others") %>% distinct(date_reported)
sen_data %>% filter(indicator_number == "2.2b", 
                    indicator_name == "count_of_pieces_shared_on_social_media_by_others") %>% 
        ggplot(data = ., mapping = aes(x = date_reported, y = values, color = partner)) +
        geom_line()
sen_data %>% filter(indicator_number == "2.2b", 
                    indicator_name == "count_of_pieces_shared_on_social_media_by_others") %>% 
        mutate(values = case_when(date_reported == "2019-04-01" ~ values / 8,
                                  date_reported == "2019-12-01" ~ values / 8,
                                  TRUE ~ values)) %>%
        ggplot(data = ., mapping = aes(x = date_reported, y = values, color = partner)) +
        geom_line()
sen_data %>% filter(indicator_number == "2.2b", 
                    indicator_name == "count_of_pieces_shared_on_social_media_by_others", 
                    date_reported != "2019-04-01") %>% 
        ggplot(data = ., mapping = aes(x = date_reported, y = values, color = partner)) +
        geom_line()



#//////////////////////


# add color_bin and color
chart_data <- sen_data %>% filter(indicator_number == "2.2b", 
                                  indicator_name == "count_of_pieces_shared_on_social_media_by_others",
                                  date_reported != "2019-04-01") %>%
        mutate(color_bin = partner,
               color = case_when(color_bin == "Partner 2" ~ color_palette %>% slice(1) %>% pull(hex),
                                 color_bin == "Partner 3" ~ color_palette %>% slice(2) %>% pull(hex),
                                 color_bin == "Partner 4" ~ color_palette %>% slice(3) %>% pull(hex),
                                 color_bin == "Partner 5" ~ color_palette %>% slice(4) %>% pull(hex),
                                 color_bin == "Partner 6" ~ color_palette %>% slice(5) %>% pull(hex),
                                 color_bin == "Partner 7" ~ color_palette %>% slice(6) %>% pull(hex)),
               linetype_bin = partner,
               linetype = case_when(TRUE ~ "solid"))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list

# create linetype_list for to pass to scale_linetype_manual
chart_data_linetype_list <- chart_data %>% count(linetype_bin, linetype) %>% pull(linetype)
names(chart_data_linetype_list) <- chart_data %>% count(linetype_bin, linetype) %>% pull(linetype_bin)
chart_data_linetype_list

# get title
title <- "Number of SEN-exchanged stories taken and shared by SEN partners, 
which were then further shared by others on social media"


#/////////////////////


# create chart
sen_2_2b_shared_on_social_media_by_others_line_chart <- chart_data %>%
        ggplot(data = ., aes(x = date_reported, 
                             y = values, 
                             color = factor(color_bin, levels = c("Partner 1", "Partner 2", "Partner 3",
                                                                  "Partner 4", "Partner 5", "Partner 6",
                                                                  "Partner 7")),
                             linetype = factor(color_bin, levels = c("Partner 1", "Partner 2", "Partner 3",
                                                                     "Partner 4", "Partner 5", "Partner 6",
                                                                     "Partner 7")))) + 
        geom_line(size = 2) + 
        geom_point(size = 4) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 1"),
                  mapping = aes(x = date_reported + 10, y = values, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 2"),
                  mapping = aes(x = date_reported + 10, y = values - 1.5, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 3"),
                  mapping = aes(x = date_reported + 10, y = values, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 4"),
                  mapping = aes(x = date_reported + 10, y = values - .3, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 5"),
                  mapping = aes(x = date_reported + 10, y = values, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 6"),
                  mapping = aes(x = date_reported + 10, y = values + 2, label = color_bin),
                  fontface = "bold", hjust = 0) +
        geom_text(data = chart_data %>% filter(date_reported == max(date_reported), partner == "Partner 7"),
                  mapping = aes(x = date_reported + 10, y = values, label = color_bin),
                  fontface = "bold", hjust = 0) +
        scale_color_manual(values = chart_data_color_list, 
                           guide = FALSE,
                           labels = c("Partner 1", "Partner 2", "Partner 3",
                                      "Partner 4", "Partner 5", "Partner 6",
                                      "Partner 7")) +
        scale_linetype_manual(values = chart_data_linetype_list, guide = FALSE,
                              labels = c("Partner 1", "Partner 2", "Partner 3",
                                         "Partner 4", "Partner 5", "Partner 6",
                                         "Partner 7")) +
        scale_y_continuous(breaks = seq(from = 0, to = 30, by = 10), limits = c(0, 35), expand = c(0, 0),
                           labels = waiver()) +
        scale_x_date(breaks = chart_data %>% distinct(date_reported) %>% pull(date_reported),
                     date_labels = "%m/%Y", expand = c(.1, 0)) +
        labs(x = NULL, y = "Number of SEN-exchanged stories\nshared by others on social media", 
             title = NULL,
             caption = NULL, color = NULL, linetype = NULL) +
        coord_fixed(ratio = 5 / 1, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 10, 0, 0), "mm"),
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
                axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                           margin = margin(t = 5, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                           margin = margin(t = 0, r = 5, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 0, r = 13, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) 
# guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333", keywidth = 4),
#        linetype = guide_legend(keywidth = 4))


# inspect
sen_2_2b_shared_on_social_media_by_others_line_chart


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(sen_2_2b_shared_on_social_media_by_others_line_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/sen_2_2b_shared_on_social_media_by_others_line_chart.docx")


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////

