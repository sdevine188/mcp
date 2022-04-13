# country_profiles ####


# note the best workflow is to create plot_layout with patchwork, but mainly just to establish output size that will cover entire
# pdf page with out need for any resizing in word before converting to pdf (just horizontal/vertical centering)
# note the pdf page has wider margins than word doc, so in word the plot will look to be over edge of margins, but
# once print to pdf it will be within pdf page boundaries (setting word margins to zero didnt have any effect)
# then all plots/images/text are saved as png, and added with cowplot::draw_image/label
# cowplot gives more control than patchwork::inset_element, since inset_element only works on ggplot objects, not png
# also the coordinates for inset_elements are highly finicky based on adding/subtracting additional elements, requiring lots of tuning
# but cowplot draw_image coordinates stay fixed even when adding/subtracting other images - a huge time saver for adjustments

# to get wider/narrower png shape on page, use combo of aspect.ratio for relative height, but then also 
# adjust draw_image width/height (cropped as needed)
# note that the draw_image x/y coordinates can specify the center, left, or right of image, depending on hjust/vjust arg

# note that manually adding an image or shape to word output caused some kind of issue preventing the banner across top
# from spanning the entire pdf. but if no manual image/shapes are added to word, the banner print correctly to pdf spanning page

# notes on ggsave dpi
# https://largeprinting.com/resources/image-resolution-and-dpi.html
# main takeaway is 300 dpi is the standard for high resoltuion print graphics
# to confirm 300 dpi image on printed page, divide pixel count by 300 dpi, to get pixels/dots per inch
# ggsave has arguments for dpi and height width - the output pixel dimensions will be width * dpi
# so dpi = 300 and width = 6 yields an 1800 x 1800 image file, which can be displayed at 300 dpi on printed page if 6 inch or less
# "So, if you want to print an image that is 1024 × 768 (listed as Width=1024px, Height=768px on a PC), 
# you need to divide each value by 300 to see how many inches you can print at 300 dpi.
# 1024 / 300 = 3.4133 (width)
# 768 / 300 = 2.56 (height)
# So, you could print this 1024px × 768px image at 300 DPI at a size of 3.4133 × 2.56 - 
# any bigger than this, and you risk the image becoming pixellated"

# note that getting rounded rectangle can be problematic in extreme cases because geom_shape has an aspect ratio limit for wide/narrow, 
# and element_rect_round has a weird outcome where it automatically fills the entire background when saved as png
# but the geom_shape aspect.ratio limit is not binding for this use case
# this is just an issue with geom_shape though, geom_rect supports any aspect.ratio
# widening ggsave width just widens the printed white box, but the plot itself still retains its same aspect ratio


#////////////////////////////////////////////////////////////////////////////////////////////////////


# create get_country_profile()
get_country_profile <- function(current_country) {
        
        print(current_country)
        
        # get current_country_full_name
        current_country_full_name <- case_when(current_country == "BiH" ~ "Bosnia and\nHerzegovina",
                                               current_country == "N. Macedonia" ~ "North\nMacedonia",
                                               current_country == "U.K." ~ "United\nKingdom",
                                               TRUE ~ current_country)
        
        #////////////////////////////////////////////////////////////////////////////////////////////////////
        #////////////////////////////////////////////////////////////////////////////////////////////////////
        #////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # create get_country_profile_radar_chart() ####
        
        # get custom radarchart2, which takes vlabcol and vlab_fontface arg
        # https://stackoverflow.com/questions/54185029/change-labels-colors-in-r-radarchart
        # https://r-charts.com/ranking/radar-chart/
        
        # updated workflow to save radarchart as ggplot w/ emf:
        # 1) use recordPlot to get last plot as saved object
        # 2) use ggplotify::as.ggplot() to wrap cowplot::as_grob to save as ggplot object, then save as normal
        # https://stackoverflow.com/questions/29583849/save-a-plot-in-an-object
        # https://cran.r-project.org/web/packages/ggplotify/vignettes/ggplotify.html
        # https://rdrr.io/cran/cowplot/man/as_grob.html
        
        # create radar chart and save as ggplot
        # grid.newpage()
        # radarchart2(df = chart_data, country = current_country)
        # y <- recordPlot()
        # z <- as.ggplot(as_grob(y))
        # 
        # # optional use of patchwork to combine ggplots
        # x <- starwars %>% count(homeworld) %>% arrange(desc(n)) %>% slice(1:10) %>%
        #         ggplot(data = ., mapping = aes(x = fct_reorder(.f = homeworld, .x = n, .desc = FALSE), y = n)) + geom_col()
        # p <- z + x
        # p
        # 
        # # combine multiple radar charts
        # q <- z + z + z
        # 
        # # save chart as emf
        # filename <- tempfile(fileext = ".emf")
        # emf(file = filename)
        # print(q)
        # dev.off()
        # 
        # # add emf to word doc
        # read_docx() %>%
        #         body_add_img(src = filename, width = 6, height = 6) %>%
        #         print(target = "output/charts/p.docx")
        
        
        #/////////////////////////
        
        
        get_country_profile_radar_chart <- function (df, country = NULL, axistype = 0, seg = 4, pty = 16, pcol = 1:8, plty = 1:6, 
                                                     plwd = 1, pdensity = NULL, pangle = 45, pfcol = NA, cglty = 3, 
                                                     cglwd = 1, cglcol = "navy", axislabcol = "blue", vlabcol = "black", vlab_fontface = 1, title = "", 
                                                     maxmin = TRUE, na.itp = TRUE, centerzero = FALSE, vlabels = NULL, 
                                                     vlcex = NULL, caxislabels = NULL, calcex = NULL, paxislabels = NULL, 
                                                     palcex = NULL, ...) {
                
                # set default parameters
                axistype = 1 
                
                # point symbol
                pty = 32
                
                # color for outside of plotted rings
                pcol = c("#083D7F", "#EF6712")
                
                # color for inside of plotted rings
                pfcol = c(NA, "#EF671270")
                
                # width of plotted rings
                # plwd = 2
                plwd = 12
                
                # line type for plotted rings
                plty = 1
                
                # number of axis segments between center and outer rim
                seg = 5 
                
                # line color for grid segments
                cglcol = "#999999"
                
                # line type for grid segments
                cglty = 1 
                
                # color of axis grid segment labels
                axislabcol = "#333333"
                
                # values for axis grid segment labels
                caxislabels = c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0")
                
                # line width for axis grid segments
                # cglwd = 0.8
                cglwd = 3
                
                # font color for outer labels
                vlabcol = "#333333"
                
                # font face for outer labels
                vlab_fontface = 1
                
                # font magnification for outer labels
                # vlcex = .75
                vlcex = 2
                
                # font magnification for center labels
                # calcex = .75
                calcex = 2
                
                # font magnification for peripheral labels
                # palcex = .75
                palcex = 2
                
                # set font as parameter; 1 is normal, 2 is bold, 3 is italic
                current_parameters <- par(family = "Calibri", font = 1)
                par(current_parameters)
                
                
                #///////////////////////////////////////////////////////////////////////////////////////////////////////
                
                
                if (!is.data.frame(df)) {
                        cat("The data must be given as dataframe.\n")
                        return()
                }
                if ((n <- length(df)) < 3) {
                        cat("The number of variables must be 3 or more.\n")
                        return()
                }
                if (maxmin == FALSE) {
                        dfmax <- apply(df, 2, max)
                        dfmin <- apply(df, 2, min)
                        df <- rbind(dfmax, dfmin, df)
                }
                plot(c(-1.2, 1.2), c(-1.2, 1.2), type = "n", frame.plot = FALSE, 
                     axes = FALSE, xlab = "", ylab = "", main = title, asp = 1, 
                     ...)
                theta <- seq(90, 450, length = n + 1) * pi/180
                theta <- theta[1:n]
                xx <- cos(theta)
                yy <- sin(theta)
                CGap <- ifelse(centerzero, 0, 1)
                for (i in 0:seg) {
                        polygon(xx * (i + CGap)/(seg + CGap), yy * (i + CGap)/(seg + 
                                                                                       CGap), lty = cglty, lwd = cglwd, border = cglcol)
                        if (axistype == 1 | axistype == 3) 
                                CAXISLABELS <- paste(i/seg * 100, "(%)")
                        if (axistype == 4 | axistype == 5) 
                                CAXISLABELS <- sprintf("%3.2f", i/seg)
                        if (!is.null(caxislabels) & (i < length(caxislabels))) 
                                CAXISLABELS <- caxislabels[i + 1]
                        if (axistype == 1 | axistype == 3 | axistype == 4 | 
                            axistype == 5) {
                                if (is.null(calcex)) 
                                        text(-0.05, (i + CGap)/(seg + CGap), CAXISLABELS, 
                                             col = axislabcol)
                                else text(-0.05, (i + CGap)/(seg + CGap), CAXISLABELS, 
                                          col = axislabcol, cex = calcex)
                        }
                }
                if (centerzero) {
                        arrows(0, 0, xx * 1, yy * 1, lwd = cglwd, lty = cglty, 
                               length = 0, col = cglcol)
                }
                else {
                        arrows(xx/(seg + CGap), yy/(seg + CGap), xx * 1, yy * 
                                       1, lwd = cglwd, lty = cglty, length = 0, col = cglcol)
                }
                PAXISLABELS <- df[1, 1:n]
                if (!is.null(paxislabels)) 
                        PAXISLABELS <- paxislabels
                if (axistype == 2 | axistype == 3 | axistype == 5) {
                        if (is.null(palcex)) 
                                text(xx[1:n], yy[1:n], PAXISLABELS, col = axislabcol)
                        else text(xx[1:n], yy[1:n], PAXISLABELS, col = axislabcol, 
                                  cex = palcex)
                }
                VLABELS <- colnames(df)
                if (!is.null(vlabels)) 
                        VLABELS <- vlabels
                if (is.null(vlcex)) 
                        text(xx * 1.2, yy * 1.2, VLABELS, col = vlabcol, font = vlab_fontface)
                else text(xx * 1.2, yy * 1.2, VLABELS, cex = vlcex, col = vlabcol, font = vlab_fontface)
                series <- length(df[[1]])
                SX <- series - 2
                if (length(pty) < SX) {
                        ptys <- rep(pty, SX)
                }
                else {
                        ptys <- pty
                }
                if (length(pcol) < SX) {
                        pcols <- rep(pcol, SX)
                }
                else {
                        pcols <- pcol
                }
                if (length(plty) < SX) {
                        pltys <- rep(plty, SX)
                }
                else {
                        pltys <- plty
                }
                if (length(plwd) < SX) {
                        plwds <- rep(plwd, SX)
                }
                else {
                        plwds <- plwd
                }
                if (length(pdensity) < SX) {
                        pdensities <- rep(pdensity, SX)
                }
                else {
                        pdensities <- pdensity
                }
                if (length(pangle) < SX) {
                        pangles <- rep(pangle, SX)
                }
                else {
                        pangles <- pangle
                }
                if (length(pfcol) < SX) {
                        pfcols <- rep(pfcol, SX)
                }
                else {
                        pfcols <- pfcol
                }
                for (i in 3:series) {
                        xxs <- xx
                        yys <- yy
                        scale <- CGap/(seg + CGap) + (df[i, ] - df[2, ])/(df[1, 
                        ] - df[2, ]) * seg/(seg + CGap)
                        if (sum(!is.na(df[i, ])) < 3) {
                                cat(sprintf("[DATA NOT ENOUGH] at %d\n%g\n", i, 
                                            df[i, ]))
                        }
                        else {
                                for (j in 1:n) {
                                        if (is.na(df[i, j])) {
                                                if (na.itp) {
                                                        left <- ifelse(j > 1, j - 1, n)
                                                        while (is.na(df[i, left])) {
                                                                left <- ifelse(left > 1, left - 1, n)
                                                        }
                                                        right <- ifelse(j < n, j + 1, 1)
                                                        while (is.na(df[i, right])) {
                                                                right <- ifelse(right < n, right + 1, 
                                                                                1)
                                                        }
                                                        xxleft <- xx[left] * CGap/(seg + CGap) + 
                                                                xx[left] * (df[i, left] - df[2, left])/(df[1, 
                                                                                                           left] - df[2, left]) * seg/(seg + CGap)
                                                        yyleft <- yy[left] * CGap/(seg + CGap) + 
                                                                yy[left] * (df[i, left] - df[2, left])/(df[1, 
                                                                                                           left] - df[2, left]) * seg/(seg + CGap)
                                                        xxright <- xx[right] * CGap/(seg + CGap) + 
                                                                xx[right] * (df[i, right] - df[2, right])/(df[1, 
                                                                                                              right] - df[2, right]) * seg/(seg + 
                                                                                                                                                    CGap)
                                                        yyright <- yy[right] * CGap/(seg + CGap) + 
                                                                yy[right] * (df[i, right] - df[2, right])/(df[1, 
                                                                                                              right] - df[2, right]) * seg/(seg + 
                                                                                                                                                    CGap)
                                                        if (xxleft > xxright) {
                                                                xxtmp <- xxleft
                                                                yytmp <- yyleft
                                                                xxleft <- xxright
                                                                yyleft <- yyright
                                                                xxright <- xxtmp
                                                                yyright <- yytmp
                                                        }
                                                        xxs[j] <- xx[j] * (yyleft * xxright - yyright * 
                                                                                   xxleft)/(yy[j] * (xxright - xxleft) - 
                                                                                                    xx[j] * (yyright - yyleft))
                                                        yys[j] <- (yy[j]/xx[j]) * xxs[j]
                                                }
                                                else {
                                                        xxs[j] <- 0
                                                        yys[j] <- 0
                                                }
                                        }
                                        else {
                                                xxs[j] <- xx[j] * CGap/(seg + CGap) + xx[j] * 
                                                        (df[i, j] - df[2, j])/(df[1, j] - df[2, 
                                                                                             j]) * seg/(seg + CGap)
                                                yys[j] <- yy[j] * CGap/(seg + CGap) + yy[j] * 
                                                        (df[i, j] - df[2, j])/(df[1, j] - df[2, 
                                                                                             j]) * seg/(seg + CGap)
                                        }
                                }
                                if (is.null(pdensities)) {
                                        polygon(xxs, yys, lty = pltys[i - 2], lwd = plwds[i - 
                                                                                                  2], border = pcols[i - 2], col = pfcols[i - 
                                                                                                                                                  2])
                                }
                                else {
                                        polygon(xxs, yys, lty = pltys[i - 2], lwd = plwds[i - 
                                                                                                  2], border = pcols[i - 2], density = pdensities[i - 
                                                                                                                                                          2], angle = pangles[i - 2], col = pfcols[i - 
                                                                                                                                                                                                           2])
                                }
                                points(xx * scale, yy * scale, pch = ptys[i - 2],
                                       col = pcols[i - 2])
                        }
                }
                
                
                #//////////////////////////////////////////////////////////////////////////////////////////////
                
                
                # set default legend
                legend(x = .6, y = 1.2, legend = c("EU-15", country), bty = "n",
                       pch = 20, col = c("#083D7F", "#EF6712") , text.col = "#333333", cex = 2, pt.cex = 6)
                
        }
        
        
        #////////////////////////////////////////////////////////////////////////////////////////////////////
        #////////////////////////////////////////////////////////////////////////////////////////////////////
        #////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # create obj_radar_charts ####
        
        
        print(str_c(current_country, " radar chart"))
        
        # get eu_obj_avg 
        # note that the only NA obj_avg for EU is luxembourg for obj_3 in 2018
        eu_obj_avg <- fmir %>% filter(mcp_grouping == "EU-15", year == 2020) %>% 
                distinct(country, year, obj_num, obj_avg) %>%
                arrange(country, obj_num) %>% 
                group_by(obj_num, year) %>%
                mutate(obj_avg_for_eu = mean(obj_avg, na.rm = TRUE)) %>%
                slice(1) %>% 
                ungroup() %>% select(-c(country, obj_avg)) 
        
        
        #//////////////////////
        
        
        # inspect
        # eu_obj_avg
        # eu_obj_avg %>% nrow() # 5
        # eu_obj_avg %>% ncol() # 3
        
        # test eu_obj_avg
        test_eu_obj_avg <- function() {
                
                # test obj_1
                expect_equal(object = eu_obj_avg %>% filter(obj_num == "obj_1", year == 2020) %>% pull(obj_avg_for_eu),
                             expected = fmir %>% filter(obj_num == "obj_1", year == 2020, mcp_grouping == "EU-15") %>% 
                                     distinct(country, year, obj_num, obj_avg) %>% 
                                     summarize(obj_avg_for_eu = mean(obj_avg, na.rm = TRUE)) %>% pull(obj_avg_for_eu))  
                
                # test obj_2
                expect_equal(object = eu_obj_avg %>% filter(obj_num == "obj_2", year == 2020) %>% pull(obj_avg_for_eu),
                             expected = fmir %>% filter(obj_num == "obj_2", year == 2020, mcp_grouping == "EU-15") %>% 
                                     distinct(country, year, obj_num, obj_avg) %>% 
                                     summarize(obj_avg_for_eu = mean(obj_avg, na.rm = TRUE)) %>% pull(obj_avg_for_eu))
                
                # test obj_3
                expect_equal(object = eu_obj_avg %>% filter(obj_num == "obj_3", year == 2020) %>% pull(obj_avg_for_eu),
                             expected = fmir %>% filter(obj_num == "obj_3", year == 2020, mcp_grouping == "EU-15") %>% 
                                     distinct(country, year, obj_num, obj_avg) %>% 
                                     summarize(obj_avg_for_eu = mean(obj_avg, na.rm = TRUE)) %>% pull(obj_avg_for_eu))
                
                # test obj_4
                expect_equal(object = eu_obj_avg %>% filter(obj_num == "obj_4", year == 2020) %>% pull(obj_avg_for_eu),
                             expected = fmir %>% filter(obj_num == "obj_4", year == 2020, mcp_grouping == "EU-15") %>% 
                                     distinct(country, year, obj_num, obj_avg) %>% 
                                     summarize(obj_avg_for_eu = mean(obj_avg, na.rm = TRUE)) %>% pull(obj_avg_for_eu))
                
                # test obj_c
                expect_equal(object = eu_obj_avg %>% filter(obj_num == "obj_c", year == 2020) %>% pull(obj_avg_for_eu),
                             expected = fmir %>% filter(obj_num == "obj_c", year == 2020, mcp_grouping == "EU-15") %>% 
                                     distinct(country, year, obj_num, obj_avg) %>% 
                                     summarize(obj_avg_for_eu = mean(obj_avg, na.rm = TRUE)) %>% pull(obj_avg_for_eu))
        }
        test_eu_obj_avg()
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # country_profile_radar_chart ####
        current_country_obj <- fmir %>% filter(country == current_country, year == 2020) %>% 
                distinct(country, year, obj_num, obj_avg) %>%
                arrange(obj_num) %>%
                select(obj_num, obj_avg) %>%
                pivot_wider(id_cols = NULL, names_from = obj_num, values_from = obj_avg)
        
        # current_country_obj
        
        # get chart_data
        chart_data <- tibble(obj_1 = c(1, 0), 
                             obj_2 = c(1, 0),
                             obj_3 = c(1, 0),
                             obj_4 = c(1, 0),
                             obj_c = c(1, 0)) %>%
                bind_rows(., eu_obj_avg %>% select(-year) %>% 
                                  pivot_wider(id_cols = NULL, names_from = obj_num, values_from = "obj_avg_for_eu")) %>%
                bind_rows(., current_country_obj) %>%
                rename("Democratic" = obj_1, "Information   \n" = obj_2, "Energy" = obj_3, 
                       "Economic" = obj_4, "   Corruption\n" = obj_c)
        
        # chart_data
        
        
        #//////////////////////////
        
        
        # save plot as png
        png(filename = str_c("output/charts/country_profile/country_profile_radar_chart_", str_to_lower(current_country), ".png"),
            width = 12, height = 8, units = "in", res = 300)
        get_country_profile_radar_chart(df = chart_data, country = current_country)
        dev.off()
        
        # load png, crop, and resave as png
        # image_read(str_c("output/charts/country_profile/country_profile_radar_chart_",
        #                  str_to_lower(current_country), ".png")) %>% image_info() # w = 2100, h = 2100 (dpi * width)
        country_profile_radar_chart_png <- image_read(str_c("output/charts/country_profile/country_profile_radar_chart_", 
                                                            str_to_lower(current_country), ".png")) %>%
                image_crop(image = ., geometry = geometry_area(width = 2400, height = 1700, x_off = 700, y_off = 250)) 
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # country_profile_dot_plots ####
        
        print(str_c(current_country, " dot_plots"))
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # country_profile_dot_plot_legend ####
        
        current_sub_obj_num <- "sub_obj_1_1"
        current_sub_obj_short_name <- fmir %>% filter(sub_obj_num == current_sub_obj_num) %>% distinct(sub_obj_short_name) %>%
                mutate(sub_obj_short_name = case_when(sub_obj_short_name == "Checks and balances and rule of law" ~
                                                              "Checks and balances\nand rule of law",
                                                      TRUE ~ sub_obj_short_name)) %>% pull(sub_obj_short_name)
        
        # get chart_data
        chart_data <- fmir %>% 
                filter(year %in% c(2020, 2019, 2018), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>% 
                select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg) %>% distinct() %>%
                bind_rows(., fmir %>% filter(year %in% c(2020, 2019, 2018), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>%
                                  group_by(mcp_grouping) %>% mutate(avg_sub_obj_avg = mean(sub_obj_avg, na.rm = TRUE)) %>%
                                  ungroup() %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg, avg_sub_obj_avg) %>%
                                  arrange(mcp_grouping) %>%
                                  select(-c(country, sub_obj_avg)) %>% 
                                  mutate(country = mcp_grouping) %>%
                                  rename(sub_obj_avg = avg_sub_obj_avg) %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg) %>%
                                  select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg)) %>%
                mutate(y_axis_order = case_when(year == 2018 ~ 1, 
                                                year == 2020 ~ 2,
                                                year == 2019 ~ 3),
                       sub_obj_avg = case_when(y_axis_order == 2 ~ sub_obj_avg,
                                                        TRUE ~ NA_real_),
                       var = mcp_grouping, values = sub_obj_avg,
                       dot_label = case_when(country == current_country ~ str_c(country, "  "),
                                             country %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", 
                                                            "CARs", "EU-15") ~ str_c(mcp_grouping, "  "),
                                             TRUE ~ "Other countries  "),
                       dot_label_plot_order = case_when(dot_label == "Other countries  " ~ 1,
                                                        dot_label == "EU-15  " ~ 2,
                                                        dot_label == "CARs  " ~ 3,
                                                        dot_label == "E&E graduates  " ~ 4,
                                                        dot_label == "E&E Eurasia  " ~ 5,
                                                        dot_label == "E&E Balkans  " ~ 6,
                                                        dot_label == str_c(current_country, "    ") ~ 7)) %>%
                arrange(dot_label_plot_order) %>%
                mutate(color_bin = dot_label,
                       color = case_when(color_bin == str_c(current_country, "  ") ~ color_palette %>% slice(11) %>% pull(hex),
                                         color_bin == "E&E Balkans  " ~ color_palette %>% slice(1) %>% pull(hex),
                                         color_bin == "E&E Eurasia  " ~ color_palette %>% slice(2) %>% pull(hex),
                                         color_bin == "E&E graduates  " ~ color_palette %>% slice(3) %>% pull(hex),
                                         color_bin == "CARs  " ~ color_palette %>% slice(5) %>% pull(hex),
                                         # color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                         color_bin == "EU-15  " ~ color_palette %>% slice(7) %>% pull(hex),
                                         # color_bin == "U.S." ~ color_palette %>% slice(8) %>% pull(hex)),
                                         color_bin == "Other countries  " ~ color_palette %>% slice(4) %>% pull(hex)),
                       point_size = case_when(
                                              TRUE ~ 3),
                       point_shape = case_when(color_bin == "Other countries  " ~ 1,
                                               TRUE ~ 19),
                       point_stroke = case_when(color_bin == "Other countries  " ~ 1,
                                                TRUE ~ 1))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == str_c(current_country, "  ") ~ 1,
                                         color_bin == "E&E Balkans  " ~ 2,
                                         color_bin == "E&E Eurasia  " ~ 3,
                                         color_bin == "E&E graduates  " ~ 4,
                                         color_bin == "CARs  " ~ 5,
                                         color_bin == "EU-15  " ~ 6,
                                         color_bin == "Other countries  " ~ 7)) %>%
                arrange(level) %>%
                pull(color)
        
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == str_c(current_country, "  ") ~ 1,
                                         color_bin == "E&E Balkans  " ~ 2,
                                         color_bin == "E&E Eurasia  " ~ 3,
                                         color_bin == "E&E graduates  " ~ 4,
                                         color_bin == "CARs  " ~ 5,
                                         color_bin == "EU-15  " ~ 6,
                                         color_bin == "Other countries  " ~ 7)) %>%
                arrange(level) %>%
                pull(color_bin)
        
        # inspect
        # chart_data_color_list
        
        
        # create chart_data_shape_list for to pass to scale_shape_manual
        chart_data_shape_list <- chart_data %>% count(color_bin, point_shape) %>% 
                mutate(level = case_when(color_bin == str_c(current_country, "  ") ~ 1,
                                         color_bin == "E&E Balkans  " ~ 2,
                                         color_bin == "E&E Eurasia  " ~ 3,
                                         color_bin == "E&E graduates  " ~ 4,
                                         color_bin == "CARs  " ~ 5,
                                         color_bin == "EU-15  " ~ 6,
                                         color_bin == "Other countries  " ~ 7)) %>%
                arrange(level) %>%
                pull(point_shape)
        
        names(chart_data_shape_list) <- chart_data %>% count(color_bin, point_shape) %>% 
                mutate(level = case_when(color_bin == str_c(current_country, "  ") ~ 1,
                                         color_bin == "E&E Balkans  " ~ 2,
                                         color_bin == "E&E Eurasia  " ~ 3,
                                         color_bin == "E&E graduates  " ~ 4,
                                         color_bin == "CARs  " ~ 5,
                                         color_bin == "EU-15  " ~ 6,
                                         color_bin == "Other countries  " ~ 7)) %>%
                arrange(level) %>%
                pull(color_bin)
        
        # inspect
        # chart_data_shape_list
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        country_profile_dot_plot_legend <- chart_data %>% 
                ggplot(data = ., mapping = aes(x = values, y = y_axis_order, 
                                               color = color_bin, shape = color_bin)) + 
                scale_color_manual(values = chart_data_color_list, 
                                   name = "",
                                   labels = names(chart_data_color_list)) +
                # scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
                scale_y_continuous(breaks = NULL, limits = c(1, 3), expand = c(0, 0),
                                   labels = number_format(accuracy = 1)) +
                # scale_x_continuous(breaks = seq(from = 2010, to = 2020, by = 2)) +
                labs(x = NULL, y = current_sub_obj_short_name, 
                     caption = NULL, color = "test", linetype = "") +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = -.02,
                #                            y = 1, yend = 3), color = "#DDDDDD", size = .25) +
                geom_segment(data = chart_data,
                             mapping = aes(x = -.02, xend = 1,
                                           y = 1, yend = 1), color = "#DDDDDD", size = .25) +
                geom_point(mapping = aes(size = point_size, stroke = point_stroke)) +
                scale_shape_manual(values = chart_data_shape_list, 
                                   name = "",
                                   labels = names(chart_data_shape_list)) +
                # scale_size_continuous(range = c(1, 3), guide = "none") +
                scale_size_continuous(range = c(4, 6), guide = "none") +
                coord_fixed(ratio = 1 / 15, clip = "off") +
                theme_bw() +
                theme(
                        # plot.background = element_rect(fill = "blue"),
                        # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                        plot.margin = unit(c(0, 0, 0, 0), "mm"),
                        plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                                    color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                        # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        # panel.grid.major.y = element_line(color = "#DDDDDD"),
                        panel.grid.major.y = element_blank(),
                        panel.border = element_blank(),
                        # panel.border = element_rect(color = "#DDDDDD", size = .5),
                        # panel.grid = element_blank(),
                        # line = element_blank(),
                        # rect = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.ticks.x = element_line(size = .5, color = "#DDDDDD"),
                        # axis.ticks.length.y.left = unit(.2, "cm"),
                        axis.ticks.length.x.bottom = unit(.1, "cm"),
                        axis.text.x = element_text(family = "Calibri", face = "plain", size = 7, color = "#333333", 
                                                   margin = margin(t = 2, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                        axis.text.y = element_text(family = "Calibri", face = "plain", size = 7, color = "#333333", angle = 0,
                                                   margin = margin(t = 0, r = -20, b = 0, l = 0)),
                        # axis.line.x.bottom = element_line(color = "#333333"),
                        axis.line.x.bottom = element_blank(),
                        axis.line.y.left = element_blank(),
                        axis.title.x = element_text(family = "Calibri", face = "plain", size = 7, color = "#333333", 
                                                    margin = margin(t = 13, r = 0, b = 5, l = 0)),
                        axis.title.y = element_text(family = "Calibri", face = "plain", size = 7, color = "#333333", angle = 0,
                                                    vjust = .5, margin = margin(t = 0, r = 0, b = 0, l = 0)),
                        # axis.title.y = element_blank(),
                        # axis.text.y = element_blank(),
                        plot.title = element_text(size = 7, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                                  margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                        legend.position = "bottom",
                        # legend.key.size = unit(2, "mm"), 
                        legend.title = element_text(size = 8, family = "Calibri", face = "bold", color = "#083D7F"),
                        # legend.title = element_blank(),
                        legend.text = element_text(size = 8.5, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                   hjust = .5, color = "#333333")
                        # legend.spacing.y = unit(5.5, "cm"),
                        # legend.key = element_rect(size = 5),
                        # legend.key.size = unit(2, 'lines')
                ) +
                guides(color = guide_legend(nrow = 2, byrow = TRUE, 
                                            label.hjust = 0,
                                            color = "#333333", 
                                            keywidth = .5,
                                            override.aes = list(size = 5)
                )
                # linetype = guide_legend(keywidth = 1)
                )
        
        # inspect
        # country_profile_dot_plot_legend
        
        
        #/////////////////////
        
        
        # extract legend
        country_profile_dot_plot_legend <- as_ggplot(get_legend(country_profile_dot_plot_legend))
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/country_profile_dot_plot_legend.png"), 
               plot = country_profile_dot_plot_legend, 
               dpi = 600, height = 6, width = 6)
        
        # crop png
        # image_read("output/charts/country_profile/country_profile_dot_plot_legend.png") %>% image_info()
        country_profile_dot_plot_legend_png <- image_read("output/charts/country_profile/country_profile_dot_plot_legend.png") %>%
                image_crop(image = ., geometry = geometry_area(width = 2750, height = 400, x_off = 390, y_off = 1600))
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # miri_country_profile_dot_plot ####
        name_placeholder <- str_c(rep(x = " ", times = 32), collapse = "")
        
        
        # get chart_data
        chart_data <- fmir %>% 
                filter(year %in% c(2020, 2019, 2018), !(country %in% c("Russia", "U.S."))) %>% 
                select(country, mcp_grouping, year, miri_avg) %>% distinct() %>%
                bind_rows(., fmir %>% filter(year %in% c(2020, 2019, 2018),
                                             !(country %in% c("Russia", "U.S."))) %>%
                                  group_by(mcp_grouping, year) %>% mutate(avg_miri_avg = mean(miri_avg, na.rm = TRUE)) %>%
                                  ungroup() %>%
                                  distinct(country, mcp_grouping, year, 
                                           miri_avg, avg_miri_avg) %>%
                                  arrange(mcp_grouping) %>%
                                  select(-c(country, miri_avg)) %>% 
                                  mutate(country = mcp_grouping) %>%
                                  rename(miri_avg = avg_miri_avg) %>%
                                  distinct(country, mcp_grouping, year,  
                                           miri_avg) %>%
                                  select(country, mcp_grouping, year, miri_avg)) %>%
                mutate(y_axis_order = case_when(year == 2018 ~ 1, 
                                                year == 2020 ~ 2,
                                                year == 2019 ~ 3),
                       miri_avg = case_when(y_axis_order == 2 ~ miri_avg,
                                            TRUE ~ NA_real_),
                       var = mcp_grouping, values = miri_avg,
                       dot_label = case_when(country == current_country ~ country,
                                             country %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs", "EU-15") ~ mcp_grouping,
                                             TRUE ~ "Other"),
                       dot_label_plot_order = case_when(dot_label == "Other" ~ 1,
                                                        dot_label == "EU-15" ~ 2,
                                                        dot_label == "CARs" ~ 3,
                                                        dot_label == "E&E graduates" ~ 4,
                                                        dot_label == "E&E Eurasia" ~ 5,
                                                        dot_label == "E&E Balkans" ~ 6,
                                                        dot_label == current_country ~ 7)) %>%
                arrange(dot_label_plot_order) %>%
                mutate(color_bin = dot_label,
                       color = case_when(color_bin == current_country ~ color_palette %>% slice(11) %>% pull(hex),
                                         color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                         color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                         color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                         color_bin == "CARs" ~ color_palette %>% slice(5) %>% pull(hex),
                                         # color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                         color_bin == "EU-15" ~ color_palette %>% slice(7) %>% pull(hex),
                                         # color_bin == "U.S." ~ color_palette %>% slice(8) %>% pull(hex)),
                                         color_bin == "Other" ~ color_palette %>% slice(4) %>% pull(hex)),
                       point_size = case_when(color_bin == "Other" ~ 2,
                                              TRUE ~ 3),
                       point_shape = case_when(color_bin == "Other" ~ 1,
                                               TRUE ~ 19),
                       point_stroke = case_when(color_bin == "Other" ~ 1,
                                                TRUE ~ 1))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color)
        
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color_bin)
        
        # inspect
        # chart_data_color_list
        
        
        # create chart_data_shape_list for to pass to scale_shape_manual
        chart_data_shape_list <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(point_shape)
        
        names(chart_data_shape_list) <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(color_bin)
        
        # inspect
        # chart_data_shape_list
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        miri_country_profile_dot_plot <- chart_data %>% 
                ggplot(data = ., mapping = aes(x = values, y = y_axis_order, 
                                               color = color_bin, shape = color_bin)) + 
                scale_color_manual(values = chart_data_color_list, guide = "none") +
                # scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
                scale_y_continuous(breaks = NULL, limits = c(1, 3), expand = c(0, 0),
                                   labels = number_format(accuracy = 1)) +
                scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.02, 1.02)) +
                labs(x = NULL, y = name_placeholder, 
                     caption = NULL, color = "", linetype = "") +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = -.02,
                #                            y = 1, yend = 3), color = "#DDDDDD", size = .25) +
                geom_segment(data = chart_data,
                             mapping = aes(x = -.02, xend = 1.02,
                                           y = 1, yend = 1), color = "#DDDDDD", size = 2.5) +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = 1,
                #                            y = 3., yend = 3), color = "#DDDDDD", size = .25) +
                geom_point(mapping = aes(size = point_size, stroke = point_stroke), show.legend = FALSE) +
                scale_shape_manual(values = chart_data_shape_list) +
                scale_size_continuous(range = c(4, 6)) +
                # coord_fixed(ratio = 1 / 20, clip = "off") +
                theme_bw() +
                theme(
                        aspect.ratio = 1 / 10,
                        # plot.background = element_rect(fill = "#E0ECF7"),
                        # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                        # plot.background = element_rect(fill = "transparent", color = NA),
                        # plot.background = element_blank(),
                        # plot.margin = unit(c(0, -8, 0, 0), "mm"),
                        plot.margin = unit(c(0, 0, 0, 0), "mm"),
                        plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Gill Sans MT", 
                                                    color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                        # text = element_text(family = "Gill Sans MT", size = 46, face = "plain", color = "#000000"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        # panel.grid.major.y = element_line(color = "#DDDDDD"),
                        # panel.background = element_rect(fill = "transparent"), 
                        panel.background = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.border = element_blank(),
                        # panel.border = element_rect(color = "#DDDDDD", size = .5),
                        # panel.grid = element_blank(),
                        # line = element_blank(),
                        # rect = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.ticks.x = element_blank(),
                        # axis.ticks.x = element_line(size = .5),
                        # axis.ticks.length.y.left = unit(.2, "cm"),
                        axis.ticks.length.x.bottom = unit(.1, "cm"),
                        axis.text.x = element_blank(),
                        # axis.text.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333",
                        # margin = margin(t = 10, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                        axis.text.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                   margin = margin(t = 0, r = -20, b = 0, l = 0)),
                        # axis.line.x.bottom = element_line(color = "#333333"),
                        axis.line.x.bottom = element_blank(),
                        axis.line.y.left = element_blank(),
                        axis.title.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", 
                                                    margin = margin(t = 13, r = 0, b = 5, l = 0)),
                        axis.title.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                    vjust = .5, margin = margin(t = 0, r = 0, b = 0, l = 5)),
                        # axis.title.y = element_blank(),
                        # axis.text.y = element_blank(),
                        plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Gill Sans", color = "#333333", 
                                                  margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                        legend.position = "bottom",
                        # legend.key.size = unit(2, "mm"), 
                        legend.title = element_text(size = 10, family = "Gill Sans MT", face = "plain", color = "#333333"),
                        legend.text = element_text(size = 8, family = "Gill Sans MT", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                   hjust = .5, color = "#333333")
                        # legend.spacing.y = unit(5.5, "cm"),
                        # legend.key = element_rect(size = 5),
                        # legend.key.size = unit(2, 'lines')
                ) 
        # guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333", keywidth = 4),
        #        linetype = guide_legend(keywidth = 4))
        
        
        # miri_country_profile_dot_plot
        
        
        #///////////////////////
        
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/miri_country_profile_dot_plot_", 
                            str_to_lower(current_country), ".png"), plot = miri_country_profile_dot_plot, 
               dpi = 300, height = 6, width = 6)
        
        # crop png
        # image_read(str_c("output/charts/country_profile/miri_country_profile_dot_plot_", 
        #                  str_to_lower(current_country), ".png")) %>% image_info()
        miri_country_profile_dot_plot_png <- image_read(str_c("output/charts/country_profile/miri_country_profile_dot_plot_", 
                                                              str_to_lower(current_country), ".png")) %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 153, x_off = 0, y_off = 825))
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # obj_1_country_profile_dot_plot ####
        current_obj_num <- "obj_1"
        obj_short_name_placeholder <- str_c(rep(x = " ", times = 32), collapse = "")
        
        
        # get chart_data
        chart_data <- fmir %>% 
                filter(year %in% c(2020, 2019, 2018), obj_num == current_obj_num, !(country %in% c("Russia", "U.S."))) %>% 
                select(country, mcp_grouping, year, obj_short_name, obj_avg) %>% distinct() %>%
                bind_rows(., fmir %>% filter(year %in% c(2020, 2019, 2018), obj_num == current_obj_num, 
                                             !(country %in% c("Russia", "U.S."))) %>%
                                  group_by(mcp_grouping, year) %>% mutate(avg_obj_avg = mean(obj_avg, na.rm = TRUE)) %>%
                                  ungroup() %>%
                                  distinct(country, mcp_grouping, year, obj_short_name, 
                                           obj_avg, avg_obj_avg) %>%
                                  arrange(mcp_grouping) %>%
                                  select(-c(country, obj_avg)) %>% 
                                  mutate(country = mcp_grouping) %>%
                                  rename(obj_avg = avg_obj_avg) %>%
                                  distinct(country, mcp_grouping, year, obj_short_name, 
                                           obj_avg) %>%
                                  select(country, mcp_grouping, year, obj_short_name, obj_avg)) %>%
                mutate(y_axis_order = case_when(year == 2018 ~ 1, 
                                                year == 2020 ~ 2,
                                                year == 2019 ~ 3),
                       obj_avg = case_when(y_axis_order == 2 ~ obj_avg,
                                           TRUE ~ NA_real_),
                       var = mcp_grouping, values = obj_avg,
                       dot_label = case_when(country == current_country ~ country,
                                             country %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs", "EU-15") ~ mcp_grouping,
                                             TRUE ~ "Other"),
                       dot_label_plot_order = case_when(dot_label == "Other" ~ 1,
                                                        dot_label == "EU-15" ~ 2,
                                                        dot_label == "CARs" ~ 3,
                                                        dot_label == "E&E graduates" ~ 4,
                                                        dot_label == "E&E Eurasia" ~ 5,
                                                        dot_label == "E&E Balkans" ~ 6,
                                                        dot_label == current_country ~ 7)) %>%
                arrange(dot_label_plot_order) %>%
                mutate(color_bin = dot_label,
                       color = case_when(color_bin == current_country ~ color_palette %>% slice(11) %>% pull(hex),
                                         color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                         color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                         color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                         color_bin == "CARs" ~ color_palette %>% slice(5) %>% pull(hex),
                                         # color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                         color_bin == "EU-15" ~ color_palette %>% slice(7) %>% pull(hex),
                                         # color_bin == "U.S." ~ color_palette %>% slice(8) %>% pull(hex)),
                                         color_bin == "Other" ~ color_palette %>% slice(4) %>% pull(hex)),
                       point_size = case_when(color_bin == "Other" ~ 2,
                                              TRUE ~ 3),
                       point_shape = case_when(color_bin == "Other" ~ 1,
                                               TRUE ~ 19),
                       point_stroke = case_when(color_bin == "Other" ~ 1,
                                                TRUE ~ 1))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color)
        
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color_bin)
        
        # inspect
        # chart_data_color_list
        
        
        # create chart_data_shape_list for to pass to scale_shape_manual
        chart_data_shape_list <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(point_shape)
        
        names(chart_data_shape_list) <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(color_bin)
        
        # inspect
        # chart_data_shape_list
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        obj_1_country_profile_dot_plot <- chart_data %>% 
                ggplot(data = ., mapping = aes(x = values, y = y_axis_order, 
                                               color = color_bin, shape = color_bin)) + 
                scale_color_manual(values = chart_data_color_list, guide = "none") +
                # scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
                scale_y_continuous(breaks = NULL, limits = c(1, 3), expand = c(0, 0),
                                   labels = number_format(accuracy = 1)) +
                scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.02, 1.02)) +
                labs(x = NULL, y = obj_short_name_placeholder, 
                     caption = NULL, color = "", linetype = "") +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = -.02,
                #                            y = 1, yend = 3), color = "#DDDDDD", size = .25) +
                geom_segment(data = chart_data,
                             mapping = aes(x = -.02, xend = 1.02,
                                           y = 1, yend = 1), color = "#DDDDDD", size = 2.5) +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = 1,
                #                            y = 3., yend = 3), color = "#DDDDDD", size = .25) +
                geom_point(mapping = aes(size = point_size, stroke = point_stroke), show.legend = FALSE) +
                scale_shape_manual(values = chart_data_shape_list) +
                scale_size_continuous(range = c(4, 6)) +
                # coord_fixed(ratio = 1 / 20, clip = "off") +
                theme_bw() +
                theme(
                        aspect.ratio = 1 / 10,
                        # plot.background = element_rect(fill = "#E0ECF7"),
                        # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                        # plot.background = element_rect(fill = "transparent", color = NA),
                        # plot.background = element_blank(),
                        # plot.margin = unit(c(0, -8, 0, 0), "mm"),
                        plot.margin = unit(c(0, 0, 0, 0), "mm"),
                        plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Gill Sans MT", 
                                                    color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                        # text = element_text(family = "Gill Sans MT", size = 46, face = "plain", color = "#000000"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        # panel.grid.major.y = element_line(color = "#DDDDDD"),
                        # panel.background = element_rect(fill = "transparent"), 
                        panel.background = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.border = element_blank(),
                        # panel.border = element_rect(color = "#DDDDDD", size = .5),
                        # panel.grid = element_blank(),
                        # line = element_blank(),
                        # rect = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.ticks.x = element_blank(),
                        # axis.ticks.x = element_line(size = .5),
                        # axis.ticks.length.y.left = unit(.2, "cm"),
                        axis.ticks.length.x.bottom = unit(.1, "cm"),
                        axis.text.x = element_blank(),
                        # axis.text.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333",
                        # margin = margin(t = 10, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                        axis.text.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                   margin = margin(t = 0, r = -20, b = 0, l = 0)),
                        # axis.line.x.bottom = element_line(color = "#333333"),
                        axis.line.x.bottom = element_blank(),
                        axis.line.y.left = element_blank(),
                        axis.title.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", 
                                                    margin = margin(t = 13, r = 0, b = 5, l = 0)),
                        axis.title.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                    vjust = .5, margin = margin(t = 0, r = 0, b = 0, l = 5)),
                        # axis.title.y = element_blank(),
                        # axis.text.y = element_blank(),
                        plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Gill Sans", color = "#333333", 
                                                  margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                        legend.position = "bottom",
                        # legend.key.size = unit(2, "mm"), 
                        legend.title = element_text(size = 10, family = "Gill Sans MT", face = "plain", color = "#333333"),
                        legend.text = element_text(size = 8, family = "Gill Sans MT", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                   hjust = .5, color = "#333333")
                        # legend.spacing.y = unit(5.5, "cm"),
                        # legend.key = element_rect(size = 5),
                        # legend.key.size = unit(2, 'lines')
                ) 
        # guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333", keywidth = 4),
        #        linetype = guide_legend(keywidth = 4))
        
        
        # obj_1_country_profile_dot_plot
        
        
        #///////////////////////
        
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/obj_1_country_profile_dot_plot_", 
                            str_to_lower(current_country), ".png"), plot = obj_1_country_profile_dot_plot, 
               dpi = 300, height = 6, width = 6)
        
        # crop png
        # image_read(str_c("output/charts/country_profile/obj_1_country_profile_dot_plot_", 
        #                  str_to_lower(current_country), ".png")) %>% image_info()
        obj_1_country_profile_dot_plot_png <- image_read(str_c("output/charts/country_profile/obj_1_country_profile_dot_plot_", 
                                                               str_to_lower(current_country), ".png")) %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 153, x_off = 0, y_off = 825))
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # obj_2_country_profile_dot_plot ####
        current_obj_num <- "obj_2"
        obj_short_name_placeholder <- str_c(rep(x = " ", times = 32), collapse = "")
        
        
        # get chart_data
        chart_data <- fmir %>% 
                filter(year %in% c(2020, 2019, 2018), obj_num == current_obj_num, !(country %in% c("Russia", "U.S."))) %>% 
                select(country, mcp_grouping, year, obj_short_name, obj_avg) %>% distinct() %>%
                bind_rows(., fmir %>% filter(year %in% c(2020, 2019, 2018), obj_num == current_obj_num, 
                                             !(country %in% c("Russia", "U.S."))) %>%
                                  group_by(mcp_grouping, year) %>% mutate(avg_obj_avg = mean(obj_avg, na.rm = TRUE)) %>%
                                  ungroup() %>%
                                  distinct(country, mcp_grouping, year, obj_short_name, 
                                           obj_avg, avg_obj_avg) %>%
                                  arrange(mcp_grouping) %>%
                                  select(-c(country, obj_avg)) %>% 
                                  mutate(country = mcp_grouping) %>%
                                  rename(obj_avg = avg_obj_avg) %>%
                                  distinct(country, mcp_grouping, year, obj_short_name, 
                                           obj_avg) %>%
                                  select(country, mcp_grouping, year, obj_short_name, obj_avg)) %>%
                mutate(y_axis_order = case_when(year == 2018 ~ 1, 
                                                year == 2020 ~ 2,
                                                year == 2019 ~ 3),
                       obj_avg = case_when(y_axis_order == 2 ~ obj_avg,
                                           TRUE ~ NA_real_),
                       var = mcp_grouping, values = obj_avg,
                       dot_label = case_when(country == current_country ~ country,
                                             country %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs", "EU-15") ~ mcp_grouping,
                                             TRUE ~ "Other"),
                       dot_label_plot_order = case_when(dot_label == "Other" ~ 1,
                                                        dot_label == "EU-15" ~ 2,
                                                        dot_label == "CARs" ~ 3,
                                                        dot_label == "E&E graduates" ~ 4,
                                                        dot_label == "E&E Eurasia" ~ 5,
                                                        dot_label == "E&E Balkans" ~ 6,
                                                        dot_label == current_country ~ 7)) %>%
                arrange(dot_label_plot_order) %>%
                mutate(color_bin = dot_label,
                       color = case_when(color_bin == current_country ~ color_palette %>% slice(11) %>% pull(hex),
                                         color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                         color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                         color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                         color_bin == "CARs" ~ color_palette %>% slice(5) %>% pull(hex),
                                         # color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                         color_bin == "EU-15" ~ color_palette %>% slice(7) %>% pull(hex),
                                         # color_bin == "U.S." ~ color_palette %>% slice(8) %>% pull(hex)),
                                         color_bin == "Other" ~ color_palette %>% slice(4) %>% pull(hex)),
                       point_size = case_when(color_bin == "Other" ~ 2,
                                              TRUE ~ 3),
                       point_shape = case_when(color_bin == "Other" ~ 1,
                                               TRUE ~ 19),
                       point_stroke = case_when(color_bin == "Other" ~ 1,
                                                TRUE ~ 1))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color)
        
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color_bin)
        
        # inspect
        # chart_data_color_list
        
        
        # create chart_data_shape_list for to pass to scale_shape_manual
        chart_data_shape_list <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(point_shape)
        
        names(chart_data_shape_list) <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(color_bin)
        
        # inspect
        # chart_data_shape_list
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        obj_2_country_profile_dot_plot <- chart_data %>% 
                ggplot(data = ., mapping = aes(x = values, y = y_axis_order, 
                                               color = color_bin, shape = color_bin)) + 
                scale_color_manual(values = chart_data_color_list, guide = "none") +
                # scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
                scale_y_continuous(breaks = NULL, limits = c(1, 3), expand = c(0, 0),
                                   labels = number_format(accuracy = 1)) +
                scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.02, 1.02)) +
                labs(x = NULL, y = obj_short_name_placeholder, 
                     caption = NULL, color = "", linetype = "") +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = -.02,
                #                            y = 1, yend = 3), color = "#DDDDDD", size = .25) +
                geom_segment(data = chart_data,
                             mapping = aes(x = -.02, xend = 1.02,
                                           y = 1, yend = 1), color = "#DDDDDD", size = 2.5) +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = 1,
                #                            y = 3., yend = 3), color = "#DDDDDD", size = .25) +
                geom_point(mapping = aes(size = point_size, stroke = point_stroke), show.legend = FALSE) +
                scale_shape_manual(values = chart_data_shape_list) +
                scale_size_continuous(range = c(4, 6)) +
                # coord_fixed(ratio = 1 / 20, clip = "off") +
                theme_bw() +
                theme(
                        aspect.ratio = 1 / 10,
                        # plot.background = element_rect(fill = "#E0ECF7"),
                        # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                        # plot.background = element_rect(fill = "transparent", color = NA),
                        # plot.background = element_blank(),
                        # plot.margin = unit(c(0, -8, 0, 0), "mm"),
                        plot.margin = unit(c(0, 0, 0, 0), "mm"),
                        plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Gill Sans MT", 
                                                    color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                        # text = element_text(family = "Gill Sans MT", size = 46, face = "plain", color = "#000000"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        # panel.grid.major.y = element_line(color = "#DDDDDD"),
                        # panel.background = element_rect(fill = "transparent"), 
                        panel.background = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.border = element_blank(),
                        # panel.border = element_rect(color = "#DDDDDD", size = .5),
                        # panel.grid = element_blank(),
                        # line = element_blank(),
                        # rect = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.ticks.x = element_blank(),
                        # axis.ticks.x = element_line(size = .5),
                        # axis.ticks.length.y.left = unit(.2, "cm"),
                        axis.ticks.length.x.bottom = unit(.1, "cm"),
                        axis.text.x = element_blank(),
                        # axis.text.x = element_text(family = "Gill Sans MT", face = "plain", size = 9, color = "#333333", 
                        #                            margin = margin(t = 10, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                        axis.text.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                   margin = margin(t = 0, r = -20, b = 0, l = 0)),
                        # axis.line.x.bottom = element_line(color = "#333333"),
                        axis.line.x.bottom = element_blank(),
                        axis.line.y.left = element_blank(),
                        axis.title.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", 
                                                    margin = margin(t = 13, r = 0, b = 5, l = 0)),
                        axis.title.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                    vjust = .5, margin = margin(t = 0, r = 0, b = 0, l = 5)),
                        # axis.title.y = element_blank(),
                        # axis.text.y = element_blank(),
                        plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Gill Sans", color = "#333333", 
                                                  margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                        legend.position = "bottom",
                        # legend.key.size = unit(2, "mm"), 
                        legend.title = element_text(size = 10, family = "Gill Sans MT", face = "plain", color = "#333333"),
                        legend.text = element_text(size = 8, family = "Gill Sans MT", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                   hjust = .5, color = "#333333")
                        # legend.spacing.y = unit(5.5, "cm"),
                        # legend.key = element_rect(size = 5),
                        # legend.key.size = unit(2, 'lines')
                ) 
        # guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333", keywidth = 4),
        #        linetype = guide_legend(keywidth = 4))
        
        
        # obj_2_country_profile_dot_plot
        
        
        #///////////////////////
        
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/obj_2_country_profile_dot_plot_", 
                            str_to_lower(current_country), ".png"), plot = obj_2_country_profile_dot_plot, 
               dpi = 300, height = 6, width = 6)
        
        # crop png
        # image_read(str_c("output/charts/country_profile/obj_2_country_profile_dot_plot_", 
        #                  str_to_lower(current_country), ".png")) %>% image_info()
        obj_2_country_profile_dot_plot_png <- image_read(str_c("output/charts/country_profile/obj_2_country_profile_dot_plot_", 
                                                               str_to_lower(current_country), ".png")) %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 153, x_off = 0, y_off = 825))
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # obj_3_country_profile_dot_plot ####
        current_obj_num <- "obj_3"
        obj_short_name_placeholder <- str_c(rep(x = " ", times = 32), collapse = "")
        
        
        # get chart_data
        chart_data <- fmir %>% 
                filter(year %in% c(2020, 2019, 2018), obj_num == current_obj_num, !(country %in% c("Russia", "U.S."))) %>% 
                select(country, mcp_grouping, year, obj_short_name, obj_avg) %>% distinct() %>%
                bind_rows(., fmir %>% filter(year %in% c(2020, 2019, 2018), obj_num == current_obj_num, 
                                             !(country %in% c("Russia", "U.S."))) %>%
                                  group_by(mcp_grouping, year) %>% mutate(avg_obj_avg = mean(obj_avg, na.rm = TRUE)) %>%
                                  ungroup() %>%
                                  distinct(country, mcp_grouping, year, obj_short_name, 
                                           obj_avg, avg_obj_avg) %>%
                                  arrange(mcp_grouping) %>%
                                  select(-c(country, obj_avg)) %>% 
                                  mutate(country = mcp_grouping) %>%
                                  rename(obj_avg = avg_obj_avg) %>%
                                  distinct(country, mcp_grouping, year, obj_short_name, 
                                           obj_avg) %>%
                                  select(country, mcp_grouping, year, obj_short_name, obj_avg)) %>%
                mutate(y_axis_order = case_when(year == 2018 ~ 1, 
                                                year == 2020 ~ 2,
                                                year == 2019 ~ 3),
                       obj_avg = case_when(y_axis_order == 2 ~ obj_avg,
                                           TRUE ~ NA_real_),
                       var = mcp_grouping, values = obj_avg,
                       dot_label = case_when(country == current_country ~ country,
                                             country %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs", "EU-15") ~ mcp_grouping,
                                             TRUE ~ "Other"),
                       dot_label_plot_order = case_when(dot_label == "Other" ~ 1,
                                                        dot_label == "EU-15" ~ 2,
                                                        dot_label == "CARs" ~ 3,
                                                        dot_label == "E&E graduates" ~ 4,
                                                        dot_label == "E&E Eurasia" ~ 5,
                                                        dot_label == "E&E Balkans" ~ 6,
                                                        dot_label == current_country ~ 7)) %>%
                arrange(dot_label_plot_order) %>%
                mutate(color_bin = dot_label,
                       color = case_when(color_bin == current_country ~ color_palette %>% slice(11) %>% pull(hex),
                                         color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                         color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                         color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                         color_bin == "CARs" ~ color_palette %>% slice(5) %>% pull(hex),
                                         # color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                         color_bin == "EU-15" ~ color_palette %>% slice(7) %>% pull(hex),
                                         # color_bin == "U.S." ~ color_palette %>% slice(8) %>% pull(hex)),
                                         color_bin == "Other" ~ color_palette %>% slice(4) %>% pull(hex)),
                       point_size = case_when(color_bin == "Other" ~ 2,
                                              TRUE ~ 3),
                       point_shape = case_when(color_bin == "Other" ~ 1,
                                               TRUE ~ 19),
                       point_stroke = case_when(color_bin == "Other" ~ 1,
                                                TRUE ~ 1))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color)
        
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color_bin)
        
        # inspect
        # chart_data_color_list
        
        
        # create chart_data_shape_list for to pass to scale_shape_manual
        chart_data_shape_list <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(point_shape)
        
        names(chart_data_shape_list) <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(color_bin)
        
        # inspect
        # chart_data_shape_list
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        obj_3_country_profile_dot_plot <- chart_data %>% 
                ggplot(data = ., mapping = aes(x = values, y = y_axis_order, 
                                               color = color_bin, shape = color_bin)) + 
                scale_color_manual(values = chart_data_color_list, guide = "none") +
                # scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
                scale_y_continuous(breaks = NULL, limits = c(1, 3), expand = c(0, 0),
                                   labels = number_format(accuracy = 1)) +
                scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.02, 1.02)) +
                labs(x = NULL, y = obj_short_name_placeholder, 
                     caption = NULL, color = "", linetype = "") +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = -.02,
                #                            y = 1, yend = 3), color = "#DDDDDD", size = .25) +
                geom_segment(data = chart_data,
                             mapping = aes(x = -.02, xend = 1.02,
                                           y = 1, yend = 1), color = "#DDDDDD", size = 2.5) +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = 1,
                #                            y = 3., yend = 3), color = "#DDDDDD", size = .25) +
                geom_point(mapping = aes(size = point_size, stroke = point_stroke), show.legend = FALSE) +
                scale_shape_manual(values = chart_data_shape_list) +
                scale_size_continuous(range = c(4, 6)) +
                # coord_fixed(ratio = 1 / 20, clip = "off") +
                theme_bw() +
                theme(
                        aspect.ratio = 1 / 10,
                        # plot.background = element_rect(fill = "#E0ECF7"),
                        # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                        # plot.background = element_rect(fill = "transparent", color = NA),
                        # plot.background = element_blank(),
                        # plot.margin = unit(c(0, -8, 0, 0), "mm"),
                        plot.margin = unit(c(0, 0, 0, 0), "mm"),
                        plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Gill Sans MT", 
                                                    color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                        # text = element_text(family = "Gill Sans MT", size = 46, face = "plain", color = "#000000"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        # panel.grid.major.y = element_line(color = "#DDDDDD"),
                        # panel.background = element_rect(fill = "transparent"), 
                        panel.background = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.border = element_blank(),
                        # panel.border = element_rect(color = "#DDDDDD", size = .5),
                        # panel.grid = element_blank(),
                        # line = element_blank(),
                        # rect = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.ticks.x = element_blank(),
                        # axis.ticks.x = element_line(size = .5),
                        # axis.ticks.length.y.left = unit(.2, "cm"),
                        axis.ticks.length.x.bottom = unit(.1, "cm"),
                        axis.text.x = element_blank(),
                        # axis.text.x = element_text(family = "Gill Sans MT", face = "plain", size = 9, color = "#333333", 
                        #                            margin = margin(t = 10, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                        axis.text.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                   margin = margin(t = 0, r = -20, b = 0, l = 0)),
                        # axis.line.x.bottom = element_line(color = "#333333"),
                        axis.line.x.bottom = element_blank(),
                        axis.line.y.left = element_blank(),
                        axis.title.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", 
                                                    margin = margin(t = 13, r = 0, b = 5, l = 0)),
                        axis.title.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                    vjust = .5, margin = margin(t = 0, r = 0, b = 0, l = 5)),
                        # axis.title.y = element_blank(),
                        # axis.text.y = element_blank(),
                        plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Gill Sans", color = "#333333", 
                                                  margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                        legend.position = "bottom",
                        # legend.key.size = unit(2, "mm"), 
                        legend.title = element_text(size = 10, family = "Gill Sans MT", face = "plain", color = "#333333"),
                        legend.text = element_text(size = 8, family = "Gill Sans MT", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                   hjust = .5, color = "#333333")
                        # legend.spacing.y = unit(5.5, "cm"),
                        # legend.key = element_rect(size = 5),
                        # legend.key.size = unit(2, 'lines')
                ) 
        # guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333", keywidth = 4),
        #        linetype = guide_legend(keywidth = 4))
        
        
        # obj_3_country_profile_dot_plot
        
        
        #///////////////////////
        
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/obj_3_country_profile_dot_plot_", 
                            str_to_lower(current_country), ".png"), plot = obj_3_country_profile_dot_plot, 
               dpi = 300, height = 6, width = 6)
        
        # crop png
        # image_read(str_c("output/charts/country_profile/obj_3_country_profile_dot_plot_", 
        #                  str_to_lower(current_country), ".png")) %>% image_info()
        obj_3_country_profile_dot_plot_png <- image_read(str_c("output/charts/country_profile/obj_3_country_profile_dot_plot_", 
                                                               str_to_lower(current_country), ".png")) %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 153, x_off = 0, y_off = 825))
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # obj_4_country_profile_dot_plot ####
        current_obj_num <- "obj_4"
        obj_short_name_placeholder <- str_c(rep(x = " ", times = 32), collapse = "")
        
        
        # get chart_data
        chart_data <- fmir %>% 
                filter(year %in% c(2020, 2019, 2018), obj_num == current_obj_num, !(country %in% c("Russia", "U.S."))) %>% 
                select(country, mcp_grouping, year, obj_short_name, obj_avg) %>% distinct() %>%
                bind_rows(., fmir %>% filter(year %in% c(2020, 2019, 2018), obj_num == current_obj_num, 
                                             !(country %in% c("Russia", "U.S."))) %>%
                                  group_by(mcp_grouping, year) %>% mutate(avg_obj_avg = mean(obj_avg, na.rm = TRUE)) %>%
                                  ungroup() %>%
                                  distinct(country, mcp_grouping, year, obj_short_name, 
                                           obj_avg, avg_obj_avg) %>%
                                  arrange(mcp_grouping) %>%
                                  select(-c(country, obj_avg)) %>% 
                                  mutate(country = mcp_grouping) %>%
                                  rename(obj_avg = avg_obj_avg) %>%
                                  distinct(country, mcp_grouping, year, obj_short_name, 
                                           obj_avg) %>%
                                  select(country, mcp_grouping, year, obj_short_name, obj_avg)) %>%
                mutate(y_axis_order = case_when(year == 2018 ~ 1, 
                                                year == 2020 ~ 2,
                                                year == 2019 ~ 3),
                       obj_avg = case_when(y_axis_order == 2 ~ obj_avg,
                                           TRUE ~ NA_real_),
                       var = mcp_grouping, values = obj_avg,
                       dot_label = case_when(country == current_country ~ country,
                                             country %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs", "EU-15") ~ mcp_grouping,
                                             TRUE ~ "Other"),
                       dot_label_plot_order = case_when(dot_label == "Other" ~ 1,
                                                        dot_label == "EU-15" ~ 2,
                                                        dot_label == "CARs" ~ 3,
                                                        dot_label == "E&E graduates" ~ 4,
                                                        dot_label == "E&E Eurasia" ~ 5,
                                                        dot_label == "E&E Balkans" ~ 6,
                                                        dot_label == current_country ~ 7)) %>%
                arrange(dot_label_plot_order) %>%
                mutate(color_bin = dot_label,
                       color = case_when(color_bin == current_country ~ color_palette %>% slice(11) %>% pull(hex),
                                         color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                         color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                         color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                         color_bin == "CARs" ~ color_palette %>% slice(5) %>% pull(hex),
                                         # color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                         color_bin == "EU-15" ~ color_palette %>% slice(7) %>% pull(hex),
                                         # color_bin == "U.S." ~ color_palette %>% slice(8) %>% pull(hex)),
                                         color_bin == "Other" ~ color_palette %>% slice(4) %>% pull(hex)),
                       point_size = case_when(color_bin == "Other" ~ 2,
                                              TRUE ~ 3),
                       point_shape = case_when(color_bin == "Other" ~ 1,
                                               TRUE ~ 19),
                       point_stroke = case_when(color_bin == "Other" ~ 1,
                                                TRUE ~ 1))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color)
        
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color_bin)
        
        # inspect
        # chart_data_color_list
        
        
        # create chart_data_shape_list for to pass to scale_shape_manual
        chart_data_shape_list <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(point_shape)
        
        names(chart_data_shape_list) <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(color_bin)
        
        # inspect
        # chart_data_shape_list
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        obj_4_country_profile_dot_plot <- chart_data %>% 
                ggplot(data = ., mapping = aes(x = values, y = y_axis_order, 
                                               color = color_bin, shape = color_bin)) + 
                scale_color_manual(values = chart_data_color_list, guide = "none") +
                # scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
                scale_y_continuous(breaks = NULL, limits = c(1, 3), expand = c(0, 0),
                                   labels = number_format(accuracy = 1)) +
                scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.02, 1.02)) +
                labs(x = NULL, y = obj_short_name_placeholder, 
                     caption = NULL, color = "", linetype = "") +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = -.02,
                #                            y = 1, yend = 3), color = "#DDDDDD", size = .25) +
                geom_segment(data = chart_data,
                             mapping = aes(x = -.02, xend = 1.02,
                                           y = 1, yend = 1), color = "#DDDDDD", size = 2.5) +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = 1,
                #                            y = 3., yend = 3), color = "#DDDDDD", size = .25) +
                geom_point(mapping = aes(size = point_size, stroke = point_stroke), show.legend = FALSE) +
                scale_shape_manual(values = chart_data_shape_list) +
                scale_size_continuous(range = c(4, 6)) +
                # coord_fixed(ratio = 1 / 20, clip = "off") +
                theme_bw() +
                theme(
                        aspect.ratio = 1 / 10,
                        # plot.background = element_rect(fill = "#E0ECF7"),
                        # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                        # plot.background = element_rect(fill = "transparent", color = NA),
                        # plot.background = element_blank(),
                        # plot.margin = unit(c(0, -8, 0, 0), "mm"),
                        plot.margin = unit(c(0, 0, 0, 0), "mm"),
                        plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Gill Sans MT", 
                                                    color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                        # text = element_text(family = "Gill Sans MT", size = 46, face = "plain", color = "#000000"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        # panel.grid.major.y = element_line(color = "#DDDDDD"),
                        # panel.background = element_rect(fill = "transparent"), 
                        panel.background = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.border = element_blank(),
                        # panel.border = element_rect(color = "#DDDDDD", size = .5),
                        # panel.grid = element_blank(),
                        # line = element_blank(),
                        # rect = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.ticks.x = element_blank(),
                        # axis.ticks.x = element_line(size = .5),
                        # axis.ticks.length.y.left = unit(.2, "cm"),
                        axis.ticks.length.x.bottom = unit(.1, "cm"),
                        axis.text.x = element_blank(),
                        # axis.text.x = element_text(family = "Gill Sans MT", face = "plain", size = 9, color = "#333333", 
                        #                            margin = margin(t = 10, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                        axis.text.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                   margin = margin(t = 0, r = -20, b = 0, l = 0)),
                        # axis.line.x.bottom = element_line(color = "#333333"),
                        axis.line.x.bottom = element_blank(),
                        axis.line.y.left = element_blank(),
                        axis.title.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", 
                                                    margin = margin(t = 13, r = 0, b = 5, l = 0)),
                        axis.title.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                    vjust = .5, margin = margin(t = 0, r = 0, b = 0, l = 5)),
                        # axis.title.y = element_blank(),
                        # axis.text.y = element_blank(),
                        plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Gill Sans", color = "#333333", 
                                                  margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                        legend.position = "bottom",
                        # legend.key.size = unit(2, "mm"), 
                        legend.title = element_text(size = 10, family = "Gill Sans MT", face = "plain", color = "#333333"),
                        legend.text = element_text(size = 8, family = "Gill Sans MT", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                   hjust = .5, color = "#333333")
                        # legend.spacing.y = unit(5.5, "cm"),
                        # legend.key = element_rect(size = 5),
                        # legend.key.size = unit(2, 'lines')
                ) 
        # guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333", keywidth = 4),
        #        linetype = guide_legend(keywidth = 4))
        
        
        # obj_4_country_profile_dot_plot
        
        
        #///////////////////////
        
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/obj_4_country_profile_dot_plot_", 
                            str_to_lower(current_country), ".png"), plot = obj_4_country_profile_dot_plot, 
               dpi = 300, height = 6, width = 6)
        
        # crop png
        # image_read(str_c("output/charts/country_profile/obj_4_country_profile_dot_plot_", 
        #                  str_to_lower(current_country), ".png")) %>% image_info()
        obj_4_country_profile_dot_plot_png <- image_read(str_c("output/charts/country_profile/obj_4_country_profile_dot_plot_", 
                                                               str_to_lower(current_country), ".png")) %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 153, x_off = 0, y_off = 825))
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # obj_c_country_profile_dot_plot ####
        current_obj_num <- "obj_c"
        obj_short_name_placeholder <- str_c(rep(x = " ", times = 32), collapse = "")
        
        
        # get chart_data
        chart_data <- fmir %>% 
                filter(year %in% c(2020, 2019, 2018), obj_num == current_obj_num, !(country %in% c("Russia", "U.S."))) %>% 
                select(country, mcp_grouping, year, obj_short_name, obj_avg) %>% distinct() %>%
                bind_rows(., fmir %>% filter(year %in% c(2020, 2019, 2018), obj_num == current_obj_num, 
                                             !(country %in% c("Russia", "U.S."))) %>%
                                  group_by(mcp_grouping, year) %>% mutate(avg_obj_avg = mean(obj_avg, na.rm = TRUE)) %>%
                                  ungroup() %>%
                                  distinct(country, mcp_grouping, year, obj_short_name, 
                                           obj_avg, avg_obj_avg) %>%
                                  arrange(mcp_grouping) %>%
                                  select(-c(country, obj_avg)) %>% 
                                  mutate(country = mcp_grouping) %>%
                                  rename(obj_avg = avg_obj_avg) %>%
                                  distinct(country, mcp_grouping, year, obj_short_name, 
                                           obj_avg) %>%
                                  select(country, mcp_grouping, year, obj_short_name, obj_avg)) %>%
                mutate(y_axis_order = case_when(year == 2018 ~ 1, 
                                                year == 2020 ~ 2,
                                                year == 2019 ~ 3),
                       obj_avg = case_when(y_axis_order == 2 ~ obj_avg,
                                           TRUE ~ NA_real_),
                       var = mcp_grouping, values = obj_avg,
                       dot_label = case_when(country == current_country ~ country,
                                             country %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs", "EU-15") ~ mcp_grouping,
                                             TRUE ~ "Other"),
                       dot_label_plot_order = case_when(dot_label == "Other" ~ 1,
                                                        dot_label == "EU-15" ~ 2,
                                                        dot_label == "CARs" ~ 3,
                                                        dot_label == "E&E graduates" ~ 4,
                                                        dot_label == "E&E Eurasia" ~ 5,
                                                        dot_label == "E&E Balkans" ~ 6,
                                                        dot_label == current_country ~ 7)) %>%
                arrange(dot_label_plot_order) %>%
                mutate(color_bin = dot_label,
                       color = case_when(color_bin == current_country ~ color_palette %>% slice(11) %>% pull(hex),
                                         color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                         color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                         color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                         color_bin == "CARs" ~ color_palette %>% slice(5) %>% pull(hex),
                                         # color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                         color_bin == "EU-15" ~ color_palette %>% slice(7) %>% pull(hex),
                                         # color_bin == "U.S." ~ color_palette %>% slice(8) %>% pull(hex)),
                                         color_bin == "Other" ~ color_palette %>% slice(4) %>% pull(hex)),
                       point_size = case_when(color_bin == "Other" ~ 2,
                                              TRUE ~ 3),
                       point_shape = case_when(color_bin == "Other" ~ 1,
                                               TRUE ~ 19),
                       point_stroke = case_when(color_bin == "Other" ~ 1,
                                                TRUE ~ 1))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color)
        
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color_bin)
        
        # inspect
        # chart_data_color_list
        
        
        # create chart_data_shape_list for to pass to scale_shape_manual
        chart_data_shape_list <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(point_shape)
        
        names(chart_data_shape_list) <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(color_bin)
        
        # inspect
        # chart_data_shape_list
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        obj_c_country_profile_dot_plot <- chart_data %>% 
                ggplot(data = ., mapping = aes(x = values, y = y_axis_order, 
                                               color = color_bin, shape = color_bin)) + 
                scale_color_manual(values = chart_data_color_list, guide = "none") +
                # scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
                scale_y_continuous(breaks = NULL, limits = c(1, 3), expand = c(0, 0),
                                   labels = number_format(accuracy = 1)) +
                scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.02, 1.02)) +
                labs(x = NULL, y = obj_short_name_placeholder, 
                     caption = NULL, color = "", linetype = "") +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = -.02,
                #                            y = 1, yend = 3), color = "#DDDDDD", size = .25) +
                geom_segment(data = chart_data,
                             mapping = aes(x = -.02, xend = 1.02,
                                           y = 1, yend = 1), color = "#333333", size = 1.5) +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = 1,
                #                            y = 3., yend = 3), color = "#DDDDDD", size = .25) +
                geom_point(mapping = aes(size = point_size, stroke = point_stroke), show.legend = FALSE) +
                scale_shape_manual(values = chart_data_shape_list) +
                scale_size_continuous(range = c(4, 6)) +
                # coord_fixed(ratio = 1 / 20, clip = "off") +
                theme_bw() +
                theme(
                        aspect.ratio = 1 / 10,
                        # plot.background = element_rect(fill = "#E0ECF7"),
                        # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                        # plot.background = element_rect(fill = "transparent", color = NA),
                        # plot.background = element_blank(),
                        # plot.margin = unit(c(0, -8, 0, 0), "mm"),
                        plot.margin = unit(c(2, 0, 2, 0), "mm"),
                        plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Gill Sans MT", 
                                                    color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                        # text = element_text(family = "Gill Sans MT", size = 46, face = "plain", color = "#000000"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        # panel.grid.major.y = element_line(color = "#DDDDDD"),
                        # panel.background = element_rect(fill = "transparent"), 
                        panel.background = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.border = element_blank(),
                        # panel.border = element_rect(color = "#DDDDDD", size = .5),
                        # panel.grid = element_blank(),
                        # line = element_blank(),
                        # rect = element_blank(),
                        axis.ticks.y = element_blank(),
                        # axis.ticks.x = element_blank(),
                        axis.ticks.x = element_line(size = .5),
                        # axis.ticks.length.y.left = unit(.2, "cm"),
                        axis.ticks.length.x.bottom = unit(.1, "cm"),
                        # axis.text.x = element_blank(),
                        axis.text.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333",
                                                   margin = margin(t = 2, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                        axis.text.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                   margin = margin(t = 0, r = -20, b = 0, l = 0)),
                        # axis.line.x.bottom = element_line(color = "#333333"),
                        axis.line.x.bottom = element_blank(),
                        axis.line.y.left = element_blank(),
                        axis.title.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", 
                                                    margin = margin(t = 13, r = 0, b = 5, l = 0)),
                        axis.title.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                    vjust = .5, margin = margin(t = 0, r = 0, b = 0, l = 5)),
                        # axis.title.y = element_blank(),
                        # axis.text.y = element_blank(),
                        plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Gill Sans", color = "#333333", 
                                                  margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                        legend.position = "bottom",
                        # legend.key.size = unit(2, "mm"), 
                        legend.title = element_text(size = 10, family = "Gill Sans MT", face = "plain", color = "#333333"),
                        legend.text = element_text(size = 8, family = "Gill Sans MT", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                   hjust = .5, color = "#333333")
                        # legend.spacing.y = unit(5.5, "cm"),
                        # legend.key = element_rect(size = 5),
                        # legend.key.size = unit(2, 'lines')
                ) 
        # guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333", keywidth = 4),
        #        linetype = guide_legend(keywidth = 4))
        
        
        # obj_c_country_profile_dot_plot
        
        
        #///////////////////////
        
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/obj_c_country_profile_dot_plot_", 
                            str_to_lower(current_country), ".png"), plot = obj_c_country_profile_dot_plot, 
               dpi = 300, height = 6, width = 6)
        
        # crop png
        # image_read(str_c("output/charts/country_profile/obj_c_country_profile_dot_plot_", 
        #                  str_to_lower(current_country), ".png")) %>% image_info()
        obj_c_country_profile_dot_plot_png <- image_read(str_c("output/charts/country_profile/obj_c_country_profile_dot_plot_", 
                                                               str_to_lower(current_country), ".png")) %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 200, x_off = 0, y_off = 810))
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # sub_obj_1_1_country_profile_dot_plot ####
        current_sub_obj_num <- "sub_obj_1_1"
        sub_obj_short_name_placeholder <- str_c(rep(x = " ", times = 32), collapse = "")
        
        
        # get chart_data
        chart_data <- fmir %>% 
                filter(year %in% c(2020, 2019, 2018), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>% 
                select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg) %>% distinct() %>%
                bind_rows(., fmir %>% filter(year %in% c(2020, 2019, 2018), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>%
                                  group_by(mcp_grouping, year) %>% mutate(avg_sub_obj_avg = mean(sub_obj_avg, na.rm = TRUE)) %>%
                                  ungroup() %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg, avg_sub_obj_avg) %>%
                                  arrange(mcp_grouping) %>%
                                  select(-c(country, sub_obj_avg)) %>% 
                                  mutate(country = mcp_grouping) %>%
                                  rename(sub_obj_avg = avg_sub_obj_avg) %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg) %>%
                                  select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg)) %>%
                mutate(y_axis_order = case_when(year == 2018 ~ 1, 
                                                year == 2020 ~ 2,
                                                year == 2019 ~ 3),
                       sub_obj_avg = case_when(y_axis_order == 2 ~ sub_obj_avg,
                                                        TRUE ~ NA_real_),
                       var = mcp_grouping, values = sub_obj_avg,
                       dot_label = case_when(country == current_country ~ country,
                                             country %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs", "EU-15") ~ mcp_grouping,
                                             TRUE ~ "Other"),
                       dot_label_plot_order = case_when(dot_label == "Other" ~ 1,
                                                        dot_label == "EU-15" ~ 2,
                                                        dot_label == "CARs" ~ 3,
                                                        dot_label == "E&E graduates" ~ 4,
                                                        dot_label == "E&E Eurasia" ~ 5,
                                                        dot_label == "E&E Balkans" ~ 6,
                                                        dot_label == current_country ~ 7)) %>%
                arrange(dot_label_plot_order) %>%
                mutate(color_bin = dot_label,
                       color = case_when(color_bin == current_country ~ color_palette %>% slice(11) %>% pull(hex),
                                         color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                         color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                         color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                         color_bin == "CARs" ~ color_palette %>% slice(5) %>% pull(hex),
                                         # color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                         color_bin == "EU-15" ~ color_palette %>% slice(7) %>% pull(hex),
                                         # color_bin == "U.S." ~ color_palette %>% slice(8) %>% pull(hex)),
                                         color_bin == "Other" ~ color_palette %>% slice(4) %>% pull(hex)),
                       point_size = case_when(color_bin == "Other" ~ 2,
                                              TRUE ~ 3),
                       point_shape = case_when(color_bin == "Other" ~ 1,
                                               TRUE ~ 19),
                       point_stroke = case_when(color_bin == "Other" ~ 1,
                                                TRUE ~ 1))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color)
        
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color_bin)
        
        # inspect
        # chart_data_color_list
        
        
        # create chart_data_shape_list for to pass to scale_shape_manual
        chart_data_shape_list <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(point_shape)
        
        names(chart_data_shape_list) <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(color_bin)
        
        # inspect
        # chart_data_shape_list
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        sub_obj_1_1_country_profile_dot_plot <- chart_data %>% 
                ggplot(data = ., mapping = aes(x = values, y = y_axis_order, 
                                               color = color_bin, shape = color_bin)) + 
                scale_color_manual(values = chart_data_color_list, guide = "none") +
                # scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
                scale_y_continuous(breaks = NULL, limits = c(1, 3), expand = c(0, 0),
                                   labels = number_format(accuracy = 1)) +
                scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.02, 1.02)) +
                labs(x = NULL, y = sub_obj_short_name_placeholder, 
                     caption = NULL, color = "", linetype = "") +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = -.02,
                #                            y = 1, yend = 3), color = "#DDDDDD", size = .25) +
                geom_segment(data = chart_data,
                             mapping = aes(x = -.02, xend = 1.02,
                                           y = 1, yend = 1), color = "#DDDDDD", size = 2.5) +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = 1,
                #                            y = 3., yend = 3), color = "#DDDDDD", size = .25) +
                geom_point(mapping = aes(size = point_size, stroke = point_stroke), show.legend = FALSE) +
                scale_shape_manual(values = chart_data_shape_list) +
                scale_size_continuous(range = c(4, 6)) +
                # coord_fixed(ratio = 1 / 20, clip = "off") +
                theme_bw() +
                theme(
                        aspect.ratio = 1 / 10,
                        # plot.background = element_rect(fill = "#E0ECF7"),
                        # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                        # plot.background = element_rect(fill = "transparent", color = NA),
                        # plot.background = element_blank(),
                        # plot.margin = unit(c(0, -8, 0, 0), "mm"),
                        plot.margin = unit(c(0, 0, 0, 0), "mm"),
                        plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Gill Sans MT", 
                                                    color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                        # text = element_text(family = "Gill Sans MT", size = 46, face = "plain", color = "#000000"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        # panel.grid.major.y = element_line(color = "#DDDDDD"),
                        # panel.background = element_rect(fill = "transparent"), 
                        panel.background = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.border = element_blank(),
                        # panel.border = element_rect(color = "#DDDDDD", size = .5),
                        # panel.grid = element_blank(),
                        # line = element_blank(),
                        # rect = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.ticks.x = element_blank(),
                        # axis.ticks.x = element_line(size = .5),
                        # axis.ticks.length.y.left = unit(.2, "cm"),
                        axis.ticks.length.x.bottom = unit(.1, "cm"),
                        axis.text.x = element_blank(),
                        # axis.text.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333",
                        # margin = margin(t = 10, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                        axis.text.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                   margin = margin(t = 0, r = -20, b = 0, l = 0)),
                        # axis.line.x.bottom = element_line(color = "#333333"),
                        axis.line.x.bottom = element_blank(),
                        axis.line.y.left = element_blank(),
                        axis.title.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", 
                                                    margin = margin(t = 13, r = 0, b = 5, l = 0)),
                        axis.title.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                    vjust = .5, margin = margin(t = 0, r = 0, b = 0, l = 5)),
                        # axis.title.y = element_blank(),
                        # axis.text.y = element_blank(),
                        plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Gill Sans", color = "#333333", 
                                                  margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                        legend.position = "bottom",
                        # legend.key.size = unit(2, "mm"), 
                        legend.title = element_text(size = 10, family = "Gill Sans MT", face = "plain", color = "#333333"),
                        legend.text = element_text(size = 8, family = "Gill Sans MT", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                   hjust = .5, color = "#333333")
                        # legend.spacing.y = unit(5.5, "cm"),
                        # legend.key = element_rect(size = 5),
                        # legend.key.size = unit(2, 'lines')
                ) 
        # guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333", keywidth = 4),
        #        linetype = guide_legend(keywidth = 4))
        
        
        # sub_obj_1_1_country_profile_dot_plot
        
        
        #///////////////////////
        
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/sub_obj_1_1_country_profile_dot_plot_", 
                            str_to_lower(current_country), ".png"), plot = sub_obj_1_1_country_profile_dot_plot, 
               dpi = 300, height = 6, width = 6)
        
        # crop png
        # image_read(str_c("output/charts/country_profile/sub_obj_1_1_country_profile_dot_plot_", 
        #                  str_to_lower(current_country), ".png")) %>% image_info()
        sub_obj_1_1_country_profile_dot_plot_png <- image_read(str_c("output/charts/country_profile/sub_obj_1_1_country_profile_dot_plot_", 
                                                                     str_to_lower(current_country), ".png")) %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 153, x_off = 0, y_off = 825))
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # sub_obj_1_2_country_profile_dot_plot ####
        current_sub_obj_num <- "sub_obj_1_2"
        sub_obj_short_name_placeholder <- str_c(rep(x = " ", times = 32), collapse = "")
        
        
        # get chart_data
        chart_data <- fmir %>% 
                filter(year %in% c(2020, 2019, 2018), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>% 
                select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg) %>% distinct() %>%
                bind_rows(., fmir %>% filter(year %in% c(2020, 2019, 2018), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>%
                                  group_by(mcp_grouping, year) %>% mutate(avg_sub_obj_avg = mean(sub_obj_avg, na.rm = TRUE)) %>%
                                  ungroup() %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg, avg_sub_obj_avg) %>%
                                  arrange(mcp_grouping) %>%
                                  select(-c(country, sub_obj_avg)) %>% 
                                  mutate(country = mcp_grouping) %>%
                                  rename(sub_obj_avg = avg_sub_obj_avg) %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg) %>%
                                  select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg)) %>%
                mutate(y_axis_order = case_when(year == 2018 ~ 1, 
                                                year == 2020 ~ 2,
                                                year == 2019 ~ 3),
                       sub_obj_avg = case_when(y_axis_order == 2 ~ sub_obj_avg,
                                                        TRUE ~ NA_real_),
                       var = mcp_grouping, values = sub_obj_avg,
                       dot_label = case_when(country == current_country ~ country,
                                             country %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs", "EU-15") ~ mcp_grouping,
                                             TRUE ~ "Other"),
                       dot_label_plot_order = case_when(dot_label == "Other" ~ 1,
                                                        dot_label == "EU-15" ~ 2,
                                                        dot_label == "CARs" ~ 3,
                                                        dot_label == "E&E graduates" ~ 4,
                                                        dot_label == "E&E Eurasia" ~ 5,
                                                        dot_label == "E&E Balkans" ~ 6,
                                                        dot_label == current_country ~ 7)) %>%
                arrange(dot_label_plot_order) %>%
                mutate(color_bin = dot_label,
                       color = case_when(color_bin == current_country ~ color_palette %>% slice(11) %>% pull(hex),
                                         color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                         color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                         color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                         color_bin == "CARs" ~ color_palette %>% slice(5) %>% pull(hex),
                                         # color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                         color_bin == "EU-15" ~ color_palette %>% slice(7) %>% pull(hex),
                                         # color_bin == "U.S." ~ color_palette %>% slice(8) %>% pull(hex)),
                                         color_bin == "Other" ~ color_palette %>% slice(4) %>% pull(hex)),
                       point_size = case_when(color_bin == "Other" ~ 2,
                                              TRUE ~ 3),
                       point_shape = case_when(color_bin == "Other" ~ 1,
                                               TRUE ~ 19),
                       point_stroke = case_when(color_bin == "Other" ~ 1,
                                                TRUE ~ 1))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color)
        
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color_bin)
        
        # inspect
        # chart_data_color_list
        
        
        # create chart_data_shape_list for to pass to scale_shape_manual
        chart_data_shape_list <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(point_shape)
        
        names(chart_data_shape_list) <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(color_bin)
        
        # inspect
        # chart_data_shape_list
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        sub_obj_1_2_country_profile_dot_plot <- chart_data %>% 
                ggplot(data = ., mapping = aes(x = values, y = y_axis_order, 
                                               color = color_bin, shape = color_bin)) + 
                scale_color_manual(values = chart_data_color_list, guide = "none") +
                # scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
                scale_y_continuous(breaks = NULL, limits = c(1, 3), expand = c(0, 0),
                                   labels = number_format(accuracy = 1)) +
                scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.02, 1.02)) +
                labs(x = NULL, y = sub_obj_short_name_placeholder, 
                     caption = NULL, color = "", linetype = "") +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = -.02,
                #                            y = 1, yend = 3), color = "#DDDDDD", size = .25) +
                geom_segment(data = chart_data,
                             mapping = aes(x = -.02, xend = 1.02,
                                           y = 1, yend = 1), color = "#DDDDDD", size = 2.5) +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = 1,
                #                            y = 3., yend = 3), color = "#DDDDDD", size = .25) +
                geom_point(mapping = aes(size = point_size, stroke = point_stroke), show.legend = FALSE) +
                scale_shape_manual(values = chart_data_shape_list) +
                scale_size_continuous(range = c(4, 6)) +
                # coord_fixed(ratio = 1 / 20, clip = "off") +
                theme_bw() +
                theme(
                        aspect.ratio = 1 / 10,
                        # plot.background = element_rect(fill = "#E0ECF7"),
                        # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                        # plot.background = element_rect(fill = "transparent", color = NA),
                        # plot.background = element_blank(),
                        # plot.margin = unit(c(0, -8, 0, 0), "mm"),
                        plot.margin = unit(c(0, 0, 0, 0), "mm"),
                        plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Gill Sans MT", 
                                                    color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                        # text = element_text(family = "Gill Sans MT", size = 46, face = "plain", color = "#000000"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        # panel.grid.major.y = element_line(color = "#DDDDDD"),
                        # panel.background = element_rect(fill = "transparent"), 
                        panel.background = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.border = element_blank(),
                        # panel.border = element_rect(color = "#DDDDDD", size = .5),
                        # panel.grid = element_blank(),
                        # line = element_blank(),
                        # rect = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.ticks.x = element_blank(),
                        # axis.ticks.x = element_line(size = .5),
                        # axis.ticks.length.y.left = unit(.2, "cm"),
                        axis.ticks.length.x.bottom = unit(.1, "cm"),
                        axis.text.x = element_blank(),
                        # axis.text.x = element_text(family = "Gill Sans MT", face = "plain", size = 9, color = "#333333", 
                        #                            margin = margin(t = 10, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                        axis.text.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                   margin = margin(t = 0, r = -20, b = 0, l = 0)),
                        # axis.line.x.bottom = element_line(color = "#333333"),
                        axis.line.x.bottom = element_blank(),
                        axis.line.y.left = element_blank(),
                        axis.title.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", 
                                                    margin = margin(t = 13, r = 0, b = 5, l = 0)),
                        axis.title.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                    vjust = .5, margin = margin(t = 0, r = 0, b = 0, l = 5)),
                        # axis.title.y = element_blank(),
                        # axis.text.y = element_blank(),
                        plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Gill Sans", color = "#333333", 
                                                  margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                        legend.position = "bottom",
                        # legend.key.size = unit(2, "mm"), 
                        legend.title = element_text(size = 10, family = "Gill Sans MT", face = "plain", color = "#333333"),
                        legend.text = element_text(size = 8, family = "Gill Sans MT", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                   hjust = .5, color = "#333333")
                        # legend.spacing.y = unit(5.5, "cm"),
                        # legend.key = element_rect(size = 5),
                        # legend.key.size = unit(2, 'lines')
                ) 
        # guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333", keywidth = 4),
        #        linetype = guide_legend(keywidth = 4))
        
        
        # sub_obj_1_2_country_profile_dot_plot
        
        
        #///////////////////////
        
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/sub_obj_1_2_country_profile_dot_plot_", 
                            str_to_lower(current_country), ".png"), plot = sub_obj_1_2_country_profile_dot_plot, 
               dpi = 300, height = 6, width = 6)
        
        # crop png
        # image_read(str_c("output/charts/country_profile/sub_obj_1_2_country_profile_dot_plot_", 
        #                  str_to_lower(current_country), ".png")) %>% image_info()
        sub_obj_1_2_country_profile_dot_plot_png <- image_read(str_c("output/charts/country_profile/sub_obj_1_2_country_profile_dot_plot_", 
                                                                     str_to_lower(current_country), ".png")) %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 153, x_off = 0, y_off = 825))
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # sub_obj_1_3_country_profile_dot_plot ####
        current_sub_obj_num <- "sub_obj_1_3"
        sub_obj_short_name_placeholder <- str_c(rep(x = " ", times = 32), collapse = "")
        
        
        # get chart_data
        chart_data <- fmir %>% 
                filter(year %in% c(2020, 2019, 2018), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>% 
                select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg) %>% distinct() %>%
                bind_rows(., fmir %>% filter(year %in% c(2020, 2019, 2018), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>%
                                  group_by(mcp_grouping, year) %>% mutate(avg_sub_obj_avg = mean(sub_obj_avg, na.rm = TRUE)) %>%
                                  ungroup() %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg, avg_sub_obj_avg) %>%
                                  arrange(mcp_grouping) %>%
                                  select(-c(country, sub_obj_avg)) %>% 
                                  mutate(country = mcp_grouping) %>%
                                  rename(sub_obj_avg = avg_sub_obj_avg) %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg) %>%
                                  select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg)) %>%
                mutate(y_axis_order = case_when(year == 2018 ~ 1, 
                                                year == 2020 ~ 2,
                                                year == 2019 ~ 3),
                       sub_obj_avg = case_when(y_axis_order == 2 ~ sub_obj_avg,
                                                        TRUE ~ NA_real_),
                       var = mcp_grouping, values = sub_obj_avg,
                       dot_label = case_when(country == current_country ~ country,
                                             country %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs", "EU-15") ~ mcp_grouping,
                                             TRUE ~ "Other"),
                       dot_label_plot_order = case_when(dot_label == "Other" ~ 1,
                                                        dot_label == "EU-15" ~ 2,
                                                        dot_label == "CARs" ~ 3,
                                                        dot_label == "E&E graduates" ~ 4,
                                                        dot_label == "E&E Eurasia" ~ 5,
                                                        dot_label == "E&E Balkans" ~ 6,
                                                        dot_label == current_country ~ 7)) %>%
                arrange(dot_label_plot_order) %>%
                mutate(color_bin = dot_label,
                       color = case_when(color_bin == current_country ~ color_palette %>% slice(11) %>% pull(hex),
                                         color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                         color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                         color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                         color_bin == "CARs" ~ color_palette %>% slice(5) %>% pull(hex),
                                         # color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                         color_bin == "EU-15" ~ color_palette %>% slice(7) %>% pull(hex),
                                         # color_bin == "U.S." ~ color_palette %>% slice(8) %>% pull(hex)),
                                         color_bin == "Other" ~ color_palette %>% slice(4) %>% pull(hex)),
                       point_size = case_when(color_bin == "Other" ~ 2,
                                              TRUE ~ 3),
                       point_shape = case_when(color_bin == "Other" ~ 1,
                                               TRUE ~ 19),
                       point_stroke = case_when(color_bin == "Other" ~ 1,
                                                TRUE ~ 1))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color)
        
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color_bin)
        
        # inspect
        # chart_data_color_list
        
        
        # create chart_data_shape_list for to pass to scale_shape_manual
        chart_data_shape_list <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(point_shape)
        
        names(chart_data_shape_list) <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(color_bin)
        
        # inspect
        # chart_data_shape_list
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        sub_obj_1_3_country_profile_dot_plot <- chart_data %>% 
                ggplot(data = ., mapping = aes(x = values, y = y_axis_order, 
                                               color = color_bin, shape = color_bin)) + 
                scale_color_manual(values = chart_data_color_list, guide = "none") +
                # scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
                scale_y_continuous(breaks = NULL, limits = c(1, 3), expand = c(0, 0),
                                   labels = number_format(accuracy = 1)) +
                scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.02, 1.02)) +
                labs(x = NULL, y = sub_obj_short_name_placeholder, 
                     caption = NULL, color = "", linetype = "") +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = -.02,
                #                            y = 1, yend = 3), color = "#DDDDDD", size = .25) +
                geom_segment(data = chart_data,
                             mapping = aes(x = -.02, xend = 1.02,
                                           y = 1, yend = 1), color = "#333333", size = 1.5) +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = 1,
                #                            y = 3., yend = 3), color = "#DDDDDD", size = .25) +
                geom_point(mapping = aes(size = point_size, stroke = point_stroke), show.legend = FALSE) +
                scale_shape_manual(values = chart_data_shape_list) +
                scale_size_continuous(range = c(4, 6)) +
                # coord_fixed(ratio = 1 / 20, clip = "off") +
                theme_bw() +
                theme(
                        aspect.ratio = 1 / 10,
                        # plot.background = element_rect(fill = "#E0ECF7"),
                        # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                        # plot.background = element_rect(fill = "transparent", color = NA),
                        # plot.background = element_blank(),
                        # plot.margin = unit(c(0, -8, 0, 0), "mm"),
                        plot.margin = unit(c(2, 0, 2, 0), "mm"),
                        plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Gill Sans MT", 
                                                    color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                        # text = element_text(family = "Gill Sans MT", size = 46, face = "plain", color = "#000000"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        # panel.grid.major.y = element_line(color = "#DDDDDD"),
                        # panel.background = element_rect(fill = "transparent"), 
                        panel.background = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.border = element_blank(),
                        # panel.border = element_rect(color = "#DDDDDD", size = .5),
                        # panel.grid = element_blank(),
                        # line = element_blank(),
                        # rect = element_blank(),
                        axis.ticks.y = element_blank(),
                        # axis.ticks.x = element_blank(),
                        axis.ticks.x = element_line(size = .5),
                        # axis.ticks.length.y.left = unit(.2, "cm"),
                        axis.ticks.length.x.bottom = unit(.1, "cm"),
                        # axis.text.x = element_blank(),
                        axis.text.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333",
                                                   margin = margin(t = 2, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                        axis.text.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                   margin = margin(t = 0, r = -20, b = 0, l = 0)),
                        # axis.line.x.bottom = element_line(color = "#333333"),
                        axis.line.x.bottom = element_blank(),
                        axis.line.y.left = element_blank(),
                        axis.title.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", 
                                                    margin = margin(t = 13, r = 0, b = 5, l = 0)),
                        axis.title.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                    vjust = .5, margin = margin(t = 0, r = 0, b = 0, l = 5)),
                        # axis.title.y = element_blank(),
                        # axis.text.y = element_blank(),
                        plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Gill Sans", color = "#333333", 
                                                  margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                        legend.position = "bottom",
                        # legend.key.size = unit(2, "mm"), 
                        legend.title = element_text(size = 10, family = "Gill Sans MT", face = "plain", color = "#333333"),
                        legend.text = element_text(size = 8, family = "Gill Sans MT", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                   hjust = .5, color = "#333333")
                        # legend.spacing.y = unit(5.5, "cm"),
                        # legend.key = element_rect(size = 5),
                        # legend.key.size = unit(2, 'lines')
                ) 
        # guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333", keywidth = 4),
        #        linetype = guide_legend(keywidth = 4))
        
        
        # sub_obj_1_3_country_profile_dot_plot
        
        
        #///////////////////////
        
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/sub_obj_1_3_country_profile_dot_plot_", 
                            str_to_lower(current_country), ".png"), plot = sub_obj_1_3_country_profile_dot_plot, 
               dpi = 300, height = 6, width = 6)
        
        # crop png
        # image_read(str_c("output/charts/country_profile/sub_obj_1_3_country_profile_dot_plot_", 
        #                  str_to_lower(current_country), ".png")) %>% image_info()
        sub_obj_1_3_country_profile_dot_plot_png <- image_read(str_c("output/charts/country_profile/sub_obj_1_3_country_profile_dot_plot_", 
                                                                     str_to_lower(current_country), ".png")) %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 200, x_off = 0, y_off = 810))
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # sub_obj_2_1_country_profile_dot_plot ####
        current_sub_obj_num <- "sub_obj_2_1"
        sub_obj_short_name_placeholder <- str_c(rep(x = " ", times = 32), collapse = "")
        
        
        # get chart_data
        chart_data <- fmir %>% 
                filter(year %in% c(2020, 2019, 2018), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>% 
                select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg) %>% distinct() %>%
                bind_rows(., fmir %>% filter(year %in% c(2020, 2019, 2018), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>%
                                  group_by(mcp_grouping, year) %>% mutate(avg_sub_obj_avg = mean(sub_obj_avg, na.rm = TRUE)) %>%
                                  ungroup() %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg, avg_sub_obj_avg) %>%
                                  arrange(mcp_grouping) %>%
                                  select(-c(country, sub_obj_avg)) %>% 
                                  mutate(country = mcp_grouping) %>%
                                  rename(sub_obj_avg = avg_sub_obj_avg) %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg) %>%
                                  select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg)) %>%
                mutate(y_axis_order = case_when(year == 2018 ~ 1, 
                                                year == 2020 ~ 2,
                                                year == 2019 ~ 3),
                       sub_obj_avg = case_when(y_axis_order == 2 ~ sub_obj_avg,
                                                        TRUE ~ NA_real_),
                       var = mcp_grouping, values = sub_obj_avg,
                       dot_label = case_when(country == current_country ~ country,
                                             country %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs", "EU-15") ~ mcp_grouping,
                                             TRUE ~ "Other"),
                       dot_label_plot_order = case_when(dot_label == "Other" ~ 1,
                                                        dot_label == "EU-15" ~ 2,
                                                        dot_label == "CARs" ~ 3,
                                                        dot_label == "E&E graduates" ~ 4,
                                                        dot_label == "E&E Eurasia" ~ 5,
                                                        dot_label == "E&E Balkans" ~ 6,
                                                        dot_label == current_country ~ 7)) %>%
                arrange(dot_label_plot_order) %>%
                mutate(color_bin = dot_label,
                       color = case_when(color_bin == current_country ~ color_palette %>% slice(11) %>% pull(hex),
                                         color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                         color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                         color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                         color_bin == "CARs" ~ color_palette %>% slice(5) %>% pull(hex),
                                         # color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                         color_bin == "EU-15" ~ color_palette %>% slice(7) %>% pull(hex),
                                         # color_bin == "U.S." ~ color_palette %>% slice(8) %>% pull(hex)),
                                         color_bin == "Other" ~ color_palette %>% slice(4) %>% pull(hex)),
                       point_size = case_when(color_bin == "Other" ~ 2,
                                              TRUE ~ 3),
                       point_shape = case_when(color_bin == "Other" ~ 1,
                                               TRUE ~ 19),
                       point_stroke = case_when(color_bin == "Other" ~ 1,
                                                TRUE ~ 1))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color)
        
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color_bin)
        
        # inspect
        # chart_data_color_list
        
        
        # create chart_data_shape_list for to pass to scale_shape_manual
        chart_data_shape_list <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(point_shape)
        
        names(chart_data_shape_list) <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(color_bin)
        
        # inspect
        # chart_data_shape_list
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        sub_obj_2_1_country_profile_dot_plot <- chart_data %>% 
                ggplot(data = ., mapping = aes(x = values, y = y_axis_order, 
                                               color = color_bin, shape = color_bin)) + 
                scale_color_manual(values = chart_data_color_list, guide = "none") +
                # scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
                scale_y_continuous(breaks = NULL, limits = c(1, 3), expand = c(0, 0),
                                   labels = number_format(accuracy = 1)) +
                scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.02, 1.02)) +
                labs(x = NULL, y = sub_obj_short_name_placeholder, 
                     caption = NULL, color = "", linetype = "") +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = -.02,
                #                            y = 1, yend = 3), color = "#DDDDDD", size = .25) +
                geom_segment(data = chart_data,
                             mapping = aes(x = -.02, xend = 1.02,
                                           y = 1, yend = 1), color = "#DDDDDD", size = 2.5) +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = 1,
                #                            y = 3., yend = 3), color = "#DDDDDD", size = .25) +
                geom_point(mapping = aes(size = point_size, stroke = point_stroke), show.legend = FALSE) +
                scale_shape_manual(values = chart_data_shape_list) +
                scale_size_continuous(range = c(4, 6)) +
                # coord_fixed(ratio = 1 / 20, clip = "off") +
                theme_bw() +
                theme(
                        aspect.ratio = 1 / 10,
                        # plot.background = element_rect(fill = "#E0ECF7"),
                        # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                        # plot.background = element_rect(fill = "transparent", color = NA),
                        # plot.background = element_blank(),
                        # plot.margin = unit(c(0, -8, 0, 0), "mm"),
                        plot.margin = unit(c(0, 0, 0, 0), "mm"),
                        plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Gill Sans MT", 
                                                    color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                        # text = element_text(family = "Gill Sans MT", size = 46, face = "plain", color = "#000000"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        # panel.grid.major.y = element_line(color = "#DDDDDD"),
                        # panel.background = element_rect(fill = "transparent"), 
                        panel.background = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.border = element_blank(),
                        # panel.border = element_rect(color = "#DDDDDD", size = .5),
                        # panel.grid = element_blank(),
                        # line = element_blank(),
                        # rect = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.ticks.x = element_blank(),
                        # axis.ticks.x = element_line(size = .5),
                        # axis.ticks.length.y.left = unit(.2, "cm"),
                        axis.ticks.length.x.bottom = unit(.1, "cm"),
                        axis.text.x = element_blank(),
                        # axis.text.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333",
                        # margin = margin(t = 10, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                        axis.text.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                   margin = margin(t = 0, r = -20, b = 0, l = 0)),
                        # axis.line.x.bottom = element_line(color = "#333333"),
                        axis.line.x.bottom = element_blank(),
                        axis.line.y.left = element_blank(),
                        axis.title.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", 
                                                    margin = margin(t = 13, r = 0, b = 5, l = 0)),
                        axis.title.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                    vjust = .5, margin = margin(t = 0, r = 0, b = 0, l = 5)),
                        # axis.title.y = element_blank(),
                        # axis.text.y = element_blank(),
                        plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Gill Sans", color = "#333333", 
                                                  margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                        legend.position = "bottom",
                        # legend.key.size = unit(2, "mm"), 
                        legend.title = element_text(size = 10, family = "Gill Sans MT", face = "plain", color = "#333333"),
                        legend.text = element_text(size = 8, family = "Gill Sans MT", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                   hjust = .5, color = "#333333")
                        # legend.spacing.y = unit(5.5, "cm"),
                        # legend.key = element_rect(size = 5),
                        # legend.key.size = unit(2, 'lines')
                ) 
        # guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333", keywidth = 4),
        #        linetype = guide_legend(keywidth = 4))
        
        
        # sub_obj_2_1_country_profile_dot_plot
        
        
        #///////////////////////
        
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/sub_obj_2_1_country_profile_dot_plot_", 
                            str_to_lower(current_country), ".png"), plot = sub_obj_2_1_country_profile_dot_plot, 
               dpi = 300, height = 6, width = 6)
        
        # crop png
        # image_read(str_c("output/charts/country_profile/sub_obj_2_1_country_profile_dot_plot_", 
        #                  str_to_lower(current_country), ".png")) %>% image_info()
        sub_obj_2_1_country_profile_dot_plot_png <- image_read(str_c("output/charts/country_profile/sub_obj_2_1_country_profile_dot_plot_", 
                                                                     str_to_lower(current_country), ".png")) %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 153, x_off = 0, y_off = 825))
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # sub_obj_2_2_country_profile_dot_plot ####
        current_sub_obj_num <- "sub_obj_2_2"
        sub_obj_short_name_placeholder <- str_c(rep(x = " ", times = 32), collapse = "")
        
        
        # get chart_data
        chart_data <- fmir %>% 
                filter(year %in% c(2020, 2019, 2018), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>% 
                select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg) %>% distinct() %>%
                bind_rows(., fmir %>% filter(year %in% c(2020, 2019, 2018), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>%
                                  group_by(mcp_grouping, year) %>% mutate(avg_sub_obj_avg = mean(sub_obj_avg, na.rm = TRUE)) %>%
                                  ungroup() %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg, avg_sub_obj_avg) %>%
                                  arrange(mcp_grouping) %>%
                                  select(-c(country, sub_obj_avg)) %>% 
                                  mutate(country = mcp_grouping) %>%
                                  rename(sub_obj_avg = avg_sub_obj_avg) %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg) %>%
                                  select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg)) %>%
                mutate(y_axis_order = case_when(year == 2018 ~ 1, 
                                                year == 2020 ~ 2,
                                                year == 2019 ~ 3),
                       sub_obj_avg = case_when(y_axis_order == 2 ~ sub_obj_avg,
                                                        TRUE ~ NA_real_),
                       var = mcp_grouping, values = sub_obj_avg,
                       dot_label = case_when(country == current_country ~ country,
                                             country %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs", "EU-15") ~ mcp_grouping,
                                             TRUE ~ "Other"),
                       dot_label_plot_order = case_when(dot_label == "Other" ~ 1,
                                                        dot_label == "EU-15" ~ 2,
                                                        dot_label == "CARs" ~ 3,
                                                        dot_label == "E&E graduates" ~ 4,
                                                        dot_label == "E&E Eurasia" ~ 5,
                                                        dot_label == "E&E Balkans" ~ 6,
                                                        dot_label == current_country ~ 7)) %>%
                arrange(dot_label_plot_order) %>%
                mutate(color_bin = dot_label,
                       color = case_when(color_bin == current_country ~ color_palette %>% slice(11) %>% pull(hex),
                                         color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                         color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                         color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                         color_bin == "CARs" ~ color_palette %>% slice(5) %>% pull(hex),
                                         # color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                         color_bin == "EU-15" ~ color_palette %>% slice(7) %>% pull(hex),
                                         # color_bin == "U.S." ~ color_palette %>% slice(8) %>% pull(hex)),
                                         color_bin == "Other" ~ color_palette %>% slice(4) %>% pull(hex)),
                       point_size = case_when(color_bin == "Other" ~ 2,
                                              TRUE ~ 3),
                       point_shape = case_when(color_bin == "Other" ~ 1,
                                               TRUE ~ 19),
                       point_stroke = case_when(color_bin == "Other" ~ 1,
                                                TRUE ~ 1))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color)
        
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color_bin)
        
        # inspect
        # chart_data_color_list
        
        
        # create chart_data_shape_list for to pass to scale_shape_manual
        chart_data_shape_list <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(point_shape)
        
        names(chart_data_shape_list) <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(color_bin)
        
        # inspect
        # chart_data_shape_list
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        sub_obj_2_2_country_profile_dot_plot <- chart_data %>% 
                ggplot(data = ., mapping = aes(x = values, y = y_axis_order, 
                                               color = color_bin, shape = color_bin)) + 
                scale_color_manual(values = chart_data_color_list, guide = "none") +
                # scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
                scale_y_continuous(breaks = NULL, limits = c(1, 3), expand = c(0, 0),
                                   labels = number_format(accuracy = 1)) +
                scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.02, 1.02)) +
                labs(x = NULL, y = sub_obj_short_name_placeholder, 
                     caption = NULL, color = "", linetype = "") +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = -.02,
                #                            y = 1, yend = 3), color = "#DDDDDD", size = .25) +
                geom_segment(data = chart_data,
                             mapping = aes(x = -.02, xend = 1.02,
                                           y = 1, yend = 1), color = "#DDDDDD", size = 2.5) +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = 1,
                #                            y = 3., yend = 3), color = "#DDDDDD", size = .25) +
                geom_point(mapping = aes(size = point_size, stroke = point_stroke), show.legend = FALSE) +
                scale_shape_manual(values = chart_data_shape_list) +
                scale_size_continuous(range = c(4, 6)) +
                # coord_fixed(ratio = 1 / 20, clip = "off") +
                theme_bw() +
                theme(
                        aspect.ratio = 1 / 10,
                        # plot.background = element_rect(fill = "#E0ECF7"),
                        # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                        # plot.background = element_rect(fill = "transparent", color = NA),
                        # plot.background = element_blank(),
                        # plot.margin = unit(c(0, -8, 0, 0), "mm"),
                        plot.margin = unit(c(0, 0, 0, 0), "mm"),
                        plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Gill Sans MT", 
                                                    color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                        # text = element_text(family = "Gill Sans MT", size = 46, face = "plain", color = "#000000"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        # panel.grid.major.y = element_line(color = "#DDDDDD"),
                        # panel.background = element_rect(fill = "transparent"), 
                        panel.background = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.border = element_blank(),
                        # panel.border = element_rect(color = "#DDDDDD", size = .5),
                        # panel.grid = element_blank(),
                        # line = element_blank(),
                        # rect = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.ticks.x = element_blank(),
                        # axis.ticks.x = element_line(size = .5),
                        # axis.ticks.length.y.left = unit(.2, "cm"),
                        axis.ticks.length.x.bottom = unit(.1, "cm"),
                        axis.text.x = element_blank(),
                        # axis.text.x = element_text(family = "Gill Sans MT", face = "plain", size = 9, color = "#333333", 
                        #                            margin = margin(t = 10, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                        axis.text.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                   margin = margin(t = 0, r = -20, b = 0, l = 0)),
                        # axis.line.x.bottom = element_line(color = "#333333"),
                        axis.line.x.bottom = element_blank(),
                        axis.line.y.left = element_blank(),
                        axis.title.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", 
                                                    margin = margin(t = 13, r = 0, b = 5, l = 0)),
                        axis.title.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                    vjust = .5, margin = margin(t = 0, r = 0, b = 0, l = 5)),
                        # axis.title.y = element_blank(),
                        # axis.text.y = element_blank(),
                        plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Gill Sans", color = "#333333", 
                                                  margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                        legend.position = "bottom",
                        # legend.key.size = unit(2, "mm"), 
                        legend.title = element_text(size = 10, family = "Gill Sans MT", face = "plain", color = "#333333"),
                        legend.text = element_text(size = 8, family = "Gill Sans MT", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                   hjust = .5, color = "#333333")
                        # legend.spacing.y = unit(5.5, "cm"),
                        # legend.key = element_rect(size = 5),
                        # legend.key.size = unit(2, 'lines')
                ) 
        # guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333", keywidth = 4),
        #        linetype = guide_legend(keywidth = 4))
        
        
        # sub_obj_2_2_country_profile_dot_plot
        
        
        #///////////////////////
        
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/sub_obj_2_2_country_profile_dot_plot_", 
                            str_to_lower(current_country), ".png"), plot = sub_obj_2_2_country_profile_dot_plot, 
               dpi = 300, height = 6, width = 6)
        
        # crop png
        # image_read(str_c("output/charts/country_profile/sub_obj_2_2_country_profile_dot_plot_", 
        #                  str_to_lower(current_country), ".png")) %>% image_info()
        sub_obj_2_2_country_profile_dot_plot_png <- image_read(str_c("output/charts/country_profile/sub_obj_2_2_country_profile_dot_plot_", 
                                                                     str_to_lower(current_country), ".png")) %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 153, x_off = 0, y_off = 825))
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # sub_obj_2_3_country_profile_dot_plot ####
        current_sub_obj_num <- "sub_obj_2_3"
        sub_obj_short_name_placeholder <- str_c(rep(x = " ", times = 32), collapse = "")
        
        
        # get chart_data
        chart_data <- fmir %>% 
                filter(year %in% c(2020, 2019, 2018), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>% 
                select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg) %>% distinct() %>%
                bind_rows(., fmir %>% filter(year %in% c(2020, 2019, 2018), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>%
                                  group_by(mcp_grouping, year) %>% mutate(avg_sub_obj_avg = mean(sub_obj_avg, na.rm = TRUE)) %>%
                                  ungroup() %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg, avg_sub_obj_avg) %>%
                                  arrange(mcp_grouping) %>%
                                  select(-c(country, sub_obj_avg)) %>% 
                                  mutate(country = mcp_grouping) %>%
                                  rename(sub_obj_avg = avg_sub_obj_avg) %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg) %>%
                                  select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg)) %>%
                mutate(y_axis_order = case_when(year == 2018 ~ 1, 
                                                year == 2020 ~ 2,
                                                year == 2019 ~ 3),
                       sub_obj_avg = case_when(y_axis_order == 2 ~ sub_obj_avg,
                                                        TRUE ~ NA_real_),
                       var = mcp_grouping, values = sub_obj_avg,
                       dot_label = case_when(country == current_country ~ country,
                                             country %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs", "EU-15") ~ mcp_grouping,
                                             TRUE ~ "Other"),
                       dot_label_plot_order = case_when(dot_label == "Other" ~ 1,
                                                        dot_label == "EU-15" ~ 2,
                                                        dot_label == "CARs" ~ 3,
                                                        dot_label == "E&E graduates" ~ 4,
                                                        dot_label == "E&E Eurasia" ~ 5,
                                                        dot_label == "E&E Balkans" ~ 6,
                                                        dot_label == current_country ~ 7)) %>%
                arrange(dot_label_plot_order) %>%
                mutate(color_bin = dot_label,
                       color = case_when(color_bin == current_country ~ color_palette %>% slice(11) %>% pull(hex),
                                         color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                         color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                         color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                         color_bin == "CARs" ~ color_palette %>% slice(5) %>% pull(hex),
                                         # color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                         color_bin == "EU-15" ~ color_palette %>% slice(7) %>% pull(hex),
                                         # color_bin == "U.S." ~ color_palette %>% slice(8) %>% pull(hex)),
                                         color_bin == "Other" ~ color_palette %>% slice(4) %>% pull(hex)),
                       point_size = case_when(color_bin == "Other" ~ 2,
                                              TRUE ~ 3),
                       point_shape = case_when(color_bin == "Other" ~ 1,
                                               TRUE ~ 19),
                       point_stroke = case_when(color_bin == "Other" ~ 1,
                                                TRUE ~ 1))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color)
        
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color_bin)
        
        # inspect
        # chart_data_color_list
        
        
        # create chart_data_shape_list for to pass to scale_shape_manual
        chart_data_shape_list <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(point_shape)
        
        names(chart_data_shape_list) <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(color_bin)
        
        # inspect
        # chart_data_shape_list
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        sub_obj_2_3_country_profile_dot_plot <- chart_data %>% 
                ggplot(data = ., mapping = aes(x = values, y = y_axis_order, 
                                               color = color_bin, shape = color_bin)) + 
                scale_color_manual(values = chart_data_color_list, guide = "none") +
                # scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
                scale_y_continuous(breaks = NULL, limits = c(1, 3), expand = c(0, 0),
                                   labels = number_format(accuracy = 1)) +
                scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.02, 1.02)) +
                labs(x = NULL, y = sub_obj_short_name_placeholder, 
                     caption = NULL, color = "", linetype = "") +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = -.02,
                #                            y = 1, yend = 3), color = "#DDDDDD", size = .25) +
                geom_segment(data = chart_data,
                             mapping = aes(x = -.02, xend = 1.02,
                                           y = 1, yend = 1), color = "#333333", size = 1.5) +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = 1,
                #                            y = 3., yend = 3), color = "#DDDDDD", size = .25) +
                geom_point(mapping = aes(size = point_size, stroke = point_stroke), show.legend = FALSE) +
                scale_shape_manual(values = chart_data_shape_list) +
                scale_size_continuous(range = c(4, 6)) +
                # coord_fixed(ratio = 1 / 20, clip = "off") +
                theme_bw() +
                theme(
                        aspect.ratio = 1 / 10,
                        # plot.background = element_rect(fill = "#E0ECF7"),
                        # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                        # plot.background = element_rect(fill = "transparent", color = NA),
                        # plot.background = element_blank(),
                        # plot.margin = unit(c(0, -8, 0, 0), "mm"),
                        plot.margin = unit(c(2, 0, 2, 0), "mm"),
                        plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Gill Sans MT", 
                                                    color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                        # text = element_text(family = "Gill Sans MT", size = 46, face = "plain", color = "#000000"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        # panel.grid.major.y = element_line(color = "#DDDDDD"),
                        # panel.background = element_rect(fill = "transparent"), 
                        panel.background = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.border = element_blank(),
                        # panel.border = element_rect(color = "#DDDDDD", size = .5),
                        # panel.grid = element_blank(),
                        # line = element_blank(),
                        # rect = element_blank(),
                        axis.ticks.y = element_blank(),
                        # axis.ticks.x = element_blank(),
                        axis.ticks.x = element_line(size = .5),
                        # axis.ticks.length.y.left = unit(.2, "cm"),
                        axis.ticks.length.x.bottom = unit(.1, "cm"),
                        # axis.text.x = element_blank(),
                        axis.text.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333",
                                                   margin = margin(t = 2, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                        axis.text.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                   margin = margin(t = 0, r = -20, b = 0, l = 0)),
                        # axis.line.x.bottom = element_line(color = "#333333"),
                        axis.line.x.bottom = element_blank(),
                        axis.line.y.left = element_blank(),
                        axis.title.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", 
                                                    margin = margin(t = 13, r = 0, b = 5, l = 0)),
                        axis.title.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                    vjust = .5, margin = margin(t = 0, r = 0, b = 0, l = 5)),
                        # axis.title.y = element_blank(),
                        # axis.text.y = element_blank(),
                        plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Gill Sans", color = "#333333", 
                                                  margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                        legend.position = "bottom",
                        # legend.key.size = unit(2, "mm"), 
                        legend.title = element_text(size = 10, family = "Gill Sans MT", face = "plain", color = "#333333"),
                        legend.text = element_text(size = 8, family = "Gill Sans MT", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                   hjust = .5, color = "#333333")
                        # legend.spacing.y = unit(5.5, "cm"),
                        # legend.key = element_rect(size = 5),
                        # legend.key.size = unit(2, 'lines')
                ) 
        # guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333", keywidth = 4),
        #        linetype = guide_legend(keywidth = 4))
        
        
        # sub_obj_2_3_country_profile_dot_plot
        
        
        #///////////////////////
        
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/sub_obj_2_3_country_profile_dot_plot_", 
                            str_to_lower(current_country), ".png"), plot = sub_obj_2_3_country_profile_dot_plot, 
               dpi = 300, height = 6, width = 6)
        
        # crop png
        # image_read(str_c("output/charts/country_profile/sub_obj_2_3_country_profile_dot_plot_", 
        #                  str_to_lower(current_country), ".png")) %>% image_info()
        sub_obj_2_3_country_profile_dot_plot_png <- image_read(str_c("output/charts/country_profile/sub_obj_2_3_country_profile_dot_plot_", 
                                                                     str_to_lower(current_country), ".png")) %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 200, x_off = 0, y_off = 810))
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # sub_obj_3_1_country_profile_dot_plot ####
        current_sub_obj_num <- "sub_obj_3_1"
        sub_obj_short_name_placeholder <- str_c(rep(x = " ", times = 32), collapse = "")
        
        # get chart_data
        chart_data <- fmir %>% 
                filter(year %in% c(2020, 2019, 2018), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>% 
                select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg) %>% distinct() %>%
                bind_rows(., fmir %>% filter(year %in% c(2020, 2019, 2018), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>%
                                  group_by(mcp_grouping, year) %>% mutate(avg_sub_obj_avg = mean(sub_obj_avg, na.rm = TRUE)) %>%
                                  ungroup() %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg, avg_sub_obj_avg) %>%
                                  arrange(mcp_grouping) %>%
                                  select(-c(country, sub_obj_avg)) %>% 
                                  mutate(country = mcp_grouping) %>%
                                  rename(sub_obj_avg = avg_sub_obj_avg) %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg) %>%
                                  select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg)) %>%
                mutate(y_axis_order = case_when(year == 2018 ~ 1, 
                                                year == 2020 ~ 2,
                                                year == 2019 ~ 3),
                       sub_obj_avg = case_when(y_axis_order == 2 ~ sub_obj_avg,
                                                        TRUE ~ NA_real_),
                       var = mcp_grouping, values = sub_obj_avg,
                       dot_label = case_when(country == current_country ~ country,
                                             country %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs", "EU-15") ~ mcp_grouping,
                                             TRUE ~ "Other"),
                       dot_label_plot_order = case_when(dot_label == "Other" ~ 1,
                                                        dot_label == "EU-15" ~ 2,
                                                        dot_label == "CARs" ~ 3,
                                                        dot_label == "E&E graduates" ~ 4,
                                                        dot_label == "E&E Eurasia" ~ 5,
                                                        dot_label == "E&E Balkans" ~ 6,
                                                        dot_label == current_country ~ 7)) %>%
                arrange(dot_label_plot_order) %>%
                mutate(color_bin = dot_label,
                       color = case_when(color_bin == current_country ~ color_palette %>% slice(11) %>% pull(hex),
                                         color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                         color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                         color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                         color_bin == "CARs" ~ color_palette %>% slice(5) %>% pull(hex),
                                         # color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                         color_bin == "EU-15" ~ color_palette %>% slice(7) %>% pull(hex),
                                         # color_bin == "U.S." ~ color_palette %>% slice(8) %>% pull(hex)),
                                         color_bin == "Other" ~ color_palette %>% slice(4) %>% pull(hex)),
                       point_size = case_when(color_bin == "Other" ~ 2,
                                              TRUE ~ 3),
                       point_shape = case_when(color_bin == "Other" ~ 1,
                                               TRUE ~ 19),
                       point_stroke = case_when(color_bin == "Other" ~ 1,
                                                TRUE ~ 1))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color)
        
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color_bin)
        
        # inspect
        # chart_data_color_list
        
        
        # create chart_data_shape_list for to pass to scale_shape_manual
        chart_data_shape_list <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(point_shape)
        
        names(chart_data_shape_list) <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(color_bin)
        
        # inspect
        # chart_data_shape_list
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        sub_obj_3_1_country_profile_dot_plot <- chart_data %>% 
                ggplot(data = ., mapping = aes(x = values, y = y_axis_order, 
                                               color = color_bin, shape = color_bin)) + 
                scale_color_manual(values = chart_data_color_list, guide = "none") +
                # scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
                scale_y_continuous(breaks = NULL, limits = c(1, 3), expand = c(0, 0),
                                   labels = number_format(accuracy = 1)) +
                scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.02, 1.02)) +
                labs(x = NULL, y = sub_obj_short_name_placeholder, 
                     caption = NULL, color = "", linetype = "") +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = -.02,
                #                            y = 1, yend = 3), color = "#DDDDDD", size = .25) +
                geom_segment(data = chart_data,
                             mapping = aes(x = -.02, xend = 1.02,
                                           y = 1, yend = 1), color = "#DDDDDD", size = 2.5) +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = 1,
                #                            y = 3., yend = 3), color = "#DDDDDD", size = .25) +
                geom_point(mapping = aes(size = point_size, stroke = point_stroke), show.legend = FALSE) +
                scale_shape_manual(values = chart_data_shape_list) +
                scale_size_continuous(range = c(4, 6)) +
                # coord_fixed(ratio = 1 / 20, clip = "off") +
                theme_bw() +
                theme(
                        aspect.ratio = 1 / 10,
                        # plot.background = element_rect(fill = "#E0ECF7"),
                        # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                        # plot.background = element_rect(fill = "transparent", color = NA),
                        # plot.background = element_blank(),
                        # plot.margin = unit(c(0, -8, 0, 0), "mm"),
                        plot.margin = unit(c(0, 0, 0, 0), "mm"),
                        plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Gill Sans MT", 
                                                    color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                        # text = element_text(family = "Gill Sans MT", size = 46, face = "plain", color = "#000000"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        # panel.grid.major.y = element_line(color = "#DDDDDD"),
                        # panel.background = element_rect(fill = "transparent"), 
                        panel.background = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.border = element_blank(),
                        # panel.border = element_rect(color = "#DDDDDD", size = .5),
                        # panel.grid = element_blank(),
                        # line = element_blank(),
                        # rect = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.ticks.x = element_blank(),
                        # axis.ticks.x = element_line(size = .5),
                        # axis.ticks.length.y.left = unit(.2, "cm"),
                        axis.ticks.length.x.bottom = unit(.1, "cm"),
                        axis.text.x = element_blank(),
                        # axis.text.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333",
                        # margin = margin(t = 10, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                        axis.text.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                   margin = margin(t = 0, r = -20, b = 0, l = 0)),
                        # axis.line.x.bottom = element_line(color = "#333333"),
                        axis.line.x.bottom = element_blank(),
                        axis.line.y.left = element_blank(),
                        axis.title.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", 
                                                    margin = margin(t = 13, r = 0, b = 5, l = 0)),
                        axis.title.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                    vjust = .5, margin = margin(t = 0, r = 0, b = 0, l = 5)),
                        # axis.title.y = element_blank(),
                        # axis.text.y = element_blank(),
                        plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Gill Sans", color = "#333333", 
                                                  margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                        legend.position = "bottom",
                        # legend.key.size = unit(2, "mm"), 
                        legend.title = element_text(size = 10, family = "Gill Sans MT", face = "plain", color = "#333333"),
                        legend.text = element_text(size = 8, family = "Gill Sans MT", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                   hjust = .5, color = "#333333")
                        # legend.spacing.y = unit(5.5, "cm"),
                        # legend.key = element_rect(size = 5),
                        # legend.key.size = unit(2, 'lines')
                ) 
        # guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333", keywidth = 4),
        #        linetype = guide_legend(keywidth = 4))
        
        
        # sub_obj_3_1_country_profile_dot_plot
        
        
        #///////////////////////
        
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/sub_obj_3_1_country_profile_dot_plot_", 
                            str_to_lower(current_country), ".png"), plot = sub_obj_3_1_country_profile_dot_plot, 
               dpi = 300, height = 6, width = 6)
        
        # crop png
        # image_read(str_c("output/charts/country_profile/sub_obj_3_1_country_profile_dot_plot_", 
        #                  str_to_lower(current_country), ".png")) %>% image_info()
        sub_obj_3_1_country_profile_dot_plot_png <- image_read(str_c("output/charts/country_profile/sub_obj_3_1_country_profile_dot_plot_", 
                                                                     str_to_lower(current_country), ".png")) %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 153, x_off = 0, y_off = 825))
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # sub_obj_3_2_country_profile_dot_plot ####
        current_sub_obj_num <- "sub_obj_3_2"
        sub_obj_short_name_placeholder <- str_c(rep(x = " ", times = 32), collapse = "")
        
        
        # get chart_data
        chart_data <- fmir %>% 
                filter(year %in% c(2020, 2019, 2018), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>% 
                select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg) %>% distinct() %>%
                bind_rows(., fmir %>% filter(year %in% c(2020, 2019, 2018), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>%
                                  group_by(mcp_grouping, year) %>% mutate(avg_sub_obj_avg = mean(sub_obj_avg, na.rm = TRUE)) %>%
                                  ungroup() %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg, avg_sub_obj_avg) %>%
                                  arrange(mcp_grouping) %>%
                                  select(-c(country, sub_obj_avg)) %>% 
                                  mutate(country = mcp_grouping) %>%
                                  rename(sub_obj_avg = avg_sub_obj_avg) %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg) %>%
                                  select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg)) %>%
                mutate(y_axis_order = case_when(year == 2018 ~ 1, 
                                                year == 2020 ~ 2,
                                                year == 2019 ~ 3),
                       sub_obj_avg = case_when(y_axis_order == 2 ~ sub_obj_avg,
                                                        TRUE ~ NA_real_),
                       var = mcp_grouping, values = sub_obj_avg,
                       dot_label = case_when(country == current_country ~ country,
                                             country %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs", "EU-15") ~ mcp_grouping,
                                             TRUE ~ "Other"),
                       dot_label_plot_order = case_when(dot_label == "Other" ~ 1,
                                                        dot_label == "EU-15" ~ 2,
                                                        dot_label == "CARs" ~ 3,
                                                        dot_label == "E&E graduates" ~ 4,
                                                        dot_label == "E&E Eurasia" ~ 5,
                                                        dot_label == "E&E Balkans" ~ 6,
                                                        dot_label == current_country ~ 7)) %>%
                arrange(dot_label_plot_order) %>%
                mutate(color_bin = dot_label,
                       color = case_when(color_bin == current_country ~ color_palette %>% slice(11) %>% pull(hex),
                                         color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                         color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                         color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                         color_bin == "CARs" ~ color_palette %>% slice(5) %>% pull(hex),
                                         # color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                         color_bin == "EU-15" ~ color_palette %>% slice(7) %>% pull(hex),
                                         # color_bin == "U.S." ~ color_palette %>% slice(8) %>% pull(hex)),
                                         color_bin == "Other" ~ color_palette %>% slice(4) %>% pull(hex)),
                       point_size = case_when(color_bin == "Other" ~ 2,
                                              TRUE ~ 3),
                       point_shape = case_when(color_bin == "Other" ~ 1,
                                               TRUE ~ 19),
                       point_stroke = case_when(color_bin == "Other" ~ 1,
                                                TRUE ~ 1))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color)
        
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color_bin)
        
        # inspect
        # chart_data_color_list
        
        
        # create chart_data_shape_list for to pass to scale_shape_manual
        chart_data_shape_list <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(point_shape)
        
        names(chart_data_shape_list) <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(color_bin)
        
        # inspect
        # chart_data_shape_list
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        sub_obj_3_2_country_profile_dot_plot <- chart_data %>% 
                ggplot(data = ., mapping = aes(x = values, y = y_axis_order, 
                                               color = color_bin, shape = color_bin)) + 
                scale_color_manual(values = chart_data_color_list, guide = "none") +
                # scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
                scale_y_continuous(breaks = NULL, limits = c(1, 3), expand = c(0, 0),
                                   labels = number_format(accuracy = 1)) +
                scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.02, 1.02)) +
                labs(x = NULL, y = sub_obj_short_name_placeholder, 
                     caption = NULL, color = "", linetype = "") +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = -.02,
                #                            y = 1, yend = 3), color = "#DDDDDD", size = .25) +
                geom_segment(data = chart_data,
                             mapping = aes(x = -.02, xend = 1.02,
                                           y = 1, yend = 1), color = "#DDDDDD", size = 2.5) +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = 1,
                #                            y = 3., yend = 3), color = "#DDDDDD", size = .25) +
                geom_point(mapping = aes(size = point_size, stroke = point_stroke), show.legend = FALSE) +
                scale_shape_manual(values = chart_data_shape_list) +
                scale_size_continuous(range = c(4, 6)) +
                # coord_fixed(ratio = 1 / 20, clip = "off") +
                theme_bw() +
                theme(
                        aspect.ratio = 1 / 10,
                        # plot.background = element_rect(fill = "#E0ECF7"),
                        # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                        # plot.background = element_rect(fill = "transparent", color = NA),
                        # plot.background = element_blank(),
                        # plot.margin = unit(c(0, -8, 0, 0), "mm"),
                        plot.margin = unit(c(0, 0, 0, 0), "mm"),
                        plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Gill Sans MT", 
                                                    color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                        # text = element_text(family = "Gill Sans MT", size = 46, face = "plain", color = "#000000"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        # panel.grid.major.y = element_line(color = "#DDDDDD"),
                        # panel.background = element_rect(fill = "transparent"), 
                        panel.background = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.border = element_blank(),
                        # panel.border = element_rect(color = "#DDDDDD", size = .5),
                        # panel.grid = element_blank(),
                        # line = element_blank(),
                        # rect = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.ticks.x = element_blank(),
                        # axis.ticks.x = element_line(size = .5),
                        # axis.ticks.length.y.left = unit(.2, "cm"),
                        axis.ticks.length.x.bottom = unit(.1, "cm"),
                        axis.text.x = element_blank(),
                        # axis.text.x = element_text(family = "Gill Sans MT", face = "plain", size = 9, color = "#333333", 
                        #                            margin = margin(t = 10, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                        axis.text.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                   margin = margin(t = 0, r = -20, b = 0, l = 0)),
                        # axis.line.x.bottom = element_line(color = "#333333"),
                        axis.line.x.bottom = element_blank(),
                        axis.line.y.left = element_blank(),
                        axis.title.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", 
                                                    margin = margin(t = 13, r = 0, b = 5, l = 0)),
                        axis.title.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                    vjust = .5, margin = margin(t = 0, r = 0, b = 0, l = 5)),
                        # axis.title.y = element_blank(),
                        # axis.text.y = element_blank(),
                        plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Gill Sans", color = "#333333", 
                                                  margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                        legend.position = "bottom",
                        # legend.key.size = unit(2, "mm"), 
                        legend.title = element_text(size = 10, family = "Gill Sans MT", face = "plain", color = "#333333"),
                        legend.text = element_text(size = 8, family = "Gill Sans MT", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                   hjust = .5, color = "#333333")
                        # legend.spacing.y = unit(5.5, "cm"),
                        # legend.key = element_rect(size = 5),
                        # legend.key.size = unit(2, 'lines')
                ) 
        # guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333", keywidth = 4),
        #        linetype = guide_legend(keywidth = 4))
        
        
        # sub_obj_3_2_country_profile_dot_plot
        
        
        #///////////////////////
        
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/sub_obj_3_2_country_profile_dot_plot_", 
                            str_to_lower(current_country), ".png"), plot = sub_obj_3_2_country_profile_dot_plot, 
               dpi = 300, height = 6, width = 6)
        
        # crop png
        # image_read(str_c("output/charts/country_profile/sub_obj_3_2_country_profile_dot_plot_", 
        #                  str_to_lower(current_country), ".png")) %>% image_info()
        sub_obj_3_2_country_profile_dot_plot_png <- image_read(str_c("output/charts/country_profile/sub_obj_3_2_country_profile_dot_plot_", 
                                                                     str_to_lower(current_country), ".png")) %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 153, x_off = 0, y_off = 825))
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # sub_obj_3_3_country_profile_dot_plot ####
        current_sub_obj_num <- "sub_obj_3_3"
        sub_obj_short_name_placeholder <- str_c(rep(x = " ", times = 32), collapse = "")
        
        
        # get chart_data
        chart_data <- fmir %>% 
                filter(year %in% c(2020, 2019, 2018), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>% 
                select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg) %>% distinct() %>%
                bind_rows(., fmir %>% filter(year %in% c(2020, 2019, 2018), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>%
                                  group_by(mcp_grouping, year) %>% mutate(avg_sub_obj_avg = mean(sub_obj_avg, na.rm = TRUE)) %>%
                                  ungroup() %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg, avg_sub_obj_avg) %>%
                                  arrange(mcp_grouping) %>%
                                  select(-c(country, sub_obj_avg)) %>% 
                                  mutate(country = mcp_grouping) %>%
                                  rename(sub_obj_avg = avg_sub_obj_avg) %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg) %>%
                                  select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg)) %>%
                mutate(y_axis_order = case_when(year == 2018 ~ 1, 
                                                year == 2020 ~ 2,
                                                year == 2019 ~ 3),
                       sub_obj_avg = case_when(y_axis_order == 2 ~ sub_obj_avg,
                                                        TRUE ~ NA_real_),
                       var = mcp_grouping, values = sub_obj_avg,
                       dot_label = case_when(country == current_country ~ country,
                                             country %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs", "EU-15") ~ mcp_grouping,
                                             TRUE ~ "Other"),
                       dot_label_plot_order = case_when(dot_label == "Other" ~ 1,
                                                        dot_label == "EU-15" ~ 2,
                                                        dot_label == "CARs" ~ 3,
                                                        dot_label == "E&E graduates" ~ 4,
                                                        dot_label == "E&E Eurasia" ~ 5,
                                                        dot_label == "E&E Balkans" ~ 6,
                                                        dot_label == current_country ~ 7)) %>%
                arrange(dot_label_plot_order) %>%
                mutate(color_bin = dot_label,
                       color = case_when(color_bin == current_country ~ color_palette %>% slice(11) %>% pull(hex),
                                         color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                         color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                         color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                         color_bin == "CARs" ~ color_palette %>% slice(5) %>% pull(hex),
                                         # color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                         color_bin == "EU-15" ~ color_palette %>% slice(7) %>% pull(hex),
                                         # color_bin == "U.S." ~ color_palette %>% slice(8) %>% pull(hex)),
                                         color_bin == "Other" ~ color_palette %>% slice(4) %>% pull(hex)),
                       point_size = case_when(color_bin == "Other" ~ 2,
                                              TRUE ~ 3),
                       point_shape = case_when(color_bin == "Other" ~ 1,
                                               TRUE ~ 19),
                       point_stroke = case_when(color_bin == "Other" ~ 1,
                                                TRUE ~ 1))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color)
        
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color_bin)
        
        # inspect
        # chart_data_color_list
        
        
        # create chart_data_shape_list for to pass to scale_shape_manual
        chart_data_shape_list <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(point_shape)
        
        names(chart_data_shape_list) <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(color_bin)
        
        # inspect
        # chart_data_shape_list
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        sub_obj_3_3_country_profile_dot_plot <- chart_data %>% 
                ggplot(data = ., mapping = aes(x = values, y = y_axis_order, 
                                               color = color_bin, shape = color_bin)) + 
                scale_color_manual(values = chart_data_color_list, guide = "none") +
                # scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
                scale_y_continuous(breaks = NULL, limits = c(1, 3), expand = c(0, 0),
                                   labels = number_format(accuracy = 1)) +
                scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.02, 1.02)) +
                labs(x = NULL, y = sub_obj_short_name_placeholder, 
                     caption = NULL, color = "", linetype = "") +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = -.02,
                #                            y = 1, yend = 3), color = "#DDDDDD", size = .25) +
                geom_segment(data = chart_data,
                             mapping = aes(x = -.02, xend = 1.02,
                                           y = 1, yend = 1), color = "#333333", size = 1.5) +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = 1,
                #                            y = 3., yend = 3), color = "#DDDDDD", size = .25) +
                geom_point(mapping = aes(size = point_size, stroke = point_stroke), show.legend = FALSE) +
                scale_shape_manual(values = chart_data_shape_list) +
                scale_size_continuous(range = c(4, 6)) +
                # coord_fixed(ratio = 1 / 20, clip = "off") +
                theme_bw() +
                theme(
                        aspect.ratio = 1 / 10,
                        # plot.background = element_rect(fill = "#E0ECF7"),
                        # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                        # plot.background = element_rect(fill = "transparent", color = NA),
                        # plot.background = element_blank(),
                        # plot.margin = unit(c(0, -8, 0, 0), "mm"),
                        plot.margin = unit(c(2, 0, 2, 0), "mm"),
                        plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Gill Sans MT", 
                                                    color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                        # text = element_text(family = "Gill Sans MT", size = 46, face = "plain", color = "#000000"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        # panel.grid.major.y = element_line(color = "#DDDDDD"),
                        # panel.background = element_rect(fill = "transparent"), 
                        panel.background = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.border = element_blank(),
                        # panel.border = element_rect(color = "#DDDDDD", size = .5),
                        # panel.grid = element_blank(),
                        # line = element_blank(),
                        # rect = element_blank(),
                        axis.ticks.y = element_blank(),
                        # axis.ticks.x = element_blank(),
                        axis.ticks.x = element_line(size = .5),
                        # axis.ticks.length.y.left = unit(.2, "cm"),
                        axis.ticks.length.x.bottom = unit(.1, "cm"),
                        # axis.text.x = element_blank(),
                        axis.text.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333",
                                                   margin = margin(t = 2, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                        axis.text.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                   margin = margin(t = 0, r = -20, b = 0, l = 0)),
                        # axis.line.x.bottom = element_line(color = "#333333"),
                        axis.line.x.bottom = element_blank(),
                        axis.line.y.left = element_blank(),
                        axis.title.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", 
                                                    margin = margin(t = 13, r = 0, b = 5, l = 0)),
                        axis.title.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                    vjust = .5, margin = margin(t = 0, r = 0, b = 0, l = 5)),
                        # axis.title.y = element_blank(),
                        # axis.text.y = element_blank(),
                        plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Gill Sans", color = "#333333", 
                                                  margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                        legend.position = "bottom",
                        # legend.key.size = unit(2, "mm"), 
                        legend.title = element_text(size = 10, family = "Gill Sans MT", face = "plain", color = "#333333"),
                        legend.text = element_text(size = 8, family = "Gill Sans MT", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                   hjust = .5, color = "#333333")
                        # legend.spacing.y = unit(5.5, "cm"),
                        # legend.key = element_rect(size = 5),
                        # legend.key.size = unit(2, 'lines')
                ) 
        # guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333", keywidth = 4),
        #        linetype = guide_legend(keywidth = 4))
        
        
        # sub_obj_3_3_country_profile_dot_plot
        
        
        #///////////////////////
        
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/sub_obj_3_3_country_profile_dot_plot_", 
                            str_to_lower(current_country), ".png"), plot = sub_obj_3_3_country_profile_dot_plot, 
               dpi = 300, height = 6, width = 6)
        
        # crop png
        # image_read(str_c("output/charts/country_profile/sub_obj_3_3_country_profile_dot_plot_", 
        #                  str_to_lower(current_country), ".png")) %>% image_info()
        sub_obj_3_3_country_profile_dot_plot_png <- image_read(str_c("output/charts/country_profile/sub_obj_3_3_country_profile_dot_plot_", 
                                                                     str_to_lower(current_country), ".png")) %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 200, x_off = 0, y_off = 810))
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # sub_obj_4_1_country_profile_dot_plot ####
        current_sub_obj_num <- "sub_obj_4_1"
        sub_obj_short_name_placeholder <- str_c(rep(x = " ", times = 32), collapse = "")
        
        
        # get chart_data
        chart_data <- fmir %>% 
                filter(year %in% c(2020, 2019, 2018), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>% 
                select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg) %>% distinct() %>%
                bind_rows(., fmir %>% filter(year %in% c(2020, 2019, 2018), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>%
                                  group_by(mcp_grouping, year) %>% mutate(avg_sub_obj_avg = mean(sub_obj_avg, na.rm = TRUE)) %>%
                                  ungroup() %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg, avg_sub_obj_avg) %>%
                                  arrange(mcp_grouping) %>%
                                  select(-c(country, sub_obj_avg)) %>% 
                                  mutate(country = mcp_grouping) %>%
                                  rename(sub_obj_avg = avg_sub_obj_avg) %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg) %>%
                                  select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg)) %>%
                mutate(y_axis_order = case_when(year == 2018 ~ 1, 
                                                year == 2020 ~ 2,
                                                year == 2019 ~ 3),
                       sub_obj_avg = case_when(y_axis_order == 2 ~ sub_obj_avg,
                                                        TRUE ~ NA_real_),
                       var = mcp_grouping, values = sub_obj_avg,
                       dot_label = case_when(country == current_country ~ country,
                                             country %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs", "EU-15") ~ mcp_grouping,
                                             TRUE ~ "Other"),
                       dot_label_plot_order = case_when(dot_label == "Other" ~ 1,
                                                        dot_label == "EU-15" ~ 2,
                                                        dot_label == "CARs" ~ 3,
                                                        dot_label == "E&E graduates" ~ 4,
                                                        dot_label == "E&E Eurasia" ~ 5,
                                                        dot_label == "E&E Balkans" ~ 6,
                                                        dot_label == current_country ~ 7)) %>%
                arrange(dot_label_plot_order) %>%
                mutate(color_bin = dot_label,
                       color = case_when(color_bin == current_country ~ color_palette %>% slice(11) %>% pull(hex),
                                         color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                         color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                         color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                         color_bin == "CARs" ~ color_palette %>% slice(5) %>% pull(hex),
                                         # color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                         color_bin == "EU-15" ~ color_palette %>% slice(7) %>% pull(hex),
                                         # color_bin == "U.S." ~ color_palette %>% slice(8) %>% pull(hex)),
                                         color_bin == "Other" ~ color_palette %>% slice(4) %>% pull(hex)),
                       point_size = case_when(color_bin == "Other" ~ 2,
                                              TRUE ~ 3),
                       point_shape = case_when(color_bin == "Other" ~ 1,
                                               TRUE ~ 19),
                       point_stroke = case_when(color_bin == "Other" ~ 1,
                                                TRUE ~ 1))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color)
        
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color_bin)
        
        # inspect
        # chart_data_color_list
        
        
        # create chart_data_shape_list for to pass to scale_shape_manual
        chart_data_shape_list <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(point_shape)
        
        names(chart_data_shape_list) <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(color_bin)
        
        # inspect
        # chart_data_shape_list
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        sub_obj_4_1_country_profile_dot_plot <- chart_data %>% 
                ggplot(data = ., mapping = aes(x = values, y = y_axis_order, 
                                               color = color_bin, shape = color_bin)) + 
                scale_color_manual(values = chart_data_color_list, guide = "none") +
                # scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
                scale_y_continuous(breaks = NULL, limits = c(1, 3), expand = c(0, 0),
                                   labels = number_format(accuracy = 1)) +
                scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.02, 1.02)) +
                labs(x = NULL, y = sub_obj_short_name_placeholder, 
                     caption = NULL, color = "", linetype = "") +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = -.02,
                #                            y = 1, yend = 3), color = "#DDDDDD", size = .25) +
                geom_segment(data = chart_data,
                             mapping = aes(x = -.02, xend = 1.02,
                                           y = 1, yend = 1), color = "#DDDDDD", size = 2.5) +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = 1,
                #                            y = 3., yend = 3), color = "#DDDDDD", size = .25) +
                geom_point(mapping = aes(size = point_size, stroke = point_stroke), show.legend = FALSE) +
                scale_shape_manual(values = chart_data_shape_list) +
                scale_size_continuous(range = c(4, 6)) +
                # coord_fixed(ratio = 1 / 20, clip = "off") +
                theme_bw() +
                theme(
                        aspect.ratio = 1 / 10,
                        # plot.background = element_rect(fill = "#E0ECF7"),
                        # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                        # plot.background = element_rect(fill = "transparent", color = NA),
                        # plot.background = element_blank(),
                        # plot.margin = unit(c(0, -8, 0, 0), "mm"),
                        plot.margin = unit(c(0, 0, 0, 0), "mm"),
                        plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Gill Sans MT", 
                                                    color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                        # text = element_text(family = "Gill Sans MT", size = 46, face = "plain", color = "#000000"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        # panel.grid.major.y = element_line(color = "#DDDDDD"),
                        # panel.background = element_rect(fill = "transparent"), 
                        panel.background = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.border = element_blank(),
                        # panel.border = element_rect(color = "#DDDDDD", size = .5),
                        # panel.grid = element_blank(),
                        # line = element_blank(),
                        # rect = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.ticks.x = element_blank(),
                        # axis.ticks.x = element_line(size = .5),
                        # axis.ticks.length.y.left = unit(.2, "cm"),
                        axis.ticks.length.x.bottom = unit(.1, "cm"),
                        axis.text.x = element_blank(),
                        # axis.text.x = element_text(family = "Gill Sans MT", face = "plain", size = 9, color = "#333333", 
                        #                            margin = margin(t = 10, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                        axis.text.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                   margin = margin(t = 0, r = -20, b = 0, l = 0)),
                        # axis.line.x.bottom = element_line(color = "#333333"),
                        axis.line.x.bottom = element_blank(),
                        axis.line.y.left = element_blank(),
                        axis.title.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", 
                                                    margin = margin(t = 13, r = 0, b = 5, l = 0)),
                        axis.title.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                    vjust = .5, margin = margin(t = 0, r = 0, b = 0, l = 5)),
                        # axis.title.y = element_blank(),
                        # axis.text.y = element_blank(),
                        plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Gill Sans", color = "#333333", 
                                                  margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                        legend.position = "bottom",
                        # legend.key.size = unit(2, "mm"), 
                        legend.title = element_text(size = 10, family = "Gill Sans MT", face = "plain", color = "#333333"),
                        legend.text = element_text(size = 8, family = "Gill Sans MT", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                   hjust = .5, color = "#333333")
                        # legend.spacing.y = unit(5.5, "cm"),
                        # legend.key = element_rect(size = 5),
                        # legend.key.size = unit(2, 'lines')
                ) 
        # guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333", keywidth = 4),
        #        linetype = guide_legend(keywidth = 4))
        
        
        # sub_obj_4_1_country_profile_dot_plot
        
        
        #///////////////////////
        
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/sub_obj_4_1_country_profile_dot_plot_", 
                            str_to_lower(current_country), ".png"), plot = sub_obj_4_1_country_profile_dot_plot, 
               dpi = 300, height = 6, width = 6)
        
        # crop png
        # image_read(str_c("output/charts/country_profile/sub_obj_4_1_country_profile_dot_plot_", 
        #                  str_to_lower(current_country), ".png")) %>% image_info()
        sub_obj_4_1_country_profile_dot_plot_png <- image_read(str_c("output/charts/country_profile/sub_obj_4_1_country_profile_dot_plot_", 
                                                                     str_to_lower(current_country), ".png")) %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 153, x_off = 0, y_off = 825))
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # sub_obj_4_2_country_profile_dot_plot ####
        current_sub_obj_num <- "sub_obj_4_2"
        sub_obj_short_name_placeholder <- str_c(rep(x = " ", times = 32), collapse = "")
        
        
        # get chart_data
        chart_data <- fmir %>% 
                filter(year %in% c(2020, 2019, 2018), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>% 
                select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg) %>% distinct() %>%
                bind_rows(., fmir %>% filter(year %in% c(2020, 2019, 2018), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>%
                                  group_by(mcp_grouping, year) %>% mutate(avg_sub_obj_avg = mean(sub_obj_avg, na.rm = TRUE)) %>%
                                  ungroup() %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg, avg_sub_obj_avg) %>%
                                  arrange(mcp_grouping) %>%
                                  select(-c(country, sub_obj_avg)) %>% 
                                  mutate(country = mcp_grouping) %>%
                                  rename(sub_obj_avg = avg_sub_obj_avg) %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg) %>%
                                  select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg)) %>%
                mutate(y_axis_order = case_when(year == 2018 ~ 1, 
                                                year == 2020 ~ 2,
                                                year == 2019 ~ 3),
                       sub_obj_avg = case_when(y_axis_order == 2 ~ sub_obj_avg,
                                                        TRUE ~ NA_real_),
                       var = mcp_grouping, values = sub_obj_avg,
                       dot_label = case_when(country == current_country ~ country,
                                             country %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs", "EU-15") ~ mcp_grouping,
                                             TRUE ~ "Other"),
                       dot_label_plot_order = case_when(dot_label == "Other" ~ 1,
                                                        dot_label == "EU-15" ~ 2,
                                                        dot_label == "CARs" ~ 3,
                                                        dot_label == "E&E graduates" ~ 4,
                                                        dot_label == "E&E Eurasia" ~ 5,
                                                        dot_label == "E&E Balkans" ~ 6,
                                                        dot_label == current_country ~ 7)) %>%
                arrange(dot_label_plot_order) %>%
                mutate(color_bin = dot_label,
                       color = case_when(color_bin == current_country ~ color_palette %>% slice(11) %>% pull(hex),
                                         color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                         color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                         color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                         color_bin == "CARs" ~ color_palette %>% slice(5) %>% pull(hex),
                                         # color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                         color_bin == "EU-15" ~ color_palette %>% slice(7) %>% pull(hex),
                                         # color_bin == "U.S." ~ color_palette %>% slice(8) %>% pull(hex)),
                                         color_bin == "Other" ~ color_palette %>% slice(4) %>% pull(hex)),
                       point_size = case_when(color_bin == "Other" ~ 2,
                                              TRUE ~ 3),
                       point_shape = case_when(color_bin == "Other" ~ 1,
                                               TRUE ~ 19),
                       point_stroke = case_when(color_bin == "Other" ~ 1,
                                                TRUE ~ 1))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color)
        
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color_bin)
        
        # inspect
        # chart_data_color_list
        
        
        # create chart_data_shape_list for to pass to scale_shape_manual
        chart_data_shape_list <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(point_shape)
        
        names(chart_data_shape_list) <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(color_bin)
        
        # inspect
        # chart_data_shape_list
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        sub_obj_4_2_country_profile_dot_plot <- chart_data %>% 
                ggplot(data = ., mapping = aes(x = values, y = y_axis_order, 
                                               color = color_bin, shape = color_bin)) + 
                scale_color_manual(values = chart_data_color_list, guide = "none") +
                # scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
                scale_y_continuous(breaks = NULL, limits = c(1, 3), expand = c(0, 0),
                                   labels = number_format(accuracy = 1)) +
                scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.02, 1.02)) +
                labs(x = NULL, y = sub_obj_short_name_placeholder, 
                     caption = NULL, color = "", linetype = "") +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = -.02,
                #                            y = 1, yend = 3), color = "#DDDDDD", size = .25) +
                geom_segment(data = chart_data,
                             mapping = aes(x = -.02, xend = 1.02,
                                           y = 1, yend = 1), color = "#333333", size = 1.5) +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = 1,
                #                            y = 3., yend = 3), color = "#DDDDDD", size = .25) +
                geom_point(mapping = aes(size = point_size, stroke = point_stroke), show.legend = FALSE) +
                scale_shape_manual(values = chart_data_shape_list) +
                scale_size_continuous(range = c(4, 6)) +
                # coord_fixed(ratio = 1 / 20, clip = "off") +
                theme_bw() +
                theme(
                        aspect.ratio = 1 / 10,
                        # plot.background = element_rect(fill = "#E0ECF7"),
                        # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                        # plot.background = element_rect(fill = "transparent", color = NA),
                        # plot.background = element_blank(),
                        # plot.margin = unit(c(0, -8, 0, 0), "mm"),
                        plot.margin = unit(c(2, 0, 2, 0), "mm"),
                        plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Gill Sans MT", 
                                                    color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                        # text = element_text(family = "Gill Sans MT", size = 46, face = "plain", color = "#000000"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        # panel.grid.major.y = element_line(color = "#DDDDDD"),
                        # panel.background = element_rect(fill = "transparent"), 
                        panel.background = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.border = element_blank(),
                        # panel.border = element_rect(color = "#DDDDDD", size = .5),
                        # panel.grid = element_blank(),
                        # line = element_blank(),
                        # rect = element_blank(),
                        axis.ticks.y = element_blank(),
                        # axis.ticks.x = element_blank(),
                        axis.ticks.x = element_line(size = .5),
                        # axis.ticks.length.y.left = unit(.2, "cm"),
                        axis.ticks.length.x.bottom = unit(.1, "cm"),
                        # axis.text.x = element_blank(),
                        axis.text.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333",
                                                   margin = margin(t = 2, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                        axis.text.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                   margin = margin(t = 0, r = -20, b = 0, l = 0)),
                        # axis.line.x.bottom = element_line(color = "#333333"),
                        axis.line.x.bottom = element_blank(),
                        axis.line.y.left = element_blank(),
                        axis.title.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", 
                                                    margin = margin(t = 13, r = 0, b = 5, l = 0)),
                        axis.title.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                    vjust = .5, margin = margin(t = 0, r = 0, b = 0, l = 5)),
                        # axis.title.y = element_blank(),
                        # axis.text.y = element_blank(),
                        plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Gill Sans", color = "#333333", 
                                                  margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                        legend.position = "bottom",
                        # legend.key.size = unit(2, "mm"), 
                        legend.title = element_text(size = 10, family = "Gill Sans MT", face = "plain", color = "#333333"),
                        legend.text = element_text(size = 8, family = "Gill Sans MT", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                   hjust = .5, color = "#333333")
                        # legend.spacing.y = unit(5.5, "cm"),
                        # legend.key = element_rect(size = 5),
                        # legend.key.size = unit(2, 'lines')
                ) 
        # guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333", keywidth = 4),
        #        linetype = guide_legend(keywidth = 4))
        
        
        # sub_obj_4_2_country_profile_dot_plot
        
        
        #///////////////////////
        
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/sub_obj_4_2_country_profile_dot_plot_", 
                            str_to_lower(current_country), ".png"), plot = sub_obj_4_2_country_profile_dot_plot, 
               dpi = 300, height = 6, width = 6)
        
        # crop png
        # image_read(str_c("output/charts/country_profile/sub_obj_4_2_country_profile_dot_plot_", 
        #                  str_to_lower(current_country), ".png")) %>% image_info()
        sub_obj_4_2_country_profile_dot_plot_png <- image_read(str_c("output/charts/country_profile/sub_obj_4_2_country_profile_dot_plot_", 
                                                                     str_to_lower(current_country), ".png")) %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 200, x_off = 0, y_off = 810))
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # sub_obj_c_country_profile_dot_plot ####
        current_sub_obj_num <- "sub_obj_c"
        sub_obj_short_name_placeholder <- str_c(rep(x = " ", times = 32), collapse = "")
        
        
        # get chart_data
        chart_data <- fmir %>% 
                filter(year %in% c(2020, 2019, 2018), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>% 
                select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg) %>% distinct() %>%
                bind_rows(., fmir %>% filter(year %in% c(2020, 2019, 2018), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>%
                                  group_by(mcp_grouping, year) %>% mutate(avg_sub_obj_avg = mean(sub_obj_avg, na.rm = TRUE)) %>%
                                  ungroup() %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg, avg_sub_obj_avg) %>%
                                  arrange(mcp_grouping) %>%
                                  select(-c(country, sub_obj_avg)) %>% 
                                  mutate(country = mcp_grouping) %>%
                                  rename(sub_obj_avg = avg_sub_obj_avg) %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg) %>%
                                  select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg)) %>%
                mutate(y_axis_order = case_when(year == 2018 ~ 1, 
                                                year == 2020 ~ 2,
                                                year == 2019 ~ 3),
                       sub_obj_avg = case_when(y_axis_order == 2 ~ sub_obj_avg,
                                                        TRUE ~ NA_real_),
                       var = mcp_grouping, values = sub_obj_avg,
                       dot_label = case_when(country == current_country ~ country,
                                             country %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs", "EU-15") ~ mcp_grouping,
                                             TRUE ~ "Other"),
                       dot_label_plot_order = case_when(dot_label == "Other" ~ 1,
                                                        dot_label == "EU-15" ~ 2,
                                                        dot_label == "CARs" ~ 3,
                                                        dot_label == "E&E graduates" ~ 4,
                                                        dot_label == "E&E Eurasia" ~ 5,
                                                        dot_label == "E&E Balkans" ~ 6,
                                                        dot_label == current_country ~ 7)) %>%
                arrange(dot_label_plot_order) %>%
                mutate(color_bin = dot_label,
                       color = case_when(color_bin == current_country ~ color_palette %>% slice(11) %>% pull(hex),
                                         color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                         color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                         color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                         color_bin == "CARs" ~ color_palette %>% slice(5) %>% pull(hex),
                                         # color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                         color_bin == "EU-15" ~ color_palette %>% slice(7) %>% pull(hex),
                                         # color_bin == "U.S." ~ color_palette %>% slice(8) %>% pull(hex)),
                                         color_bin == "Other" ~ color_palette %>% slice(4) %>% pull(hex)),
                       point_size = case_when(color_bin == "Other" ~ 2,
                                              TRUE ~ 3),
                       point_shape = case_when(color_bin == "Other" ~ 1,
                                               TRUE ~ 19),
                       point_stroke = case_when(color_bin == "Other" ~ 1,
                                                TRUE ~ 1))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color)
        
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color_bin)
        
        # inspect
        # chart_data_color_list
        
        
        # create chart_data_shape_list for to pass to scale_shape_manual
        chart_data_shape_list <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(point_shape)
        
        names(chart_data_shape_list) <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(color_bin)
        
        # inspect
        # chart_data_shape_list
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        sub_obj_c_country_profile_dot_plot <- chart_data %>% 
                ggplot(data = ., mapping = aes(x = values, y = y_axis_order, 
                                               color = color_bin, shape = color_bin)) + 
                scale_color_manual(values = chart_data_color_list, guide = "none") +
                # scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
                scale_y_continuous(breaks = NULL, limits = c(1, 3), expand = c(0, 0),
                                   labels = number_format(accuracy = 1)) +
                scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.02, 1.02)) +
                labs(x = NULL, y = sub_obj_short_name_placeholder, 
                     caption = NULL, color = "", linetype = "") +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = -.02,
                #                            y = 1, yend = 3), color = "#DDDDDD", size = .25) +
                geom_segment(data = chart_data,
                             mapping = aes(x = -.02, xend = 1.02,
                                           y = 1, yend = 1), color = "#333333", size = 1.5) +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = 1,
                #                            y = 3., yend = 3), color = "#DDDDDD", size = .25) +
                geom_point(mapping = aes(size = point_size, stroke = point_stroke), show.legend = FALSE) +
                scale_shape_manual(values = chart_data_shape_list) +
                scale_size_continuous(range = c(4, 6)) +
                # coord_fixed(ratio = 1 / 20, clip = "off") +
                theme_bw() +
                theme(
                        aspect.ratio = 1 / 10,
                        # plot.background = element_rect(fill = "#E0ECF7"),
                        # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                        # plot.background = element_rect(fill = "transparent", color = NA),
                        # plot.background = element_blank(),
                        # plot.margin = unit(c(0, -8, 0, 0), "mm"),
                        plot.margin = unit(c(2, 0, 2, 0), "mm"),
                        plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Gill Sans MT", 
                                                    color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                        # text = element_text(family = "Gill Sans MT", size = 46, face = "plain", color = "#000000"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        # panel.grid.major.y = element_line(color = "#DDDDDD"),
                        # panel.background = element_rect(fill = "transparent"), 
                        panel.background = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.border = element_blank(),
                        # panel.border = element_rect(color = "#DDDDDD", size = .5),
                        # panel.grid = element_blank(),
                        # line = element_blank(),
                        # rect = element_blank(),
                        axis.ticks.y = element_blank(),
                        # axis.ticks.x = element_blank(),
                        axis.ticks.x = element_line(size = .5),
                        # axis.ticks.length.y.left = unit(.2, "cm"),
                        axis.ticks.length.x.bottom = unit(.1, "cm"),
                        # axis.text.x = element_blank(),
                        axis.text.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333",
                                                   margin = margin(t = 2, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                        axis.text.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                   margin = margin(t = 0, r = -20, b = 0, l = 0)),
                        # axis.line.x.bottom = element_line(color = "#333333"),
                        axis.line.x.bottom = element_blank(),
                        axis.line.y.left = element_blank(),
                        axis.title.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", 
                                                    margin = margin(t = 13, r = 0, b = 5, l = 0)),
                        axis.title.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                    vjust = .5, margin = margin(t = 0, r = 0, b = 0, l = 5)),
                        # axis.title.y = element_blank(),
                        # axis.text.y = element_blank(),
                        plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Gill Sans", color = "#333333", 
                                                  margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                        legend.position = "bottom",
                        # legend.key.size = unit(2, "mm"), 
                        legend.title = element_text(size = 10, family = "Gill Sans MT", face = "plain", color = "#333333"),
                        legend.text = element_text(size = 8, family = "Gill Sans MT", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                   hjust = .5, color = "#333333")
                        # legend.spacing.y = unit(5.5, "cm"),
                        # legend.key = element_rect(size = 5),
                        # legend.key.size = unit(2, 'lines')
                ) 
        # guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333", keywidth = 4),
        #        linetype = guide_legend(keywidth = 4))
        
        
        # sub_obj_c_country_profile_dot_plot
        
        
        #///////////////////////
        
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/sub_obj_c_country_profile_dot_plot_", 
                            str_to_lower(current_country), ".png"), plot = sub_obj_c_country_profile_dot_plot, 
               dpi = 300, height = 6, width = 6)
        
        # crop png
        # image_read(str_c("output/charts/country_profile/sub_obj_c_country_profile_dot_plot_", 
        #                  str_to_lower(current_country), ".png")) %>% image_info()
        sub_obj_c_country_profile_dot_plot_png <- image_read(str_c("output/charts/country_profile/sub_obj_c_country_profile_dot_plot_", 
                                                                   str_to_lower(current_country), ".png")) %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 200, x_off = 0, y_off = 810))
        
        
        #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # create rect_background ####
        
        print(str_c(current_country, " blue_rect_background"))
        
        # inspect colors
        # brewer.blues(10) %>% show_col() # obj_dot_plots "#E0ECF7", 
        # ri_dot_plot "#002F6C" is usaid blue: # https://www.usaid.gov/sites/default/files/documents/1869/USAID_GSM-02_04_2020.pdf
        # brewer.greys(10) %>% show_col() # "#DEDEDE"
        # color_palette %>% pull(hex) %>% show_col() # "#CBCBCB" is too light
        
        
        #//////////////////////////////
        
        
        # top/bottom margin are height of magnifying glass
        one_row_blue_rect_background <- tibble(x = c(0, 0, 2, 2), y = c(0, 2, 2, 0)) %>%
                ggplot(data = ., mapping = aes(x = x, y = y)) +
                geom_shape(radius = unit(.5, 'cm'), fill = "#E0ECF7") +
                scale_x_continuous(limits = c(0, 2)) +
                scale_y_continuous(limits = c(0, 2)) +
                theme_nothing() +
                theme(aspect.ratio = 1 / 5.3) 
        
        # one_row_blue_rect_background
        ggsave(filename = "output/charts/country_profile/one_row_blue_rect_background.png", 
               plot = one_row_blue_rect_background, dpi = 300, width = 6, height = 6)
        one_row_blue_rect_background_png <- image_read(path = "output/charts/country_profile/one_row_blue_rect_background.png") %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 346, x_off = 0, y_off = 727))
        
        
        #//////////////////////////////
        
        
        # top/bottom margin are height of magnifying glass
        two_row_blue_rect_background <- tibble(x = c(0, 0, 2, 2), y = c(0, 2, 2, 0)) %>%
                ggplot(data = ., mapping = aes(x = x, y = y)) +
                geom_shape(radius = unit(.5, 'cm'), fill = "#E0ECF7") +
                scale_x_continuous(limits = c(0, 2)) +
                scale_y_continuous(limits = c(0, 2)) +
                theme_nothing() +
                theme(aspect.ratio = 1 / 3.8)
        
        # two_row_blue_rect_background
        ggsave(filename = "output/charts/country_profile/two_row_blue_rect_background.png", 
               plot = two_row_blue_rect_background, dpi = 300, width = 6, height = 6)
        two_row_blue_rect_background_png <- image_read(path = "output/charts/country_profile/two_row_blue_rect_background.png") %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 466, x_off = 0, y_off = 667))
        
        
        #/////////////////////
        
        
        # top/bottom margin are height of magnifying glass
        three_row_blue_rect_background <- tibble(x = c(0, 0, 2, 2), y = c(0, 2, 2, 0)) %>%
                ggplot(data = ., mapping = aes(x = x, y = y)) +
                geom_shape(radius = unit(.5, 'cm'), fill = "#E0ECF7") +
                scale_x_continuous(limits = c(0, 2)) +
                scale_y_continuous(limits = c(0, 2)) +
                theme_nothing() +
                theme(aspect.ratio = 1 / 2.95)
        
        # three_row_blue_rect_background
        ggsave(filename = "output/charts/country_profile/three_row_blue_rect_background.png", 
               plot = three_row_blue_rect_background, dpi = 300, width = 6, height = 6)
        three_row_blue_rect_background_png <- image_read(path = "output/charts/country_profile/three_row_blue_rect_background.png") %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 590, x_off = 0, y_off = 605)) 
        
        
        #/////////////////////
        
        
        # top/bottom margin are height of magnifying glass
        six_row_blue_rect_background <- tibble(x = c(0, 0, 2, 2), y = c(0, 2, 2, 0)) %>%
                ggplot(data = ., mapping = aes(x = x, y = y)) +
                geom_shape(radius = unit(.5, 'cm'), fill = "#1B69AF") +
                scale_x_continuous(limits = c(0, 2)) +
                scale_y_continuous(limits = c(0, 2)) +
                theme_nothing() +
                theme(aspect.ratio = 1 / 1.75)
        
        # six_row_blue_rect_background
        ggsave(filename = "output/charts/country_profile/six_row_blue_rect_background.png", 
               plot = six_row_blue_rect_background, dpi = 300, width = 6, height = 6)
        six_row_blue_rect_background_png <- image_read(path = "output/charts/country_profile/six_row_blue_rect_background.png") %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 1000, x_off = 0, y_off = 400))
        
        
        #////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        legend_gray_rect_background <- tibble(x = c(0, 0, 2, 2), y = c(0, 2, 2, 0)) %>%
                ggplot(data = ., mapping = aes(x = x, y = y)) +
                geom_shape(radius = unit(.5, 'cm'), fill = "#DEDEDE") +
                scale_x_continuous(limits = c(0, 2)) +
                scale_y_continuous(limits = c(0, 2)) +
                theme_nothing() +
                theme(aspect.ratio = 1 / 4.3) 
        
        # legend_gray_rect_background
        ggsave(filename = "output/charts/country_profile/legend_gray_rect_background.png", 
               plot = legend_gray_rect_background, dpi = 300, width = 6, height = 6)
        legend_gray_rect_background_png <- image_read(path = "output/charts/country_profile/legend_gray_rect_background.png") %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 420, x_off = 0, y_off = 690))
        
        
        #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # get gray_border ####
        
        line_chart_gray_border <- tibble(x = c(0, 0, 2, 2), y = c(0, 2, 2, 0)) %>%
                ggplot(data = ., mapping = aes(x = x, y = y)) +
                geom_shape(radius = unit(.5, 'cm'), color = "#CBCBCB", size = .5, fill = NA) +
                scale_x_continuous(limits = c(0, 2)) +
                scale_y_continuous(limits = c(0, 2)) +
                theme_nothing() +
                theme(aspect.ratio = 1 / 2.2) 
        line_chart_gray_border
        
        
        ggsave(filename = "output/charts/country_profile/line_chart_gray_border.png", 
               plot = line_chart_gray_border, dpi = 300, width = 6, height = 6)
        line_chart_gray_border_png <- image_read(path = "output/charts/country_profile/line_chart_gray_border.png") %>%
                # image_crop(image = ., geometry = geometry_area(width = 1700, height = 1150, x_off = 50, y_off = 325))
                image_crop(image = ., geometry = geometry_area(width = 1700, height = 800, x_off = 50, y_off = 500))
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # country_profile_line_chart ####
        
        print(str_c(current_country, " line_chart"))
        
        # plot
        # fmir %>% 
        #         filter(country == current_country) %>%
        #         mutate(values = obj_avg) %>%
        #         distinct(country, mcp_grouping, year, obj_num, values) %>%
        #         bind_rows(., fmir %>% 
        #                           filter(country == current_country) %>%
        #                           distinct(country, mcp_grouping, year, miri_avg) %>%
        #                           mutate(obj_num = "miri",
        #                                  values = miri_avg) %>%
        #                           select(obj_num, country, year, mcp_grouping, values)) %>%
        #         ggplot(data = ., mapping = aes(x = year, y = values, color = obj_num)) + geom_line()
        
        
        #//////////////////
        
        
        # add color_bin and color
        chart_data <- fmir %>% 
                filter(country == current_country) %>%
                mutate(values = obj_avg) %>%
                distinct(country, mcp_grouping, year, obj_num, values) %>%
                bind_rows(., fmir %>% 
                                  filter(country == current_country) %>%
                                  distinct(country, mcp_grouping, year, miri_avg) %>%
                                  mutate(obj_num = "miri",
                                         values = miri_avg) %>%
                                  select(obj_num, country, year, mcp_grouping, values)) %>%
                arrange(desc(obj_num)) %>%
                mutate(color_bin = case_when(obj_num == "obj_1" ~ "Democratic",
                                             obj_num == "obj_2" ~ "Information",
                                             obj_num == "obj_3" ~ "Energy",
                                             obj_num == "obj_4" ~ "Economic",
                                             obj_num == "obj_c" ~ "Corruption",
                                             obj_num == "miri" ~ "Resilience Index"),
                       color = case_when(color_bin == "Democratic" ~ color_palette %>% slice(1) %>% pull(hex),
                                         color_bin == "Information" ~ color_palette %>% slice(2) %>% pull(hex),
                                         color_bin == "Energy" ~ color_palette %>% slice(3) %>% pull(hex),
                                         color_bin == "Economic" ~ color_palette %>% slice(4) %>% pull(hex),
                                         color_bin == "Corruption" ~ color_palette %>% slice(5) %>% pull(hex),
                                         color_bin == "Resilience Index" ~ color_palette %>% slice(7) %>% pull(hex)),
                       linetype_bin = color_bin,
                       linetype = case_when(TRUE ~ "solid"))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
        chart_data_color_list
        
        # create linetype_list for to pass to scale_linetype_manual
        chart_data_linetype_list <- chart_data %>% count(linetype_bin, linetype) %>% pull(linetype)
        names(chart_data_linetype_list) <- chart_data %>% count(linetype_bin, linetype) %>% pull(linetype_bin)
        chart_data_linetype_list
        
        
        #/////////////////////
        
        
        # get label_position
        chart_data <- chart_data %>% arrange(country, year, values) %>% 
                group_by(country, year) %>%
                mutate(label_position = values,
                       dist_to_lower = label_position - lag(label_position, n = 1),
                       dist_to_higher = lead(label_position, n = 1) - label_position,
                       overlap_flag = case_when(dist_to_lower <= .04 ~ 1,
                                                TRUE ~ 0)) %>%
                ungroup()
        
        # inspect
        # chart_data
        
        # set counter
        counter <- 0
        
        # run loop to update label_position
        while((chart_data %>% mutate(sum_overlap_flag = sum(overlap_flag)) %>% slice(1) %>% pull(sum_overlap_flag)) != 0) {
                
                counter <- counter + 1
                # print(counter + 1)
                
                chart_data <- chart_data %>% arrange(country, year, values) %>% 
                        group_by(country, year) %>%
                        mutate(label_position = case_when(is.na(dist_to_lower) | dist_to_lower >= .05 ~ label_position,
                                                          TRUE ~ label_position + (.05 - dist_to_lower)),
                               dist_to_higher = lead(label_position, n = 1) - label_position,
                               dist_to_lower = label_position - lag(label_position, n = 1),
                               overlap_flag = case_when(dist_to_lower <= .04 ~ 1,
                                                        TRUE ~ 0)) %>%
                        ungroup()
        }
        
        # inspect
        # chart_data
        
        
        #/////////////////////
        
        
        # create chart
        country_profile_line_chart <- chart_data %>%
                ggplot(data = ., aes(x = year, 
                                     y = values, 
                                     color = color_bin,
                                     linetype = color_bin)) + 
                # geom_line(size = 1) +
                # geom_point(size = 2) +
                geom_text(data = chart_data %>% filter(year == max(year), color_bin == "Democratic"),
                          mapping = aes(x = year + .35, y = label_position, label = color_bin),
                          fontface = "bold", hjust = 0, size = 3.25, family = "Calibri") +
                geom_text(data = chart_data %>% filter(year == max(year), color_bin == "Information"),
                          mapping = aes(x = year + .35, y = label_position, label = color_bin),
                          fontface = "bold", hjust = 0, size = 3.25, family = "Calibri") +
                geom_text(data = chart_data %>% filter(year == max(year), color_bin == "Energy"),
                          mapping = aes(x = year + .35, y = label_position, label = color_bin),
                          fontface = "bold", hjust = 0, size = 3.25, family = "Calibri") +
                geom_text(data = chart_data %>% filter(year == max(year), color_bin == "Economic"),
                          mapping = aes(x = year + .35, y = label_position, label = color_bin),
                          fontface = "bold", hjust = 0, size = 3.25, family = "Calibri") +
                geom_text(data = chart_data %>% filter(year == max(year), color_bin == "Corruption"),
                          mapping = aes(x = year + .35, y = label_position, label = color_bin),
                          fontface = "bold", hjust = 0, size = 3.25, family = "Calibri") +
                geom_text(data = chart_data %>% filter(year == max(year), color_bin == "Resilience Index"),
                          mapping = aes(x = year + .35, y = label_position, label = color_bin),
                          fontface = "bold", hjust = 0, size = 3.25, family = "Calibri") +
                scale_color_manual(values = chart_data_color_list, guide = "none") +
                scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
                scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), limits = c(0, 1), expand = c(0, 0),
                                   labels = number_format(accuracy = .01)) +
                scale_x_continuous(breaks = seq(from = 2010, to = 2020, by = 2)) +
                labs(x = NULL, y = "Score (higher is better)", 
                     caption = NULL, color = "", linetype = "") +
                coord_fixed(ratio = 6 / 1, clip = "off") +
                # annotate(geom = "rect", xmin = 2008.5, xmax = 2018.5, ymin = 0, ymax = 1.8, alpha = 0, color = "#DDDDDD") +
                geom_segment(data = chart_data,
                             mapping = aes(x = chart_data %>% filter(year == min(year)) %>% slice(1) %>% pull(year) - .25,
                                           xend = chart_data %>% filter(year == max(year)) %>% slice(1) %>% pull(year) + .25,
                                           y = .2, yend = .2), color = "#DDDDDD", size = .75) +
                geom_segment(data = chart_data,
                             mapping = aes(x = chart_data %>% filter(year == min(year)) %>% slice(1) %>% pull(year) - .25,
                                           xend = chart_data %>% filter(year == max(year)) %>% slice(1) %>% pull(year) + .25,
                                           y = .4, yend = .4), color = "#DDDDDD", size = .75) +
                geom_segment(data = chart_data,
                             mapping = aes(x = chart_data %>% filter(year == min(year)) %>% slice(1) %>% pull(year) - .25,
                                           xend = chart_data %>% filter(year == max(year)) %>% slice(1) %>% pull(year) + .25,
                                           y = .6, yend = .6), color = "#DDDDDD", size = .75) +
                geom_segment(data = chart_data,
                             mapping = aes(x = chart_data %>% filter(year == min(year)) %>% slice(1) %>% pull(year) - .25,
                                           xend = chart_data %>% filter(year == max(year)) %>% slice(1) %>% pull(year) + .25,
                                           y = .8, yend = .8), color = "#DDDDDD", size = .75) +
                geom_segment(data = chart_data,
                             mapping = aes(x = chart_data %>% filter(year == min(year)) %>% slice(1) %>% pull(year) - .25,
                                           xend = chart_data %>% filter(year == max(year)) %>% slice(1) %>% pull(year) + .25,
                                           y = 1, yend = 1), color = "#DDDDDD", size = .75) +
                geom_segment(data = chart_data,
                             mapping = aes(x = chart_data %>% filter(year == min(year)) %>% slice(1) %>% pull(year) - .25,
                                           xend = chart_data %>% filter(year == max(year)) %>% slice(1) %>% pull(year) + .25,
                                           y = 0, yend = 0), color = "#333333", size = .75) +
                geom_line(size = 1.5) + 
                # geom_point(size = 2) +
                # geom_segment(data = segment_tbl, mapping = aes(x = x, xend = xend, y = y, yend = yend)) +
                theme_bw() +
                theme(
                        # plot.background = element_rect(fill = "blue"),
                        # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                        plot.margin = unit(c(0, 25, 0, 0), "mm"),
                        plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                                    color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                        # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        # panel.grid.major.y = element_line(color = "#DDDDDD"),
                        panel.grid.major.y = element_blank(),
                        panel.border = element_blank(),
                        # panel.border = element_rect(color = "#DDDDDD", size = .5),
                        # panel.grid = element_blank(),
                        # line = element_blank(),
                        # rect = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.ticks.x = element_line(size = .5),
                        # axis.ticks.length.y.left = unit(.2, "cm"),
                        axis.ticks.length.x.bottom = unit(.1, "cm"),
                        axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333", 
                                                   margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                        axis.text.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                                   margin = margin(t = 0, r = 0, b = 0, l = 0)),
                        # axis.line.x.bottom = element_line(color = "#333333"),
                        axis.line.x.bottom = element_blank(),
                        axis.line.y.left = element_blank(),
                        axis.title.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333", 
                                                    margin = margin(t = 13, r = 0, b = 5, l = 0)),
                        axis.title.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                                    margin = margin(t = 0, r = 10, b = 0, l = 5)),
                        # axis.title.y = element_blank(),
                        # axis.text.y = element_blank(),
                        plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                                  margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                        legend.position = "bottom",
                        # legend.key.size = unit(2, "mm"), 
                        legend.title = element_text(size = 10, family = "Calibri", face = "plain", color = "#333333"),
                        legend.text = element_text(size = 10, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                   hjust = .5, color = "#333333")
                        # legend.spacing.y = unit(5.5, "cm"),
                        # legend.key = element_rect(size = 5),
                        # legend.key.size = unit(2, 'lines')
                ) 
        # guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333", keywidth = 4),
        #        linetype = guide_legend(keywidth = 4))
        
        # inspect
        # country_profile_line_chart
        
        
        #///////////////////////
        
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/country_profile_line_chart_", 
                            str_to_lower(current_country), ".png"), plot = country_profile_line_chart, 
               dpi = 400, height = 6, width = 6)
        
        # crop png
        # image_read(str_c("output/charts/country_profile/country_profile_line_chart_", 
        #                  str_to_lower(current_country), ".png")) %>% image_info()
        country_profile_line_chart_png <- image_read(str_c("output/charts/country_profile/country_profile_line_chart_", 
                                                           str_to_lower(current_country), ".png")) %>%
                image_crop(image = ., geometry = geometry_area(width = 2350, height = 1050, x_off = 0, y_off = 650))
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # country_profile_map ####
        
        print(str_c(current_country, " map"))
        
        # https://wilkelab.org/practicalgg/articles/Winkel_tripel.html
        # note that getMap uses natural earth for country boundaries, but fixes several issues, so better to pull from rworldmap
        # (see rworldmap docs: https://cran.r-project.org/web/packages/rworldmap/rworldmap.pdf)
        # ?countriesCoarse
        # ?countriesCoarseLessIslands
        
        # another option is rnaturalearth package: https://docs.ropensci.org/rnaturalearth/
        
        
        # get world map as sf object
        world_sf <- st_as_sf(getMap(resolution = "high"))
        # world_sf
        # world_sf %>% glimpse()
        # world_sf %>% class()
        # world_sf %>% nrow() # 253
        # st_crs(world_sf)
        
        # note when I pull SOVEREIGNT out as a character in a tibble, it's only 202 countries; 
        # there is 253 rows though, and 253 distinct SOVEREIGNT - but some are duplicates, 
        # the duplicates appear to be countries with non-contiguous boundaries (aka mainlands and islands)
        # since world_sf is an sf object, calling distinct(SOVEREIGNT) kind of always implicitly includes the geometry variable
        # and since these non-contiguous countries have multiple geometries, distinct() returns those multiple rows
        # world_sf %>% nrow() # 253
        # world_sf %>% as_tibble() %>% distinct(SOVEREIGNT) %>% nrow() # 204
        
        # check country names
        # fmir %>% distinct(country) %>% filter(!(country %in% c("U.S.", "Russia"))) %>%
        #         anti_join(., world_sf %>% 
        #                           mutate(SOVEREIGNT = as.character(SOVEREIGNT),
        #                                   SOVEREIGNT = case_when(SOVEREIGNT == "Czech Republic" ~ "Czechia",
        #                                                         SOVEREIGNT == "Macedonia" ~ "N. Macedonia",
        #                                                         SOVEREIGNT == "Republic of Serbia" ~ "Serbia",
        #                                                         SOVEREIGNT == "United Kingdom" ~ "U.K.",
        #                                                         SOVEREIGNT == "Bosnia and Herzegovina" ~ "BiH",
        #                                                         TRUE ~ SOVEREIGNT)) %>%
        #                           as_tibble() %>% distinct(SOVEREIGNT), 
        #                   by = c("country" = "SOVEREIGNT"))
        # 
        # world_sf %>% as_tibble() %>% distinct(SOVEREIGNT) %>%
        #         filter(str_detect(string = SOVEREIGNT, pattern = "Bosnia"))
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        country_profile_map <- world_sf %>% 
                mutate(SOVEREIGNT = as.character(SOVEREIGNT),
                       SOVEREIGNT = case_when(SOVEREIGNT == "Czech Republic" ~ "Czechia",
                                              SOVEREIGNT == "Macedonia" ~ "N. Macedonia",
                                              SOVEREIGNT == "Republic of Serbia" ~ "Serbia",
                                              SOVEREIGNT == "United Kingdom" ~ "U.K.",
                                              SOVEREIGNT == "Bosnia and Herzegovina" ~ "BiH",
                                              TRUE ~ SOVEREIGNT),
                       fill_color_bin = "selected_color") %>%
                filter(SOVEREIGNT != "Antarctica") %>%
                filter(SOVEREIGNT == current_country) %>%
                ggplot(data = ., mapping = aes(fill = factor(fill_color_bin))) + 
                geom_sf(color = "#E0ECF7", size = 0.1) +
                scale_fill_manual(values = list("selected_color" = "#E0ECF7"), guide = "none") +
                theme_bw() +
                theme(panel.grid.major = element_line(color = "transparent"),
                      plot.background = element_blank(), 
                      panel.grid.minor = element_blank(), panel.border = element_blank(),
                      axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
                      axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
                      # plot.title = element_text(size = 6, face = "bold", hjust = 0, family = "Trebuchet MS"), 
                      plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                                  color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                      legend.position = "right",
                      legend.key.size = unit(2, "mm"),
                      legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")), 
                      legend.title = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
                      panel.grid = element_blank(),
                      line = element_blank(),
                      rect = element_blank(),
                      text = element_blank())
        
        # inspect
        # country_profile_map
        
        ggsave(filename = str_c("output/charts/country_profile/map_", str_to_lower(current_country), ".png"), 
               plot = country_profile_map, 
               dpi = 300, width = 6, height = 6)
        
        # crop
        # image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), ".png")) %>% image_info() # w = 1800, h = 1800 (dpi * width)
        
        if(current_country == "Albania") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 800, height = 1700, x_off = 500, y_off = 50))
                
        } else if(current_country == "Azerbaijan") {

                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 1300, x_off = 50, y_off = 250))

        } else if(current_country == "Belarus") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 1500, x_off = 50, y_off = 150))
                
        } else if(current_country == "Bulgaria") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 1200, x_off = 50, y_off = 300))
                
        } else if(current_country == "Czechia") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 1000, x_off = 50, y_off = 400))
                
        } else if(current_country == "Estonia") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 1200, x_off = 50, y_off = 300))
                
        } else if(current_country == "Georgia") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 900, x_off = 50, y_off = 450))
                
        } else if(current_country == "Hungary") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 1100, x_off = 50, y_off = 350))
                
        } else if(current_country == "Kazakhstan") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 1000, x_off = 50, y_off = 400))
                
        } else if(current_country == "Kyrgyzstan") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 800, x_off = 50, y_off = 500))
                
        } else if(current_country == "Latvia") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 1100, x_off = 50, y_off = 350))
                
        } else if(current_country == "Lithuania") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 1300, x_off = 50, y_off = 250))
                
        } else if(current_country == "N. Macedonia") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 1300, x_off = 50, y_off = 250))
                
        } else if(current_country == "Moldova") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1300, height = 1700, x_off = 250, y_off = 50))
                
        } else if(current_country == "Montenegro") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1500, height = 1700, x_off = 150, y_off = 50))
                
        } else if(current_country == "Poland") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 1600, x_off = 50, y_off = 75))
                
        } else if(current_country == "Romania") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 1200, x_off = 50, y_off = 300))
                
        } else if(current_country == "Serbia") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1300, height = 1700, x_off = 250, y_off = 50))
                
        } else if(current_country == "Slovakia") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 900, x_off = 50, y_off = 450))
                
        } else if(current_country == "Slovenia") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 1100, x_off = 50, y_off = 350))
                
        } else if(current_country == "Tajikistan") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 1200, x_off = 50, y_off = 300))
                
        } else if(current_country == "Turkmenistan") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 1200, x_off = 50, y_off = 300))
                
        } else if(current_country == "Ukraine") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 1200, x_off = 50, y_off = 300))
                
        } else if(current_country == "Uzbekistan") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 1200, x_off = 50, y_off = 300))
                
        } else {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country),
                                                            ".png"))
                
        }

        
        #//////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # country_profile_title ####
        
        # p1 <- ggplot(mtcars) + 
        #         geom_point(aes(mpg, disp))
        # ggdraw(p1) +
        #         draw_label(label = current_country_full_name, color = "#002F6C", size = 20, angle = 0,
        #                    fontface = "bold", fontfamily = "Calibri",
        #                    x = .5, y = .5) +
        #         draw_label(label = "Resilience Index: FY 2020 Country Profile", color = "#002F6C", size = 15, angle = 0,
        #                    fontface = "plain", fontfamily = "Calibri",
        #                    x = .79, y = .42) 
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # country_profile_flag ####
        
        print(str_c(current_country, " flag"))
        
        # best source is britannica which has high-quality pngs cropped to proper dimensions
        # https://www.britannica.com/topic/flag-of-Bosnia-and-Herzegovina
        
        # unused official state dept. flags have correct dimensions, but they require manual cropping based on wikipedia dimensions
        # https://www.state.gov/countries-areas/
        # https://en.wikipedia.org/wiki/List_of_aspect_ratios_of_national_flags
        
        # unused open source flag svgs (does not always have correct dimensions however)
        # https://flagicons.lipis.dev/
        
        # image_read(str_c("output/charts/flags/", current_country, ".png")) %>% image_info()
        country_profile_flag_png <- image_read(str_c("output/charts/flags/", current_country, ".png"))
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # country_profile_usaid_logo ####
        
        print(str_c(current_country, " logo"))
        
        # https://www.usaid.gov/branding/resources # best source w white logo
        # https://www.usaid.gov/sites/default/files/documents/1869/USAID_GSM-02_04_2020.pdf
        # usaid blue: #002F6C
        
        # extra un-used sources
        # https://cloudconvert.com/ai-to-svg
        # https://seekvectorlogo.net/download-vector-logo-3901/#
        # https://stephanieevergreen.com/events/uganda/1200px-usaid-logo-svg/
        
        # read usaid white logo as png, convert to resized ggplot w blue background, resave as png
        # note that there was no discernable difference in 300 vs 600 dpi image; pdf file size increased by 200 kb, will go w 300 dpi
        usaid_png <- image_read(path = "output/charts/usaid_logo/Horizontal_RGB_294_White.png")
        usaid_logo <- ggplot() + background_image(usaid_png) + 
                coord_fixed(1 / 2.5) +
                theme_nothing() +
                theme(
                        # panel.background = element_rect(fill = "transparent"),
                        # plot.background = element_rect(fill = "transparent", color = NA)
                        panel.background = element_rect(fill = "#002F6C"),
                        plot.background = element_rect(fill = "#002F6C"),
                        rect = element_rect(fill = "#002F6C")
                )
        ggsave(filename = "output/charts/usaid_logo/usaid_logo_white_resized.png", plot = usaid_logo, dpi = 300, width = 6, height = 6)
        # ggsave(filename = "output/charts/usaid_logo/usaid_logo_white_resized.png", plot = usaid_logo, dpi = 600, width = 6, height = 6)
        
        # crop
        # image_read("output/charts/usaid_logo/usaid_logo_white_resized.png") %>% image_info() # w = 1800, h = 1800 (dpi * width)
        country_profile_usaid_logo_png <- image_read("output/charts/usaid_logo/usaid_logo_white_resized.png") %>%
                image_crop(image = ., geometry = geometry_area(width = 1600, height = 600, x_off = 100, y_off = 600)) # crop 300 dpi
        # image_crop(image = ., geometry = geometry_area(width = 3200, height = 1100, x_off = 200, y_off = 1200)) # crop 600 dpi
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # create country_profile_blue_banner ####
        
        print(str_c(current_country, " blue_banner"))
        
        country_profile_blue_banner <- tibble(x = c(0, 0, 2, 2), y = c(0, 2, 2, 0)) %>%
                ggplot(data = ., mapping = aes(x = x, y = y)) +
                geom_rect(xmin = 0, xmax = 2, ymin = 0, ymax = 2, color = "#002F6C", fill = "#002F6C") +
                theme_nothing() +
                theme(
                        aspect.ratio = 1 / 100
                )
        # country_profile_blue_banner 
        ggsave(filename = "output/charts/usaid_logo/country_profile_blue_banner.png", 
               plot = country_profile_blue_banner, dpi = 300, width = 6, height = 6)
        
        # crop
        # image_read("output/charts/usaid_logo/country_profile_blue_banner.png") %>% image_info() # w = 1800, h = 1800 (dpi * width)
        country_profile_blue_banner_png <- image_read("output/charts/usaid_logo/country_profile_blue_banner.png") %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 16, x_off = 0, y_off = 892)) 
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # country_profile_divider ####
        
        print(str_c(current_country, " divider"))
        
        country_profile_divider <- tibble(x = c(0, 1), y = c(1, 3), color = "dot_color") %>%
                ggplot(data = ., mapping = aes(x = x, y = y, color = color)) +
                geom_point() +
                scale_color_manual(values = "#ffffff") +
                geom_segment(x = 0, y = 2, xend = 1, yend = 2, colour = "#CBCBCB", lineend = "round", size = 1) +
                theme_nothing() +
                theme(
                        aspect.ratio = 1 / 50
                )
        ggsave(filename = "output/charts/country_profile/country_profile_divider.png", 
               plot = country_profile_divider, dpi = 300, width = 6, height = 6)
        
        # read resized usaid logo png and crop
        # image_read("output/charts/country_profile/country_profile_divider.png") %>% image_info() # w = 1800, h = 1800 (dpi * width)
        country_profile_divider_png <- image_read("output/charts/country_profile/country_profile_divider.png") %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 50, x_off = 0, y_off = 875)) 
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # create country_profile layout ####
        
        print(str_c(current_country, " layout"))
        
        page_height <- 100
        page_width <- 100
        side_margin <- 11 # 11 give appropriate to edge in pdf
        center_margin <- 2
        below_margin <- 2
        dot_plot_section_height <- 60
        top_section_height <- 100 - dot_plot_section_height
        dot_plot_width <- (page_width - (2 * side_margin) - center_margin) / 2
        dot_plot_ri_height <- round((dot_plot_section_height - (3 * below_margin)) * (4/11))
        dot_plot_obj_1_height <- round((dot_plot_section_height - (3 * below_margin)) * (2/11))
        dot_plot_obj_2_height <- round((dot_plot_section_height - (3 * below_margin)) * (2/11))
        dot_plot_obj_3_height <- round((dot_plot_section_height - (3 * below_margin)) * (2/11))
        dot_plot_obj_4_height <- round((dot_plot_section_height - (3 * below_margin)) * (1.5/11))
        dot_plot_obj_c_height <- round((dot_plot_section_height - (3 * below_margin)) * (1/11))
        
        layout <- c(
                area(t = 1, l = 1, b = top_section_height, r = page_width), # spacer 1 to 1
                
                # dot_plot_ri
                area(t = (top_section_height) + 1, 
                     l = (side_margin) + 1, 
                     b = (top_section_height) + dot_plot_ri_height, 
                     r = (side_margin) + dot_plot_width), 
                
                # dot_plot_obj_1
                area(t = (top_section_height) + 1, 
                     l = (side_margin + dot_plot_width + center_margin) + 1, 
                     b = (top_section_height) + dot_plot_obj_1_height, 
                     r = (side_margin + dot_plot_width + center_margin) + dot_plot_width), 
                
                # dot_plot_obj_2
                area(t = (top_section_height + dot_plot_ri_height + below_margin) + 1, 
                     l = (side_margin) + 1, 
                     b = (top_section_height + dot_plot_ri_height + below_margin) + dot_plot_obj_2_height, 
                     r = (side_margin) + dot_plot_width), 
                
                # dot_plot_obj_3
                area(t = (top_section_height + dot_plot_obj_1_height + below_margin) + 1, 
                     l = (side_margin + dot_plot_width + center_margin) + 1, 
                     b = (top_section_height + dot_plot_obj_1_height + below_margin) + dot_plot_obj_3_height, 
                     r = (side_margin + dot_plot_width + center_margin) + dot_plot_width), 
                
                # dot_plot_obj_4
                area(t = (top_section_height + dot_plot_ri_height + below_margin + dot_plot_obj_2_height + below_margin) + 1, 
                     l = (side_margin) + 1, 
                     b = (top_section_height + dot_plot_ri_height + below_margin + dot_plot_obj_2_height) + dot_plot_obj_4_height, 
                     r = (side_margin) + dot_plot_width),
                
                # dot_plot_obj_c
                area(t = (top_section_height + dot_plot_obj_1_height + below_margin + dot_plot_obj_3_height + below_margin) + 1, 
                     l = (side_margin + dot_plot_width + center_margin) + 1, 
                     b = (top_section_height + dot_plot_obj_1_height + below_margin + dot_plot_obj_3_height + below_margin) + 
                             dot_plot_obj_c_height, 
                     r = (side_margin + dot_plot_width + center_margin) + dot_plot_width)
                
                # area(t = (top_section_height + dot_plot_ri_height + below_margin + dot_plot_obj_3_height + below_margin) + 1, 
                #      l = 1, 
                #      b = page_height, 
                #      r = page_width)
        )
        # plot(layout)
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # assemble country_profile patchwork ####
        
        print(str_c(current_country, " assembly"))
        
        # note this patchwork layout is no longer used, except to create canvas fitted to pdf page dimensions
        # instead cowplot is used to draw png images, which gave more efficiency / control because
        # patchwork inset_element had strange reference/alignment behavior requiring continual position adjustments
        # when new objects were added, whereas cowplot::draw_image sets permanent coordinates that are robust to additions
        # note however, that cowplot still has some weird behavior - in particular, it seems like coordinates are relative to
        # image size, which is weird. eg plot two country maps of same height but different width, and set vjust = 1
        # which should ensure that the top of each image is at the same height on the page, but instead one will be higher???
        # (eg azerbaijan and albania maps)
        
        # create country_profile_layout
        country_profile_layout <- wrap_plots(plot_spacer(),
                                             # six_row_blue_rect_background,
                                             # three_row_blue_rect_background,
                                             # three_row_blue_rect_background,
                                             # three_row_blue_rect_background,
                                             # three_row_blue_rect_background,
                                             # three_row_blue_rect_background
                                             plot_spacer(),
                                             plot_spacer(),
                                             plot_spacer(),
                                             plot_spacer(),
                                             plot_spacer(),
                                             plot_spacer()
        ) +
                plot_layout(design = layout)
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # set reference points
        
        top_dot_plot <- .435
        
        # dot_plot_legend
        dot_plot_legend <- top_dot_plot - .0127 
        
        # ri_dot_plot
        ri_dot_plot_ri <- top_dot_plot
        ri_dot_plot_obj_1 <- ri_dot_plot_ri - .028
        ri_dot_plot_obj_2 <- ri_dot_plot_obj_1 - .028
        ri_dot_plot_obj_3 <- ri_dot_plot_obj_2 - .028
        ri_dot_plot_obj_4 <- ri_dot_plot_obj_3 - .028
        ri_dot_plot_obj_c <- ri_dot_plot_obj_4 - .032
        
        # obj_1_dot_plot
        # obj_1_dot_plot_sub_obj_1_1 <- top_dot_plot
        # obj_1_dot_plot_sub_obj_1_1 <- dot_plot_legend - .079
        obj_1_dot_plot_sub_obj_1_1 <- dot_plot_legend - .086
        obj_1_dot_plot_sub_obj_1_2 <- obj_1_dot_plot_sub_obj_1_1 - .028
        obj_1_dot_plot_sub_obj_1_3 <- obj_1_dot_plot_sub_obj_1_2 - .032
        
        # obj_2_dot_plot
        obj_2_dot_plot_sub_obj_2_1 <- ri_dot_plot_obj_c - .079
        obj_2_dot_plot_sub_obj_2_2 <- obj_2_dot_plot_sub_obj_2_1 - .028
        obj_2_dot_plot_sub_obj_2_3 <- obj_2_dot_plot_sub_obj_2_2 - .032
        
        # obj_3_dot_plot
        obj_3_dot_plot_sub_obj_3_1 <- obj_1_dot_plot_sub_obj_1_3 - .079
        obj_3_dot_plot_sub_obj_3_2 <- obj_3_dot_plot_sub_obj_3_1 - .028
        obj_3_dot_plot_sub_obj_3_3 <- obj_3_dot_plot_sub_obj_3_2 - .032
        
        # obj_4_dot_plot
        obj_4_dot_plot_sub_obj_4_1 <- obj_2_dot_plot_sub_obj_2_3 - .079
        obj_4_dot_plot_sub_obj_4_2 <- obj_4_dot_plot_sub_obj_4_1 - .032
        
        # obj_c_dot_plot
        obj_c_dot_plot_sub_obj_c <- obj_3_dot_plot_sub_obj_3_3 - .079
        
        
        #///////////////////
        
        
        # set map scaling to account for cowplot::draw_image's odd behavior with coordinates being relative to image area
        # eg albania and azerbaijan maps set with vjust = 1 to same y coordinate will NOT be printed at same height on page???
        if(current_country == "Albania") {
                map_height_width_scale = .17
        }
        if(current_country == "Armenia") {
                map_height_width_scale = .18
        }
        if(current_country == "Azerbaijan") {
                map_height_width_scale = .21
        }
        if(current_country == "Belarus") {
                map_height_width_scale = .19
        }
        if(current_country == "BiH") {
                map_height_width_scale = .18
        }
        if(current_country == "Bulgaria") {
                map_height_width_scale = .25
        }
        if(current_country == "Croatia") {
                map_height_width_scale = .18
        }
        if(current_country == "Czechia") {
                map_height_width_scale = .28
        }
        if(current_country == "Estonia") {
                map_height_width_scale = .25
        }
        if(current_country == "Georgia") {
                map_height_width_scale = .32
        }
        if(current_country == "Hungary") {
                map_height_width_scale = .26
        }
        if(current_country == "Kazakhstan") {
                map_height_width_scale = .30
        }
        if(current_country == "Kosovo") {
                map_height_width_scale = .18
        }
        if(current_country == "Kyrgyzstan") {
                map_height_width_scale = .33
        }
        if(current_country == "Latvia") {
                map_height_width_scale = .27
        }
        if(current_country == "Lithuania") {
                map_height_width_scale = .22
        }
        if(current_country == "N. Macedonia") {
                map_height_width_scale = .22
        }
        if(current_country == "Moldova") {
                map_height_width_scale = .17
        }
        if(current_country == "Montenegro") {
                map_height_width_scale = .17
        }
        if(current_country == "Poland") {
                map_height_width_scale = .18
        }
        if(current_country == "Romania") {
                map_height_width_scale = .23
        }
        if(current_country == "Serbia") {
                map_height_width_scale = .17
        }
        if(current_country == "Slovakia") {
                map_height_width_scale = .34
        }
        if(current_country == "Slovenia") {
                map_height_width_scale = .25
        }
        if(current_country == "Tajikistan") {
                map_height_width_scale = .24
        }
        if(current_country == "Turkmenistan") {
                map_height_width_scale = .25
        }
        if(current_country == "Ukraine") {
                map_height_width_scale = .25
        }
        if(current_country == "Uzbekistan") {
                map_height_width_scale = .25
        }
        
        
        #///////////////////////////
        
        
        # draw images
        country_profile <- ggdraw(country_profile_layout) +
                
                # draw banner and logo
                draw_image(country_profile_blue_banner_png, x = .5, y = .97, hjust = .5, vjust = .5, width = 7, height = 7) +
                draw_image(country_profile_usaid_logo_png, x = .12, y = .969, hjust = 0, vjust = .5, width = 0.14, height = 0.14) +
                
                
                #//////////////////////////////////////////////////////////////////////////////////////////////////////////
                
                
                # draw map
                draw_image(country_profile_map_png, x = .5, y = .845, hjust = 1, vjust = .5, 
                           width = map_height_width_scale, height = map_height_width_scale) +
                # draw_image(country_profile_map_albania, x = .7, y = .845, hjust = 1, vjust = .5, width = 0.17, height = 0.17) +
                
                
                #//////////////////////////////////////////////////////////////////////////////////////////////////////////
                
                
                # draw radar chart
                draw_image(country_profile_radar_chart_png, 
                           x = .84, y = .955,
                           hjust = 1, vjust = 1, width = 0.22, height = 0.22) +
                
                
                #//////////////////////////////////////////////////////////////////////////////////////////////////////////
                
                
                # draw line chart
                draw_image(line_chart_gray_border_png, x = .504, y = .629, 
                           hjust = .5, vjust = .5, width = 0.55, height = 0.55) +
                draw_image(country_profile_line_chart_png, x = .504, y = .631, 
                           hjust = .5, vjust = .5, width = 0.5, height = 0.5) +
                
                
                #//////////////////////////////////////////////////////////////////////////////////////////////////////////
                
                
                # draw divider
                draw_image(country_profile_divider_png, x = .092, y = top_dot_plot + .048, 
                           hjust = 0, vjust = .5, width = .82, height = .82) +
                
                
                #//////////////////////////////////////////////////////////////////////////////////////////////////////////
                #//////////////////////////////////////////////////////////////////////////////////////////////////////////
                #//////////////////////////////////////////////////////////////////////////////////////////////////////////
                
                
                # draw sub_obj_dot_plots
                
                # note the top and bottom dot_plots have tailored label heights
                
        
                # ri_dot_plot background and label
                # note that for some reason the blue_rect y coordinate does not seem to have same reference as label and dot_plots
                draw_image(six_row_blue_rect_background_png, x = .51, y = ri_dot_plot_ri + .128, 
                         hjust = 1, vjust = 1, width = .4, height = .4) +
                draw_label(label = "Resilience Index objectives", color = "#ffffff", size = 6, angle = 0,
                           fontface = "bold", fontfamily = "Calibri",
                           x = .138, y = ri_dot_plot_ri + .023, hjust = 0, vjust = .5) +
                
                # ri_dot_plot_ri
                draw_image(miri_country_profile_dot_plot_png, x = .139, y = ri_dot_plot_ri, 
                           hjust = 0, vjust = .5, width = 0.34, height = 0.34) +
                draw_label(label = "Resilience Index", color = "#333333", size = 5, angle = 0,
                           fontface = "plain", fontfamily = "Calibri",
                           x = .183, y = ri_dot_plot_ri + .003, hjust = .5, vjust = .5) +
                
                # ri_dot_plot_obj_1
                draw_image(obj_1_country_profile_dot_plot_png, x = .139, y = ri_dot_plot_obj_1, 
                           hjust = 0, vjust = .5, width = 0.34, height = 0.34) +
                draw_label(label = "Democratic", color = "#333333", size = 4.5, angle = 0,
                           fontface = "plain", fontfamily = "Calibri",
                           x = .183, y = ri_dot_plot_obj_1 + .004, hjust = .5, vjust = .5) +
                
                # ri_dot_plot_obj_2
                draw_image(obj_2_country_profile_dot_plot_png, x = .139, y = ri_dot_plot_obj_2, 
                           hjust = 0, vjust = .5, width = 0.34, height = 0.34) +
                draw_label(label = "Information", color = "#333333", size = 5, angle = 0,
                           fontface = "plain", fontfamily = "Calibri",
                           x = .183, y = ri_dot_plot_obj_2 + .004, hjust = .5, vjust = .5) +
                
                # ri_dot_plot_obj_3
                draw_image(obj_3_country_profile_dot_plot_png, x = .139, y = ri_dot_plot_obj_3, 
                           hjust = 0, vjust = .5, width = 0.34, height = 0.34) +
                draw_label(label = "Energy", color = "#333333", size = 5, angle = 0,
                           fontface = "plain", fontfamily = "Calibri",
                           x = .183, y = ri_dot_plot_obj_3 + .004, hjust = .5, vjust = .5) +
                
                # ri_dot_plot_obj_4
                draw_image(obj_4_country_profile_dot_plot_png, x = .139, y = ri_dot_plot_obj_4, 
                           hjust = 0, vjust = .5, width = 0.34, height = 0.34) +
                draw_label(label = "Economic", color = "#333333", size = 5, angle = 0,
                           fontface = "plain", fontfamily = "Calibri",
                           x = .183, y = ri_dot_plot_obj_4 + .004, hjust = .5, vjust = .5) +
                
                # ri_dot_plot_obj_c
                draw_image(obj_c_country_profile_dot_plot_png, x = .139, y = ri_dot_plot_obj_c, 
                           hjust = 0, vjust = .5, width = 0.34, height = 0.34) +
                draw_label(label = "Corruption", color = "#333333", size = 5, angle = 0,
                           fontface = "plain", fontfamily = "Calibri",
                           x = .183, y = ri_dot_plot_obj_c + .008, hjust = .5, vjust = .5) +
                
                
                #///////////////////////////////////////////////////////////////////////////////////////////////////////
                
                
                # obj_1_dot_plot background and label
                # note that for some reason the blue_rect y coordinate does not seem to have same reference as label and dot_plots
                draw_image(three_row_blue_rect_background_png, x = .895, y = obj_1_dot_plot_sub_obj_1_1 + .170, 
                           hjust = 1, vjust = 1, width = .4, height = .4) +
                draw_label(label = "Democratic sub-objectives", color = "#002F6C", size = 6, angle = 0,
                           fontface = "bold", fontfamily = "Calibri",
                           x = .523, y = obj_1_dot_plot_sub_obj_1_1 + .023, hjust = 0, vjust = .5) +
                
                # obj_1_dot_plot_sub_obj_1_1
                draw_image(sub_obj_1_1_country_profile_dot_plot_png, x = .524, y = obj_1_dot_plot_sub_obj_1_1, 
                           hjust = 0, vjust = .5, width = 0.34, height = 0.34) +
                draw_label(label = "Checks / balances\nand rule of law", color = "#333333", size = 5, angle = 0,
                           fontface = "plain", fontfamily = "Calibri",
                           x = .568, y = obj_1_dot_plot_sub_obj_1_1 + .003, hjust = .5, vjust = .5) +
                
                # obj_1_dot_plot_sub_obj_1_2
                draw_image(sub_obj_1_2_country_profile_dot_plot_png, x = .524, y = obj_1_dot_plot_sub_obj_1_2, 
                           hjust = 0, vjust = .5, width = 0.34, height = 0.34) +
                draw_label(label = "Civil society", color = "#333333", size = 4.5, angle = 0,
                           fontface = "plain", fontfamily = "Calibri",
                           x = .568, y = obj_1_dot_plot_sub_obj_1_2 + .004, hjust = .5, vjust = .5) +
                
                # obj_1_dot_plot_sub_obj_1_3
                draw_image(sub_obj_1_3_country_profile_dot_plot_png, x = .524, y = obj_1_dot_plot_sub_obj_1_3, 
                           hjust = 0, vjust = .5, width = 0.34, height = 0.34) +
                draw_label(label = "Electoral / political\ninterference", color = "#333333", size = 4.5, angle = 0,
                           fontface = "plain", fontfamily = "Calibri",
                           x = .568, y = obj_1_dot_plot_sub_obj_1_3 + .008, hjust = .5, vjust = .5) +
                
                
                #///////////////////////////////////////////////////////////////////////////////////////////////////////
                
                
                # obj_2_dot_plot background and label
                # note that for some reason the blue_rect y coordinate does not seem to have same reference as label and dot_plots
                draw_image(three_row_blue_rect_background_png, x = .51, y = obj_2_dot_plot_sub_obj_2_1 + .171, 
                           hjust = 1, vjust = 1, width = .4, height = .4) +
                draw_label(label = "Information sub-objectives", color = "#002F6C", size = 6, angle = 0,
                           fontface = "bold", fontfamily = "Calibri",
                           x = .138, y = obj_2_dot_plot_sub_obj_2_1 + .023, hjust = 0, vjust = .5) +
                
                # obj_2_dot_plot_sub_obj_2_1
                draw_image(sub_obj_2_1_country_profile_dot_plot_png, x = .139, y = obj_2_dot_plot_sub_obj_2_1, 
                           hjust = 0, vjust = .5, width = 0.34, height = 0.34) +
                draw_label(label = "Trusted media\nand information", color = "#333333", size = 5, angle = 0,
                           fontface = "plain", fontfamily = "Calibri",
                           x = .183, y = obj_2_dot_plot_sub_obj_2_1 + .003, hjust = .5, vjust = .5) +
                
                # obj_2_dot_plot_sub_obj_2_2
                draw_image(sub_obj_2_2_country_profile_dot_plot_png, x = .139, y = obj_2_dot_plot_sub_obj_2_2, 
                           hjust = 0, vjust = .5, width = 0.34, height = 0.34) +
                draw_label(label = "Media demand", color = "#333333", size = 5, angle = 0,
                           fontface = "plain", fontfamily = "Calibri",
                           x = .183, y = obj_2_dot_plot_sub_obj_2_2 + .004, hjust = .5, vjust = .5) +
                
                # obj_2_dot_plot_sub_obj_2_3
                draw_image(sub_obj_2_3_country_profile_dot_plot_png, x = .139, y = obj_2_dot_plot_sub_obj_2_3, 
                           hjust = 0, vjust = .5, width = 0.34, height = 0.34) +
                draw_label(label = "Media freedom", color = "#333333", size = 5, angle = 0,
                           fontface = "plain", fontfamily = "Calibri",
                           x = .183, y = obj_2_dot_plot_sub_obj_2_3 + .008, hjust = .5, vjust = .5) +
                
                
                #///////////////////////////////////////////////////////////////////////////////////////////////////////
                
                
                # obj_3_dot_plot background and label
                # note that for some reason the blue_rect y coordinate does not seem to have same reference as label and dot_plots
                draw_image(three_row_blue_rect_background_png, x = .895, y = obj_3_dot_plot_sub_obj_3_1 + .171, 
                           hjust = 1, vjust = 1, width = .4, height = .4) +
                draw_label(label = "Energy sub-objectives", color = "#002F6C", size = 6, angle = 0,
                           fontface = "bold", fontfamily = "Calibri",
                           x = .523, y = obj_3_dot_plot_sub_obj_3_1 + .023, hjust = 0, vjust = .5) +
                
                # obj_3_dot_plot_sub_obj_3_1
                draw_image(sub_obj_3_1_country_profile_dot_plot_png, x = .524, y = obj_3_dot_plot_sub_obj_3_1, 
                           hjust = 0, vjust = .5, width = 0.34, height = 0.34) +
                draw_label(label = "Energy security", color = "#333333", size = 5, angle = 0,
                           fontface = "plain", fontfamily = "Calibri",
                           x = .568, y = obj_3_dot_plot_sub_obj_3_1 + .003, hjust = .5, vjust = .5) +
                
                # obj_3_dot_plot_sub_obj_3_2
                draw_image(sub_obj_3_2_country_profile_dot_plot_png, x = .524, y = obj_3_dot_plot_sub_obj_3_2, 
                           hjust = 0, vjust = .5, width = 0.34, height = 0.34) +
                draw_label(label = "Independence from\nRussian energy", color = "#333333", size = 5, angle = 0,
                           fontface = "plain", fontfamily = "Calibri",
                           x = .568, y = obj_3_dot_plot_sub_obj_3_2 + .004, hjust = .5, vjust = .5) +
                
                # obj_3_dot_plot_sub_obj_3_3
                draw_image(sub_obj_3_3_country_profile_dot_plot_png, x = .524, y = obj_3_dot_plot_sub_obj_3_3, 
                           hjust = 0, vjust = .5, width = 0.34, height = 0.34) +
                draw_label(label = "Energy regulation", color = "#333333", size = 5, angle = 0,
                           fontface = "plain", fontfamily = "Calibri",
                           x = .568, y = obj_3_dot_plot_sub_obj_3_3 + .008, hjust = .5, vjust = .5) +
                
                
                #///////////////////////////////////////////////////////////////////////////////////////////////////////
                
                
                # obj_4_dot_plot background and label
                # note that for some reason the blue_rect y coordinate does not seem to have same reference as label and dot_plots
                draw_image(two_row_blue_rect_background_png, x = .51, y = obj_4_dot_plot_sub_obj_4_1 + .185, 
                           hjust = 1, vjust = 1, width = .4, height = .4) +
                draw_label(label = "Economic sub-objectives", color = "#002F6C", size = 6, angle = 0,
                           fontface = "bold", fontfamily = "Calibri",
                           x = .138, y = obj_4_dot_plot_sub_obj_4_1 + .023, hjust = 0, vjust = .5) +
                
                # obj_4_dot_plot_sub_obj_4_1
                draw_image(sub_obj_4_1_country_profile_dot_plot_png, x = .139, y = obj_4_dot_plot_sub_obj_4_1, 
                           hjust = 0, vjust = .5, width = 0.34, height = 0.34) +
                draw_label(label = "Economic\ncompetitiveness", color = "#333333", size = 5, angle = 0,
                           fontface = "plain", fontfamily = "Calibri",
                           x = .183, y = obj_4_dot_plot_sub_obj_4_1 + .003, hjust = .5, vjust = .5) +
                
                # obj_4_dot_plot_sub_obj_4_3
                draw_image(sub_obj_4_2_country_profile_dot_plot_png, x = .139, y = obj_4_dot_plot_sub_obj_4_2, 
                           hjust = 0, vjust = .5, width = 0.34, height = 0.34) +
                draw_label(label = "Financial health", color = "#333333", size = 5, angle = 0,
                           fontface = "plain", fontfamily = "Calibri",
                           x = .183, y = obj_4_dot_plot_sub_obj_4_2 + .008, hjust = .5, vjust = .5) +
                
                
                #///////////////////////////////////////////////////////////////////////////////////////////////////////
                
                
                # obj_c_dot_plot background and label
                # note that for some reason the blue_rect y coordinate does not seem to have same reference as label and dot_plots
                draw_image(one_row_blue_rect_background_png, x = .895, y = obj_c_dot_plot_sub_obj_c + .198, 
                           hjust = 1, vjust = 1, width = .4, height = .4) +
                draw_label(label = "Corruption sub-objectives", color = "#002F6C", size = 6, angle = 0,
                           fontface = "bold", fontfamily = "Calibri",
                           x = .523, y = obj_c_dot_plot_sub_obj_c + .023, hjust = 0, vjust = .5) +
                
                # obj_c_dot_plot_sub_obj_c_3
                # note that y coordinate for obj_c_dot_plot_sub_obj_c_3 and label needs special handling 
                # because it's uniquely both top and bottom, and bottom always has special handling
                draw_image(sub_obj_c_country_profile_dot_plot_png, x = .524, y = obj_c_dot_plot_sub_obj_c - .005, 
                           hjust = 0, vjust = .5, width = 0.34, height = 0.34) +
                draw_label(label = "Control of\ncorruption", color = "#333333", size = 5, angle = 0,
                           fontface = "plain", fontfamily = "Calibri",
                           x = .568, y = obj_c_dot_plot_sub_obj_c + .008 - .005, hjust = .5, vjust = .5) +
                
                
                #///////////////////////////////////////////////////////////////////////////////////////////////////////
                
                
                # dot_plot_legend background and label
                # note that for some reason the blue_rect y coordinate does not seem to have same reference as label and dot_plots
                # draw_image(legend_gray_rect_background_png, x = .895, y = .263, 
                draw_image(legend_gray_rect_background_png, x = .895, y = .624, 
                           hjust = 1, vjust = 1, width = .4, height = .4) +
                draw_label(label = "Legend", color = "#002F6C", size = 6, angle = 0,
                           fontface = "bold", fontfamily = "Calibri",
                           x = .523, y = dot_plot_legend + .036, hjust = 0, vjust = .5) +
                
                # dot_plot_legend
                # note that y coordinate for obj_c_dot_plot_sub_obj_c_3 and label needs special handling 
                # because it's uniquely both top and bottom, and bottom always has special handling
                draw_image(country_profile_dot_plot_legend_png, x = .524, y = dot_plot_legend, 
                           hjust = 0, vjust = .5, width = 0.34, height = 0.34) 
 
                
        #///////////////////////////////////////////////////////////////////////////////////////////////////////
        
                
        # draw flag and title based on whether country name takes one or two rows
        if(!(current_country %in% c("BiH", "N. Macedonia"))) {
                
                country_profile <- ggdraw(country_profile) +
                        
                        # draw flag
                        draw_image(country_profile_flag_png, x = .4455, y = .862, hjust = 0, vjust = .5, width = 0.07, height = 0.07) +
                        
                        # draw title
                        draw_label(label = current_country_full_name, color = "#002F6C", size = 20, angle = 0,
                                   fontface = "bold", fontfamily = "Calibri",
                                   x = .13, y = .91, hjust = 0, vjust = 1) +
                        draw_label(label = "Resilience Index: 2020 Country Profile", color = "#002F6C", size = 10, angle = 0,
                                   fontface = "plain", fontfamily = "Calibri",
                                   x = .13, y = .862, hjust = 0, vjust = .5)
        }

        if(current_country %in% c("BiH", "N. Macedonia")) {
        
                country_profile <- ggdraw(country_profile) +
                
                        # draw flag
                        draw_image(country_profile_flag_png, x = .4455, y = .862, hjust = 0, vjust = .5, width = 0.07, height = 0.07) +
                        
                        
                        # draw title
                        draw_label(label = current_country_full_name, color = "#002F6C", size = 20, angle = 0,
                                   fontface = "bold", fontfamily = "Calibri",
                                   x = .13, y = .91, hjust = 0, vjust = 1) +
                        draw_label(label = "Resilience Index: 2020 Country Profile", color = "#002F6C", size = 10, angle = 0,
                                   fontface = "plain", fontfamily = "Calibri",
                                   x = .13, y = .82, hjust = 0, vjust = .5)
        }
        
        # inspect
        # country_profile
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # save chart as emf
        filename <- tempfile(fileext = ".emf")
        emf(file = filename)
        print(country_profile)
        dev.off()
        
        # add emf to word doc
        output_name <- str_c("output/charts/country_profile_", str_to_lower(current_country), ".docx")
        read_docx() %>%
                body_add_img(src = filename, width = 11.68, height = 11.68) %>%
                print(target = output_name)
        
}

#//////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////



