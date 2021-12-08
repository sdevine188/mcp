




three_row_blue_rect_background <- tibble(x = c(0, 1), y = c(1, 3), color = "dot_color") %>%
        ggplot(data = ., mapping = aes(x = x, y = y, color = color)) +
        geom_point() +
        scale_color_manual(values = "#E0ECF7") +
        theme_nothing() +
        theme(
                aspect.ratio = 1 / 1.5,
                plot.background  = element_rect_round(fill = "#E0ECF7", linetype = "blank", radius = unit(.05, "snpc"))
        ) 
three_row_blue_rect_background

three_row_blue_rect_background_orange <- tibble(x = c(0, 1), y = c(1, 3), color = "dot_color") %>%
        ggplot(data = ., mapping = aes(x = x, y = y, color = color)) +
        geom_point() +
        scale_color_manual(values = "#EF6712") +
        theme_nothing() +
        theme(
                aspect.ratio = 1 / 1.5,
                plot.background  = element_rect_round(fill = "#EF6712", linetype = "blank", radius = unit(.05, "snpc"))
        ) 
three_row_blue_rect_background_orange

obj_1_country_profile_dot_plots_w_background <- three_row_blue_rect_background  
        # inset_element(sub_obj_1_1_country_profile_dot_plot, left = .02, bottom = .05, right = .8, top = .8,
        #                       on_top = TRUE, align_to = "full")
obj_1_country_profile_dot_plots_w_background

ri_country_profile_dot_plots_w_background <- three_row_blue_rect_background +
        # inset_element(three_row_blue_rect_background_orange, left = 0, bottom = .5, right = 1, top = 1.5,
        #               on_top = TRUE) 
        inset_element(three_row_blue_rect_background_orange, left = 0, bottom = .2, right = 1, top = 1.2,
                      on_top = TRUE) 
        # inset_element(sub_obj_1_1_country_profile_dot_plot, left = .02, bottom = .05, right = .8, top = .8,
        #               on_top = TRUE, align_to = "full") +
        # inset_element(sub_obj_1_1_country_profile_dot_plot, left = .02, bottom = .05, right = .8, top = .8,
        #               on_top = TRUE, align_to = "full")
ri_country_profile_dot_plots_w_background


country_profile_dot_plots <- wrap_plots(plot_spacer(),
                                        # ri_country_profile_dot_plots_w_background,
                                        obj_1_country_profile_dot_plots_w_background,
                                        obj_1_country_profile_dot_plots_w_background,
                                        obj_1_country_profile_dot_plots_w_background,
                                        obj_1_country_profile_dot_plots_w_background,
                                        plot_spacer()) +
        plot_layout(design = layout)

country_profile_dot_plots <- country_profile_dot_plots +
        inset_element(sub_obj_1_1_country_profile_dot_plot, left = .17, bottom = 1.1, right = .48, top = 1.6,
                                    on_top = TRUE, align_to = "full") +
        inset_element(sub_obj_1_1_country_profile_dot_plot, left = .17, bottom = 1.3, right = .48, top = 1.8,
                      on_top = TRUE, align_to = "full") +
        inset_element(flag, left = .17, bottom = 4, right = .48, top = 5,
                      on_top = TRUE, align_to = "full") +
        inset_element(blue_banner, left = -1, bottom = 4, right = 1, top = 5,
                                    on_top = TRUE, align_to = "full")
        # inset_element(usaid_logo, left = .17, bottom = 2, right = .48, top = 3,
        #               on_top = TRUE, align_to = "full")
        # draw_image("output/charts/usaid_logo/Horizontal_RGB_294_White.png",
        #            x = 1, y = 1, hjust = 1, vjust = 1, width = 1.25, height = .5)
        # inset_element(obj_1_country_profile_dot_plots, left = 0, bottom = 1.1, right = .7, top = 1.6,
        #               on_top = TRUE, align_to = "full") # close
        # inset_element(obj_1_country_profile_dot_plots, left = -.015, bottom = 1.09, right = .7, top = 1.6,
        #               on_top = TRUE, align_to = "full") +
        # inset_element(ri_country_profile_dot_plots, left = -.015, bottom = 2.09, right = .7, top = 2.6,
        #               on_top = TRUE, align_to = "full")

# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(country_profile_dot_plots)
dev.off()

# add emf to word doc

read_docx() %>%
        body_add_img(src = filename, width = 11.65, height = 11.65) %>%
        print(target = "output/charts/country_profile_dot_plots.docx")



#/////////////////////////


obj_1_country_profile_dot_plots_w_background <- ggdraw(three_row_blue_rect_background) 
        # draw_plot(plot = sub_obj_1_1_country_profile_dot_plot, x = .025, y = .01, width = .95, height = .95)
obj_1_country_profile_dot_plots_w_background

ri_country_profile_dot_plots_w_background <- ggdraw(three_row_blue_rect_background) 
        # draw_plot(plot = sub_obj_1_1_country_profile_dot_plot, x = .025, y = .1, width = .95, height = .95)
ri_country_profile_dot_plots_w_background



#/////////////////////////


obj_1_country_profile_dot_plots_w_background <- three_row_blue_rect_background

ri_country_profile_dot_plots_w_background <- three_row_blue_rect_background +
        inset_element(three_row_blue_rect_background_orange, left = 0, bottom = .5, right = 1, top = 1.5,
                      on_top = TRUE, align_to = "full")

obj_1_country_profile_dot_plots_w_background +
        inset_element(ri_country_profile_dot_plots_w_background, left = 0, bottom = -1, right = 1, top = .5,
                      on_top = TRUE, align_to = "full")


#/////////////////////////////


obj_1_country_profile_dot_plots_w_background <- ggdraw(three_row_blue_rect_background) 
        # draw_plot(plot = obj_1_country_profile_dot_plots, x = .025, y = .01, width = .95, height = .95)
obj_1_country_profile_dot_plots_w_background

ri_country_profile_dot_plots_w_background <- ggdraw(three_row_blue_rect_background) +
        draw_plot(plot = three_row_blue_rect_background_orange, x = 0, y = .2, width = 1, height = 1)
ri_country_profile_dot_plots_w_background






#///////////////////////////////////////////////////
#///////////////////////////////////////////////////
#///////////////////////////////////////////////////

grid.newpage()
get_country_profile_radar_chart(df = chart_data, country = current_country)
country_profile_radar_chart <- recordPlot()
x <- as.ggplot(as_grob(country_profile_radar_chart)) +
        theme_nothing()
# theme(
#         plot.margin = unit(c(-100, 0, -100, 0), "mm"),
# )

ggsave(filename = "output/charts/x.png", plot = x, dpi = 600, width = 7, height = 7)

# save ggplot chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(x)
dev.off()

# add emf to word doc
read_docx() %>%
        body_add_img(src = filename, width = 6, height = 6) %>%
        print(target = "output/charts/x.docx")

png("output/charts/x.png")
print(x)
dev.off()

# old: 4200 x 4200
image_read("output/charts/x.png") %>% image_info()
z <- image_read("output/charts/x.png") %>%
        image_crop(image = ., geometry = geometry_area(width = 3200, height = 2900, x_off = 600, y_off = 500)) 
z

p1 <- ggplot(mtcars) + 
        geom_point(aes(mpg, disp))
p1
p2 <- ggplot(mtcars) + 
        geom_boxplot(aes(gear, disp, group = gear))
p2
p1 + inset_element(p2, left = 0.6, bottom = 0.6, right = 1, top = 1)
ggdraw(p1) + draw_plot(country_profile_radar_chart, x = .45, y = .45, width = .5, height = .5) 
# inset_element(p2, left = 0.6, bottom = 0.6, right = 1, top = 1)
ggdraw(p1) + draw_plot(x, x = .45, y = .45, width = .5, height = .5) 
# inset_element(p2, left = 0.6, bottom = 0.6, right = 1, top = 1)

ggdraw(p1) + draw_plot(country_profile_radar_chart, x = .45, y = .45, width = .45, height = .45)


z_plot <- ggdraw(p1) +
        # draw_image("output/charts/x.png", x = 1, y = 1, hjust = 1, vjust = 1, width = 0.5, height = 0.5)
        draw_image(z, x = .5, y = .5, hjust = 1, vjust = 1, width = 0.35, height = 0.35)
z_plot

# save ggplot chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(z_plot)
dev.off()

# add emf to word doc
read_docx() %>%
        body_add_img(src = filename, width = 6, height = 6) %>%
        print(target = "output/charts/z.docx")










# convert to a ggplot
grid.newpage()
get_country_profile_radar_chart(df = chart_data, country = current_country)
country_profile_radar_chart <- recordPlot()
country_profile_radar_chart <- as.ggplot(as_grob(country_profile_radar_chart))
country_profile_radar_chart

# save ggplot as a png
ggsave(filename = str_c("output/charts/country_profile_radar_chart_", str_to_lower(current_country), ".png"), 
       plot = country_profile_radar_chart, dpi = 600)

# load png, crop, and resave as png
country_profile_radar_chart <- image_read(str_c("output/charts/country_profile_radar_chart_", 
                                                str_to_lower(current_country), ".png")) %>%
        # image_crop(image = ., geometry = geometry_area(width = 3600, height = 3200, x_off = 400, y_off = 300)) 
        image_crop(image = ., geometry = geometry_area(width = 4200, height = 2100, x_off = 200, y_off = 200)) 
# image_write(path = str_c("output/charts/country_profile_radar_chart_", 
#                          str_to_lower(current_country), ".png"))

# load cropped png
# country_profile_radar_chart <- readPNG(source = str_c("output/charts/country_profile_radar_chart_", 
#                                              str_to_lower(current_country), ".png"), native = TRUE)

# # convert cropped png to a ggplot object
# country_profile_radar_chart <- ggplot() + background_image(country_profile_radar_chart) + coord_fixed()



#///////////////////////////////////////////////

# usaid_png <- image_read(path = "output/charts/usaid_logo/Horizontal_RGB_294.png")

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
ggsave(filename = "output/charts/usaid_logo/usaid_logo_white_resized.png", plot = usaid_logo, dpi = 600, width = 7, height = 7)

image_read("output/charts/usaid_logo/usaid_logo_white_resized.png") %>% image_info() # w = 4200, h = 4200 (dpi * width)
usaid_logo <- image_read("output/charts/usaid_logo/usaid_logo_white_resized.png") %>%
        image_crop(image = ., geometry = geometry_area(width = 3600, height = 1400, x_off = 300, y_off = 1400)) 

blue_banner <- tibble(x = c(0, 1), y = c(1, 3), color = "dot_color") %>%
        ggplot(data = ., mapping = aes(x = x, y = y, color = color)) +
        geom_point() +
        scale_color_manual(values = "#002F6C") +
        theme_nothing() +
        theme(
                panel.background = element_rect(fill = "#002F6C"),
                plot.background = element_rect(fill = "#002F6C"),
                rect = element_rect(fill = "#002F6C")
        ) 

p1 <- ggplot(mtcars) + 
        geom_point(aes(mpg, disp))
# usaid_plot <- ggdraw(blue_banner) + draw_image(usaid_logo, x = .5, y = .5, hjust = 1, vjust = 1, width = 0.35, height = 0.35)
usaid_plot <- ggdraw(p1) + draw_image(usaid_logo, x = .5, y = .5, hjust = 1, vjust = 1, width = 0.35, height = 0.35)

# save ggplot chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(usaid_plot)
dev.off()

# add emf to word doc
read_docx() %>%
        body_add_img(src = filename, width = 6, height = 6) %>%
        print(target = "output/charts/usaid_plot.docx")



blue_banner <- tibble(x = c(0, 1), y = c(1, 3), color = "dot_color") %>%
        ggplot(data = ., mapping = aes(x = x, y = y, color = color)) +
        geom_point() +
        scale_color_manual(values = "#002F6C") +
        theme_nothing() +
        theme(
                panel.background = element_rect(fill = "#002F6C"),
                plot.background = element_rect(fill = "#002F6C"),
                rect = element_rect(fill = "#002F6C")
        ) 

# inset_element(blue_banner, left = -2, bottom = 7, right = 2, top = 8,
#               on_top = TRUE, align_to = "full")
# inset_element(usaid_logo, left = -.01, bottom = 7, right = .35, top = 7.4,
#               on_top = TRUE, align_to = "full")




library("rnaturalearth")
library("rnaturalearthdata")
library("rnaturalearthhires")

# https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html

world_sf <- ne_countries(scale = "large", returnclass = "sf")

# inspect
world_sf
world_sf %>% glimpse()
world_sf %>% nrow() # 241
world_sf %>% ncol() # 64

world_sf %>% as_tibble() %>% distinct(sovereignt) %>% nrow() # 200
world_sf %>% as_tibble() %>% distinct(sovereignt) %>% print(n = nrow(.))

# check country names
fmir %>% distinct(country) %>% filter(!(country %in% c("U.S.", "Russia"))) %>%
        anti_join(., world_sf %>% 
                          mutate(sovereignt = as.character(sovereignt),
                                 sovereignt = case_when(sovereignt == "Czech Republic" ~ "Czechia",
                                                        sovereignt == "Macedonia" ~ "N. Macedonia",
                                                        sovereignt == "Republic of Serbia" ~ "Serbia",
                                                        sovereignt == "United Kingdom" ~ "U.K.",
                                                        sovereignt == "Bosnia and Herzegovina" ~ "BiH",
                                                        TRUE ~ sovereignt)) %>%
                          as_tibble() %>% distinct(sovereignt), 
                  by = c("country" = "sovereignt"))

world_sf %>% 
        mutate(sovereignt = as.character(sovereignt),
               sovereignt = case_when(sovereignt == "Czech Republic" ~ "Czechia",
                                      sovereignt == "Macedonia" ~ "N. Macedonia",
                                      sovereignt == "Republic of Serbia" ~ "Serbia",
                                      sovereignt == "United Kingdom" ~ "U.K.",
                                      sovereignt == "Bosnia and Herzegovina" ~ "BiH",
                                      TRUE ~ sovereignt),
               fill_color_bin = "selected_color") %>%
        filter(sovereignt != "Antarctica") %>%
        filter(sovereignt == current_country) %>%
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







country_profile_divider <- tibble(x = c(0, 1), y = c(1, 3), color = "dot_color") %>%
        ggplot(data = ., mapping = aes(x = x, y = y, color = color)) +
        geom_point() +
        scale_color_manual(values = "#ffffff") +
        geom_segment(x = 0, y = 2, xend = 1, yend = 2, colour = "#7D7D7D", lineend = "round", size = 10) +
        theme_nothing() +
        theme(
                aspect.ratio = 1 / 100
        )
ggsave(filename = "output/charts/country_profile/country_profile_divider.png", 
       plot = country_profile_divider, dpi = 300, width = 6, height = 6)

# read resized usaid logo png and crop
image_read("output/charts/country_profile_divider.png") %>% image_info() # w = 1800, h = 1800 (dpi * width)
country_profile_divider_png <- image_read("output/charts/country_profile/country_profile_divider.png") %>%
        image_crop(image = ., geometry = geometry_area(width = 1800, height = 50, x_off = 0, y_off = 875)) 




p1 <- ggplot(mtcars) + 
        geom_point(aes(mpg, disp))
p1

z_plot <- ggdraw(p1) +
        draw_image(country_profile_divider, x = 1, y = 1, hjust = 1, vjust = 1, width = 1, height = 1)
z_plot

# save ggplot chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(z_plot)
dev.off()

# add emf to word doc
read_docx() %>%
        body_add_img(src = filename, width = 6, height = 6) %>%
        print(target = "output/charts/z.docx")




# create three_row_blue_rect_background ####
country_profile_divider <- tibble(x = c(0, 1), y = c(1, 3), color = "dot_color") %>%
        ggplot(data = ., mapping = aes(x = x, y = y, color = color)) +
        geom_point() +
        scale_color_manual(values = "#7D7D7D") +
        theme_nothing() +
        theme(
                aspect.ratio = 1 / 2.5,
                plot.background  = element_rect_round(fill = "#7D7D7D", linetype = "blank")
        ) 
country_profile_divider



country_profile_divider <- ggplot() + coord_fixed() +
        theme(
                aspect.ratio = 1 / 80,
                panel.background = element_rect(fill = "#7D7D7D"),
                plot.background = element_rect(fill = "#7D7D7D"),
                rect = element_rect(fill = "#7D7D7D")
        ) 



#///////////////////////


# save plot
ggsave(file = str_c("output/charts/country_profile/sub_obj_1_1_country_profile_dot_plot_", 
                    str_to_lower(current_country), ".png"), plot = sub_obj_1_1_country_profile_dot_plot, 
       dpi = 600, height = 6, width = 6)
image_read(str_c("output/charts/country_profile/sub_obj_1_1_country_profile_dot_plot_", 
                 str_to_lower(current_country), ".png")) %>% image_info() # h = 3600, w = 3600
sub_obj_1_1_country_profile_dot_plot_png <- image_read(str_c("output/charts/country_profile/sub_obj_1_1_country_profile_dot_plot_", 
                                                             str_to_lower(current_country), ".png")) %>%
        image_crop(image = ., geometry = geometry_area(width = 3400, height = 400, x_off = 10, y_off = 1600))


#/////////////////

country_profile_usaid_logo_png <- image_read("output/charts/usaid_logo/usaid_logo_white_resized.png") %>%
        image_crop(image = ., geometry = geometry_area(width = 1600, height = 600, x_off = 100, y_off = 600))

z_plot <- ggdraw(p1) + draw_image(country_profile_usaid_logo_png, x = .75, y = .75, hjust = 1, vjust = 1, width = .5, height = .5)

# save ggplot chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(z_plot)
dev.off()

# add emf to word doc
read_docx() %>%
        body_add_img(src = filename, width = 6, height = 6) %>%
        print(target = "output/charts/z.docx")


#///////////////////////////////


country_profile_blue_banner <- tibble(x = c(0, 0, 2, 2), y = c(0, 2, 2, 0)) %>%
        ggplot(data = ., mapping = aes(x = x, y = y)) +
        geom_rect(xmin = 0, xmax = 2, ymin = 0, ymax = 2, color = "#002F6C", fill = "#002F6C") +
        theme_nothing() +
        theme(
                aspect.ratio = 1 / 100
        )
country_profile_blue_banner 
ggsave(filename = "output/charts/usaid_logo/country_profile_blue_banner.png", 
       plot = country_profile_blue_banner, dpi = 300, width = 6, height = 6)

# crop
image_read("output/charts/usaid_logo/country_profile_blue_banner.png") %>% image_info() # w = 1800, h = 1800 (dpi * width)
country_profile_blue_banner_png <- image_read("output/charts/usaid_logo/country_profile_blue_banner.png") %>%
        image_crop(image = ., geometry = geometry_area(width = 1800, height = 16, x_off = 0, y_off = 892)) 

# country_profile_blue_banner <- ggplot() + 
#         theme(
#                 panel.background = element_rect(fill = "#002F6C"),
#                 plot.background = element_rect(fill = "#002F6C"),
#                 rect = element_rect(fill = "#002F6C")
#         ) 


#///////////////////////////////


# make a plot with blue background
p <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) + geom_point(color = "#C4E7FF") +
        theme_nothing()
        theme(plot.background = element_rect(fill = "#C4E7FF"),
              panel.background = element_blank(),
              plot.margin = margin(20, 20, 20, 20))
p

# switch out background grob
g <- ggplotGrob(p)
bg <- g$grobs[[1]]
round_bg <- roundrectGrob(x=bg$x, y=bg$y, width=bg$width, height=bg$height,
                          r=unit(0.1, "snpc"),
                          just=bg$just, name=bg$name, gp=bg$gp, vp=bg$vp)
g$grobs[[1]] <- round_bg

# draw both plots side by side
cowplot::plot_grid(p, g, labels = c("rectangular", "rounded"),
                   scale = 0.85, hjust = 0.5, label_x = 0.5)

plot(g)
z <- recordPlot()
z <- as.ggplot(as_grob(z))
z

#/////////////////////////////


# widening geom_shape via aspect.ratio or coord_fixed has a weird limit where it no longer plots the shape, 
# even though it seems like it should. enlargening the graphic device or ggsave size can remedy it sometimes,
# but it gets harder and harder to do the narrower the rectangle is; 
# this is just an issue with geom_shape though, geom_rect supports any aspect.ratio
# widening ggsave width just widens the printed white box, but the plot itself still retains its same aspect ratio

# to get wider/narrower shape on page, use combo of aspect.ratio, but then also larger draw_image width/height (cropped as needed)
# but getting rounded rectangle is problematic because geom_shape has an aspect ratio limit, 
# and element_rect_round has a weird outcome where it automatically fills the entire background when saved as png
# still, geom_shape limits on narrow/wide rectangle may not be binding for this case

# Expand and round
z <- tibble(x = c(0, 0, 2, 2), y = c(0, 2, 2, 0)) %>%
        ggplot(data = ., mapping = aes(x = x, y = y)) +
        # geom_rect(xmin = 0, xmax = 2, ymin = 0, ymax = 2, color = "red", fill = "red") +
        # geom_shape(radius = unit(2, "cm"), fill = "red") +
        # geom_segment(x = 0, y = 1, xend = 2, yend = 1, color = "red", size = 10, lineend = "round") +
        # scale_x_continuous(limits = c(0, 2)) +
        # scale_y_continuous(limits = c(0, 2)) +
        # coord_fixed(ratio = 1 / 4)
        # theme_nothing() +
        theme(
                aspect.ratio = 1 / 10,
                plot.background  = element_rect_round(fill = "red", linetype = "blank", radius = unit(.3, "snpc"))
        )
z
z <- tibble(x = c(0, 0, 2, 2), y = c(0, 2, 2, 0)) %>%
        ggplot(data = ., mapping = aes(x = x, y = y)) +
        geom_rect(xmin = 0, xmax = 2, ymin = 0, ymax = 2, color = "red", fill = "red") +
        # geom_shape(radius = unit(1, 'cm'), fill = "red") +
        # scale_x_continuous(limits = c(0, 2)) +
        # scale_y_continuous(limits = c(0, 2)) +
        # coord_fixed(ratio = 1 / 4)
        # theme_nothing() +
        theme(
                aspect.ratio = 1 / 25
              # plot.background  = element_rect_round(fill = "red", linetype = "blank", radius = unit(.05, "snpc"))
        )
z
ggsave(filename = "output/charts/country_profile/z.png", 
       plot = z, dpi = 600, height = 15, width = 15)
image_read(path = "output/charts/country_profile/z.png") %>% image_info()
z_png <- image_read(path = "output/charts/country_profile/z.png") %>%
        image_crop(image = ., geometry = geometry_area(width = 3600, height = 1000, x_off = 0, y_off = 1000)) 

# z_png
z_plot <- ggdraw(p1) + draw_image(z_png, x = .85, y = .85, hjust = 1, vjust = 1, width = .9, height = .9)

# save ggplot chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(z_plot)
dev.off()

# add emf to word doc
read_docx() %>%
        body_add_img(src = filename, width = 6, height = 6) %>%
        print(target = "output/charts/z.docx")


# blue_rect_background <- tibble(x = c(0, 1), y = c(1, 3), color = "dot_color") %>%
#         ggplot(data = ., mapping = aes(x = x, y = y, color = color)) +
#         geom_point() +
#         scale_color_manual(values = "#E0ECF7") +
#         theme_nothing() +
#         theme(
#                 aspect.ratio = 1 / 3,
#                 plot.background  = element_rect_round(fill = "#E0ECF7", linetype = "blank", radius = unit(.3, "snpc"))
#         )
# blue_rect_background

# note that aspect.ratio controls the relative height, and ggsave controls the width on page

two_row_blue_rect_background <- tibble(x = c(0, 0, 2, 2), y = c(0, 2, 2, 0)) %>%
        ggplot(data = ., mapping = aes(x = x, y = y)) +
        geom_shape(radius = unit(.5, 'cm'), fill = "red") +
        scale_x_continuous(limits = c(0, 2)) +
        scale_y_continuous(limits = c(0, 2)) +
        theme_nothing() +
        theme(aspect.ratio = 1 / 6)
# two_row_blue_rect_background
ggsave(filename = "output/charts/country_profile/two_row_blue_rect_background.png", 
       plot = two_row_blue_rect_background, dpi = 600, width = 6, height = 6)
two_row_blue_rect_background_png <- image_read(path = "output/charts/country_profile/two_row_blue_rect_background.png")


three_row_blue_rect_background <- tibble(x = c(0, 0, 2, 2), y = c(0, 2, 2, 0)) %>%
        ggplot(data = ., mapping = aes(x = x, y = y)) +
        geom_shape(radius = unit(.5, 'cm'), fill = "#E0ECF7") +
        scale_x_continuous(limits = c(0, 2)) +
        scale_y_continuous(limits = c(0, 2)) +
        theme_nothing() +
        theme(aspect.ratio = 1 / 3)
# three_row_blue_rect_background
ggsave(filename = "output/charts/country_profile/three_row_blue_rect_background.png", 
       plot = three_row_blue_rect_background, dpi = 600, width = 6, height = 6)
three_row_blue_rect_background_png <- image_read(path = "output/charts/country_profile/three_row_blue_rect_background.png")


six_row_blue_rect_background <- tibble(x = c(0, 0, 2, 2), y = c(0, 2, 2, 0)) %>%
        ggplot(data = ., mapping = aes(x = x, y = y)) +
        geom_shape(radius = unit(.5, 'cm'), fill = "green") +
        scale_x_continuous(limits = c(0, 2)) +
        scale_y_continuous(limits = c(0, 2)) +
        theme_nothing() +
        theme(aspect.ratio = 1 / 1)
# six_row_blue_rect_background
ggsave(filename = "output/charts/country_profile/six_row_blue_rect_background.png", 
       plot = six_row_blue_rect_background, dpi = 600, width = 6, height = 6)
six_row_blue_rect_background_png <- image_read(path = "output/charts/country_profile/six_row_blue_rect_background.png")


#/////////////////////////////


# create one_row_blue_rect_background
one_row_blue_rect_background <- tibble(x = c(0, 1), y = c(1, 3), color = "dot_color") %>%
        ggplot(data = ., mapping = aes(x = x, y = y, color = color)) +
        geom_point() +
        scale_color_manual(values = "#E0ECF7") +
        theme_nothing() +
        theme(
                aspect.ratio = 1 / 7,
                plot.background  = element_rect_round(fill = "#E0ECF7", linetype = "blank", radius = .5)
        )
one_row_blue_rect_background
ggsave(filename = "output/charts/country_profile/one_row_blue_rect_background.png", 
       plot = one_row_blue_rect_background, dpi = 600, width = 2, height = 2)
one_row_blue_rect_background_png <- image_read(path = "output/charts/country_profile/one_row_blue_rect_background.png")


#///////////////////////////


# create two_row_blue_rect_background
two_row_blue_rect_background <- tibble(x = c(0, 1), y = c(1, 3), color = "dot_color") %>%
        ggplot(data = ., mapping = aes(x = x, y = y, color = color)) +
        geom_point() +
        scale_color_manual(values = "#E0ECF7") +
        theme_nothing() +
        theme(
                aspect.ratio = 1 / 4,
                plot.background  = element_rect_round(fill = "#E0ECF7", linetype = "blank")
        )
two_row_blue_rect_background


#///////////////////////////


# create three_row_blue_rect_background
three_row_blue_rect_background <- tibble(x = c(0, 1), y = c(1, 3), color = "dot_color") %>%
        ggplot(data = ., mapping = aes(x = x, y = y, color = color)) +
        geom_point() +
        scale_color_manual(values = "#E0ECF7") +
        theme_nothing() +
        theme(
                aspect.ratio = 1 / 3,
                plot.background  = element_rect_round(fill = "#E0ECF7", linetype = "blank")
        )
three_row_blue_rect_background


# create six_row_blue_rect_background
six_row_blue_rect_background <- tibble(x = c(0, 1), y = c(1, 3), color = "dot_color") %>%
        ggplot(data = ., mapping = aes(x = x, y = y, color = color)) +
        geom_point() +
        scale_color_manual(values = "#E0ECF7") +
        theme_nothing() +
        theme(
                aspect.ratio = 1 / 2,
                plot.background  = element_rect_round(fill = "#E0ECF7", linetype = "blank")
        )
six_row_blue_rect_background



# layout <- c(
#         area(t = 1, l = 1, b = top_section_height, r = page_width), # spacer 1 to 1
#         
#         # dot_plot row 1
#         area(t = (top_section_height) + 1, 
#              l = (side_margin) + 1, 
#              b = (top_section_height) + dot_plot_ri_height, 
#              r = (side_margin) + dot_plot_width), # dot_plot_ri 
#         area(t = (top_section_height) + 1, 
#              l = (side_margin + dot_plot_width + center_margin) + 1, 
#              b = (top_section_height) + dot_plot_obj_1_height, 
#              r = (side_margin + dot_plot_width + center_margin) + dot_plot_width), # dot_plot_obj_1
#         
#         # dot_plot row 2
#         area(t = (top_section_height + dot_plot_ri_height + below_margin) + 1, 
#              l = (side_margin) + 1, 
#              b = (top_section_height + dot_plot_ri_height + below_margin) + dot_plot_obj_2_height, 
#              r = (side_margin) + dot_plot_width), # dot_plot_obj_2
#         area(t = (top_section_height + dot_plot_ri_height + below_margin) + 1, 
#              l = (side_margin + dot_plot_width + center_margin) + 1, 
#              b = (top_section_height + dot_plot_ri_height + below_margin) + dot_plot_obj_3_height, 
#              r = (side_margin + dot_plot_width + center_margin) + dot_plot_width), # dot_plot_obj_3
#         
#         area(t = (top_section_height + dot_plot_ri_height + below_margin + dot_plot_obj_3_height + below_margin) + 1, 
#              l = 1, 
#              b = page_height, 
#              r = page_width)
# )
# plot(layout)



# draw images
country_profile <- ggdraw(country_profile_layout) +
        
        # draw banner and logo
        draw_image(country_profile_blue_banner_png, x = .5, y = .97, hjust = .5, vjust = .5, width = 7, height = 7) +
        draw_image(country_profile_usaid_logo_png, x = .12, y = .969, hjust = 0, vjust = .5, width = 0.14, height = 0.14) +
        
        # draw map and flag
        draw_image(country_profile_map_png, x = .5, y = .86, hjust = 1, vjust = .5, width = 0.15, height = 0.15) +
        draw_image(country_profile_flag_png, x = .4455, y = .868, hjust = 0, vjust = .5, width = 0.05, height = 0.05) +
        
        # draw blue_rect_backgrounds for dot plots
        draw_image(six_row_blue_rect_background_png, x = .51, y = .5, hjust = 1, vjust = 1, width = .4, height = .4) +
        draw_image(six_row_blue_rect_background_png, x = .895, y = .5, hjust = 1, vjust = 1, width = .4, height = .4) +
        # draw_image(three_row_blue_rect_background_png, x = .45, y = .55, hjust = 1, vjust = 1, width = .4, height = .4) +
        # draw_image(three_row_blue_rect_background_png, x = .95, y = .55, hjust = 1, vjust = 1, width = .4, height = .4) +
        # draw_image(two_row_blue_rect_background_png, x = .45, y = .35, hjust = 1, vjust = 1, width = .45, height = .45) +
        # draw_image(two_row_blue_rect_background_png, x = 1.05, y = .35, hjust = 1, vjust = 1, width = .45, height = .45) +
        
        # draw divider
        draw_image(country_profile_divider_png, x = .092, y = .5, hjust = 0, vjust = .5, width = .82, height = .82) +
        
        # draw dot plots
        draw_image(sub_obj_1_1_country_profile_dot_plot_png, x = .139, y = .4, hjust = 0, vjust = .5, width = 0.34, height = 0.34) +
        draw_image(sub_obj_1_1_country_profile_dot_plot_png, x = .139, y = .372, hjust = 0, vjust = .5, width = 0.34, height = 0.34) +
        # draw_image(sub_obj_1_3_country_profile_dot_plot_png, x = .85, y = .85, hjust = 1, vjust = 1, width = 0.15, height = 0.15) +
        
        # draw radar chart
        draw_image(country_profile_radar_chart_png, x = .85, y = .85, hjust = 1, vjust = 1, width = 0.15, height = 0.15) +
        
        # draw title
        draw_label(label = current_country_full_name, color = "#002F6C", size = 20, angle = 0,
                   fontface = "bold", fontfamily = "Gill Sans MT",
                   x = .13, y = .9, hjust = 0, vjust = .5) +
        draw_label(label = "Resilience Index: 2020 Country Profile", color = "#002F6C", size = 10, angle = 0,
                   fontface = "plain", fontfamily = "Gill Sans MT",
                   x = .13, y = .87, hjust = 0, vjust = .5) 

# inspect
# country_profile


#//////////////////////////////////////////////////////////////////////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(country_profile)
dev.off()

# add emf to word doc
output_name <- str_c("output/charts/country_profile_", str_to_lower(current_country_full_name), ".docx")
read_docx() %>%
        body_add_img(src = filename, width = 11.68, height = 11.68) %>%
        print(target = output_name)























country_profile_flag <- image_read(path = str_c("output/charts/flags/bih.svg"))
country_profile_flag <- ggplot() + background_image(country_profile_flag) +
        coord_fixed(ratio = 1 / 2)
# country_profile_flag

ggsave(filename = str_c("output/charts/flags/bih.png"), plot = country_profile_flag, 
       dpi = 300, width = 6, height = 6)

# crop
# image_read(str_c("output/charts/flags/", current_country_flag_abbreviation, ".png")) %>% image_info() # w = 1800, h = 1800 (dpi * width)
country_profile_flag_png <- image_read(str_c("output/charts/flags/bih.png")) %>%
        image_crop(image = ., geometry = geometry_area(width = 1748, height = 875, x_off = 29, y_off = 465)) 



#//////////////


# old

# current_country_flag_abbreviation <- case_when(current_country == "Albania" ~ "al",
#                                current_country == "Armenia" ~ "am",
#                                current_country == "BiH" ~ "ba",
#                                TRUE ~ "xx")
# current_country_flag_abbreviation

country_profile_flag <- image_read(path = str_c("output/charts/flags/", current_country, ".svg"))
# country_profile_flag <- image_read(path = str_c("output/charts/flags/", current_country_flag_abbreviation, ".svg"))
country_profile_flag <- ggplot() + background_image(country_profile_flag) + coord_fixed(ratio = 2 / 3)
# country_profile_flag

ggsave(filename = str_c("output/charts/flags/", current_country_flag_abbreviation, ".png"), plot = country_profile_flag, 
       dpi = 300, width = 6, height = 6)

# crop
# image_read(str_c("output/charts/flags/", current_country_flag_abbreviation, ".png")) %>% image_info() # w = 1800, h = 1800 (dpi * width)
country_profile_flag_png <- image_read(str_c("output/charts/flags/", current_country_flag_abbreviation, ".png")) %>%
        image_crop(image = ., geometry = geometry_area(width = 1754, height = 1171, x_off = 23, y_off = 315)) 








#////////////////////////////////////


tibble(fmir %>% distinct(sub_obj_short_name)) %>%
        mutate(sub_obj_dotplot_name = case_when(sub_obj_short_name == "Checks and balances and rule of law" ~ 
                                                        "Checks / balances\nand rule of law",
                                                sub_obj_short_name == "Resilience to electoral/political interference and polarization" ~
                                                        "Electoral / political\ninterference",
                                                sub_obj_short_name %in% 
                                                        c("Media professionalism", "Trusted media/information") ~ 
                                                        "Trusted media /\ninformation",
                                                sub_obj_short_name == "Independence from Russian energy" ~ 
                                                        "Indepence from\nRussian energy",
                                                # sub_obj_short_name == "Energy regulation" ~ "Energy\nregulation",
                                                sub_obj_short_name == "Economic independence" ~ "Economic\nindependence",
                                                # sub_obj_short_name %in% c("Financial practices", "Financial standards") ~ 
                                                        # "Financial\npractices",
                                                sub_obj_short_name == "Control of corruption" ~ "Corruption",
                                                TRUE ~ sub_obj_short_name)) %>%
        mutate(nchar = nchar(sub_obj_short_name)) %>%
        ggplot(data = ., mapping = aes(x = sub_obj_dotplot_name, y = nchar)) +
        geom_col() + coord_flip() + 
        theme(
                axis.text.y = element_text(family = "Gill Sans MT", face = "plain", size = 9, color = "#333333",
                                           margin = margin(t = 10, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
        )


# axis.text.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
#                            margin = margin(t = 0, r = -20, b = 0, l = 0)),

image_crop(image = ., geometry = geometry_area(width = 1800, height = 153, x_off = 0, y_off = 825))





# top/bottom margin are height of magnifying glass
six_row_blue_rect_background <- tibble(x = c(0, 0, 2, 2), y = c(0, 2, 2, 0)) %>%
        ggplot(data = ., mapping = aes(x = x, y = y)) +
        geom_shape(radius = unit(.5, 'cm'), fill = "#E0ECF7") +
        scale_x_continuous(limits = c(0, 2)) +
        scale_y_continuous(limits = c(0, 2)) +
        theme_nothing() +
        theme(aspect.ratio = 1 / 1.75)
# six_row_blue_rect_background
ggsave(filename = "output/charts/country_profile/six_row_blue_rect_background.png", 
       plot = six_row_blue_rect_background, dpi = 300, width = 6, height = 6)
six_row_blue_rect_background_png <- image_read(path = "output/charts/country_profile/six_row_blue_rect_background.png") %>%
        image_crop(image = ., geometry = geometry_area(width = 1800, height = 1000, x_off = 0, y_off = 400))




# top/bottom margin are height of magnifying glass
three_row_blue_rect_background <- tibble(x = c(0, 0, 2, 2), y = c(0, 2, 2, 0)) %>%
        ggplot(data = ., mapping = aes(x = x, y = y)) +
        geom_shape(radius = unit(.5, 'cm'), fill = "#E0ECF7") +
        scale_x_continuous(limits = c(0, 2)) +
        scale_y_continuous(limits = c(0, 2)) +
        theme_nothing() +
        # theme(aspect.ratio = 1 / 3)
        theme(aspect.ratio = 1 / 2.95)

# three_row_blue_rect_background
ggsave(filename = "output/charts/country_profile/three_row_blue_rect_background.png", 
       plot = three_row_blue_rect_background, dpi = 300, width = 6, height = 6)
three_row_blue_rect_background_png <- image_read(path = "output/charts/country_profile/three_row_blue_rect_background.png") %>%
        # image_crop(image = ., geometry = geometry_area(width = 1800, height = 584, x_off = 0, y_off = 608)) # 1/3
        # image_crop(image = ., geometry = geometry_area(width = 1800, height = 600, x_off = 0, y_off = 600)) # 1/2.9
        image_crop(image = ., geometry = geometry_area(width = 1800, height = 590, x_off = 0, y_off = 605)) # 1/2.95

        # image_crop(image = ., geometry = geometry_area(width = 1800, height = 564, x_off = 0, y_off = 618)) # 1/3.1




# top/bottom margin are height of magnifying glass
two_row_blue_rect_background <- tibble(x = c(0, 0, 2, 2), y = c(0, 2, 2, 0)) %>%
        ggplot(data = ., mapping = aes(x = x, y = y)) +
        geom_shape(radius = unit(.5, 'cm'), fill = "#E0ECF7") +
        scale_x_continuous(limits = c(0, 2)) +
        scale_y_continuous(limits = c(0, 2)) +
        theme_nothing() +
        # theme(aspect.ratio = 1 / 3)
        theme(aspect.ratio = 1 / 3.8)

# two_row_blue_rect_background
ggsave(filename = "output/charts/country_profile/two_row_blue_rect_background.png", 
       plot = two_row_blue_rect_background, dpi = 300, width = 6, height = 6)
two_row_blue_rect_background_png <- image_read(path = "output/charts/country_profile/two_row_blue_rect_background.png") %>%
        # image_crop(image = ., geometry = geometry_area(width = 1800, height = 446, x_off = 0, y_off = 677))
        image_crop(image = ., geometry = geometry_area(width = 1800, height = 466, x_off = 0, y_off = 667))



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




# top/bottom margin are height of magnifying glass
legend_blue_rect_background <- tibble(x = c(0, 0, 2, 2), y = c(0, 2, 2, 0)) %>%
        ggplot(data = ., mapping = aes(x = x, y = y)) +
        geom_shape(radius = unit(.5, 'cm'), fill = "#E0ECF7") +
        scale_x_continuous(limits = c(0, 2)) +
        scale_y_continuous(limits = c(0, 2)) +
        theme_nothing() +
        theme(aspect.ratio = 1 / 4.15) 

# legend_blue_rect_background
ggsave(filename = "output/charts/country_profile/legend_blue_rect_background.png", 
       plot = legend_blue_rect_background, dpi = 300, width = 6, height = 6)
legend_blue_rect_background_png <- image_read(path = "output/charts/country_profile/legend_blue_rect_background.png") %>%
        image_crop(image = ., geometry = geometry_area(width = 1800, height = 430, x_off = 0, y_off = 685))




while((chart_data %>% mutate(sum_overlap_flag = sum(overlap_flag)) %>% slice(1) %>% pull(sum_overlap_flag)) != 0) {
        
        counter <- counter + 1
        print(counter + 1)
        
        chart_data <- chart_data %>% arrange(country, year, values) %>% 
                group_by(country, year) %>%
                mutate(label_position = case_when(is.na(dist_to_lower) | dist_to_lower >= .02 ~ label_position,
                                                  TRUE ~ label_position + (.02 - dist_to_lower)),
                       dist_to_higher = lead(label_position, n = 1) - label_position,
                       dist_to_lower = label_position - lag(label_position, n = 1),
                       overlap_flag = case_when(dist_to_lower <= .01 ~ 1,
                                                TRUE ~ 0)) %>%
                ungroup()
}




fmir %>% filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs")) %>% 
        distinct(country) %>% pull(country)








filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(country_profile)
dev.off()

# add emf to word doc
output_name <- str_c("output/charts/country_profile_", str_to_lower(current_country), ".docx")
read_docx() %>%
        body_add_img(src = filename, width = 11.68, height = 11.68) %>%
        print(target = output_name)





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




radar_chart_gray_border <- tibble(x = c(0, 0, 2, 2), y = c(0, 2, 2, 0)) %>%
        ggplot(data = ., mapping = aes(x = x, y = y)) +
        geom_shape(radius = unit(.5, 'cm'), color = "#CBCBCB", size = .5, fill = NA) +
        scale_x_continuous(limits = c(0, 2)) +
        scale_y_continuous(limits = c(0, 2)) +
        theme_nothing() +
        theme(aspect.ratio = 1 / 1.5) 
radar_chart_gray_border


ggsave(filename = "output/charts/country_profile/radar_chart_gray_border.png", 
       plot = radar_chart_gray_border, dpi = 300, width = 6, height = 6)
radar_chart_gray_border_png <- image_read(path = "output/charts/country_profile/radar_chart_gray_border.png") %>%
        # image_crop(image = ., geometry = geometry_area(width = 1700, height = 1150, x_off = 50, y_off = 325))
        image_crop(image = ., geometry = geometry_area(width = 1700, height = 1126, x_off = 50, y_off = 337))




# "#1B69AF" is 3

# line chart y = .616 to start
# .618
# .62 and .622





#////////////////////////


# create get_label_position() for use in facet charts (note this is not tailored for us in country profiles)
get_label_position <- function(chart_data) {
        
        # get initial label_position
        chart_data <- chart_data %>% arrange(year, values) %>% 
                group_by(year) %>%
                mutate(label_position = round(values, digits = 3),
                       dist_to_lower = round(label_position - lag(label_position, n = 1), digits = 3),
                       dist_to_higher = round(lead(label_position, n = 1) - label_position, digits = 3),
                       overlap_flag = case_when(dist_to_lower < .03 ~ 1,
                                                TRUE ~ 0)) %>%
                ungroup()
        
        # set counter
        counter <- 0
        
        # run loop to update label_position
        while((chart_data %>% mutate(sum_overlap_flag = sum(overlap_flag)) %>% slice(1) %>% pull(sum_overlap_flag)) != 0) {
                
                counter <- counter + 1
                print(counter + 1)
                
                chart_data <- chart_data %>% arrange(year, values) %>% 
                        group_by(year) %>%
                        mutate(label_position = case_when(is.na(dist_to_lower) | 
                                                                  dist_to_lower >= .03 ~ round(label_position, digits = 3),
                                                          TRUE ~ round(label_position + (.03 - dist_to_lower), digits = 3)),
                               dist_to_higher = round(lead(label_position, n = 1) - label_position, digits = 3),
                               dist_to_lower = round(label_position - lag(label_position, n = 1), digits = 3),
                               overlap_flag = case_when(dist_to_lower < .03 ~ 1,
                                                        TRUE ~ 0)) %>%
                        ungroup() %>%
                        arrange(desc(year), values)
        }
        
        # return
        return(chart_data)
}



