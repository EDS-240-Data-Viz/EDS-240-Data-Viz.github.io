# FROM SLIDES

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                    setup                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#..........................load packages.........................
library(tidyverse)
library(janitor)
library(scales)

#..........................import data...........................
water_use <- read_csv(here::here("course-materials", "data", "lecture", "combined_iwa-assessment-outputs-conus-2025_CONUS_200910-202009_long.csv"))

#..................create df of subregion names..................
# data only contain HUC codes; must manually join names if we want to include those in our viz
# subregions identified in https://water.usgs.gov/GIS/wbd_huc8.pdf
subregions <- tribble(
  ~subregion_HUC, ~subregion_name,
  1801L, "Klamath-Northern California Coastal",
  1802L, "Sacramento",
  1803L, "Tulare-Buena Vista Lakes",
  1804L, "San Joaquin",
  1805L, "San Francisco Bay",
  1806L, "Central California Coastal",
  1807L, "Southern California Coastal",
  1808L, "North Lahontan", 
  1809L, "Northern Mojave-Mono Lake",
  1810L, "Southern Mojave-Salton Sea"
) |> 
  mutate(subregion_HUC = as.factor(subregion_HUC))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                wrangle data                              ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ca_region <- water_use |> 

  # make nicer column names ---
  clean_names() |> 

  # create columns for 2-unit (region-level) and 4-unit (subregion-level) HUCs using full 12-unit HUC ----
  mutate(
    region_HUC = str_sub(string = huc12_id, start = 1, end = 2),
    subregion_HUC = str_sub(string = huc12_id, start = 1, end = 4)
  ) |> 

  # filter for just CA region (HUC 18) ----
  filter(region_HUC == "18") |> 
 
  # separate year and month into two columns ----
  separate_wider_delim(cols = year_month,
                       delim = "-",
                       names = c("year", "month")) |> 

  # convert year and month from chr to num ---                     
  mutate(year = as.numeric(year),
         month = as.numeric(month)) |> 

  # coerce subregion_HUC from chr to factor ----
  mutate(subregion_HUC = as.factor(subregion_HUC)) |> 

  # join subregion names from our subregions df (common key = subregion_huc) ----
  left_join(subregions) |> 

  # reorder columns (not necesssary, but personally easier for me to navigate) ----
  relocate(year, month, huc12_id, region_HUC, subregion_HUC, subregion_name, availab_mm_mo, consum_mm_mo, strflow_mm_mo, sui_frac)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            explore missing data                          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sub1805 <- ca_region |> 
  filter(subregion_HUC == "1805") 

see_NAs <- ca_region |> 
  group_by(year) |> 
  naniar::miss_var_summary() |>
  filter(variable == "availab_mm_mo")

avail <- sub1805 |> select(availab_mm_mo)
missing_avail <- naniar::vis_miss(avail)

ca_region |> 
  group_by(subregion_name) |> 
  summarise(avg_sui = mean(sui_frac, na.rm = TRUE)) |> 
  mutate(subregion_name = fct_reorder(.f = subregion_name, .x = avg_sui)) |>
  ggplot(aes(x = avg_sui, y = subregion_name)) +
  geom_col() +
  geom_text(aes(label = round(avg_sui, 2)), hjust = 1.2, color = "white") 

ca_region |> 
  group_by(subregion_name) |> 
  summarise(avg_sui = mean(sui_frac, na.rm = TRUE)) |> 
  mutate(subregion_name = fct_reorder(.f = subregion_name, .x = avg_sui)) |>
  ggplot(aes(x = avg_sui, y = subregion_name)) +
  geom_point() +
  geom_linerange(aes(xmin = 0, xmax = avg_sui)) +
  geom_text(aes(label = round(avg_sui, 2)), hjust = -0.3) + 
  scale_x_continuous(limits = c(0, 0.6))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                wrangle data                              ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#..........df of average annual SUI by subresion & year..........
avg_annual_sui <- ca_region |> 
  select(year, month, subregion_HUC, subregion_name, sui_frac) |> 
  group_by(subregion_name, year) |> 
  summarise(
    avg_ann_sui = mean(sui_frac, na.rm = TRUE)
  ) |> 
  ungroup()

 #.determine order of subregions based on highest avg SUI in 2015.
order_2015 <- avg_annual_sui |> 
  filter(year == 2015) |> 
  arrange(avg_ann_sui) |> 
  mutate(order = row_number()) |> 
  select(avg_ann_sui, order) 

#........join order with rest of data to set factor levels.......
heatmap_order <- avg_annual_sui |> 
  left_join(order_2015, by = "avg_ann_sui") |> 
  mutate(subregion_name = fct_reorder(subregion_name, order))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                               create heatmap                             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#.........................create heatmap.........................
ggplot(heatmap_order, aes(x = year, y = subregion_name, fill = avg_ann_sui)) +
  geom_tile() +
  labs(fill = "Average Surface Water-Supply and\nUse Index (SUI)") + # caption = "A higher index value indicates greater water stress."
  scale_fill_viridis_c() +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  guides(fill = guide_colorbar(barwidth = 15, barheight = 0.75, title.position = "top")) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                wrangle data                              ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

avail_vs_consum <- ca_region |> 
  group_by(subregion_name) |>
  summarize(mean_avail = mean(availab_mm_mo, na.rm = TRUE),
            mean_consum = mean(consum_mm_mo, na.rm = TRUE)) |> 
  mutate(subregion_name = fct_reorder(.f = subregion_name, .x = mean_avail))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            create dumbbell chart                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ggplot(avail_vs_consum) +
  geom_linerange(aes(y = subregion_name,
                     xmin = mean_consum, xmax = mean_avail)) + 
  geom_point(aes(x = mean_avail, y = subregion_name), 
             color = "#448F9C", 
             size = 4) +
  geom_point(aes(x = mean_consum, y = subregion_name), 
             color = "#9C7344", 
             shape = 17,
             size = 4)



# --------------------------

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                    setup                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#..........................load packages.........................
library(tidyverse)
library(janitor)
library(scales)
library(showtext)

#..........................import data...........................
iwa_data <- read_csv(here::here("course-materials", "data", "lecture", "combined_iwa-assessment-outputs-conus-2025_CONUS_200910-202009_long.csv"))

#..................create df of subregions names.................
# data only contain HUC codes; must manually join names if we want to include those in our viz (which we do! we'll mainly be looking at CA subregions)
# subregions (& others) identified in: https://water.usgs.gov/GIS/wbd_huc8.pdf
# there may be a downloadable dataset containing HUCs & names out there...but I couldn't find it
subregion_names <- tribble(
  ~subregion_HUC, ~subregion_name,
  "1801", "Klamath-Northern California Coastal",
  "1802", "Sacramento",
  "1803", "Tulare-Buena Vista Lakes",
  "1804", "San Joaquin",
  "1805", "San Francisco Bay",
  "1806", "Central California Coastal",
  "1807", "Southern California Coastal",
  "1808", "North Lahontan", 
  "1809", "Northern Mojave-Mono Lake",
  "1810", "Southern Mojave-Salton Sea",
)

#......................import Google fonts.......................
# `name` is the name of the font as it appears in Google Fonts
# `family` is the user-specified id that you'll use to apply a font in your ggpplot
font_add_google(name = "Sarala", family = "sarala")
font_add_google(name = "Noto Sans", family = "noto-sans")
font_add_google(name = "Red Hat Mono", family = "red")

#....................import Font Awesome fonts...................
font_add(family = "fa-brands",
         regular = here::here("fonts", "Font Awesome 7 Brands-Regular-400.otf"))
font_add(family = "fa-regular",
         regular = here::here("fonts", "Font Awesome 7 Free-Regular-400.otf")) 
font_add(family = "fa-solid",
         regular = here::here("fonts", "Font Awesome 7 Free-Solid-900.otf"))


#................enable {showtext} for rendering.................
showtext_auto()
showtext_opts(dpi = 300)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                wrangle data                              ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#......create df with just CA water resource region (HUC 18).....
ca_region <- iwa_data |> 
  clean_names() |> 
  mutate(region_HUC = str_sub(string = huc12_id, start = 1, end = 2),
         subregion_HUC = str_sub(string = huc12_id, start = 1, end = 4)) |> 
  filter(region_HUC == "18") |> 
  separate_wider_delim(cols = year_month,
                       delim = "-",
                       names = c("year", "month")) |>                            
  mutate(year = as.numeric(year),
         month = as.numeric(month)) |> 
  left_join(subregion_names) |> 
  select(year, month, huc12_id, region_HUC, subregion_HUC, subregion_name, availab_mm_mo, consum_mm_mo, sui_frac)

#.................create df of just SBC subbasin.................
sbc_subbasin_monthly <- ca_region |> 
  mutate(subbasin_HUC = str_sub(string = huc12_id, start = 1, end = 8)) |> 
  filter(subbasin_HUC == "18060013") |> #
  group_by(month) |> 
  summarize(mean_avail = mean(availab_mm_mo, na.rm = TRUE),
            mean_consum = mean(consum_mm_mo, na.rm = TRUE)) |> 
  mutate(month = month.abb[month],
         month = factor(month, levels = rev(c(month.abb[10:12], month.abb[1:9]))))

pal <- c("avail" = "#448F9C",
         "consum" = "#9C7344",
         "dark_text" = "#0C1509",
         "light_text" = "#4E514D") 

monochromeR::view_palette(pal)

title <- glue::glue("Water
                    <span style='color:#448F9C;'>**supply**</span> 
                    vs. 
                    <span style='color:#9C7344;'>**demand**</span>
                    across the Santa Barbara Coastal<br>
                    subbasin (2010-2020)")

github_icon <- "&#xf09b"
github_username <- "samanthacsik"

caption <- glue::glue(
  "Data Source: USGS CONUS 2025<br>
  <span style='font-family:fa-brands;'>{github_icon};</span>
  {github_username}"
)    


ggplot(sbc_subbasin_monthly) +
  geom_linerange(aes(y = month,
                     xmin = mean_consum, xmax = mean_avail)) + 
  geom_point(aes(x = mean_avail, y = month), 
             color = pal["avail"], 
             size = 5.5,
            stroke = 2) +
  geom_point(aes(x = mean_consum, y = month), 
             color = pal["consum"], 
             fill = "white",
             shape = 21,
             stroke = 2,
             size = 5.5) +
  labs(title = title,
       subtitle = "Winter water availability exceeds demand, but summer months experience deficits",
       caption = caption,
       x = "Mean monthly water supply & demand (mm)") +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.title = ggtext::element_textbox(family = "sarala",
                              face = "bold",
                              size = 22,
                              lineheight = 1.2,
                              color = pal["dark_text"]),
    plot.subtitle = element_text(family = "noto-sans",
                                 size = 16,
                                 color = pal["light_text"],
                                 margin = margin(t = 3, r = 0, b = 8, l = 0)),
    axis.text = element_text(family = "red",
                             size = 13,
                             color = pal["light_text"]),
    axis.title.x = element_text(family = "noto-sans",
                                size = 14,
                                margin = margin(t = 20, r = 0, b = 0, l = 0)),
    axis.title.y = element_blank(),
    plot.caption = ggtext::element_textbox(family = "noto-sans",
                                face = "italic",
                                color = pal["light_text"],
                                halign = 1, 
                                lineheight = 1.5,
                                margin = margin(t = 15, r = 0, b = 0, l = 0)),
    plot.margin = margin(t = 1, r = 1, b = 0.5, l = 1, "cm")
  )

#https://santabarbaraca.gov/government/departments/public-works/water-resources/water-system/water-sources/groundwater

ggsave(
  filename = here::here("course-materials", "1-DPI300.png"),
  plot = last_plot(), # otherwise, save your plot as an object, and pass the `plot` argument your plot object name
  device = "png",
  width = 8, 
  height = 7,
  unit = "in",
  dpi = 300
)
