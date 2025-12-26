# https://www.data-is-plural.com/archive/2025-02-26-edition/ (found out about data here)
# https://www.usgs.gov/special-topics/integrated-water-availability-assessments/national-water-availability-assessments
# https://water.usgs.gov/nwaa-data/
# https://www.usgs.gov/media/images/water-supply-vs-demand-2010-2020 (example viz)

# HUCs explained: https://nas.er.usgs.gov/hucs.aspx
# SUI: The USGS Surface Water-Supply and Use Index (SUI) shows water limitation in a watershed (HUC12), from 0 (no limit) to 1 (total depletion), by comparing median long-term supply to current supply minus consumptive uses (irrigation, power, public) and climate variability. A higher SUI means more water is unavailable, indicating greater stress (e.g., SUI=0.2 means 80% available). You interpret it by looking at its value within a HUC12—a SUI near 1 signals severe stress, while near 0 suggests ample supply, guiding water management decisions for environmental and societal needs. 
# CA water resource region: https://en.wikipedia.org/wiki/California_water_resource_region#:~:text=Article,California%2C%20Nevada%2C%20and%20Oregon.
# USGS glossary: https://water.usgs.gov/vizlab/water-availability/glossary

### dataset download options
# https://doi-usgs.github.io/dataRetrieval/ (R package API)
# download subsetted data: https://water.usgs.gov/nwaa-data/subset-download?model=iwa-assessment-outputs-conus-2025&variable=availab,consum
# download full integrated datasets: https://water.usgs.gov/nwaa-data/data-file-directory?path=data/
  # I downloaded this: https://water.usgs.gov/nwaa-data/data-file-directory?path=data/integrated-water-availability/iwa-assessment-outputs-conus-2025/ 
  # metadata: https://water.usgs.gov/nwaa-data/data/integrated-water-availability/iwa-assessment-outputs-conus-2025/iwa-assessment-outputs-conus-2025_README.html

library(tidyverse)
library(janitor)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                import data                               ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# import usgs data ---
water_use <- read_csv(here::here("course-materials", "data", "lecture", "combined_iwa-assessment-outputs-conus-2025_CONUS_200910-202009_long.csv"))

# create df of subregion names to append to usgs data; extracted from https://water.usgs.gov/GIS/wbd_huc8.pdf ----
subregion_names <- tribble(
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
##                    wrangle data and filter for just CA                   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ca_region <- water_use |> 
  clean_names() |> 
  mutate(
    region_HUC = str_sub(string = huc12_id, start = 1, end = 2),
    subregion_HUC = str_sub(string = huc12_id, start = 1, end = 4)
  ) |> 
  filter(region_HUC == "18") |> 
  separate_wider_delim(cols = year_month,
                       delim = "-",
                       names = c("year", "month")) |> 
  mutate(year = as.numeric(year),
         month = as.numeric(month)) |> 
  filter(year != 2009) |> 
  mutate(subregion_HUC = as.factor(subregion_HUC)) |> 
  left_join(subregion_names) |> 
  relocate(year, month, huc12_id, region_HUC, subregion_HUC, subregion_name, availab_mm_mo, consum_mm_mo, strflow_mm_mo, sui_frac)

#......................explore missing data......................
# test_1805 <- ca_region_dumbell |> 
#   filter(subregion == "1805") 

# see_NAs <- ca_region_dumbell |> 
#   group_by(year) |> 
#   naniar::miss_var_summary() |>
#   filter(variable == "availab_mm_mo")

# avail <- test_1805 |> select(availab_mm_mo)
# missing_avail <- naniar::vis_miss(avail)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                 create bar chart & lollipop of average SUI               ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#....................create df of average SUI....................
avg_sui <- ca_region |> 
  select(year, month, subregion_HUC, subregion_name, sui_frac) |> 
  group_by(subregion_name) |> 
  summarise(
    avg_sui = mean(sui_frac, na.rm = TRUE)
  )
  
#.....................create basic bar chart.....................
avg_sui |> 
  mutate(subregion_name = fct_reorder(.f = subregion_name, .x = avg_sui)) |>
  ggplot(aes(x = subregion_name, y = avg_sui)) +
  geom_col() + 
  geom_text(aes(label = round(avg_sui, 2)), hjust = 1.2, color = "white") + 
  coord_flip()

#......................create basic lollipop.....................
avg_sui |> 
  mutate(subregion_name = fct_reorder(.f = subregion_name, .x = avg_sui)) |>
  ggplot(aes(x = subregion_name, y = avg_sui)) + 
  geom_point() +
  geom_linerange(aes(ymin = 0, ymax = avg_sui)) +
  geom_text(aes(label = round(avg_sui, 2)), hjust = -0.2) + 
  scale_y_continuous(limits = c(0, 0.6)) + # expand axis to make room for values
  coord_flip()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                    example of geom_bar() vs geom_col()                   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# tbd


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                    create heatmap of avg SUI over years                  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# heatmap (x: year, y = subregion, fill = avg_sui)
avg_annual_sui <- ca_region |> 
  select(year, month, subregion_HUC, subregion_name, sui_frac) |> 
  group_by(subregion_name, year) |> 
  summarise(
    avg_ann_sui = mean(sui_frac, na.rm = TRUE)
  ) |> 
  ungroup()

# determine order of subregions based on highest avg SUI in 2015 (end-ish of drought) ----
order_2015 <- avg_annual_sui |> 
  filter(year == 2015) |> 
  arrange(avg_ann_sui) |> 
  mutate(order = row_number()) |> 
  select(avg_ann_sui, order) 

# join order with rest of data to set factor levels ----
heatmap_order <- avg_annual_sui |> 
  left_join(order_2015, by = "avg_ann_sui") |> 
  mutate(subregion_name = fct_reorder(subregion_name, order))

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
##                  create dumbbell chart of avail vs consup                ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

avail_vs_consum <- ca_region |> 
  group_by(subregion_name) |>
  summarize(mean_avail = mean(availab_mm_mo, na.rm = TRUE),
            mean_consum = mean(consum_mm_mo, na.rm = TRUE)) |> 
  mutate(subregion_name = fct_reorder(.f = subregion_name, .x = mean_avail))


ggplot(avail_vs_consum) +
  # create dumbbells ----
  geom_linerange(aes(y = subregion_name,
                     xmin = mean_consum, xmax = mean_avail)) + 
  geom_point(aes(x = mean_avail, y = subregion_name), 
             color = "blue", 
             size = 2.5) +
  geom_point(aes(x = mean_consum, y = subregion_name), 
             color = "brown", 
             size = 2.5)
