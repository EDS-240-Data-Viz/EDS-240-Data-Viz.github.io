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

