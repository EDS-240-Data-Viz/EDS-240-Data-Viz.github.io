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
iwa_data <- read_csv(here::here("course-materials", "data", "lecture", "combined_iwa-assessment-outputs-conus-2025_CONUS_200910-202009_long.csv"))

# downloaded from https://resilience.climate.gov/datasets/esri::watershed-boundary-dataset-huc-12s/about ----
# huc12 <- read_csv(here::here("course-materials", "data", "lecture", "Watershed_Boundary_Dataset_HUC_12s_7445451579594868972.csv"))
# create df of subregion names to append to usgs data; extracted from https://water.usgs.gov/GIS/wbd_huc8.pdf ----
# subregion_names <- tribble(
#   ~subregion_HUC, ~subregion_name,
#   1801L, "Klamath-Northern California Coastal",
#   1802L, "Sacramento",
#   1803L, "Tulare-Buena Vista Lakes",
#   1804L, "San Joaquin",
#   1805L, "San Francisco Bay",
#   1806L, "Central California Coastal",
#   1807L, "Southern California Coastal",
#   1808L, "North Lahontan", 
#   1809L, "Northern Mojave-Mono Lake",
#   1810L, "Southern Mojave-Salton Sea"
# ) |> 
#   mutate(subregion_HUC = as.factor(subregion_HUC))

HUC_names <- tribble(
  ~HUC, ~type, ~name,

  # all ten CA subregions ----
  "1801", "subregion", "Klamath-Northern California Coastal",
  "1802", "subregion", "Sacramento",
  "1803", "subregion", "Tulare-Buena Vista Lakes",
  "1804", "subregion", "San Joaquin",
  "1805", "subregion", "San Francisco Bay",
  "1806", "subregion", "Central California Coastal",
  "1807", "subregion", "Southern California Coastal",
  "1808", "subregion", "North Lahontan", 
  "1809", "subregion", "Northern Mojave-Mono Lake",
  "1810", "subregion", "Southern Mojave-Salton Sea",

  # the twelve Central California Coastal subbasins ----
  "18060002", "subbasin", "Pajaro",
  "18060003", "subbasin", "Carrizo Plain",
  "18060004", "subbasin", "Estrella",
  "18060005", "subbasin", "Salinas",
  "18060006", "subbasin", "Central Coastal",
  "18060007", "subbasin", "Cuyama",
  "18060008", "subbasin", "Santa Maria",
  "18060009", "subbasin", "San Antonio", 
  "18060010", "subbasin", "Santa Ynez",
  "18060013", "subbasin", "Santa Barbara Coastal",
  "18060014", "subbasin", "Santa Barbara Channel Islands",
  "18060015", "subbasin", "Monterey Bay"
)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                    wrangle data and filter for just CA                   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# # get CA region HUCs ----
# ca_HUCs <- huc12 |> 
#   clean_names() |> 
#   mutate(
#     region_HUC = str_sub(string = huc12, start = 1, end = 2),
#     subregion_HUC = str_sub(string = huc12, start = 1, end = 4),
#     subbasin_HUC = str_sub(string = huc12, start = 1, end = 8)
#   ) |>
#   filter(region_HUC == 18)


# filter for just CA water resource region (HUC 18) ---- 
ca_region <- iwa_data |> 
  clean_names() |> 
  mutate(
    region_HUC = str_sub(string = huc12_id, start = 1, end = 2),
  ) |> 
  filter(region_HUC == "18") |> 
  separate_wider_delim(cols = year_month,
                       delim = "-",
                       names = c("year", "month")) |> 
  mutate(year = as.numeric(year),
         month = as.numeric(month)) |> 
  select(year, month, huc12_id, availab_mm_mo, consum_mm_mo, sui_frac)
  # left_join(ca_HUCs) |> 
  # rename(subregion_name = name) |> 
  # select(-type) |> 
  # left_join(HUC_names |> filter(type == "subbasin"), by = c("subbasin_HUC" = "HUC")) |> 
  # rename(subbasin_name = name) |> 
  # select(-type) |> 
  #mutate(subregion_HUC = as.factor(subregion_HUC)) |> 
  #left_join(subregion_names) |> 
  

# create df of subregions ----
ca_subregions <- ca_region |> 
   mutate(subregion_HUC = str_sub(string = huc12_id, start = 1, end = 4)) |> 
  left_join(HUC_names |> filter(type == "subregion"), by = c("subregion_HUC" = "HUC"))

# create df of subbasins --
ca_subbasins <- ca_subregions |> 
  filter(subregion_HUC == 1806) |> 
  mutate(subbasin_HUC = str_sub(string = huc12_id, start = 1, end = 8)) |> 
  left_join(HUC_names |> filter(type == "subbasin"), by = c("subbasin_HUC" = "HUC")) |> 
  drop_na(name)


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
avg_monthly_sui <- ca_subregions |> 
  select(year, month, subregion_HUC, name, sui_frac) |> 
  group_by(name) |> 
  summarise(
    avg_monthly_sui = mean(sui_frac, na.rm = TRUE)
  )
  
#.....................create basic bar chart.....................
avg_monthly_sui |> 
  mutate(name = fct_reorder(.f = name, .x = avg_monthly_sui)) |>
  ggplot(aes(x = avg_monthly_sui, y = name)) +
  geom_col() + 
  geom_text(aes(label = round(avg_monthly_sui, 2)), hjust = 1.2, color = "white") 


# ca_region |> 
#   group_by(name) |> 
#   summarise(avg_sui = mean(sui_frac, na.rm = TRUE)) |> 
#   ggplot(aes(x = name, y = avg_sui)) +
#   geom_col() 

#......................create basic lollipop.....................
avg_monthly_sui |> 
  mutate(name = fct_reorder(.f = name, .x = avg_monthly_sui)) |>
  ggplot(aes(x = avg_monthly_sui, y = name)) + 
  geom_point() +
  geom_linerange(aes(xmin = 0, xmax = avg_monthly_sui)) +
  geom_text(aes(label = round(avg_monthly_sui, 2)), hjust = -0.3) + 
  scale_x_continuous(limits = c(0, 0.6))  # expand axis to make room for values


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                    example of geom_bar() vs geom_col()                   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# tbd


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                    create heatmap of avg monthly SUI for each year       ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#..........df of average monthly SUI by subregion & year..........
df <- ca_subregions |> 
  group_by(name, year) |> 
  summarize(monthly_avg_sui = mean(sui_frac, na.rm = TRUE)) |> 
  ungroup()

# avg_annual_sui <- ca_region |> 
  # select(year, month, subregion_HUC, subregion_name, sui_frac) |> 
  # group_by(subregion_name, year) |> 
  # summarise(
  #   avg_ann_sui = mean(sui_frac, na.rm = TRUE)
  # ) |> 
  # ungroup()

 #.determine order of subregions based on highest avg SUI in 2015.
order_2015 <- df |> 
  filter(year == 2015) |> 
  arrange(monthly_avg_sui) |> 
  mutate(order = row_number()) |> 
  select(monthly_avg_sui, order) 

#........join order with rest of data to set factor levels.......
heatmap_order <- df |> 
  left_join(order_2015, by = "monthly_avg_sui") |> 
  mutate(name = fct_reorder(name, order))

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
##                  create dumbbell chart of avail vs consup                ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

subbasins_1806 <- ca_region |> 
  mutate(
    subbasin_HUC = str_sub(string = huc12_id, start = 1, end = 8)
  ) |> 
  filter(subregion_HUC == "1806") |>
  left_join(HUC_names |> filter(type == "subbasin"), by = c("subbasin_HUC" = "HUC")) |> 
  rename(subbasin_name = name) |> 
  select(year, month, huc12_id, subbasin_HUC, subbasin_name, availab_mm_mo, consum_mm_mo, strflow_mm_mo, sui_frac) |> 
  group_by(subbasin_name) |> 
  # summarize(tot_avail = sum(availab_mm_mo), na.rm = TRUE,
  #           tot_consum = sum(consum_mm_mo, na.rm = TRUE)) |> 
  # ungroup() |> 
  # group_by(subbasin_name) |> 
  summarize(mean_avail = mean(availab_mm_mo),
            mean_consum = mean(consum_mm_mo)) |> 
  mutate(subbasin_name = fct_reorder(.f = subbasin_name, .x = mean_avail)) 

# water_use_1806 <- ca_region |> 
#   group_by(subregion_name, year) |> 
#   summarize(tot_avail = sum(availab_mm_mo, na.rm = TRUE),
#             tot_consum = sum(consum_mm_mo, na.rm = TRUE)) |> 
#   ungroup() |> 
#   group_by(subregion_name) |> 
#   summarize(mean_avail = mean(tot_avail),
#             mean_consum = mean(tot_consum)) |> 
#   mutate(subregion_name = fct_reorder(.f = subregion_name, .x = mean_avail)) 

ggplot(subbasins_1806) +
  geom_linerange(aes(y = subbasin_name,
                     xmin = mean_consum, xmax = mean_avail)) + 
  geom_point(aes(x = mean_avail, y = subbasin_name), 
             color = "blue", 
             size = 2.5) +
  geom_point(aes(x = mean_consum, y = subbasin_name), 
             color = "brown", 
             size = 2.5) +
  scale_x_continuous(labels = scales::label_comma(scale = 1)) +
  labs(x = "millimeters of water per year") +
  theme(
    axis.title.y = element_blank()
  )


#  #### ignore for now ####
# sum_avail_v_consum <- ca_region |> 
#   group_by(subregion_name, year) |>
#   summarize(sum_avail = sum(availab_mm_mo, na.rm = TRUE),
#             sum_consum = sum(consum_mm_mo, na.rm = TRUE)) |> 
#   #mutate(subregion_name = fct_reorder(.f = subregion_name, .x = mean_avail)) |> 
#   ungroup()

# #### ignore for now ####
# mean_avail_v_consum <- sum_avail_v_consum |> 
#   group_by(subregion_name) |>
#   summarize(mean_avail = mean(sum_avail, na.rm = TRUE),
#             mean_consum = mean(sum_consum, na.rm = TRUE)) |> 
#   mutate(subregion_name = fct_reorder(.f = subregion_name, .x = mean_avail))


# avail_vs_consum <- ca_region |> 
#   group_by(subregion_name) |>
#   summarize(mean_avail = mean(availab_mm_mo, na.rm = TRUE),
#             mean_consum = mean(consum_mm_mo, na.rm = TRUE)) |> 
#   mutate(subregion_name = fct_reorder(.f = subregion_name, .x = mean_avail))

# -----

# subbasins <- tribble(
#   ~subbasin_HUC, ~subbasin_name,
#   18060002L, "Pajaro",
#   18060003L, "Carrizo Plain",
#   18060004L, "Estrella",
#   18060005L, "Salinas",
#   18060006L, "Central Coastal",
#   18060007L, "Cuyama",
#   18060008L, "Santa Maria",
#   18060009L, "San Antonio", 
#   18060010L, "Santa Ynez",
#   18060013L, "Santa Barbara Coastal",
#   18060014L, "Santa Barbara Channel Islands",
#   18060015L, "Monterey Bay"
# ) |> 
#   mutate(subbasin_HUC = as.factor(subbasin_HUC))




