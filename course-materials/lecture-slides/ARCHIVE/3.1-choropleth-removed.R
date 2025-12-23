

# ```{r}
#| eval: false
#| echo: false
#| message: false
#| warning: false
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ##                              important links                             ----
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# # State-level precipitation by year/month: <https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/statewide/mapping>
# # County-level precipitation by year/month: <https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/county/mapping/110/pcp/202310/1/value> and remove first few rows manually
# # Tutorial: <https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html>
#
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ##                                    setup                                 ----
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# #..........................load packages.........................
# library(tidyverse)
# library(maps)
#
# #.........................get shape data.........................
# states <- map_data("state")
# counties <- map_data("county")
#
# #....................import precipitation data...................
# precip_counties <- read_csv(here::here("course-materials", "lecture-slides", "data", "county-precip-oct2023.csv"), skip = 4) |>
#   janitor::clean_names()
#
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ##                               data wrangling                             ----
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# #..............update names of counties df variables.............
# us_counties <- counties |>
#
#   # select / rename cols of interest ----
#   select(state = region, county = subregion, long, lat, group, order) |>
#
#   # make all county & state names Title Case ----
#   mutate(county = str_to_title(county),
#          state = str_to_title(state)) |>
#
#   # fix different spellings so that they match county names in precip data ----
#   mutate(county = case_when(
#     county == "De Kalb" ~ "DeKalb",
#     county == "De Soto" ~ "DeSoto", # works for FL & MI
#     county == "Du Page" ~ "DuPage",
#     county == "La Porte" ~ "LaPorte",
#     county == "Obrien" ~ "O'Brien",
#     county == "Prince Georges" ~ "Prince George's",
#     county == "Queen Annes" ~ "Queen Anne's",
#     county == "Ste Genevieve" ~ "Ste. Genevieve",
#     county == "La Moure" ~ "LaMoure",
#     state == "Texas" & county == "De Witt" ~ "DeWitt",
#     # county == "St. Marys" ~ "St. Mary's", # isn't working??
#     TRUE ~ county
#     )) |>
#   mutate(county = str_replace(string = county, pattern = "St ", replacement = "St. ")) |>
#
#   # remove DC & Yellowstone National Park ----
#   filter(!state %in% c("District Of Columbia")) |>
#   filter(!county %in% c("Yellowstone National"))
#
#
# #..........clean precip data & make sure var names match.........
# precip_counties_wrangled <- precip_counties |>
#
#   # more intuitive col name ----
#   rename(county = name) |>
#
#   # make all county & state names lower case ----
#   mutate(county = str_to_lower(county),
#          state = str_to_lower(state)) |>
#
#   # remove recurring patterns ----
#   mutate(county = str_remove(county, pattern = " county")) |> # across all county names
#   mutate(county = str_remove(county, pattern = " parish")) |> # LA
#
#   # make state & county names Title Case again ----
#   mutate(county = str_to_title(county),
#          state = str_to_title(state)) |>
#
#   # fix different spellings so that they match county names in precip data ----
#   mutate(county = case_when(
#     county == "Dekalb" ~ "DeKalb",
#     county == "Desoto" ~ "DeSoto", # works for FL & MI
#     state == "Louisiana" & county == "De Soto" ~ "DeSoto",
#     county == "Dupage" ~ "DuPage",
#     county == "Lasalle" ~ "La Salle",
#     county == "Laporte" ~ "LaPorte",
#     county == "O'brien" ~ "O'Brien",
#     county == "Lamoure" ~ "LaMoure",
#     state == "Texas" & county == "Dewitt" ~ "DeWitt",
#     county == "St. Mary's" ~ "St. Marys",
#     county == "Suffolk City" ~ "Suffolk",
#     county == "Hampton City" ~ "Hampton",
#     county == "Virginia Beach City" ~ "Virginia Beach",
#     county == "Newport News City" ~ "Newport News",
#     county == "Norfolk City" ~ "Norfolk",
#     TRUE ~ county
#   )) |>
#
#   # remove DC & other non-counties
#   filter(!county %in% c("Washington, D.c.", "Alexandria City", "Bristol City")) |>
#
#   # select & rename cols ----
#   select(state, county, precip = value, mean_1901_2000 = x1901_2000_mean, anomaly = anomaly_1901_2000_base_period) |>
#
#   # coerce precip from chr to numeric ----
#   mutate(precip = as.numeric(precip))
#
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ##  ~ used for identifying which county names to edit  ----
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# # unique_us_counties <- us_counties |>
# #   distinct(state, county)
# #
# # unique_precip_counties <- precip_counties_wrangled |>
# #   distinct(state, county)
# #
# # missing_from_us_counties <- anti_join(unique_precip_counties, unique_us_counties) |>
# #   rename(county_in_us = county)
# # missing_from_precip_counties <- anti_join(unique_us_counties, unique_precip_counties) |>
# #   rename(county_in_precip = county)
#
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ##  ~ -----------------------------------------------  ----
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# #............................join dfs............................
# joined_precip_us_counties <- full_join(precip_counties_wrangled, us_counties)
#```


#```{r}
#| eval: true
#| echo: false
# ## ----------------- Create base map -----------------
#
# # 'group' controls whether adjacent points are connected by lines (each county is a "group," therefore points are connected) ----
# base_map <- ggplot(data = states, mapping = aes(x = long, y = lat, group = group)) +
#
#   # plot precip values by county; geom_polygon() drawn lines between points and “closes them up” (i.e. draws a line from the last point back to the first point) ----
#   geom_polygon(data = joined_precip_us_counties, aes(fill = precip)) +
#
#   # darken state lines ----
#   geom_polygon(color = "#2F2D2C", fill = NA, linewidth = 0.1) +
#
#   # to fix the relationship between one unit in the y direction and one unit in the x direction; may need different values for different regions depending on where they are on the globe (e.g. close to the poles)
#   #coord_fixed(1.3) +
#
#   # update labels ----
#   labs(title = "Total Precipitation, by County",
#        subtitle = "October 2023") +
#
#   # set theme to clean up appearance ----
#   theme_void() +
#
#   # theme adjustments ----
#   theme(
#     legend.position = "bottom"
#   )

#```


#```{r}
#| eval: true
#| echo: false
# ## ----------------- Add projection -----------------
# base_map_proj <- base_map +
#   coord_map(projection = "mercator")
#```

#```{r}
#| eval: true
#| echo: false
# ## ----------------- Create custom guide mod fxn -----------------
#
# # apply to following maps to customize legend appearance ----
# custom_guide <- function(type) {
#
#   # if gradient, use guide_color()
#   if (type == "g") {
#     guides(fill = guide_colorbar(title = "Precipitation (inches)",
#                                  title.position = "top",
#                                  barwidth = 15, barheight = 1))
#
#   # if bin, use guide_colorsteps()
#   } else if (type == "b") {
#     guides(fill = guide_colorsteps(title = "Precipitation (inches)",
#                                    title.position = "top",
#                                    barwidth = 15, barheight = 1))
#   }
# }
#```

#```{r}
#| eval: true
#| echo: false
#| fig-align: "center"
#| out-width: "100%"
# ## ----------------- Create plot -----------------
#
# # define palette ----
# my_palette <- c("#543006", "#975F1C", "#AC7E42", "#C09C66", "#D6BB8C", "#EBD9B0","#D5D4CE",
#                  "#B2DDD7", "#8BC2BC", "#64A7A1", "#3B8E86", "#16726B", "#003C30")
#
# # plot (gradient) ----
# base_map_proj +
#   scale_fill_gradientn(colors = my_palette) +
#   custom_guide(type = "g")
#```















```{r}
#| eval: true
#| echo: false
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ##                              important links                             ----
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# # State-level precipitation by year/month: <https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/statewide/mapping>
# # County-level precipitation by year/month: <https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/county/mapping/110/pcp/202310/1/value> and remove first few rows manually
# # Tutorial: <https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html>
#
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ##                                    setup                                 ----
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# #..........................load packages.........................
# library(tidyverse)
# library(maps)
#
# #.........................get shape data.........................
# # usa <- map_data("usa")
# states <- map_data("state")
# counties <- map_data("county")
#
# #....................import precipitation data...................
# # NOTE: manually removed the first 4 rows of metadata from csv file before importing
# precip_counties <- read_csv(here::here("slides", "data", "county-precip-jan2023.csv"), skip = 4) |>
#   janitor::clean_names()
#
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ##                               data wrangling                             ----
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# #..............update names of counties df variables.............
# us_counties <- counties |>
#   select(state = region, county = subregion, long, lat, group, order)
#
# #..........clean precip data & make sure var names match.........
# precip_counties_wrangled <- precip_counties |>
#
#   # more intuitive column name ----
#   rename(county = name) |>
#
#   # convert count and state names to all lowercase ----
#   mutate(county = str_to_lower(county),
#          state = str_to_lower(state)) |>
#
#   # remove the word "county" from all county names ----
#   mutate(county = str_remove(county, pattern = " county")) |>
#
#   # remove the word "parish" from the end of Louisiana counties ----
#   # NOTE: there are more counties that need cleaning for seamless joining with `us_counties` df; I've don't done a complete sweep of the data, so you'll notice data gaps once plotted
#   mutate(county = str_remove(county, pattern = " parish")) |>
#
#   # add col denoting if Oct precip is above or below 1901 - 2000 mean
#   mutate(comp_mean = ifelse(value > x1901_2000_mean, # if `value` > `x1901_2000_mean`
#                      yes = "above mean", # replace abbreviation with full state name
#                      no = "below mean")) |> # if string in 'sn' col is not 2 chrs long, keep state name as-is
#
#   # select & rename relevant cols ----
#   select(state, county, precip = value, mean_1901_2000 = x1901_2000_mean, anomaly = anomaly_1901_2000_base_period, comp_mean) |>
#
#   # make precip var numeric (was chr) ----
#   mutate(precip = as.numeric(precip))
#
# #............................join dfs............................
# joined_precip_us_counties <- full_join(precip_counties_wrangled, us_counties)
```

```{r}
#| eval: true
#| echo: false
#| fig-align: "center"
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ##                                 create map                               ----
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# # 'group' controls whether adjacent points are connected by lines (each county is a "group," therefore points are connected) ----
# ggplot(data = states, mapping = aes(x = long, y = lat, group = group)) +
#
#   # plot precip values by county; geom_polygon() drawn lines between points and “closes them up” (i.e. draws a line from the last point back to the first point) ----
#   geom_polygon(data = joined_precip_us_counties, aes(fill = precip)) +
#
#   # darken state lines ----
#   geom_polygon(color = "#2F2D2C", fill = NA, linewidth = 0.1) +
#
#     # to fix the relationship between one unit in the y direction and one unit in the x direction; may need different values for different regions depending on where they are on the globe (e.g. close to the poles)
#   coord_fixed(1.3) +
#
#   # # fill with pre-made palette & set legend breaks ----
#   #scale_fill_viridis_c() + # breaks = c(0, 2, 4, 6, 8, 10, 12)
#
#   # OR fill manually & set legend breaks ----
#   scale_fill_gradientn(colours = rev(c("#B10025", "#FF8A37", "#FBFE82", "#00712F", "#9CDC94", "#C2EDBA", "#FFFFFF")),
#                        breaks = c(0, 2, 4, 6, 8, 10, 12)) +
#
#   # update labels ----
#   labs(title = "Total Precipitation, by County",
#        subtitle = "January 2023",
#        fill = "Precip\n(inches)") +
#
#   # set theme to clean up appearance ----
#   theme_void()
#```





#```{r}
#| eval: true
#| echo: false
#| message: false
#| warning: false
#| fig-align: "center"
#| layout-ncol: 2
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ##                              important links                             ----
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# # State-level precipitation by year/month: <https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/statewide/mapping>
# # County-level precipitation by year/month: <https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/county/mapping/110/pcp/202310/1/value>
# # Tutorial: <https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html>
#
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ##                                    setup                                 ----
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# #..........................load packages.........................
# library(tidyverse)
# library(maps)
#
# #.........................get shape data.........................
# states <- map_data("state")
# counties <- map_data("county")
#
# #....................import precipitation data...................
# precip_counties <- read_csv(here::here("course-materials", "lecture-slides", "data", "county-precip-oct2023.csv"), skip = 4) |>
#   janitor::clean_names()
#
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ##                               data wrangling                             ----
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# #..............update names of counties df variables.............
# us_counties <- counties |>
#
#   # select / rename cols of interest ----
#   select(state = region, county = subregion, long, lat, group, order) |>
#
#   # make all county & state names Title Case ----
#   mutate(county = str_to_title(county),
#          state = str_to_title(state)) |>
#
#   # fix different spellings so that they match county names in precip data ----
#   mutate(county = case_when(
#     county == "De Kalb" ~ "DeKalb",
#     county == "De Soto" ~ "DeSoto", # works for FL & MI
#     county == "Du Page" ~ "DuPage",
#     county == "La Porte" ~ "LaPorte",
#     county == "Obrien" ~ "O'Brien",
#     county == "Prince Georges" ~ "Prince George's",
#     county == "Queen Annes" ~ "Queen Anne's",
#     county == "Ste Genevieve" ~ "Ste. Genevieve",
#     county == "La Moure" ~ "LaMoure",
#     state == "Texas" & county == "De Witt" ~ "DeWitt",
#     # county == "St. Marys" ~ "St. Mary's", # isn't working??
#     TRUE ~ county
#     )) |>
#   mutate(county = str_replace(string = county, pattern = "St ", replacement = "St. ")) |>
#
#   # remove DC & Yellowstone National Park ----
#   filter(!state %in% c("District Of Columbia")) |>
#   filter(!county %in% c("Yellowstone National"))
#
#
# #..........clean precip data & make sure var names match.........
# precip_counties_wrangled <- precip_counties |>
#
#   # more intuitive col name ----
#   rename(county = name) |>
#
#   # make all county & state names lower case ----
#   mutate(county = str_to_lower(county),
#          state = str_to_lower(state)) |>
#
#   # remove recurring patterns ----
#   mutate(county = str_remove(county, pattern = " county")) |> # across all county names
#   mutate(county = str_remove(county, pattern = " parish")) |> # LA
#
#   # make state & county names Title Case again ----
#   mutate(county = str_to_title(county),
#          state = str_to_title(state)) |>
#
#   # fix different spellings so that they match county names in precip data ----
#   mutate(county = case_when(
#     county == "Dekalb" ~ "DeKalb",
#     county == "Desoto" ~ "DeSoto", # works for FL & MI
#     state == "Louisiana" & county == "De Soto" ~ "DeSoto",
#     county == "Dupage" ~ "DuPage",
#     county == "Lasalle" ~ "La Salle",
#     county == "Laporte" ~ "LaPorte",
#     county == "O'brien" ~ "O'Brien",
#     county == "Lamoure" ~ "LaMoure",
#     state == "Texas" & county == "Dewitt" ~ "DeWitt",
#     county == "St. Mary's" ~ "St. Marys",
#     county == "Suffolk City" ~ "Suffolk",
#     county == "Hampton City" ~ "Hampton",
#     county == "Virginia Beach City" ~ "Virginia Beach",
#     county == "Newport News City" ~ "Newport News",
#     county == "Norfolk City" ~ "Norfolk",
#     TRUE ~ county
#   )) |>
#
#   # remove DC & other non-counties
#   filter(!county %in% c("Washington, D.c.", "Alexandria City", "Bristol City")) |>
#
#   # select & rename cols ----
#   select(state, county, precip = value, mean_1901_2000 = x1901_2000_mean, anomaly = anomaly_1901_2000_base_period) |>
#
#   # coerce precip from chr to numeric ----
#   mutate(precip = as.numeric(precip))
#
# #............................join dfs............................
# joined_precip_us_counties <- full_join(precip_counties_wrangled, us_counties)
#
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ##                                 create map                               ----
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# # define palette ----
# my_palette <- c("#543006", "#975F1C", "#AC7E42", "#C09C66", "#D6BB8C", "#EBD9B0","#D5D4CE",
#                  "#B2DDD7", "#8BC2BC", "#64A7A1", "#3B8E86", "#16726B", "#003C30")
#
# # create map -----
# ggplot(data = states, mapping = aes(x = long, y = lat, group = group)) +
#   geom_polygon(data = joined_precip_us_counties, aes(fill = precip)) +
#   coord_map(projection = "mercator") +
#   geom_polygon(color = "#2F2D2C", fill = NA, linewidth = 0.1) +
#   scale_fill_gradientn(colors = my_palette) +
#   guides(fill = guide_colorbar(title = "Precipitation (inches)",
#                                  title.position = "top",
#                                  barwidth = 15, barheight = 1)) +
#   labs(title = "Total Precipitation, by County",
#        subtitle = "October 2023") +
#   theme_void() +
#   theme(
#     legend.position = "bottom"
#   )
#
# # ggplot(data = states, mapping = aes(x = long, y = lat, group = group)) +
# #   coord_fixed(1.3) +
# #   geom_polygon(data = joined_precip_us_counties, aes(fill = precip)) + #, color = "gray"
# #   geom_polygon(color = "#2F2D2C", fill = NA, linewidth = 0.1) +
# #   scale_fill_gradientn(colours = rev(c("#B10025", "#FF8A37", "#FBFE82", "#00712F", "#9CDC94", "#C2EDBA", "#FFFFFF")),
# #                        breaks = c(0, 2, 4, 6, 8, 10, 12)) +
# #   labs(title = "Total Precipitation, by County",
# #        subtitle = "October 2023",
# #        fill = "Precip\n(inches)") +
# #   theme_void()
#
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ##                              create bar chart                            ----
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# #...............precipitation data for CA counties...............
# ca_precip <- precip_counties_wrangled |>
#   filter(state == "California") |>
#   mutate(county = str_to_title(county))
#
# #.........................create barplot.........................
#
# ca_precip |>
#   slice_max(n = 20, order_by = precip) |>
#   ggplot(aes(x = reorder(county, precip), y = precip)) +
#   geom_bar(stat = "identity", width = 0.9, fill = "#635F5F") +
#   coord_flip() +
#   labs(x = "", y = "Total Precipitation (inches)",
#        title = "Top 20 California counties to recieve the most rainfall in October 2023") +
#   theme_classic()
#```





