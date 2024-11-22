##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                    setup                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#..........................load packages.........................
library(tidyverse)
library(showtext)

#......................import Google fonts.......................
# `name` is the name of the font as it appears in Google Fonts
# `family` is the user-specified id that you'll use to apply a font in your ggpplot
font_add_google(name = "Josefin Sans", family = "josefin")
font_add_google(name = "Sen", family = "sen")

#....................import Font Awesome fonts...................
font_add(family = "fa-brands",
         regular = here::here("fonts", "Font Awesome 6 Brands-Regular-400.otf"))
font_add(family = "fa-regular",
         regular = here::here("fonts", "Font Awesome 6 Free-Regular-400.otf"))
font_add(family = "fa-solid",
         regular = here::here("fonts", "Font Awesome 6 Free-Solid-900.otf"))

#................enable {showtext} for rendering.................
showtext_auto()
showtext_opts(dpi = 300)

#..........................import data...........................
# find import code at: https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-03-05#grab-the-clean-data-here
jobs <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/jobs_gender.csv")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                wrangle data                              ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

jobs_clean <- jobs |>

  # add col with % men in a given occupation (% females in a given occupation was already included) ----
mutate(percent_male = 100 - percent_female) |>

  # rearrange columns ----
relocate(year, major_category, minor_category, occupation,
         total_workers, workers_male, workers_female,
         percent_male, percent_female,
         total_earnings, total_earnings_male, total_earnings_female,
         wage_percent_of_male) |>

  # drop rows with missing earnings data ----
drop_na(total_earnings_male, total_earnings_female) |>

  # make occupation a factor (necessary for reordering groups in our plot) ----
mutate(occupation = as.factor(occupation)) |>

  # classify jobs by percentage male or female (these will become facet labels in our plot) ----
mutate(group_label = case_when(
  percent_female >= 75 ~ "Occupations that are 75%+ female",
  percent_female >= 45 & percent_female <= 55 ~ "Occupations that are 45-55% female",
  percent_male >= 75 ~ "Occupations that are 75%+ male"
))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                              create subset df                            ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#....guarantee the same random samples each time we run code.....
set.seed(0)

#...............get random samples from each group...............

# 10 random jobs that are 75%+ female (2016) ----
f75 <- jobs_clean |>
  filter(year == 2016, group_label == "Occupations that are 75%+ female") |>
  slice_sample(n = 10)

# 10 random jobs that are 75%+ male (2016) ----
m75 <- jobs_clean |>
  filter(year == 2016, group_label == "Occupations that are 75%+ male") |>
  slice_sample(n = 10)

# 10 random jobs that are 45-55%+ female (2016) ----
f50 <- jobs_clean |>
  filter(year == 2016, group_label == "Occupations that are 45-55% female") |>
  slice_sample(n = 10)

#.......combine dfs & relevel factors (for plotting order).......
subset_jobs <- rbind(f75, m75, f50) |>
  mutate(group_label = fct_relevel(group_label,
                                   "Occupations that are 75%+ female",
                                   "Occupations that are 45-55% female",
                                   "Occupations that are 75%+ male"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                create plot                               ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#...................create named color palette...................
earnings_pal <- c("males" = "#2D7787",
                  "females" = "#FC6B4B",
                  "dark_text" = "#0C1509",
                  "light_text" = "#4E514D")

#.........................create caption.........................
github_icon <- "&#xf09b"
github_username <- "yourGitHubUsername"

caption <- glue::glue(
  "Data Source: TidyTuesday (March 5, 2019) |
  <span style='font-family:fa-brands;'>{github_icon};</span> {github_username}"
)

#........................create subtitle.........................
money_icon <- "&#xf3d1"

subtitle <- glue::glue("<span style='font-family:fa-regular;'>{money_icon};</span>
                       Median earnings of full-time
                       <span style='color:#2D7787;'>**male**</span> versus
                       <span style='color:#FC6B4B;'>**female**</span>
                       workers by occupation in 2016")

salary_plot <- ggplot(subset_jobs) +
  geom_segment(aes(x = total_earnings_female, xend = total_earnings_male,
                   y = fct_reorder(occupation, total_earnings), yend = occupation)) +
  geom_point(aes(x = total_earnings_male, y = occupation),
             color = earnings_pal["males"], size = 3.25) +
  geom_point(aes(x = total_earnings_female, y = occupation),
             color = earnings_pal["females"], size = 3.25) +
  facet_wrap(~group_label, nrow = 3, scales = "free_y") +
  scale_x_continuous(labels = scales::label_dollar(scale = 0.001, suffix = "k"),
                     breaks = c(25000, 50000, 75000, 100000, 125000)) +
  labs(title = "Males earn more than females across most occupations",
       subtitle = subtitle,
       caption = caption) +
  theme_minimal(base_family = "sen") +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(family = "josefin",
                              face = "bold",
                              size = 25,
                              color = earnings_pal["dark_text"]),
    plot.subtitle = ggtext::element_textbox(
      family = "sen",
      size = 16,
      #fill = "lightgray",
      color = earnings_pal["light_text"],
      margin = margin(t = 1, r = 0, b = 1, l = 0),
      padding = margin(10, 10, 10, 10), # Add more padding for spacing around text
      width = unit(25, "cm"),           # Set a specific width to control text wrapping
      lineheight = 1.5,                 # Increase line height for better readability
      halign = 0                        # Adjust horizontal alignment as needed (0 = left, 0.5 = center, 1 = right)
    ),
    plot.caption = ggtext::element_textbox(
      family = "sen",
      face = "italic",
      color = earnings_pal["light_text"],
      margin = margin(t = 15, r = 0, b = 0, l = 0),
      #padding = margin(t = 0, r = 10, b = 0, l = 10),
      halign = 1,
      size = 12,
      #fill = "lightgray",
      lineheight = 1.5,         # Increase line height
      width = unit(20, "cm")     # Set a fixed width for the text box
    ),
    strip.text.x = element_text(family = "josefin",
                                face = "bold",
                                size = 14,
                                hjust = 0),
    panel.spacing.y = unit(x = 0.6, "cm"),
    axis.text = element_text(color = earnings_pal["light_text"]),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 10),
    axis.title = element_blank()
  )

salary_plot

# Save the plot
ggsave(
  filename = here::here("course-materials", "discussion", "week6", "images", "salary-plot.png"),
  plot = salary_plot,
  width = 13,    # Width in inches
  height = 8,   # Height in inches
  dpi = 300      # Match DPI in showtext_opts()
)





salary_plot <- ggplot(subset_jobs) +
  geom_segment(aes(x = total_earnings_female, xend = total_earnings_male,
                   y = fct_reorder(occupation, total_earnings), yend = occupation)) +
  geom_point(aes(x = total_earnings_male, y = occupation),
             color = earnings_pal["males"], size = 3.25) +
  geom_point(aes(x = total_earnings_female, y = occupation),
             color = earnings_pal["females"], size = 3.25) +
  facet_wrap(~group_label, nrow = 3, scales = "free_y") +
  scale_x_continuous(labels = scales::label_dollar(scale = 0.001, suffix = "k"),
                     breaks = c(25000, 50000, 75000, 100000, 125000)) +
  labs(title = "Males earn more than females across most occupations",
       subtitle = subtitle,
       caption = caption) +
  theme_minimal(base_family = "sen") +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(family = "josefin",
                              face = "bold",
                              size = 25,
                              color = earnings_pal["dark_text"]),
    plot.subtitle = ggtext::element_textbox(
      family = "sen",
      size = 16,
      #fill = "lightgray",
      color = earnings_pal["light_text"],
      margin = margin(t = 1, r = 0, b = 1, l = 0),
      padding = margin(10, 10, 10, 10), # Add more padding for spacing around text
      width = unit(25, "cm"),           # Set a specific width to control text wrapping
      lineheight = 1.5,                 # Increase line height for better readability
      halign = 0                        # Adjust horizontal alignment as needed (0 = left, 0.5 = center, 1 = right)
    ),
    plot.caption = ggtext::element_textbox(
      family = "sen",
      face = "italic",
      color = earnings_pal["light_text"],
      margin = margin(t = 15, r = 0, b = 0, l = 0),
      padding = margin(t = 15, r = 10, b = 15, l = 10),
      halign = 1,
      size = 12,
      #fill = "lightgray",
      lineheight = 1.5,         # Increase line height
      width = unit(10, "cm")     # Set a fixed width for the text box
    ),
    strip.text.x = element_text(family = "josefin",
                                face = "bold",
                                size = 14,
                                hjust = 0),
    panel.spacing.y = unit(x = 0.6, "cm"),
    axis.text = element_text(color = earnings_pal["light_text"]),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 10),
    axis.title = element_blank()
  )

salary_plot











