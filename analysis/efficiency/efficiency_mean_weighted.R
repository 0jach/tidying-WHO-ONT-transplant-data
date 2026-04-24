library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(readr)
library(scales)

data_path <- "."

donors_tidy_total <- read_csv(
  file.path(data_path, "donors_tidy_total.csv"),
  show_col_types = FALSE
)

yearly <- donors_tidy_total %>%
  pivot_wider(names_from = donor_status, values_from = amount) %>%
  rename(actual = Actual, utilized = Utilized) %>%
  mutate(
    efficiency   = ifelse(!is.na(actual) & !is.na(utilized) & actual > 0,
                          utilized / actual, NA_real_),
    actual_pmp   = ifelse(!is.na(actual) & POPULATION > 0,
                          actual / POPULATION, NA_real_),
    utilized_pmp = ifelse(!is.na(utilized) & POPULATION > 0,
                          utilized / POPULATION, NA_real_)
  )

country_summary <- yearly %>%
  filter(!is.na(actual), !is.na(utilized), actual > 0) %>%
  group_by(REGION, COUNTRY) %>%
  summarise(
    years            = n(),
    sum_actual       = sum(actual),
    sum_utilized     = sum(utilized),
    weighted_eff     = sum(utilized) / sum(actual),
    mean_actual_pmp  = mean(actual_pmp, na.rm = TRUE),
    mean_utilized_pmp = mean(utilized_pmp, na.rm = TRUE),
    mean_pop         = mean(POPULATION, na.rm = TRUE),
    .groups = "drop"
  )

make_scatter <- function(data, title_text, subtitle_text) {
  ggplot(data, aes(
    x     = mean_actual_pmp,
    y     = weighted_eff,
    color = group,
    size  = mean_pop,
    label = COUNTRY
  )) +
    geom_point(alpha = 0.7) +
    geom_text_repel(
      size         = 3,
      max.overlaps = 30,
      show.legend  = FALSE,
      fontface     = "bold"
    ) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      limits = c(NA, 1.02)
    ) +
    scale_size_continuous(range = c(2, 14), guide = "none") +
    scale_color_manual(values = c(
      "Top"    = "#2c7bb6",
      "Bottom" = "#d7191c"
    )) +
    labs(
      title    = title_text,
      subtitle = subtitle_text,
      x        = "Mean Actual Donors per Million Population",
      y        = "Weighted Efficiency (Utilized / Actual)",
      color    = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "top",
      plot.title      = element_text(face = "bold")
    )
}


# Top 25 and Bottom 25 by volume

top25_vol <- country_summary %>% slice_max(sum_actual, n = 25) %>% pull(COUNTRY)
bot25_vol <- country_summary %>% slice_min(sum_actual, n = 25) %>% pull(COUNTRY)

data_25vol <- country_summary %>%
  filter(COUNTRY %in% c(top25_vol, bot25_vol)) %>%
  mutate(group = ifelse(COUNTRY %in% top25_vol, "Top", "Bottom"))

p1 <- make_scatter(
  data_25vol,
  "Efficiency vs Donation Rate (pmp) — by Volume",
  "Top 25 and Bottom 25 by total actual donor volume. Point size = population."
)
print(p1)


# Top 25 and Bottom 25 by PMP

top25_pmp <- country_summary %>% slice_max(mean_actual_pmp, n = 25) %>% pull(COUNTRY)
bot25_pmp <- country_summary %>% slice_min(mean_actual_pmp, n = 25) %>% pull(COUNTRY)

data_25pmp <- country_summary %>%
  filter(COUNTRY %in% c(top25_pmp, bot25_pmp)) %>%
  mutate(group = ifelse(COUNTRY %in% top25_pmp, "Top", "Bottom"))

p2 <- make_scatter(
  data_25pmp,
  "Efficiency vs Donation Rate (pmp) — by PMP",
  "Top 25 and Bottom 25 by mean actual donors per million population."
)
print(p2)


# All countries — colored by region, labels on outliers only

all_countries <- country_summary %>%
  mutate(
    rank_eff_top = rank(-weighted_eff),
    rank_eff_bot = rank(weighted_eff),
    show_label   = rank_eff_top <= 5 | rank_eff_bot <= 5 | COUNTRY == "Italy"
  )

p3 <- ggplot(all_countries, aes(
  x     = mean_actual_pmp,
  y     = weighted_eff,
  color = REGION,
  size  = mean_pop
)) +
  geom_point(alpha = 0.6) +
  geom_text_repel(
    data        = all_countries %>% filter(show_label),
    aes(label = COUNTRY),
    size        = 3,
    max.overlaps = 20,
    show.legend = FALSE,
    fontface    = "bold"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(NA, 1.02)) +
  scale_size_continuous(range = c(2, 14), guide = "none") +
  labs(
    title    = "Efficiency vs Donation Rate (pmp) — All Countries",
    subtitle = "Labels on top/bottom 5 by efficiency + Italy. Point size = population.",
    x        = "Mean Actual Donors per Million Population",
    y        = "Weighted Efficiency (Utilized / Actual)",
    color    = "Region"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title      = element_text(face = "bold")
  )
print(p3)

p4 <- ggplot(all_countries, aes(
  x     = mean_actual_pmp,
  y     = weighted_eff,
  color = REGION,
  size  = mean_pop
)) +
  geom_point(alpha = 0.6) +
  geom_text_repel(
    data        = all_countries %>% filter(show_label),
    aes(label = COUNTRY),
    size        = 3,
    max.overlaps = 20,
    show.legend = FALSE,
    fontface    = "bold"
  ) +
  scale_x_log10() +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(NA, 1.02)) +
  scale_size_continuous(range = c(2, 14), guide = "none") +
  labs(
    title    = "Efficiency vs Donation Rate (pmp, log scale) — All Countries",
    subtitle = "Log scale on X spreads out low-pmp countries. Labels on top/bottom 5 + Italy.",
    x        = "Mean Actual Donors per Million Population (log10)",
    y        = "Weighted Efficiency (Utilized / Actual)",
    color    = "Region"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title      = element_text(face = "bold")
  )
print(p4)