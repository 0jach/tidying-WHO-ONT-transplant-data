library(tidyverse)
library(readr)

donors   <- read_csv("./assets/donors_tidy_total.csv")
trans    <- read_csv("./assets/transplant_tidy.csv")
hdi_raw  <- read_csv("./assets/human-development-index/human-development-index.csv")

name_map <- c(
  "Bolivia (Plurinational State of)"           = "Bolivia",
  "Brunei Darussalam"                          = "Brunei",
  "Czech Republic"                             = "Czechia",
  "Côte d'Ivoire"                              = "Cote d'Ivoire",
  "Democratic People's Republic of Korea"      = "North Korea",
  "Democratic Republic of The Congo"           = "Democratic Republic of Congo",
  "Iran (Islamic Republic of)"                 = "Iran",
  "Lao People's Democratic Republic"           = "Laos",
  "Micronesia (Federated States of)"           = "Micronesia",
  "Republic of Korea"                          = "South Korea",
  "Republic of Moldova"                        = "Moldova",
  "Republic of North Macedonia"                = "North Macedonia",
  "Russian Federation"                         = "Russia",
  "State of Libya"                             = "Libya",
  "Syrian Arab Republic"                       = "Syria",
  "Timor-Leste"                                = "Timor",
  "Türkiye"                                    = "Turkey",
  "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom",
  "United Republic of Tanzania"                = "Tanzania",
  "Venezuela (Bolivarian Republic of)"         = "Venezuela",
  "Viet Nam"                                   = "Vietnam"
)

harmonise_name <- function(x) ifelse(x %in% names(name_map), name_map[x], x)

donors <- donors %>% mutate(COUNTRY_HDI = harmonise_name(COUNTRY))
trans   <- trans  %>% mutate(COUNTRY_HDI = harmonise_name(COUNTRY))

hdi <- hdi_raw %>%
  rename(COUNTRY_HDI = Entity, REPORTYEAR = Year, HDI = `Human Development Index`)

hdi_tier <- function(h) {
  case_when(
    h < 0.55 ~ "Low",
    h < 0.70 ~ "Medium",
    h < 0.80 ~ "High",
    TRUE      ~ "Very High"
  )
}

tier_levels <- c("Low", "Medium", "High", "Very High")

donors_agg <- donors %>%
  filter(donor_status == "Actual", !is.na(amount)) %>%
  group_by(COUNTRY_HDI, REPORTYEAR, POPULATION) %>%
  summarise(total_donors = sum(amount), .groups = "drop") %>%
  mutate(donors_pmp = total_donors / POPULATION)

trans_agg <- trans %>%
  filter(!is.na(transplant_count)) %>%
  group_by(COUNTRY_HDI, REPORTYEAR, POPULATION) %>%
  summarise(total_transplants = sum(transplant_count), .groups = "drop") %>%
  mutate(transplants_pmp = total_transplants / POPULATION)

merged_donors <- donors_agg %>%
  inner_join(hdi %>% select(COUNTRY_HDI, REPORTYEAR, HDI),
             by = c("COUNTRY_HDI", "REPORTYEAR")) %>%
  mutate(tier = factor(hdi_tier(HDI), levels = tier_levels))

merged_trans <- trans %>%
  filter(!is.na(transplant_count)) %>%
  inner_join(hdi %>% select(COUNTRY_HDI, REPORTYEAR, HDI),
             by = c("COUNTRY_HDI", "REPORTYEAR")) %>%
  mutate(tier = factor(hdi_tier(HDI), levels = tier_levels))

scatter_df <- merged_donors %>%
  group_by(COUNTRY_HDI) %>%
  slice_max(REPORTYEAR, n = 1) %>%
  ungroup()

theme_clean <- theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13, margin = margin(b = 8)),
    plot.subtitle    = element_text(color = "grey40", size = 10, margin = margin(b = 12)),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    plot.margin      = margin(15, 15, 10, 15)
  )

labels_to_show <- c("Spain", "Japan", "Croatia", "Portugal", "Italy",
                    "Brazil", "Germany", "Singapore", "India", "Belarus")

scatter_labelled <- scatter_df %>%
  mutate(show_label = COUNTRY_HDI %in% labels_to_show)

r_val   <- cor(scatter_df$HDI, scatter_df$donors_pmp, method = "pearson")
rho_val <- cor(scatter_df$HDI, scatter_df$donors_pmp, method = "spearman")

p1 <- ggplot(scatter_labelled, aes(x = HDI, y = donors_pmp)) +
  geom_smooth(method = "lm", se = TRUE, color = "#534AB7", fill = "#EEEDFE",
              linewidth = 0.8, alpha = 0.5) +
  geom_point(aes(size = POPULATION), alpha = 0.55, color = "#1D9E75") +
  geom_point(data = filter(scatter_labelled, show_label),
             aes(size = POPULATION), color = "#534AB7", alpha = 0.8) +
  ggrepel::geom_text_repel(
    data = filter(scatter_labelled, show_label),
    aes(label = COUNTRY_HDI),
    size = 3, color = "grey30", max.overlaps = 20,
    segment.color = "grey70", segment.size = 0.3
  ) +
  scale_size_continuous(range = c(1.5, 12), guide = "none") +
  labs(
    title    = "1. Higher HDI = more organ donors",
    subtitle = sprintf("Pearson r = %.2f | Spearman ρ = %.2f  (latest year per country, n = %d)",
                       r_val, rho_val, nrow(scatter_df)),
    x = "Human Development Index",
    y = "Deceased donors per million"
  ) +
  theme_clean

source_by_tier <- merged_trans %>%
  filter(donor_source %in% c("living", "deceased")) %>%
  group_by(tier, donor_source) %>%
  summarise(total = sum(transplant_count), .groups = "drop") %>%
  group_by(tier) %>%
  mutate(pct = total / sum(total) * 100) %>%
  ungroup()

p2 <- ggplot(source_by_tier, aes(x = tier, y = pct, fill = donor_source)) +
  geom_col(position = "stack", width = 0.65) +
  geom_text(aes(label = paste0(round(pct), "%")),
            position = position_stack(vjust = 0.5),
            size = 3.2, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("deceased" = "#3266ad", "living" = "#D85A30"),
                    labels = c("Deceased donor", "Living donor")) +
  labs(
    title    = "2. Low-HDI countries rely almost entirely on living donors",
    subtitle = "Share of transplants by donor source",
    x = "Human Development Index", y = "% of transplants"
  ) +
  theme_clean +
  theme(legend.position = "bottom")

organ_by_tier <- merged_trans %>%
  group_by(tier, organ_transplanted) %>%
  summarise(total = sum(transplant_count), .groups = "drop") %>%
  group_by(tier) %>%
  mutate(pct = total / sum(total) * 100) %>%
  ungroup() %>%
  mutate(organ_transplanted = factor(
    organ_transplanted,
    levels = c("kidney", "liver", "lung", "pancreas", "kidney_pancreas", "small_bowel")
  ))

organ_labels <- c(
  "kidney"          = "Kidney",
  "liver"           = "Liver",
  "lung"            = "Lung",
  "pancreas"        = "Pancreas",
  "kidney_pancreas" = "Kidney-Pancreas",
  "small_bowel"     = "Small bowel"
)

organ_colors <- c(
  "kidney"          = "#3266ad",
  "liver"           = "#1D9E75",
  "lung"            = "#7F77DD",
  "pancreas"        = "#BA7517",
  "kidney_pancreas" = "#D4537E",
  "small_bowel"     = "#888780"
)

p3 <- ggplot(organ_by_tier, aes(x = tier, y = pct, fill = organ_transplanted)) +
  geom_col(position = "stack", width = 0.65) +
  scale_fill_manual(values = organ_colors, labels = organ_labels) +
  labs(
    title    = "3. Organ diversity increases with development",
    subtitle = "Low HDI ≈ kidney only; Very High HDI adds liver, lung, pancreas",
    x = "Human Development Index", y = "% of transplants"
  ) +
  theme_clean +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 9))

fit <- lm(donors_pmp ~ HDI, data = merged_donors)

residuals_df <- merged_donors %>%
  mutate(predicted = predict(fit, newdata = .),
         residual  = donors_pmp - predicted) %>%
  group_by(COUNTRY_HDI) %>%
  summarise(
    avg_residual = mean(residual),
    avg_hdi      = mean(HDI),
    avg_donors   = mean(donors_pmp),
    .groups      = "drop"
  )

top10    <- residuals_df %>% slice_max(avg_residual, n = 10)
bottom10 <- residuals_df %>% slice_min(avg_residual, n = 10)
outliers <- bind_rows(
  top10    %>% mutate(group = "Overperformer"),
  bottom10 %>% mutate(group = "Underperformer")
) %>%
  mutate(COUNTRY_HDI = fct_reorder(COUNTRY_HDI, avg_residual))

p4 <- ggplot(outliers, aes(x = avg_residual, y = COUNTRY_HDI, fill = group)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0, linewidth = 0.4, color = "grey40") +
  scale_fill_manual(values = c("Overperformer" = "#1D9E75", "Underperformer" = "#E24B4A")) +
  labs(
    title    = "4. Policy matters: some countries defy their HDI prediction",
    subtitle = "Average residual from linear model (donors PMP ~ HDI)",
    x = "Donors PMP above/below prediction", y = NULL
  ) +
  theme_clean +
  theme(legend.position = "bottom")

library(patchwork)

combined <- (p1 + p2) / (p3 + p4) +
  patchwork::plot_annotation(
    title    = "Human Development Index & Organ Donation: 4 Key Findings",
    subtitle = "Data: WHO Global Observatory on Donation and Transplantation + UNDP Human Development Index",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(color = "grey40", size = 11)
    )
  )

ggsave("./analysis/HDI/hdi_transplant_4_findings.png", combined,
       width = 16, height = 12, dpi = 300, bg = "white")

file_to_open = normalizePath("./analysis/HDI/hdi_transplant_4_findings.png")

if (.Platform$OS.type == "windows") { ## pay respect to linux demons
  shell.exec(file_to_open)
} else {
  browseURL(file_to_open) ## opens on browser
}
library(dplyr)
library(tidyr)

# 1. Sum transplant counts by country-year and organ
organ_counts = merged_trans %>%
  group_by(
    REGION, COUNTRY, REPORTYEAR, POPULATION,
    COUNTRY_HDI, HDI, tier, organ_transplanted
  ) %>%
  summarise(
    transplant_count = sum(transplant_count, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Turn organs into columns
organ_wide = organ_counts %>%
  pivot_wider(
    names_from = organ_transplanted,
    values_from = transplant_count,
    values_fill = 0
  )

# 3. Build total transplants and organ shares
organ_share_df = organ_wide %>%
  mutate(
    total_tx = kidney + liver +  lung + pancreas + small_bowel + kidney_pancreas,
    kidney_share = kidney / total_tx,
    liver_share = liver / total_tx,
    lung_share = lung / total_tx,
    pancreas_share = pancreas / total_tx
  ) %>%
  filter(
    total_tx > 0,
    !is.na(HDI)
  )

plot(organ_share_df$HDI, organ_share_df$lung_share,
     xlab = "HDI",
     ylab = "Lung share of total transplants",
     main = "Lung share vs HDI",
     pch = 1)

lines(lowess(organ_share_df$HDI, organ_share_df$lung_share), lwd = 2, col = "red")

active_df = subset(organ_share_df, total_tx >= 100)
region_colors = c(
  "Africa" = "red",
  "America" = "blue",
  "Eastern Mediterranean" = "darkgreen",
  "Europe" = "orange",
  "South-East Asia" = "purple",
  "Western Pacific" = "brown"
)

plot(active_df$HDI, active_df$kidney_share,
     col = region_colors[active_df$REGION],
     pch = 16,
     xlab = "HDI",
     ylab = "Kidney share of total transplants",
     main = "Kidney share vs HDI by region")

lines(lowess(organ_share_df$HDI, organ_share_df$kidney_share), lwd = 2, col = "black")

legend("topright",
       legend = names(region_colors),
       col = region_colors,
       pch = 16,
       cex = 0.8)

cor(active_df$HDI, active_df$lung_share, use = "complete.obs")
cor(active_df$HDI, active_df$kidney_share, use = "complete.obs")
