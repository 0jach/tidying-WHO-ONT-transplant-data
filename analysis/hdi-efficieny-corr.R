library(tidyverse)
library(readr)

donors   <- read_csv("./assets/donors_tidy.csv")
donors_totals <- read_csv("./assets/donors_tidy_total.csv")
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


eff = donors_totals %>%
  select(COUNTRY, REPORTYEAR, donor_status, amount) %>%
  pivot_wider(names_from = donor_status, values_from = amount)%>%
  distinct() %>%
  filter(!is.na(`Actual`), !is.na(`Utilized`), `Actual` > 0) %>%
  mutate(
    efficiency  = `Utilized` / `Actual`,
    COUNTRY_HDI = harmonise_name(COUNTRY)
  ) %>%
  filter(efficiency <= 1)

hdi <- hdi_raw %>%
  rename(COUNTRY_HDI = Entity, REPORTYEAR = Year, HDI = `Human Development Index`)

merged <- eff %>%
  inner_join(hdi %>% select(COUNTRY_HDI, REPORTYEAR, HDI),
             by = c("COUNTRY_HDI", "REPORTYEAR"))

cat("=== All country-year observations ===\n")
cat("n =", nrow(merged), "\n")
cor.test(merged$HDI, merged$efficiency, method = "pearson") %>% print()
cor.test(merged$HDI, merged$efficiency, method = "spearman") %>% print()

scatter <- merged %>%
  group_by(COUNTRY_HDI) %>%
  slice_max(REPORTYEAR, n = 1) %>%
  ungroup()

cat("\n=== Latest year per country ===\n")
cat("n =", nrow(scatter), "\n")
cor.test(scatter$HDI, scatter$efficiency, method = "pearson") %>% print()
cor.test(scatter$HDI, scatter$efficiency, method = "spearman") %>% print()

cat("\n=== Volume vs Efficiency (all country-year obs) ===\n")
cat("n =", nrow(eff), "\n")
cor.test(eff$`Actual`, eff$efficiency, method = "pearson") %>% print()
cor.test(eff$`Actual`, eff$efficiency, method = "spearman") %>% print()

scatter_vol <- eff %>%
  group_by(COUNTRY) %>%
  slice_max(REPORTYEAR, n = 1) %>%
  ungroup()

cat("\n=== Volume vs Efficiency (latest year per country) ===\n")
cat("n =", nrow(scatter_vol), "\n")
cor.test(scatter_vol$`Actual`, scatter_vol$efficiency, method = "pearson") %>% print()
cor.test(scatter_vol$`Actual`, scatter_vol$efficiency, method = "spearman") %>% print()

