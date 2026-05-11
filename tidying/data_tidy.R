library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(ggplot2)
library(here)

path = here("assets", "Dataset_raw.xlsx")

if (!file.exists(path)) {
  stop("Dataset_raw.xlsx not found in assets/")
}

df = read_excel(path)

df = df |>
  mutate(
    POPULATION = as.double(gsub(",", "", as.character(POPULATION))),
    REPORTYEAR = as.integer(REPORTYEAR)
  )

df_donors_raw = df |>
  select(
    REGION, COUNTRY, REPORTYEAR, POPULATION,
    `TOTAL Actual DD`, `Total Utilized DD`,
    `Actual DBD`, `Actual DCD`,
    `Utilized DBD`, `Utilized DCD`
  )

donor_by_type = df_donors_raw |>
  pivot_longer(
    cols = c(`Actual DBD`, `Actual DCD`, `Utilized DBD`, `Utilized DCD`),
    names_to = c("donor_status", "donor_type"),
    names_sep = " ",
    values_to = "donors"
  ) |>
  filter(!is.na(donors)
)|> select(- `TOTAL Actual DD`, - `Total Utilized DD`)

donor_totals_tidy = df_donors_raw |>
  mutate(
    total_actual = case_when(
      !is.na(`TOTAL Actual DD`) ~ `TOTAL Actual DD`,
      !is.na(`Actual DBD`) & !is.na(`Actual DCD`) ~ `Actual DBD` + `Actual DCD`,
      TRUE ~ NA_real_
    ),
    total_utilized = case_when(
      !is.na(`Total Utilized DD`) ~ `Total Utilized DD`,
      !is.na(`Utilized DBD`) & !is.na(`Utilized DCD`) ~ `Utilized DBD` + `Utilized DCD`,
      TRUE ~ NA_real_
    )
  ) |>
  select(REGION, COUNTRY, REPORTYEAR, POPULATION, total_actual, total_utilized) |>
  pivot_longer(
    cols = c(total_actual, total_utilized),
    names_to = "donor_status",
    values_to = "amount"
  ) |>
  mutate(
    donor_status = recode(
      donor_status,
      total_actual = "Actual",
      total_utilized = "Utilized"
    )
  )

transplants_tidy = df |>
  select(
    REGION, COUNTRY, REPORTYEAR, POPULATION,
    `DD Kidney Tx`, `LD Kidney Tx`,
    `DD Liver Tx`, `DOMINO Liver Tx`, `LD Liver Tx`,
    `DD Lung Tx`, `LD Lung Tx`,
    `Pancreas Tx`, `Kidney Pancreas Tx`, `Small Bowel Tx`
  ) |>
  pivot_longer(
    cols = c(
      `DD Kidney Tx`, `LD Kidney Tx`,
      `DD Liver Tx`, `DOMINO Liver Tx`, `LD Liver Tx`,
      `DD Lung Tx`, `LD Lung Tx`,
      `Pancreas Tx`, `Kidney Pancreas Tx`, `Small Bowel Tx`
    ),
    names_to = "tx_key",
    values_to = "transplant_count"
  ) |>
  mutate(
    donor_source = case_when(
      str_detect(tx_key, "^DD ") ~ "deceased",
      str_detect(tx_key, "^LD ") ~ "living",
      str_detect(tx_key, "^DOMINO ") ~ "domino",
      tx_key %in% c("Pancreas Tx", "Kidney Pancreas Tx", "Small Bowel Tx") ~ "deceased",
      TRUE ~ NA_character_
    ),
    organ_transplanted = case_when(
      tx_key == "Kidney Pancreas Tx" ~ "kidney_pancreas",
      tx_key == "Small Bowel Tx" ~ "small_bowel",
      tx_key == "Pancreas Tx" ~ "pancreas",
      str_detect(tx_key, "Kidney") ~ "kidney",
      str_detect(tx_key, "Liver") ~ "liver",
      str_detect(tx_key, "Lung") ~ "lung",
      TRUE ~ NA_character_
    )
  ) |>
  select(-tx_key)

write.csv(donor_by_type, here("assets", "donors_tidy.csv"), row.names = FALSE)
write.csv(donor_totals_tidy, here("assets", "donors_tidy_total.csv"), row.names = FALSE)
write.csv(transplants_tidy, here("assets", "transplant_tidy.csv"), row.names = FALSE)
