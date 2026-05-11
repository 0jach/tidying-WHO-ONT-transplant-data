# Tidying WHO/ONT Transplant Data

This repository contains an R project for tidying and exploring WHO/ONT organ donation and transplantation data.

## Goal

The goal of the project is to transform the raw WHO/ONT transplant dataset into tidy, analysis-ready tables and use them for exploratory analysis of international organ donation and transplantation patterns.

The project also focuses on the **Human Development Index (HDI)**, using it to compare transplant activity with broader country-level development indicators.

The analysis focuses mainly on:

- donor and transplant activity by country and year;
- deceased and living donor activity;
- organ-specific transplant counts;
- population-adjusted indicators;
- relationships between transplant activity and HDI.

The main results and discussion are available in:

```text
OrganTransplants_HDI_analysis.pdf
```

## Repository structure

```text
.
├── analysis/
│   ├── hdi-efficieny-corr.R
│   ├── hdi_organ_donation_full_report.txt
│   ├── efficiency/
│   │   └── efficiency_mean_weighted.R
│   └── HDI/
│       ├── HDI-Org analysis.R
│       ├── hdi_analysis_summary.txt
│       └── hdi_transplant_4_findings.png
│
├── assets/
│   ├── Dataset_raw.xlsx
│   ├── donors_tidy.csv
│   ├── donors_tidy_total.csv
│   ├── transplant_tidy.csv
│   └── human-development-index/
│       ├── human-development-index.csv
│       ├── human-development-index.metadata.json
│       └── readme.md
│
├── tidying/
│   └── data_tidy.R
│
├── OrganTransplants_HDI_analysis.pdf
├── requirements.R
└── README.md
```

## Main components

- `tidying/data_tidy.R`: script used to clean and reshape the raw WHO/ONT data.
- `assets/`: raw data, tidy outputs, and HDI data.
- `analysis/`: scripts and summaries for the exploratory analysis.
- `OrganTransplants_HDI_analysis.pdf`: final report containing the results.
