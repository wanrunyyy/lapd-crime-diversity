# Crime Diversity Across LAPD Reporting Districts

This repository contains the R code and supporting files used to
reproduce the analysis for my master's thesis on spatial and temporal
crime diversity across Los Angeles Police Department (LAPD) Reporting
Districts.

## Research Questions

The study addresses the following questions:

1. Can a Crime Diversity Index provide additional insights into local
   crime patterns beyond traditional crime counts?
2. How does crime diversity vary across LAPD Reporting Districts?
3. How does crime diversity change over time across months and seasons?

## Study Period

The analysis includes crime incidents occurring between January 1,
2020 and December 31, 2023.

Although the source dataset also contains 2024 observations, they are
excluded because 2024 does not represent a complete study year.

## Data Sources

### LAPD Crime Data

Crime records come from the Los Angeles Open Data Portal dataset
**Crime Data from 2020 to 2024**.

The raw dataset is not included in this repository because of its file
size. Download the CSV file and save it as:

```text
data/raw/Crime_Data_from_2020_to_2024.csv
```

### LAPD Reporting District Boundaries

The spatial analysis requires LAPD Reporting District boundary files.
Place the shapefile and its associated files in:

```text
data/raw/LAPD_Reporting_District_polygons/
```

The analysis expects the main shapefile at:

```text
data/raw/LAPD_Reporting_District_polygons/LAPD_Reporting_District_polygons.shp
```

### Crime Category Mapping

The file:

```text
data/raw/crime_mapping_final.csv
```

is included in this repository and maps the original LAPD crime
descriptions into the eight crime categories used in the analysis.

## Crime Categories

The original LAPD crime descriptions are consolidated into eight
categories:

1. Financial Crime
2. Other Crime
3. Property Crime
4. Public Order Crime
5. Sexual Crime
6. Vehicle-related Crime
7. Violent Crime
8. Weapon-related Crime

## Crime Diversity Measure

For Reporting District \(k\), the Shannon Diversity Index is:

\[
H_k = -\sum_{i=1}^{R} p_{ik}\log(p_{ik}),
\]

where \(p_{ik}\) is the proportion of crime incidents belonging to
category \(i\) within Reporting District \(k\).

The normalized Crime Diversity Index is calculated as:

\[
H_k^{*} = \frac{H_k}{\log(R)},
\]

where \(R=8\) is the number of crime categories. The normalized index
ranges from 0 to 1, with higher values indicating a more even
distribution of crime across categories.

## Analysis

The reproducible workflow includes:

- Data cleaning and crime-category assignment
- Reporting District-level Crime Diversity Index calculation
- Monthly and seasonal crime diversity analysis
- Spatial comparison of crime counts and crime diversity
- Global Moran's I spatial autocorrelation analysis
- Seasonal ANOVA, effect size, and post-hoc comparisons
- Pearson correlation and linear regression between log crime count
  and crime diversity
- Half-year spatio-temporal analysis from 2020 to 2023

## Repository Structure

```text
lapd-crime-diversity/
├── R/
│   ├── 01_data_cleaning.R
│   ├── 02_analysis.R
│   ├── 03_visualization.R
│   └── run_all.R
├── data/
│   ├── raw/
│   │   └── crime_mapping_final.csv
│   └── processed/
├── output/
│   ├── figures/
│   └── tables/
├── .gitignore
├── README.md
└── lapd-crime-diversity.Rproj
```

Large raw data and reproducible processed data are excluded from the
repository using `.gitignore`.

## Required R Packages

Before running the analysis, install the required packages:

```r
install.packages(c(
  "tidyverse",
  "lubridate",
  "sf",
  "spdep",
  "car",
  "effectsize",
  "patchwork",
  "viridis",
  "broom"
))
```

## Running the Project

After downloading the required raw LAPD crime data and Reporting
District boundary files and placing them in the locations described
above, run:

```r
source("R/run_all.R")
```

The script runs the complete workflow in the following order:

```text
01_data_cleaning.R
        ↓
02_analysis.R
        ↓
03_visualization.R
```

Generated results are saved to:

```text
output/tables/
output/figures/
```

## Author

Wanrun Yang  
Master of Applied Statistics and Data Science  
University of California, Los Angeles