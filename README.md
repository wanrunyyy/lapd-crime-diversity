# Crime Diversity Across LAPD Reporting Districts

This repository contains the R code and supporting files used to
reproduce the analysis for my master's thesis on spatial and temporal
crime diversity across Los Angeles Police Department Reporting
Districts.

## Research questions

The study addresses the following questions:

1. Can a Crime Diversity Index provide additional insights into local
   crime patterns beyond traditional crime rates?
2. How does crime diversity vary across LAPD Reporting Districts?
3. How does crime diversity change across months and seasons?

## Study period

The analysis includes crime incidents occurring between January 1,
2020 and December 31, 2023.

Although the source dataset also contains 2024 observations, they are
excluded from the study period.

## Data sources

### LAPD crime data

The crime records come from the Los Angeles Open Data Portal dataset:

**Crime Data from 2020 to 2024**

Download the CSV file and save it as:

```text
data/raw/Crime_Data_from_2020_to_2024.csv
```
The spatial analysis also requires LAPD Reporting District boundary
data. Save the required spatial files inside:

`data/raw/`

## Crime categories

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

## Diversity measure

For Reporting District \(k\), the Shannon Diversity Index is:

\[
H_k = -\sum_{i=1}^{R}p_{ik}\log(p_{ik}),
\]

where \(p_{ik}\) is the proportion of crime incidents in category
\(i\) within Reporting District \(k\).

The normalized index is calculated as:

\[
H_k^{*} = \frac{H_k}{\log(R)},
\]

where \(R=8\) is the number of crime categories.

## Software

The analysis was conducted in R.

## Required R packages

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
  "viridis"
))
```

## Running the project

After placing the raw data in the correct folders, run:

```r
source("R/run_all.R")
```

This script will:

- Clean the raw data
- Calculate the Crime Diversity Index
- Perform the statistical analyses
- Generate all tables
- Generate all figures

## Author

Wanrun Yang  
Master of Applied Statistics and Data Science  
University of California, Los Angeles

