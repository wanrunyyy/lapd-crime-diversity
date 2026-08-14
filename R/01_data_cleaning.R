# ============================================================
# 01_data_cleaning.R
# Clean LAPD crime data and calculate district-level diversity
# ============================================================

library(tidyverse)
library(lubridate)

# File paths
crime_file <- "data/raw/Crime_Data_from_2020_to_2024.csv"
mapping_file <- "data/raw/crime_mapping_final.csv"

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

if (!file.exists(crime_file)) {
  stop("Missing raw crime data: ", crime_file)
}

if (!file.exists(mapping_file)) {
  stop("Missing crime mapping file: ", mapping_file)
}

# 1. Import data ----------------------------------------------------------

crime_raw <- read.csv(
  crime_file,
  stringsAsFactors = FALSE
)

crime_mapping <- read.csv(
  mapping_file,
  stringsAsFactors = FALSE
)

# 2. Clean crime records --------------------------------------------------

crime_clean <- crime_raw %>%
  mutate(
    DATE_OCC = parse_date_time(
      DATE.OCC,
      orders = "Y b d I:M:S p"
    )
  ) %>%
  filter(
    DATE_OCC >= as.POSIXct("2020-01-01"),
    DATE_OCC < as.POSIXct("2024-01-01")
  ) %>%
  distinct(DR_NO, .keep_all = TRUE) %>%
  filter(
    LAT != 0,
    LON != 0
  ) %>%
  mutate(
    year = year(DATE_OCC),
    month = month(DATE_OCC),
    year_month = floor_date(DATE_OCC, "month"),
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5) ~ "Spring",
      month %in% c(6, 7, 8) ~ "Summer",
      TRUE ~ "Fall"
    ),
    season = factor(
      season,
      levels = c("Winter", "Spring", "Summer", "Fall")
    )
  )

# 3. Apply final crime-category mapping ----------------------------------

crime_clean <- crime_clean %>%
  left_join(
    crime_mapping,
    by = c("Crm.Cd.Desc" = "crime_type")
  )

crime_categories <- c(
  "Financial Crime",
  "Other Crime",
  "Property Crime",
  "Public Order Crime",
  "Sexual Crime",
  "Vehicle-related Crime",
  "Violent Crime",
  "Weapon-related Crime"
)

# Check that every crime description has a mapping
if (any(is.na(crime_clean$crime_category))) {
  stop(
    "Some crime descriptions are missing from crime_mapping_final.csv."
  )
}

# 4. Save cleaned data ----------------------------------------------------

write_rds(
  crime_clean,
  "data/processed/crime_clean.rds"
)

write_csv(
  crime_clean %>%
    count(crime_category, name = "crime_count") %>%
    mutate(
      proportion = crime_count / sum(crime_count)
    ),
  "output/tables/crime_category_summary.csv"
)

# 5. Reproducibility checks ----------------------------------------------

cat("\nData cleaning completed.\n")
cat("Crime records:", nrow(crime_clean), "\n")
cat(
  "Reporting Districts:",
  n_distinct(crime_clean$Rpt.Dist.No),
  "\n"
)
cat(
  "Date range:",
  as.character(min(crime_clean$DATE_OCC)),
  "to",
  as.character(max(crime_clean$DATE_OCC)),
  "\n"
)