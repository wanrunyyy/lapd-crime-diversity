# ============================================================
# 01_data_cleaning.R
# Clean LAPD crime data and calculate district-level diversity
# ============================================================

library(tidyverse)
library(lubridate)

# File paths
raw_file <- "data/raw/Crime_Data_from_2020_to_2024.csv"

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

if (!file.exists(raw_file)) {
  stop(
    "Raw data not found. Place the CSV file at: ",
    raw_file
  )
}

# 1. Import and clean data -----------------------------------------------

crime <- read_csv(
  raw_file,
  show_col_types = FALSE
) %>%
  mutate(
    DATE_OCC = parse_date_time(
      `DATE OCC`,
      orders = "Y b d I:M:S p"
    ),
    `Rpt Dist No` = str_pad(
      as.character(`Rpt Dist No`),
      width = 4,
      pad = "0"
    )
  ) %>%
  filter(
    DATE_OCC >= as.POSIXct("2020-01-01"),
    DATE_OCC < as.POSIXct("2024-01-01")
  ) %>%
  distinct(DR_NO, .keep_all = TRUE) %>%
  filter(
    !is.na(LAT),
    !is.na(LON),
    LAT != 0,
    LON != 0
  ) %>%
  mutate(
    year = year(DATE_OCC),
    year_month = floor_date(DATE_OCC, "month"),
    month_number = month(DATE_OCC),
    month_name = month(
      DATE_OCC,
      label = TRUE,
      abbr = TRUE
    ),
    season = case_when(
      month_number %in% c(12, 1, 2) ~ "Winter",
      month_number %in% c(3, 4, 5) ~ "Spring",
      month_number %in% c(6, 7, 8) ~ "Summer",
      month_number %in% c(9, 10, 11) ~ "Fall"
    ),
    season = factor(
      season,
      levels = c("Winter", "Spring", "Summer", "Fall")
    )
  )

# 2. Consolidate crime descriptions -------------------------------------

crime <- crime %>%
  mutate(
    crime_category = case_when(
      grepl(
        paste0(
          "RAPE|SEX|LEWD|PORNOGRAPHY|PEEPING|INDECENT|",
          "ORAL COPULATION|CHILD ANNOYING"
        ),
        `Crm Cd Desc`,
        ignore.case = TRUE
      ) ~ "Sexual Crime",
      
      grepl(
        "FIREARM|WEAPON|SHOT|BOMB",
        `Crm Cd Desc`,
        ignore.case = TRUE
      ) ~ "Weapon-related Crime",
      
      grepl(
        paste0(
          "FRAUD|EMBEZZLEMENT|FORGERY|COUNTERFEIT|",
          "BUNCO|CREDIT|EXTORTION|COMPUTER"
        ),
        `Crm Cd Desc`,
        ignore.case = TRUE
      ) ~ "Financial Crime",
      
      grepl(
        "VEHICLE|BIKE|BOAT|DRIVING",
        `Crm Cd Desc`,
        ignore.case = TRUE
      ) ~ "Vehicle-related Crime",
      
      grepl(
        paste0(
          "ASSAULT|BATTERY|ROBBERY|HOMICIDE|KIDNAPPING|",
          "THREAT|MANSLAUGHTER|CHILD ABUSE|",
          "CHILD NEGLECT|STALKING"
        ),
        `Crm Cd Desc`,
        ignore.case = TRUE
      ) ~ "Violent Crime",
      
      grepl(
        paste0(
          "THEFT|BURGLARY|SHOPLIFTING|VANDALISM|",
          "ARSON|PICKPOCKET|PURSE"
        ),
        `Crm Cd Desc`,
        ignore.case = TRUE
      ) ~ "Property Crime",
      
      grepl(
        paste0(
          "TRESPASS|VIOLATION|COURT|RESTRAINING|",
          "CONTEMPT|DISTURBING|DISPERSE|RESISTING"
        ),
        `Crm Cd Desc`,
        ignore.case = TRUE
      ) ~ "Public Order Crime",
      
      TRUE ~ "Other Crime"
    )
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

crime <- crime %>%
  mutate(
    crime_category = factor(
      crime_category,
      levels = crime_categories
    )
  )

# 3. Calculate district-level Shannon diversity --------------------------

calculate_shannon <- function(counts) {
  proportions <- counts / sum(counts)
  proportions <- proportions[proportions > 0]
  
  -sum(proportions * log(proportions))
}

crime_wide <- crime %>%
  count(
    `Rpt Dist No`,
    crime_category,
    .drop = FALSE
  ) %>%
  pivot_wider(
    names_from = crime_category,
    values_from = n,
    values_fill = 0
  ) %>%
  rowwise() %>%
  mutate(
    total_crime = sum(
      c_across(all_of(crime_categories))
    ),
    shannon_index = calculate_shannon(
      c_across(all_of(crime_categories))
    ),
    normalized_shannon = shannon_index / log(8)
  ) %>%
  ungroup()

district_analysis <- crime_wide %>%
  select(
    `Rpt Dist No`,
    total_crime,
    shannon_index,
    normalized_shannon
  )

# 4. Save essential processed data ---------------------------------------

write_rds(
  crime,
  "data/processed/crime_clean.rds"
)

write_rds(
  district_analysis,
  "data/processed/district_analysis.rds"
)

write_csv(
  district_analysis,
  "output/tables/district_analysis.csv"
)

write_csv(
  crime %>%
    count(crime_category, name = "crime_count") %>%
    mutate(proportion = crime_count / sum(crime_count)),
  "output/tables/crime_category_summary.csv"
)

# 5. Reproducibility checks ----------------------------------------------

cat("\nData cleaning completed.\n")
cat("Crime records:", nrow(crime), "\n")
cat(
  "Reporting Districts:",
  n_distinct(crime$`Rpt Dist No`),
  "\n"
)
cat(
  "Crime categories:",
  n_distinct(crime$crime_category),
  "\n"
)
cat(
  "Date range:",
  as.character(min(crime$DATE_OCC)),
  "to",
  as.character(max(crime$DATE_OCC)),
  "\n\n"
)

print(
  crime %>%
    count(crime_category) %>%
    mutate(proportion = n / sum(n))
)

print(summary(district_analysis$normalized_shannon))
