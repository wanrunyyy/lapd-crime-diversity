# ============================================================
# 02_analysis.R
# Descriptive, spatial, and temporal analyses
# ============================================================

library(tidyverse)
library(lubridate)
library(sf)
library(spdep)
library(car)
library(effectsize)

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

# 1. Load processed data --------------------------------------------------

crime_file <- "data/processed/crime_clean.rds"
district_file <- "data/processed/district_analysis.rds"

if (!file.exists(crime_file) || !file.exists(district_file)) {
  stop("Processed data not found. Run R/01_data_cleaning.R first.")
}

crime <- read_rds(crime_file)
district_analysis <- read_rds(district_file)

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

calculate_shannon <- function(counts) {
  p <- counts / sum(counts)
  p <- p[p > 0]
  -sum(p * log(p))
}


# 2. Reporting District descriptive analysis -----------------------------

district_summary <- district_analysis %>%
  summarise(
    number_of_districts = n(),
    mean_crime = mean(total_crime),
    median_crime = median(total_crime),
    sd_crime = sd(total_crime),
    min_crime = min(total_crime),
    max_crime = max(total_crime),
    mean_diversity = mean(normalized_shannon),
    median_diversity = median(normalized_shannon),
    sd_diversity = sd(normalized_shannon),
    min_diversity = min(normalized_shannon),
    max_diversity = max(normalized_shannon)
  )

pearson_test <- cor.test(
  district_analysis$total_crime,
  district_analysis$normalized_shannon,
  method = "pearson"
)

spearman_test <- cor.test(
  district_analysis$total_crime,
  district_analysis$normalized_shannon,
  method = "spearman",
  exact = FALSE
)

correlation_results <- tibble(
  method = c("Pearson", "Spearman"),
  correlation = c(
    unname(pearson_test$estimate),
    unname(spearman_test$estimate)
  ),
  p_value = c(
    pearson_test$p.value,
    spearman_test$p.value
  )
)

write_csv(
  district_summary,
  "output/tables/district_summary.csv"
)

write_csv(
  correlation_results,
  "output/tables/correlation_results.csv"
)


# 3. Spatial analysis -----------------------------------------------------

shapefile_path <- paste0(
  "data/raw/LAPD_Reporting_District_polygons/",
  "LAPD_Reporting_District_polygons.shp"
)

if (!file.exists(shapefile_path)) {
  stop(
    "Reporting District shapefile not found at: ",
    shapefile_path
  )
}

rd_shape <- st_read(
  shapefile_path,
  quiet = TRUE
)

# Identify the Reporting District field in the shapefile
if ("REPDIST" %in% names(rd_shape)) {
  spatial_id <- "REPDIST"
} else if ("NAME" %in% names(rd_shape)) {
  spatial_id <- "NAME"
} else {
  stop("The shapefile does not contain REPDIST or NAME.")
}

rd_shape <- rd_shape %>%
  mutate(
    rd_id = str_pad(
      as.character(.data[[spatial_id]]),
      width = 4,
      side = "left",
      pad = "0"
    )
  ) %>%
  filter(
    !is.na(rd_id),
    rd_id != "0000"
  ) %>%
  st_make_valid()

rd_spatial <- rd_shape %>%
  left_join(
    district_analysis,
    by = c("rd_id" = "Rpt Dist No")
  ) %>%
  filter(
    !is.na(total_crime),
    !is.na(normalized_shannon)
  ) %>%
  mutate(
    log_total_crime = log1p(total_crime)
  )

# Queen-contiguity neighbors and row-standardized weights
rd_neighbors <- poly2nb(
  rd_spatial,
  queen = TRUE
)

rd_weights <- nb2listw(
  rd_neighbors,
  style = "W",
  zero.policy = TRUE
)

# Global Moran's I
global_moran_crime <- moran.test(
  rd_spatial$log_total_crime,
  rd_weights,
  alternative = "greater",
  zero.policy = TRUE
)

global_moran_diversity <- moran.test(
  rd_spatial$normalized_shannon,
  rd_weights,
  alternative = "greater",
  zero.policy = TRUE
)

# Permutation tests
set.seed(123)

moran_mc_crime <- moran.mc(
  rd_spatial$log_total_crime,
  rd_weights,
  nsim = 999,
  alternative = "greater",
  zero.policy = TRUE
)

set.seed(123)

moran_mc_diversity <- moran.mc(
  rd_spatial$normalized_shannon,
  rd_weights,
  nsim = 999,
  alternative = "greater",
  zero.policy = TRUE
)

moran_results <- tibble(
  indicator = c(
    "Log Total Crime Count",
    "Normalized Shannon Crime Diversity Index"
  ),
  morans_i = c(
    unname(global_moran_crime$estimate["Moran I statistic"]),
    unname(global_moran_diversity$estimate["Moran I statistic"])
  ),
  expected_i = c(
    unname(global_moran_crime$estimate["Expectation"]),
    unname(global_moran_diversity$estimate["Expectation"])
  ),
  variance = c(
    unname(global_moran_crime$estimate["Variance"]),
    unname(global_moran_diversity$estimate["Variance"])
  ),
  analytical_p_value = c(
    global_moran_crime$p.value,
    global_moran_diversity$p.value
  ),
  permutation_p_value = c(
    moran_mc_crime$p.value,
    moran_mc_diversity$p.value
  )
)

spatial_join_summary <- tibble(
  shapefile_polygons = nrow(rd_shape),
  matched_reporting_districts = nrow(rd_spatial),
  unmatched_polygons = nrow(rd_shape) - nrow(rd_spatial),
  minimum_neighbors = min(card(rd_neighbors)),
  mean_neighbors = mean(card(rd_neighbors)),
  maximum_neighbors = max(card(rd_neighbors)),
  districts_without_neighbors = sum(card(rd_neighbors) == 0)
)

write_csv(
  moran_results,
  "output/tables/moran_results.csv"
)

write_csv(
  spatial_join_summary,
  "output/tables/spatial_join_summary.csv"
)

write_rds(
  rd_spatial,
  "data/processed/rd_spatial.rds"
)

write_rds(
  rd_weights,
  "data/processed/rd_weights.rds"
)


# 4. Monthly Reporting District diversity --------------------------------

monthly_diversity <- crime %>%
  count(
    `Rpt Dist No`,
    year_month,
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
  ungroup() %>%
  mutate(
    month_number = month(year_month),
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

monthly_average <- monthly_diversity %>%
  group_by(year_month) %>%
  summarise(
    mean_diversity = mean(normalized_shannon),
    sd_diversity = sd(normalized_shannon),
    n = n(),
    .groups = "drop"
  )

season_summary <- monthly_diversity %>%
  group_by(season) %>%
  summarise(
    mean_diversity = mean(normalized_shannon),
    median_diversity = median(normalized_shannon),
    sd_diversity = sd(normalized_shannon),
    min_diversity = min(normalized_shannon),
    max_diversity = max(normalized_shannon),
    n = n(),
    se = sd_diversity / sqrt(n),
    ci_95 = qt(0.975, df = n - 1) * se,
    .groups = "drop"
  )


# 5. Seasonal statistical analysis ---------------------------------------

levene_result <- leveneTest(
  normalized_shannon ~ season,
  data = monthly_diversity
)

anova_model <- aov(
  normalized_shannon ~ season,
  data = monthly_diversity
)

anova_table <- as.data.frame(
  summary(anova_model)[[1]]
) %>%
  rownames_to_column("term") %>%
  as_tibble()

eta_result <- eta_squared(
  anova_model,
  ci = 0.95
) %>%
  as.data.frame() %>%
  as_tibble()

tukey_result <- TukeyHSD(
  anova_model
)$season %>%
  as.data.frame() %>%
  rownames_to_column("comparison") %>%
  as_tibble()

levene_table <- as.data.frame(
  levene_result
) %>%
  rownames_to_column("term") %>%
  as_tibble()


# 6. Save temporal and statistical results -------------------------------

write_rds(
  monthly_diversity,
  "data/processed/monthly_diversity.rds"
)

write_rds(
  anova_model,
  "data/processed/anova_model.rds"
)

write_csv(
  monthly_average,
  "output/tables/monthly_average.csv"
)

write_csv(
  season_summary,
  "output/tables/season_summary.csv"
)

write_csv(
  levene_table,
  "output/tables/levene_test.csv"
)

write_csv(
  anova_table,
  "output/tables/anova_results.csv"
)

write_csv(
  eta_result,
  "output/tables/anova_effect_size.csv"
)

write_csv(
  tukey_result,
  "output/tables/tukey_results.csv"
)

write_rds(
  rd_spatial,
  "data/processed/rd_spatial.rds"
)

write_rds(
  monthly_average,
  "data/processed/monthly_average.rds"
)

write_rds(
  season_summary,
  "data/processed/season_summary.rds"
)


# 7. Reproducibility checks ----------------------------------------------

cat("\nAnalysis completed.\n")
cat("----------------------------------------\n")
cat("District records:", nrow(district_analysis), "\n")
cat("Spatially matched districts:", nrow(rd_spatial), "\n")
cat("Monthly district observations:", nrow(monthly_diversity), "\n\n")

cat("Correlations:\n")
print(correlation_results)

cat("\nGlobal Moran's I:\n")
print(moran_results)

cat("\nSeasonal summary:\n")
print(season_summary)

cat("\nANOVA:\n")
print(summary(anova_model))

cat("\nEffect size:\n")
print(eta_result)