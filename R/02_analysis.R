# ============================================================
# 02_analysis.R
# District, temporal, spatial, regression, and seasonal analyses
# ============================================================

library(tidyverse)
library(lubridate)
library(sf)
library(spdep)
library(car)
library(effectsize)
library(ggplot2)

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

crime_clean <- read_rds(
  "data/processed/crime_clean.rds"
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

calculate_shannon <- function(counts) {
  p <- counts / sum(counts)
  p <- p[p > 0]
  -sum(p * log(p))
}


# ============================================================
# 1. Reporting District aggregation
# ============================================================

district_summary <- crime_clean %>%
  group_by(
    Rpt.Dist.No,
    crime_category
  ) %>%
  summarise(
    crime_count = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = crime_category,
    values_from = crime_count,
    values_fill = 0
  ) %>%
  mutate(
    total_crime = rowSums(
      across(all_of(crime_categories))
    )
  ) %>%
  rowwise() %>%
  mutate(
    shannon_index = calculate_shannon(
      c_across(all_of(crime_categories))
    )
  ) %>%
  ungroup() %>%
  mutate(
    normalized_shannon =
      shannon_index / log(8)
  )

write_rds(
  district_summary,
  "data/processed/district_summary.rds"
)

write_csv(
  district_summary,
  "output/tables/district_summary.csv"
)


# ============================================================
# 2. Monthly Reporting District diversity
# ============================================================

monthly_diversity <- crime_clean %>%
  group_by(
    Rpt.Dist.No,
    year_month,
    season,
    crime_category
  ) %>%
  summarise(
    crime_count = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = crime_category,
    values_from = crime_count,
    values_fill = 0
  ) %>%
  mutate(
    total_crime = rowSums(
      across(all_of(crime_categories))
    )
  ) %>%
  rowwise() %>%
  mutate(
    shannon_index = calculate_shannon(
      c_across(all_of(crime_categories))
    )
  ) %>%
  ungroup() %>%
  mutate(
    normalized_shannon =
      shannon_index / log(8),
    year = year(year_month),
    month = month(year_month)
  )

monthly_average <- monthly_diversity %>%
  group_by(year_month) %>%
  summarise(
    mean_diversity = mean(normalized_shannon),
    sd_diversity = sd(normalized_shannon),
    .groups = "drop"
  )

write_rds(
  monthly_diversity,
  "data/processed/monthly_diversity.rds"
)

write_rds(
  monthly_average,
  "data/processed/monthly_average.rds"
)

write_csv(
  monthly_average,
  "output/tables/monthly_average.csv"
)

median_diversity <- median(
  district_summary$normalized_shannon,
  na.rm = TRUE
)

diversity_distribution <- ggplot(
  district_summary,
  aes(x = normalized_shannon)
) +
  geom_histogram(
    binwidth = 0.025,
    boundary = 0,
    fill = "steelblue",
    color = "white",
    linewidth = 0.3,
    alpha = 0.9
  ) +
  geom_vline(
    xintercept = median_diversity,
    linetype = "dashed",
    linewidth = 0.8
  ) +
  annotate(
    "text",
    x = median_diversity + 0.015,
    y = 245,
    label = paste0(
      "Median = ",
      round(median_diversity, 3)
    ),
    hjust = 0,
    size = 4
  ) +
  labs(
    title = "Distribution of Crime Diversity",
    subtitle = "LAPD Reporting Districts, 2020–2023",
    x = "Normalized Crime Diversity Index",
    y = "Number of Reporting Districts"
  ) +
  scale_x_continuous(
    breaks = seq(0, 0.9, 0.1),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  coord_cartesian(
    xlim = c(0, 0.9)
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 18
    ),
    plot.subtitle = element_text(
      size = 13
    ),
    axis.title = element_text(
      size = 13
    ),
    axis.text = element_text(
      size = 11
    ),
    panel.grid.minor = element_blank()
  )

ggsave(
  "output/figures/diversity_distribution.png",
  plot = diversity_distribution,
  width = 8,
  height = 5,
  dpi = 300
)


# ============================================================
# 3. Spatial join
# ============================================================

shape_file <- paste0(
  "data/raw/LAPD_Reporting_District_polygons/",
  "LAPD_Reporting_District_polygons.shp"
)

if (!file.exists(shape_file)) {
  stop("Reporting District shapefile not found.")
}

rd_shape <- st_read(
  shape_file,
  quiet = TRUE
)

rd_shape_clean <- rd_shape %>%
  filter(REPDIST != "0") %>%
  mutate(
    REPDIST = sprintf(
      "%04d",
      as.integer(REPDIST)
    )
  )

district_summary <- district_summary %>%
  mutate(
    Rpt.Dist.No = sprintf(
      "%04d",
      as.integer(Rpt.Dist.No)
    )
  )

rd_spatial <- rd_shape_clean %>%
  left_join(
    district_summary,
    by = c(
      "REPDIST" = "Rpt.Dist.No"
    )
  )

if (sum(is.na(rd_spatial$total_crime)) != 0) {
  warning("Some Reporting District polygons did not match.")
}

write_rds(
  rd_spatial,
  "data/processed/rd_spatial.rds"
)


# ============================================================
# 4. Spatial autocorrelation
# ============================================================

rd_neighbors <- poly2nb(
  rd_spatial,
  queen = TRUE
)

rd_weights <- nb2listw(
  rd_neighbors,
  style = "W",
  zero.policy = TRUE
)

moran_crime_count <- moran.test(
  log1p(rd_spatial$total_crime),
  rd_weights,
  zero.policy = TRUE
)

moran_diversity <- moran.test(
  rd_spatial$normalized_shannon,
  rd_weights,
  zero.policy = TRUE
)

set.seed(123)

moran_mc_crime <- moran.mc(
  log1p(rd_spatial$total_crime),
  rd_weights,
  nsim = 999,
  zero.policy = TRUE
)

set.seed(123)

moran_mc_diversity <- moran.mc(
  rd_spatial$normalized_shannon,
  rd_weights,
  nsim = 999,
  zero.policy = TRUE
)

moran_results <- tibble(
  indicator = c(
    "Log Crime Count",
    "Crime Diversity Index"
  ),
  morans_i = c(
    unname(
      moran_crime_count$estimate[
        "Moran I statistic"
      ]
    ),
    unname(
      moran_diversity$estimate[
        "Moran I statistic"
      ]
    )
  ),
  analytical_p = c(
    moran_crime_count$p.value,
    moran_diversity$p.value
  ),
  permutation_p = c(
    moran_mc_crime$p.value,
    moran_mc_diversity$p.value
  )
)

write_rds(
  rd_weights,
  "data/processed/rd_weights.rds"
)

write_csv(
  moran_results,
  "output/tables/moran_results.csv"
)


# ============================================================
# 5. Seasonal analysis
# ============================================================

season_summary <- monthly_diversity %>%
  group_by(season) %>%
  summarise(
    Mean = mean(normalized_shannon),
    SD = sd(normalized_shannon),
    n = n(),
    .groups = "drop"
  )

levene_result <- leveneTest(
  normalized_shannon ~ season,
  data = monthly_diversity
)

anova_model <- aov(
  normalized_shannon ~ season,
  data = monthly_diversity
)

eta_result <- eta_squared(
  anova_model
)

tukey_result <- TukeyHSD(
  anova_model
)

write_rds(
  season_summary,
  "data/processed/season_summary.rds"
)

write_csv(
  season_summary,
  "output/tables/season_summary.csv"
)

write_csv(
  as.data.frame(levene_result),
  "output/tables/levene_test.csv"
)

write_csv(
  broom::tidy(anova_model),
  "output/tables/anova_results.csv"
)

write_csv(
  as.data.frame(eta_result),
  "output/tables/anova_effect_size.csv"
)

write_csv(
  as.data.frame(tukey_result$season) %>%
    rownames_to_column("comparison"),
  "output/tables/tukey_results.csv"
)


# ============================================================
# 6. Crime count vs crime diversity
# ============================================================

# Pearson correlation
correlation_test <- cor.test(
  log1p(district_summary$total_crime),
  district_summary$normalized_shannon,
  method = "pearson"
)

# Linear regression
crime_diversity_lm <- lm(
  normalized_shannon ~ log1p(total_crime),
  data = district_summary
)

# Save correlation results
correlation_results <- tibble(
  correlation = unname(correlation_test$estimate),
  p_value = correlation_test$p.value
)

# Regression coefficients
regression_results <- broom::tidy(
  crime_diversity_lm
)

# Regression fit statistics
regression_fit <- broom::glance(
  crime_diversity_lm
)

# Save results
write_csv(
  correlation_results,
  "output/tables/correlation_results.csv"
)

write_csv(
  regression_results,
  "output/tables/regression_coefficients.csv"
)

write_csv(
  regression_fit,
  "output/tables/regression_fit.csv"
)

write_rds(
  crime_diversity_lm,
  "data/processed/crime_diversity_lm.rds"
)


# ============================================================
# 7. Half-year spatial-temporal analysis
# ============================================================

period_levels <- c(
  "2020 H1", "2020 H2",
  "2021 H1", "2021 H2",
  "2022 H1", "2022 H2",
  "2023 H1", "2023 H2"
)

crime_st <- crime_clean %>%
  mutate(
    period = case_when(
      year == 2020 & month <= 6 ~ "2020 H1",
      year == 2020 & month > 6 ~ "2020 H2",
      year == 2021 & month <= 6 ~ "2021 H1",
      year == 2021 & month > 6 ~ "2021 H2",
      year == 2022 & month <= 6 ~ "2022 H1",
      year == 2022 & month > 6 ~ "2022 H2",
      year == 2023 & month <= 6 ~ "2023 H1",
      year == 2023 & month > 6 ~ "2023 H2",
      TRUE ~ NA_character_
    ),
    period = factor(
      period,
      levels = period_levels
    )
  )

district_period_summary <- crime_st %>%
  group_by(
    period,
    Rpt.Dist.No,
    crime_category
  ) %>%
  summarise(
    crime_count = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = crime_category,
    values_from = crime_count,
    values_fill = 0
  ) %>%
  mutate(
    total_crime = rowSums(
      across(all_of(crime_categories))
    )
  ) %>%
  rowwise() %>%
  mutate(
    shannon_index = calculate_shannon(
      c_across(all_of(crime_categories))
    )
  ) %>%
  ungroup() %>%
  mutate(
    normalized_shannon =
      shannon_index / log(8),
    Rpt.Dist.No = sprintf(
      "%04d",
      as.integer(Rpt.Dist.No)
    )
  )

rd_spatial_period <- do.call(
  rbind,
  lapply(
    period_levels,
    function(p) {
      rd_shape_clean %>%
        mutate(period = p)
    }
  )
)

rd_spatial_period$period <- factor(
  rd_spatial_period$period,
  levels = period_levels
)

rd_spatial_period <- rd_spatial_period %>%
  left_join(
    district_period_summary,
    by = c(
      "REPDIST" = "Rpt.Dist.No",
      "period" = "period"
    )
  )

write_rds(
  district_period_summary,
  "data/processed/district_period_summary.rds"
)

write_rds(
  rd_spatial_period,
  "data/processed/rd_spatial_period.rds"
)


# ============================================================
# Final checks
# ============================================================

cat("\nAnalysis completed.\n")
cat("Districts:", nrow(district_summary), "\n")
cat("Monthly observations:", nrow(monthly_diversity), "\n")
cat("Spatial polygons:", nrow(rd_spatial), "\n")
cat(
  "Pearson correlation:",
  round(unname(correlation_test$estimate), 4),
  "\n"
)
cat(
  "Regression R-squared:",
  round(summary(crime_diversity_lm)$r.squared, 4),
  "\n"
)