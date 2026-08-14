# ============================================================
# 03_visualization.R
# Create final EDA figures
# ============================================================

library(tidyverse)
library(sf)
library(spdep)
library(viridis)
library(patchwork)

dir.create(
  "output/figures",
  recursive = TRUE,
  showWarnings = FALSE
)

district_summary <- read_rds(
  "data/processed/district_summary.rds"
)

rd_spatial <- read_rds(
  "data/processed/rd_spatial.rds"
)

rd_weights <- read_rds(
  "data/processed/rd_weights.rds"
)

monthly_diversity <- read_rds(
  "data/processed/monthly_diversity.rds"
)

monthly_average <- read_rds(
  "data/processed/monthly_average.rds"
)

season_summary <- read_rds(
  "data/processed/season_summary.rds"
)

crime_diversity_lm <- read_rds(
  "data/processed/crime_diversity_lm.rds"
)

rd_spatial_period <- read_rds(
  "data/processed/rd_spatial_period.rds"
)


# ============================================================
# 1. Spatial comparison maps
# ============================================================

crime_count_map <- ggplot(rd_spatial) +
  geom_sf(
    aes(fill = log1p(total_crime)),
    color = "white",
    linewidth = 0.05
  ) +
  scale_fill_viridis_c(
    name = "Log Crime Count"
  ) +
  coord_sf(datum = NA) +
  theme_void() +
  labs(
    title = "Spatial Distribution of Crime Count",
    subtitle = "LAPD Reporting Districts"
  )

diversity_map <- ggplot(rd_spatial) +
  geom_sf(
    aes(fill = normalized_shannon),
    color = "white",
    linewidth = 0.05
  ) +
  scale_fill_viridis_c(
    limits = c(0.55, 0.90),
    breaks = c(0.60, 0.70, 0.80, 0.90),
    oob = scales::squish,
    name = "Crime Diversity Index"
  ) +
  coord_sf(datum = NA) +
  theme_void() +
  labs(
    title = "Spatial Distribution of Crime Diversity",
    subtitle = "Normalized Shannon Index"
  )

spatial_comparison <- crime_count_map + diversity_map

ggsave(
  "output/figures/spatial_comparison_maps.png",
  spatial_comparison,
  width = 12,
  height = 6,
  dpi = 300
)


# ============================================================
# 2. Moran scatterplots
# ============================================================

crime_z <- as.numeric(
  scale(log1p(rd_spatial$total_crime))
)

crime_lag <- lag.listw(
  rd_weights,
  crime_z,
  zero.policy = TRUE
)

diversity_z <- as.numeric(
  scale(rd_spatial$normalized_shannon)
)

diversity_lag <- lag.listw(
  rd_weights,
  diversity_z,
  zero.policy = TRUE
)

moran_plot_crime <- ggplot(
  data.frame(
    value = crime_z,
    lag = crime_lag
  ),
  aes(value, lag)
) +
  geom_point(
    alpha = 0.5,
    color = "steelblue",
    size = 1
  ) +
  geom_smooth(
    method = "lm",
    color = "red",
    se = FALSE
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Moran Scatterplot: Crime Count",
    x = "Standardized Log Crime Count",
    y = "Spatial Lag"
  ) +
  theme_minimal()

moran_plot_diversity <- ggplot(
  data.frame(
    value = diversity_z,
    lag = diversity_lag
  ),
  aes(value, lag)
) +
  geom_point(
    alpha = 0.5,
    color = "steelblue",
    size = 1
  ) +
  geom_smooth(
    method = "lm",
    color = "red",
    se = FALSE
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Moran Scatterplot: Crime Diversity",
    x = "Standardized Normalized Shannon Index",
    y = "Spatial Lag"
  ) +
  theme_minimal()

ggsave(
  "output/figures/moran_scatterplots.png",
  moran_plot_crime + moran_plot_diversity,
  width = 12,
  height = 6,
  dpi = 300
)


# ============================================================
# 3. Monthly diversity trend
# ============================================================

monthly_plot <- ggplot(
  monthly_average,
  aes(
    year_month,
    mean_diversity
  )
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_smooth(
    method = "loess",
    se = FALSE,
    color = "steelblue",
    linewidth = 1
  ) +
  labs(
    title = "Monthly Average Crime Diversity",
    x = "Month",
    y = "Average Normalized Shannon Index"
  ) +
  theme_minimal()

ggsave(
  "output/figures/monthly_diversity_trend.png",
  monthly_plot,
  width = 9,
  height = 6,
  dpi = 300
)


# ============================================================
# 4. Seasonal figures
# ============================================================

seasonal_boxplot <- ggplot(
  monthly_diversity,
  aes(
    season,
    normalized_shannon,
    fill = season
  )
) +
  geom_boxplot(
    alpha = 0.8,
    outlier.alpha = 0.3
  ) +
  labs(
    title = "Seasonal Distribution of Crime Diversity",
    x = "Season",
    y = "Normalized Shannon Index"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

ggsave(
  "output/figures/seasonal_diversity_boxplot.png",
  seasonal_boxplot,
  width = 8,
  height = 6,
  dpi = 300
)

season_mean_plot <- ggplot(
  season_summary,
  aes(
    season,
    Mean,
    fill = season
  )
) +
  geom_col(
    width = 0.6
  ) +
  geom_errorbar(
    aes(
      ymin = Mean - SD,
      ymax = Mean + SD
    ),
    width = 0.2
  ) +
  labs(
    title = "Average Crime Diversity by Season",
    x = "Season",
    y = "Mean Normalized Shannon Index"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

ggsave(
  "output/figures/seasonal_mean_diversity.png",
  season_mean_plot,
  width = 8,
  height = 6,
  dpi = 300
)


# ============================================================
# 5. Crime count vs crime diversity
# ============================================================

volume_plot <- ggplot(
  district_summary,
  aes(
    log1p(total_crime),
    normalized_shannon
  )
) +
  geom_point(
    alpha = 0.25,
    color = "steelblue",
    size = 1.3
  ) +
  geom_smooth(
    method = "lm",
    color = "red",
    se = TRUE,
    linewidth = 1
  ) +
  labs(
    title = "Crime Count and Crime Diversity",
    x = "Log(Total Crime Count + 1)",
    y = "Normalized Shannon Index"
  ) +
  theme_minimal()

ggsave(
  "output/figures/volume_diversity_relationship.png",
  volume_plot,
  width = 8,
  height = 6,
  dpi = 300
)

hex_plot <- ggplot(
  district_summary,
  aes(
    log1p(total_crime),
    normalized_shannon
  )
) +
  geom_hex() +
  labs(
    x = "Log(Total Crime Count + 1)",
    y = "Normalized Shannon Index"
  )

ggsave(
  "output/figures/volume_diversity_hexbin.png",
  hex_plot,
  width = 8,
  height = 6,
  dpi = 300
)


# ============================================================
# 6. Regression diagnostics
# ============================================================

png(
  "output/figures/regression_diagnostics.png",
  width = 1800,
  height = 1800,
  res = 250
)

par(mfrow = c(2, 2))
plot(crime_diversity_lm)
par(mfrow = c(1, 1))

dev.off()


# ============================================================
# 7. Spatial-temporal maps
# ============================================================

crime_count_st_map <- ggplot(
  rd_spatial_period
) +
  geom_sf(
    aes(fill = log1p(total_crime)),
    color = "white",
    linewidth = 0.03
  ) +
  scale_fill_viridis_c(
    name = "Log Crime Count",
    na.value = "grey90",
    limits = c(0.7, 6.6),
    oob = scales::squish
  ) +
  facet_wrap(
    ~ period,
    ncol = 4
  ) +
  coord_sf(datum = NA) +
  labs(
    title = "Spatial-Temporal Evolution of Crime Count",
    subtitle = "LAPD Reporting Districts, 2020–2023"
  ) +
  theme_void() +
  theme(
    strip.text = element_text(
      face = "bold",
      size = 10
    ),
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      hjust = 0.5
    )
  )

ggsave(
  "output/figures/crime_count_spatiotemporal.png",
  crime_count_st_map,
  width = 14,
  height = 7,
  dpi = 300
)

diversity_st_map <- ggplot(
  rd_spatial_period
) +
  geom_sf(
    aes(fill = normalized_shannon),
    color = "white",
    linewidth = 0.03
  ) +
  scale_fill_viridis_c(
    name = "Crime Diversity Index",
    na.value = "grey90",
    limits = c(0.58, 0.90),
    breaks = c(0.60, 0.70, 0.80, 0.90),
    oob = scales::squish
  ) +
  facet_wrap(
    ~ period,
    ncol = 4
  ) +
  coord_sf(datum = NA) +
  labs(
    title = "Spatial-Temporal Evolution of Crime Diversity",
    subtitle = paste(
      "Normalized Shannon Index by Reporting District,",
      "2020–2023"
    )
  ) +
  theme_void() +
  theme(
    strip.text = element_text(
      face = "bold",
      size = 10
    ),
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      hjust = 0.5
    )
  )

ggsave(
  "output/figures/crime_diversity_spatiotemporal.png",
  diversity_st_map,
  width = 14,
  height = 7,
  dpi = 300
)

cat("\nAll final EDA figures created successfully.\n")