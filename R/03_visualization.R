# ============================================================
# 03_visualization.R
# Create and save the final thesis figures
# ============================================================

library(tidyverse)
library(sf)
library(spdep)
library(patchwork)
library(viridis)

dir.create(
  "output/figures",
  recursive = TRUE,
  showWarnings = FALSE
)

# 1. Load processed data --------------------------------------------------

required_files <- c(
  "data/processed/district_analysis.rds",
  "data/processed/rd_spatial.rds",
  "data/processed/rd_weights.rds",
  "data/processed/monthly_diversity.rds",
  "data/processed/monthly_average.rds",
  "data/processed/season_summary.rds"
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {
  stop(
    paste0(
      "Missing processed files:\n",
      paste(missing_files, collapse = "\n"),
      "\n\nRun R/01_data_cleaning.R and R/02_analysis.R first."
    )
  )
}

district_analysis <- read_rds(
  "data/processed/district_analysis.rds"
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


# 2. Crime volume and diversity relationship -----------------------------

volume_diversity_plot <- ggplot(
  district_analysis,
  aes(
    x = total_crime,
    y = normalized_shannon
  )
) +
  geom_point(
    alpha = 0.45,
    size = 2
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  scale_x_log10() +
  theme_minimal(base_size = 13) +
  labs(
    x = "Total Reported Crime Count (log scale)",
    y = "Normalized Shannon Crime Diversity Index"
  )

ggsave(
  "output/figures/volume_diversity_relationship.png",
  volume_diversity_plot,
  width = 8,
  height = 6,
  dpi = 300
)


# 3. Reporting District maps ---------------------------------------------

crime_count_map <- ggplot(rd_spatial) +
  geom_sf(
    aes(fill = log_total_crime),
    color = "white",
    linewidth = 0.05
  ) +
  scale_fill_viridis_c(
    name = "Log Crime Count"
  ) +
  coord_sf(datum = NA) +
  theme_void() +
  labs(
    title = "Spatial Distribution of Reported Crime",
    subtitle = "LAPD Reporting Districts"
  )

diversity_map <- ggplot(rd_spatial) +
  geom_sf(
    aes(fill = normalized_shannon),
    color = "white",
    linewidth = 0.05
  ) +
  scale_fill_viridis_c(
    limits = c(0, 1),
    name = "Diversity Index"
  ) +
  coord_sf(datum = NA) +
  theme_void() +
  labs(
    title = "Spatial Distribution of Crime Diversity",
    subtitle = paste(
      "Normalized Shannon Index by",
      "LAPD Reporting District"
    )
  )

combined_spatial_maps <- crime_count_map + diversity_map +
  plot_annotation(
    title = "Comparison of Crime Volume and Crime Diversity"
  ) &
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    )
  )

ggsave(
  "output/figures/spatial_comparison_maps.png",
  combined_spatial_maps,
  width = 14,
  height = 7,
  dpi = 300
)


# 4. Moran scatterplots ---------------------------------------------------

crime_z <- as.numeric(
  scale(rd_spatial$log_total_crime)
)

diversity_z <- as.numeric(
  scale(rd_spatial$normalized_shannon)
)

crime_spatial_lag <- lag.listw(
  rd_weights,
  crime_z,
  zero.policy = TRUE
)

diversity_spatial_lag <- lag.listw(
  rd_weights,
  diversity_z,
  zero.policy = TRUE
)

moran_plot_data <- rd_spatial %>%
  mutate(
    crime_z = crime_z,
    diversity_z = diversity_z,
    crime_spatial_lag = crime_spatial_lag,
    diversity_spatial_lag = diversity_spatial_lag
  )

moran_crime_plot <- ggplot(
  moran_plot_data,
  aes(
    x = crime_z,
    y = crime_spatial_lag
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  geom_point(
    alpha = 0.45,
    size = 1.5
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = FALSE
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Moran Scatterplot: Crime Volume",
    x = "Standardized Log Crime Count",
    y = "Spatial Lag"
  )

moran_diversity_plot <- ggplot(
  moran_plot_data,
  aes(
    x = diversity_z,
    y = diversity_spatial_lag
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  geom_point(
    alpha = 0.45,
    size = 1.5
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = FALSE
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Moran Scatterplot: Crime Diversity",
    x = "Standardized Normalized Shannon Index",
    y = "Spatial Lag"
  )

combined_moran_plots <- moran_crime_plot +
  moran_diversity_plot +
  plot_annotation(
    title = paste(
      "Spatial Autocorrelation Across",
      "LAPD Reporting Districts"
    )
  ) &
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    )
  )

ggsave(
  "output/figures/moran_scatterplots.png",
  combined_moran_plots,
  width = 12,
  height = 6,
  dpi = 300
)


# 5. Monthly crime diversity trend ---------------------------------------

monthly_diversity_plot <- ggplot(
  monthly_average,
  aes(
    x = year_month,
    y = mean_diversity
  )
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  geom_smooth(
    method = "loess",
    se = FALSE,
    linewidth = 1
  ) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Monthly Average Crime Diversity",
    x = "Month",
    y = "Average Normalized Shannon Index"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    )
  )

ggsave(
  "output/figures/monthly_diversity_trend.png",
  monthly_diversity_plot,
  width = 9,
  height = 6,
  dpi = 300
)


# 6. Seasonal distribution -----------------------------------------------

seasonal_boxplot <- ggplot(
  monthly_diversity,
  aes(
    x = season,
    y = normalized_shannon
  )
) +
  geom_boxplot(
    outlier.size = 1,
    width = 0.6
  ) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Seasonal Distribution of Crime Diversity",
    x = "Season",
    y = "Normalized Shannon Index"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    )
  )

ggsave(
  "output/figures/seasonal_diversity_boxplot.png",
  seasonal_boxplot,
  width = 8,
  height = 6,
  dpi = 300
)


# 7. Seasonal means and confidence intervals -----------------------------

seasonal_mean_plot <- ggplot(
  season_summary,
  aes(
    x = season,
    y = mean_diversity
  )
) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(
      ymin = mean_diversity - ci_95,
      ymax = mean_diversity + ci_95
    ),
    width = 0.15,
    linewidth = 0.8
  ) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Mean Crime Diversity by Season",
    x = "Season",
    y = "Mean Normalized Shannon Index"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    )
  )

ggsave(
  "output/figures/seasonal_mean_diversity.png",
  seasonal_mean_plot,
  width = 8,
  height = 6,
  dpi = 300
)


# 8. Completion message ---------------------------------------------------

cat("\nVisualization script completed.\n")
cat("Figures saved in output/figures/:\n")
cat(
  paste0(
    "- ",
    list.files("output/figures"),
    collapse = "\n"
  ),
  "\n"
)