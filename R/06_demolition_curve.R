# ==============================================================================
# 06_demolition_curve.R
#
# Estimate when wood and concrete currently stored in buildings may become
# available through demolition. Uses an illustrative Weibull lifetime model.
#
# Probabilities are conditional on the building surviving to the baseline year
# (2026). The lifetime parameters have NOT been calibrated to observed
# demolitions. Results are development-stage scenarios, not validated forecasts.
# The model and material intensity estimates will be revised with EU-MAT partner
# methods when available.
#
# Input:
#   data/processed/buildings_material_stock_joensuu.gpkg
#
# Outputs:
#   data/processed/demolition_potential_by_building.csv
#   data/processed/demolition_potential_by_period.csv
#
# Run from R/. The output is also used by 06a_static_cluster_timeline.R.
# ==============================================================================

library(sf)
library(dplyr)

# ==============================================================================
# 1. Model parameters
# ==============================================================================

baseline_year <- 2026
period_breaks <- c(2026, 2030, 2035, 2040, 2045, 2050, 2055)

median_lifetime <- 80
shape <- 3
scale <- median_lifetime / log(2)^(1 / shape)

show_plots <- FALSE

stopifnot(
  length(period_breaks) >= 2,
  all(diff(period_breaks) > 0),
  period_breaks[1] == baseline_year,
  median_lifetime > 0,
  shape > 0
)

# The first period is four years; subsequent periods are five years.
# Keep these boundaries unchanged for comparison with earlier results.

# ==============================================================================
# 2. Read and validate building stock
# ==============================================================================

building_stock <- st_read(
  "data/processed/buildings_material_stock_joensuu.gpkg",
  quiet = TRUE
)

stopifnot(
  nrow(building_stock) > 0,
  all(c("pysyva_rakennustunnus", "building_year",
        "material_wood_t", "material_concrete_t") %in% names(building_stock))
)

# Exclude unknown or implausible construction years and future buildings.
# This is the same selection as in the original script.
buildings <- building_stock |>
  st_drop_geometry() |>
  filter(between(building_year, 1800, baseline_year))

stopifnot(
  nrow(buildings) > 0,
  !anyNA(buildings$pysyva_rakennustunnus),
  !anyDuplicated(buildings$pysyva_rakennustunnus),
  all(is.na(buildings$material_wood_t) | buildings$material_wood_t >= 0),
  all(is.na(buildings$material_concrete_t) | buildings$material_concrete_t >= 0)
)

cat("\nDemolition scenario inputs:\n")
cat("Baseline year:", baseline_year, "\n")
cat("Buildings in source:", nrow(building_stock), "\n")
cat("Buildings included:", nrow(buildings), "\n")
cat("Buildings excluded (year outside 1800–2026 or missing):",
    nrow(building_stock) - nrow(buildings), "\n")
cat("Buildings without wood estimates:", sum(is.na(buildings$material_wood_t)), "\n")
cat("Buildings without concrete estimates:", sum(is.na(buildings$material_concrete_t)), "\n")
cat("Weibull median lifetime:", median_lifetime, "years\n")
cat("Weibull shape:", shape, "\n")

# ==============================================================================
# 3. Conditional demolition probability
# ==============================================================================

# Probability of demolition in [start_year, end_year), conditional on survival
# to baseline_year. The survival factor at the period start avoids counting
# the same building as a new expected demolition in multiple periods.
demolition_probability <- function(building_year, start_year, end_year) {
  hazard <- function(year) {
    (pmax(year - building_year, 0) / scale)^shape
  }
  
  h_baseline <- hazard(baseline_year)
  h_start <- hazard(start_year)
  h_end <- hazard(end_year)
  
  exp(-(h_start - h_baseline)) *
    (-expm1(-(h_end - h_start)))
}

# ==============================================================================
# 4. Building-level material potential by period
# ==============================================================================

demolition_results <- bind_rows(
  lapply(seq_len(length(period_breaks) - 1), function(i) {
    start <- period_breaks[i]
    end <- period_breaks[i + 1]
    
    buildings |>
      transmute(
        pysyva_rakennustunnus,
        building_year,
        period = paste0(start, "-", end),
        demolition_probability = demolition_probability(
          building_year, start, end
        ),
        expected_wood_t = material_wood_t * demolition_probability,
        expected_concrete_t = material_concrete_t * demolition_probability
      )
  })
)

# ==============================================================================
# 5. Summarise and validate the results
# ==============================================================================

demolition_summary <- demolition_results |>
  group_by(period) |>
  summarise(
    expected_demolitions = sum(demolition_probability),
    wood_t = sum(expected_wood_t, na.rm = TRUE),
    concrete_t = sum(expected_concrete_t, na.rm = TRUE),
    .groups = "drop"
  )

stopifnot(
  nrow(demolition_results) == nrow(buildings) * (length(period_breaks) - 1),
  all(is.finite(demolition_results$demolition_probability)),
  all(between(demolition_results$demolition_probability, 0, 1)),
  all(
    demolition_results |>
      group_by(pysyva_rakennustunnus) |>
      summarise(p = sum(demolition_probability), .groups = "drop") |>
      pull(p) <= 1 + 1e-10
  ),
  all(demolition_summary$wood_t >= 0),
  all(demolition_summary$concrete_t >= 0)
)

cat("\nExpected demolition material potential by period (illustrative):\n")
print(demolition_summary)

# ==============================================================================
# 6. Save results
# ==============================================================================

write.csv(
  demolition_results,
  "data/processed/demolition_potential_by_building.csv",
  row.names = FALSE
)

write.csv(
  demolition_summary,
  "data/processed/demolition_potential_by_period.csv",
  row.names = FALSE
)

cat("\nDemolition scenario outputs saved to data/processed/.\n")

# ==============================================================================
# 7. Optional plots
# ==============================================================================

if (show_plots) {
  if (!requireNamespace("ggplot2", quietly = TRUE) ||
      !requireNamespace("tidyr", quietly = TRUE)) {
    stop("Install ggplot2 and tidyr to display plots.")
  }
  library(ggplot2)
  library(tidyr)
  
  # Weibull survival curve for buildings of different ages.
  print(
    ggplot(
      data.frame(
        age = 0:150,
        survival_pct = 100 * pweibull(
          0:150, shape = shape, scale = scale, lower.tail = FALSE
        )
      ),
      aes(age, survival_pct)
    ) +
      geom_line(linewidth = 1) +
      geom_vline(xintercept = median_lifetime, linetype = "dashed") +
      labs(
        title = "Weibull building survival curve",
        x = "Building age (years)",
        y = "Surviving buildings (%)"
      ) +
      theme_minimal()
  )
  
  # Expected wood and concrete releases by period.
  print(
    demolition_summary |>
      pivot_longer(
        cols = c(wood_t, concrete_t),
        names_to = "material",
        values_to = "tonnes"
      ) |>
      ggplot(aes(period, tonnes, fill = material)) +
      geom_col(position = "dodge") +
      labs(
        title = "Expected demolition material potential",
        x = "Period",
        y = "Expected material (tonnes)",
        fill = "Material"
      ) +
      theme_minimal()
  )
}
