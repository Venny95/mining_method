
# ==============================================================================
# 06_demolition_curve.R
#
# Estimate future demolition material potential using a Weibull lifetime model.
# Probabilities are conditional on survival to the baseline year.
# Model parameters are illustrative and not calibrated to Finnish data.
#
# Outputs:
#   data/processed/demolition_potential_by_building.csv
#   data/processed/demolition_potential_by_period.csv
# ==============================================================================

library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)


# ==============================================================================
# 1. Define model parameters
# ==============================================================================

baseline_year <- 2026
period_breaks <- c(2026, 2030, 2035, 2040, 2045, 2050, 2055)

median_lifetime <- 80
shape <- 3
scale <- median_lifetime / log(2)^(1 / shape)


# ==============================================================================
# 2. Read building stock
# ==============================================================================

buildings <- st_read(
  "data/processed/buildings_material_stock_joensuu.gpkg",
  quiet = TRUE
) |>
  st_drop_geometry() |>
  filter(between(building_year, 1800, baseline_year))

cat("Buildings included:", nrow(buildings), "\n")

# Check permanent building identifiers
stopifnot(
  !anyNA(buildings$pysyva_rakennustunnus),
  !anyDuplicated(buildings$pysyva_rakennustunnus)
)


# ==============================================================================
# 3. Calculate demolition probabilities
# ==============================================================================

# Conditional probability of demolition during a period
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
# 4. Estimate demolition material potential
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
# 5. Summarise and validate results
# ==============================================================================

demolition_summary <- demolition_results |>
  group_by(period) |>
  summarise(
    expected_demolitions = sum(demolition_probability),
    wood_t = sum(expected_wood_t, na.rm = TRUE),
    concrete_t = sum(expected_concrete_t, na.rm = TRUE),
    .groups = "drop"
  )

print(demolition_summary)

# Check period probabilities and cumulative probability per building
stopifnot(
  all(between(demolition_results$demolition_probability, 0, 1)),
  all(
    demolition_results |>
      group_by(pysyva_rakennustunnus) |>
      summarise(p = sum(demolition_probability), .groups = "drop") |>
      pull(p) <= 1 + 1e-10
  )
)


# ==============================================================================
# 6. Visualise results
# ==============================================================================

# Weibull survival curve
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

# Expected material quantities by period
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


# ==============================================================================
# 7. Save results
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


