# rakennukset
# ruudut
# klusteripolygonit
# tulostaulukot
# MapLibre-aineistot

library(ggplot2)
library(scales)

scenario_palette <- c(
  "below threshold" = "#D9D9D9",
  "low" = "#FEE08B",
  "medium" = "#FC8D59",
  "high" = "#D73027"
)

wood_plot <- ggplot() +
  
  # Cluster classification as the background colour
  geom_sf(
    data = wood_results$clusters,
    aes(fill = material_scenario),
    color = "grey20",
    linewidth = 0.5
  ) +
  
  # Grid-cell material quantities as a transparent dark overlay
  geom_sf(
    data = wood_results$high_cells,
    aes(alpha = material_wood_t),
    fill = "black",
    color = "white",
    linewidth = 0.1
  ) +
  
  scale_fill_manual(
    values = scenario_palette,
    name = "Cluster material\nscenario",
    drop = FALSE
  ) +
  
  scale_alpha_continuous(
    range = c(0.03, 0.55),
    trans = "sqrt",
    labels = label_number(
      big.mark = " ",
      accuracy = 1
    ),
    name = "Wood in grid\ncell (tonnes)"
  ) +
  
  coord_sf(datum = NA) +
  
  labs(
    title = "Spatial concentrations of wood stock",
    subtitle = "Cluster classification and grid-cell material quantities"
  ) +
  
  theme_void() +
  
  theme(
    plot.title = element_text(
      size = 12,
      face = "bold"
    ),
    plot.subtitle = element_text(size = 9),
    legend.position = "right",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8)
  )

wood_plot