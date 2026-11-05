# Spatial material clustering: method and sensitivity assessment

**Project:** EU-MAT – Effective Urban Material Mining in Cities  
**Case study:** Joensuu, Finland  
**Scripts:** `R/02_grid_clusters.R`, `R/02b_cluster_sensitivity.R`  
**Status:** Provisional parameter choice; numerical sensitivity assessed, final map comparison pending  
**Code checked:** 2026-11-05 (`main`, commit `a7a5720`)

## 1. Purpose and method

Spatial clustering identifies concentrations of estimated wood and concrete stocks in Joensuu's existing building stock. The aim is to describe **where material potential is concentrated**; road-network accessibility is examined separately in scripts `03`–`05b`.

The baseline method (`02_grid_clusters.R`) follows these steps:

1. Project buildings to **EPSG:3067**, represent them by internal centroid points and aggregate estimated material stocks to a **100 m grid**.
2. Calculate material sums within a **3 × 3-cell (300 m) moving window**.
3. Identify **seed cells** at or above the 90th percentile and **growth cells** at or above the 85th percentile of positive moving-window sums.
4. Delineate connected growth regions using **eight-direction connectivity**, retaining regions containing at least one seed cell.
5. Sum each region's material from the **original grid values**, rather than overlapping moving-window sums, to avoid double-counting.
6. Classify regions by their material stock and retain those meeting the minimum threshold.

| Class | Wood (t) | Concrete (t) |
| --- | ---: | ---: |
| Below threshold | <500 | <5,000 |
| Significant | 500–<2,000 | 5,000–<20,000 |
| Medium | 2,000–<5,000 | 20,000–<50,000 |
| High | ≥5,000 | ≥50,000 |

The material thresholds are **exploratory descriptive categories**, not validated thresholds for material recovery. The implementation is shared through `R/functions/cluster_functions.R`. Script `02` saves accepted clusters as GeoPackage polygons; cluster-ID rasters also retain seed-connected regions below the minimum material threshold.

## 2. Sensitivity analysis

Script `02b_cluster_sensitivity.R` evaluates **18 scenarios** covering grid size, moving-window width, seed and growth thresholds, connectivity, grid origin and selected combinations. It compares accepted-cluster count, area, stock, density, shape and spatial overlap. The **Jaccard index** measures overlap of the *combined accepted-cluster areas* with the baseline (1 = identical); it does not evaluate one-to-one cluster correspondence. Overlap is approximated using a 25 m comparison grid.

### Baseline and main alternative

| Indicator | Wood: 100 m / 300 m | Wood: 50 m / 250 m | Concrete: 100 m / 300 m | Concrete: 50 m / 250 m |
| --- | ---: | ---: | ---: | ---: |
| Accepted clusters | 15 | 22 | 21 | 29 |
| Accepted cluster area (ha) | 740 | 691.25 | 726 | 668.75 |
| Share of city-wide estimated stock (%) | 49.7 | 50.4 | 50.9 | 51.2 |
| Jaccard vs baseline | 1.00 | 0.74 | 1.00 | 0.72 |
| Largest cluster (ha) | 440 | 392 | 413 | 378 |
| Median cluster area (ha) | 10.0 | 7.5 | 10.0 | 6.3 |

The 50 m / 250 m configuration identifies **more, somewhat smaller clusters**, while capturing a similar share of the estimated city-wide stock. The largest clusters nevertheless remain extensive. Neither configuration is demonstrably superior on these indicators alone.

### Other main findings

- **Spatial scale matters:** the 50 m / 150 m setting produces 32 wood and 39 concrete clusters, whereas the 50 m / 350 m setting produces 13 and 16. Changing grid size with a fixed cell window also changes physical window width, so those effects cannot be isolated in every scenario.
- **Growth threshold matters:** lowering it from 0.85 to 0.75 expands cluster areas substantially (Jaccard 0.65 for wood, 0.63 for concrete). Raising the seed threshold from 0.90 to 0.95 also reduces spatial overlap (0.84 and 0.73).
- **Grid origin matters:** shifting the 100 m grid by 50 m changes the accepted-cluster footprint (Jaccard approximately 0.67–0.77 across the tested shifts).
- **Connectivity has less effect on overall footprint:** four- instead of eight-direction connectivity gives Jaccard approximately 0.99 for both materials, although individual connections may still change.
- **Classification thresholds are comparatively stable:** changing the stock-class limits by ±25% leaves 15 accepted wood clusters; the accepted concrete count remains 21 except at +25%, when it becomes 20. The distribution among classes changes more.

The baseline's largest wood and concrete clusters contain approximately **71% of the accepted material stock** of each material. Their areas (440 and 413 ha) and irregular shapes warrant care when interpreting individual cluster boundaries.

### Pielisjoki example

A preliminary map review found that Joensuu’s large city-centre cluster connects areas on **both sides of the Pielisjoki River**. At 100 m resolution, the connection persists with four- and eight-direction connectivity; the preliminary 50 m / 250 m map review showed a diagonal connection. Some connecting cells have **zero material stock** but qualify through moving-window sums. This illustrates that cluster connectivity describes smoothed spatial concentrations, **not physical continuity or road accessibility**, which is assessed separately in scripts `03`–`05b`.

## 3. Parameter choice and rationale

**Current reference configuration:** 100 m grid, 300 m window, seed quantile 0.90, growth quantile 0.85 and eight-direction connectivity.

This remains a **provisional, pragmatic choice** because it supports city-scale analysis, has been consistently applied across the current workflow and has a documented sensitivity assessment. The 50 m / 250 m alternative produces finer delineation but has not shown a clear advantage in interpreting material concentrations. The sensitivity results do **not** establish an objectively optimal parameter set.

**Before confirming the choice**, compare baseline and 50 m / 250 m cluster maps side by side for wood and concrete, focusing on large regions, narrow connections and whether the delineations are meaningful for assessing material availability. The Pielisjoki case has been reviewed preliminarily, but the systematic map comparison is still pending. Record the outcome before changing script `02` or downstream analyses.

## 4. Interpretation and limitations

- **Input estimates:** Current material-intensity coefficients are provisional and not validated for the entire building stock. The sensitivity analysis tests clustering parameters, **not uncertainty in estimated material quantities**. Updated inputs from Aalto University may change the results.
- **Spatial boundaries:** Cluster polygons depend on grid alignment, neighbourhood size and thresholds. They describe analytical concentrations, **not exact physical or operational boundaries**.
- **Accessibility:** Geographic connection within a cluster is not equivalent to road-network accessibility. That is evaluated separately in scripts `03`–`05b`.
- **Time:** Scripts `06`–`06a` estimate future material potential using a separate preliminary demolition scenario; temporal assumptions are not tested in `02b`.

## 5. Reproducibility and evidence

Run `02b_cluster_sensitivity.R` from the `R/` directory. By default results remain in memory; **set `save_tables <- TRUE` within the script before running it** to export the seven CSV result tables to `R/data/processed/cluster_sensitivity/`. The underlying analysis data and outputs should be stored in an organisation-approved location; add only approved documentation and figures to the public repository. Record the data version, Git commit, R/package versions and the location of the archived results.

**Evidence:** Joensuu 02b outputs previously reviewed on 4 November 2026; Pielisjoki preliminary map observations reviewed on 3 November 2026. The code was cross-checked on 5 November 2026; numerical results were **not independently re-run** during this documentation review.