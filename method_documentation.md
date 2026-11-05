# Spatial material clustering: method, sensitivity and parameter decisions

**Project:** EU-MAT – Effective Urban Material Mining in Cities  
**Implementation:** Joensuu GIS workflow (`R/02_grid_clusters.R`, `R/02b_cluster_sensitivity.R`)  
**Status:** Working method and **provisional parameter selection**; preliminary Pielisjoki map review documented, systematic cartographic comparison and independent validation still pending  
**Date:** 2026-11-04

## 1. Purpose and scope

This component identifies **spatial concentrations of estimated material stocks** in the existing building stock to support assessment of **material availability and accessibility** at the urban scale.

The current case estimates wood and concrete stocks for Joensuu. Input material-intensity (MI) values are provisional and do not represent validated quantities for the full building stock. Aalto University's updated MI methodology is expected to replace or revise these inputs. Therefore, sensitivity results evaluate the **spatial algorithm conditional on the current material-stock estimates**, not uncertainty in the MI estimates themselves.

## 2. Method implemented in script 02

1. Building materials are aggregated into an aligned **100 m** raster grid (ETRS-TM35FIN, EPSG:3067).
2. A **3 × 3 cell moving window** calculates local material sums (a 300 m-wide neighbourhood).
3. **Seed cells** are identified at or above the **90th percentile** of *positive* local sums.
4. **Growth cells** are identified at or above the **85th percentile** of positive local sums. Adjacent growth cells form connected regions using **eight-direction connectivity**.
5. A connected region is retained if it contains at least one seed cell.
6. For each region, the material stock is summed from the **original raster material values**, not from overlapping moving-window sums. This avoids double-counting.
7. The resulting regions are classified using *exploratory* material-stock thresholds. Regions below the minimum threshold are excluded from the accepted polygon outputs, but original cluster-ID rasters retain them.

| Class | Wood (tonnes) | Concrete (tonnes) |
| --- | ---: | ---: |
| Below threshold | < 500 | < 5,000 |
| Significant | 500–<2,000 | 5,000–<20,000 |
| Medium | 2,000–<5,000 | 20,000–<50,000 |
| High | >= 5,000 | >= 50,000 |

The thresholds are **descriptive stock categories**, not validated criteria for material recovery or reuse.

**Implementation references:** `R/functions/cluster_functions.R` (algorithm); `R/02_grid_clusters.R` (parameters and accepted outputs); `R/02b_cluster_sensitivity.R` (sensitivity evaluation); `R/functions/cluster_metrics.R` (geometry metrics).

## 3. Evaluation design

Eighteen scenarios assess grid size, moving-window width, seed/growth quantiles, four-/eight-direction connectivity, grid origin shifts and selected parameter combinations. Evaluated indicators include:

- Count and total area of accepted clusters;
- Estimated material stock and share of city-wide estimated stock within accepted clusters;
- Material density (tonnes per hectare);
- Spatial overlap with the baseline as the **Jaccard index** (intersection area / union area);
- Selected cluster geometry metrics: maximum extent and compactness;
- Sensitivity of stock-based classification thresholds to +/-25% changes.

**Comparability caveats:** When grid resolution changes with a constant 3 × 3 cell window, physical neighbourhood width also changes. Such scenarios therefore **do not isolate grid resolution**. Jaccard is calculated by nearest-neighbour alignment to a **25 m comparison grid**, so boundaries are approximate. It compares the combined **accepted-cluster footprint**, not one-to-one cluster identity, splitting or merging.

## 4. Main baseline versus alternative comparison

The working baseline is **100 m grid / 300 m window**. The principal alternative examined here is **50 m grid / 250 m window** (five-by-five cells, growth quantile 0.85).

| Indicator | Wood: baseline | Wood: 50 m / 250 m | Concrete: baseline | Concrete: 50 m / 250 m |
| --- | ---: | ---: | ---: | ---: |
| Accepted clusters | 15 | 22 | 21 | 29 |
| Accepted-cluster area (ha) | 740.00 | 691.25 | 726.00 | 668.75 |
| City-stock share in accepted clusters (%) | 49.73 | 50.43 | 50.91 | 51.21 |
| Jaccard vs baseline | 1.000 | 0.741 | 1.000 | 0.718 |
| Largest cluster area (ha) | 440.00 | 391.75 | 413.00 | 377.75 |
| Median accepted-cluster area (ha) | 10.00 | 7.50 | 10.00 | 6.25 |
| Median material density (t/ha) | 123.03 | 144.03 | 1,211.08 | 1,323.81 |
| Median compactness | 0.540 | 0.393 | 0.491 | 0.458 |

*Results: existing Joensuu 02b run. Values are rounded for reporting and are not independently re-computed in this document.*

**Interpretation:** The 50 m / 250 m setting produces more, generally smaller accepted regions while retaining approximately the same share of total estimated city material stock. However, the largest clusters remain extensive (approximately 392 ha for wood and 378 ha for concrete). The alternative is therefore **not demonstrated to resolve the issue of very large, irregular concentrations**. Its Jaccard indices also indicate appreciable differences in spatial footprint, despite comparable total stock coverage.

For context, the **50 m / 150 m** setting produces 32 wood and 39 concrete accepted clusters and has lower Jaccard overlap (0.534 and 0.520). The **50 m / 350 m** setting produces 13 wood and 16 concrete accepted clusters (Jaccard 0.773 and 0.753). These results illustrate the effect of spatial aggregation scale, not a simple ranking of better and worse methods.

## 5. Other sensitivity findings

| Parameter change from baseline | Jaccard wood | Jaccard concrete | Interpretation |
| --- | ---: | ---: | --- |
| Seed quantile 0.90 -> 0.875 | 0.969 | 0.971 | Nearly unchanged combined footprint |
| Seed quantile 0.90 -> 0.95 | 0.835 | 0.730 | More selective seed definition; only five accepted clusters per material |
| Growth quantile 0.85 -> 0.80 | 0.789 | 0.771 | Growth regions expand |
| Growth quantile 0.85 -> 0.75 | 0.655 | 0.627 | Growth regions expand considerably |
| Connectivity 8 -> 4 directions | 0.985 | 0.985 | Small impact on combined accepted footprint in this dataset |
| Grid origin shifted 50 m in X | 0.757 | 0.744 | Material impact from grid alignment |
| Grid origin shifted 50 m in Y | 0.774 | 0.749 | Material impact from grid alignment |
| Grid origin shifted 50 m in both | 0.682 | 0.669 | Material impact from grid alignment |

The growth threshold and spatial scale substantially affect cluster extent. Grid origin also affects the mapped footprint, so cluster boundaries **must not be presented as precise naturally occurring boundaries**. Four- versus eight-direction connectivity has a smaller effect on total accepted footprint in this dataset, but footprint overlap alone does not exclude differences in individual cluster shape or fragmentation.

Changing material classification thresholds by +/-25% retains 15 accepted wood clusters and changes accepted concrete clusters from 21 to 20 only at +25%. The distribution among the descriptive stock classes changes more clearly.

### Dominance of large clusters

Under the baseline, the largest accepted wood cluster contains approximately **91,798 t** across **440 ha** (70.7% of accepted wood stock), and the largest concrete cluster approximately **948,934 t** across **413 ha** (70.8% of accepted concrete stock). Their compactness values are approximately **0.10** and **0.14**, indicating irregular shapes. This may reflect large connected urban concentrations, linkage through narrow qualifying corridors, or both. **Map-based inspection is needed before attributing a physical interpretation.**

### Case observation: Pielisjoki and the large Joensuu city-centre cluster

A preliminary map review on **3 November 2026** raised a concrete interpretive issue: the large city-centre material-stock cluster extends across **both banks of the Pielisjoki River**. Although its mapped footprint is connected, the river is a physical separation between the areas. The cluster should therefore **not** be interpreted as a single physically continuous area of material availability or as a single logistics/service area.

The review compared the baseline **100 m grid / 3 × 3 cells (300 m) / eight-direction connectivity**, the same 100 m setting with **four-direction connectivity**, and the alternative **50 m grid / 5 × 5 cells (250 m) / eight-direction connectivity**:

- **100 m, eight- versus four-direction connectivity:** Both versions still connected the areas on opposite banks. Changing connectivity alone did **not** separate the Pielisjoki-spanning cluster at this resolution.
- **50 m / 250 m, eight-direction connectivity:** The areas on opposite banks were still connected, but the observed connection consisted of **diagonally touching cells**. Four-direction connectivity might split that connection; this was an inference from the map, **not a separately verified 50 m / four-direction result**.
- **Zero-stock cells:** The map showed cells with **zero underlying material stock** forming part of the connection across the river. Because cluster growth is determined from **moving-window material sums**, a cell can meet the growth threshold even when its own material-stock value is zero. In this case, neighbourhood smoothing produces a *computational bridge* between material concentrations on opposite banks.

**Methodological interpretation:** This is an expected possible consequence of the chosen spatial smoothing and connected-region delineation, **not evidence that the river can be crossed directly or that the entire cluster functions as one accessible area**. It also explains why the high overall Jaccard similarity between four- and eight-direction connectivity does not settle the issue of a specific cluster's internal linkage.

**Decision for the current method:** Do **not** introduce the Pielisjoki or other physical obstacles as hard barriers in the baseline clustering algorithm. The clustering stage is intended to describe **spatial concentrations of estimated material stocks**. Physical travel connections and reachability are assessed **separately** through the road-network accessibility component (`03`–`05b`). Keep the Pielisjoki connection as a documented limitation and a case example when interpreting large or irregular clusters. This is a scope decision, not a claim that the computed boundary is geographically exact.

**Evidence status:** These are observations from a preliminary visual review discussed on 3 November 2026. The comparison map or screenshot has **not yet been archived in this document**; retain an approved annotated map as evidence if available.

## 6. Parameter decision and rationale

**Provisional decision:** Retain the **100 m grid / 300 m window / seed 0.90 / growth 0.85 / eight-direction connectivity** as the current reference configuration.

**Reasons:**

1. It provides a consistent, already tested baseline across the stock clustering and subsequent availability/accessibility analyses.
2. It identifies broad urban material concentrations relevant to exploratory city-scale assessment.
3. The sensitivity analysis describes its limitations explicitly; it does **not** establish a unique statistically optimal parameter set.
4. The 50 m / 250 m alternative changes regional delineation and yields more clusters, but does not yet provide demonstrated superiority in the absence of independent geographic reference evidence or an explicit target spatial scale.
5. The Pielisjoki case demonstrates a smoothing-induced connection across a physical barrier. Since clusters describe spatial material concentrations rather than physically connected logistics areas, this observation calls for an explicit interpretation warning, not automatically for a barrier-constrained algorithm.

**Decision status:** This is a pragmatic working choice, **not scientific proof that the baseline is optimal**. Revisit it only if cartographic review, case-study evidence or a clarified intended spatial scale gives a convincing reason.

## 7. Limited remaining review before finalising the decision

Prepare **two side-by-side map comparisons** (wood and concrete) showing accepted clusters for the baseline and 50 m / 250 m alternative over a suitable neutral reference map. Examine the largest regions and areas where the delineations diverge. Specifically record:

- Whether large clusters connect visibly distinct concentrations through narrow corridors, especially across the Pielisjoki;
- Whether zero-stock cells in those corridors are retained because of moving-window smoothing;
- Whether the alternative yields more interpretable areas for the project's **material availability and accessibility** objective;
- Whether either approach introduces small scattered regions of doubtful planning relevance;
- Whether the map interpretation agrees with the area, density and Jaccard indicators.

**Do not change script 02 on the basis of numerical sensitivity results alone.** If the map review favours a new baseline, record the justification, update downstream scripts intentionally and repeat the regression/consistency tests.

**Map review status:** The **Pielisjoki case has undergone preliminary visual inspection (3 November 2026)**, with the observations recorded in Section 5. The planned **systematic side-by-side comparison of the full baseline and alternative cluster maps remains incomplete**. Archive the approved comparison maps and add a dated final decision note after that review.

## 8. Limitations and correct interpretation

The results should be interpreted with the following qualifications:

- **Input uncertainty:** Provisional MI coefficients and missing building information affect the material-stock estimates. Script 02b evaluates clustering parameters, not these input uncertainties.
- **Spatial interpretation:** Cluster polygons are analytical concentrations, not exact boundaries of individual material sources. Their shapes depend on grid and threshold choices, and moving-window smoothing can connect concentrations across physical barriers (Section 5).
- **Availability and accessibility:** Spatial cluster connectivity is distinct from road-network accessibility, assessed separately in scripts 03–05b. Neither a cluster boundary nor a distance-based facility assignment represents an observed material flow.
- **Temporal interpretation:** Scripts 06–06a estimate potential material release using a provisional demolition-lifetime model; these scenarios are separate from the current-stock clustering.

## 9. Reproducibility

To rerun the evaluation from `R/`:

```r
# In R/02b_cluster_sensitivity.R set:
save_tables <- TRUE

source("02b_cluster_sensitivity.R")
```

This saves scenario definitions and summary tables under `data/processed/cluster_sensitivity/` according to the current script. **Do not publish source spatial datasets or derived tables/maps to a public repository without confirming their data-use and organisational permissions.** Store restricted analysis data and full results in an organisation-approved location. Use GitHub for approved code and methodological documentation.

Record the data version/date, script commit identifier, R and package versions, and the approved storage location of the input data and saved tables. See `README.md` for the analysis workflow and run order.

## 10. Decision record

| Date | Decision | Basis | Revisit when |
| --- | --- | --- | --- |
| 2026-11-03 | Document Pielisjoki as a **smoothing-induced cluster connection**; do not encode river barriers into baseline clustering | Preliminary map comparison: 100 m clusters remained connected with 4 or 8 directions; 50 m / 250 m connection was diagonal; zero-stock cells bridged the river | Systematic cartographic comparison and any future change to the meaning of a cluster |
| 2026-11-04 | Keep 100 m / 300 m as a **provisional** reference setting | Reviewed 02b numerical sensitivity; no verified independent criterion for preferring another setting | Baseline versus 50 m / 250 m map comparison, case-study validation, or revised MI data |

**Evidence source:** outputs of `02b_cluster_sensitivity.R` provided during the Joensuu methodological review. Numerical output should be archived in the approved project location with the analysis provenance before treating this document as the final record.
