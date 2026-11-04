# Urban Material Mining Method (EU-MAT)

**Status:** Research and development workflow, currently implemented for the Joensuu case study. The methods and results are preliminary and subject to further evaluation.

## Overview

This repository contains an **R-based GIS analytical workflow** developed within **EU-MAT: Effective Urban Material Mining in Cities**. The workflow combines building-level material stock estimation, spatial aggregation and clustering, and road-network accessibility analysis to support urban circular economy planning. A preliminary temporal analysis is also included to explore the potential future release of materials from demolition.

The current implementation focuses on **wood and concrete** in the existing building stock of **Joensuu, Finland**. The method is intended to be applicable to other cities, subject to the availability and harmonisation of building, material, road-network and facility data. Transferability will be evaluated through applications in other EU-MAT case-study municipalities.

The repository contains the **GIS analysis component** of the wider Urban Material Mining Method developed in EU-MAT Work Package 2 (WP2). Other WP2 activities include building-specific case studies, material intensity methodology development, machine-learning research and municipal pilot testing.

## Methodological framework

The workflow examines four related dimensions of urban material mining potential:

1. **Material stock:** Estimated quantities of materials embedded in the existing building stock.
2. **Spatial concentration:** The distribution of material stocks and their concentrations within geographically connected areas.
3. **Accessibility:** Road-network distances between material sources and potential recovery, processing or reuse facilities.
4. **Temporal material potential:** Scenario-based estimates of when materials embedded in buildings may become available through demolition.

The main analytical components are connected as follows:

```text
Building stock data + material intensity coefficients
                         |
                         v
             Building-level material stocks
                         |
            +------------+-------------+
            |                          |
            v                          v
     Spatial aggregation        Road-network data
       and clustering           + facility locations
            |                          |
            +------------+-------------+
                         |
                         v
           Spatial and accessibility results
                         |
                         v
             Planning-oriented interpretation

Building-level material stocks
            |
            v
  Preliminary demolition model
            |
            v
  Temporal material potential within
     fixed material-stock clusters
```

The resulting quantities describe **estimated material potential**, not confirmed recoverable material flows. The current workflow does not establish material condition, technical recoverability, future demand or economic feasibility. Its outputs are intended as spatial information for planning and further assessment, not as definitive decisions on recovery or facility locations.

## Material intensity coefficients and demolition modelling

### Material intensity (MI) coefficients

The current development version estimates building-level wood and concrete stocks using material intensity (MI) coefficients from Nasiri et al. (2023). These coefficients were developed for wooden residential buildings in Finland and cover a limited range of construction periods. **They are therefore not directly applicable to the entire building stock.**

In the current implementation, the coefficients are nevertheless applied to the wider building stock, with provisional assumptions for construction decades not covered by the original study. This simplified approach is used solely to develop and test the GIS-based analytical workflow until a more comprehensive material estimation methodology becomes available. Consequently, the resulting material stock estimates should be interpreted as preliminary and are not considered validated estimates of the city's actual material stocks.

**Aalto University is developing an updated material intensity estimation methodology within the EU-MAT project.** The current coefficients and provisional assumptions will be replaced or revised using Aalto University's methodology once it becomes available.

**Reference**

Nasiri, F., Kaasalainen, T., & Hughes, M. (2023). *Estimating the material intensity of wooden residential houses in Finland*. Resources, Conservation and Recycling, 198, 107142. https://doi.org/10.1016/j.resconrec.2023.107142

### Demolition curve

The current temporal development version uses an **illustrative Weibull building-lifetime model** to estimate the expected release of materials during future periods. The calculation is conditional on a building having survived to the baseline year (2026). The current illustrative parameters are a median lifetime of 80 years and a shape parameter of 3.

These parameters **have not been calibrated to observed Finnish demolition events**. The outputs therefore represent development-stage scenarios of expected material potential, not validated demolition forecasts.

**Aalto University is also developing a demolition curve within EU-MAT.** The Weibull model in this repository is a **temporary implementation** used to test the temporal GIS workflow. It is intended to be replaced or updated when Aalto University's demolition modelling method becomes available.

## Analytical workflow

The scripts currently included in the repository are organised as follows. All paths below are relative to the `R/` directory.

| Script | Purpose | Role |
| --- | --- | --- |
| `01_prepare_buildings.R` | Prepare building data and estimate wood and concrete material stocks using provisional MI coefficients | Core |
| `02_grid_clusters.R` | Aggregate material stocks to a grid and identify spatially connected material concentrations | Core |
| `functions/cluster_functions.R` | Shared calculation for spatial clustering | Core function |
| `02b_cluster_sensitivity.R` | Evaluate the effects of grid resolution, neighbourhood size, quantile thresholds, connectivity and grid origin | Method evaluation |
| `functions/cluster_metrics.R` | Calculate cluster-level area, density, spatial extent and shape metrics | Evaluation function |
| `03_prepare_network.R` | Prepare and weight an OpenStreetMap road network for heavy goods vehicle (HGV) routing analysis | Core |
| `04_building_accessibility.R` | Calculate road-network distances from buildings to the nearest eligible wood and concrete receiving facilities | Core |
| `05_cluster_accessibility.R` | Summarise building-level accessibility at the material-cluster level | Core |
| `05b_cluster_facility_allocation.R` | Summarise how material stocks within clusters are allocated to receiving facilities based on network distances | Supplementary |
| `06_demolition_curve.R` | Estimate preliminary building-level and period-specific demolition material potential | Temporal development |
| `06a_static_cluster_timeline.R` | Quantify expected temporal material potential within fixed, stock-based clusters | Temporal development |

The numerical ordering reflects the principal analytical workflow. The sensitivity, facility-allocation and temporal scripts are complementary analyses and do not all need to be run for every application.

### Running the Joensuu workflow

1. Install R and the packages used by the scripts, including `sf`, `terra`, `dplyr`, `tidyr`, `dodgr`, `ggplot2` and `mapview`.
2. Prepare the required local datasets described in the [Input data](#input-data) section. **Input datasets are not distributed in this public repository.**
3. Set the working directory to the repository's `R/` folder. The scripts currently use paths relative to this directory.
4. Run the core workflow in order:

   ```r
   source("01_prepare_buildings.R")
   source("02_grid_clusters.R")
   source("03_prepare_network.R")
   source("04_building_accessibility.R")
   source("05_cluster_accessibility.R")
   ```

5. Optionally run `02b_cluster_sensitivity.R` to examine methodological sensitivity, or `05b_cluster_facility_allocation.R` for further facility-allocation summaries, after their required inputs have been generated.
6. For the preliminary temporal analysis within fixed clusters, run the following after `01` and `02`:

   ```r
   source("06_demolition_curve.R")
   source("06a_static_cluster_timeline.R")
   ```

The workflow is under active development. A fully reproducible release, including recorded R package versions, automated execution checks and more general configuration of city-specific inputs, has not yet been completed.

## Input data

### Building stock data

The Joensuu case application uses building information from the **Finnish Building Stock Data (RYHTI)**. The relevant attributes include building locations, completion dates, gross floor areas and demolition records. Building location and attributes are combined with MI coefficients to estimate material stocks.

Building data completeness and classifications differ between countries. Applications elsewhere may require harmonised building types, dates, floor-area variables, material-intensity assumptions and identifiers, or a reduced scope where specific attributes are missing.

### Road-network data

**OpenStreetMap (OSM)** road data are used for network preparation, routing and accessibility analysis. The Joensuu implementation uses road geometries and road classes to build a weighted network with `dodgr` and its HGV profile. Network distances are used to assess proximity between buildings and potential material destinations.

### Recovery, processing and reuse facilities

**Project-specific facility data** describe the locations of potential receiving or processing facilities and which material fractions they accept. Wood and concrete are assessed separately because the relevant receiving facilities may differ between materials. Facility locations are used as potential destinations in accessibility analyses; inclusion does not establish future availability or the suitability of a particular recovery operation.

### Local input files

The current Joensuu scripts expect these files relative to the `R/` directory:

| Input path | Used by | Contents |
| --- | --- | --- |
| `data/processed/ryhti_buildings_joensuu.gpkg` | `01` | Building geometries and attributes for material stock estimation |
| `data/processed/OSM_roads_joensuu_small.gpkg` | `03` | Road geometry and road class (`fclass`) |
| `data/processed/existing_facilities_joensuu.gpkg` | `04` | Facility locations, names and material acceptance information |

The analytical scripts generate intermediate and output files in `data/processed/`, including GeoPackage (`.gpkg`), GeoTIFF (`.tif`), RDS (`.rds`) and CSV (`.csv`) files. The `.gitignore` excludes local analysis data and common spatial data formats. Data access and distribution must follow the applicable project and source restrictions.

The current Finnish implementation uses **ETRS-TM35FIN (EPSG:3067)** for spatial analyses. Applying the method in another city requires an appropriate local projected coordinate reference system and consistent spatial units.

## Spatial material clustering

`02_grid_clusters.R` uses `calculate_clusters()` from `functions/cluster_functions.R`. The current method:

1. Aggregates building-level material stock estimates to a **100 m grid**.
2. Calculates local material sums using a **3 × 3-cell moving window** (300 m neighbourhood).
3. Defines seed cells at or above the **90th percentile** of positive local material sums.
4. Identifies connected areas at or above the **85th percentile**, retaining only areas that contain seed cells. The default is **eight-neighbour connectivity**.
5. Quantifies each resulting cluster using the **original grid material stock values**, rather than the overlapping moving-window sums, to avoid double-counting.
6. Classifies clusters by material-specific stock thresholds. The present thresholds are **exploratory** rather than validated recovery or processing requirements.

This can be described as **moving-window aggregation combined with dual-threshold (hysteresis-style) region growing and connected-component delineation**. The grid resolution, window size, quantile thresholds and connectivity are methodological choices, not universal parameters. Nearby concentrations can be connected through narrow corridors of qualifying cells.

`02b_cluster_sensitivity.R` evaluates the influence of selected parameter choices on cluster counts, material quantities, spatial extent and related metrics. The results should be interpreted together with the assumptions of the MI estimates.

## Road-network accessibility and facility allocation

The road-network component links building-level material stocks and cluster locations with potential material-receiving facilities:

- `03_prepare_network.R` constructs the weighted road network from OSM data.
- `04_building_accessibility.R` calculates building-level road-network distances to the nearest reachable, material-specific receiving facility.
- `05_cluster_accessibility.R` aggregates the building-level results to material clusters, allowing the accessibility of spatial material concentrations to be compared.
- `05b_cluster_facility_allocation.R` provides supplementary summaries of how material quantities within clusters are assigned to receiving facilities based on the calculated routes.

These analyses indicate **potential transport distances and spatial relationships**, not actual future material deliveries, costs or verified facility capacity.

## Temporal material potential: fixed clusters (analysis A)

`06_demolition_curve.R` estimates period-specific expected demolition material potential at the building level using the provisional Weibull model. The current period boundaries are **2026, 2030, 2035, 2040, 2045, 2050 and 2055** (the first interval is four years; subsequent intervals are five years).

`06a_static_cluster_timeline.R` keeps the original material-stock cluster boundaries fixed and calculates expected demolition material quantities within them for each period. This enables a comparison of potential future material release across existing concentrations of wood and concrete stocks.

This analysis describes **temporal potential within fixed material-stock clusters**. It does not identify new clusters independently for each period and should not be presented as a validated forecast. The estimates will need to be revisited when the demolition model being developed at Aalto University is available.

## Repository structure

```text
mining_method/
├── R/
│   ├── 01_prepare_buildings.R
│   ├── 02_grid_clusters.R
│   ├── 02b_cluster_sensitivity.R
│   ├── 03_prepare_network.R
│   ├── 04_building_accessibility.R
│   ├── 05_cluster_accessibility.R
│   ├── 05b_cluster_facility_allocation.R
│   ├── 06_demolition_curve.R
│   ├── 06a_static_cluster_timeline.R
│   └── functions/
│       ├── cluster_functions.R
│       └── cluster_metrics.R
├── frontend/
│   ├── index.html
│   ├── script.js
│   └── style.css
├── README.md
└── .gitignore
```

## Visualisation and demo development

The `frontend/` folder contains an **early MapLibre and JavaScript map-visualisation prototype**. It is separate from the current R-based analytical calculations and is not a completed end-user interface.

A future demo interface is envisaged to allow users to explore buildings, view building-specific material estimates and examine routes or distances to potential material-processing or receiving facilities. Further development of visualisation and user-interface functionality will depend on testing and feedback within the project.

## Development and evaluation priorities

The ongoing work includes:

- documenting the analytical workflow and evaluating the sensitivity of its spatial parameters;
- comparing preliminary MI-based material estimates with building-specific case-study evidence when available;
- integrating **Aalto University's updated MI methodology and demolition curve** when these are ready;
- linking spatial material potential, transport-network analysis and planning-oriented interpretation;
- evaluating applicability with datasets from **Skellefteå (Sweden)** and **Sortland (Norway)** and gathering municipal feedback; and
- preparing analyses and outputs for further GIS, visualisation and decision-support applications.

Possible subsequent extensions include additional temporal clustering comparisons, more detailed transport or facility-location scenarios, and improvements to the demonstration interface. These are **development directions**, not implemented core features of the current repository.

The broader WP2 also includes **machine-learning development with UiT** and building-specific case studies. These activities are connected to the mining method but are not implemented by the R scripts documented here.

## Project

**EU-MAT: Effective Urban Material Mining in Cities**  
**Programme:** Interreg Aurora  
**Project duration:** 1 September 2025 – 31 August 2028

The project brings together partners from **Finland, Sweden and Norway** to develop methods and applications supporting the identification, recovery and reuse of materials embedded in the built environment. **Karelia University of Applied Sciences** is responsible for the GIS-based development of the material mining workflow in cooperation with the project partners.