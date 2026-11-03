
**This README file is a work in progress and will be updated continuously as development proceeds.**

# OVERVIEW: URBAN MATERIAL MINING METHOD

This repository contains the **R-based analytical workflow** for the Urban Material Mining Method developed within the **EU-MAT: Effective Urban Material Mining in Cities** project.

The method supports the identification and spatial assessment of materials embedded in the existing building stock. It combines building-level material stock estimation, spatial aggregation, material concentration analysis and network-based accessibility analysis.

The current workflow focuses on concrete and wood. The method is initially developed and tested using building stock data for **Joensuu, Finland**. Its transferability to other municipal and national data contexts will be assessed during the project.

The EU-MAT project is implemented from 1 September 2025 to 31 August 2028. Karelia University of Applied Sciences is responsible for the spatial development of the material mining method in cooperation with the project partners.

## METHODOLOGICAL FRAMEWORK

The method currently distinguishes between three related dimensions of urban mining potential:

1. **Material availability**: The estimated quantity of material embedded in the building stock.
2. **Spatial concentration**: The extent to which material stocks are concentrated within spatially connected areas.
3. **Accessibility**: The network-based distance from material sources to potential recovery, processing or reuse locations.

A fourth dimension, **temporal availability**, is planned to estimate when materials may become available through future demolition.

The current analytical workflow is:

```text
Building stock data
    ↓
Building-level material stock estimation
    ↓
Spatial aggregation
    ↓
Material concentration and cluster analysis
    ↓
Network-based accessibility analysis
    ↓
Planning-oriented interpretation
```

The current results describe estimated material stocks embedded in the existing building stock and their potential transport destinations. They should not be interpreted directly as recoverable material flows unless information about demolition timing, material condition, technical recoverability and other relevant constraints is available.

## CURRENT WORKFLOW

The R workflow is organised into the following scripts.

**01: Building data and material stock preparation**  
*01_prepare_buildings.R* prepares building stock data and calculates preliminary building-level estimates of concrete and wood stocks using material intensity (MI) coefficients.

**02: Spatial aggregation and material clusters**  
*02_grid_clusters.R* aggregates building-level material estimates into spatial units and identifies spatially connected concentrations of concrete and wood.

*02b_cluster_sensitivity.R* evaluates the sensitivity of clustering results to methodological parameters, including grid resolution, neighbourhood size, quantile thresholds, connectivity and grid origin.

**03: Road-network preparation**  
*03_prepare_network.R* prepares and weights the OpenStreetMap road network for heavy goods vehicle routing.

**04: Building-level accessibility**  
*04_building_accessibility.R* calculates road-network distances from buildings to eligible material-specific receiving facilities.

**05: Cluster-level accessibility and facility allocation**  
*05_cluster_accessibility.R* aggregates building-level accessibility and transport estimates to material clusters.

*05b_cluster_facility_allocation.R* analyses how material stocks within clusters are allocated to receiving facilities based on building-level road-network distances.

**06: Results visualisation**  
*06_export_results.R* contains preliminary visualisation of material clusters and their classifications. Further development of result exports and visualisations is planned.

### Material intensity (MI) calculation

Material intensity coefficients are used to estimate concrete and wood quantities at the building level. The development of the material intensity methodology is led by **Aalto University** within the EU-MAT project.

A preliminary foundation for the approach is presented in:

Nasiri, F., Kaasalainen, T., Hughes, M. (2023).  
*Estimating the material intensity of wooden residential houses in Finland*.  
Resources, Conservation and Recycling, Volume 198, 107142.  
https://doi.org/10.1016/j.resconrec.2023.107142

Further development of the MI coefficients method is ongoing, with the aim of extending it to a broader range of building types across the building stock. An updated and more comprehensive MI coefficients method will be integrated into the Urban Material Mining Method as the project progresses.

### Demolition curve

The development of a demolition curve to estimate the timing of future demolition is led by **Aalto University** within the EU-MAT project.

Temporal demolition modelling will be integrated into the analytical workflow to estimate when materials embedded in the existing building stock may become available.

## PLANNED TEMPORAL ANALYSES

Two complementary approaches are planned to examine the spatial and temporal distribution of potential demolition materials.

### A. Temporal material potential within static clusters

The existing material clusters, identified from the current building stock, will remain spatially fixed.

Expected demolition material quantities will be calculated for each cluster and time period. This will allow comparison of how material availability develops over time within existing material concentrations.

### B. Period-specific temporal clusters

Separate material clusters will be identified for each time period using estimated demolition material quantities.

This approach will examine whether the locations and spatial extent of material concentrations change over time.

### Comparison and logistical assessment

The two approaches will be compared to assess whether existing material stock concentrations adequately represent future demolition material potential or whether temporal modelling reveals different spatial patterns.

Temporal material estimates will also be linked to network-based accessibility analysis to assess potential transport requirements and opportunities for local material processing and reuse.

## DATA

### Building stock data

The Finnish case application uses building information obtained from the **Finnish Building Stock Data (RYHTI)**. The data provide building locations and attributes used in MI estimation.

Data availability and completeness vary between datasets and countries. Therefore, the method may require harmonisation, alternative assumptions or a reduced analytical scope when applied outside the Finnish case.

### Road-network data

**OpenStreetMap (OSM) road network data** are used for network preparation, routing and accessibility analysis.

### Recovery, processing and reuse locations

**Project-specific facility data** are collected within the EU-MAT project. The datasets describe the locations of facilities that process demolition materials and the material fractions accepted by each facility.

These data are used to identify potential destinations for recovered materials. Concrete and wood are analysed separately because the relevant facility networks may differ between materials.

## REPOSITORY STRUCTURE

```text
MINING_METHOD/
├── R/
│   ├── 01_prepare_buildings.R
│   ├── 02_grid_clusters.R
│   ├── 02b_cluster_sensitivity.R
│   ├── 03_prepare_network.R
│   ├── 04_building_accessibility.R
│   ├── 05_cluster_accessibility.R
│   ├── 05b_cluster_facility_allocation.R
│   ├── 06_export_results.R
│   └── functions/
├── frontend/
├── README.md
└── .gitignore
```

## MAIN R PACKAGES

The workflow currently uses packages including:

- `sf` for vector spatial data;
- `dplyr` for data processing;
- `terra` for rasterisation and focal calculations;
- `dodgr` for road-network preparation and routing;
- `ggplot2` and `mapview` for visualisation.

## DEMO VERSION

A demo version of the method is planned to be developed with a visual user interface.

The planned interface will use **JavaScript** and **MapLibre** for browser-based map visualisation.

The interface is intended to allow users to select buildings and examine building-specific material estimates and transport routes to relevant processing or reuse facilities.

## FUTURE DEVELOPMENT

Planned development includes:

- integrating improved material intensity estimates;
- integrating demolition timing and temporal material availability;
- implementing and comparing static-cluster and period-specific temporal analyses;
- linking temporal material potential with accessibility and transport estimates;
- assessing opportunities for local material processing and reuse;
- testing transferability across different building datasets and case-study locations; and
- preparing analytical outputs for visualisation and decision-support applications.

## PROJECT

**EU-MAT: Effective Urban Material Mining in Cities**

**Project duration:** 1 September 2025 – 31 August 2028

The project brings together partners from Finland, Sweden and Norway to develop methods and applications supporting the identification, recovery and reuse of materials embedded in the built environment.
