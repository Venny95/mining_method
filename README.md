**This README file is a work in progress and will be updated continuously as development proceeds.**

# OVERVIEW: URBAN MATERIAL MINING METHOD

This repository contains the **R**-based analytical workflow for the Urban Material Mining Method developed within the **EU-MAT: Effective Urban Material Mining in Cities project**.

The method supports the identification and spatial assessment of materials embedded in the existing building stock. It combines building-level material stock estimation, spatial aggregation, material concentration analysis and network-based accessibility analysis.

The current workflow focuses on concrete and wood. The method is initially developed and tested using building stock data for **Joensuu, Finland**. Its transferability to other municipal and national data contexts will be assessed during the project.

The EU-MAT project is implemented from 1 September 2025 to 31 August 2028. Karelia University of Applied Sciences is responsible for the spatial development of the material mining method in cooperation with the project partners.

## METHODOLOGICAL FRAMEWORK

The method distinguishes between three related dimensions of urban mining potential:

1. **Material availability**: The estimated quantity of material embedded in the building stock.
2. **Spatial concentration**: The extent to which material stocks are concentrated within spatially connected areas.
3. **Accessibility**: The network-based distance from material concentrations to potential recovery, processing or reuse locations.

The analytical workflow is:

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

The current results describe estimated material stocks embedded in the existing building stock. They should not be interpreted directly as recoverable material flows unless information about demolition timing, material condition, technical recoverability and other relevant constraints is available.

## CURRENT WORKFLOW

The original analysis has been divided into six numbered R scripts. The scripts form a sequential workflow and should be run in numerical order.

**01: Building data and material stock preparation**
*01_prepare_buildings.R* prepares the building stock data and calculates preliminary building-level estimates of concrete and wood stocks using MI coefficients.

**02: Road-network preparation**
*02_prepare_network.R* prepares and weights the OpenStreetMap road network for routing and accessibility analysis.

**03: Building-level accessibility**
*03_buildings_accessibility.R* calculates the network distance from each building to the nearest reachable facility accepting wood or concrete.

**04: Spatial aggregation and material clusters**
*04_grid_clusters.R* aggregates building-level material estimates into spatial units and identifies local concentrations of concrete and wood. Spatially connected areas are combined into material clusters.

**05: Cluster-level accessibility**
*05_cluster_accessibility.R* is planned to define representative origins for material clusters and calculate routes to relevant material-specific facilities.

**06: Export results**
*06_export_results* will prepare selected analytical outputs for further analysis, mapping and visualisation. Its content will be specified as the workflow develops.

#### Material intensity (MI) calculation
Material intensity coefficients are used to estimate concrete and wood quantities at the building level. The development of the material intensity methodology is led by **Aalto University** within the EU-MAT project.

A preliminary foundation for the approach is presented in:
Nasiri, F., Kaasalainen, T., Hughes, M. (2023).  
*Estimating the material intensity of wooden residential houses in Finland*.  
Resources, Conservation and Recycling, Volume 198, 107142.  
https://doi.org/10.1016/j.resconrec.2023.107142

Further development of the MI coefficients method is ongoing, with the aim of extending it to be applicable to a broader range of building types across the building stock. As the EU-MAT project progresses, an updated and more comprehensive MI coefficients method will be integrated into the Urban Material Mining Method.

#### Demolition curve
The ongoing development of the demolition curve provides information about demolition timing. The development is led by **Aalto University** within the EU-MAT project.

### Demo version

A demo version of the method is planned to be developed with a visual user interface. The demo will combine:
- **JavaScript** and **MapLibre** for browser-based demo version map visualisation,
- **OSMR** for routes of demo version visualisation.

The MapLibre-based web interface will allow the user to select a building of interest directly on the map. Based on the selected building:
- Python and OSMR will calculate transport routes from the building to facilities for further processing or reuse of demolition materials.
- The map view will display the calculated routes visually.
- A pop-up window will present building-specific results, including:
  - estimated quantities of materials (in tonnes),
  - transport distance (kilometres)

## DATA

#### Building stock data
The Finnish case application uses building information obtained from the **Finnish Building Stock Data (RYHTI)**. The data provide building locations and attributes used in MI estimation.
The source data may include information such as:

Data availability and completeness vary between datasets and countries. Therefore, the method may require harmonisation, alternative assumptions or a reduced analytical scope when applied outside the Finnish case.

#### Road-network data
**OpenStreetMap (OSM) road network data** is used for network preparation, routing and accessibility analysis.

#### Recovery, processing and reuse locations
**Project-specific facility data** are collected within the EU-MAT project. The datasets describe the locations of facilities that process demolition materials and the material fractions accepted by each facility. These data are used to identify potential destinations for recovered materials. Concrete and wood are analysed separately because the relevant facility networks may differ between materials.

## REPOSITORY STRUCTURE

```text
MINING_METHOD/
├── R/
│   ├── 01_prepare_buildings.R
│   ├── 02_prepare_network.R
│   ├── 03_buildings_accessibility.R
│   ├── 04_grid_clusters.R
│   ├── 05_cluster_accessibility.R
│   ├── 06_export_results.R
│   └── data/
│       ├── raw/
│       └── processed/
├── frontend/
│   ├── index.html
│   ├── script.js
│   └── style.css
├── README.md
└── .gitignore
```


## MAIN R PACKAGES
The workflow currently uses packages including:
- sf for vector spatial data;
- dplyr for data processing;
- terra for rasterisation and focal calculations;
- igraph for identifying connected grid-cell clusters; and
- dodgr for road-network preparation and routing.


## FUTURE DEVELOPMENT

Planned development includes:
- integrating improved material intensity estimates;
- integrating a demolition curve, e.g. temporal information on potential material release;
- testing transferability across different building datasets and case-study locations; and
- preparing analytical outputs for later visualisation and decision-support applications.

## PROJECT

**EU-MAT: Effective Urban Material Mining in Cities**
Project duration: 1 September 2025 – 31 August 2028
The project brings together partners from Finland, Sweden and Norway to develop methods and applications supporting the identification, recovery and reuse of materials embedded in the built environment.

