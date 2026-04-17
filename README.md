# MATERIAL MINING METHOD

This repository contains scripts supporting the development of an **Urban Material Mining Method**.

The method is developed within the **EU-MAT: Effective Urban Material Mining in Cities** project as part of research, development, and innovation (RDI) activities. Karelia University of Applied Sciences acts as the responsible project partner for the methodological development.

## WORKFLOW AND FUTURE DEVELOPMENT

### Demo version

Data processing and analysis will be primarily conducted using **R**, which serves as the core environment for handling building stock data, material calculations, and analytical workflows. A demo version of the method is planned to be developed with a **visual user interface**. The demo will combine:
- **MapLibre (JavaScript)** for browser-based map visualisation,
- **Python** and **OSMR** for route visualisation.

The MapLibre-based web interface will allow the user to select a building of interest directly on the map. Based on the selected building:
- Python and OSMR will calculate transport routes from the building to facilities for further processing or reuse of demolition materials.
- The map view will display the calculated routes visually.
- A pop-up window will present building-specific results, including:
  - estimated quantities of materials (in tonnes),
  - transport distance (kilometres),
  - estimated transport costs,
  - greenhouse gas emissions related to material transport.

### Material intensity (MI) calculation

The development of material intensity (MI) calculation is led by **Aalto University**, a project partner in the EU-MAT project.

A preliminary version of the method already exists and is presented in the following scientific publication:

Nasiri, F., Kaasalainen, T., Hughes, M. (2023).  
*Estimating the material intensity of wooden residential houses in Finland*.  
Resources, Conservation and Recycling, Volume 198, 107142.  
https://doi.org/10.1016/j.resconrec.2023.107142

At the current stage of development, the Urban Material Mining Method utilises this MI coefficients approach as its basis for material quantity estimation. Further development of the MI coefficients method is ongoing, with the aim of extending it to be applicable to a broader range of building types across the building stock. As the EU-MAT project progresses, an updated and more comprehensive MI coefficients method will be integrated into the Urban Material Mining Method.

## DATA

The primary input dataset for the method is the **Finnish Building Stock Data (RYHTI)**, which provides information on buildings and supports the assessment of materials embedded in the existing building stock.

In addition, the method utilises **OpenStreetMap (OSM) road network data** to support proximity analysis related to the transport of demolition materials.

The analysis is further complemented with **project-specific data collected within the EU-MAT project**, describing the locations of facilities for further processing of demolition materials.


## STRUCTURE

\-R\\: scripts

\-data\\raw\\: raw data

\-data\\processed\\: processed data

