**This README file is a work in progress and will be updated continuously as development proceeds.**

# URBAN MATERIAL MINING METHOD

This repository contains scripts supporting the development of an **Urban Material Mining Method**.

The method is developed within the **EU-MAT: Effective Urban Material Mining in Cities** project as part of research, development, and innovation (RDI) activities. Karelia University of Applied Sciences acts as the responsible project partner for the methodological development. The project takes place 1.9.2025 - 31.8.2028.

The Urban Material Mining method is initially developed for **Joensuu, Finland**. As the project progresses, the method will be expanded to Skelleftea, Sweden, and Sortland, Norway.

## WORKFLOW AND FUTURE DEVELOPMENT

### Material mining method

The material mining method is implemented using **R**, which is used for data processing, analysis, and result generation.

#### Material intensity calculations
Material intensities for wood and concrete material are calculated at the individual building level using material intensity (MI) coefficients developed by Aalto University (see the *Material Intensity (MI) Calculation* section below for details).

#### Proximity and Routing Analysis
**Proximity analysis** for the transportation of demolition materials is conducted using the `dodgr` R package. Routing calculations are based on road network (see the *Data* section for details) distances and are weighted to favour routes suitable for motor vehicle traffic, ensuring realistic transportation paths for heavy vehicles.

#### Transport Emissions Calculation
**Transport-related CO2 emissions** are calculated using a standard tonne-kilometre approach, which is widely applied in international greenhouse gas accounting frameworks such as the GHG Protocol and IPCC guidelines.

Emissions are calculated as:
**E = m x d x EF**
where:
- **E** = transport emissions (kg CO2e)  
- **m** = transported mass (tonnes)  
- **d** = transport distance (km)  
- **EF** = emission factor (kg CO2e / tonne-km)

The emission factor is derived from the Finnish national emissions database **CO2data.fi**.

The transport emissions calculations are based on the following assumptions:
- The emission factor represents a heavy-duty earthmoving truck (*maansiirtoauto*) with the gross vehicle weight of 32 tonnes and payload capacity of 19 tonnes.
- The transport consists of an 80% loaded trip from the demolition site.
- The vehicle returns to the site as an empty return trip.
- All transportation is assumed to take place in an urban environment.

### Demo version

A demo version of the method is planned to be developed with a visual user interface. The demo will combine:
- **JavaScript** and **MapLibre** for browser-based demo version map visualisation,
- **OSMR** for routes of demo version visualisation.

The MapLibre-based web interface will allow the user to select a building of interest directly on the map. Based on the selected building:
- Python and OSMR will calculate transport routes from the building to facilities for further processing or reuse of demolition materials.
- The map view will display the calculated routes visually.
- A pop-up window will present building-specific results, including:
  - estimated quantities of materials (in tonnes),
  - transport distance (kilometres),
  - CO2 related to material transport.

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

\-R\\: R scripts
\-R\\data\\raw\\: raw data
\-R\\data\\processed\\: processed data

\-frontend\\: demo version frontend scripts
\-frontend\\data\\: processed data for demo version

\-QGIS\\: QGIS project for data inspection

\-.gitignore\\: untracked files
