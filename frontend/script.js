
const map = new maplibregl.Map({
    container: 'map',
    style: 'https://api.maptiler.com/maps/base-v4/style.json?key=Gk1A4lP9NbUvWbxnI98J',
    center: [29.76, 62.6],
    zoom: 14
});

// Load data and layers once the map has finished loading
map.on('load', () => {

    // Load data and layers once the map has finished loading
    map.addSource('buildings', {
        type: 'geojson',
        data: 'data/buil_polygons.geojson'
    });
    
    // Add the building layer to the ma
    map.addLayer({
        id: 'buildings-layer',
        type: 'fill',
        source: 'buildings',
        paint: {
            'fill-color': '#ff5500',
            'fill-opacity': 0.6
        }
    });

    // Define constants for reuse
    const BUILDING_LAYER = 'buildings-layer';
    const BUILDING_ID_FIELD = 'id';

    // Change cursor to pointer when hovering over a building
    map.on('mouseenter', BUILDING_LAYER, () => {
        map.getCanvas().style.cursor = 'pointer';
    });

    // Reset cursor when leaving a building feature
    map.on('mouseleave', BUILDING_LAYER, () => {
        map.getCanvas().style.cursor = '';
    });    

    // Show a popup when a building is clicked
    map.on('click', BUILDING_LAYER, (e) => {

        // e.features contains all features at the click location;
        // e.features[0] is the topmost feature
        const feature = e.features && e.features[0];
        if (!feature) return;  // Exit if no feature was clicked

        const props = feature.properties || {};
        const buildingId = props?.[BUILDING_ID_FIELD];

        // Warn if the expected building ID attribute is missing
        if (buildingId == null) {
            console.warn(`Puuttuu kenttä ${BUILDING_ID_FIELD}`, props);
        }
   

        // Fields to be shown in the popup, with units and formatting
        const popupFields = [
        {
            key: "decade",
            label: "Building decade"
        },
        {
            key: "MI_wood",
            label: "Wood material intensity",
            unit: "tonnes",
            format: (v) => v.toFixed(1)
        },
        {
            key: "company_wood",
            label: "Wood waste processing facility"
        },
        {
            key: "MI_concrete",
            label: "Concrete material intensity",
            unit: "tonnes",
            format: (v) => v.toFixed(1)
        },
        {
            key: "company_conc",
            label: "Concrete waste processing facility"
        }
        ];


        // Build the popup HTML conten

        const html = `                                   
            <div style="min-width:180px">
                <strong>Kohde</strong><br/>
                 ${
                    popupFields
                    .map(({ key, label, unit, format }) => {
                        const value = props[key];
                        if (value == null) return null;

                        const formattedValue = format
                        ? format(Number(value))
                        : value;

                    return `<strong>${label}:</strong> ${formattedValue}${unit ? " " + unit : ""}`;
                })
                .filter(Boolean)
                .join("<br/>")
            }
            </div>
        `;

        // Create and display the popup at the click locatio
        new maplibregl.Popup({ closeButton: true, closeOnClick: true})  // Luo pop up-ikkunan
            .setLngLat(e.lngLat)                                          //Pop upin sijainti klikatun pisteen kohdalle
            .setHTML(html)                                              // Asetetaan HTML sisältö pop uppiin
            .addTo(map);                                                // Pop up näkyviin kartalle


        });
    });

// Add zoom and rotation controls to the map
map.addControl(new maplibregl.NavigationControl());