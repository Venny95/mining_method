// Vektoritiilet suoraan PostGIS:stä ovat tehokkaampi vaihtehto isojen aineistojen esittämiseen


const map = new maplibregl.Map({
    container: 'map',
    style: 'https://api.maptiler.com/maps/base-v4/style.json?key=Gk1A4lP9NbUvWbxnI98J',
    center: [29.76, 62.6],
    zoom: 14
});

// Lisätään GeoJSOn-tiedostot
map.on('load', () => {

    //GeoJSON rakennukset
    map.addSource('buildings', {
        type: 'geojson',
        data: 'data/buil_polygons.geojson'
    });

    map.addLayer({
        id: 'buildings-layer',
        type: 'fill',
        source: 'buildings',
        paint: {
            'fill-color': '#ff5500',
            'fill-opacity': 0.6
        }
    });

    //Määritellään vakioita
    const BUILDING_LAYER = 'buildings-layer';
    const BUILDING_ID_FIELD = 'id';

    //Kursori ku hiiri on kohteen päällä
    map.on('mouseenter', BUILDING_LAYER, () => {       // Hiiren osoitin kädeksi kun se menee pisteen päälle
        map.getCanvas().style.cursor = 'pointer';
    });

    map.on('mouseleave', BUILDING_LAYER, () => {       // Kursori normaaliksi, kun menee pois pisteen päältä
        map.getCanvas().style.cursor = '';
    });    

    // Kun klikataan, näkyy pop up
    map.on('click', BUILDING_LAYER, (e) => {
        const feature = e.features && e.features[0];          // e.features sisältää kaikki featuret, joiden päällä klikkaus tapahtui, e.features[0] on ensimmäinen niistä
        if (!feature) return;                                 // Jos ei featurea, lopetetaan

        const props = feature.properties || {};
        const buildingId = props?.[BUILDING_ID_FIELD];

        if (buildingId == null) {
            console.warn(`Puuttuu kenttä ${BUILDING_ID_FIELD}`, props);
        }
   

        // Pop up fields
        
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


        // HTML pop up:

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

        new maplibregl.Popup({ closeButton: true, closeOnClick: true})  // Luo pop up-ikkunan
            .setLngLat(e.lngLat)                                          //Pop upin sijainti klikatun pisteen kohdalle
            .setHTML(html)                                              // Asetetaan HTML sisältö pop uppiin
            .addTo(map);                                                // Pop up näkyviin kartalle


        });
    });


map.addControl(new maplibregl.NavigationControl());