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
            'circle-radius': 6,
            'circle-color': '#ff5500'
        }
    });

        //GeoJSON tiet
    map.addSource('tiet_testi_json', {
        type: 'geojson',
        data: 'tiet_testi_json.geojson'
    });

    map.addLayer({
        id: 'tiet_testi_json-layer',
        type: 'line',
        source: 'tiet_testi_json',
        paint: {
            'line-color': '#0077ff',
            'line-width': 3
        }
    });

    //Piilotetaan viivat
    map.setLayoutProperty('tiet_testi_json-layer', 'visibility', 'none')
    //Nollafiltteri
    map.setFilter('tiet_testi_json-layer', ['==', ['literal', 1], 0]);

    //Määritellään vakioita
    const POINT_LAYER = 'rakennukset_testi_json-layer';
    const LINE_LAYER = 'tiet_testi_json-layer';
    
    const POINT_ID_FIELD = 'id';
    const LINE_JOIN_FIELD = 'buil_id';

    //Kursori ku hiiri on pisteen päällä
    map.on('mouseenter', POINT_LAYER, () => {       // Hiiren osoitin kädeksi kun se menee pisteen päälle
        map.getCanvas().style.cursor = 'pointer';
    });

    map.on('mouseleave', POINT_LAYER, () => {       // Kursori normaaliksi, kun menee pois pisteen päältä
        map.getCanvas().style.cursor = '';
    });    

    // Kun klikataan pistettä, näkyy pop up ja viivat
    map.on('click', POINT_LAYER, (e) => {
        const f = e.features && e.features[0];                   // e.features sisältää kaikki featuret, joiden päällä klikkaus tapahtui, e.features[0] on ensimmäinen niistä
        if (!f) return;                                 // Jos ei featurea, lopetetaan

        const coords = f.geometry.coordinates.slice();  // Koordinaatit kertovat, mihin pop up avautuu. Slice() kopioi taulukon muistiin.
        const props = f.properties || {};               //GeoJSONin properties-osio sisältää attribuutit. Jos properties puuttuu, annetaan tyhjä objekti.

        const idRak = f.properties?.[POINT_ID_FIELD];
        if (idRak == null) {
            console.warn(`Puuttuu kenttä ${POINT_ID_FIELD} pisteessä`, props);
        }        

        // HTML-pop up:
        const html = `                                   
            <div style="min-width:180px">
                <strong>Kohde</strong><br/>
                 ${Object.entries(props)
                    .map(([k, v]) => `${k}: ${v}`)
                    .join('<br/>')}
            </div>
        `;

        new maplibregl.Popup({ closeButton: true, closeOnClick: true})  // Luo pop up-ikkunan
            .setLngLat(coords)                                          //Pop upin sijainti klikatun pisteen kohdalle
            .setHTML(html)                                              // Asetetaan HTML sisältö pop uppiin
            .addTo(map);                                                // Pop up näkyviin kartalle

        //filtteri, joka näyttää viivat joiden LINE_JOIN_FIELD == idRak
        const filter = ['==', ['to-string', ['get', LINE_JOIN_FIELD]], String(idRak)];
        map.setFilter(LINE_LAYER, filter);
        map.setLayoutProperty(LINE_LAYER, 'visibility', 'visible');

        })
    })


map.addControl(new maplibregl.NavigationControl());