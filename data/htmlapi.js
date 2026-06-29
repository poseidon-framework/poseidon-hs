window.onload = function() {

// transform table to sortable version
if (document.querySelector('#currentTable')) {
    let options = {
        searchable: true,
        perPage: 10
    };
    new simpleDatatables.DataTable('#currentTable', options);
}

const rawSamples = JSON.parse(
  document.getElementById('samples-json').textContent
);

// load samples and try to complete some mean age information
const samples = rawSamples.map(s => {
  let mmAge = s.mmAge;
  if (mmAge == null &&
      s.mmAgeStart != null &&
      s.mmAgeStop != null &&
      Number.isFinite(s.mmAgeStart) &&
      Number.isFinite(s.mmAgeStop)) {
    mmAge = Math.round((s.mmAgeStart + s.mmAgeStop) / 2);
  }
  return {...s, mmAge: mmAge};
});

// prepare samples for plotting
const initialLength = samples.length;
const mapMarkers = samples.filter(s =>
  s.mmLat != null &&
  s.mmLon != null &&
  Number.isFinite(s.mmLat) &&
  Number.isFinite(s.mmLon)
);
const timelineMarkers = samples.filter(s =>
  s.mmAge != null &&
  Number.isFinite(s.mmAge)
);
const missingCoordinates = initialLength - mapMarkers.length;
const missingAge = initialLength - timelineMarkers.length;

function formatYear(x) {
  if (x == null || Number.isNaN(Number(x))) { return 'unknown'; }
  x = Number(x);
  return x < 0 ? `${Math.abs(x).toLocaleString()} BC` : `${x.toLocaleString()} AD`;
}

// timeline plot with Vega-Lite
if (document.querySelector('#timelineid')) {
    const timelineEl = document.querySelector('#timelineid');
    const binWidth = 100;
    // bin data
    const bins = new Map();
    for (const s of timelineMarkers) {
        const age = Number(s.mmAge);
        if (!Number.isFinite(age)) continue;
        const binStart = Math.floor(age / binWidth) * binWidth;
        const binEnd = binStart + binWidth;
        const key = `${binStart}:${binEnd}`;
        if (!bins.has(key)) {
            bins.set(key, {
                binStart,
                binEnd,
                count: 0,
                ids: []
            });
        }
        const entry = bins.get(key);
        entry.count += 1;
        entry.ids.push(s.mmPoseidonID);
    }
    const timelineBins = Array.from(bins.values())
        .sort((a, b) => a.binStart - b.binStart)
        .map(d => {
            const idsPreview = d.ids.length > 20
                ? d.ids.slice(0, 20).join(', ') + `, … ${d.ids.length - 20} more`
                : d.ids.join(', ');
            return {
                binStart: d.binStart,
                binEnd: d.binEnd,
                count: d.count,
                binLabel: `${formatYear(d.binStart)} – ${formatYear(d.binEnd)}`,
                idsPreview
            };
        });
    const ages = timelineMarkers
        .map(s => Number(s.mmAge))
        .filter(Number.isFinite);
    const dataMin = Math.min(...ages);
    const dataMax = Math.max(...ages);
    const domainStart = Math.floor(dataMin / binWidth) * binWidth;
    const domainEnd = Math.ceil(dataMax / binWidth) * binWidth;
    const timelineSpec = {
        $schema: 'https://vega.github.io/schema/vega-lite/v6.json',
        width: 'container',
        height: 120,
        autosize: { type: 'fit', contains: 'padding' },
        data: { values: timelineBins },
        params: [
            {
                name: 'x_zoom',
                select: {
                    type: 'interval',
                    encodings: ['x']
                },
                bind: 'scales'
            }
        ],
        mark: { type: 'bar', color: '#13171f' },
        encoding: {
            x: {
                field: 'binStart',
                type: 'quantitative',
                title: null,
                scale: {
                    domain: [domainStart, domainEnd],
                    nice: false
                },
                axis: {
                    grid: true,
                    tickCount: 12,
                    labelExpr: `
                        datum.value < 0
                          ? format(abs(datum.value), ',') + ' BC'
                          : format(datum.value, ',') + ' AD'
                    `
                }
            },
            x2: { field: 'binEnd' },
            y: {
                field: 'count',
                type: 'quantitative',
                title: 'Samples',
                axis: { tickMinStep: 1 }
            },
            tooltip: [
                {
                    field: 'binLabel',
                    type: 'nominal',
                    title: 'Interval'
                },
                {
                    field: 'count',
                    type: 'quantitative',
                    title: 'Samples'
                },
                {
                    field: 'idsPreview',
                    type: 'nominal',
                    title: 'Poseidon IDs'
                }
            ]
        },
        config: {
            view: {
                stroke: null
            },
            axis: {
                labelFontSize: 11,
                titleFontSize: 12
            }
        }
    };
    vegaEmbed('#timelineid', timelineSpec, {
        actions: false,
        renderer: 'canvas'
    });
}

// leaflet map
if (document.querySelector('#mapid')) {
    var mymap = L.map('mapid').setView([35, 10], 1);
    L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
        maxZoom: 19,
        attribution: 'Map data <a href="https://www.openstreetmap.org/">OpenStreetMap</a> contributors'
    }).addTo(mymap);
    // add legend
    var legend = L.control({position: 'bottomright'});
    legend.onAdd = function (map) {
        var div = L.DomUtil.create('div', 'info legend');
        div.innerHTML = initialLength + ' samples loaded<br>' +
                        missingCoordinates + ' lat/lon missing<br>' +
                        missingAge + ' ages missing<br>';
        return div;
    };
    legend.addTo(mymap);
    // markers
    var markers = L.markerClusterGroup({
        chunkedLoading: true,
    });
    for (var i = 0; i<mapMarkers.length; i++) {
        const s = mapMarkers[i];
        // prepare popup message
        const packageLink = '<a href="/explorer/' + s.mmArchiveName + '/' + s.mmPackageName + '/' + s.mmPackageVersion + '/' + s.mmPoseidonID + '" style="text-decoration: underline; cursor: pointer;">Open sample</a>';
        const popupContentLines = [];
        popupContentLines.push('<b>Poseidon ID:</b> ' + s.mmPoseidonID);
        popupContentLines.push('<b>Package:</b> ' + s.mmPackageName);
        popupContentLines.push('<b>Package version:</b> ' + s.mmPackageVersion);
        popupContentLines.push('<b>Archive:</b> ' + s.mmArchiveName);
        popupContentLines.push('<b>Location:</b> ' + s.mmLocation);
        popupContentLines.push('<b>Age:</b> ' + formatYear(s.mmAge) + " (" + formatYear(s.mmAgeStart) + " - " + formatYear(s.mmAgeStop) + ")");
        popupContentLines.push('<b>' + packageLink + '</b>');
        const popupContent = popupContentLines.join("<br>");
        // create a marker with a popup
        L.marker([s.mmLat, s.mmLon])
        .bindTooltip(s.mmPoseidonID, {
          direction: 'right',
          sticky: true,
          opacity: 1,
          className: 'poseidon-tooltip'
        })
        .bindPopup(popupContent)
        .addTo(markers);
    }
    mymap.addLayer(markers);
  }
}
