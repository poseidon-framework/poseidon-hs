function loadCss(href, integrity) {
  return new Promise((resolve, reject) => {
    if (document.querySelector(`link[href="${href}"]`)) {
      resolve();
      return;
    }
    const link = document.createElement("link");
    link.rel = "stylesheet";
    link.type = "text/css";
    link.href = href;
    if (integrity) link.integrity = integrity;
    link.crossOrigin = "";
    link.onload = resolve;
    link.onerror = reject;
    document.head.appendChild(link);
  });
}

function loadScript(src, integrity) {
  return new Promise((resolve, reject) => {
    if (document.querySelector(`script[src="${src}"]`)) {
      resolve();
      return;
    }
    const script = document.createElement("script");
    script.src = src;
    script.defer = true;
    if (integrity) script.integrity = integrity;
    script.crossOrigin = "";
    script.onload = resolve;
    script.onerror = reject;
    document.head.appendChild(script);
  });
}

async function loadPlotLibraries() {
  await Promise.all([
    loadCss(
      "https://unpkg.com/leaflet@1.9.4/dist/leaflet.css",
      "sha256-p4NxAoJBhIIN+hmNHrzRCf9tD/miZyoHS5obTRR9BMY="
    ),
    loadCss(
      "https://unpkg.com/leaflet.markercluster@1.5.3/dist/MarkerCluster.css",
      "sha256-YU3qCpj/P06tdPBJGPax0bm6Q1wltfwjsho5TR4+TYc="
    ),
    loadCss(
      "https://unpkg.com/leaflet.markercluster@1.5.3/dist/MarkerCluster.Default.css",
      "sha256-YSWCMtmNZNwqex4CEw1nQhvFub2lmU7vcCKP+XVwwXA="
    )
  ]);
  await loadScript(
    "https://unpkg.com/leaflet@1.9.4/dist/leaflet.js",
    "sha256-20nQCchB9co0qIjJZRGuk2/Z9VM+kNiyxNV1lvTlZBo="
  );
  await loadScript(
    "https://unpkg.com/leaflet.markercluster@1.5.3/dist/leaflet.markercluster.js",
    "sha256-Hk4dIpcqOSb0hZjgyvFOP+cEmDXUKKNE/tT542ZbNQg="
  );
  await loadScript(
    "https://cdn.jsdelivr.net/npm/vega@6.2.0",
    "sha256-6mpZNjgdBvkSRmNU4raiDrZsTUHxyp3/HgI23+5u45I="
  );
  await loadScript(
    "https://cdn.jsdelivr.net/npm/vega-lite@6.4.3",
    "sha256-NamCHfg4glsFpqc+lBS1h0ehsYMhWDhY7ZA8Zjk6XH4="
  );
  await loadScript(
    "https://cdn.jsdelivr.net/npm/vega-embed@7.1.0",
    "sha256-w2JUJwIZ7uWPubHZVN7K2VT7B7/Jq3gMXUQBvURc1Qw="
  );
}


async function initialisePlots() {
  await loadPlotLibraries();

  const jsonEl = document.getElementById("samples-json");
  if (!jsonEl) return;

  const rawSamples = JSON.parse(jsonEl.textContent);

  const samples = rawSamples.map(s => {
    let mmAge = s.mmAge;
    if (
      mmAge == null &&
      s.mmAgeStart != null &&
      s.mmAgeStop != null &&
      Number.isFinite(s.mmAgeStart) &&
      Number.isFinite(s.mmAgeStop)
    ) {
      mmAge = Math.round((s.mmAgeStart + s.mmAgeStop) / 2);
    }
    return { ...s, mmAge: mmAge };
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
              return {
                  binStart: d.binStart,
                  binEnd: d.binEnd,
                  binMid: (d.binStart + d.binEnd) / 2,
                  count: d.count
              };
          });
      const maxCount = Math.max(...timelineBins.map(d => d.count));
      const yMax = Math.ceil(maxCount * 1.15);
      const ages = timelineMarkers
          .map(s => Number(s.mmAge))
          .filter(Number.isFinite);
      const dataMin = Math.min(...ages);
      const dataMax = Math.max(...ages);
      const domainStart = dataMin - 2000;
      const domainEnd = dataMax + 2000;
      const timelineSpec = {
          $schema: 'https://vega.github.io/schema/vega-lite/v6.json',
          width: 'container',
          height: 115, // less than the surrounding div, to show 1px border
          autosize: { type: 'fit', contains: 'padding' },
          data: { values: timelineBins },
          layer: [{
              // main histogram bars
              params: [
                  {
                      name: 'x_zoom',
                      select: { type: 'interval', encodings: ['x'] },
                      bind: 'scales'
                  }
              ],
              mark: { type: 'bar', orient: 'vertical', color: '#13171f' },
              encoding: {
                  x: {
                      field: 'binStart',
                      type: 'quantitative',
                      bin: 'binned',
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
                      scale: {
                          domain: [0, yMax],
                          nice: false
                      },
                      axis: { tickMinStep: 1, tickCount: 5 }
                  },
                  y2: { datum: 0 } // force bars to extend down to zero
              }
          },
          {
              // rug/presence ticks
              mark: {
                  type: 'point',
                  color: '#d94801',
                  size: 20,
                  shape: "triangle-down"
              },
              encoding: {
                  x: { field: 'binMid', type: 'quantitative' },
                  y: { field: 'count', type: 'quantitative' },
                  yOffset: { value: -5 } // move up by 5 pixels
              }
          }],
          resolve: { scale: { x: 'shared' } },
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

document.addEventListener("DOMContentLoaded", function () {
  const hasPlots =
    document.getElementById("timelineid") &&
    document.getElementById("mapid") &&
    document.getElementById("samples-json");

  if (!hasPlots) return;

  const start = () => initialisePlots();

  if ("requestIdleCallback" in window) {
    requestIdleCallback(start, { timeout: 3000 });
  } else {
    setTimeout(start, 1500);
  }
});
