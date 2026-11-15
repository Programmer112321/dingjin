/* Datasets page globe: true-colour Earth (NASA Blue Marble, public domain) with
   remote-sensing satellites scanning it (LiDAR, optical, radar) and the NEON
   field sites (window.NEON_SITES from assets/neon-sites.js).
   Uses globe.gl (MIT) from jsDelivr. If WebGL or the CDN is unavailable,
   the CSS fallback sphere stays visible. */
(function () {
  "use strict";

  var GLOBE_JS    = "https://cdn.jsdelivr.net/npm/globe.gl@2.46.2/dist/globe.gl.min.js";
  var IMG_BASE    = "https://cdn.jsdelivr.net/npm/three-globe@2.45.6/example/img/";
  var BLUE_MARBLE = IMG_BASE + "earth-blue-marble.jpg";   /* true colour from space */
  var TOPOLOGY    = IMG_BASE + "earth-topology.png";      /* elevation, used as relief */

  function loadScript(src) {
    return new Promise(function (resolve, reject) {
      var s = document.createElement("script");
      s.src = src; s.async = true;
      s.onload = resolve; s.onerror = reject;
      document.head.appendChild(s);
    });
  }

  function webglOK() {
    try {
      var c = document.createElement("canvas");
      return !!(window.WebGLRenderingContext && (c.getContext("webgl") || c.getContext("experimental-webgl")));
    } catch (e) { return false; }
  }

  function build(el) {
    var reduce = window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;

    var globe = Globe({ animateIn: !reduce })(el)
      .backgroundColor("rgba(0,0,0,0)")
      .globeImageUrl(BLUE_MARBLE)
      .bumpImageUrl(TOPOLOGY)
      .showAtmosphere(true)
      .atmosphereColor("#9fc9f2")
      .atmosphereAltitude(0.15)
      .onGlobeReady(function () {
        var fb = el.parentNode.querySelector(".hp-globe-fallback");
        if (fb) fb.style.display = "none";
      });

    /* stronger relief so mountain ranges read as terrain */
    var mat = globe.globeMaterial();
    mat.bumpScale = 12;
    mat.shininess = 6;

    var controls = globe.controls();
    controls.autoRotate = !reduce;
    controls.autoRotateSpeed = 0.45;
    controls.enableZoom = false;

    globe.pointOfView({ lat: 38, lng: -100, altitude: 2.2 }, 0);

    function fit() {
      globe.width(el.clientWidth).height(el.clientHeight);
    }
    fit();
    window.addEventListener("resize", fit);

    /* pause rotation while the visitor holds or drags the globe; resume 2.5 s after release */
    var resumeTimer = null;
    el.addEventListener("pointerdown", function () {
      controls.autoRotate = false;
      clearTimeout(resumeTimer);
    });
    ["pointerup", "pointercancel", "pointerleave"].forEach(function (ev) {
      el.addEventListener(ev, function () {
        clearTimeout(resumeTimer);
        if (!reduce) resumeTimer = setTimeout(function () { controls.autoRotate = true; }, 2500);
      });
    });

    neonSites(globe);
    el.__globe = globe;   /* handle for debugging in the browser console */
    if (!reduce) satellites(globe, el);
  }

  /* ── Remote-sensing satellites scanning the Earth ─────────────────────────
     Real orbit inclinations and altitudes (altitude in Earth radii); time is
     sped up so one orbit takes about a minute. Ground tracks drift west
     slowly, as real tracks do while the Earth turns underneath. */
  var SATS = [
    { name: "GEDI on the ISS · spaceborne LiDAR", kind: "lidar",   inc: 51.6, raan: -70,  u0: 10,  h: 0.066, period: 58, color: "#62ff9a" },
    { name: "ICESat-2 · spaceborne LiDAR",        kind: "lidar",   inc: 92.0, raan: 15,   u0: 150, h: 0.078, period: 64, color: "#9dffc4" },
    { name: "Landsat 9 · optical imager",         kind: "optical", inc: 98.2, raan: 150,  u0: 250, h: 0.111, period: 72, color: "#ffe28a" },
    { name: "Sentinel-1 · C-band radar",          kind: "radar",   inc: 98.2, raan: -140, u0: 70,  h: 0.109, period: 72, color: "#8fd6ff" }
  ];
  var DRIFT = 0.35;          /* deg per second of westward ground-track drift */
  var D2R = Math.PI / 180, R2D = 180 / Math.PI;

  function subPoint(s, t) {
    var u = (s.u0 + 360 * t / s.period) * D2R;
    var inc = s.inc * D2R;
    var lat = Math.asin(Math.sin(inc) * Math.sin(u)) * R2D;
    var lng = s.raan - DRIFT * t + Math.atan2(Math.cos(inc) * Math.sin(u), Math.cos(u)) * R2D;
    lng = ((lng + 540) % 360) - 180;
    return [lat, lng];
  }

  /* point offset sideways (perpendicular to the ground track) by `deg` degrees */
  function sideways(p, q, deg) {
    var cl = Math.cos(p[0] * D2R) || 1e-6;
    var dLat = q[0] - p[0];
    var dLng = ((q[1] - p[1] + 540) % 360 - 180) * cl;
    var n = Math.sqrt(dLat * dLat + dLng * dLng) || 1;
    var px = -dLng / n, py = dLat / n;            /* perpendicular in local tangent */
    return [p[0] + px * deg, p[1] + (py * deg) / cl];
  }

  function satIcon(color) {
    var d = document.createElement("div");
    d.style.cssText = "width:30px;height:30px;transform:translate(-50%,-50%);pointer-events:auto;cursor:default;filter:drop-shadow(0 0 4px " + color + ");transition:opacity .3s";
    d.innerHTML =
      '<svg viewBox="0 0 24 24" width="30" height="30">' +
      '<rect x="1" y="9.5" width="7" height="5" fill="' + color + '" opacity=".85"/>' +
      '<rect x="16" y="9.5" width="7" height="5" fill="' + color + '" opacity=".85"/>' +
      '<rect x="8.5" y="8" width="7" height="8" rx="1.2" fill="#ffffff"/>' +
      '<line x1="8" y1="12" x2="8.5" y2="12" stroke="#fff"/><line x1="15.5" y1="12" x2="16" y2="12" stroke="#fff"/>' +
      '</svg>';
    return d;
  }

  function satellites(globe, el) {
    var t0 = performance.now();
    var footprints = [];       /* LiDAR shots left on the ground */
    var swaths = {};           /* optical ground swath history per satellite */
    var radarHits = [];        /* radar ground pulses */
    var lastShot = 0, lastPulse = 0, lastOrbit = -1, lastDraw = 0;

    var marks = SATS.map(function (s) { return { s: s, lat: 0, lng: 0, alt: s.h, el: null }; });

    globe
      .htmlElementsData(marks)
      .htmlLat("lat").htmlLng("lng").htmlAltitude("alt")
      .htmlElement(function (m) {
        m.el = satIcon(m.s.color);
        m.el.title = m.s.name;
        return m.el;
      })
      .htmlTransitionDuration(0)
      .htmlElementVisibilityModifier(function (node, visible) { node.style.opacity = visible ? 1 : 0; })
      .pathsData([])
      .pathPoints("pts")
      .pathPointLat(function (p) { return p[0]; })
      .pathPointLng(function (p) { return p[1]; })
      .pathPointAlt(function (p) { return p[2]; })
      .pathColor("color")
      .pathStroke("stroke")
      .pathTransitionDuration(0)
      .pointsData([])
      .pointLat(function (p) { return p[0]; }).pointLng(function (p) { return p[1]; })
      .pointAltitude(0.004)
      .pointRadius(0.17)
      .pointColor(function (p) { return p[2]; })
      .pointsMerge(true)
      .pointsTransitionDuration(0)
      .ringsData([])
      .ringLat(function (p) { return p[0]; }).ringLng(function (p) { return p[1]; })
      .ringColor(function () { return function (t) { return "rgba(143,214,255," + (1 - t) + ")"; }; })
      .ringMaxRadius(4.5)
      .ringPropagationSpeed(3.5)
      .ringRepeatPeriod(900);

    var orbits = [];

    function frame(now) {
      if (!document.hidden && now - lastDraw > 40) {
        lastDraw = now;
        var t = (now - t0) / 1000;
        var paths = orbits.slice();

        /* orbit rings are re-drawn once a second (they drift slowly) */
        if (Math.floor(t) !== lastOrbit) {
          lastOrbit = Math.floor(t);
          orbits = SATS.map(function (s) {
            var pts = [];
            for (var k = 0; k <= 120; k++) {
              var p = subPoint(s, t + (k / 120) * s.period);
              pts.push([p[0], p[1], s.h]);
            }
            return { pts: pts, color: "rgba(255,255,255,0.45)", stroke: 1 };
          });
          paths = orbits.slice();
        }

        marks.forEach(function (m) {
          var s = m.s;
          var p = subPoint(s, t), q = subPoint(s, t + 0.4);
          m.lat = p[0]; m.lng = p[1];

          if (s.kind === "lidar") {
            [-0.7, 0, 0.7].forEach(function (off) {
              var g = sideways(p, q, off);
              paths.push({ pts: [[p[0], p[1], s.h], [g[0], g[1], 0.002]], color: "rgba(120,255,170,1)", stroke: 2 });
              if (now - lastShot > 240) footprints.push([g[0], g[1], "rgba(120,255,170,0.8)"]);
            });
          } else if (s.kind === "optical") {
            var hist = swaths[s.name] = swaths[s.name] || [];
            if (!hist.length || now - hist.stamp > 150) { hist.push([p[0], p[1], 0.003]); hist.stamp = now; }
            if (hist.length > 70) hist.shift();
            if (hist.length > 1) paths.push({ pts: hist.slice(), color: ["rgba(255,226,138,0)", "rgba(255,226,138,0.5)"], stroke: 10 });
            var a = sideways(p, q, -1.4), b = sideways(p, q, 1.4);
            paths.push({ pts: [[a[0], a[1], 0.003], [p[0], p[1], s.h], [b[0], b[1], 0.003]], color: "rgba(255,226,138,0.85)", stroke: 1.6 });
          } else if (s.kind === "radar") {
            var side = sideways(p, q, 2.2);       /* radar looks to one side */
            paths.push({ pts: [[p[0], p[1], s.h], [side[0], side[1], 0.003]], color: "rgba(150,220,255,1)", stroke: 2 });
            if (now - lastPulse > 700) radarHits.push([side[0], side[1]]);
          }
        });

        if (now - lastShot > 240) {
          lastShot = now;
          if (footprints.length > 700) footprints.splice(0, footprints.length - 700);
          globe.pointsData(footprints.slice());
        }
        if (now - lastPulse > 700) {
          lastPulse = now;
          if (radarHits.length > 4) radarHits.splice(0, radarHits.length - 4);
          globe.ringsData(radarHits.slice());
        }

        globe.pathsData(paths);
        globe.htmlElementsData(marks);
      }
      requestAnimationFrame(frame);
    }
    requestAnimationFrame(frame);
  }

  /* NEON field sites as small orange dots; hover shows the site name.
     Uses the labels layer (dot only, no text) so the points layer stays free
     for the LiDAR footprints. */
  function neonSites(globe) {
    var sites = (window.NEON_SITES || []).map(function (r) {
      return { code: r[0], name: r[1], type: r[2], domain: r[3], lat: r[4], lng: r[5] };
    });
    globe
      .labelsData(sites)
      .labelLat("lat").labelLng("lng")
      .labelText(function () { return ""; })
      .labelSize(0)
      .labelDotRadius(function (d) { return d.type === "CORE" ? 0.55 : 0.42; })
      .labelColor(function (d) { return d.type === "CORE" ? "rgba(255,140,66,1)" : "rgba(255,190,120,0.95)"; })
      .labelAltitude(0.004)
      .labelResolution(1)
      .labelLabel(function (d) { return d.name + " (" + d.code + ", " + d.domain + ")"; })
      .labelsTransitionDuration(0);
  }

  function start() {
    var el = document.getElementById("data-globe");
    if (!el || !webglOK()) return;
    loadScript(GLOBE_JS)
      .then(function () { build(el); })
      .catch(function () { /* fallback sphere stays */ });
  }

  if (document.readyState === "complete") start();
  else window.addEventListener("load", start);
})();
