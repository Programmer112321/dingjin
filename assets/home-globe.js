/* Homepage globe: shaded-relief Earth (exaggerated raised terrain) with
   decades of warming painted over it: NASA GISTEMP decade-mean surface
   temperature anomalies vs 1951-1980 (assets/gistemp-decades.json), fading
   from the 1950s to the 2020s and looping. Visitors can drag the globe.
   Uses globe.gl (MIT) from jsDelivr. If WebGL or the CDN is unavailable,
   the CSS fallback sphere stays visible. */
(function () {
  "use strict";

  var GLOBE_JS = "https://cdn.jsdelivr.net/npm/globe.gl@2.46.2/dist/globe.gl.min.js";
  var TOPOLOGY = "https://cdn.jsdelivr.net/npm/three-globe@2.45.6/example/img/earth-topology.png";

  var RELIEF_EXAGGERATION = 3.2;   /* displacement in globe units (radius = 100) */

  function loadScript(src) {
    return new Promise(function (resolve, reject) {
      var s = document.createElement("script");
      s.src = src; s.async = true;
      s.onload = resolve; s.onerror = reject;
      document.head.appendChild(s);
    });
  }

  function loadImage(src) {
    return new Promise(function (resolve, reject) {
      var img = new Image();
      img.crossOrigin = "anonymous";
      img.onload = function () { resolve(img); };
      img.onerror = reject;
      img.src = src;
    });
  }

  function webglOK() {
    try {
      var c = document.createElement("canvas");
      return !!(window.WebGLRenderingContext && (c.getContext("webgl") || c.getContext("experimental-webgl")));
    } catch (e) { return false; }
  }

  function isDark() { return document.body.classList.contains("quarto-dark"); }

  /* Shaded relief from the elevation image (0 = ocean). */
  function reliefTexture(img, dark) {
    var W = 2048, H = 1024;
    var c = document.createElement("canvas");
    c.width = W; c.height = H;
    var ctx = c.getContext("2d");
    ctx.drawImage(img, 0, 0, W, H);
    var src = ctx.getImageData(0, 0, W, H);
    var p = src.data;

    /* elevation 0..1, stretched so lowland relief shows too */
    var z = new Float32Array(W * H);
    for (var i = 0, j = 0; j < z.length; i += 4, j++) z[j] = Math.pow(p[i] / 255, 0.5);

    var out = ctx.createImageData(W, H);
    var o = out.data;
    var az = 315 * Math.PI / 180, alt = 45 * Math.PI / 180;     /* NW light */
    var lx = Math.cos(alt) * Math.sin(az), ly = Math.cos(alt) * Math.cos(az), lz = Math.sin(alt);
    var ZF = 38;                                                 /* vertical factor for shading */
    var sea  = dark ? [30, 36, 44]    : [232, 236, 240];
    var low  = dark ? [150, 140, 124] : [222, 212, 194];         /* lowland tone */
    var high = dark ? [210, 202, 188] : [178, 160, 136];         /* highland tone */

    for (var y = 0; y < H; y++) {
      for (var x = 0; x < W; x++) {
        var k = y * W + x, q = k * 4;
        if (p[q] === 0) { o[q] = sea[0]; o[q + 1] = sea[1]; o[q + 2] = sea[2]; o[q + 3] = 255; continue; }
        var xl = z[y * W + ((x - 1 + W) % W)], xr = z[y * W + ((x + 1) % W)];
        var yu = z[Math.max(0, y - 1) * W + x], yd = z[Math.min(H - 1, y + 1) * W + x];
        var dx = (xr - xl) * ZF, dy = (yd - yu) * ZF;
        var n = Math.sqrt(dx * dx + dy * dy + 1);
        var shade = (-dx * lx + dy * ly + lz) / n;                /* -1..1 */
        shade = Math.max(0, Math.min(1, 0.35 + 0.75 * shade));
        var e = z[k];
        for (var ch = 0; ch < 3; ch++) {
          var base = low[ch] + (high[ch] - low[ch]) * e;
          o[q + ch] = Math.max(0, Math.min(255, base * (0.55 + 0.6 * shade)));
        }
        o[q + 3] = 255;
      }
    }
    ctx.putImageData(out, 0, 0);
    return c;
  }

  function build(el, topoImg) {
    var dark = isDark();
    var reliefCanvas = reliefTexture(topoImg, dark);
    var reduce = window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;

    var globe = Globe({ animateIn: !reduce })(el)
      .backgroundColor("rgba(0,0,0,0)")
      .globeCurvatureResolution(0.6)            /* fine mesh so the raised terrain is smooth */
      .globeImageUrl(reliefCanvas.toDataURL("image/jpeg", 0.92))
      .showAtmosphere(true)
      .atmosphereColor(dark ? "#3a4a5c" : "#c9d6e3")
      .atmosphereAltitude(0.12);

    var mat = globe.globeMaterial();
    mat.shininess = 2;

    /* raise the land: reuse the texture class three.js used for the globe map */
    var tries = 0;
    var wait = setInterval(function () {
      tries++;
      if (mat.map && mat.map.constructor) {
        clearInterval(wait);
        var disp = new mat.map.constructor(topoImg);
        disp.needsUpdate = true;
        mat.displacementMap = disp;
        mat.displacementScale = RELIEF_EXAGGERATION;
        mat.needsUpdate = true;
        var fb = el.parentNode.querySelector(".hp-globe-fallback");
        if (fb) fb.style.display = "none";
        temperature(el, mat, reliefCanvas, reduce);
      } else if (tries > 100) {
        clearInterval(wait);
      }
    }, 100);

    var controls = globe.controls();
    controls.autoRotate = !reduce;
    controls.autoRotateSpeed = 3.2;
    controls.enableZoom = false;

    globe.pointOfView({ lat: 28, lng: 85, altitude: 1.6 }, 0);   /* start over the Himalaya */

    function fit() { globe.width(el.clientWidth).height(el.clientHeight); }
    fit();
    window.addEventListener("resize", fit);

    el.addEventListener("pointerdown", function () { controls.autoRotate = false; });
    el.__globe = globe;   /* handle for debugging in the browser console */
  }

  /* ── Decades of warming: NASA GISTEMP decade-mean anomalies (vs 1951–1980)
     painted over the relief, fading from one decade to the next. ───────── */
  var TEMP_JSON = "assets/gistemp-decades.json";
  var STOPS = [            /* anomaly (°C) → colour */
    [-2.0, [33, 102, 172]], [-1.0, [103, 169, 207]], [-0.25, [209, 229, 240]],
    [0.0, [247, 247, 247]], [0.25, [253, 219, 199]], [1.0, [244, 165, 130]],
    [2.0, [214, 96, 77]], [3.0, [178, 24, 43]], [4.0, [103, 0, 31]]
  ];

  function tempColour(v) {
    if (v <= STOPS[0][0]) return STOPS[0][1];
    for (var i = 1; i < STOPS.length; i++) {
      if (v <= STOPS[i][0]) {
        var a = STOPS[i - 1], b = STOPS[i], t = (v - a[0]) / (b[0] - a[0]);
        return [a[1][0] + (b[1][0] - a[1][0]) * t, a[1][1] + (b[1][1] - a[1][1]) * t, a[1][2] + (b[1][2] - a[1][2]) * t];
      }
    }
    return STOPS[STOPS.length - 1][1];
  }

  function tempOverlay(box) {
    var d = document.createElement("div");
    d.className = "hp-temp";
    var grad = STOPS.filter(function (s) { return s[0] >= -2 && s[0] <= 4; })
      .map(function (s) { return "rgb(" + s[1].join(",") + ") " + ((s[0] + 2) / 6 * 100).toFixed(1) + "%"; }).join(",");
    d.innerHTML =
      '<div class="hp-temp-dec">1950s</div>' +
      '<div class="hp-temp-bar" style="background:linear-gradient(90deg,' + grad + ')"></div>' +
      '<div class="hp-temp-ticks"><span>−2</span><span>0</span><span>+2</span><span>+4 °C</span></div>' +
      '<div class="hp-temp-src">Surface temperature vs 1951–1980 · NASA GISTEMP</div>';
    box.appendChild(d);
    return d.querySelector(".hp-temp-dec");
  }

  function temperature(el, mat, reliefCanvas, reduce) {
    fetch(TEMP_JSON).then(function (r) { return r.json(); }).then(function (T) {
      var nx = T.nx, ny = T.ny, N = nx * ny;
      var W = reliefCanvas.width, H = reliefCanvas.height;

      var frame = document.createElement("canvas");
      frame.width = W; frame.height = H;
      var fctx = frame.getContext("2d");
      var cell = document.createElement("canvas");
      cell.width = nx; cell.height = ny;
      var cctx = cell.getContext("2d");
      var img = cctx.createImageData(nx, ny);

      var label = tempOverlay(el.parentNode);
      var last = T.decades.length - 1;
      if (T.decades[last] === "2020s" && T.last_year) T.decades[last] = "2020s (to " + T.last_year + ")";

      function paint(k, f) {                 /* decade k blended toward k+1 by f */
        var A = T.data[k], B = T.data[Math.min(k + 1, last)];
        for (var i = 0; i < N; i++) {
          var a = A[i], b = B[i], v;
          if (a === -9999 && b === -9999) { img.data[i * 4 + 3] = 0; continue; }
          if (a === -9999) v = b / 10; else if (b === -9999) v = a / 10;
          else v = (a + (b - a) * f) / 10;
          var c = tempColour(v);
          img.data[i * 4] = c[0]; img.data[i * 4 + 1] = c[1]; img.data[i * 4 + 2] = c[2];
          img.data[i * 4 + 3] = Math.round(255 * (0.3 + 0.5 * Math.min(1, Math.abs(v) / 2)));
        }
        cctx.putImageData(img, 0, 0);
        fctx.globalAlpha = 1;
        fctx.drawImage(reliefCanvas, 0, 0);
        fctx.imageSmoothingEnabled = true;
        fctx.globalAlpha = 0.8;
        fctx.drawImage(cell, 0, 0, W, H);
        mat.map.image = frame;
        mat.map.needsUpdate = true;
        label.textContent = T.decades[f < 0.5 ? k : Math.min(k + 1, last)];
      }

      if (reduce) { paint(last, 0); return; }

      var HOLD = 533, FADE = 433, END_HOLD = 1200;
      var k = 0, phase = "hold", t0 = performance.now(), lastPaint = 0;
      paint(0, 0);
      function tick(now) {
        if (!document.hidden) {
          var dt = now - t0;
          if (phase === "hold" && dt > (k === last ? END_HOLD : HOLD)) {
            phase = "fade"; t0 = now; dt = 0;
          }
          if (phase === "fade") {
            var f = Math.min(1, dt / FADE);
            if (now - lastPaint > 60 || f === 1) {
              lastPaint = now;
              if (k === last) paint(0, 0);   /* loop back to the 1950s */
              else paint(k, f);
            }
            if (f === 1 || k === last) {
              k = (k === last) ? 0 : k + 1;
              phase = "hold"; t0 = now;
              paint(k, 0);
            }
          }
        }
        requestAnimationFrame(tick);
      }
      requestAnimationFrame(tick);
    }).catch(function () { /* keep the plain relief globe */ });
  }

  function start() {
    var el = document.getElementById("hp-globe");
    if (!el || !webglOK()) return;
    Promise.all([loadScript(GLOBE_JS), loadImage(TOPOLOGY)])
      .then(function (r) { build(el, r[1]); })
      .catch(function () { /* fallback sphere stays */ });
  }

  if (document.readyState === "complete") start();
  else window.addEventListener("load", start);
})();
