/* Homepage globe: terrain-only relief Earth. A shaded-relief texture is built
   from an elevation map (light from the north-west, warm grey land, plain
   ocean), and the same elevation physically raises the land (exaggerated)
   on a high-resolution sphere. No satellites or points; visitors can drag it.
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
    return c.toDataURL("image/jpeg", 0.92);
  }

  function build(el, topoImg) {
    var dark = isDark();
    var reduce = window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;

    var globe = Globe({ animateIn: !reduce })(el)
      .backgroundColor("rgba(0,0,0,0)")
      .globeCurvatureResolution(0.6)            /* fine mesh so the raised terrain is smooth */
      .globeImageUrl(reliefTexture(topoImg, dark))
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
      } else if (tries > 100) {
        clearInterval(wait);
      }
    }, 100);

    var controls = globe.controls();
    controls.autoRotate = !reduce;
    controls.autoRotateSpeed = 0.4;
    controls.enableZoom = false;

    globe.pointOfView({ lat: 28, lng: 85, altitude: 2.2 }, 0);   /* start over the Himalaya */

    function fit() { globe.width(el.clientWidth).height(el.clientHeight); }
    fit();
    window.addEventListener("resize", fit);

    el.addEventListener("pointerdown", function () { controls.autoRotate = false; });
    el.__globe = globe;   /* handle for debugging in the browser console */
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
