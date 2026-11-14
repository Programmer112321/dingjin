/* Homepage globe: true-colour Earth (NASA Blue Marble, public domain) with
   elevation relief, slowly turning; visitors can drag it.
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
        var fb = document.querySelector(".hp-globe-fallback");
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

    globe.pointOfView({ lat: 32, lng: -40, altitude: 2.25 }, 0);

    function fit() {
      globe.width(el.clientWidth).height(el.clientHeight);
    }
    fit();
    window.addEventListener("resize", fit);

    /* pause rotation while the visitor is dragging */
    el.addEventListener("pointerdown", function () { controls.autoRotate = false; });
  }

  function start() {
    var el = document.getElementById("hp-globe");
    if (!el || !webglOK()) return;
    loadScript(GLOBE_JS)
      .then(function () { build(el); })
      .catch(function () { /* fallback sphere stays */ });
  }

  if (document.readyState === "complete") start();
  else window.addEventListener("load", start);
})();
