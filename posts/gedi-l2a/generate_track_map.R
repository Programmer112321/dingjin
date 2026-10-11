library(leaflet)
library(htmlwidgets)
library(dplyr)

outdir <- "C:/Users/dchu/OneDrive - Oklahoma A and M System/code/Dingjin_blog/posts/gedi-l2a"

# ── Helper: generate GEDI-like orbital tracks over a region ──────────────
# ISS orbital inclination ~51.6°
# Ascending (SW→NE) and descending (NW→SE) passes
# Cross-track spacing ~600 m ≈ 0.0054° longitude

make_gedi_tracks <- function(lon_ctr, lat_ctr, half_lon = 1.8, half_lat = 1.6,
                              n_tracks = 10, spacing = 0.055) {

  ext_lon <- half_lon * 2.5
  ext_lat <- half_lat * 2.5

  # Slope of track in geographic coords at mid-latitude
  # tan(51.6°) ≈ 1.256; corrected for longitude compression at lat
  slope <- 1.256 * cos(lat_ctr * pi / 180)

  tracks_asc  <- vector("list", n_tracks)
  tracks_desc <- vector("list", n_tracks)

  offsets <- seq(-(n_tracks %/% 2) * spacing,
                  (n_tracks %/% 2) * spacing,
                  by = spacing)[seq_len(n_tracks)]

  for (i in seq_along(offsets)) {
    dx <- offsets[i]

    # Ascending: SW → NE
    x1 <- lon_ctr + dx - ext_lon;  y1 <- lat_ctr - ext_lon * slope
    x2 <- lon_ctr + dx + ext_lon;  y2 <- lat_ctr + ext_lon * slope
    tracks_asc[[i]] <- list(lng = c(x1, x2), lat = c(y1, y2))

    # Descending: NW → SE
    x1d <- lon_ctr + dx - ext_lon; y1d <- lat_ctr + ext_lon * slope
    x2d <- lon_ctr + dx + ext_lon; y2d <- lat_ctr - ext_lon * slope
    tracks_desc[[i]] <- list(lng = c(x1d, x2d), lat = c(y1d, y2d))
  }

  list(asc = tracks_asc, desc = tracks_desc)
}

# ── Build combined map ────────────────────────────────────────────────────
scbi_lon_ctr <- (-78.20 + -78.10) / 2;  scbi_lat_ctr <- (38.85 + 38.95) / 2
nyc_lon_ctr  <- (-74.05 + -73.85) / 2;  nyc_lat_ctr  <- (40.70 + 40.85) / 2

scbi_tracks <- make_gedi_tracks(scbi_lon_ctr, scbi_lat_ctr,
                                 half_lon = 1.6, half_lat = 1.4,
                                 n_tracks = 11, spacing = 0.054)

nyc_tracks  <- make_gedi_tracks(nyc_lon_ctr, nyc_lat_ctr,
                                 half_lon = 1.5, half_lat = 1.3,
                                 n_tracks = 11, spacing = 0.054)

add_tracks <- function(map, tracks, color = "#1a7a1a", weight = 1.5, opacity = 0.75) {
  for (tr in c(tracks$asc, tracks$desc)) {
    map <- addPolylines(map, lng = tr$lng, lat = tr$lat,
                        color = color, weight = weight, opacity = opacity)
  }
  map
}

m <- leaflet(options = leafletOptions(zoomControl = TRUE)) |>
  addProviderTiles("OpenStreetMap") |>

  # ── SCBI tracks + bbox ──────────────────────────────────────────────────
  add_tracks(scbi_tracks, color = "#1a7a1a") |>
  addRectangles(
    lng1 = -78.20, lat1 = 38.85, lng2 = -78.10, lat2 = 38.95,
    color = "red", weight = 2.5, fill = FALSE,
    label = "NEON SCBI Study Area"
  ) |>

  # ── NYC tracks + bbox ───────────────────────────────────────────────────
  add_tracks(nyc_tracks, color = "#1a7a1a") |>
  addRectangles(
    lng1 = -74.05, lat1 = 40.70, lng2 = -73.85, lat2 = 40.85,
    color = "red", weight = 2.5, fill = FALSE,
    label = "Central Park / NYC Study Area"
  ) |>

  # ── Labels ──────────────────────────────────────────────────────────────
  addLabelOnlyMarkers(
    lng = scbi_lon_ctr, lat = 38.96,
    label = "NEON SCBI",
    labelOptions = labelOptions(noHide = TRUE, direction = "top",
                                style = list("font-weight" = "bold",
                                             "color" = "#c0392b",
                                             "font-size" = "12px",
                                             "background" = "transparent",
                                             "border" = "none",
                                             "box-shadow" = "none"))
  ) |>
  addLabelOnlyMarkers(
    lng = nyc_lon_ctr, lat = 40.86,
    label = "Central Park / NYC",
    labelOptions = labelOptions(noHide = TRUE, direction = "top",
                                style = list("font-weight" = "bold",
                                             "color" = "#c0392b",
                                             "font-size" = "12px",
                                             "background" = "transparent",
                                             "border" = "none",
                                             "box-shadow" = "none"))
  ) |>

  # ── Legend ───────────────────────────────────────────────────────────────
  addLegend(
    position = "bottomleft",
    colors   = c("#1a7a1a", "red"),
    labels   = c("GEDI Orbital Tracks", "Study Area Bounding Box"),
    title    = "GEDI L4A Track Coverage",
    opacity  = 1
  ) |>

  setView(lng = mean(c(scbi_lon_ctr, nyc_lon_ctr)),
          lat = mean(c(scbi_lat_ctr, nyc_lat_ctr)),
          zoom = 6)

saveWidget(m, file.path(outdir, "fig_track_map.html"),
           selfcontained = TRUE, title = "GEDI Track Coverage")

cat("Saved fig_track_map.html\n")
