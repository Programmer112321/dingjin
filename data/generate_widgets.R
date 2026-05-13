library(lidR)
library(plotly)
library(htmlwidgets)
library(sf)
library(leaflet)

out_dir <- "C:/Users/dchu/OneDrive - Oklahoma A and M System/code/Dingjin_blog/widgets"
dir.create(out_dir, showWarnings = FALSE)

# ── 1. Central Park — 3D elevation plot ───────────────────────────────────────
cat("Reading central_park.laz...\n")
cp     <- readLAS("C:/Users/dchu/OneDrive - Oklahoma A and M System/code/Dingjin_blog/data/central_park.laz")
cp_sub <- decimate_points(cp, random(density = 0.05))
df_cp  <- as.data.frame(cp_sub@data[, c("X", "Y", "Z")])
df_cp$Xn <- df_cp$X - min(df_cp$X)
df_cp$Yn <- df_cp$Y - min(df_cp$Y)

fig_cp <- plot_ly(
  df_cp, x = ~Xn, y = ~Yn, z = ~Z,
  type = "scatter3d", mode = "markers",
  marker = list(
    size        = 1.2,
    opacity     = 0.85,
    color       = df_cp$Z,
    colorscale  = "Viridis",
    showscale   = TRUE,
    colorbar    = list(title = "Elevation (m)", thickness = 15)
  ),
  text      = ~paste("Elevation:", round(Z, 1), "m"),
  hoverinfo = "text"
) |>
  layout(
    scene = list(
      xaxis  = list(title = "Easting (m)",  showgrid = FALSE),
      yaxis  = list(title = "Northing (m)", showgrid = FALSE),
      zaxis  = list(title = "Elevation (m)"),
      camera = list(eye = list(x = 1.5, y = -1.5, z = 0.8))
    ),
    margin = list(l = 0, r = 0, b = 0, t = 0)
  )

cat("Saving central_park_3d.html...\n")
saveWidget(fig_cp, file.path(out_dir, "central_park_3d.html"),
           selfcontained = TRUE, title = "Central Park LiDAR — Elevation")
cat("Done.\n")


# ── 2. Central Park — Leaflet location map ────────────────────────────────────
cat("Building Leaflet map...\n")

bbox_utm <- st_sfc(
  st_polygon(list(matrix(
    c(585000, 4512000,
      586500, 4512000,
      586500, 4513500,
      585000, 4513500,
      585000, 4512000),
    ncol = 2, byrow = TRUE
  ))),
  crs = 26918
)
bbox_wgs <- st_transform(bbox_utm, 4326)
bb  <- st_bbox(bbox_wgs)
cx  <- as.numeric((bb["xmin"] + bb["xmax"]) / 2)
cy  <- as.numeric((bb["ymin"] + bb["ymax"]) / 2)

map_cp <- leaflet() |>
  addProviderTiles("Esri.WorldImagery",
                   options = providerTileOptions(maxZoom = 19)) |>
  addProviderTiles("CartoDB.PositronOnlyLabels",
                   options = providerTileOptions(maxZoom = 19, opacity = 0.8)) |>
  setView(lng = cx, lat = cy, zoom = 14) |>
  addPolygons(
    data        = bbox_wgs,
    color       = "#F03B20", weight = 2,
    fillColor   = "#F03B20", fillOpacity = 0.15,
    popup       = "USGS 3DEP LiDAR tile (~2 km² near Central Park)"
  ) |>
  addScaleBar(position = "bottomleft")

cat("Saving central_park_map.html...\n")
saveWidget(map_cp, file.path(out_dir, "central_park_map.html"),
           selfcontained = TRUE, title = "Central Park Location Map")
cat("Done.\n")


# ── 3. Tree plot — 3D elevation plot ─────────────────────────────────────────
cat("Reading tree.laz...\n")
tr    <- readLAS("C:/Users/dchu/OneDrive - Oklahoma A and M System/code/Dingjin_blog/data/tree.laz")
df_tr <- as.data.frame(tr@data[, c("X", "Y", "Z")])
df_tr$Xn <- df_tr$X - min(df_tr$X)
df_tr$Yn <- df_tr$Y - min(df_tr$Y)

fig_tr <- plot_ly(
  df_tr, x = ~Xn, y = ~Yn, z = ~Z,
  type = "scatter3d", mode = "markers",
  marker = list(
    size        = 2.5,
    opacity     = 0.9,
    color       = df_tr$Z,
    colorscale  = list(
      list(0.0, "#08301B"),
      list(0.2, "#1A6B3C"),
      list(0.5, "#41AB5D"),
      list(0.8, "#A1D99B"),
      list(1.0, "#FFFFCC")
    ),
    showscale   = TRUE,
    colorbar    = list(title = "Elevation (m)", thickness = 15)
  ),
  text      = ~paste("Elevation:", round(Z, 2), "m"),
  hoverinfo = "text"
) |>
  layout(
    scene = list(
      xaxis  = list(title = "Easting (m)",  showgrid = FALSE),
      yaxis  = list(title = "Northing (m)", showgrid = FALSE),
      zaxis  = list(title = "Elevation (m)"),
      camera = list(eye = list(x = 1.5, y = -1.5, z = 1.2))
    ),
    margin = list(l = 0, r = 0, b = 0, t = 0)
  )

cat("Saving tree_3d.html...\n")
saveWidget(fig_tr, file.path(out_dir, "tree_3d.html"),
           selfcontained = TRUE, title = "Tree Plot LiDAR — Elevation")
cat("Done.\n")
cat("All widgets generated.\n")
