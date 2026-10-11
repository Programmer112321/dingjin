library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(scales)
library(viridis)

set.seed(2025)
outdir <- "C:/Users/dchu/OneDrive - Oklahoma A and M System/code/Dingjin_blog/posts/gedi-urban-nyc"

# ── Study area: Central Park + surrounding Manhattan ─────────────────────────
# lon -74.02 to -73.90, lat 40.73 to 40.82

# Simulate GEDI track footprints (ascending + descending passes)
slope_asc  <-  1.256 * cos(40.77 * pi / 180)
slope_desc <- -slope_asc

make_track_shots <- function(lon_base, slope, lat_range, n = 50) {
  lats <- seq(lat_range[1], lat_range[2], length.out = n)
  data.frame(
    lon = lon_base + (lats - mean(lat_range)) / slope + rnorm(n, 0, 0.0003),
    lat = lats + rnorm(n, 0, 0.00012)
  )
}

lon_bases <- seq(-74.018, -73.902, by = 0.0054)

shots <- bind_rows(
  lapply(lon_bases, make_track_shots, slope = slope_asc,
         lat_range = c(40.732, 40.818), n = 52),
  lapply(lon_bases, make_track_shots, slope = slope_desc,
         lat_range = c(40.732, 40.818), n = 52)
) |>
  filter(lon >= -74.02, lon <= -73.90,
         lat >=  40.73, lat <=  40.82) |>
  distinct(round(lon, 4), round(lat, 4), .keep_all = TRUE)

n <- nrow(shots)
cat("Total shots:", n, "\n")

# ── Zone classification ───────────────────────────────────────────────────────
shots <- shots |>
  mutate(zone = case_when(
    lon >= -73.981 & lon <= -73.949 &
      lat >= 40.764 & lat <= 40.800              ~ "Central Park",
    lat >= 40.764 & lat <= 40.800 & lon < -73.981 ~ "Upper West Side",
    lat >= 40.764 & lat <= 40.800 & lon > -73.949 ~ "Upper East Side",
    lat > 40.800                                  ~ "North Manhattan",
    lat >= 40.748 & lat < 40.764                  ~ "Midtown",
    TRUE                                          ~ "Lower Manhattan"
  ))

cat("Zone breakdown:\n"); print(table(shots$zone))

# ── Surface type assignment per zone ─────────────────────────────────────────
surf_prob <- list(
  "Central Park"    = c(veg = 0.82, building = 0.04, street = 0.14),
  "Upper West Side" = c(veg = 0.09, building = 0.56, street = 0.35),
  "Upper East Side" = c(veg = 0.08, building = 0.58, street = 0.34),
  "Midtown"         = c(veg = 0.03, building = 0.63, street = 0.34),
  "Lower Manhattan" = c(veg = 0.02, building = 0.66, street = 0.32),
  "North Manhattan" = c(veg = 0.11, building = 0.50, street = 0.39)
)

shots$surface <- sapply(shots$zone, function(z) {
  p <- surf_prob[[z]]; sample(names(p), 1, prob = p)
})

# ── Building heights per zone (log-normal, metres) ───────────────────────────
bh_params <- list(
  "Central Park"    = c(ml = 1.5, sl = 0.30),  # small park structures ~4m
  "Upper West Side" = c(ml = 2.9, sl = 0.55),  # brownstones ~15m, apt ~25m
  "Upper East Side" = c(ml = 3.0, sl = 0.60),
  "Midtown"         = c(ml = 3.7, sl = 1.00),  # mix low-commercial + high-rise
  "Lower Manhattan" = c(ml = 3.8, sl = 1.05),
  "North Manhattan" = c(ml = 2.8, sl = 0.50)
)

shots$bldg_height <- mapply(function(z, s) {
  if (s != "building") return(NA_real_)
  p <- bh_params[[z]]
  h <- rlnorm(1, meanlog = p["ml"], sdlog = p["sl"])
  pmax(3, pmin(430, h))
}, shots$zone, shots$surface)

# ── Quality flag: vegetation passes most; buildings/streets fail more ─────────
qp <- c(veg = 0.88, building = 0.50, street = 0.44)
shots$quality_flag <- as.integer(mapply(function(s) rbinom(1, 1, qp[s]),
                                        shots$surface))

# ── RH metrics ───────────────────────────────────────────────────────────────
# Vegetation: broad waveform — low RH25 (ground/understory) + high RH95 (canopy)
# Building:   narrow waveform — all RH metrics cluster near roof height
# Street:     flat low returns — all RH metrics near zero

bh_fill <- ifelse(!is.na(shots$bldg_height), shots$bldg_height, 12)

shots <- shots |>
  mutate(
    # vegetation RH (broad spread)
    v25  = pmax(1.0, rnorm(n, 5.5,  2.5)),
    v50  = pmax(v25, rnorm(n, 12.0, 3.5)),
    v75  = pmax(v50, rnorm(n, 17.5, 3.5)),
    v95  = pmax(v75, rnorm(n, 22.0, 4.5)),
    v98  = pmax(v95, rnorm(n, 24.5, 5.0)),

    # building RH (narrow, scaled to roof height; footprint edge averages down)
    b25  = pmax(0.5, 0.54 * bh_fill + rnorm(n, 0, 2.0)),
    b50  = pmax(b25, 0.70 * bh_fill + rnorm(n, 0, 2.5)),
    b75  = pmax(b50, 0.81 * bh_fill + rnorm(n, 0, 3.0)),
    b95  = pmax(b75, 0.87 * bh_fill + rnorm(n, 0, 3.5)),
    b98  = pmax(b95, 0.90 * bh_fill + rnorm(n, 0, 4.0)),

    # street RH (near-zero)
    s25  = pmax(0, rnorm(n, 0.6, 0.5)),
    s50  = pmax(s25, rnorm(n, 1.0, 0.7)),
    s75  = pmax(s50, rnorm(n, 1.6, 0.9)),
    s95  = pmax(s75, rnorm(n, 2.4, 1.2)),
    s98  = pmax(s95, rnorm(n, 2.9, 1.4)),

    rh25 = case_when(surface == "veg" ~ v25, surface == "building" ~ b25, TRUE ~ s25),
    rh50 = case_when(surface == "veg" ~ v50, surface == "building" ~ b50, TRUE ~ s50),
    rh75 = case_when(surface == "veg" ~ v75, surface == "building" ~ b75, TRUE ~ s75),
    rh95 = case_when(surface == "veg" ~ v95, surface == "building" ~ b95, TRUE ~ s95),
    rh98 = case_when(surface == "veg" ~ v98, surface == "building" ~ b98, TRUE ~ s98),

    waveform_width = rh95 - rh25
  ) |>
  select(-v25, -v50, -v75, -v95, -v98,
         -b25, -b50, -b75, -b95, -b98,
         -s25, -s50, -s75, -s95, -s98)

shots_clean <- shots |> filter(quality_flag == 1)
cat("Quality-passing shots:", nrow(shots_clean), "\n")

# ── Palette helpers ───────────────────────────────────────────────────────────
zone_cols <- c(
  "Central Park"    = "#2ecc71",
  "Upper West Side" = "#3498db",
  "Upper East Side" = "#9b59b6",
  "Midtown"         = "#e67e22",
  "Lower Manhattan" = "#e74c3c",
  "North Manhattan" = "#1abc9c"
)
surf_cols <- c(veg = "#27ae60", building = "#2980b9", street = "#95a5a6")
surf_labs <- c(veg = "Vegetation", building = "Building", street = "Street / Open")

# ── Figure 1: Spatial overview — footprints coloured by zone ─────────────────
p_zone <- ggplot(shots_clean, aes(x = lon, y = lat, color = zone)) +
  geom_point(size = 1.1, alpha = 0.75) +
  scale_color_manual(values = zone_cols, name = "Zone") +
  annotate("rect",
           xmin = -73.981, xmax = -73.949, ymin = 40.764, ymax = 40.800,
           fill = NA, color = "#27ae60", linewidth = 0.8, linetype = "dashed") +
  coord_equal() +
  labs(
    title    = "GEDI L2A Footprints — Central Park & Surrounding Manhattan",
    subtitle = "Quality-passing shots (quality_flag = 1) coloured by urban zone",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 9, color = "grey35"),
        legend.position = "right")

ggsave(file.path(outdir, "fig_zone_spatial.png"), p_zone,
       width = 9, height = 6, dpi = 150, bg = "white")
cat("Saved fig_zone_spatial.png\n")

# ── Figure 2: RH95 distribution by zone ──────────────────────────────────────
p_rh95_zone <- ggplot(shots_clean, aes(x = reorder(zone, rh95, median),
                                        y = rh95, fill = zone)) +
  geom_violin(alpha = 0.70, width = 0.8, trim = TRUE) +
  geom_boxplot(width = 0.12, alpha = 0.95, outlier.size = 0.5, outlier.alpha = 0.3) +
  scale_fill_manual(values = zone_cols, guide = "none") +
  coord_flip() +
  labs(
    title    = "GEDI RH95 by Urban Zone",
    subtitle = "Central Park shows tallest canopy; Midtown shows widest spread from buildings",
    x = NULL, y = "RH95 (m)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 9, color = "grey35"))

ggsave(file.path(outdir, "fig_rh95_zone.png"), p_rh95_zone,
       width = 9, height = 5.5, dpi = 150, bg = "white")
cat("Saved fig_rh95_zone.png\n")

# ── Figure 3: Waveform width (RH95–RH25) — the key diagnostic ────────────────
shots_clean <- shots_clean |>
  mutate(surf_label = surf_labs[surface])

p_width <- ggplot(shots_clean,
                  aes(x = waveform_width, fill = surf_label)) +
  geom_histogram(binwidth = 3, color = "white", alpha = 0.85) +
  facet_wrap(~surf_label, ncol = 3, scales = "free_y") +
  scale_fill_manual(values = c("Vegetation"    = "#27ae60",
                                "Building"      = "#2980b9",
                                "Street / Open" = "#95a5a6"),
                    guide = "none") +
  scale_x_continuous(limits = c(0, 60), expand = c(0, 0)) +
  labs(
    title    = "Waveform Width (RH95 − RH25) by Surface Type",
    subtitle = "Vegetation returns are broad (canopy top vs understory); building returns are narrow (roof peak); streets near zero",
    x = "Waveform Width — RH95 − RH25 (m)",
    y = "Count"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 9,  color = "grey35"),
        strip.text    = element_text(face = "bold", size = 11))

ggsave(file.path(outdir, "fig_waveform_width.png"), p_width,
       width = 11, height = 4.5, dpi = 150, bg = "white")
cat("Saved fig_waveform_width.png\n")

# ── Figure 4: RH profile (RH25–RH95) for each surface type ───────────────────
rh_long <- shots_clean |>
  select(surface, rh25, rh50, rh75, rh95) |>
  pivot_longer(rh25:rh95, names_to = "metric", values_to = "height") |>
  mutate(
    metric     = factor(toupper(metric),
                        levels = c("RH25","RH50","RH75","RH95")),
    surf_label = surf_labs[surface]
  )

p_profile <- ggplot(rh_long, aes(x = metric, y = height, fill = surf_label)) +
  geom_boxplot(outlier.size = 0.4, outlier.alpha = 0.3,
               width = 0.5, alpha = 0.85) +
  scale_fill_manual(values = c("Vegetation"    = "#27ae60",
                                "Building"      = "#2980b9",
                                "Street / Open" = "#95a5a6"),
                    name = NULL) +
  labs(
    title    = "RH25–RH95 Vertical Profile by Surface Type",
    subtitle = "Vegetation profile rises steeply (tall canopy, low understory); building profile shifts uniformly (flat roof); streets stay near zero",
    x = "Relative Height Metric", y = "Height (m)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title      = element_text(face = "bold", size = 13),
        plot.subtitle   = element_text(size = 9, color = "grey35"),
        legend.position = "bottom")

ggsave(file.path(outdir, "fig_rh_profile.png"), p_profile,
       width = 9, height = 5.5, dpi = 150, bg = "white")
cat("Saved fig_rh_profile.png\n")

# ── Figure 5: GEDI RH95 vs building height scatter ───────────────────────────
bldg_shots <- shots_clean |> filter(surface == "building", !is.na(bldg_height))
r_val      <- round(cor(bldg_shots$rh95, bldg_shots$bldg_height), 2)

p_bldg <- ggplot(bldg_shots, aes(x = bldg_height, y = rh95, color = zone)) +
  geom_point(alpha = 0.45, size = 1.4) +
  geom_smooth(method = "lm", se = FALSE, color = "#c0392b",
              linewidth = 1.1, inherit.aes = FALSE,
              aes(x = bldg_height, y = rh95)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "grey45", linewidth = 0.8) +
  scale_color_manual(values = zone_cols, name = "Zone") +
  scale_x_continuous(limits = c(0, 350), expand = c(0.01, 0)) +
  scale_y_continuous(limits = c(0, 350), expand = c(0.01, 0)) +
  annotate("text", x = 240, y = 25, label = paste0("r = ", r_val),
           size = 4.5, color = "#c0392b", fontface = "bold") +
  labs(
    title    = "GEDI RH95 vs OSM Building Height — NYC",
    subtitle = "GEDI underestimates tall buildings; the gap widens for high-rises because the 25 m footprint averages the roof edge with the adjacent street",
    x = "OSM Building Height (m)",
    y = "GEDI RH95 (m)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 9, color = "grey35"),
        legend.position = "right")

ggsave(file.path(outdir, "fig_bldg_scatter.png"), p_bldg,
       width = 9, height = 6, dpi = 150, bg = "white")
cat("Saved fig_bldg_scatter.png\n")

# ── Figure 6: Quality flag pass rate by zone + surface type ──────────────────
qflag_summary <- shots |>
  group_by(zone, surface) |>
  summarise(pass_rate = mean(quality_flag), n = n(), .groups = "drop") |>
  mutate(surf_label = surf_labs[surface],
         zone = factor(zone, levels = c("Lower Manhattan","Midtown","Upper East Side",
                                        "Upper West Side","Central Park","North Manhattan")))

p_qflag <- ggplot(qflag_summary, aes(x = zone, y = pass_rate * 100,
                                      fill = surf_label)) +
  geom_col(position = "dodge", alpha = 0.85, width = 0.65) +
  scale_fill_manual(values = c("Vegetation"    = "#27ae60",
                                "Building"      = "#2980b9",
                                "Street / Open" = "#95a5a6"),
                    name = NULL) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  coord_flip() +
  labs(
    title    = "GEDI Quality Flag Pass Rate by Zone and Surface Type",
    subtitle = "Vegetation footprints pass QC at much higher rates than buildings or streets",
    x = NULL, y = "Pass Rate (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title      = element_text(face = "bold", size = 13),
        plot.subtitle   = element_text(size = 9,  color = "grey35"),
        legend.position = "bottom",
        panel.grid.minor = element_blank())

ggsave(file.path(outdir, "fig_quality_flag.png"), p_qflag,
       width = 9, height = 5, dpi = 150, bg = "white")
cat("Saved fig_quality_flag.png\n")

# ── Summary statistics ────────────────────────────────────────────────────────
stats <- shots_clean |>
  group_by(zone) |>
  summarise(
    `N shots`        = n(),
    `% Vegetation`   = round(100 * mean(surface == "veg"), 1),
    `% Building`     = round(100 * mean(surface == "building"), 1),
    `% Street`       = round(100 * mean(surface == "street"), 1),
    `Mean RH95 (m)`  = round(mean(rh95), 1),
    `Median RH95 (m)` = round(median(rh95), 1),
    `Mean Width (m)` = round(mean(waveform_width), 1),
    .groups = "drop"
  )
print(stats)
saveRDS(stats,        file.path(outdir, "summary_stats.rds"))
saveRDS(shots,        file.path(outdir, "shots_all.rds"))
saveRDS(shots_clean,  file.path(outdir, "shots_clean.rds"))

cat("\nAll figures saved to:", outdir, "\n")
