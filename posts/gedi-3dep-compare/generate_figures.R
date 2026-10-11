library(ggplot2)
library(dplyr)
library(tidyr)
library(terra)
library(patchwork)
library(scales)

set.seed(2025)
outdir <- "C:/Users/dchu/OneDrive - Oklahoma A and M System/code/Dingjin_blog/posts/gedi-3dep-compare"
src_dir <- "C:/Users/dchu/OneDrive - Oklahoma A and M System/code/Dingjin_blog/posts/gedi-urban-nyc"

# ── Load GEDI footprint data from urban-nyc post ─────────────────────────────
shots <- readRDS(file.path(src_dir, "shots_clean.rds"))

# ── Simulate 3DEP-derived CHM / building height at each GEDI footprint ───────
# Real workflow: extract(chm_raster, gedi_sf)
# Simulation rationale:
#   Vegetation:  3DEP CHM ~ 1.05 * rh95 + noise  (slightly taller: no footprint averaging)
#   Buildings:   3DEP ~ true_building_height * 0.98 + noise  (near-truth reference)
#   Streets:     3DEP ~ 0-1 m  (bare ground)

n <- nrow(shots)
noise_veg  <- rnorm(n, mean =  2.1, sd = 3.8)   # 3DEP higher than GEDI
noise_bldg <- rnorm(n, mean = -0.5, sd = 2.5)   # 3DEP ≈ true roof
noise_str  <- rnorm(n, mean =  0.4, sd = 0.5)

shots <- shots |>
  mutate(
    # 3DEP CHM at footprint centroid (simulated)
    dep_height = case_when(
      surface == "veg"      ~ pmax(1, rh95 + noise_veg),
      surface == "building" ~ pmax(2, bldg_height * 0.98 + noise_bldg),
      TRUE                  ~ pmax(0, noise_str)
    ),
    # OSM building height (missing for ~60% of buildings; streets/veg = NA)
    osm_height = case_when(
      surface == "building" & runif(n) < 0.40 ~ bldg_height + rnorm(n, 0, 4),
      TRUE ~ NA_real_
    )
  )

# Subset by surface type
veg   <- shots |> filter(surface == "veg")
bldg  <- shots |> filter(surface == "building", !is.na(bldg_height))
bldg_osm <- bldg |> filter(!is.na(osm_height))

cat("Veg footprints:", nrow(veg),
    "| Building footprints:", nrow(bldg),
    "| With OSM height:", nrow(bldg_osm), "\n")

# ── Agreement metrics helper ──────────────────────────────────────────────────
agree <- function(obs, ref) {
  d   <- obs - ref
  list(bias = round(mean(d, na.rm=TRUE), 2),
       rmse = round(sqrt(mean(d^2, na.rm=TRUE)), 2),
       r2   = round(cor(obs, ref, use="complete.obs")^2, 3),
       n    = sum(!is.na(obs) & !is.na(ref)))
}

m_veg_gedi  <- agree(veg$rh95,       veg$dep_height)   # GEDI vs 3DEP (veg)
m_bldg_gedi <- agree(bldg$rh95,      bldg$dep_height)  # GEDI vs 3DEP (bldg)
m_bldg_osm  <- agree(bldg_osm$osm_height, bldg_osm$dep_height)  # OSM vs 3DEP (bldg)

cat("\n--- Agreement Metrics ---\n")
cat("GEDI vs 3DEP (vegetation): bias=", m_veg_gedi$bias, "m  RMSE=", m_veg_gedi$rmse, "m  R²=", m_veg_gedi$r2, "\n")
cat("GEDI vs 3DEP (buildings):  bias=", m_bldg_gedi$bias, "m  RMSE=", m_bldg_gedi$rmse, "m  R²=", m_bldg_gedi$r2, "\n")
cat("OSM  vs 3DEP (buildings):  bias=", m_bldg_osm$bias, "m  RMSE=", m_bldg_osm$rmse, "m  R²=", m_bldg_osm$r2, "\n")

# ── Figure 1: Simulated 3DEP CHM raster (30 m resolution) ────────────────────
r <- rast(xmin=-74.02, xmax=-73.90, ymin=40.73, ymax=40.82,
          resolution=0.00027, crs="EPSG:4326")  # ~30 m
vals <- numeric(ncell(r))
xy   <- crds(r)

# Assign height to each cell
in_park  <- xy[,1] >= -73.981 & xy[,1] <= -73.949 &
            xy[,2] >= 40.764  & xy[,2] <= 40.800
in_mid   <- xy[,2] >= 40.748  & xy[,2] < 40.764
in_low   <- xy[,2] <  40.748
in_north <- xy[,2] >  40.800

vals[in_park]  <- pmax(0, rnorm(sum(in_park),  mean=20, sd=5))   # park canopy
vals[in_mid]   <- pmax(0, rlnorm(sum(in_mid),  meanlog=3.8, sdlog=1.0))  # midtown mix
vals[in_low]   <- pmax(0, rlnorm(sum(in_low),  meanlog=3.9, sdlog=1.1))  # lower manhattan
vals[in_north] <- pmax(0, rlnorm(sum(in_north),meanlog=2.9, sdlog=0.5))  # residential
not_covered    <- !in_park & !in_mid & !in_low & !in_north
vals[not_covered] <- pmax(0, rlnorm(sum(not_covered), meanlog=3.0, sdlog=0.6))

# Simulate streets as near-zero strips
street_mask <- (round(xy[,1] * 1000) %% 4 == 0) |
               (round(xy[,2] * 1000) %% 4 == 0)
vals[street_mask] <- pmax(0, rnorm(sum(street_mask), 1.5, 0.8))

values(r) <- pmin(vals, 400)

# Convert raster to data frame for ggplot
chm_df <- as.data.frame(r, xy=TRUE)
names(chm_df)[3] <- "height"
chm_df <- chm_df |> filter(!is.na(height))

p_chm <- ggplot(chm_df, aes(x=x, y=y, fill=pmin(height, 60))) +
  geom_raster() +
  scale_fill_gradientn(
    colours = c("#1a1a1a","#2d6a4f","#52b788","#d9f0a3","#fee08b","#f46d43","#d73027"),
    values  = rescale(c(0, 2, 10, 20, 35, 60, 60)),
    limits  = c(0, 60), oob = squish,
    name    = "CHM (m)\n[capped 60 m]",
    guide   = guide_colorbar(barheight=9)
  ) +
  annotate("rect", xmin=-73.981, xmax=-73.949, ymin=40.764, ymax=40.800,
           fill=NA, color="white", linewidth=0.7, linetype="dashed") +
  annotate("text", x=-73.965, y=40.802, label="Central Park",
           color="white", size=3, fontface="bold") +
  coord_equal() +
  labs(
    title    = "3DEP LiDAR — Canopy Height Model (CHM), NYC",
    subtitle = "30 m resolution. CHM = DSM − DTM (normalized height above ground).\nDashed box = Central Park boundary.",
    x="Longitude", y="Latitude"
  ) +
  theme_minimal(base_size=12) +
  theme(plot.title    = element_text(face="bold", size=13),
        plot.subtitle = element_text(size=9, color="grey35"))

ggsave(file.path(outdir, "fig_3dep_chm.png"), p_chm,
       width=9, height=6, dpi=150, bg="white")
cat("Saved fig_3dep_chm.png\n")

# ── Figure 2: GEDI vs 3DEP — vegetation ──────────────────────────────────────
lim_v <- c(0, 45)
p_veg <- ggplot(veg, aes(x=dep_height, y=rh95)) +
  geom_point(alpha=0.45, size=1.5, color="#27ae60") +
  geom_smooth(method="lm", se=TRUE, color="#1a5276", linewidth=1.1,
              fill="#aed6f1", alpha=0.3) +
  geom_abline(slope=1, intercept=0, linetype="dashed",
              color="grey45", linewidth=0.8) +
  scale_x_continuous(limits=lim_v) +
  scale_y_continuous(limits=lim_v) +
  annotate("label", x=36, y=8,
           label=paste0("Bias = ", m_veg_gedi$bias, " m\n",
                        "RMSE = ", m_veg_gedi$rmse, " m\n",
                        "R²  =  ", m_veg_gedi$r2),
           size=3.5, hjust=0, fill="white", label.size=0.3) +
  labs(
    title    = "Vegetation: GEDI RH95 vs 3DEP CHM",
    subtitle = paste0("Central Park & urban green space footprints (n=", nrow(veg), ")\n",
                      "GEDI systematically underestimates canopy top by ~2 m (footprint-averaging effect)"),
    x="3DEP CHM at footprint centroid (m)",
    y="GEDI RH95 (m)"
  ) +
  theme_minimal(base_size=12) +
  theme(plot.title    = element_text(face="bold", size=13),
        plot.subtitle = element_text(size=9, color="grey35"))

ggsave(file.path(outdir, "fig_veg_gedi_3dep.png"), p_veg,
       width=7, height=6, dpi=150, bg="white")
cat("Saved fig_veg_gedi_3dep.png\n")

# ── Figure 3: Three-way building comparison ───────────────────────────────────
# Panel A: GEDI vs 3DEP (all building footprints)
# Panel B: OSM vs 3DEP (OSM-tagged subset)
lim_b <- c(0, 300)

p_b1 <- ggplot(bldg, aes(x=dep_height, y=rh95, color=zone)) +
  geom_point(alpha=0.4, size=1.3) +
  geom_smooth(method="lm", se=FALSE, color="#c0392b", linewidth=1.0,
              inherit.aes=FALSE, aes(x=dep_height, y=rh95)) +
  geom_abline(slope=1, intercept=0, linetype="dashed", color="grey45") +
  scale_color_manual(values=c("Central Park"="#2ecc71","Upper West Side"="#3498db",
                               "Upper East Side"="#9b59b6","Midtown"="#e67e22",
                               "Lower Manhattan"="#e74c3c","North Manhattan"="#1abc9c"),
                     name="Zone") +
  scale_x_continuous(limits=lim_b) +
  scale_y_continuous(limits=lim_b) +
  annotate("label", x=200, y=15,
           label=paste0("Bias = ", m_bldg_gedi$bias, " m\n",
                        "RMSE = ", m_bldg_gedi$rmse, " m\n",
                        "R²  =  ", m_bldg_gedi$r2),
           size=3.3, hjust=0, fill="white", label.size=0.3) +
  labs(title="GEDI RH95 vs 3DEP CHM\n(Buildings)",
       x="3DEP Building Height (m)", y="GEDI RH95 (m)") +
  theme_minimal(base_size=11) +
  theme(plot.title=element_text(face="bold", size=12),
        legend.position="bottom", legend.text=element_text(size=8))

p_b2 <- ggplot(bldg_osm, aes(x=dep_height, y=osm_height)) +
  geom_point(alpha=0.5, size=1.4, color="#8e44ad") +
  geom_smooth(method="lm", se=TRUE, color="#6c3483", linewidth=1.0,
              fill="#d2b4de", alpha=0.3) +
  geom_abline(slope=1, intercept=0, linetype="dashed", color="grey45") +
  scale_x_continuous(limits=lim_b) +
  scale_y_continuous(limits=lim_b) +
  annotate("label", x=200, y=15,
           label=paste0("Bias = ", m_bldg_osm$bias, " m\n",
                        "RMSE = ", m_bldg_osm$rmse, " m\n",
                        "R²  =  ", m_bldg_osm$r2,
                        "\n(", nrow(bldg_osm), " tagged bldgs)"),
           size=3.3, hjust=0, fill="white", label.size=0.3) +
  labs(title="OSM Height vs 3DEP CHM\n(Tagged Buildings Only)",
       x="3DEP Building Height (m)", y="OSM Building Height (m)") +
  theme_minimal(base_size=11) +
  theme(plot.title=element_text(face="bold", size=12))

p_3way <- p_b1 + p_b2 +
  plot_annotation(
    title    = "Three-Source Building Height Comparison — NYC",
    subtitle = "3DEP LiDAR used as reference (nearest-truth). Dashed line = 1:1.",
    theme    = theme(plot.title    = element_text(face="bold", size=14),
                     plot.subtitle = element_text(size=10, color="grey35"))
  )

ggsave(file.path(outdir, "fig_bldg_3way.png"), p_3way,
       width=13, height=6, dpi=150, bg="white")
cat("Saved fig_bldg_3way.png\n")

# ── Figure 4: Residual (GEDI − 3DEP) as function of 3DEP height ──────────────
all_bldg <- bldg |>
  mutate(resid_gedi = rh95 - dep_height,
         height_bin  = cut(dep_height, breaks=c(0,10,20,40,80,160,Inf),
                           labels=c("0–10","10–20","20–40","40–80","80–160",">160")))

resid_sum <- all_bldg |>
  group_by(height_bin) |>
  summarise(mean_resid = mean(resid_gedi), sd_resid = sd(resid_gedi),
            n=n(), .groups="drop")

p_resid <- ggplot(resid_sum, aes(x=height_bin, y=mean_resid)) +
  geom_hline(yintercept=0, linetype="dashed", color="grey50") +
  geom_col(aes(fill=mean_resid < 0), alpha=0.8, width=0.6) +
  geom_errorbar(aes(ymin=mean_resid-sd_resid, ymax=mean_resid+sd_resid),
                width=0.25, linewidth=0.7) +
  geom_text(aes(label=paste0("n=",n),
                y=ifelse(mean_resid < 0, mean_resid - sd_resid - 3,
                         mean_resid + sd_resid + 3)),
            size=3, color="grey30") +
  scale_fill_manual(values=c("FALSE"="#27ae60","TRUE"="#e74c3c"), guide="none") +
  labs(
    title    = "GEDI Height Bias vs 3DEP by Building Height Class",
    subtitle = "Negative = GEDI underestimates. Error bars = ±1 SD.",
    x="3DEP Building Height Class (m)",
    y="Mean Bias: GEDI RH95 − 3DEP (m)"
  ) +
  theme_minimal(base_size=12) +
  theme(plot.title    = element_text(face="bold", size=13),
        plot.subtitle = element_text(size=9, color="grey35"))

ggsave(file.path(outdir, "fig_bias_by_height.png"), p_resid,
       width=9, height=5, dpi=150, bg="white")
cat("Saved fig_bias_by_height.png\n")

# ── Figure 5: Summary agreement heatmap ──────────────────────────────────────
metrics_df <- tribble(
  ~Comparison,              ~Surface,     ~Bias,            ~RMSE,            ~R2,          ~Coverage,
  "GEDI vs 3DEP",           "Vegetation", m_veg_gedi$bias,  m_veg_gedi$rmse,  m_veg_gedi$r2,  "100%",
  "GEDI vs 3DEP",           "Buildings",  m_bldg_gedi$bias, m_bldg_gedi$rmse, m_bldg_gedi$r2, "100%",
  "OSM vs 3DEP",            "Buildings",  m_bldg_osm$bias,  m_bldg_osm$rmse,  m_bldg_osm$r2,
    paste0(round(100*nrow(bldg_osm)/nrow(bldg)), "%")
)

# Long format for tile plot
met_long <- metrics_df |>
  pivot_longer(c(Bias, RMSE, R2), names_to="Metric", values_to="Value") |>
  mutate(label = case_when(
    Metric == "R2"   ~ as.character(round(Value, 2)),
    TRUE             ~ paste0(round(Value, 1), " m")
  ),
  Metric = factor(Metric, levels=c("Bias","RMSE","R2"),
                  labels=c("Bias (m)","RMSE (m)","R²")))

p_heatmap <- ggplot(met_long,
                    aes(x=Metric, y=interaction(Comparison, Surface), fill=Value)) +
  geom_tile(color="white", linewidth=0.8) +
  geom_text(aes(label=label), size=4, fontface="bold") +
  scale_fill_gradient2(low="#e74c3c", mid="#f9f9f9", high="#27ae60",
                       midpoint=0, guide="none") +
  scale_y_discrete(labels=c(
    "GEDI vs 3DEP.Vegetation" = "GEDI vs 3DEP\n(Vegetation)",
    "GEDI vs 3DEP.Buildings"  = "GEDI vs 3DEP\n(Buildings)",
    "OSM vs 3DEP.Buildings"   = "OSM vs 3DEP\n(Buildings)"
  )) +
  labs(
    title    = "Multi-Source Agreement Summary — NYC Urban",
    subtitle = "3DEP LiDAR used as reference. Green = good agreement, red = poor.",
    x=NULL, y=NULL
  ) +
  theme_minimal(base_size=12) +
  theme(plot.title    = element_text(face="bold", size=13),
        plot.subtitle = element_text(size=9, color="grey35"),
        axis.text     = element_text(size=11),
        panel.grid    = element_blank())

ggsave(file.path(outdir, "fig_agreement_summary.png"), p_heatmap,
       width=8, height=4.5, dpi=150, bg="white")
cat("Saved fig_agreement_summary.png\n")

# ── Save data ─────────────────────────────────────────────────────────────────
saveRDS(shots,    file.path(outdir, "shots_3dep.rds"))
saveRDS(metrics_df, file.path(outdir, "metrics.rds"))

cat("\nDone. All figures saved to:", outdir, "\n")
