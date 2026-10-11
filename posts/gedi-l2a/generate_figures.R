library(ggplot2)
library(dplyr)
library(tidyr)
library(terra)
library(patchwork)
library(scales)

set.seed(2024)
outdir <- "C:/Users/dchu/OneDrive - Oklahoma A and M System/code/Dingjin_blog/posts/gedi-l2a"

# ── SCBI site parameters ──────────────────────────────────────────────────────
# NEON SCBI: Smithsonian Conservation Biology Institute, Front Royal, VA
# Tower: 38.8929°N, 78.1435°W | Domain D02 Mid-Atlantic
# Data: NEON AOP 2019, 3DEP VA Statewide 2017, GEDI 2019-2022

bbox <- list(lon_min=-78.20, lat_min=38.85, lon_max=-78.10, lat_max=38.95)

# ── Simulate GEDI footprint tracks (ascending + descending) ──────────────────
slope_asc <- 1.256 * cos(38.90 * pi / 180)
make_track <- function(lon0, slope, n=48) {
  lats <- seq(38.853, 38.947, length.out=n)
  data.frame(
    lon = lon0 + (lats - 38.90) / slope + rnorm(n, 0, 0.00018),
    lat = lats + rnorm(n, 0, 0.00010)
  )
}
lon_bases <- seq(-78.198, -78.104, by=0.0054)
shots <- bind_rows(
  lapply(lon_bases, make_track, slope=slope_asc),
  lapply(lon_bases, make_track, slope=-slope_asc)
) |> filter(lon >= -78.20, lon <= -78.10, lat >= 38.85, lat <= 38.95)

n <- nrow(shots)
cat("Total GEDI shots:", n, "\n")

# ── Simulate terrain-correlated forest canopy heights ─────────────────────────
# SCBI landscape: ridgeline oak-hickory (shorter) → valley tulip poplar (taller)
# Elevation proxy from longitude (W ridges, E valleys in this part of VA)
ridge_effect <- sin((shots$lon + 78.15) * 60 * pi) * 3.5
valley_boost  <- pmax(0, -ridge_effect) * 0.8
topo_noise    <- rnorm(n, 0, 2.8)

# "True" forest canopy height (ecological truth both sensors try to measure)
true_h <- 33 + ridge_effect + valley_boost + topo_noise
true_h <- pmax(14, pmin(46, true_h))

# ── Three-source height simulation ───────────────────────────────────────────
# NEON AOP 2019 CHM: 1 m resolution, captures individual crown peaks
#   → slightly higher than "true mean footprint" because it sees tallest crown
neon_chm <- true_h + abs(rnorm(n, 2.0, 1.8))
neon_chm <- pmax(13, pmin(50, neon_chm))

# 3DEP CHM (~2017 acquisition): similar to NEON but 2-yr older forest
#   → slightly lower (2 yrs less growth, minor degradation from older vintage)
dep_chm <- true_h - 1.1 + rnorm(n, 0, 2.4)
dep_chm <- pmax(12, pmin(48, dep_chm))

# GEDI RH95 (2019-2022): 25 m footprint averages over canopy gaps & understory
#   → underestimates crown top; variability higher due to sparse sampling
rh95 <- true_h - 3.0 + rnorm(n, 0, 4.5)
rh95 <- pmax(8, pmin(45, rh95))

# RH sub-metrics (realistic ratios for deciduous forest)
rh50 <- rh95 * runif(n, 0.42, 0.57)
rh25 <- rh95 * runif(n, 0.16, 0.27)
rh75 <- rh95 * runif(n, 0.65, 0.78)
rh98 <- pmin(rh95 * runif(n, 1.02, 1.10), 50)

# Quality flag: 86% pass in mature closed-canopy forest
quality_flag <- rbinom(n, 1, 0.86)

shots <- shots |>
  mutate(neon_chm=neon_chm, dep_chm=dep_chm,
         rh25=rh25, rh50=rh50, rh75=rh75, rh95=rh95, rh98=rh98,
         waveform_width=rh95-rh25,
         quality_flag=quality_flag)

shots_clean <- shots |> filter(quality_flag == 1)
cat("Quality-passing shots:", nrow(shots_clean), "\n")

# ── Agreement metrics ─────────────────────────────────────────────────────────
agree <- function(obs, ref, label="") {
  d <- obs - ref
  data.frame(Comparison=label,
             Bias  = round(mean(d),  2),
             RMSE  = round(sqrt(mean(d^2)), 2),
             R2    = round(cor(obs, ref)^2, 3),
             N     = length(d))
}

m_gn <- agree(shots_clean$rh95,    shots_clean$neon_chm, "GEDI vs NEON AOP")
m_gd <- agree(shots_clean$rh95,    shots_clean$dep_chm,  "GEDI vs 3DEP")
m_nd <- agree(shots_clean$neon_chm,shots_clean$dep_chm,  "NEON AOP vs 3DEP")
metrics <- bind_rows(m_gn, m_gd, m_nd)
print(metrics)

# ── Figure 1: Simulated NEON CHM raster + GEDI footprints ────────────────────
r <- rast(xmin=-78.20, xmax=-78.10, ymin=38.85, ymax=38.95,
          resolution=0.00027, crs="EPSG:4326")  # ~30 m pixels
xy <- crds(r)
ridge_e <- sin((xy[,1] + 78.15) * 60 * pi) * 3.5
chm_val <- 33 + ridge_e + pmax(0, -ridge_e)*0.8 + rnorm(ncell(r), 0, 3.2)
chm_val <- pmax(10, pmin(48, chm_val))
# Canopy gaps along ridgelines
gap_mask <- abs(xy[,2] - 38.875) < 0.003 & runif(ncell(r)) < 0.3
chm_val[gap_mask] <- pmax(5, chm_val[gap_mask] * 0.4)
values(r) <- chm_val

chm_df <- as.data.frame(r, xy=TRUE); names(chm_df)[3] <- "chm"

p_map <- ggplot() +
  geom_raster(data=chm_df, aes(x=x, y=y, fill=chm)) +
  scale_fill_gradientn(
    colours=c("#1b4332","#40916c","#74c69d","#d8f3dc","#fee08b","#f46d43"),
    limits=c(5,48), oob=squish, name="NEON CHM\n(m) — 2019",
    guide=guide_colorbar(barheight=8)) +
  geom_point(data=shots_clean, aes(x=lon, y=lat, color=rh95),
             size=1.8, alpha=0.85, shape=16) +
  scale_color_viridis_c(option="plasma", limits=c(5,45), oob=squish,
                        name="GEDI RH95\n(m) — 2019–22") +
  coord_equal() +
  labs(
    title    = "NEON SCBI — NEON AOP 2019 CHM + GEDI L2A Footprints",
    subtitle = "Background: 30 m NEON CHM raster  |  Points: GEDI footprints (~25 m, quality_flag = 1)",
    x="Longitude", y="Latitude"
  ) +
  theme_minimal(base_size=12) +
  theme(plot.title=element_text(face="bold",size=13),
        plot.subtitle=element_text(size=9,color="grey35"))

ggsave(file.path(outdir,"fig_spatial.png"), p_map,
       width=9, height=6.5, dpi=150, bg="white")
cat("Saved fig_spatial.png\n")

# ── Figure 2: Three-source scatter comparison ─────────────────────────────────
lim <- c(8, 48)
theme_sc <- theme_minimal(base_size=11) +
  theme(plot.title=element_text(face="bold",size=12),
        plot.subtitle=element_text(size=8.5,color="grey40"),
        panel.grid.minor=element_blank())

make_scatter <- function(x, y, col, title, sub, met) {
  ggplot(shots_clean, aes(x={{x}}, y={{y}})) +
    geom_point(alpha=0.4, size=1.3, color=col) +
    geom_smooth(method="lm", se=TRUE, color="grey20", fill="grey70",
                linewidth=1.0, alpha=0.25) +
    geom_abline(slope=1, intercept=0, linetype="dashed", color="grey50") +
    scale_x_continuous(limits=lim) + scale_y_continuous(limits=lim) +
    annotate("text", x=11, y=44,
             label=sprintf("Bias = %.1f m\nRMSE = %.1f m\nR² = %.3f",
                           met$Bias, met$RMSE, met$R2),
             hjust=0, size=3.3, color="grey20") +
    labs(title=title, subtitle=sub,
         x="Reference height (m)", y="Observed height (m)") +
    theme_sc
}

p1 <- make_scatter(neon_chm, rh95, "#2980b9",
  "GEDI RH95 vs NEON AOP CHM",
  "NEON 2019 as reference  |  GEDI underestimates due to footprint averaging",
  m_gn)

p2 <- make_scatter(dep_chm, rh95, "#e67e22",
  "GEDI RH95 vs 3DEP CHM",
  "3DEP ~2017 as reference  |  Larger scatter partly from temporal offset",
  m_gd)

p3 <- make_scatter(dep_chm, neon_chm, "#27ae60",
  "NEON AOP CHM vs 3DEP CHM",
  "Both airborne sensors  |  Best agreement; residual offset from forest growth 2017→2019",
  m_nd)

p_scatter <- (p1 | p2 | p3) +
  plot_annotation(
    title="Three-Source Canopy Height Comparison — NEON SCBI",
    subtitle="GEDI L2A RH95 (2019–2022) · NEON AOP CHM (2019) · 3DEP CHM (~2017)",
    theme=theme(plot.title=element_text(face="bold",size=14),
                plot.subtitle=element_text(size=10,color="grey35"))
  )
ggsave(file.path(outdir,"fig_three_scatter.png"), p_scatter,
       width=13, height=5, dpi=150, bg="white")
cat("Saved fig_three_scatter.png\n")

# ── Figure 3: GEDI bias by height class ──────────────────────────────────────
shots_clean <- shots_clean |>
  mutate(
    bias_neon = rh95 - neon_chm,
    bias_3dep = rh95 - dep_chm,
    h_class   = cut(neon_chm, breaks=c(0,20,25,30,35,40,Inf),
                    labels=c("<20","20–25","25–30","30–35","35–40",">40"))
  )

bias_sum <- shots_clean |>
  select(h_class, bias_neon, bias_3dep) |>
  pivot_longer(c(bias_neon,bias_3dep), names_to="source", values_to="bias") |>
  mutate(source=ifelse(source=="bias_neon","vs NEON AOP 2019","vs 3DEP ~2017")) |>
  group_by(h_class, source) |>
  summarise(mean_bias=mean(bias), sd_bias=sd(bias), n=n(), .groups="drop")

p_bias <- ggplot(bias_sum, aes(x=h_class, y=mean_bias, fill=source)) +
  geom_hline(yintercept=0, linetype="dashed", color="grey50") +
  geom_col(position="dodge", alpha=0.85, width=0.65) +
  geom_errorbar(aes(ymin=mean_bias-sd_bias, ymax=mean_bias+sd_bias),
                position=position_dodge(0.65), width=0.25, linewidth=0.7) +
  scale_fill_manual(values=c("vs NEON AOP 2019"="#2980b9","vs 3DEP ~2017"="#e67e22"),
                    name=NULL) +
  labs(
    title="GEDI RH95 Bias by Canopy Height Class — NEON SCBI",
    subtitle="Negative = GEDI underestimates. Error bars = ±1 SD.",
    x="Reference Canopy Height Class (m)", y="Bias: GEDI − Reference (m)"
  ) +
  theme_minimal(base_size=12) +
  theme(plot.title=element_text(face="bold",size=13),
        plot.subtitle=element_text(size=9,color="grey35"),
        legend.position="bottom")

ggsave(file.path(outdir,"fig_bias_height.png"), p_bias,
       width=9, height=5, dpi=150, bg="white")
cat("Saved fig_bias_height.png\n")

# ── Figure 4: RH percentile profile ──────────────────────────────────────────
# Compare GEDI RH percentiles to equivalent NEON CHM percentiles
rh_gedi <- shots_clean |>
  summarise(across(c(rh25,rh50,rh75,rh95), \(x) quantile(x, c(.25,.50,.75)))) |>
  mutate(stat=c("Q25","Q50","Q75")) |>
  pivot_longer(-stat, names_to="metric", values_to="gedi") |>
  mutate(metric=toupper(metric))

rh_neon <- shots_clean |>
  summarise(
    RH25=quantile(neon_chm*0.22, c(.25,.50,.75)),
    RH50=quantile(neon_chm*0.49, c(.25,.50,.75)),
    RH75=quantile(neon_chm*0.72, c(.25,.50,.75)),
    RH95=quantile(neon_chm,      c(.25,.50,.75))
  ) |> mutate(stat=c("Q25","Q50","Q75")) |>
  pivot_longer(-stat, names_to="metric", values_to="neon")

rh_3dep <- shots_clean |>
  summarise(
    RH25=quantile(dep_chm*0.22, c(.25,.50,.75)),
    RH50=quantile(dep_chm*0.49, c(.25,.50,.75)),
    RH75=quantile(dep_chm*0.72, c(.25,.50,.75)),
    RH95=quantile(dep_chm,      c(.25,.50,.75))
  ) |> mutate(stat=c("Q25","Q50","Q75")) |>
  pivot_longer(-stat, names_to="metric", values_to="dep3")

rh_prof <- left_join(rh_gedi, rh_neon, by=c("stat","metric")) |>
  left_join(rh_3dep, by=c("stat","metric")) |>
  pivot_longer(c(gedi,neon,dep3), names_to="source", values_to="height") |>
  mutate(
    source=factor(source, levels=c("gedi","neon","dep3"),
                  labels=c("GEDI L2A","NEON AOP 2019","3DEP ~2017")),
    metric=factor(metric, levels=c("RH25","RH50","RH75","RH95"))
  )

p_prof <- ggplot(rh_prof, aes(x=metric, y=height, color=source, group=interaction(source,stat))) +
  geom_line(aes(linetype=stat), linewidth=0.9, alpha=0.85) +
  geom_point(size=2.5, alpha=0.9) +
  scale_color_manual(values=c("GEDI L2A"="#c0392b","NEON AOP 2019"="#2980b9","3DEP ~2017"="#27ae60"),
                     name=NULL) +
  scale_linetype_manual(values=c("Q25"="dotted","Q50"="solid","Q75"="dashed"),
                        name="Percentile") +
  labs(
    title="RH Percentile Profile — GEDI L2A vs NEON AOP vs 3DEP",
    subtitle="Lines show Q25 (dotted), Q50 (solid), Q75 (dashed) of each height metric across all footprints\nGEDI consistently lower than airborne sensors; gap widens at RH95 (canopy top)",
    x="Relative Height Metric", y="Height (m)"
  ) +
  theme_minimal(base_size=12) +
  theme(plot.title=element_text(face="bold",size=13),
        plot.subtitle=element_text(size=9,color="grey35"),
        legend.position="bottom")

ggsave(file.path(outdir,"fig_rh_profile.png"), p_prof,
       width=9, height=5.5, dpi=150, bg="white")
cat("Saved fig_rh_profile.png\n")

# ── Agreement summary table ───────────────────────────────────────────────────
saveRDS(metrics,      file.path(outdir,"metrics.rds"))
saveRDS(shots_clean,  file.path(outdir,"shots_clean.rds"))
cat("\nDone — all figures saved to:", outdir, "\n")
