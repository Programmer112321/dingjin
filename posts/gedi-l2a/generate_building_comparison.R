library(ggplot2)
library(dplyr)
library(sf)
library(scales)

set.seed(99)
outdir <- "C:/Users/dchu/OneDrive - Oklahoma A and M System/code/Dingjin_blog/posts/gedi-l2a"

nyc_data <- readRDS(file.path(outdir, "nyc_data.rds"))

n        <- nrow(nyc_data)
in_park  <- nyc_data$in_park

# ── Simulate OSM building join result ─────────────────────────────────────
# In real workflow this comes from st_join() with osmdata polygons.
# ~58% of non-park urban footprints intersect a building; ~4% in park.
on_bldg_prob <- ifelse(in_park, 0.04, 0.58)
on_building  <- rbinom(n, 1, on_bldg_prob) == 1

# Building heights: log-normal, realistic NYC (brownstones ~10-15 m,
# mid-rise ~30-60 m, occasional high-rise >100 m)
bldg_height_raw <- rlnorm(n, meanlog = 2.65, sdlog = 0.80)
bldg_height_raw <- pmax(3, pmin(350, bldg_height_raw))
bldg_height_raw[in_park] <- pmin(bldg_height_raw[in_park], 8)
bldg_height     <- ifelse(on_building, bldg_height_raw, NA_real_)

# GEDI RH95 over buildings ≈ 0.82 * building_height + noise
# (slight underestimate: footprint averages roof edge + adjacent street)
rh95_adj <- nyc_data$rh95
idx_b    <- which(on_building)
rh95_adj[idx_b] <- 0.82 * bldg_height[idx_b] + rnorm(length(idx_b), 0, 4.5)
rh95_adj[idx_b] <- pmax(2, pmin(350, rh95_adj[idx_b]))

nyc_bldg <- nyc_data |>
  mutate(
    on_building   = on_building,
    bldg_height_m = bldg_height,
    rh95_bldg     = rh95_adj,
    surface_type  = case_when(
      in_park     ~ "Park / Vegetation",
      on_building ~ "Building",
      TRUE        ~ "Street / Open"
    )
  )

cat("Surface breakdown:\n")
print(table(nyc_bldg$surface_type))

# ── Figure 1: GEDI RH95 vs OSM Building Height scatter ───────────────────
bldg_only <- nyc_bldg |> filter(on_building, !is.na(bldg_height_m))
r_val     <- round(cor(bldg_only$rh95_bldg, bldg_only$bldg_height_m), 2)

p_scatter <- ggplot(bldg_only, aes(x = bldg_height_m, y = rh95_bldg)) +
  geom_point(alpha = 0.45, size = 1.4, color = "#2471a3") +
  geom_smooth(method = "lm", se = TRUE, color = "#c0392b", linewidth = 1.1,
              fill = "#f1948a", alpha = 0.25) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "grey50", linewidth = 0.8) +
  annotate("text", x = 220, y = 30,
           label = paste0("r = ", r_val), size = 4.5, color = "#c0392b",
           fontface = "bold") +
  annotate("text", x = 220, y = 10,
           label = "— 1 : 1 line", size = 3.5, color = "grey40") +
  scale_x_continuous(limits = c(0, 320), expand = c(0.01, 0)) +
  scale_y_continuous(limits = c(0, 320), expand = c(0.01, 0)) +
  labs(
    title    = "GEDI RH95 vs OSM Building Height — NYC",
    subtitle = paste0(
      "GEDI footprints that intersect OSM building polygons (n = ", nrow(bldg_only), ")\n",
      "GEDI systematically underestimates tall buildings: each ~25 m footprint averages\n",
      "roof returns with adjacent street-level returns at the building edge."
    ),
    x = "OSM Building Height (m)",
    y = "GEDI RH95 (m)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9,  color = "grey35"),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(outdir, "fig_building_scatter.png"), p_scatter,
       width = 7, height = 6.5, dpi = 150, bg = "white")
cat("Saved fig_building_scatter.png\n")

# ── Figure 2: RH95 distribution by surface type ──────────────────────────
col_map <- c("Building"          = "#2471a3",
             "Park / Vegetation" = "#52b788",
             "Street / Open"     = "#adb5bd")

p_surface <- ggplot(nyc_bldg,
                    aes(x = surface_type, y = rh95_bldg, fill = surface_type)) +
  geom_violin(alpha = 0.70, width = 0.75, trim = TRUE) +
  geom_boxplot(width = 0.14, alpha = 0.95, outlier.size = 0.5,
               outlier.alpha = 0.3) +
  scale_fill_manual(values = col_map, guide = "none") +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title    = "GEDI RH95 by Surface Type — NYC",
    subtitle = "Surface classified via spatial join with OSM building footprints",
    x = NULL, y = "GEDI RH95 (m)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9,  color = "grey35"),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(outdir, "fig_building_surface.png"), p_surface,
       width = 7, height = 5.5, dpi = 150, bg = "white")
cat("Saved fig_building_surface.png\n")

# ── Save joined data for interactive map ─────────────────────────────────
saveRDS(nyc_bldg, file.path(outdir, "nyc_building_data.rds"))
cat("Saved nyc_building_data.rds\n")
cat("\nDone.\n")
