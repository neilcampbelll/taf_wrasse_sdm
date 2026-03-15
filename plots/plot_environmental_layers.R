# plots/plot_environmental_layers.R
# 4-panel map of environmental predictors
# depth and dist_to_coast from environmental_grid (point layer, Scottish waters)
# SST and salinity read directly from source polygon layers (full study extent)

cat("Plotting environmental layers...\n")

coastline <- st_read(GEOPACKAGE_PATH, "Coastline",        quiet = TRUE)
grid      <- st_read(OUTPUT_PATH,     "environmental_grid", quiet = TRUE)

cat("  Columns in environmental_grid:", paste(names(grid), collapse = ", "), "\n")

# Read SST and salinity directly from source layers to avoid column name
# issues if the environmental_grid was built before the sst/sss rename fix
sst_layer <- st_read(GEOPACKAGE_PATH, "sst_temperature", quiet = TRUE)
sss_layer <- st_read(GEOPACKAGE_PATH, "sss_salinity",    quiet = TRUE)

# Detect column names — handle both old ("temperature"/"salinity") and
# new ("sst"/"sss") naming
sst_col <- intersect(c("sst", "temperature"), names(sst_layer))[1]
sss_col <- intersect(c("sss", "salinity"),    names(sss_layer))[1]

if (is.na(sst_col)) stop("Cannot find SST column in sst_temperature layer. Columns: ", paste(names(sst_layer), collapse = ", "))
if (is.na(sss_col)) stop("Cannot find salinity column in sss_salinity layer. Columns: ", paste(names(sss_layer), collapse = ", "))

cat("  Using SST column:", sst_col, "| Salinity column:", sss_col, "\n")

theme_env <- theme_minimal() +
  theme(axis.title        = element_blank(),
        panel.grid        = element_line(colour = "grey88"),
        legend.position   = "right",
        plot.title        = element_text(size = 10, face = "bold"),
        legend.key.height = unit(1.5, "cm"))

# Depth — points from environmental_grid
p_depth <- ggplot() +
  geom_sf(data = grid, aes(colour = depth_m), size = 0.3, shape = 15) +
  geom_sf(data = coastline, fill = "#ece8e1", colour = "grey60", linewidth = 0.2) +
  scale_colour_viridis_c(name = "m", option = "mako", direction = -1) +
  labs(title = "Depth") +
  theme_env

# Distance to coast — points from environmental_grid
p_dist <- ggplot() +
  geom_sf(data = grid, aes(colour = dist_to_coast_km), size = 0.3, shape = 15) +
  geom_sf(data = coastline, fill = "#ece8e1", colour = "grey60", linewidth = 0.2) +
  scale_colour_viridis_c(name = "km", option = "plasma", direction = -1) +
  labs(title = "Distance to coast") +
  theme_env

# SST — polygons from source layer
p_sst <- ggplot() +
  geom_sf(data = sst_layer, aes(fill = .data[[sst_col]]), colour = NA) +
  geom_sf(data = coastline, fill = "#ece8e1", colour = "grey60", linewidth = 0.2) +
  scale_fill_viridis_c(name = "°C", option = "inferno") +
  labs(title = "Sea surface temperature") +
  theme_env

# Salinity — polygons from source layer
p_sss <- ggplot() +
  geom_sf(data = sss_layer, aes(fill = .data[[sss_col]]), colour = NA) +
  geom_sf(data = coastline, fill = "#ece8e1", colour = "grey60", linewidth = 0.2) +
  scale_fill_viridis_c(name = "PSU", option = "viridis") +
  labs(title = "Sea surface salinity") +
  theme_env

p <- (p_depth | p_dist) / (p_sst | p_sss) +
  plot_annotation(title    = "Wrasse SDM — Environmental Predictors",
                  subtitle = "Depth and distance to coast: Scottish waters grid  |  SST and salinity: full study extent")

ggsave("output/environmental_layers.png", p, width = 14, height = 12, dpi = 150)
cat("Saved: output/environmental_layers.png\n")

rm(coastline, grid, sst_layer, sss_layer, sst_col, sss_col,
   theme_env, p_depth, p_dist, p_sst, p_sss, p)
