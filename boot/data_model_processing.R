# ============================================================================
# Before: Environmental layers and Scottish boundaries
# After: Grid of points in Scottish inshore waters with all environmental values
# ============================================================================

cat("Processing Scottish Data Grid\n")

# Set up modelling data geopackage
if (!exists("OUTPUT_PATH")) {
  OUTPUT_PATH <- "data/modelling_data.gpkg"
}
cat("Modelling data will be saved to:", OUTPUT_PATH, "\n\n")

# Load points with distances
cat("Loading Scottish waters distance grid as base...\n")
scottish_waters <- st_read(GEOPACKAGE_PATH, "Scottish_Coastal_Distances", quiet = TRUE)
cat("Loaded\n")

# Extract bathymetry values
cat("\nLoading bathymetry values...\n")
bathy_layer <- st_read(GEOPACKAGE_PATH, "bathymetry", quiet = TRUE)
cat("Loaded\n")

cat("Finding nearest depth to each point\n")
nearest_bathy <- st_nearest_feature(scottish_waters, bathy_layer)
scottish_waters$depth_m <- bathy_layer$depth_m[nearest_bathy]
scottish_waters$depth_m[scottish_waters$depth_m > 0] <- 0

cat("Added bathymetry values\n")
cat("  Range:", round(min(scottish_waters$depth_m, na.rm = TRUE), 1), "to",
    round(max(scottish_waters$depth_m, na.rm = TRUE), 1), "m\n")

# Extract salinity values
cat("\nLoading salinity values...\n")
salinity_layer <- st_read(GEOPACKAGE_PATH, "sss_salinity", quiet = TRUE)
cat("Loaded\n")
cat("  Salinity layer has", nrow(salinity_layer), "features\n")

nearest_sal <- st_nearest_feature(scottish_waters, salinity_layer)
scottish_waters$sss <- salinity_layer$sss[nearest_sal]

# Extract temperature values
cat("\nLoading temperature values...\n")
temp_layer <- st_read(GEOPACKAGE_PATH, "sst_temperature", quiet = TRUE)
cat("Loaded\n")

nearest_temp <- st_nearest_feature(scottish_waters, temp_layer)
scottish_waters$sst <- temp_layer$sst[nearest_temp]

# Extract habitat values
cat("\nLoading habitat values...\n")
habitat_layer <- st_read(GEOPACKAGE_PATH, "MSFD_Habitats", quiet = TRUE)
cat("Loaded\n")

# Transform to BNG for accurate intersection
grid_proj    <- st_transform(scottish_waters, 27700)
habitat_proj <- st_transform(habitat_layer, 27700)

grid_proj <- st_join(grid_proj, habitat_proj, join = st_intersects, left = TRUE)

# Fill missing habitat values with nearest neighbour
missing_habitat   <- is.na(grid_proj$MSFD_BBHT) | trimws(grid_proj$MSFD_BBHT) == ""
points_with_values <- !missing_habitat

if (sum(missing_habitat) > 0) {
  nearest_indices <- st_nearest_feature(grid_proj[missing_habitat, ],
                                        grid_proj[points_with_values, ])
  grid_proj$MSFD_BBHT[missing_habitat] <- grid_proj$MSFD_BBHT[points_with_values][nearest_indices]
}

scottish_waters <- st_transform(grid_proj, 4326)
cat("Added habitat values\n")

# Summary
cat("Total prediction points:", nrow(scottish_waters), "\n")

vars_to_check <- c("dist_to_coast_km", "depth_m", "sss", "sst")
for (var in vars_to_check) {
  if (var %in% names(scottish_waters)) {
    values    <- scottish_waters[[var]]
    n_missing <- sum(is.na(values))
    if (n_missing == 0) {
      var_range <- range(values, na.rm = TRUE)
      cat(sprintf("%s: %.2f to %.2f (no missing values)\n", var, var_range[1], var_range[2]))
    } else {
      cat(sprintf("%s: %d missing values (%.1f%%)\n",
                  var, n_missing, 100 * n_missing / nrow(scottish_waters)))
    }
  }
}

if ("MSFD_BBHT" %in% names(scottish_waters)) {
  hab_counts <- table(scottish_waters$MSFD_BBHT, useNA = "ifany")
  cat("Habitat types:", length(hab_counts), "categories\n")
  print(hab_counts)
}

# Save prediction grid to modelling geopackage
st_write(scottish_waters, OUTPUT_PATH, layer = "environmental_grid",
         delete_layer = TRUE, quiet = TRUE)

cat("Saved prediction grid to modelling geopackage:", OUTPUT_PATH, "\n")

rm(scottish_waters, bathy_layer, nearest_bathy, salinity_layer, nearest_sal,
   temp_layer, nearest_temp, habitat_layer, grid_proj, habitat_proj,
   vars_to_check, var, values, n_missing, var_range, hab_counts,
   missing_habitat, points_with_values, nearest_indices)
