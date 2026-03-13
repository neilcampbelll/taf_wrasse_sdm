
# ============================================================================
# Before: layer of the Scottish coastline
# After: grid of distance to nearest point on the coast, at specified resolution
# ============================================================================


cat("Processing Coastal Distances\n")

scottish_waters <-  st_read(dsn = GEOPACKAGE_PATH, layer = "Scottish_Inshore_Waters")

# Validate dependencies
if (!exists("scottish_waters")) {
  stop("Scottish_Inshore_Waters layer not found. Run data_scottish_inshore_waters.R first.")
}

if (!exists("csq_scale")) {
  csq_scale <- 0.05  # Default grid resolution
  cat("Using default grid resolution:", csq_scale, "degrees\n")
}

# Load coastline data with error handling
cat("Loading coastline data...\n")
coast <- st_read(GEOPACKAGE_PATH, layer = "Coastline")
scotcoast <- st_read(GEOPACKAGE_PATH, "Scottish_Coastline")

# Ensure CRS consistency
cat("Checking coordinate reference systems...\n")
if (st_crs(scottish_waters) != st_crs(scotcoast)) {
  cat("! CRS mismatch detected, transforming Scottish coastline to match scottish_waters\n")
  coast <- st_transform(scotcoast, st_crs(scottish_waters))
}
if (st_crs(scottish_waters) != st_crs(coast)) {
  cat("! CRS mismatch detected, transforming coastline to match scottish_waters\n")
  coast <- st_transform(coast, st_crs(scottish_waters))
}


# Create grid with boundaries at even multiples of resolution
cat("\nCreating spatial grid...\n")
cat("Grid resolution:", csq_scale, "degrees\n")

tryCatch({
  # Use the bounding box of scottish_waters
  combined_bbox <- st_bbox(scottish_waters)

  # Validate bounding box
  if (any(is.na(combined_bbox))) {
    stop("Invalid bounding box from scottish_waters")
  }

  # Round to even multiples of csq_scale degrees
  grid_xmin <- floor(combined_bbox["xmin"] / csq_scale) * csq_scale
  grid_ymin <- floor(combined_bbox["ymin"] / csq_scale) * csq_scale
  grid_xmax <- ceiling(combined_bbox["xmax"] / csq_scale) * csq_scale
  grid_ymax <- ceiling(combined_bbox["ymax"] / csq_scale) * csq_scale

  # Validate grid dimensions
  grid_width <- grid_xmax - grid_xmin
  grid_height <- grid_ymax - grid_ymin

  if (grid_width <= 0 || grid_height <= 0) {
    stop("Invalid grid dimensions: width=", grid_width, ", height=", grid_height)
  }

  # Check if grid will be reasonable size
  n_x_points <- length(seq(from = grid_xmin, to = grid_xmax, by = csq_scale))
  n_y_points <- length(seq(from = grid_ymin, to = grid_ymax, by = csq_scale))
  total_points <- n_x_points * n_y_points

  cat("Grid dimensions:", n_x_points, "x", n_y_points, "=", total_points, "points\n")

  # Create the grid sequences
  x_seq <- seq(from = grid_xmin, to = grid_xmax, by = csq_scale)
  y_seq <- seq(from = grid_ymin, to = grid_ymax, by = csq_scale)

  # Create the grid points
  grid_points <- expand.grid(x = x_seq, y = y_seq)

  # Convert to sf object
  grid_sf <- st_as_sf(grid_points, coords = c("x", "y"), crs = st_crs(scottish_waters))

  cat("Created grid with", nrow(grid_sf), "points\n")

}, error = function(e) {
  stop("Grid creation failed: ", e$message)
})

# Clip grid points to study area
cat("Clipping grid to Scottish waters...\n")
tryCatch({
  grid_in_waters <- st_filter(grid_sf, scottish_waters)

  cat("Grid clipped to", nrow(grid_in_waters), "points within study area\n")

}, error = function(e) {
  stop("Grid clipping failed: ", e$message)
})

# Transform to projected CRS for distance calculations
cat("Transforming to projected coordinate system...\n")
tryCatch({
  coast_projected <- st_transform(coast, 27700)
  scotcoast_projected <- st_transform(scotcoast, 27700)
  grid_in_waters_projected <- st_transform(grid_in_waters, 27700)

  cat("Transformed layers to British National Grid (EPSG:27700)\n")

}, error = function(e) {
  stop("Coordinate transformation failed: ", e$message)
})


# Set the batch size
batch_size <- 1000 # This can be adjusted based on memory constraints

# Calculate distances using the batched approach
grid_with_distances <- calculate_distances_in_batches(
  grid_in_waters_projected,
  coast_projected,
  batch_size = batch_size
)

scot_grid_with_distances <- calculate_distances_in_batches(
  grid_in_waters_projected,
  scotcoast_projected,
  batch_size = batch_size
)

# Combine two sf objects, keeping the minimum distance for each point
grid_with_distances$dist_to_coast <- pmin(grid_with_distances$dist_to_coast, 
                                    scot_grid_with_distances$dist_to_coast)

# Convert distances from meters to kilometers
grid_with_distances$dist_to_coast_km <- grid_with_distances$dist_to_coast/1000


# Transform back to original CRS (matching scottish_waters)
grid_with_distances <- st_transform(grid_with_distances, st_crs(scottish_waters))

# Add the original grid coordinates for reference
coords_matrix <- st_coordinates(grid_with_distances)
grid_with_distances$grid_x <- coords_matrix[, "X"]
grid_with_distances$grid_y <- coords_matrix[, "Y"]

# Round coordinates to 5 decimal places (optional, for matching purposes)
grid_with_distances$grid_x_rounded <- round(grid_with_distances$grid_x, 5)
grid_with_distances$grid_y_rounded <- round(grid_with_distances$grid_y, 5)

grid_with_distances$dist_to_coast <- NULL
# Save the grid with distance calculations
st_write(grid_with_distances, GEOPACKAGE_PATH, "Scottish_Coastal_Distances") 

rm(scottish_waters, coast, scotcoast, combined_bbox, grid_xmin, grid_ymin, grid_xmax, grid_ymax, grid_width, grid_height, n_x_points, n_y_points, total_points, x_seq, y_seq, grid_points, grid_sf, grid_in_waters, coast_projected, scotcoast_projected, grid_in_waters_projected, batch_size, grid_with_distances, scot_grid_with_distances, coords_matrix)
