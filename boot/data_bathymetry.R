# ============================================================================
# Before: Study_Area layer in geopackage
# After: Bathymetry points layer in geopackage
# ============================================================================

cat("Processing bathymetry data\n")

bbox <- st_read(GEOPACKAGE_PATH, "Study_Area", quiet = TRUE)
bb   <- st_bbox(bbox)

# Download in 10x8 degree chunks (each ~80 sq degrees, within NOAA limit)
lon_breaks <- seq(bb["xmin"], bb["xmax"], by = 10)
if (tail(lon_breaks, 1) < bb["xmax"]) lon_breaks <- c(lon_breaks, bb["xmax"])

lat_breaks <- seq(bb["ymin"], bb["ymax"], by = 8)
if (tail(lat_breaks, 1) < bb["ymax"]) lat_breaks <- c(lat_breaks, bb["ymax"])

n_chunks <- (length(lon_breaks) - 1) * (length(lat_breaks) - 1)
cat("Downloading", n_chunks, "chunks from NOAA...\n")

chunks      <- list()
chunk_count <- 0

for (i in seq_along(lon_breaks[-1])) {
  for (j in seq_along(lat_breaks[-1])) {
    chunk_count <- chunk_count + 1
    cat("  Chunk", chunk_count, "/", n_chunks, "\n")

    bathy_chunk <- marmap::getNOAA.bathy(
      lon1       = lon_breaks[i],
      lon2       = lon_breaks[i + 1],
      lat1       = lat_breaks[j],
      lat2       = lat_breaks[j + 1],
      resolution = 1,
      keep       = FALSE
    )

    chunks[[chunk_count]] <- marmap::as.xyz(bathy_chunk)
  }
}

# Combine chunks, drop NA depths, rename columns
bathy_df        <- do.call(rbind, chunks)
names(bathy_df) <- c("lon", "lat", "depth_m")
bathy_df        <- bathy_df[!is.na(bathy_df$depth_m), ]

cat("Total points:", nrow(bathy_df), "\n")
cat("Depth range:", round(min(bathy_df$depth_m), 1), "to",
    round(max(bathy_df$depth_m), 1), "m\n")

# Convert to sf and save
bathy_sf <- st_as_sf(bathy_df, coords = c("lon", "lat"), crs = 4326)

st_write(bathy_sf, GEOPACKAGE_PATH, layer = "bathymetry",
         delete_layer = TRUE, quiet = TRUE)

cat("Bathymetry saved to geopackage\n")

rm(bbox, bb, lon_breaks, lat_breaks, n_chunks, chunks, chunk_count,
   i, j, bathy_chunk, bathy_df, bathy_sf)
