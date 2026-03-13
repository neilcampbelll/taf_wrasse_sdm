## Before: Climate data text files
## After: SST values as polygons in geopackage

cat("processessing sst data\n")

# Define the file path
filepath <- "clim_db_v01-2_unzipped/clim_db_v01-2/surface_temperature/ann_avg_tempS.txt"

# Verify data file exists
if (!file.exists(filepath)) {
  stop("Temperature data file not found: ", filepath,
       "\nEnsure climate data has been downloaded and extracted (i.e. run data_salinity.R first!.")
}

# Read the temperature data file
temp_data_df <- read.table(filepath, skip = 1, fill = TRUE, stringsAsFactors = FALSE)

# Extract latitude values from the first column and convert to numeric
lat_values <- as.numeric(temp_data_df[, 1])
if(any(is.na(lat_values))) {
  stop("NAs introduced in latitude values upon conversion. Check data.")
}

# Extract temperature values (all columns except the first)
temp_values_matrix <- as.matrix(temp_data_df[, -1])

# Replace "NaN" strings with R's NA
temp_values_matrix[temp_values_matrix == "NaN"] <- NA

# Convert temperature values to numeric
temp_values_numeric <- apply(temp_values_matrix, 2, as.numeric)

# Read the first line of the file to get longitudes
con <- file(filepath, "r")
lon_line <- readLines(con, n = 1)
close(con)

# Parse longitude values from the first line
# Split by whitespace, trim, and convert to numeric
lon_values <- as.numeric(strsplit(trimws(lon_line), "\\s+")[[1]])
if(any(is.na(lon_values))) {
  stop("NAs introduced in longitude values upon conversion. Check data.")
}

# Check for dimension consistency (optional but good practice)
if (length(lon_values) != ncol(temp_values_numeric)) {
  stop("Mismatch between number of longitude values and columns in temperature data.")
}
if (length(lat_values) != nrow(temp_values_numeric)) {
  stop("Mismatch between number of latitude values and rows in temperature data.")
}

# Calculate cell resolutions (assuming regular grid)
# Longitudes are typically increasing (West to East)
res_x <- abs(lon_values[2] - lon_values[1])
# Latitudes in the file are decreasing (North to South)
res_y <- abs(lat_values[2] - lat_values[1]) # abs because diff will be negative

# Create extent for the raster
# Coordinates are cell centers. Extent should be outer edges.
xmin <- min(lon_values) - res_x / 2
xmax <- max(lon_values) + res_x / 2
ymin <- min(lat_values) - res_y / 2
ymax <- max(lat_values) + res_y / 2

# Create raster using modern terra package
temp_raster <- terra::rast(nrows = length(lat_values),
                          ncols = length(lon_values),
                          xmin = xmin, xmax = xmax,
                          ymin = ymin, ymax = ymax,
                          crs = "EPSG:4326")

# Set values and layer name
terra::values(temp_raster) <- as.vector(t(temp_values_numeric))
names(temp_raster) <- "sst"



# TEMPERATURE PROCESSING - Keep full extent for consistent training/prediction
cat("Saving temperature data...\n")

# Note: terra::as.polygons can round values, so let's preserve precision
temp_sf <- terra::as.polygons(temp_raster, na.rm = TRUE, digits = 3) %>%
  st_as_sf()

if (nrow(temp_sf) > 0) {
  # Validate data before saving
  cat("  Temperature data summary:\n")
  cat("  Features:", nrow(temp_sf), "\n")
  cat("  Columns:", paste(names(temp_sf), collapse = ", "), "\n")

  # Use central geopackage saving function
  if (exists("save_to_geopackage")) {
    tryCatch({
      save_to_geopackage(temp_sf, "sst_temperature", GEOPACKAGE_PATH)
      cat("Temperature data saved to geopackage\n")

    }, error = function(e) {
      cat("Failed to save to geopackage:", e$message, "\n")
      # Fallback to standalone file
       })
  }
} else {
  cat("No temperature data to save - sf object is empty\n")
}

rm(filepath, temp_data_df, lat_values, temp_values_matrix, temp_values_numeric,
   con, lon_line, lon_values, res_x, res_y, xmin, xmax, ymin, ymax, temp_raster,
   temp_sf)
