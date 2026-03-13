# before: nothing
# after: salinity data saved to the geopackage, plus a file to take the SST from

# Define the URL for the zip file
zip_url <- "https://data.marine.gov.scot/sites/default/files//clim_db_v01-2.zip"

# Define the destination for the downloaded zip file
temp_zip_file <- "clim_db_v01-2.zip"

# Download the zip file
download.file(zip_url, temp_zip_file, mode = "wb")

# Define the directory to unzip into
unzip_dir <- "clim_db_v01-2_unzipped" # You can choose a different name if you prefer

# Unzip the file
unzip(temp_zip_file, exdir = unzip_dir)

# update the filepath to reflect the unzipped directory
filepath <- file.path(unzip_dir, "clim_db_v01-2", "surface_salinity", "ann_avg_saltS.txt")

# Read the salinity data file
sal_data_df <- read.table(filepath, skip = 1, fill = TRUE, stringsAsFactors = FALSE)

# unlink the file 
 unlink(temp_zip_file)
# (you might want to remove the downloaded zip file after unzipping to keep your workspace clean)

 # Extract latitude values from the first column and convert to numeric
lat_values <- as.numeric(sal_data_df[, 1])
if(any(is.na(lat_values))) {
  stop("NAs introduced in latitude values upon conversion. Check data.")
}

# Extract salinity values (all columns except the first)
sal_values_matrix <- as.matrix(sal_data_df[, -1])

# Replace "NaN" strings with R's NA
sal_values_matrix[sal_values_matrix == "NaN"] <- NA

# Convert salinity values to numeric
sal_values_numeric <- apply(sal_values_matrix, 2, as.numeric)

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

# A quick check that dimensions are consistent 
if (length(lon_values) != ncol(sal_values_numeric)) {
  stop("Mismatch between number of longitude values and columns in salinity data.")
}
if (length(lat_values) != nrow(sal_values_numeric)) {
  stop("Mismatch between number of latitude values and rows in salinity data.")
}

# Calculate cell resolutions 
res_x <- abs(lon_values[2] - lon_values[1])
res_y <- abs(lat_values[2] - lat_values[1]) # abs because diff will be negative

# Create extent for the raster
# Coordinates are cell centers. Extent should be outer edges.
xmin <- min(lon_values) - res_x / 2
xmax <- max(lon_values) + res_x / 2
ymin <- min(lat_values) - res_y / 2
ymax <- max(lat_values) + res_y / 2

# Create raster using terra
sal_raster <- terra::rast(nrows = length(lat_values),
                         ncols = length(lon_values),
                         xmin = xmin, xmax = xmax,
                         ymin = ymin, ymax = ymax,
                         crs = "EPSG:4326")

terra::values(sal_raster) <- as.vector(t(sal_values_numeric))

# Set name for the raster layer
names(sal_raster) <- "sss"

# Plot to visualise
plot(sal_raster, main = "Annual Average Salinity")

# Summary of the data
sal_ext <- terra::ext(sal_raster)
print(paste("Extent:",
            "Longitude:", round(sal_ext[1], 2), "to", round(sal_ext[2], 2),
            "Latitude:", round(sal_ext[3], 2), "to", round(sal_ext[4], 2)))
print(paste("Resolution (calculated by raster):", round(terra::res(sal_raster)[1], 4), "x", round(terra::res(sal_raster)[2], 4), "degrees"))
print(paste("Resolution (derived from coords):", round(res_x, 4), "x", round(res_y, 4), "degrees"))
print(paste("Number of cells:", terra::ncell(sal_raster)))
print(paste("Non-NA cells:", sum(!is.na(terra::values(sal_raster)))))


cat("\nExtracting salinity values...\n")

# Convert to polygons
sal_sf <- terra::as.polygons(sal_raster, na.rm = TRUE, digits = 3) %>%
  st_as_sf()

cat("  Created", nrow(sal_sf), "salinity polygons\n")
sal_range <- range(sal_sf$sss, na.rm = TRUE)
cat("  Salinity range:", round(sal_range[1], 2), "to", round(sal_range[2], 2), "PSU\n")
cat("  Spatial extent: Lon", round(st_bbox(sal_sf)[1], 1), "to", round(st_bbox(sal_sf)[3], 1),
    "| Lat", round(st_bbox(sal_sf)[2], 1), "to", round(st_bbox(sal_sf)[4], 1), "\n")

  # print(str(sal_sf)) # For more detailed structure

# Save to geopackage
      st_write(sal_sf, GEOPACKAGE_PATH, layer = "sss_salinity", delete_layer = TRUE, quiet = TRUE)
      cat("saved salinity to geopackage.\n")

rm(zip_url, temp_zip_file, unzip_dir, filepath, sal_data_df, lat_values, sal_values_matrix, sal_values_numeric, con, lon_line, lon_values, res_x, res_y, xmin, xmax, ymin, ymax, sal_raster, sal_ext, sal_sf, sal_range)

