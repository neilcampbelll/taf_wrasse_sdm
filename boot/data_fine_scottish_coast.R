###Coastline
# before: nothing
# after: a layer defining the Scottish coastline saved to geopackage
# the rnaturalearth data lacks Sula Sgierr and some of the outlying islands

# Define the URL for the GeoJSON file
url <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/ITL1_JAN_2025_UK_BFC/FeatureServer/replicafilescache/ITL1_JAN_2025_UK_BFC_-4663630678116259011.geojson"

# Define the local file name for temporary storage
local_file <- "data/UK_BFC_Jan2025.geojson"

# Download the file
message("Downloading the UK Boundary File (BFC) GeoJSON...")
download_result <- GET(url, 
                       write_disk(local_file, overwrite = TRUE),
                       progress())

# Check if download was successful
if (download_result$status_code == 200) {
  message("Download completed successfully!")
}  

# Document download in metadata
download_metadata <- list(
  dataset_name = "UK Boundary File Collection January 2025",
  source = "ArcGIS Online (ESMARspQHYMw9BZ9)",
  source_url = url,
  download_date = Sys.Date(),
  file_size_mb = file.size(local_file) / 1024^2,
  download_status = "Complete"
)

# Read the GeoJSON file
message("Reading the GeoJSON file into R...")

# We need to set GDAL configuration to allow larger GeoJSON files
# Setting to 0 removes any size limit
Sys.setenv(OGR_GEOJSON_MAX_OBJ_SIZE = 0)

# Now try reading the file again
uk_boundary <- st_read(local_file)

# Extract Scotland and save directly to geopackage
scotland <- uk_boundary[11,]

# Save to geopackage instead of shapefile
save_to_geopackage(scotland, "Scottish_Coastline", GEOPACKAGE_PATH)

# Clean up temporary files and memory
unlink(local_file)
rm(url, local_file, download_result, download_metadata, uk_boundary, scotland)