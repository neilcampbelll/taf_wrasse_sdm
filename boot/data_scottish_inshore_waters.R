###INSHORE WATERS
# before: nothing
# after: a layer defining the Scottish 12nm zone


url <- "https://msmap1.atkinsgeospatial.com/geoserver/nmpwfs/ows?token=d46ffd2a-e192-4e51-8a6a-b3292c20f1ee&request=GetFeature&service=WFS&version=1.1.0&outputFormat=json&typeName=administrative_units_scottish_marine_regions"

# Define the local file name
local_file <- "Scottish_Marine_Regions.geojson"

# Method 1: Direct download with httr ~15Mb
message("Downloading Scottish Marine Regions GeoJSON...")
download_result <- GET(url, 
                       write_disk(local_file, overwrite = TRUE),
                       progress())

# Check if download was successful
if (download_result$status_code == 200) {
  message("Download completed successfully!")
}    

# read the file in
scottish_marine_regions <- st_read(local_file)

# merge and transform the regional bits
scottish_waters <- st_transform(scottish_marine_regions, crs=4326)  %>%
  st_union() %>%
  st_as_sf()

save_to_geopackage(scottish_waters, "Scottish_Inshore_Waters", GEOPACKAGE_PATH)

rm(url, local_file, download_result, scottish_marine_regions, scottish_waters)

message("Coastal waters polygon downloaded and saved to geopackage")