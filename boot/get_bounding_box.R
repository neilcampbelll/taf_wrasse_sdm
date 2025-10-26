# Bootstrap script for study area bounding box definition
# This script creates a bounding box for Scottish waters wrasse study area

# Load required libraries
library(sf)
library(jsonlite)

# CONFIGURATION ===============================================================
AREA_CONFIG <- list(
  # Study area bounding box
  study_bounds = c(-14, 47, 9, 63))

# ===============================================================
bbox <- AREA_CONFIG$study_bounds
names(bbox) <- c("xmin", "ymin", "xmax", "ymax")

# Set the CRS for the bounding box

# Create bbox object with proper CRS
bbox <- st_bbox(bbox, crs = st_crs(4326))


# Convert bbox to an sf polygon object before writing
bbox_polygon <- st_as_sfc(bbox)
bbox_sf <- st_sf(geometry = bbox_polygon)

# Write to geopackage with correct parameter order
st_write(bbox_sf, GEOPACKAGE_PATH, "Study_Area", append = F)

# Create metadata
metadata <- list(
  study_area = list(
    description = "Bounding box for Scottish waters wrasse distribution study",
    projection = "WGS84 (EPSG:4326)",
    created = as.character(Sys.Date()),
    creator = Sys.getenv("USERNAME"),
    coordinates = list(
      min_longitude = bbox$xmin,
      max_longitude = bbox$xmax,
      min_latitude = bbox$ymin,
      max_latitude = bbox$ymax
    )
  )
)

# Save metadata to JSON
write_json(metadata, "boot/initial/data/wrasse_metadata.json", pretty = TRUE)

rm(AREA_CONFIG, bbox, bbox_polygon, bbox_sf, metadata)

