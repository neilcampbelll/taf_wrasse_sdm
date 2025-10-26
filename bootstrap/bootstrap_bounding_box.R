# Bootstrap script for study area bounding box definition
# This script creates a bounding box for Scottish waters wrasse study area

# Define bounding box coordinates for Scottish waters
# Approximate coordinates covering Scottish coastal waters
scottish_bbox <- data.frame(
  min_longitude = -8.5,   # Western boundary
  max_longitude = -0.5,   # Eastern boundary
  min_latitude = 54.5,    # Southern boundary
  max_latitude = 61.0     # Northern boundary (including Shetland)
)

# Add metadata
attr(scottish_bbox, "description") <- "Bounding box for Scottish waters wrasse distribution study"
attr(scottish_bbox, "projection") <- "WGS84 (EPSG:4326)"
attr(scottish_bbox, "created") <- Sys.Date()

# Save the bounding box data
save(scottish_bbox, file = "bounding_box.RData")

# Also save as CSV for external use
write.csv(scottish_bbox, "bounding_box.csv", row.names = FALSE)

cat("Bounding box data created successfully\n")
cat("Longitude range:", scottish_bbox$min_longitude, "to", scottish_bbox$max_longitude, "\n")
cat("Latitude range:", scottish_bbox$min_latitude, "to", scottish_bbox$max_latitude, "\n")