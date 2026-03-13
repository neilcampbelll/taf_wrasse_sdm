# ============================================================================
# Create coastline polygon for the study area
# Before: nothing
# After: European/North Atlantic coastline for core of wrasse distribution range
# Range: 47-63N, 14W-9E 
# ============================================================================

bbox <- st_read(GEOPACKAGE_PATH, "Study_Area", quiet=T)

countries <- ne_countries(scale = 10, country = c("United Kingdom", "Ireland", "France", "Norway", "Denmark", "Germany", 
                                                  "Faroe Islands", "Sweden", "Belgium", "Netherlands", "Jersey", "Guernsey",
                                                  "Isle of Man", "Luxembourg"))

countries <- st_union(countries) %>%
  st_crop(bbox)

# Save to geopackage
cat("Saving coastline to geopackage...\n")

st_write(countries, GEOPACKAGE_PATH, "Coastline")

rm(bbox, countries)
