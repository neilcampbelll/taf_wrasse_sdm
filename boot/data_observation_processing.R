# ============================================================================
# Before: Species occurrence records (coordinates only) in GEOPACKAGE_PATH
# After:  Presence records clipped to Scottish waters with environmental values
#         assigned from the same gridded layers used for pseudo-absences,
#         saved to OUTPUT_PATH
# ============================================================================

sf_use_s2(FALSE)

cat("Processing species records\n")

WRASSE_SPECIES <- c("Labrus_bergylta", "Labrus_mixtus", "Centrolabrus_exoletus",
                    "Ctenolabrus_rupestris", "Symphodus_melops")

# LOAD ENVIRONMENTAL LAYERS ==================================================

cat("Loading environmental layers...\n")

scottish_waters <- st_read(GEOPACKAGE_PATH, "Scottish_Inshore_Waters",   quiet = TRUE)
dist_grid       <- st_read(GEOPACKAGE_PATH, "Scottish_Coastal_Distances", quiet = TRUE)
bathymetry      <- st_read(GEOPACKAGE_PATH, "bathymetry",                 quiet = TRUE)
sst_layer       <- st_read(GEOPACKAGE_PATH, "sst_temperature",            quiet = TRUE)
sss_layer       <- st_read(GEOPACKAGE_PATH, "sss_salinity",               quiet = TRUE)

sst_col <- intersect(c("sst", "temperature"), names(sst_layer))[1]
sss_col <- intersect(c("sss", "salinity"),    names(sss_layer))[1]
cat("  SST column:", sst_col, "| Salinity column:", sss_col, "\n")

  cat("  Loading and cropping habitat layer to Scottish waters...\n")
  habitat_layer <- st_read(GEOPACKAGE_PATH, "MSFD_Habitats", quiet = TRUE)
  habitat_layer <- st_crop(habitat_layer, st_bbox(scottish_waters))
  habitat_proj  <- st_transform(habitat_layer["MSFD_BBHT"], 27700)
  cat("  Habitat layer ready\n")

cat("Environmental layers ready\n\n")

# Combine into one object so all spatial joins run once across all species

cat("Clipping species records...\n")

gp_layers    <- st_layers(GEOPACKAGE_PATH)$name
records_list <- list()

for (sp in WRASSE_SPECIES) {
  layer_name <- paste0(sp, "_records")
  if (!layer_name %in% gp_layers) {
    cat("  Layer not found, skipping:", layer_name, "\n")
    next
  }
  records   <- st_read(GEOPACKAGE_PATH, layer_name, quiet = TRUE)
  keep_cols <- intersect(c("basisOfRecord", "originalScientificName", "aphiaID"),
                         names(records))
  records   <- records[, keep_cols]
  records   <- st_filter(records, scottish_waters)
  if (nrow(records) > 0) {
    records$species_layer <- sp
    records_list[[sp]]    <- records
    cat(" ", gsub("_", " ", sp), ":", nrow(records), "records within Scottish waters\n")
  } else {
    cat(" ", gsub("_", " ", sp), ": no records within Scottish waters\n")
  }
}

if (length(records_list) == 0) stop("No species records found in Scottish waters.")

combined <- do.call(rbind, records_list)
cat("\nTotal records to process:", nrow(combined), "\n\n")

# ASSIGN ENVIRONMENTAL VALUES ONCE ACROSS ALL SPECIES ========================

cat("Assigning environmental values...\n")

cat("  Depth...\n")
nn <- st_nearest_feature(combined, bathymetry)
combined$depth_m <- bathymetry$depth_m[nn]
combined$depth_m[combined$depth_m > 0] <- 0

cat("  Distance to coast...\n")
nn <- st_nearest_feature(combined, dist_grid)
combined$dist_to_coast_km <- dist_grid$dist_to_coast_km[nn]

cat("  SST...\n")
nn <- st_nearest_feature(combined, sst_layer)
combined$sst <- sst_layer[[sst_col]][nn]

cat("  Salinity...\n")
nn <- st_nearest_feature(combined, sss_layer)
combined$sss <- sss_layer[[sss_col]][nn]

if (has_habitat) {
  cat("  Habitat (point-in-polygon + nearest-neighbour fallback)...\n")
  combined_proj <- st_transform(combined, 27700)

  joined <- st_join(combined_proj, habitat_proj, join = st_intersects, left = TRUE)
  joined <- joined[!duplicated(row.names(joined)), ]
  combined$MSFD_BBHT <- joined$MSFD_BBHT

  missing <- is.na(combined$MSFD_BBHT)
  if (any(missing)) {
    nn_hab <- st_nearest_feature(combined_proj[missing, ], habitat_proj)
    combined$MSFD_BBHT[missing] <- habitat_proj$MSFD_BBHT[nn_hab]
  }
  cat("  Habitat assigned:", sum(!is.na(combined$MSFD_BBHT)), "/", nrow(combined), "\n")
  rm(combined_proj, joined, missing, nn_hab)
}

combined$presence <- 1L
cat("Environmental assignment complete\n\n")

# SPLIT BY SPECIES AND SAVE ==================================================

cat("Saving species records to modelling geopackage...\n")

for (sp in WRASSE_SPECIES) {
  sp_records <- combined[combined$species_layer == sp, ]
  sp_records$species_layer <- NULL

  if (nrow(sp_records) == 0) next

  layer_name <- paste0(sp, "_records")
  fmt <- function(x) paste(round(min(x, na.rm=TRUE), 1), "to", round(max(x, na.rm=TRUE), 1))
  cat(" ", gsub("_", " ", sp), "(n =", nrow(sp_records), ")\n")
  cat("    depth_m:", fmt(sp_records$depth_m), "m |",
      "sst:", fmt(sp_records$sst), "°C |",
      "sss:", fmt(sp_records$sss), "PSU |",
      "dist:", fmt(sp_records$dist_to_coast_km), "km\n")

  st_write(sp_records, OUTPUT_PATH, layer = layer_name,
           delete_layer = TRUE, quiet = TRUE)
}

cat("\n=== OBSERVATION PROCESSING COMPLETE ===\n")

rm(WRASSE_SPECIES, scottish_waters, dist_grid, bathymetry, sst_layer, sss_layer,
   sst_col, sss_col, gp_layers, records_list, sp, layer_name, records, keep_cols,
   combined, nn, fmt, sp_records)
if (has_habitat) rm(has_habitat, habitat_layer, habitat_proj)
if (exists("has_habitat")) rm(has_habitat)
