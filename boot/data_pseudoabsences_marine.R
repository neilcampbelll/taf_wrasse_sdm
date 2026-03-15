# ============================================================================
# MARINE PSEUDO-ABSENCE GENERATION (OPTIMIZED)
# TAF Phase: Data preparation
# Before: Species records in GEOPACKAGE_PATH, environmental layers
# After: Marine pseudo-absences with habitat saved to OUTPUT_PATH
# ============================================================================

# NOTE: Assumes utilities.R has already been sourced

sf_use_s2(FALSE)  # Turn off spherical geometry

cat("=== GENERATING MARINE PSEUDO-ABSENCES (OPTIMIZED) ===\n\n")

# CONFIGURATION ===============================================================

PSEUDO_CONFIG <- list(
  n_ratio = 3,              # Ratio of pseudo-absences to presences
  min_distance_km = 1.0,    # Minimum distance from presence points (km)
  decay_rate = 0.3,         # Exponential decay rate for distance weighting
  seed = 123,               # Random seed for reproducibility
  n_sample_bath = 100000,   # Number of bathymetry points to sample

  # Distance strata proportions (coastal to offshore)
  strata = list(
    coastal = list(min = 0, max = 5, prop = 0.50),      # 0-5km: 50%
    nearshore = list(min = 5, max = 15, prop = 0.25),   # 5-15km: 25%
    midshore = list(min = 15, max = 50, prop = 0.15),   # 15-50km: 15%
    offshore = list(min = 50, max = 200, prop = 0.10)   # 50-200km: 10%
  ),

  # Species-specific strata for offshore species
  strata_offshore = list(
    coastal = list(min = 0, max = 5, prop = 0.30),
    nearshore = list(min = 5, max = 15, prop = 0.25),
    midshore = list(min = 15, max = 50, prop = 0.25),
    offshore = list(min = 50, max = 200, prop = 0.20)
  )
)

# Target species
WRASSE_SPECIES <- c("Labrus bergylta", "Labrus mixtus", "Centrolabrus exoletus",
                    "Ctenolabrus rupestris", "Symphodus melops")

# STEP 1: CREATE MASTER SAMPLING POOL (ONCE) ==================================

cat("STEP 1: Creating master marine sampling pool...\n")

# Load bathymetry
cat("  Loading bathymetry...\n")
bathymetry <- st_read(GEOPACKAGE_PATH, layer = "bathymetry", quiet = TRUE)
cat("  ✓ Loaded", nrow(bathymetry), "bathymetry points\n")

# Filter to marine-only (negative depths)
bathymetry_marine <- bathymetry %>%
  filter(depth_m < 0)

cat("  ✓ Filtered to", nrow(bathymetry_marine), "marine points (negative depth)\n")

# Clip to Scottish inshore waters — must match presence record extent
cat("  Clipping to Scottish inshore waters...\n")
scottish_waters   <- st_read(GEOPACKAGE_PATH, "Scottish_Inshore_Waters", quiet = TRUE)
bathymetry_marine <- st_filter(bathymetry_marine, scottish_waters)
cat("  ✓ Clipped to", nrow(bathymetry_marine), "points within Scottish inshore waters\n")

# Sample subset for efficiency
set.seed(PSEUDO_CONFIG$seed)
if (nrow(bathymetry_marine) > PSEUDO_CONFIG$n_sample_bath) {
  bath_sample <- bathymetry_marine[sample(nrow(bathymetry_marine), PSEUDO_CONFIG$n_sample_bath), ]
  cat("  ✓ Sampled", nrow(bath_sample), "points for efficiency\n")
} else {
  bath_sample <- bathymetry_marine
}

# Calculate distance to coast (ONCE)
cat("  Calculating distances to coast...\n")
coastline <- st_read(GEOPACKAGE_PATH, layer = "Coastline", quiet = TRUE)

bath_proj <- st_transform(bath_sample, 27700)
coastline_proj <- st_transform(coastline, 27700)

distances_m <- st_distance(bath_proj, coastline_proj)
min_distances_km <- as.numeric(apply(distances_m, 1, min)) / 1000

bath_sample$dist_to_coast_km <- min_distances_km

cat("  ✓ Distance range:", round(min(min_distances_km), 1), "-",
    round(max(min_distances_km), 1), "km\n")

# Extract environmental data (ONCE)
cat("  Extracting environmental data...\n")

bath_wgs84 <- st_transform(bath_sample, 4326)

# Extract SST
sst_layer <- st_read(GEOPACKAGE_PATH, layer = "sst_temperature", quiet = TRUE)
sst_join <- st_join(bath_wgs84, sst_layer["sst"], join = st_nearest_feature)
bath_wgs84$sst <- sst_join$sst
cat("  ✓ Extracted SST:", sum(!is.na(bath_wgs84$sst)), "values\n")

# Extract SSS
sss_layer <- st_read(GEOPACKAGE_PATH, layer = "sss_salinity", quiet = TRUE)
sss_join <- st_join(bath_wgs84, sss_layer["sss"], join = st_nearest_feature)
bath_wgs84$sss <- sss_join$sss
cat("  ✓ Extracted SSS:", sum(!is.na(bath_wgs84$sss)), "values\n")

# Extract MSFD_BBHT habitat (this is the slow step - only do once!)
cat("  Extracting habitat (this may take a few minutes)...\n")
habitat_layer <- st_read(GEOPACKAGE_PATH, layer = "MSFD_Habitats", quiet = TRUE)
habitat_join <- st_join(bath_wgs84, habitat_layer["MSFD_BBHT"], join = st_nearest_feature)
bath_wgs84$MSFD_BBHT <- habitat_join$MSFD_BBHT
cat("  ✓ Extracted habitat:", sum(!is.na(bath_wgs84$MSFD_BBHT)), "values\n")

# Filter for complete cases
master_pool <- bath_wgs84 %>%
  st_drop_geometry() %>%
  mutate(
    coords = st_coordinates(bath_wgs84),
    lon = coords[,1],
    lat = coords[,2]
  ) %>%
  filter(
    !is.na(depth_m) & !is.na(sst) & !is.na(sss) & !is.na(dist_to_coast_km),
    is.finite(depth_m) & is.finite(sst) & is.finite(sss) & is.finite(dist_to_coast_km)
  )

cat("  ✓ Master pool ready:", nrow(master_pool), "points with complete data\n")
cat("  ✓ Depth range:", round(min(master_pool$depth_m), 1), "to",
    round(max(master_pool$depth_m), 1), "m\n\n")

# STEP 2: GENERATE SPECIES-SPECIFIC PSEUDO-ABSENCES ===========================

generate_species_pseudo_absences <- function(species_name, master_pool, config = PSEUDO_CONFIG) {

  cat("--- Processing:", species_name, "---\n")

  # Adjust configuration for offshore species
  if (species_name == "Labrus mixtus") {
    cat("  Using offshore strata configuration\n")
    config$strata <- config$strata_offshore
  }

  # Load presence data
  layer_name <- paste0(gsub("\\s", "_", species_name), "_records")

  if (!layer_name %in% st_layers(GEOPACKAGE_PATH)$name) {
    cat("  ✗ Species layer not found:", layer_name, "\n")
    return(NULL)
  }

  presence_data <- st_read(GEOPACKAGE_PATH, layer = layer_name, quiet = TRUE)
  cat("  ✓ Loaded", nrow(presence_data), "presence records (raw OBIS)\n")

  # Use processed (Scottish-waters-clipped) count if available, so the 3:1 ratio
  # is calculated against the same records that will be used for modelling
  output_layers <- st_layers(OUTPUT_PATH)$name
  if (layer_name %in% output_layers) {
    n_presences <- nrow(st_read(OUTPUT_PATH, layer = layer_name, quiet = TRUE))
    cat("  ✓ Using processed presence count for ratio:", n_presences, "records\n")
  } else {
    n_presences <- nrow(presence_data)
    cat("  ⚠ Processed layer not found in OUTPUT_PATH — using raw count for ratio\n")
  }

  # Apply species-specific distance buffer exclusion
  cat("  Applying", config$min_distance_km, "km exclusion buffer...\n")

  # Create spatial version of master pool for buffering
  master_pool_sf <- st_as_sf(master_pool, coords = c("lon", "lat"), crs = 4326)

  presence_proj <- st_transform(presence_data, 27700)
  master_proj <- st_transform(master_pool_sf, 27700)

  # Create exclusion buffer
  presence_buffer <- st_buffer(presence_proj, dist = config$min_distance_km * 1000) %>%
    st_union()

  # Find points outside buffer
  outside_buffer <- st_disjoint(master_proj, presence_buffer, sparse = FALSE)[,1]
  eligible_pool <- master_pool[outside_buffer, ]

  cat("  ✓ Eligible points after buffer:", nrow(eligible_pool), "\n")

  if (nrow(eligible_pool) < 50) {
    cat("  ✗ Too few eligible points\n")
    return(NULL)
  }

  # Distance-weighted stratified sampling
  cat("  Applying distance-weighted stratified sampling...\n")

  set.seed(config$seed)

  distances <- eligible_pool$dist_to_coast_km
  weights <- exp(-config$decay_rate * distances)
  weights <- pmax(weights, 0.01)
  weights <- weights / sum(weights)

  # Sample from each stratum
  n_total_target <- n_presences * config$n_ratio
  sampled_indices <- c()

  for (stratum_name in names(config$strata)) {
    stratum <- config$strata[[stratum_name]]

    in_stratum <- which(distances >= stratum$min & distances < stratum$max)
    n_available <- length(in_stratum)

    if (n_available == 0) {
      cat("  ⚠ No points in", stratum_name, "stratum\n")
      next
    }

    n_target <- max(1, round(n_total_target * stratum$prop))
    n_sample <- min(n_target, n_available)

    stratum_weights <- weights[in_stratum]
    stratum_weights <- stratum_weights / sum(stratum_weights)

    sampled <- sample(in_stratum, n_sample, prob = stratum_weights, replace = FALSE)
    sampled_indices <- c(sampled_indices, sampled)

    cat("  ✓", stratum_name, ":", n_sample, "points\n")
  }

  # Create pseudo-absence dataset
  sampled_data <- eligible_pool[sampled_indices, ]

  pseudo_absences <- st_as_sf(
    sampled_data,
    coords = c("lon", "lat"),
    crs = 4326
  ) %>%
    mutate(
      basisOfRecord = "PseudoAbsence",
      originalScientificName = species_name,
      aphiaID = presence_data$aphiaID[1],
      flags = "Marine"
    ) %>%
    dplyr::select(basisOfRecord, depth_m, originalScientificName, aphiaID,
                  flags, sst, sss, dist_to_coast_km, MSFD_BBHT)

  cat("  ✓ Generated", nrow(pseudo_absences), "marine pseudo-absences\n")
  cat("  ✓ Ratio:", round(nrow(pseudo_absences) / nrow(presence_data), 2), ":1\n")

  # Summary statistics
  # Presence records may use shoredistance (metres) if not yet standardised
  pres_dist_km <- if (is.numeric(presence_data$dist_to_coast_km)) {
    presence_data$dist_to_coast_km
  } else if (is.numeric(presence_data$shoredistance)) {
    presence_data$shoredistance / 1000
  } else {
    NULL
  }

  if (!is.null(pres_dist_km)) {
    pres_dist_mean  <- mean(pres_dist_km, na.rm = TRUE)
    pseudo_dist_mean <- mean(pseudo_absences$dist_to_coast_km, na.rm = TRUE)
    cat("  ✓ Mean distance - Presences:", round(pres_dist_mean, 2),
        "km | Pseudo-absences:", round(pseudo_dist_mean, 2), "km\n")
    cat("  ✓ Distance ratio:", round(pseudo_dist_mean / pres_dist_mean, 2), "\n")
  }

  return(pseudo_absences)
}

# Process all species
cat("STEP 2: Generating species-specific pseudo-absences...\n\n")

for (species_name in WRASSE_SPECIES) {

  tryCatch({
    pseudo_absences <- generate_species_pseudo_absences(species_name, master_pool, PSEUDO_CONFIG)

    if (!is.null(pseudo_absences) && nrow(pseudo_absences) > 0) {
      layer_name <- paste0(gsub("\\s", "_", species_name), "_pseudo_absences_extended")

      success <- save_to_geopackage(pseudo_absences, layer_name, OUTPUT_PATH)

      if (success) {
        cat("  ✓ Saved to layer:", layer_name, "\n\n")
      } else {
        cat("  ✗ Failed to save pseudo-absences for", species_name, "\n\n")
      }
    }

  }, error = function(e) {
    cat("  ✗ Error processing", species_name, ":", e$message, "\n\n")
  })
}

# SUMMARY =====================================================================

cat("=== MARINE PSEUDO-ABSENCE GENERATION COMPLETE ===\n")

layers <- st_layers(OUTPUT_PATH)$name
pseudo_layers <- layers[grepl("_pseudo_absences_extended$", layers)]

cat("Generated pseudo-absence layers:\n")
for (layer in pseudo_layers) {
  layer_info <- st_layers(OUTPUT_PATH)
  idx <- which(layer_info$name == layer)
  n_features <- layer_info$features[idx]
  cat("  -", layer, ":", n_features, "points\n")
}

cat("\n✓ All pseudo-absences are marine-only (depth < 0)\n")
cat("✓ All include habitat classification (MSFD_BBHT)\n")
cat("✓ Optimized: Environmental data extracted once, reused for all species\n")
cat("✓ Ready for SDM modeling\n")
