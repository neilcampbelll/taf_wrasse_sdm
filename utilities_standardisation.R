# ============================================================================
# VARIABLE NAMING STANDARDIZATION SYSTEM
# Ensures consistent variable names and units across all scripts
# ============================================================================

# STANDARD VARIABLE DEFINITIONS ==============================================

#' Standard variable names and conventions used throughout the workflow
STANDARD_VARIABLES <- list(
  # Spatial coordinates
  longitude = "lon",           # Decimal degrees, WGS84
  latitude = "lat",            # Decimal degrees, WGS84

  # Environmental variables
  depth = "depth_m",           # Water depth in meters (NEGATIVE values, e.g., -50m)
  temperature = "sst",         # Sea surface temperature in Celsius
  salinity = "sss",            # Sea surface salinity in PSU
  distance_to_coast = "dist_to_coast_km",  # Distance to coast in kilometers

  # Species data
  presence = "presence",       # Binary: 1 = presence, 0 = absence/pseudo-absence
  species_name = "species",    # Scientific name

  # Habitat classification
  habitat = "habitat_simplified",  # Simplified habitat categories

  # Model predictions
  rf_prediction = "rf",
  gbm_prediction = "gbm",
  gam_prediction = "gam",
  inla_prediction = "inla",
  ensemble_prediction = "ensemble",
  uncertainty = "uncertainty"
)

# UNIT CONVERSIONS ===========================================================

#' Convert depth values to standard negative meters
#' @param depth_values Numeric vector of depth values
#' @param current_unit Current unit: "m", "positive_m", "negative_m", "fathoms"
#' @return Numeric vector with depths as negative meters
standardize_depth <- function(depth_values, current_unit = "detect") {

  if (current_unit == "detect") {
    # Auto-detect based on data characteristics
    if (all(depth_values <= 0, na.rm = TRUE)) {
      current_unit <- "negative_m"
    } else if (all(depth_values >= 0, na.rm = TRUE)) {
      current_unit <- "positive_m"
    } else {
      warning("Mixed positive/negative depth values detected. Assuming positive = depth below surface.")
      current_unit <- "positive_m"
    }
  }

  standardized <- switch(current_unit,
    "negative_m" = depth_values,
    "positive_m" = -abs(depth_values),
    "m" = -abs(depth_values),
    "fathoms" = -abs(depth_values * 1.8288),  # Convert fathoms to meters
    stop("Unknown depth unit: ", current_unit)
  )

  # Validation: depths should be negative (below sea level)
  if (any(standardized > 0, na.rm = TRUE)) {
    warning("Some depth values are positive (above sea level). Check data quality.")
  }

  return(standardized)
}

#' Convert distance values to standard kilometers
#' @param distance_values Numeric vector of distance values
#' @param current_unit Current unit: "km", "m", "nautical_miles", "degrees"
#' @return Numeric vector with distances in kilometers
standardize_distance <- function(distance_values, current_unit = "detect") {

  if (current_unit == "detect") {
    # Auto-detect based on typical ranges
    max_val <- max(distance_values, na.rm = TRUE)
    if (max_val > 1000) {
      current_unit <- "m"
    } else if (max_val > 10) {
      current_unit <- "km"
    } else {
      current_unit <- "degrees"
    }
    message("Auto-detected distance unit: ", current_unit)
  }

  standardized <- switch(current_unit,
    "km" = distance_values,
    "m" = distance_values / 1000,
    "nautical_miles" = distance_values * 1.852,
    "degrees" = distance_values * 111.32,  # Approximate km per degree at 60°N
    stop("Unknown distance unit: ", current_unit)
  )

  return(standardized)
}

# COLUMN MAPPING FUNCTIONS ===================================================

#' Map various column name variants to standard names
#' @param data Data frame or sf object
#' @return Data frame with standardized column names
standardize_column_names <- function(data) {

  # Define column name mappings (alternative names -> standard name)
  column_mappings <- list(
    # Coordinates
    c("longitude", "decimalLongitude", "lon", "long", "x") -> "lon",
    c("latitude", "decimalLatitude", "lat", "y") -> "lat",

    # Depth (handle various depth column names)
    c("depth", "depth_m", "bathymetry", "water_depth", "DEPTH") -> "depth_m",

    # Temperature
    c("temperature", "sst", "sea_surface_temperature", "surface_temperature", "SST", "temp") -> "sst",

    # Salinity
    c("salinity", "sss", "sea_surface_salinity", "surface_salinity", "SSS", "sal") -> "sss",

    # Distance to coast
    c("distance_to_coast", "dist_to_coast", "dist_to_coast_km", "shoredistance", "shore_distance", "coastal_distance") -> "dist_to_coast_km",

    # Habitat
    c("habitat", "habitat_type", "MSFD_BBHT", "benthic_habitat", "substrate") -> "habitat_simplified",

    # Species information
    c("scientific_name", "scientificName", "originalScientificName", "taxon") -> "species"
  )

  # Apply mappings
  original_names <- names(data)
  new_names <- original_names

  for (mapping in column_mappings) {
    # Extract source and target
    source_names <- mapping[[1]]
    target_name <- mapping[[2]]

    # Find which source names exist in data
    existing_sources <- intersect(source_names, original_names)

    if (length(existing_sources) > 0) {
      # Use the first existing source name
      source_to_use <- existing_sources[1]
      new_names[new_names == source_to_use] <- target_name

      # Warn if multiple variants exist
      if (length(existing_sources) > 1) {
        warning("Multiple variants found for ", target_name, ": ",
                paste(existing_sources, collapse = ", "),
                ". Using: ", source_to_use)
      }
    }
  }

  # Apply new names
  names(data) <- new_names

  # Report changes
  changed <- original_names != new_names
  if (any(changed)) {
    changes <- data.frame(
      original = original_names[changed],
      standardized = new_names[changed]
    )
    cat("Standardized column names:\n")
    print(changes)
  }

  return(data)
}

#' Standardize environmental variable values and units
#' @param data Data frame with environmental variables
#' @return Data frame with standardized values and units
standardize_environmental_values <- function(data) {

  cat("Standardizing environmental variable values...\n")

  # Standardize depth values
  if ("depth_m" %in% names(data)) {
    data$depth_m <- standardize_depth(data$depth_m)
    cat("✓ Standardized depth values (negative meters)\n")
  }

  # Standardize distance to coast
  distance_cols <- intersect(names(data), c("dist_to_coast_km", "shoredistance"))
  for (col in distance_cols) {
    if (col == "shoredistance") {
      # shoredistance is typically in meters
      data$dist_to_coast_km <- standardize_distance(data[[col]], "m")
      data[[col]] <- NULL  # Remove original column
      cat("✓ Converted shoredistance (meters) to dist_to_coast_km\n")
    } else {
      data[[col]] <- standardize_distance(data[[col]])
      cat("✓ Standardized", col, "\n")
    }
  }

  # Validate temperature ranges (basic quality check)
  if ("sst" %in% names(data)) {
    temp_range <- range(data$sst, na.rm = TRUE)
    if (temp_range[1] < -5 || temp_range[2] > 35) {
      warning("Temperature values outside expected range [-5, 35°C]: ",
              paste(round(temp_range, 1), collapse = " to "))
    }
    cat("✓ Temperature range:", paste(round(temp_range, 1), collapse = " to "), "°C\n")
  }

  # Validate salinity ranges
  if ("sss" %in% names(data)) {
    sal_range <- range(data$sss, na.rm = TRUE)
    if (sal_range[1] < 0 || sal_range[2] > 40) {
      warning("Salinity values outside expected range [0, 40 PSU]: ",
              paste(round(sal_range, 1), collapse = " to "))
    }
    cat("✓ Salinity range:", paste(round(sal_range, 1), collapse = " to "), "PSU\n")
  }

  return(data)
}

# HABITAT STANDARDIZATION ====================================================

#' Standardize habitat classification to simplified categories
#' @param habitat_column Character vector of habitat classifications
#' @return Factor with standardized habitat categories
standardize_habitat_classification <- function(habitat_column) {

  # Define habitat mapping rules
  habitat_rules <- list(
    "Rock" = c("HabBenCircalitRock", "HabBenInfralitRock", "HabBenOffshRock",
               "rock", "rocky", "hard", "bedrock", "reef"),
    "Sand" = c("HabBenInfralitSand", "HabBenCircalitSand", "HabBenOffshSand",
               "sand", "sandy", "fine sand", "medium sand"),
    "Mixed_Sediment" = c("HabBenOffshMxdSed", "HabBenCircalitMxdSed", "HabBenInfralitMxdSed",
                        "mixed", "mixed sediment", "heterogeneous"),
    "Coarse_Sediment" = c("HabBenOffshCoarSed", "HabBenInfralitCoarSed", "HabBenCircalitCoarSed",
                         "coarse", "gravel", "pebble", "cobble"),
    "Mud" = c("HabBenOffshMud", "HabBenCircalitMud", "HabBenInfralitMud",
              "mud", "muddy", "clay", "silt", "fine"),
    "Unknown" = c("unknown", "unclassified", "", NA)
  )

  # Initialize result vector
  standardized <- rep("Other", length(habitat_column))

  # Apply mapping rules
  for (standard_name in names(habitat_rules)) {
    patterns <- habitat_rules[[standard_name]]

    # Create regex pattern (case insensitive)
    regex_pattern <- paste0("(", paste(patterns, collapse = "|"), ")")
    matches <- grepl(regex_pattern, habitat_column, ignore.case = TRUE)

    standardized[matches] <- standard_name
  }

  # Handle explicit NA values
  standardized[is.na(habitat_column)] <- "Unknown"

  # Convert to factor with defined levels
  standard_levels <- c("Rock", "Sand", "Mixed_Sediment", "Coarse_Sediment", "Mud", "Unknown", "Other")
  result <- factor(standardized, levels = standard_levels)

  # Report conversion summary
  conversion_summary <- table(result)
  cat("Habitat standardization summary:\n")
  print(conversion_summary)

  return(result)
}

# MAIN STANDARDIZATION FUNCTION ==============================================

#' Apply complete standardization to a dataset
#' @param data Data frame or sf object
#' @param standardize_values Whether to standardize values/units (default TRUE)
#' @return Standardized data frame
standardize_dataset <- function(data, standardize_values = TRUE) {

  cat("APPLYING VARIABLE STANDARDIZATION\n")
  cat("==================================\n")

  original_cols <- ncol(data)
  original_rows <- nrow(data)

  # Step 1: Standardize column names
  cat("1. Standardizing column names...\n")
  data <- standardize_column_names(data)

  # Step 2: Standardize values and units
  if (standardize_values) {
    cat("\n2. Standardizing values and units...\n")
    data <- standardize_environmental_values(data)
  }

  # Step 3: Standardize habitat classification
  if ("habitat_simplified" %in% names(data)) {
    cat("\n3. Standardizing habitat classification...\n")
    data$habitat_simplified <- standardize_habitat_classification(data$habitat_simplified)
  }

  # Summary
  cat("\nSTANDARDIZATION COMPLETE\n")
  cat("Rows:", original_rows, "→", nrow(data), "\n")
  cat("Columns:", original_cols, "→", ncol(data), "\n")
  cat("Standard variables present:", paste(intersect(names(data), unlist(STANDARD_VARIABLES)), collapse = ", "), "\n")

  return(data)
}

# VALIDATION FUNCTIONS =======================================================

#' Check if dataset follows standard variable conventions
#' @param data Data frame to validate
#' @return List with validation results
validate_standardization <- function(data) {

  results <- list(
    missing_standards = character(),
    non_standard_names = character(),
    value_issues = character(),
    warnings = character(),
    passed = TRUE
  )

  # Check for expected standard variables
  expected_vars <- c("depth_m", "sst", "sss", "dist_to_coast_km")
  missing <- setdiff(expected_vars, names(data))
  if (length(missing) > 0) {
    results$missing_standards <- missing
    results$warnings <- c(results$warnings, paste("Missing standard variables:", paste(missing, collapse = ", ")))
  }

  # Check for non-standard naming patterns
  non_standard <- names(data)[!names(data) %in% unlist(STANDARD_VARIABLES)]
  if (length(non_standard) > 0) {
    results$non_standard_names <- non_standard
  }

  # Validate depth values
  if ("depth_m" %in% names(data)) {
    if (any(data$depth_m > 0, na.rm = TRUE)) {
      results$value_issues <- c(results$value_issues, "Positive depth values found")
      results$passed <- FALSE
    }
  }

  return(results)
}
