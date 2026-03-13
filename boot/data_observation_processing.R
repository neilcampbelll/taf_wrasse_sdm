# ============================================================================
# DATA OBSERVATION PROCESSING 
# ============================================================================
# Before: Species occurrence records from OBIS with basic coordinates
# After: Enhanced species records with habitat assignments and standardized variables
# ============================================================================

# LOAD UTILITIES AND CONFIGURATION ==========================================
source("utilities.R")

cat("=== DATA PROCESSING ===\n")

OUTPUT_PATH <- "data/modelling_data.gpkg"

# TARGET SPECIES LIST (from project configuration)
WRASSE_SPECIES <- c("Labrus bergylta", "Labrus mixtus", "Centrolabrus exoletus",
                    "Ctenolabrus rupestris", "Symphodus melops")

cat("Processing", length(WRASSE_SPECIES), "wrasse species:\n")
for(sp in WRASSE_SPECIES) cat("-", sp, "\n")
cat("\n")

# LOAD HABITAT DATA ======================================================
cat("Step 1: Loading benthic habitat data...\n")

tryCatch({
  # Check if habitat layer exists
  available_layers <- st_layers(GEOPACKAGE_PATH)$name

  if(!"MSFD_Habitats" %in% available_layers) {
    stop("MSFD_Habitats layer not found. Available layers: ",
         paste(available_layers, collapse = ", "))
  }

  # Load habitat layer with validation
  habitat_layer <- st_read(GEOPACKAGE_PATH, layer = "MSFD_Habitats", quiet = TRUE)

  # Validate habitat layer
  if(nrow(habitat_layer) == 0) {
    stop("Habitat layer is empty")
  }

  if(!inherits(habitat_layer, "sf")) {
    stop("Habitat layer is not a valid sf object")
  }

  # Identify habitat attribute columns
  habitat_cols <- names(habitat_layer)[!names(habitat_layer) %in% c("geometry", "geom")]

  cat("Loaded habitat layer with", nrow(habitat_layer), "polygons\n")
  cat("Available habitat attributes:", paste(habitat_cols, collapse = ", "), "\n")

}, error = function(e) {
  stop("Failed to load habitat data: ", e$message)
})


# PROCESS EACH SPECIES ===================================================
cat("\nStep 2: Processing species occurrence data...\n")

# Process each wrasse species individually
for(species_name in WRASSE_SPECIES) {

  cat("\n--- Processing:", species_name, "---\n")

  # Generate layer name following project conventions
  layer_name <- paste0(gsub("\\s", "_", species_name), "_records")

  # Load species occurrence data with validation
  tryCatch({

    # Check if species layer exists
    if(!layer_name %in% st_layers(GEOPACKAGE_PATH)$name) {
      cat("Warning: Layer", layer_name, "not found, skipping species\n")
      next
    }

    # Load species data
    wrasse_points <- st_read(GEOPACKAGE_PATH, layer = layer_name, quiet = TRUE)

    # Validate species data
    if(nrow(wrasse_points) == 0) {
      cat("Warning: No records found for", species_name, ", skipping\n")
      next
    }

    cat("✓ Loaded", nrow(wrasse_points), "occurrence records\n")
  
    # HABITAT ASSIGNMENT ================================================
    cat("Assigning habitat values to occurrence points...\n")

    # Transform to British National Grid for accurate spatial operations
    cat("Transforming to British National Grid (EPSG:27700)...\n")
    wrasse_proj <- st_transform(wrasse_points, 27700)
    habitat_proj <- st_transform(habitat_layer, 27700)


    # METHOD 1: Point-in-polygon intersection (primary method)
    cat("Step 1: Point-in-polygon intersection...\n")

    intersected <- tryCatch({
      st_intersection(wrasse_proj, habitat_proj)
    }, error = function(e) {
      cat("Intersection failed, using alternative approach:\n")
      cat("  Error:", e$message, "\n")
      return(NULL)
    })

    if(is.null(intersected)) {
      # Fallback: use st_join if intersection fails
      intersected <- st_join(wrasse_proj, habitat_proj, join = st_within)
      intersected <- intersected[!is.na(intersected[[habitat_cols[1]]]), ]
    }

    # Assess intersection success
    points_with_habitat <- nrow(intersected)
    points_without_habitat <- nrow(wrasse_points) - points_with_habitat

    cat("Points with habitat data:", points_with_habitat, "\n")
    cat("Points requiring nearest neighbor:", points_without_habitat, "\n")


    # METHOD 2: Nearest neighbor for points without habitat assignment
    if(points_without_habitat > 0) {
      cat("Step 2: Nearest neighbor assignment for", points_without_habitat, "points...\n")

      # Identify points that need nearest neighbor assignment
      intersected_ids <- as.numeric(row.names(intersected))
      missing_ids <- setdiff(1:nrow(wrasse_proj), intersected_ids)
      missing_points <- wrasse_proj[missing_ids, ]

      # Find nearest habitat polygon for each missing point
      nearest_indices <- st_nearest_feature(missing_points, habitat_proj)
      nearest_habitat <- habitat_proj[nearest_indices, ]

      # Calculate assignment distances for quality control
      distances <- st_distance(missing_points, nearest_habitat, by_element = TRUE)
      distances_km <- as.numeric(distances) / 1000

      cat("Nearest neighbor distances - Mean:", round(mean(distances_km), 2),
          "km, Max:", round(max(distances_km), 2), "km\n")

      # Quality control: warn about very distant assignments
      distant_assignments <- sum(distances_km > 10)
      if(distant_assignments > 0) {
        cat("Warning:", distant_assignments, "points assigned habitat >10km away\n")
      }

      # Create complete dataset with habitat assignments
      missing_with_habitat <- missing_points

      # Add habitat attributes from nearest neighbors
      for(col in habitat_cols) {
        if(col %in% names(nearest_habitat)) {
          missing_with_habitat[[col]] <- st_drop_geometry(nearest_habitat)[[col]]
        }
      }

      # Harmonize column structure between datasets
      intersected_cols <- names(intersected)
      original_cols <- names(wrasse_proj)
      habitat_only_cols <- setdiff(intersected_cols, original_cols)

      # Ensure missing_with_habitat has all required columns
      for(col in habitat_only_cols) {
        if(!col %in% names(missing_with_habitat)) {
          # Determine appropriate NA type
          if(col %in% names(intersected)) {
            col_class <- class(intersected[[col]])[1]
            missing_with_habitat[[col]] <- switch(col_class,
              "character" = NA_character_,
              "numeric" = NA_real_,
              "integer" = NA_integer_,
              NA
            )
          }
        }
      }

      # Reorder columns to match intersected dataset
      missing_with_habitat <- missing_with_habitat[, intersected_cols]

      # Combine all data
      all_with_habitat <- rbind(intersected, missing_with_habitat)

      # Restore original point order
      all_ids <- c(intersected_ids, missing_ids)
      reorder_indices <- order(all_ids)
      all_with_habitat <- all_with_habitat[reorder_indices, ]

    } else {
      all_with_habitat <- intersected
      cat("All points successfully assigned habitat via intersection\n")
    }


    # FINALIZE SPECIES DATA =============================================

    # Transform back to project CRS (WGS84)
    wrasse_with_habitat <- st_transform(all_with_habitat, st_crs(wrasse_points))

    # Apply standardization system to ensure consistent variable names and units
    cat("Applying standardization system...\n")
    wrasse_with_habitat <- standardize_dataset(wrasse_with_habitat, standardize_values = TRUE)


    # DATA QUALITY ASSESSMENT ===========================================
    cat("\n--- Quality Assessment for", species_name, "---\n")
    cat("Total observations with habitat:", nrow(wrasse_with_habitat), "\n")

    # Assess habitat assignment completeness
    for(col in habitat_cols) {
      if(col %in% names(wrasse_with_habitat)) {
        na_count <- sum(is.na(wrasse_with_habitat[[col]]))
        complete_pct <- round((1 - na_count/nrow(wrasse_with_habitat)) * 100, 1)
        cat(" ", col, "completeness:", complete_pct, "% (", na_count, "missing)\n")

        # Summarize categorical habitat variables
        if(is.character(wrasse_with_habitat[[col]]) || is.factor(wrasse_with_habitat[[col]])) {
          habitat_summary <- table(wrasse_with_habitat[[col]], useNA = "ifany")

          if(length(habitat_summary) <= 10) {
            cat("  Habitat distribution:\n")
            for(i in 1:length(habitat_summary)) {
              cat("    ", names(habitat_summary)[i], ":", habitat_summary[i], "\n")
            }
          } else {
            cat("  ", length(habitat_summary), "habitat categories (showing top 5):\n")
            top_habitats <- sort(habitat_summary, decreasing = TRUE)[1:5]
            for(i in 1:length(top_habitats)) {
              cat("    ", names(top_habitats)[i], ":", top_habitats[i], "\n")
            }
          }
        }
      }
    }


    # FINAL VALIDATION ===============================================

    # Check for spatial validity
    if(!all(st_is_valid(wrasse_with_habitat))) {
      cat("Warning: Some geometries are invalid, attempting repair...\n")
      wrasse_with_habitat <- st_make_valid(wrasse_with_habitat)
    }

    # Ensure consistent CRS
    if(st_crs(wrasse_with_habitat)$epsg != 4326) {
      cat("Warning: CRS mismatch, transforming to WGS84...\n")
      wrasse_with_habitat <- st_transform(wrasse_with_habitat, 4326)
    }


    # SAVE ENHANCED SPECIES DATA ====================================
    cat("\nSaving species data...\n")

    # Save using enhanced geopackage function with standardization
    success <- save_to_geopackage(
      data_sf = wrasse_with_habitat,
      layer_name = layer_name,
      geopackage_path = OUTPUT_PATH,
      overwrite = TRUE,
      standardize = FALSE  # Already standardized above
    )

    if(success) {
      cat("Saved", species_name, "data with habitat assignments\n")
    } else {
      cat("Failed to save", species_name, "data\n")
    }


    # OPTIONAL: CREATE HABITAT DISTRIBUTION PLOT ====================
    if(get_config(c("visualization", "create_plots"), FALSE)) {

      # Check if primary habitat column exists
      primary_habitat_col <- NULL
      for(col in c("MSFD_BBHT", "habitat_type", "substrate_type")) {
        if(col %in% names(wrasse_with_habitat)) {
          primary_habitat_col <- col
          break
        }
      }

      if(!is.null(primary_habitat_col)) {
        # Create plot data (remove NA values)
        plot_data <- wrasse_with_habitat[!is.na(wrasse_with_habitat[[primary_habitat_col]]), ]

        if(nrow(plot_data) > 0) {
          cat("Creating habitat distribution plot...\n")

          # Ensure plots directory exists
          if(!dir.exists("plots")) dir.create("plots", showWarnings = FALSE)

          # Create habitat distribution plot
          habitat_plot <- ggplot(plot_data) +
            geom_sf(aes_string(color = primary_habitat_col), size = 0.8, alpha = 0.7) +
            labs(
              title = paste(species_name, "- Habitat Distribution"),
              subtitle = paste("n =", nrow(plot_data), "observations with habitat data"),
              color = "Habitat Type"
            ) +
            theme_minimal() +
            theme(
              legend.position = "bottom",
              legend.text = element_text(size = 8),
              axis.text = element_text(size = 8),
              plot.title = element_text(size = 12, face = "bold")
            ) +
            guides(color = guide_legend(ncol = 2, override.aes = list(size = 2)))

          # Save plot
          plot_filename <- file.path("plots", paste0(gsub("\\s", "_", species_name), "_habitat_distribution.png"))

          tryCatch({
            ggsave(plot_filename, habitat_plot, width = 12, height = 8, dpi = 300)
            cat("Saved habitat plot:", plot_filename, "\n")
          }, error = function(e) {
            cat("Failed to save plot:", e$message, "\n")
          })
        }
      }
    }

  }, error = function(e) {
    cat("Error processing", species_name, ":", e$message, "\n")
  })

}
cat("DATA PROCESSING COMPLETE")