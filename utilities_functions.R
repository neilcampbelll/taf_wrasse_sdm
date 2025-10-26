### Bespoke functions used in the SDM process

# Save to geopackage function
save_to_geopackage <- function(data_sf, layer_name, geopackage_path = "Scottish_Wrasse_SDM_Data.gpkg", 
                               overwrite = TRUE, quiet = FALSE) {
  # Ensure the data is valid SF object
  if (!inherits(data_sf, "sf")) {
    stop("Input data must be an sf object")
  }
  
  # Handle FID issues (common cause of write failures)
  if ("FID" %in% names(data_sf)) data_sf$FID <- NULL
  if ("fid" %in% names(data_sf)) data_sf$fid <- NULL
  
  # Reset row names (helps avoid issues)
  row.names(data_sf) <- NULL
  
  # Handle existing layers (delete_layer is safer than delete_dsn)
  delete_option <- if(overwrite) TRUE else FALSE
  
  # Attempt to write with error handling
  tryCatch({
    st_write(data_sf, 
             dsn = geopackage_path, 
             layer = layer_name,
             driver = "GPKG",
             delete_layer = delete_option,
             quiet = quiet)
    
    if(!quiet) cat("✓ Saved", nrow(data_sf), "features to layer:", layer_name, "\n")
    return(TRUE)
  }, error = function(e) {
    cat("✗ Error saving to layer", layer_name, ":", e$message, "\n")
    return(FALSE)
  })
}

# Add metadata to geopackage
add_metadata_to_geopackage <- function(geopackage_path) {
  # Create a metadata table
  metadata <- data.frame(
    parameter = c("creation_date", "created_by", "description", 
                  "crs", "extent_xmin", "extent_xmax", "extent_ymin", "extent_ymax"),
    value = c(
      as.character(Sys.Date()),
      "Scottish_Wrasse_SDM_Preparation_Script",
      "Consolidated environmental and species data for Scottish wrasse SDM",
      "EPSG:4326 (WGS84)",
      "-14.0", "9.0", "47.0", "63.0"  # Approximate Scottish waters extent
    ),
    stringsAsFactors = FALSE
  )
  
  # Convert to sf object (point geometry at origin for storage)
  metadata_sf <- st_sf(
    metadata,
    geometry = st_sfc(st_point(c(0, 0)), crs = 4326)
  )
  
  save_to_geopackage(metadata_sf, "metadata", geopackage_path)
  
  # Also add a layer listing
  layers <- st_layers(geopackage_path)
  layer_list <- data.frame(
    layer_name = layers$name,
    feature_count = layers$features,
    geometry_type = layers$geomtype,
    stringsAsFactors = FALSE
  )
  
  layer_list_sf <- st_sf(
    layer_list,
    geometry = st_sfc(replicate(nrow(layer_list), st_point(c(0, 0)), simplify = FALSE), crs = 4326)
  )
  
  save_to_geopackage(layer_list_sf, "layer_list", geopackage_path)
}


# distance calculation 
calculate_distances_in_batches <- function(grid_points, coastline, batch_size = 1000) {
  
  total_points <- nrow(grid_points)
  num_batches <- ceiling(total_points / batch_size)
  
  cat("Calculating distances in", num_batches, "batches of", batch_size, "points...\n")
  
  result_grid <- grid_points
  result_grid$dist_to_coast <- NA_real_
  
  # Progress reporting
  progress_interval <- max(1, floor(num_batches / 10))
  
  for (i in 1:num_batches) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, total_points)
    
    tryCatch({
      current_batch <- grid_points[start_idx:end_idx, ]
      
      # Calculate distance matrix
      dist_matrix <- st_distance(current_batch, coastline)
      
      # Find minimum distance for each point
      min_distances <- apply(dist_matrix, 1, min)
      
      # Validate distances
      if (any(is.na(min_distances) | min_distances < 0)) {
        warning("Invalid distances calculated in batch ", i)
      }
      
      result_grid$dist_to_coast[start_idx:end_idx] <- min_distances
      
      # Progress reporting
      if (i %% progress_interval == 0 || i == num_batches) {
        percent_complete <- round(100 * i / num_batches, 1)
        cat("  Batch", i, "/", num_batches, "(", percent_complete, "% complete)\n")
      }
      
    }, error = function(e) {
      stop("Distance calculation failed in batch ", i, ": ", e$message)
    })
  }
  
  cat("✓ Distance calculation completed\n")
  
  # Validate results
  n_missing <- sum(is.na(result_grid$dist_to_coast))
  if (n_missing > 0) {
    warning(n_missing, " points have missing distance values")
  }
  
  dist_range <- range(result_grid$dist_to_coast, na.rm = TRUE)
  cat("Distance range: ", round(as.numeric(dist_range[1]), 1), " to ",
      round(as.numeric(dist_range[2]), 1), " meters\n")
  
  
  return(result_grid)
}

# OBIS DATA DOWNLOAD =================================================

obis_occurrence <- function(species_name) {
  
  # Attempt download
  cat("Downloading OBIS data for", species_name, "...\n")
  
      # Download occurrence data
      occurrence_data <- robis::occurrence(scientificname = species_name)
      
      if (is.null(occurrence_data) || nrow(occurrence_data) == 0) {
        cat("No occurrence data found for", species_name, "\n")
        return(NULL)
      }
      
      # Validate data quality
      required_cols <- c("decimalLongitude", "decimalLatitude")
      if (!all(required_cols %in% names(occurrence_data))) {
        stop("Missing required coordinate columns in OBIS data")
      }
      
      # Remove records with missing coordinates
      clean_data <- occurrence_data %>%
        filter(!is.na(decimalLongitude) & !is.na(decimalLatitude)) %>%
        filter(abs(decimalLongitude) <= 180 & abs(decimalLatitude) <= 90)
      
      if (nrow(clean_data) == 0) {
        cat("No valid coordinates found for", species_name, "\n")
        return(NULL)
      }
      

  return(clean_data)
      
  
  
}
