# Bootstrap script for wrasse species occurrence records from OBIS
# This script downloads wrasse species distribution data for Scottish waters

# Load required libraries (remove this once the libraries section works)
if (!require("robis")) {
  cat("Installing robis package...\n")
  install.packages("robis")
  library(robis)
}
library(sf)
library(jsonlite)

# Before: nothing
# After: Spatial objects giving the distribution of wrasse records
# ============================================================================


# Define target species with common names for logging
TARGET_SPECIES <- list(
  "Labrus bergylta" = "Ballan wrasse",
  "Labrus mixtus" = "Cuckoo wrasse",
  "Centrolabrus exoletus" = "Rock cook",
  "Ctenolabrus rupestris" = "Goldsinny wrasse",
  "Symphodus melops" = "Corkwing wrasse"
)

# Process each species with robust error handling
species_summary <- data.frame(
  species = character(),
  common_name = character(),
  downloaded = integer(),
  cleaned = integer(),
  status = character(),
  stringsAsFactors = FALSE
)

# Read the bbox back
bbox <- st_read(GEOPACKAGE_PATH, "Study_Area", quiet = TRUE)

for(species_name in names(TARGET_SPECIES)) {
  common_name <- TARGET_SPECIES[[species_name]]
  cat(paste(rep("=", 60), collapse = ""), "\n")
  cat("PROCESSING:", species_name, "(", common_name, ")\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  # Initialize status tracking
  n_downloaded <- 0
     n_cleaned <- 0
        status <- "Failed"
  
  tryCatch({
    # Download species data
    obis_occurrences <- obis_occurrence(species_name)
    
    if (is.null(obis_occurrences)) {
      cat("No data available for", species_name, "\n")
      status <- "No data"
    } else {
      n_downloaded <- nrow(obis_occurrences)
      
      # Clean and filter the data, column checking
       required_cols <- c("basisOfRecord", "decimalLongitude", "decimalLatitude")
      available_cols <- intersect(names(obis_occurrences),
                                  c("basisOfRecord", "decimalLongitude", "decimalLatitude",
                                    "depth", "originalScientificName", "aphiaID",
                                    "shoredistance", "sst", "sss", "flags"))
      
      cat("Available columns:", paste(available_cols, collapse = ", "), "\n")
      
      # Clean data with available columns only
      temp <- obis_occurrences %>%
        dplyr::select(all_of(available_cols))
      
      # Apply quality filters if columns exist
      if ("flags" %in% available_cols) {
        temp <- temp %>% filter(is.na(flags) | flags == "")
      }
      
      # Remove records with invalid coordinates
      temp <- temp %>%
        filter(!is.na(decimalLongitude) & !is.na(decimalLatitude)) %>%
        filter(abs(decimalLongitude) <= 180 & abs(decimalLatitude) <= 90) %>%
        filter(decimalLongitude != 0 | decimalLatitude != 0)
      
      n_cleaned <- nrow(temp)
      
      # Convert to spatial object and crop to study area
      if (n_cleaned > 0) {
        cat("Converting", n_cleaned, "records to spatial format...\n")
        
        temp_sf <- temp %>%
          st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>%
          st_crop(bbox)
        
        # Generate layer name
        layer_name <- paste0(gsub("\\s", "_", species_name), "_records")
        
        # Save to geopackage
        success <- save_to_geopackage(temp_sf, layer_name, GEOPACKAGE_PATH)
        
        if (success) {
          cat("Successfully saved", n_cleaned, "records to layer:", layer_name, "\n")
          status <- "Success"
        } else {
          cat("Failed to save data to geopackage\n")
          status <- "Save failed"
        }
      } else {
        cat("No records found\n")
        status <- "No records"
      }
    }
  }, error = function(e) {
    cat("Error processing", species_name, ":", e$message, "\n")
    status <- paste("Error:", e$message)
  })
  
  # Add to summary
  species_summary <- rbind(species_summary, data.frame(
    species = species_name,
    common_name = common_name,
    downloaded = n_downloaded,
    cleaned = n_cleaned,
    status = status,
    stringsAsFactors = FALSE
  ))
  
  cat("Status:", status, "\n\n")
}

# Print summary table
cat("SPECIES DOWNLOAD SUMMARY\n")
cat("========================\n")
print(species_summary)

# Combine all species data
if (length(wrasse_data_list) > 0) {
  # Ensure all datasets have the same columns before combining
  # Get all column names across all datasets
  all_columns <- unique(unlist(lapply(wrasse_data_list, names)))

  # Standardize each dataset to have the same columns
  for (i in seq_along(wrasse_data_list)) {
    missing_cols <- setdiff(all_columns, names(wrasse_data_list[[i]]))
    if (length(missing_cols) > 0) {
      # Add missing columns with NA values
      for (col in missing_cols) {
        wrasse_data_list[[i]][[col]] <- NA
      }
    }
    # Reorder columns to match
    wrasse_data_list[[i]] <- wrasse_data_list[[i]][all_columns]
  }

  wrasse_occurrence <- do.call(rbind, wrasse_data_list)

  # Add download metadata
  attr(wrasse_occurrence, "download_date") <- Sys.Date()
  attr(wrasse_occurrence, "source") <- "OBIS (Ocean Biodiversity Information System)"
  attr(wrasse_occurrence, "bbox") <- bbox_coords
  attr(wrasse_occurrence, "species_searched") <- wrasse_species
}

  # Update metadata with species information
  if (file.exists("boot/initial/data/wrasse_metadata.json")) {
    metadata <- read_json("boot/initial/data/wrasse_metadata.json")
  } else {
    metadata <- list()
  }

  metadata$species_occurrences <- list(
    description = "Wrasse species occurrence records from OBIS",
    source = "OBIS (Ocean Biodiversity Information System)",
    download_date = as.character(Sys.Date()),
    bbox = bbox_coords,
    species_searched = wrasse_species,
    total_records = nrow(wrasse_occurrence),
    summary = lapply(split(wrasse_occurrence, wrasse_occurrence$scientificName), function(x) list(
      count = nrow(x),
      date_range = if("eventDate" %in% names(x)) range(x$eventDate, na.rm = TRUE) else "Unknown"
    ))
  )

  # Save updated metadata
  write_json(metadata, "boot/initial/data/wrasse_metadata.json", pretty = TRUE)

