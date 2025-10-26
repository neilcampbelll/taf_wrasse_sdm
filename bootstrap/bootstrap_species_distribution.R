# Bootstrap script for wrasse species occurrence records from OBIS
# This script downloads wrasse species distribution data for Scottish waters

# Load required libraries
if (!require("robis")) {
  cat("Installing robis package...\n")
  install.packages("robis")
  library(robis)
}

# Define study area bounding box (Scottish waters)
# These coordinates match the bounding box definition
bbox_coords <- list(
  min_longitude = -8.5,
  max_longitude = -0.5,
  min_latitude = 54.5,
  max_latitude = 61.0
)

# Common wrasse species found in Scottish waters
wrasse_species <- c(
  "Labrus bergylta",      # Ballan wrasse
  "Labrus mixtus",        # Cuckoo wrasse
  "Symphodus melops",     # Corkwing wrasse
  "Centrolabrus exoletus", # Rock cook
  "Ctenolabrus rupestris"  # Goldsinny wrasse
)

cat("Downloading wrasse occurrence data from OBIS...\n")

# Initialize empty list to store data
wrasse_data_list <- list()

# Download occurrence data for each species
for (species in wrasse_species) {
  cat("Downloading data for", species, "...\n")

  try({
    # Download occurrence data within bounding box
    species_data <- occurrence(
      scientificname = species,
      geometry = paste0("POLYGON((",
                       bbox_coords$min_longitude, " ", bbox_coords$min_latitude, ",",
                       bbox_coords$max_longitude, " ", bbox_coords$min_latitude, ",",
                       bbox_coords$max_longitude, " ", bbox_coords$max_latitude, ",",
                       bbox_coords$min_longitude, " ", bbox_coords$max_latitude, ",",
                       bbox_coords$min_longitude, " ", bbox_coords$min_latitude, "))")
    )

    if (nrow(species_data) > 0) {
      wrasse_data_list[[species]] <- species_data
      cat("Found", nrow(species_data), "records for", species, "\n")
    } else {
      cat("No records found for", species, "\n")
    }
  }, silent = TRUE)
}

# Combine all species data
if (length(wrasse_data_list) > 0) {
  wrasse_occurrence <- do.call(rbind, wrasse_data_list)

  # Add download metadata
  attr(wrasse_occurrence, "download_date") <- Sys.Date()
  attr(wrasse_occurrence, "source") <- "OBIS (Ocean Biodiversity Information System)"
  attr(wrasse_occurrence, "bbox") <- bbox_coords
  attr(wrasse_occurrence, "species_searched") <- wrasse_species

  # Save the occurrence data
  save(wrasse_occurrence, file = "species_distribution.RData")

  # Also save as CSV
  write.csv(wrasse_occurrence, "species_distribution.csv", row.names = FALSE)

  cat("Species distribution data downloaded successfully\n")
  cat("Total records:", nrow(wrasse_occurrence), "\n")
  cat("Species with data:", length(unique(wrasse_occurrence$scientificName)), "\n")

} else {
  cat("No occurrence data found for any wrasse species in the specified area\n")
  # Create empty data frame with expected structure
  wrasse_occurrence <- data.frame(
    scientificName = character(0),
    decimalLongitude = numeric(0),
    decimalLatitude = numeric(0),
    eventDate = character(0),
    stringsAsFactors = FALSE
  )
  save(wrasse_occurrence, file = "species_distribution.RData")
}