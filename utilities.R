# ==========================
# UTILITIES FOR WRASSE SDM
# ==========================

# Check and install pacman if needed
if (!require(pacman, quietly = TRUE)) {
  install.packages("pacman")
}

# Load essential packages
pacman::p_load(

  # Spatial data handling
  sf,                    # Simple features for vector data
  terra,                 # Raster data handling

  # Data manipulation
  dplyr,                 # Data wrangling

  # Modeling packages
  randomForest,          # Random Forest models
  gbm,                   # Gradient Boosting Machine
  mgcv,                  # GAM models

  # Model evaluation
  pROC,                  # ROC curves and AUC
  blockCV,               # Spatial cross-validation

  # Data access
  robis,                 # Ocean biogeographic data
  marmap,                # Bathymetry data

  # Visualization
  ggplot2,               # Plotting
  viridis,               # Colour scales
  corrplot,              # Correlation plots
  patchwork,
  
  # Web access and parallel processing
  httr,                  # HTTP requests for data download
  doParallel,            # Parallel processing
  foreach                # Parallel loops
)

# Load core system components
source("utilities_standardization.R", local = TRUE)

# Load performance enhancement utilities
source("utilities_performance.R", local = TRUE)

# ENHANCED FUNCTIONS =========================================================

#' Enhanced save function
save_to_geopackage <- function(data_sf, layer_name,
                               geopackage_path = "Scottish_Wrasse_SDM_Data.gpkg",
                               overwrite = TRUE, standardize = TRUE) {

  # Validate input
  if (!inherits(data_sf, "sf")) {
    stop("Input must be an sf object")
  }

  # Apply standardization if requested
  if (standardize) {
    data_sf <- standardize_dataset(data_sf, standardize_values = TRUE)
  }

  # Clean data to avoid common write issues
  if ("FID" %in% names(data_sf)) data_sf$FID <- NULL
  if ("fid" %in% names(data_sf)) data_sf$fid <- NULL
  row.names(data_sf) <- NULL

  # Write to geopackage
  tryCatch({
    st_write(data_sf,
             dsn = geopackage_path,
             layer = layer_name,
             driver = "GPKG",
             delete_layer = overwrite,
             quiet = TRUE)

    cat("Saved", nrow(data_sf), "features to layer:", layer_name, "\n")
    return(TRUE)

  }, error = function(e) {
    cat("Error saving layer", layer_name, ":", e$message, "\n")
    return(FALSE)
  })
}


#' Check data quality for SDM modeling
#' @param data data.frame with environmental and occurrence data
#' @param response_col name of presence/absence column
check_data_quality <- function(data, response_col = "presence") {
  
  cat("=== DATA QUALITY CHECK ===\n")
  cat("Total records:", nrow(data), "\n")
  
  if (response_col %in% names(data)) {
    cat("Presences:", sum(data[[response_col]] == 1, na.rm = TRUE), "\n")
    cat("Absences/Background:", sum(data[[response_col]] == 0, na.rm = TRUE), "\n")
  }
  
  # Check for missing values in key environmental variables
  env_vars <- c("depth", "sst", "sss", "dist_to_coast")
  missing_counts <- sapply(env_vars[env_vars %in% names(data)], 
                           function(x) sum(is.na(data[[x]])))
  
  if (any(missing_counts > 0)) {
    cat("Missing values:\n")
    print(missing_counts[missing_counts > 0])
  } else {
    cat("No missing values in environmental variables\n")
  }
  
  # Check for outliers (values beyond 99th percentile)
  for (var in env_vars[env_vars %in% names(data)]) {
    if (is.numeric(data[[var]])) {
      q99 <- quantile(data[[var]], 0.99, na.rm = TRUE)
      q01 <- quantile(data[[var]], 0.01, na.rm = TRUE)
      outliers <- sum(data[[var]] > q99 | data[[var]] < q01, na.rm = TRUE)
      if (outliers > 0) {
        cat("Potential outliers in", var, ":", outliers, "records\n")
      }
    }
  }
  
  cat("==========================\n")
}

#' Initialize project geopackage with metadata
#' @param geopackage_path path to geopackage file 
#' @param overwrite whether to overwrite existing geopackage
#' @param target_crs EPSG code for project CRS
initialize_geopackage <- function(geopackage_path = "Scottish_Wrasse_SDM_Data.gpkg", 
                                  overwrite = FALSE,
                                  target_crs = 4326) {
  
  # Check if geopackage exists
  if (file.exists(geopackage_path)) {
    if (overwrite) {
      file.remove(geopackage_path)
      cat("Removed existing geopackage\n")
    } else {
      cat("Using existing geopackage:", geopackage_path, "\n")
      return(geopackage_path)
    }
  }
  
  # Create new geopackage with metadata
  cat("Creating new geopackage:", geopackage_path, "\n")
  
  # Create project metadata
  metadata <- data.frame(
    parameter = c("creation_date", "created_by", "description", "crs", 
                  "extent_xmin", "extent_xmax", "extent_ymin", "extent_ymax"),
    value = c(
      as.character(Sys.Date()),
      "Scottish_Wrasse_SDM",
      "Species Distribution Model data for Scottish wrasse species",
      paste0("EPSG:", target_crs),
      "-8.0", "0.0", "54.0", "60.0"  # Scottish waters extent
    ),
    stringsAsFactors = FALSE
  )
  
  # Convert to sf and save (using a point at origin for storage)
  metadata_sf <- st_sf(
    metadata,
    geometry = st_sfc(st_point(c(0, 0)), crs = target_crs)
  )
  
  st_write(metadata_sf, 
           dsn = geopackage_path, 
           layer = "project_metadata",
           driver = "GPKG",
           quiet = TRUE)
  
  cat("Geopackage initialized with metadata\n")
  return(geopackage_path)
}

#' Set consistent CRS for all spatial data
#' @param target_crs EPSG code for target coordinate reference system
set_project_crs <- function(target_crs = 4326) {
  
  # Commonly used CRS for UK waters
  crs_options <- list(
    "4326" = "WGS84 Geographic (suitable for global analysis)",
    "27700" = "British National Grid (accurate for UK, projected)",
    "3857" = "Web Mercator (for web mapping)"
  )
  
  if (as.character(target_crs) %in% names(crs_options)) {
    cat("Using CRS EPSG:", target_crs, "-", crs_options[[as.character(target_crs)]], "\n")
  } else {
    cat("Using CRS EPSG:", target_crs, "\n")
  }
  
  return(target_crs)
}

#' List all layers in the project geopackage
#' @param geopackage_path path to geopackage
list_geopackage_layers <- function(geopackage_path = GEOPACKAGE_PATH) {
  
  if (!file.exists(geopackage_path)) {
    cat("Geopackage not found:", geopackage_path, "\n")
    return(NULL)
  }
  
  layers <- st_layers(geopackage_path)
  
  cat("=== GEOPACKAGE CONTENTS ===\n")
  cat("File:", geopackage_path, "\n")
  cat("Size:", round(file.size(geopackage_path) / 1024^2, 1), "MB\n\n")
  
  if (length(layers$name) > 0) {
    layer_info <- data.frame(
      Layer = layers$name,
      Features = layers$features,
      Geometry = layers$geomtype,
      stringsAsFactors = FALSE
    )
    print(layer_info)
  } else {
    cat("No layers found in geopackage\n")
  }
  
  cat("===========================\n")
  return(layers)
}



# GLOBAL SETTINGS & INITIALIZATION =========================================

# Set consistent options for the project
options(
  # Suppress sf use_s2 warnings for simpler geometry operations
  sf_use_s2 = FALSE,
  
  # Set default string behaviour
  stringsAsFactors = FALSE,
  
  # Increase timeout for large downloads
  timeout = 300
)

# Set default CRS for project (can be changed as needed)
PROJECT_CRS <- set_project_crs(4326)  # WGS84 Geographic

# File paths (TAF-relative)

OUTPUT_PATH <- "data/modelling_data.gpkg"

GEOPACKAGE_PATH <- "boot/initial/data/Scottish_Wrasse_SDM_Data.gpkg"

BOOT_GEOPACKAGE_PATH <- "boot/data/Scottish_Wrasse_SDM_Data.gpkg"


# Initialize the project geopackage
dir.create("boot/initial/data", recursive = TRUE, showWarnings = FALSE)
dir.create("data", recursive = TRUE, showWarnings = FALSE)
dir.create("boot/data", recursive = TRUE, showWarnings = FALSE)


GEOPACKAGE_PATH <- initialize_geopackage(GEOPACKAGE_PATH, overwrite = FALSE, target_crs = PROJECT_CRS)
OUTPUT_PATH <- initialize_geopackage(OUTPUT_PATH, overwrite = FALSE, target_crs = PROJECT_CRS)

