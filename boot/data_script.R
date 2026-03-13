## data_script.R

# before: nothing
# after: all the data needed to run the model in a single geopackage

# C-squares scale parameter
csq_scale <- 0.05

# Source individual data processing scripts
source("boot/data_coastline.R")               # broad coastline for modeling (rnaturalearth)
source("boot/data_fine_scottish_coast.R")     # finer Scottish coastline (includes outlying islands)
source("boot/data_scottish_inshore_waters.R") # polygon of Scottish territorial waters (12nm)
source("boot/data_habitats.R")                # benthic habitat layer - use with care, slow to run
source("boot/data_coastal_distance.R")        # distance-to-coast grid within Scottish waters
source("boot/data_salinity.R")                # download and process salinity data
source("boot/data_SST.R")                     # download and process sea surface temperature data
source("boot/data_bathymetry.R")              # download and process bathymetry data


source("boot/data_model_processing.R")        # create environmental prediction grid
source("boot/data_observation_processing.R")  # species records with habitat assignment
source("boot/data_pseudoabsences_marine.R")   # marine pseudo-absences for all species

cat("\nData Preparation Complete\n")
cat("data layers saved to:", GEOPACKAGE_PATH, "\n")
cat("Model input data and prediction grid saved to:", OUTPUT_PATH, "\n")

# Update project metadata JSON with data source information
metadata_path <- "boot/initial/data/wrasse_metadata.json"
metadata <- if (file.exists(metadata_path)) jsonlite::read_json(metadata_path) else list()

metadata$environmental_layers <- list(
  coastline = list(
    description = "European/North Atlantic coastline clipped to study area",
    source = "Natural Earth (via rnaturalearth package)",
    scale = "1:10m",
    countries = c("United Kingdom", "Ireland", "France", "Norway", "Denmark",
                  "Germany", "Faroe Islands", "Sweden", "Belgium", "Netherlands",
                  "Jersey", "Guernsey", "Isle of Man", "Luxembourg"),
    layer = "Coastline"
  ),
  scottish_coastline = list(
    description = "Detailed Scottish coastline including outlying islands",
    source = "ONS/ONS Geography - ITL1 January 2025 UK BFC",
    source_url = "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/ITL1_JAN_2025_UK_BFC/FeatureServer",
    layer = "Scottish_Coastline"
  ),
  scottish_inshore_waters = list(
    description = "Scottish Marine Regions defining the inshore study extent (~12nm zone)",
    source = "NatureScot / Atkins Geospatial WFS",
    source_url = "https://msmap1.atkinsgeospatial.com/geoserver/nmpwfs",
    layer = "Scottish_Inshore_Waters"
  ),
  coastal_distances = list(
    description = paste0("Distance to nearest coastline for each grid point, at ", csq_scale, " degree resolution"),
    source = "Derived from Coastline and Scottish_Coastline layers",
    resolution_degrees = csq_scale,
    layer = "Scottish_Coastal_Distances"
  ),
  salinity = list(
    description = "Annual average sea surface salinity",
    source = "Marine Scotland Climate Database v01-2",
    source_url = "https://data.marine.gov.scot",
    variable = "ann_avg_saltS",
    layer = "sss_salinity"
  ),
  sst = list(
    description = "Annual average sea surface temperature",
    source = "Marine Scotland Climate Database v01-2",
    source_url = "https://data.marine.gov.scot",
    variable = "ann_avg_tempS",
    layer = "sst_temperature"
  ),
  bathymetry = list(
    description = "Bathymetry (water depth) at 1 arc-minute resolution",
    source = "NOAA ETOPO via marmap package",
    resolution_arcmin = 1,
    layer = "bathymetry"
  ),
  habitats = list(
    description = "MSFD Broad-scale Benthic Habitat Types (EMODnet 2023)",
    source = "ICES SharePoint (originally EMODnet Seabed Habitats)",
    layer = "MSFD_Habitats",
    note = "Large file (~1.2 GB). Script currently disabled in data_script.R — run manually."
  )
)

metadata$derived_layers <- list(
  environmental_grid = list(
    description = "Prediction grid for Scottish inshore waters with all environmental predictors assigned",
    variables = c("depth_m", "sss", "sst", "dist_to_coast_km", "MSFD_BBHT"),
    resolution_degrees = csq_scale,
    geopackage = OUTPUT_PATH,
    layer = "environmental_grid"
  ),
  species_observations = list(
    description = "OBIS species occurrence records with habitat assigned, one layer per species",
    processing = "Habitat assigned by point-in-polygon intersection, nearest-neighbour fallback",
    geopackage = OUTPUT_PATH
  ),
  pseudo_absences = list(
    description = "Marine pseudo-absences for each species, distance-weighted stratified sampling",
    ratio_to_presences = 3,
    min_distance_km = 1,
    strata = c("coastal (0-5km)", "nearshore (5-15km)", "midshore (15-50km)", "offshore (50-200km)"),
    geopackage = OUTPUT_PATH
  )
)

jsonlite::write_json(metadata, metadata_path, pretty = TRUE, auto_unbox = TRUE)

