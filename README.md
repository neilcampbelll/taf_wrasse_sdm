# Wrasse Species Distribution Model — Northeast Atlantic

A species distribution model (SDM) for five wrasse species in the northeast Atlantic, developed following the [ICES Transparent Assessment Framework (TAF)](https://github.com/ices-taf/doc/wiki/Creating-a-TAF-analysis).

## Target species

| Scientific name | Common name |
|---|---|
| *Labrus bergylta* | Ballan wrasse |
| *Labrus mixtus* | Cuckoo wrasse |
| *Centrolabrus exoletus* | Rock cook |
| *Ctenolabrus rupestris* | Goldsinny wrasse |
| *Symphodus melops* | Corkwing wrasse |

## Study area

Northeast Atlantic / Scottish waters: longitude −14° to 9° E, latitude 47° to 63° N (WGS84, EPSG:4326).

---

## Repository structure

This project follows the standard TAF directory layout:

```
taf_wrasse_sdm/
├── boot/                        # Data and software bootstrapping
│   ├── SOFTWARE.bib             # Pinned R package versions
│   ├── DATA.bib                 # Data source registry
│   ├── get_bounding_box.R       # Creates study area polygon
│   ├── get_obis_data.R          # Downloads species occurrence records from OBIS
│   ├── data_script.R            # Master data preparation script (sources all below)
│   ├── data_coastline.R         # Broad NE Atlantic coastline (Natural Earth)
│   ├── data_fine_scottish_coast.R  # Detailed Scottish coastline incl. outlying islands
│   ├── data_scottish_inshore_waters.R  # Scottish Marine Regions (~12 nm zone)
│   ├── data_coastal_distance.R  # Distance-to-coast grid within Scottish waters
│   ├── data_salinity.R          # Annual average sea surface salinity
│   ├── data_SST.R               # Annual average sea surface temperature
│   ├── data_bathymetry.R        # Bathymetry (NOAA ETOPO, 1 arc-minute)
│   ├── data_habitats.R          # MSFD broad benthic habitat types (slow — disabled by default)
│   ├── data_model_processing.R  # Assembles environmental prediction grid
│   ├── data_observation_processing.R  # Species records with habitat assignment
│   └── data_pseudoabsences_marine.R   # Marine pseudo-absences for each species
├── data/                        # Processed data (TAF-managed)
├── model/                       # Model outputs (TAF-managed)
├── output/                      # Derived outputs (TAF-managed)
├── report/                      # Report files (TAF-managed)
├── wrasse_sdm_script.R          # Main entry point
├── libraries.R                  # Loads TAF-managed packages
├── utilities.R                  # Project settings, paths, and core functions
├── utilities_functions.R        # Bespoke spatial and data functions
├── utilities_standardization.R  # Variable naming and unit conventions
└── utilities_performance.R      # Parallel processing and batch prediction helpers
```

---

## How to run

The main entry point is `wrasse_sdm_script.R`. Source this file from the project root directory. It runs the workflow in the following order:

1. Boots the TAF software library (`taf.boot()`)
2. Loads packages (`libraries.R`)
3. Sets up the project environment (`utilities.R`)
4. Creates the study area bounding box (`boot/get_bounding_box.R`)
5. Downloads species occurrence records from OBIS (`boot/get_obis_data.R`)
6. Boots the TAF data (`taf.boot()`)
7. Prepares all environmental data layers (`boot/data_script.R`)

---

## Data sources

All spatial data are stored in a GeoPackage at `boot/initial/data/Scottish_Wrasse_SDM_Data.gpkg` during bootstrapping. Processed modelling data is written to `data/modelling_data.gpkg`.

| Layer (geopackage) | Source | Script |
|---|---|---|
| `Study_Area` | Defined internally | `get_bounding_box.R` |
| `Labrus_bergylta_records` (+ 4 other species) | [OBIS](https://obis.org) | `get_obis_data.R` |
| `Coastline` | Natural Earth 1:10m (via `rnaturalearth`) | `data_coastline.R` |
| `Scottish_Coastline` | ONS ITL1 UK Boundary File Jan 2025 (ArcGIS Online) | `data_fine_scottish_coast.R` |
| `Scottish_Inshore_Waters` | Scottish Marine Regions WFS (NatureScot / Atkins Geospatial) | `data_scottish_inshore_waters.R` |
| `Scottish_Coastal_Distances` | Derived from coastline layers | `data_coastal_distance.R` |
| `sss_salinity` | Marine Scotland Climate Database v01-2 (`ann_avg_saltS`) | `data_salinity.R` |
| `sst_temperature` | Marine Scotland Climate Database v01-2 (`ann_avg_tempS`) | `data_SST.R` |
| `bathymetry` | NOAA ETOPO 1 arc-minute (via `marmap`) | `data_bathymetry.R` |
| `MSFD_Habitats` | EMODnet Seabed Habitats 2023 (hosted via ICES) | `data_habitats.R` |
| `environmental_grid` (modelling gpkg) | Derived — all environmental layers joined to Scottish waters grid | `data_model_processing.R` |
| `*_records` (modelling gpkg) | Derived — OBIS records with habitat assigned | `data_observation_processing.R` |
| `*_pseudo_absences_extended` (modelling gpkg) | Derived — marine pseudo-absences | `data_pseudoabsences_marine.R` |

### Environmental predictors

The following variables are used as predictors in the SDM:

| Variable | Description | Units |
|---|---|---|
| `depth_m` | Water depth (negative = below sea level) | metres |
| `sst` | Annual average sea surface temperature | °C |
| `sss` | Annual average sea surface salinity | PSU |
| `dist_to_coast_km` | Distance to nearest coastline | km |
| `MSFD_BBHT` | MSFD broad-scale benthic habitat type | categorical |

### Pseudo-absence design

Pseudo-absences are generated separately for each species using distance-weighted stratified sampling from marine bathymetry points, at a 3:1 ratio to presences. Points within 1 km of any presence record are excluded. Sampling is stratified by distance to coast:

| Stratum | Distance range | Proportion |
|---|---|---|
| Coastal | 0–5 km | 50% |
| Nearshore | 5–15 km | 25% |
| Midshore | 15–50 km | 15% |
| Offshore | 50–200 km | 10% |

*Labrus mixtus* (Cuckoo wrasse) uses a modified offshore-weighted stratum due to its deeper-water distribution.

---

## Dependencies

All R packages are version-pinned in `boot/SOFTWARE.bib` and managed by `icesTAF`. They are installed to `boot/library/` rather than the system library. Key packages:

| Package | Purpose |
|---|---|
| `sf`, `terra` | Spatial data handling |
| `dplyr` | Data manipulation |
| `marmap` | Bathymetry download (NOAA ETOPO) |
| `rnaturalearth`, `rnaturalearthdata` | Coastline data |
| `robis` | OBIS species occurrence download |
| `httr` | HTTP downloads |
| `curl` | Large file downloads (habitat layer) |
| `randomForest`, `gbm`, `mgcv`, `INLA` | SDM algorithms |
| `ggplot2` | Visualisation |

---

## Variable naming conventions

All datasets follow the naming conventions defined in `utilities_standardization.R`. Key standards:

- Depth: `depth_m`, negative values (e.g. −50 m = 50 m below sea level)
- Distance to coast: `dist_to_coast_km`
- Temperature: `sst`
- Salinity: `sss`
- Species name: `species`
- Presence/absence: `presence` (1 = presence, 0 = pseudo-absence)

---

## Metadata

A project metadata file is maintained at `boot/initial/data/wrasse_metadata.json`. It is built up progressively as the workflow runs, recording the study area definition, OBIS download details, and the source and provenance of each environmental data layer.
