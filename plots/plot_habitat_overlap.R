# plots/plot_habitat_overlap.R
# Suitable habitat maps per species (binary at MaxSSS threshold) and a
# combined species richness map showing overlap
# Outputs:
#   output/habitat_suitability.png  — 6-panel: 5 species + richness
#   output/habitat_richness.png     — richness map alone at larger size

cat("Plotting habitat suitability and species overlap...\n")

WRASSE_SPECIES <- list(
  "Labrus_bergylta"       = "Ballan wrasse",
  "Labrus_mixtus"         = "Cuckoo wrasse",
  "Centrolabrus_exoletus" = "Rock cook",
  "Ctenolabrus_rupestris" = "Goldsinny wrasse",
  "Symphodus_melops"      = "Corkwing wrasse"
)

# Species colours for individual panels
SP_COLOURS <- c(
  "Labrus_bergylta"       = "#2ecc71",
  "Labrus_mixtus"         = "#e67e22",
  "Centrolabrus_exoletus" = "#9b59b6",
  "Ctenolabrus_rupestris" = "#e74c3c",
  "Symphodus_melops"      = "#3498db"
)

# Load MaxSSS thresholds
thresholds_df <- read.csv("model/habitat_thresholds.csv", stringsAsFactors = FALSE)

coastline       <- st_read(GEOPACKAGE_PATH, "Coastline",               quiet = TRUE)
scottish_waters <- st_read(GEOPACKAGE_PATH, "Scottish_Inshore_Waters", quiet = TRUE)
scot_bb         <- st_bbox(scottish_waters)
xlim <- c(scot_bb["xmin"] - 0.3, scot_bb["xmax"] + 0.3)
ylim <- c(scot_bb["ymin"] - 0.3, scot_bb["ymax"] + 0.3)

theme_hab <- theme_minimal() +
  theme(axis.title      = element_blank(),
        axis.text       = element_text(size = 6),
        panel.grid      = element_line(colour = "grey90"),
        plot.title      = element_text(face = "bold.italic", size = 10),
        plot.subtitle   = element_text(size = 7, colour = "grey40"),
        legend.position = "right",
        legend.title    = element_text(size = 8),
        legend.text     = element_text(size = 7))

# Accumulate binary suitable layers for richness calculation
richness_df <- NULL
species_panels <- list()

for (sp in names(WRASSE_SPECIES)) {

  common <- WRASSE_SPECIES[[sp]]
  sp_tag <- tolower(sp)

  pred_gpkg  <- file.path("model", paste0(sp_tag, "_predictions.gpkg"))
  pred_layer <- paste0(sp_tag, "_predictions")

  if (!file.exists(pred_gpkg)) {
    cat("  Skipping", common, "(no prediction file)\n")
    next
  }

  # Get MaxSSS threshold for this species
  thr_row <- thresholds_df[thresholds_df$species == common, ]
  if (nrow(thr_row) == 0) {
    cat("  Skipping", common, "(no threshold found)\n")
    next
  }
  thr <- thr_row$maxsss_threshold

  pred_sf  <- st_read(pred_gpkg, pred_layer, quiet = TRUE)
  pred_sf  <- pred_sf[!is.na(pred_sf$pred_ensemble), ]
  coords   <- st_coordinates(pred_sf)

  sp_df <- data.frame(
    lon      = coords[, 1],
    lat      = coords[, 2],
    ensemble = pred_sf$pred_ensemble,
    suitable = as.integer(pred_sf$pred_ensemble >= thr)
  )

  cat(" ", common, "— threshold:", thr, "|",
      sum(sp_df$suitable), "/", nrow(sp_df), "cells suitable\n")

  # Accumulate richness
  sp_contrib <- data.frame(lon = sp_df$lon, lat = sp_df$lat,
                            suitable = sp_df$suitable)
  names(sp_contrib)[3] <- sp

  richness_df <- if (is.null(richness_df)) {
    sp_contrib
  } else {
    merge(richness_df, sp_contrib, by = c("lon", "lat"), all = TRUE)
  }

  # Individual species panel
  sp_col <- SP_COLOURS[sp]
  suitable_df <- sp_df[sp_df$suitable == 1, ]

  species_panels[[sp]] <- ggplot() +
    geom_sf(data = scottish_waters, fill = "#f0f4f8", colour = NA) +
    geom_tile(data = sp_df[sp_df$suitable == 0, ],
              aes(x = lon, y = lat), fill = "grey88",
              width = 0.05, height = 0.05) +
    geom_tile(data = suitable_df,
              aes(x = lon, y = lat), fill = sp_col,
              width = 0.05, height = 0.05, alpha = 0.85) +
    geom_sf(data = coastline, fill = "#ece8e1", colour = "grey50", linewidth = 0.2) +
    coord_sf(xlim = xlim, ylim = ylim) +
    labs(title    = common,
         subtitle = paste0("MaxSSS threshold: ", thr, "  |  ",
                           round(sum(sp_df$suitable) * 16.9 / 1000, 1),
                           "k km²")) +
    theme_hab +
    theme(legend.position = "none")

  rm(pred_sf, coords, sp_df, suitable_df, sp_contrib, thr, thr_row,
     pred_gpkg, pred_layer, sp_col, common, sp_tag)
}

# Richness map ----------------------------------------------------------------
richness_df$n_species <- rowSums(
  richness_df[, names(WRASSE_SPECIES), drop = FALSE],
  na.rm = TRUE
)

richness_df$n_species_f <- factor(richness_df$n_species, levels = 0:5)

richness_colours <- c("0" = "grey92",
                       "1" = "#c7e9b4",
                       "2" = "#7fcdbb",
                       "3" = "#41b6c4",
                       "4" = "#1d91c0",
                       "5" = "#0c2c84")

p_rich <- ggplot() +
  geom_sf(data = scottish_waters, fill = "grey92", colour = NA) +
  geom_tile(data = richness_df,
            aes(x = lon, y = lat, fill = n_species_f),
            width = 0.05, height = 0.05) +
  geom_sf(data = coastline, fill = "#ece8e1", colour = "grey50", linewidth = 0.2) +
  scale_fill_manual(values = richness_colours,
                    name   = "No. species",
                    drop   = FALSE) +
  coord_sf(xlim = xlim, ylim = ylim) +
  labs(title    = "Wrasse species richness",
       subtitle = "Number of species with suitable habitat (at MaxSSS threshold)") +
  theme_hab

# 6-panel combined figure -----------------------------------------------------
p_combined <- wrap_plots(c(species_panels, list(p_rich)), ncol = 3) +
  plot_annotation(
    title    = "Wrasse habitat suitability — Scottish inshore waters",
    subtitle = "Suitable habitat defined by MaxSSS (maximum sensitivity + specificity) threshold",
    theme    = theme(plot.title    = element_text(size = 13, face = "bold"),
                     plot.subtitle = element_text(size = 9, colour = "grey40"))
  )

ggsave("output/habitat_suitability.png", p_combined,
       width = 15, height = 12, dpi = 150)
cat("  Saved: output/habitat_suitability.png\n")

# Richness map alone at larger size -------------------------------------------
ggsave("output/habitat_richness.png", p_rich,
       width = 9, height = 10, dpi = 150)
cat("  Saved: output/habitat_richness.png\n")

rm(WRASSE_SPECIES, SP_COLOURS, thresholds_df, coastline, scottish_waters,
   scot_bb, xlim, ylim, theme_hab, richness_df, species_panels,
   richness_colours, p_rich, p_combined, sp)
