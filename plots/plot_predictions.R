# plots/plot_predictions.R
# Ensemble suitability predictions for each species
# Two-panel per species: ensemble probability + model uncertainty
# Saved to output/predictions_<species>.png

cat("Plotting species distribution predictions...\n")

WRASSE_SPECIES <- list(
  "Labrus_bergylta"       = "Ballan wrasse",
  "Labrus_mixtus"         = "Cuckoo wrasse",
  "Centrolabrus_exoletus" = "Rock cook",
  "Ctenolabrus_rupestris" = "Goldsinny wrasse",
  "Symphodus_melops"      = "Corkwing wrasse"
)

coastline       <- st_read(GEOPACKAGE_PATH, "Coastline",               quiet = TRUE)
scottish_waters <- st_read(GEOPACKAGE_PATH, "Scottish_Inshore_Waters", quiet = TRUE)
scot_bb         <- st_bbox(scottish_waters)

# Expand bounding box slightly for breathing room
xlim <- c(scot_bb["xmin"] - 0.3, scot_bb["xmax"] + 0.3)
ylim <- c(scot_bb["ymin"] - 0.3, scot_bb["ymax"] + 0.3)

theme_pred <- theme_minimal() +
  theme(axis.title        = element_blank(),
        axis.text         = element_text(size = 7),
        panel.grid        = element_line(colour = "grey88"),
        plot.title        = element_text(face = "bold.italic", size = 12),
        plot.subtitle     = element_text(size = 9),
        legend.position   = "right",
        legend.key.height = unit(1.2, "cm"),
        legend.title      = element_text(size = 9))

output_layers <- st_layers(OUTPUT_PATH)$name

for (sp in names(WRASSE_SPECIES)) {

  common    <- WRASSE_SPECIES[[sp]]
  sp_tag    <- tolower(sp)
  pred_layer <- paste0(sp_tag, "_predictions")
  pred_gpkg  <- file.path("model", paste0(sp_tag, "_predictions.gpkg"))

  if (!file.exists(pred_gpkg)) {
    cat(" ", common, "— prediction file not found, skipping\n")
    next
  }

  pred_sf <- st_read(pred_gpkg, pred_layer, quiet = TRUE)

  if (!"pred_ensemble" %in% names(pred_sf)) {
    cat(" ", common, "— pred_ensemble column not found, skipping\n")
    next
  }

  # Extract coordinates for geom_tile (regular grid, faster than geom_sf for points)
  coords         <- st_coordinates(pred_sf)
  pred_df        <- st_drop_geometry(pred_sf)
  pred_df$lon    <- coords[, 1]
  pred_df$lat    <- coords[, 2]
  pred_df_valid  <- pred_df[!is.na(pred_df$pred_ensemble), ]

  n_high <- sum(pred_df_valid$pred_ensemble > 0.5, na.rm = TRUE)
  cat(" ", common, "—", nrow(pred_df_valid), "grid points,",
      n_high, "with ensemble >0.5\n")

  # Load presence points for overlay (from OUTPUT_PATH if available, else GEOPACKAGE_PATH)
  pres_layer <- paste0(sp, "_records")
  pres_sf <- if (pres_layer %in% output_layers) {
    st_read(OUTPUT_PATH, pres_layer, quiet = TRUE)
  } else if (pres_layer %in% st_layers(GEOPACKAGE_PATH)$name) {
    st_read(GEOPACKAGE_PATH, pres_layer, quiet = TRUE)
  } else NULL

  # Panel 1: ensemble suitability ------------------------------------------
  p_ens <- ggplot() +
    geom_sf(data = scottish_waters, fill = "#e8f4f8", colour = NA) +
    geom_tile(data = pred_df_valid,
              aes(x = lon, y = lat, fill = pred_ensemble),
              width = 0.05, height = 0.05) +
    geom_sf(data = coastline, fill = "#ece8e1", colour = "grey50", linewidth = 0.2) +
    scale_fill_viridis_c(name = "Suitability",
                         option = "viridis", limits = c(0, 1),
                         labels = scales::label_number(accuracy = 0.1)) +
    coord_sf(xlim = xlim, ylim = ylim) +
    labs(title = common, subtitle = "Ensemble suitability") +
    theme_pred

  # Overlay presence points if available
  if (!is.null(pres_sf))
    p_ens <- p_ens +
      geom_sf(data = pres_sf, colour = "white", fill = "#e74c3c",
              shape = 21, size = 0.8, stroke = 0.3, alpha = 0.7)

  # Panel 2: model uncertainty (coefficient of variation) ------------------
  pred_df_unc <- pred_df[!is.na(pred_df$pred_uncertainty), ]

  p_unc <- ggplot() +
    geom_sf(data = scottish_waters, fill = "#e8f4f8", colour = NA) +
    geom_tile(data = pred_df_unc,
              aes(x = lon, y = lat, fill = pred_uncertainty),
              width = 0.05, height = 0.05) +
    geom_sf(data = coastline, fill = "#ece8e1", colour = "grey50", linewidth = 0.2) +
    scale_fill_viridis_c(name = "CV",
                         option = "magma", direction = -1,
                         labels = scales::label_number(accuracy = 0.01)) +
    coord_sf(xlim = xlim, ylim = ylim) +
    labs(title = common, subtitle = "Model uncertainty (CV between RF/GBM/GAM)") +
    theme_pred

  p <- p_ens + p_unc +
    plot_annotation(
      caption = paste0("Presences (n = ", if (!is.null(pres_sf)) nrow(pres_sf) else 0,
                       ")  shown as red points  |  Grid: 0.05°  |  Scottish inshore waters")
    )

  filename <- paste0("output/predictions_", tolower(gsub(" ", "_", common)), ".png")
  ggsave(filename, p, width = 14, height = 10, dpi = 150)
  cat("  Saved:", filename, "\n")

  rm(pred_sf, pred_df, pred_df_valid, pred_df_unc, coords, p_ens, p_unc, p,
     n_high, pres_layer, pred_layer, pred_gpkg, sp_tag, common, filename)
  if (exists("pres_sf")) rm(pres_sf)
}

cat("Done\n")
rm(WRASSE_SPECIES, coastline, scottish_waters, scot_bb, xlim, ylim,
   theme_pred, output_layers, sp)
