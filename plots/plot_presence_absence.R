# plots/plot_presence_absence.R
# One map per species showing presences and pseudo-absences
# Saved to output/presence_absence_<species>.png

cat("Plotting presences and pseudo-absences...\n")

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
output_layers   <- st_layers(OUTPUT_PATH)$name

theme_sp <- theme_minimal() +
  theme(axis.title      = element_blank(),
        panel.grid      = element_line(colour = "grey88"),
        plot.title      = element_text(face = "bold.italic", size = 13),
        plot.subtitle   = element_text(size = 10),
        legend.position = "bottom",
        legend.title    = element_blank())

for (sp in names(WRASSE_SPECIES)) {

  common     <- WRASSE_SPECIES[[sp]]
  pres_layer <- paste0(sp, "_records")
  abs_layer  <- paste0(sp, "_pseudo_absences_extended")

  # Prefer processed records from OUTPUT_PATH; fall back to raw OBIS records
  # in GEOPACKAGE_PATH if not yet available there
  gp_layers <- st_layers(GEOPACKAGE_PATH)$name
  pres <- if (pres_layer %in% output_layers) {
    dat <- st_read(OUTPUT_PATH, pres_layer, quiet = TRUE)
    dat[, attr(dat, "sf_column")]
  } else if (pres_layer %in% gp_layers) {
    dat <- st_read(GEOPACKAGE_PATH, pres_layer, quiet = TRUE)
    dat[, attr(dat, "sf_column")]
  } else NULL

  abs <- if (abs_layer %in% output_layers) {
    dat <- st_read(OUTPUT_PATH, abs_layer, quiet = TRUE)
    dat[, attr(dat, "sf_column")]
  } else NULL

  if (is.null(pres) && is.null(abs)) {
    cat("  No data found for", common, "— skipping\n")
    next
  }

  n_pres <- if (!is.null(pres)) nrow(pres) else 0
  n_abs  <- if (!is.null(abs))  nrow(abs)  else 0
  cat(" ", common, "—", n_pres, "presences,", n_abs, "pseudo-absences\n")

  p <- ggplot() +
    geom_sf(data = scottish_waters, fill = "#d6eaf8", colour = NA) +
    geom_sf(data = coastline, fill = "#ece8e1", colour = "grey60", linewidth = 0.2)

  if (!is.null(abs))
    p <- p + geom_sf(data = abs,  aes(colour = "Pseudo-absence"),
                     size = 0.5, alpha = 0.4)

  if (!is.null(pres))
    p <- p + geom_sf(data = pres, aes(colour = "Presence"),
                     size = 1.2, alpha = 0.8)

  p <- p +
    coord_sf(xlim = c(scot_bb["xmin"] - 0.5, scot_bb["xmax"] + 0.5),
             ylim = c(scot_bb["ymin"] - 0.5, scot_bb["ymax"] + 0.5)) +
    scale_colour_manual(values = c("Presence" = "#e74c3c", "Pseudo-absence" = "grey55"),
                        name = NULL) +
    labs(title    = common,
         subtitle = paste0("n = ", n_pres, " presences  |  ",
                           n_abs, " pseudo-absences")) +
    theme_sp

  filename <- paste0("output/presence_absence_", tolower(gsub(" ", "_", common)), ".png")
  ggsave(filename, p, width = 9, height = 10, dpi = 150)
  cat("  Saved:", filename, "\n")

  rm(pres, abs, n_pres, n_abs, p, filename, pres_layer, abs_layer, common)
}

cat("Done\n")

rm(WRASSE_SPECIES, coastline, scottish_waters, scot_bb, output_layers, theme_sp, sp)
