# plots/plot_species_occurrences.R
# Map of OBIS occurrence records for all five wrasse species

cat("Plotting species occurrences...\n")

WRASSE_SPECIES <- list(
  "Labrus_bergylta"      = "Ballan wrasse",
  "Labrus_mixtus"        = "Cuckoo wrasse",
  "Centrolabrus_exoletus"= "Rock cook",
  "Ctenolabrus_rupestris"= "Goldsinny wrasse",
  "Symphodus_melops"     = "Corkwing wrasse"
)

SPECIES_COLOURS <- c(
  "Ballan wrasse"   = "#e74c3c",
  "Cuckoo wrasse"   = "#3498db",
  "Rock cook"       = "#2ecc71",
  "Goldsinny wrasse"= "#f39c12",
  "Corkwing wrasse" = "#9b59b6"
)

coastline <- st_read(GEOPACKAGE_PATH, "Coastline", quiet = TRUE)

# Load all species records, label with common name, and combine
records_list <- lapply(names(WRASSE_SPECIES), function(sp) {
  layer <- paste0(sp, "_records")
  layers_available <- st_layers(GEOPACKAGE_PATH)$name
  if (!layer %in% layers_available) {
    cat("  Layer not found, skipping:", layer, "\n")
    return(NULL)
  }
  dat <- st_read(GEOPACKAGE_PATH, layer, quiet = TRUE)
  dat$common_name <- WRASSE_SPECIES[[sp]]
  dat[, c("common_name", attr(dat, "sf_column"))]
})

records <- do.call(rbind, Filter(Negate(is.null), records_list))
records$common_name <- factor(records$common_name, levels = names(SPECIES_COLOURS))

cat("  Total records:", nrow(records), "\n")

study_area <- st_read(GEOPACKAGE_PATH, "Study_Area", quiet = TRUE)
bb <- st_bbox(study_area)

p <- ggplot() +
  geom_sf(data = study_area, fill = "#d6eaf8", colour = NA) +
  geom_sf(data = coastline,  fill = "#ece8e1", colour = "grey60", linewidth = 0.2) +
  geom_sf(data = records, aes(colour = common_name), size = 0.8, alpha = 0.6) +
  scale_colour_manual(values = SPECIES_COLOURS, name = NULL) +
  coord_sf(xlim = c(bb["xmin"], bb["xmax"]), ylim = c(bb["ymin"], bb["ymax"])) +
  labs(title    = "Wrasse SDM — OBIS Occurrence Records",
       subtitle  = paste0("n = ", nrow(records), " records across ", length(WRASSE_SPECIES), " species")) +
  guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1))) +
  theme_minimal() +
  theme(axis.title      = element_blank(),
        panel.grid      = element_line(colour = "grey88"),
        legend.position = "right",
        plot.title      = element_text(face = "bold"))

ggsave("output/species_occurrences.png", p, width = 12, height = 9, dpi = 150)
cat("Saved: output/species_occurrences.png\n")

rm(WRASSE_SPECIES, SPECIES_COLOURS, coastline, records_list, records,
   study_area, bb, p)
