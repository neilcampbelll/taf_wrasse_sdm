# plots/plot_study_area.R
# Overview map: study area extent, coastlines, and Scottish inshore waters

cat("Plotting study area...\n")

study_area      <- st_read(GEOPACKAGE_PATH, "Study_Area",              quiet = TRUE)
coastline       <- st_read(GEOPACKAGE_PATH, "Coastline",               quiet = TRUE)
scottish_waters <- st_read(GEOPACKAGE_PATH, "Scottish_Inshore_Waters", quiet = TRUE)

# Inset bounding box for the Scottish waters zoom panel
scot_bb <- st_bbox(scottish_waters)

theme_map <- theme_minimal() +
  theme(axis.title       = element_blank(),
        panel.grid       = element_line(colour = "grey88"),
        legend.position  = "right",
        plot.title       = element_text(face = "bold"))

# Full study area
p_full <- ggplot() +
  geom_sf(data = study_area,      fill = "#d6eaf8", colour = NA) +
  geom_sf(data = coastline,       fill = "#ece8e1", colour = "grey60", linewidth = 0.2) +
  geom_sf(data = scottish_waters, fill = NA,        colour = "#2471a3", linewidth = 0.5) +
  labs(title = "Study area", subtitle = "NE Atlantic extent") +
  theme_map

# Zoomed view of Scottish waters
p_zoom <- ggplot() +
  geom_sf(data = coastline,       fill = "#ece8e1", colour = "grey60", linewidth = 0.3) +
  geom_sf(data = scottish_waters, fill = "#aed6f1", colour = "#2471a3", linewidth = 0.5) +
  coord_sf(xlim = c(scot_bb["xmin"] - 0.5, scot_bb["xmax"] + 0.5),
           ylim = c(scot_bb["ymin"] - 0.5, scot_bb["ymax"] + 0.5)) +
  labs(title = "Scottish inshore waters", subtitle = "Model extent") +
  theme_map

p <- p_full + p_zoom +
  plot_annotation(title    = "Wrasse SDM — Study Area",
                  subtitle = "Blue outline = Scottish Marine Regions (model extent)")

ggsave("output/study_area.png", p, width = 14, height = 7, dpi = 150)
cat("Saved: output/study_area.png\n")

rm(study_area, coastline, scottish_waters, scot_bb, theme_map, p_full, p_zoom, p)
