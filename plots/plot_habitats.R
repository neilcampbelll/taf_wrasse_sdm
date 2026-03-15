# plots/plot_habitats.R
# Maps and summary of MSFD broad benthic habitat types
# Panel 1: habitat polygons across full study extent
# Panel 2: habitat assigned to Scottish waters model grid
# Panel 3: bar chart of habitat composition in the model grid

cat("Plotting habitat data...\n")

gp_layers     <- st_layers(GEOPACKAGE_PATH)$name
output_layers <- st_layers(OUTPUT_PATH)$name

has_habitat_layer <- "MSFD_Habitats"    %in% gp_layers
has_grid_habitat  <- "environmental_grid" %in% output_layers

if (!has_habitat_layer && !has_grid_habitat) {
  cat("  No habitat data found — skipping. Run data_habitats.R first.\n")
  rm(gp_layers, output_layers, has_habitat_layer, has_grid_habitat)
  return(invisible(NULL))
}

coastline <- st_read(GEOPACKAGE_PATH, "Coastline", quiet = TRUE)

theme_map <- theme_minimal() +
  theme(axis.title      = element_blank(),
        panel.grid      = element_line(colour = "grey88"),
        legend.position = "right",
        plot.title      = element_text(size = 10, face = "bold"))

plots <- list()

# Panel 1: full habitat polygon layer
if (has_habitat_layer) {
  cat("  Loading MSFD_Habitats layer...\n")
  habitats <- st_read(GEOPACKAGE_PATH, "MSFD_Habitats", quiet = TRUE)

  hab_col <- intersect(c("MSFD_BBHT", "HabType", "habitat"), names(habitats))[1]
  if (is.na(hab_col)) {
    cat("  Warning: could not identify habitat column. Columns:", paste(names(habitats), collapse = ", "), "\n")
  } else {
    cat("  Using habitat column:", hab_col, "\n")
    n_types <- length(unique(habitats[[hab_col]]))

    plots[["full"]] <- ggplot() +
      geom_sf(data = habitats,  aes(fill = .data[[hab_col]]), colour = NA) +
      geom_sf(data = coastline, fill = "#ece8e1", colour = "grey60", linewidth = 0.2) +
      scale_fill_viridis_d(name = "Habitat type", option = "turbo", na.value = "grey80") +
      labs(title    = "MSFD Benthic Habitats",
           subtitle = paste(n_types, "habitat types")) +
      guides(fill = guide_legend(ncol = 1, keyheight = unit(0.5, "cm"))) +
      theme_map

    rm(habitats, hab_col, n_types)
  }
}

# Panel 2: habitat at model grid points; Panel 3: bar chart
if (has_grid_habitat) {
  grid <- st_read(OUTPUT_PATH, "environmental_grid", quiet = TRUE)

  if ("MSFD_BBHT" %in% names(grid) && any(!is.na(grid$MSFD_BBHT))) {

    scottish_waters <- st_read(GEOPACKAGE_PATH, "Scottish_Inshore_Waters", quiet = TRUE)
    scot_bb         <- st_bbox(scottish_waters)

    plots[["grid"]] <- ggplot() +
      geom_sf(data = scottish_waters, fill = "#d6eaf8", colour = NA) +
      geom_sf(data = grid, aes(colour = MSFD_BBHT), size = 0.4, shape = 15) +
      geom_sf(data = coastline, fill = "#ece8e1", colour = "grey60", linewidth = 0.2) +
      coord_sf(xlim = c(scot_bb["xmin"] - 0.5, scot_bb["xmax"] + 0.5),
               ylim = c(scot_bb["ymin"] - 0.5, scot_bb["ymax"] + 0.5)) +
      scale_colour_viridis_d(name = "Habitat type", option = "turbo", na.value = "grey80") +
      labs(title    = "Habitat at model grid points",
           subtitle = "Scottish inshore waters") +
      guides(colour = guide_legend(ncol = 1, keyheight = unit(0.5, "cm"),
                                   override.aes = list(size = 3))) +
      theme_map

    hab_counts <- as.data.frame(table(grid$MSFD_BBHT, useNA = "no"))
    names(hab_counts) <- c("habitat", "count")
    hab_counts <- hab_counts[order(-hab_counts$count), ]
    hab_counts$habitat <- factor(hab_counts$habitat, levels = hab_counts$habitat)

    plots[["bar"]] <- ggplot(hab_counts, aes(x = count, y = habitat, fill = habitat)) +
      geom_col(show.legend = FALSE) +
      scale_fill_viridis_d(option = "turbo") +
      scale_x_continuous(labels = scales::comma) +
      labs(title = "Habitat composition",
           subtitle = "Grid points in Scottish waters",
           x = "Number of grid points", y = NULL) +
      theme_minimal() +
      theme(plot.title  = element_text(size = 10, face = "bold"),
            axis.text.y = element_text(size = 8))

    rm(scottish_waters, scot_bb, hab_counts)
  } else {
    cat("  MSFD_BBHT not found in environmental_grid — grid panel skipped.\n")
  }

  rm(grid)
}

# Assemble and save
if (length(plots) == 0) {
  cat("  No habitat plots produced.\n")
} else if (length(plots) == 1) {
  p <- plots[[1]]
  ggsave("output/habitats.png", p, width = 12, height = 9, dpi = 150)
  cat("Saved: output/habitats.png\n")
} else if (length(plots) == 2) {
  p <- wrap_plots(plots) +
    plot_annotation(title = "Wrasse SDM — Benthic Habitat (MSFD)")
  ggsave("output/habitats.png", p, width = 16, height = 9, dpi = 150)
  cat("Saved: output/habitats.png\n")
} else {
  p <- (plots[["full"]] | plots[["grid"]]) / plots[["bar"]] +
    plot_layout(heights = c(2, 1)) +
    plot_annotation(title = "Wrasse SDM — Benthic Habitat (MSFD)")
  ggsave("output/habitats.png", p, width = 16, height = 14, dpi = 150)
  cat("Saved: output/habitats.png\n")
}

rm(gp_layers, output_layers, has_habitat_layer, has_grid_habitat,
   coastline, theme_map, plots, p)
