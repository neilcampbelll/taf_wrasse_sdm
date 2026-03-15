# model_thresholds.R
# Before: fitted models in model/<species>_models.RData and prediction grids
#         in model/<species>_predictions.gpkg
# After:  model/habitat_thresholds.csv  — MaxSSS threshold per species
#         model/habitat_area.csv        — suitable area (km²) per species × threshold

source("utilities.R")

WRASSE_SPECIES <- list(
  "Labrus_bergylta"       = "Ballan wrasse",
  "Labrus_mixtus"         = "Cuckoo wrasse",
  "Centrolabrus_exoletus" = "Rock cook",
  "Ctenolabrus_rupestris" = "Goldsinny wrasse",
  "Symphodus_melops"      = "Corkwing wrasse"
)

# Fixed thresholds to report alongside MaxSSS
FIXED_THRESHOLDS <- c(0.3, 0.5, 0.7)

# Cell area in km² for a 0.05° grid cell at a given latitude
cell_area_km2 <- function(lat, cell_deg = 0.05) {
  cell_deg * 111.32 * cell_deg * 111.32 * cos(lat * pi / 180)
}

threshold_rows <- list()
area_rows      <- list()

for (sp in names(WRASSE_SPECIES)) {

  common <- WRASSE_SPECIES[[sp]]
  sp_tag <- tolower(sp)
  cat("\n---", common, "---\n")

  # 1. Load final models ---------------------------------------------------
  model_file <- file.path("model", paste0(sp_tag, "_models.RData"))
  if (!file.exists(model_file)) {
    cat("  Model file not found, skipping\n")
    next
  }
  load(model_file)   # loads rf_final, gbm_final, gam_final, weights

  # 2. Load modelling data (presences + pseudo-absences) -------------------
  pres_layer <- paste0(sp, "_records")
  abs_layer  <- paste0(sp, "_pseudo_absences_extended")

  pres_sf        <- st_read(OUTPUT_PATH, pres_layer, quiet = TRUE)
  abs_sf         <- st_read(OUTPUT_PATH, abs_layer,  quiet = TRUE)
  abs_sf$presence <- 0L

  keep     <- c("depth_m", "sst", "sss", "dist_to_coast_km", "MSFD_BBHT", "presence")
  pres_sf  <- pres_sf[, intersect(keep, names(pres_sf))]
  abs_sf   <- abs_sf[,  intersect(keep, names(abs_sf))]
  model_df <- st_drop_geometry(rbind(pres_sf, abs_sf))
  model_df$MSFD_BBHT <- as.factor(model_df$MSFD_BBHT)

  # 3. In-sample ensemble predictions for threshold derivation -------------
  # Using full training data is standard for MaxSSS in SDMs; the CV AUCs
  # already provide an unbiased performance estimate.
  cat("  Computing in-sample ensemble predictions...\n")

  p_rf  <- predict(rf_final,        model_df, type = "prob")[, 2]
  p_gbm <- predict(gbm_final$model, model_df,
                   n.trees = gbm_final$best_iter, type = "response")
  p_gam <- predict(gam_final, model_df, type = "response")

  p_ens <- weights["rf"]  * p_rf  +
           weights["gbm"] * p_gbm +
           weights["gam"] * p_gam

  response <- as.integer(model_df$presence)

  # 4. MaxSSS via Youden's J (equivalent to max sensitivity + specificity) -
  roc_obj  <- pROC::roc(response, p_ens, quiet = TRUE)
  best     <- pROC::coords(roc_obj, "best", best.method = "youden",
                            ret = c("threshold", "sensitivity", "specificity"))

  # coords() can return multiple rows if several thresholds tie — take first
  if (nrow(best) > 1) best <- best[1, ]

  maxsss_threshold <- best$threshold
  sensitivity      <- best$sensitivity
  specificity      <- best$specificity

  cat("  MaxSSS threshold:", round(maxsss_threshold, 3),
      "| Sensitivity:", round(sensitivity, 3),
      "| Specificity:", round(specificity, 3), "\n")

  threshold_rows[[sp]] <- data.frame(
    species           = common,
    maxsss_threshold  = round(maxsss_threshold, 3),
    sensitivity       = round(sensitivity, 3),
    specificity       = round(specificity, 3),
    auc               = round(as.numeric(pROC::auc(roc_obj)), 3)
  )

  # 5. Suitable area from prediction grid ----------------------------------
  pred_gpkg  <- file.path("model", paste0(sp_tag, "_predictions.gpkg"))
  pred_layer <- paste0(sp_tag, "_predictions")

  if (!file.exists(pred_gpkg)) {
    cat("  Prediction grid not found, skipping area calculation\n")
    next
  }

  pred_sf  <- st_read(pred_gpkg, pred_layer, quiet = TRUE)
  pred_sf  <- pred_sf[!is.na(pred_sf$pred_ensemble), ]
  coords   <- st_coordinates(pred_sf)
  ens_vals <- pred_sf$pred_ensemble
  lats     <- coords[, 2]

  # Area of each cell at its latitude
  areas_km2 <- cell_area_km2(lats)
  total_area <- sum(areas_km2)
  cat("  Total grid area:", round(total_area, 0), "km²\n")

  # Area above each threshold
  all_thresholds <- sort(unique(c(maxsss_threshold, FIXED_THRESHOLDS)))

  for (thr in all_thresholds) {
    suitable      <- ens_vals >= thr
    suitable_area <- sum(areas_km2[suitable])
    pct           <- 100 * suitable_area / total_area

    thr_label <- if (abs(thr - maxsss_threshold) < 1e-6) {
      paste0("MaxSSS (", round(thr, 3), ")")
    } else {
      as.character(round(thr, 2))
    }

    cat("  Threshold", thr_label, "—", round(suitable_area, 0),
        "km² (", round(pct, 1), "% of grid)\n")

    area_rows[[paste(sp, thr)]] <- data.frame(
      species        = common,
      threshold_type = if (abs(thr - maxsss_threshold) < 1e-6) "MaxSSS" else "fixed",
      threshold      = round(thr, 3),
      suitable_km2   = round(suitable_area, 1),
      total_km2      = round(total_area, 1),
      pct_suitable   = round(pct, 1)
    )
  }

  rm(rf_final, gbm_final, gam_final, weights,
     pres_sf, abs_sf, model_df, p_rf, p_gbm, p_gam, p_ens,
     response, roc_obj, best, pred_sf, coords, ens_vals, lats, areas_km2)
}

# 6. Save results ------------------------------------------------------------
mkdir("model")

thresholds_df <- do.call(rbind, threshold_rows)
area_df       <- do.call(rbind, area_rows)
rownames(thresholds_df) <- NULL
rownames(area_df)       <- NULL

write.csv(thresholds_df, "model/habitat_thresholds.csv", row.names = FALSE)
write.csv(area_df,       "model/habitat_area.csv",       row.names = FALSE)

cat("\n=== THRESHOLD SUMMARY ===\n")
print(thresholds_df)

cat("\n=== SUITABLE AREA SUMMARY ===\n")
print(area_df)

cat("\nSaved: model/habitat_thresholds.csv\n")
cat("Saved: model/habitat_area.csv\n")

rm(WRASSE_SPECIES, FIXED_THRESHOLDS, sp, common, sp_tag, model_file,
   pres_layer, abs_layer, keep, maxsss_threshold, sensitivity, specificity,
   pred_gpkg, pred_layer, total_area, all_thresholds, thr, thr_label,
   suitable, suitable_area, pct, threshold_rows, area_rows,
   thresholds_df, area_df)
