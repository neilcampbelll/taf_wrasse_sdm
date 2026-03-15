# model_ensemble.R
# Before: modelling_data.gpkg with presences and pseudo-absences per species
# After:  model/ directory with fitted models, CV performance, and spatial
#         predictions for each species

source("utilities.R")
source("models/model_rf.R")
source("models/model_gbm.R")
source("models/model_gam.R")

PREDICTORS <- c("depth_m", "sst", "sss", "dist_to_coast_km", "MSFD_BBHT")

WRASSE_SPECIES <- c("Labrus_bergylta", "Labrus_mixtus", "Centrolabrus_exoletus",
                    "Ctenolabrus_rupestris", "Symphodus_melops")


# SPATIAL CROSS-VALIDATION ====================================================

evaluate_spatial_cv <- function(spatial_blocks, model_sf) {

  k          <- spatial_blocks$k
  aucs_rf    <- numeric(k)
  aucs_gbm   <- numeric(k)
  aucs_gam   <- numeric(k)
  model_data <- st_drop_geometry(model_sf)

  cat("Performing spatial cross-validation (", k, "folds)...\n")

  for (i in seq_len(k)) {
    cat("  Fold", i, "of", k, "... ")

    train_idx <- spatial_blocks$folds[[i]][[1]]
    test_idx  <- spatial_blocks$folds[[i]][[2]]
    train_df  <- model_data[train_idx, ]
    test_df   <- model_data[test_idx,  ]

    # Align MSFD_BBHT factor levels so test fold only contains levels seen in
    # training — prevents GAM prediction length mismatch on rare habitat folds
    train_df$MSFD_BBHT <- factor(train_df$MSFD_BBHT)
    test_df$MSFD_BBHT  <- factor(test_df$MSFD_BBHT,
                                  levels = levels(train_df$MSFD_BBHT))

    sufficient <- nrow(train_df) >= 20 &&
                  nrow(test_df)  >= 10 &&
                  length(unique(train_df$presence)) == 2 &&
                  sum(train_df$presence == 1) >= 5 &&
                  sum(train_df$presence == 0) >= 5 &&
                  length(unique(test_df$presence))  == 2

    if (!sufficient) {
      cat("skipped (insufficient data)\n")
      aucs_rf[i] <- aucs_gbm[i] <- aucs_gam[i] <- NA
      next
    }

    resp <- as.numeric(as.character(test_df$presence))

    # Fit each model independently so one failure doesn't discard the others
    aucs_rf[i] <- tryCatch({
      rf_cv  <- fit_rf(train_df)
      p_rf   <- predict(rf_cv, newdata = test_df, type = "prob")[, 2]
      as.numeric(pROC::auc(pROC::roc(resp, p_rf, quiet = TRUE)))
    }, error = function(e) { cat("RF error:", e$message, "\n"); NA_real_ })

    aucs_gbm[i] <- tryCatch({
      gbm_cv <- fit_gbm(train_df)
      p_gbm  <- predict(gbm_cv$model, newdata = test_df,
                        n.trees = gbm_cv$best_iter, type = "response")
      as.numeric(pROC::auc(pROC::roc(resp, p_gbm, quiet = TRUE)))
    }, error = function(e) { cat("GBM error:", e$message, "\n"); NA_real_ })

    aucs_gam[i] <- tryCatch({
      gam_cv <- fit_gam(train_df)
      p_gam  <- predict(gam_cv, newdata = test_df, type = "response")
      as.numeric(pROC::auc(pROC::roc(resp, p_gam, quiet = TRUE)))
    }, error = function(e) { cat("GAM error:", e$message, "\n"); NA_real_ })

    cat("AUCs (RF/GBM/GAM):", round(aucs_rf[i], 3),
        round(aucs_gbm[i], 3), round(aucs_gam[i], 3), "\n")
  }

  data.frame(
    Model       = c("Random Forest", "GBM", "GAM"),
    Mean_AUC    = c(mean(aucs_rf,  na.rm = TRUE),
                    mean(aucs_gbm, na.rm = TRUE),
                    mean(aucs_gam, na.rm = TRUE)),
    SE_AUC      = c(sd(aucs_rf,  na.rm = TRUE) / sqrt(sum(!is.na(aucs_rf))),
                    sd(aucs_gbm, na.rm = TRUE) / sqrt(sum(!is.na(aucs_gbm))),
                    sd(aucs_gam, na.rm = TRUE) / sqrt(sum(!is.na(aucs_gam)))),
    Valid_Folds = c(sum(!is.na(aucs_rf)),
                    sum(!is.na(aucs_gbm)),
                    sum(!is.na(aucs_gam)))
  )
}


# SPATIAL PREDICTIONS =========================================================

make_spatial_predictions <- function(rf_model, gbm_result, gam_model,
                                     grid_sf, ensemble_weights) {

  cat("Making spatial predictions on", nrow(grid_sf), "grid points...\n")

  grid_df      <- st_drop_geometry(grid_sf)
  complete_idx <- complete.cases(grid_df[, intersect(PREDICTORS, names(grid_df))])
  pred_df      <- grid_df[complete_idx, ]

  # Initialise prediction columns with NA
  grid_sf$pred_rf          <- NA_real_
  grid_sf$pred_gbm         <- NA_real_
  grid_sf$pred_gam         <- NA_real_
  grid_sf$pred_ensemble    <- NA_real_
  grid_sf$pred_uncertainty <- NA_real_

  if (nrow(pred_df) == 0) {
    cat("No complete grid points for prediction.\n")
    return(grid_sf)
  }

  pred_rf  <- predict(rf_model,        pred_df, type = "prob")[, 2]
  pred_gbm <- predict(gbm_result$model, pred_df,
                      n.trees = gbm_result$best_iter, type = "response")
  pred_gam <- predict(gam_model, pred_df, type = "response")

  ensemble <- ensemble_weights["rf"]  * pred_rf  +
              ensemble_weights["gbm"] * pred_gbm +
              ensemble_weights["gam"] * pred_gam

  pred_mat    <- cbind(pred_rf, pred_gbm, pred_gam)
  uncertainty <- apply(pred_mat, 1, function(x) {
    m <- mean(x, na.rm = TRUE)
    if (is.na(m) || m == 0) NA_real_ else sd(x, na.rm = TRUE) / m
  })

  grid_sf$pred_rf[complete_idx]          <- pred_rf
  grid_sf$pred_gbm[complete_idx]         <- pred_gbm
  grid_sf$pred_gam[complete_idx]         <- pred_gam
  grid_sf$pred_ensemble[complete_idx]    <- ensemble
  grid_sf$pred_uncertainty[complete_idx] <- uncertainty

  cat("Predictions complete. Ensemble range: [",
      round(min(ensemble, na.rm = TRUE), 3), ",",
      round(max(ensemble, na.rm = TRUE), 3), "]\n")
  grid_sf
}


# VALIDATION ==================================================================

validate_predictions <- function(pred_sf, presence_df) {
  cat("=== PREDICTION VALIDATION ===\n")

  if (nrow(pred_sf) == 0 || all(is.na(pred_sf$pred_ensemble))) {
    cat("No valid predictions to validate.\n")
    return(invisible(NULL))
  }

  ens <- pred_sf$pred_ensemble
  cat("Proportion >0.7:", round(mean(ens > 0.7, na.rm = TRUE) * 100, 2), "%\n")

  if ("dist_to_coast_km" %in% names(pred_sf)) {
    max_dist_high <- if (any(ens > 0.5, na.rm = TRUE))
      max(pred_sf$dist_to_coast_km[ens > 0.5], na.rm = TRUE) else NA
    max_obs_dist <- max(presence_df$dist_to_coast_km, na.rm = TRUE)
    cat("Max distance (>0.5 prob):", round(max_dist_high, 2), "km |",
        "Max observed:", round(max_obs_dist, 2), "km\n")
    if (!is.na(max_dist_high) && max_dist_high > max_obs_dist * 1.5)
      cat("Warning: High suitability predicted beyond observed distance range.\n")
  }

  if ("depth_m" %in% names(pred_sf)) {
    deep_mean <- if (any(pred_sf$depth_m < -100, na.rm = TRUE))
      mean(ens[pred_sf$depth_m < -100], na.rm = TRUE) else NA
    cat("Mean prediction in water >100m deep:", round(deep_mean, 3), "\n")
    if (!is.na(deep_mean) && deep_mean > 0.3)
      cat("Warning: High predictions in deep water — check depth influence.\n")
  }

  cat("High model disagreement (CV > 0.1):",
      round(mean(pred_sf$pred_uncertainty > 0.1, na.rm = TRUE) * 100, 2), "%\n")
}


# MAIN WORKFLOW ===============================================================

run_sdm <- function(species_name) {
  cat("\n=== SDM:", gsub("_", " ", species_name), "===\n\n")

  # 1. Load presence and pseudo-absence data ----------------------------------
  cat("1. Loading modelling data...\n")

  pres_layer <- paste0(species_name, "_records")
  abs_layer  <- paste0(species_name, "_pseudo_absences_extended")

  output_layers <- st_layers(OUTPUT_PATH)$name
  if (!pres_layer %in% output_layers) stop("Presence layer not found: ", pres_layer)
  if (!abs_layer  %in% output_layers) stop("Absence layer not found: ",  abs_layer)

  pres_sf        <- st_read(OUTPUT_PATH, pres_layer, quiet = TRUE)
  abs_sf         <- st_read(OUTPUT_PATH, abs_layer,  quiet = TRUE)
  abs_sf$presence <- 0L

  # Subset to required columns (geometry retained by sf)
  keep   <- c(PREDICTORS, "presence")
  pres_sf <- pres_sf[, intersect(keep, names(pres_sf))]
  abs_sf  <- abs_sf[,  intersect(keep, names(abs_sf))]

  model_sf           <- rbind(pres_sf, abs_sf)
  model_sf$MSFD_BBHT <- as.factor(model_sf$MSFD_BBHT)

  cat("  Presences:", sum(model_sf$presence == 1),
      "| Pseudo-absences:", sum(model_sf$presence == 0), "\n")

  if (length(unique(model_sf$presence)) < 2)
    stop("Need both presences and absences for modelling.")

  # 2. Spatial cross-validation -----------------------------------------------
  cat("\n2. Spatial cross-validation...\n")
  set.seed(123)
  sb <- tryCatch({
    spatialBlock(
      speciesData = model_sf,
      species     = "presence",
      theRange    = 70000,
      k           = 5,
      selection   = "random",
      iteration   = 100,
      showBlocks  = FALSE,
      biomod2     = FALSE
    )
  }, error = function(e) {
    cat("spatialBlock error:", e$message, "\n")
    NULL
  })
  if (is.null(sb)) stop("Spatial block CV setup failed.")

  cv_results <- evaluate_spatial_cv(sb, model_sf)
  print(cv_results)

  # 3. AUC-weighted ensemble weights ------------------------------------------
  weights        <- setNames(cv_results$Mean_AUC, c("rf", "gbm", "gam"))
  weights[is.na(weights) | !is.finite(weights)] <- 0
  total_w        <- sum(weights)
  if (total_w == 0) {
    weights[] <- 1 / 3
    cat("All CV AUCs invalid — using equal weights.\n")
  } else {
    weights <- weights / total_w
  }
  cat("Ensemble weights (RF/GBM/GAM):", paste(round(weights, 3), collapse = "/"), "\n")

  # 4. Fit final models on full dataset ---------------------------------------
  cat("\n3. Fitting final models on full dataset...\n")
  model_df  <- st_drop_geometry(model_sf)
  rf_final  <- fit_rf(model_df)
  gbm_final <- fit_gbm(model_df)
  gam_final <- fit_gam(model_df)

  # 5. Spatial predictions ----------------------------------------------------
  cat("\n4. Loading prediction grid...\n")
  grid_sf           <- st_read(OUTPUT_PATH, "environmental_grid", quiet = TRUE)
  grid_sf$MSFD_BBHT <- factor(grid_sf$MSFD_BBHT, levels = levels(model_sf$MSFD_BBHT))

  cat("\n5. Predicting over grid...\n")
  pred_sf <- make_spatial_predictions(rf_final, gbm_final, gam_final, grid_sf, weights)

  # 6. Validation summary -----------------------------------------------------
  cat("\n6. Validation...\n")
  validate_predictions(pred_sf, st_drop_geometry(pres_sf))

  # 7. Save outputs -----------------------------------------------------------
  cat("\n7. Saving results...\n")
  mkdir("model")
  sp_tag <- tolower(species_name)

  save(rf_final, gbm_final, gam_final, weights,
       file = file.path("model", paste0(sp_tag, "_models.RData")))

  st_write(pred_sf,
           file.path("model", paste0(sp_tag, "_predictions.gpkg")),
           layer        = paste0(sp_tag, "_predictions"),
           delete_layer = TRUE,
           quiet        = TRUE)

  write.csv(cv_results,
            file.path("model", paste0(sp_tag, "_cv_performance.csv")),
            row.names = FALSE)

  cat("Results saved to model/", sp_tag, "_*\n\n", sep = "")

  invisible(list(
    models           = list(rf = rf_final, gbm = gbm_final, gam = gam_final),
    ensemble_weights = weights,
    predictions_sf   = pred_sf,
    cv_performance   = cv_results
  ))
}


# RUN ALL SPECIES =============================================================

for (sp in WRASSE_SPECIES) {
  tryCatch(
    run_sdm(sp),
    error = function(e) cat("Error for", sp, ":", e$message, "\n\n")
  )
}
 