# ============================================================================
# PERFORMANCE ENHANCEMENT UTILITIES
# Parallel processing, memory optimization, and batch processing improvements
# ============================================================================

# PARALLEL PROCESSING SETUP ==================================================

#' Initialize parallel processing cluster
#' @param n_cores Number of cores to use (NULL for auto-detect)
#' @param method Method: "doParallel", "future", or "none"
#' @return Cluster object or NULL
setup_parallel_processing <- function(n_cores = NULL, method = "doParallel") {

  if (method == "none" || !get_config(c("performance", "use_parallel"), TRUE)) {
    cat("✓ Parallel processing disabled\n")
    return(NULL)
  }

  # Auto-detect cores if not specified
  if (is.null(n_cores)) {
    n_cores <- get_config(c("performance", "n_cores"), NULL)
    if (is.null(n_cores)) {
      n_cores <- max(1, detectCores() - 1)  # Leave one core free
    }
  }

  # Ensure reasonable limits
  n_cores <- max(1, min(n_cores, detectCores()))

  cat("Setting up parallel processing with", n_cores, "cores using", method, "...\n")

  if (method == "doParallel") {
    if (.Platform$OS.type == "windows") {
      cl <- makeCluster(n_cores)
    } else {
      cl <- makeCluster(n_cores, type = "FORK")
    }

    registerDoParallel(cl)
    cat("✓ doParallel cluster registered with", n_cores, "cores\n")
    return(cl)

  } else if (method == "future") {
    plan(multisession, workers = n_cores)
    cat("✓ Future multisession plan with", n_cores, "workers\n")
    return(NULL)

  } else {
    stop("Unknown parallel method: ", method)
  }
}

#' Cleanup parallel processing cluster
#' @param cluster Cluster object from setup_parallel_processing()
cleanup_parallel_processing <- function(cluster = NULL) {
  if (!is.null(cluster)) {
    stopCluster(cluster)
    cat("✓ Parallel cluster stopped\n")
  }

  # Reset to sequential processing
  if (exists("plan")) {
    plan(sequential)
  }
}

# PARALLEL CROSS-VALIDATION ==================================================

#' Parallel spatial cross-validation for ensemble models
#' @param model_data Training data with coordinates and response
#' @param spatial_blocks Spatial blocks from create_spatial_blocks()
#' @param models_to_fit Vector of models to include c("rf", "gbm", "gam", "inla")
#' @param cluster Parallel cluster (optional)
#' @return List with AUC results and summary statistics
evaluate_models_cv_parallel <- function(model_data, spatial_blocks,
                                       models_to_fit = c("rf", "gbm", "gam", "inla"),
                                       cluster = NULL) {

  cat("5. Evaluating models with PARALLEL spatial cross-validation...\n")
  cat("   Models to fit:", paste(models_to_fit, collapse = ", "), "\n")

  n_folds <- spatial_blocks$k

  # Setup parallel processing if not provided
  local_cluster <- FALSE
  if (is.null(cluster) && get_config(c("performance", "use_parallel"), TRUE)) {
    cluster <- setup_parallel_processing()
    local_cluster <- TRUE
  }

  # Prepare fold data
  fold_data <- prepare_cv_fold_data(model_data, spatial_blocks)

  tryCatch({

    if (!is.null(cluster)) {
      # Export necessary objects to cluster
      clusterExport(cluster, c("fit_rf_model", "fit_gbm_model", "fit_gam_model",
                              "fit_inla_model_adaptive"),
                   envir = environment())

      # Parallel execution
      cat("   Running", n_folds, "folds in parallel...\n")
      cv_results <- parLapply(cluster, 1:n_folds, function(i) {
        evaluate_single_fold(fold_data[[i]], models_to_fit)
      })

    } else {
      # Sequential execution with progress
      cat("   Running", n_folds, "folds sequentially...\n")
      cv_results <- lapply(1:n_folds, function(i) {
        cat("   Fold", i, "of", n_folds, "...")
        result <- evaluate_single_fold(fold_data[[i]], models_to_fit)
        cat(" completed\n")
        return(result)
      })
    }

    # Process results
    aucs <- process_cv_results(cv_results, models_to_fit)
    cv_summary <- calculate_cv_summary(aucs)

    cat("   ✓ Parallel cross-validation completed\n")
    cat("   Cross-validation results:\n")
    print(cv_summary)

    return(list(aucs = aucs, summary = cv_summary))

  }, finally = {
    # Clean up local cluster if we created it
    if (local_cluster) {
      cleanup_parallel_processing(cluster)
    }
  })
}

#' Prepare fold data for parallel processing
#' @param model_data Full model dataset
#' @param spatial_blocks Spatial blocks object
#' @return List of fold data objects
prepare_cv_fold_data <- function(model_data, spatial_blocks) {

  fold_data <- list()

  for (i in 1:spatial_blocks$k) {
    # Get fold indices
    if ("foldID" %in% names(spatial_blocks)) {
      test_idx <- which(spatial_blocks$foldID == i)
      train_idx <- which(spatial_blocks$foldID != i)
    } else {
      test_idx <- spatial_blocks$folds[[i]][[2]]
      train_idx <- spatial_blocks$folds[[i]][[1]]
    }

    train_data <- model_data[train_idx, ]
    test_data <- model_data[test_idx, ]

    # Check for sufficient data
    sufficient_data <- (nrow(train_data) >= 20 && nrow(test_data) >= 5 &&
                       sum(train_data$presence) >= 5 && sum(test_data$presence) >= 2)

    fold_data[[i]] <- list(
      fold_id = i,
      train_data = train_data,
      test_data = test_data,
      sufficient_data = sufficient_data
    )
  }

  return(fold_data)
}

#' Evaluate a single CV fold
#' @param fold_data Single fold data from prepare_cv_fold_data()
#' @param models_to_fit Vector of model names
#' @return Named vector of AUC values
evaluate_single_fold <- function(fold_data, models_to_fit) {

  if (!fold_data$sufficient_data) {
    return(setNames(rep(NA_real_, length(models_to_fit)), models_to_fit))
  }

  aucs <- numeric(length(models_to_fit))
  names(aucs) <- models_to_fit

  # Fit and evaluate each model
  for (model_name in models_to_fit) {
    tryCatch({

      # Fit model
      model <- switch(model_name,
                     "rf" = fit_rf_model(fold_data$train_data),
                     "gbm" = fit_gbm_model(fold_data$train_data),
                     "gam" = fit_gam_model(fold_data$train_data),
                     "inla" = fit_inla_model_adaptive(fold_data$train_data),
                     stop("Unknown model: ", model_name))

      if (!is.null(model)) {
        # Make predictions
        if (model_name == "inla" && !is.null(model$type)) {
          # Handle INLA predictions
          if (model$type == "spatial") {
            predictions <- predict_spatial_inla(model, fold_data$test_data)
          } else {
            predictions <- predict_nonspatial_inla(model, fold_data$test_data)
          }
        } else {
          # Standard predictions
          predictions <- predict(model, newdata = fold_data$test_data, type = "prob")
          if (is.matrix(predictions)) {
            predictions <- predictions[, 2]  # Take probability of presence
          } else if (is.data.frame(predictions)) {
            predictions <- predictions[[2]]  # Take second column
          }
        }

        # Calculate AUC
        if (length(predictions) == nrow(fold_data$test_data)) {
          roc_obj <- pROC::roc(fold_data$test_data$presence, as.numeric(predictions),
                              quiet = TRUE)
          aucs[model_name] <- as.numeric(roc_obj$auc)
        }
      }

    }, error = function(e) {
      # Model failed - leave as NA
    })
  }

  return(aucs)
}

#' Process CV results into AUC dataframe
#' @param cv_results List of results from evaluate_single_fold()
#' @param models_to_fit Vector of model names
#' @return Dataframe with fold results
process_cv_results <- function(cv_results, models_to_fit) {

  n_folds <- length(cv_results)
  aucs <- data.frame(fold = 1:n_folds)

  for (model_name in models_to_fit) {
    aucs[[model_name]] <- sapply(cv_results, function(x) x[model_name])
  }

  return(aucs)
}

#' Calculate CV summary statistics
#' @param aucs AUC dataframe from process_cv_results()
#' @return Summary statistics dataframe
calculate_cv_summary <- function(aucs) {

  cv_summary <- aucs %>%
    dplyr::select(-fold) %>%
    summarise(across(everything(), list(
      mean = ~mean(.x, na.rm = TRUE),
      se = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))),
      n_folds = ~sum(!is.na(.x))
    ))) %>%
    tidyr::pivot_longer(everything()) %>%
    tidyr::separate(name, into = c("model", "stat"), sep = "_") %>%
    tidyr::pivot_wider(names_from = stat, values_from = value)

  return(cv_summary)
}

# MEMORY OPTIMIZATION ========================================================

#' Batch prediction for large grids
#' @param model Fitted model object
#' @param prediction_data Large prediction dataset
#' @param batch_size Number of rows per batch
#' @param progress Whether to show progress
#' @return Vector of predictions
predict_in_batches <- function(model, prediction_data,
                              batch_size = get_config(c("performance", "batch_size"), 10000),
                              progress = TRUE) {

  n_rows <- nrow(prediction_data)
  n_batches <- ceiling(n_rows / batch_size)

  if (n_batches == 1) {
    # Small dataset - predict normally
    return(predict(model, newdata = prediction_data, type = "response"))
  }

  if (progress) {
    cat("   Predicting", n_rows, "points in", n_batches, "batches of", batch_size, "...\n")
  }

  predictions <- numeric(n_rows)

  for (i in 1:n_batches) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, n_rows)
    batch_data <- prediction_data[start_idx:end_idx, ]

    tryCatch({
      batch_predictions <- predict(model, newdata = batch_data, type = "response")
      predictions[start_idx:end_idx] <- batch_predictions

      if (progress && i %% max(1, n_batches %/% 10) == 0) {
        cat("     Batch", i, "of", n_batches, "completed\n")
      }

      # Periodic garbage collection
      if (i %% get_config(c("performance", "gc_interval"), 5) == 0) {
        gc(verbose = FALSE)
      }

    }, error = function(e) {
      cat("     Warning: Batch", i, "failed:", e$message, "\n")
      predictions[start_idx:end_idx] <- NA
    })
  }

  return(predictions)
}

#' Custom batch prediction for models with special requirements
#' @param predict_func Custom prediction function
#' @param model Fitted model object
#' @param prediction_data Large prediction dataset
#' @param batch_size Number of rows per batch
#' @param progress Whether to show progress
#' @return Vector of predictions
predict_in_batches_custom <- function(predict_func, model, prediction_data,
                                    batch_size = get_config(c("performance", "batch_size"), 10000),
                                    progress = TRUE) {

  n_rows <- nrow(prediction_data)
  n_batches <- ceiling(n_rows / batch_size)

  if (n_batches == 1) {
    # Small dataset - predict normally
    return(predict_func(model, newdata = prediction_data))
  }

  if (progress) {
    cat("   Predicting", n_rows, "points in", n_batches, "batches of", batch_size, "...\n")
  }

  predictions <- numeric(n_rows)

  for (i in 1:n_batches) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, n_rows)
    batch_data <- prediction_data[start_idx:end_idx, ]

    tryCatch({
      batch_predictions <- predict_func(model, newdata = batch_data)
      predictions[start_idx:end_idx] <- batch_predictions

      if (progress && i %% max(1, n_batches %/% 10) == 0) {
        cat("     Batch", i, "of", n_batches, "completed\n")
      }

      # Periodic garbage collection
      if (i %% get_config(c("performance", "gc_interval"), 5) == 0) {
        gc(verbose = FALSE)
      }

    }, error = function(e) {
      cat("     Warning: Batch", i, "failed:", e$message, "\n")
      predictions[start_idx:end_idx] <- NA
    })
  }

  return(predictions)
}

#' Memory usage monitoring
#' @return Memory usage information
check_memory_usage <- function() {
  if (.Platform$OS.type == "windows") {
    # Windows memory check
    mem_info <- memory.limit()
    mem_used <- memory.size()
    mem_available <- mem_info - mem_used
  } else {
    # Unix-like systems
    mem_info <- system("free -m | awk 'NR==2{printf \"%.1f %.1f\", $3/1024, $2/1024}'",
                      intern = TRUE)
    mem_parts <- as.numeric(strsplit(mem_info, " ")[[1]])
    mem_used <- mem_parts[1]
    mem_available <- mem_parts[2] - mem_parts[1]
  }

  mem_limit_gb <- get_config(c("performance", "memory_limit_gb"), 4)

  if (mem_used > mem_limit_gb) {
    warning("Memory usage (", round(mem_used, 1), "GB) exceeds limit (",
            mem_limit_gb, "GB). Consider reducing batch size.")
  }

  return(list(used_gb = mem_used, available_gb = mem_available))
}

# BATCH PROCESSING FOR SPECIES ===============================================

#' Run SDM for multiple species in parallel
#' @param species_list Vector of species names
#' @param cluster Parallel cluster (optional)
#' @return List of results for each species
run_species_sdm_parallel <- function(species_list, cluster = NULL) {

  cat("Running SDM for", length(species_list), "species...\n")

  # Setup parallel processing if not provided
  local_cluster <- FALSE
  if (is.null(cluster) && get_config(c("performance", "use_parallel"), TRUE)) {
    cluster <- setup_parallel_processing()
    local_cluster <- TRUE
  }

  tryCatch({

    if (!is.null(cluster)) {
      # Load required packages and scripts on each worker
      cat("   Setting up parallel workers...\n")
      clusterEvalQ(cluster, {
        source("utilities.R")
        source("sdm_modelling.R")
      })

      # Export necessary variables and functions
      clusterExport(cluster, c("run_sdm_analysis", "OUTPUT_PATH", "final_improved_pseudo_absences"),
                   envir = .GlobalEnv)

      # Parallel species processing
      cat("   Processing species in parallel...\n")
      results <- parLapply(cluster, species_list, function(species) {
        tryCatch({
          run_sdm_analysis(species)
        }, error = function(e) {
          list(species = species, error = e$message, success = FALSE)
        })
      })

    } else {
      # Sequential processing
      cat("   Processing species sequentially...\n")
      results <- lapply(species_list, function(species) {
        cat("   Processing", species, "...\n")
        tryCatch({
          run_sdm_analysis(species)
        }, error = function(e) {
          cat("   ✗ Failed:", e$message, "\n")
          list(species = species, error = e$message, success = FALSE)
        })
      })
    }

    # Summary
    successful <- sum(sapply(results, function(x) is.null(x$error) || x$success))
    cat("✓ Completed SDM for", successful, "of", length(species_list), "species\n")

    return(results)

  }, finally = {
    # Clean up local cluster if we created it
    if (local_cluster) {
      cleanup_parallel_processing(cluster)
    }
  })
}
