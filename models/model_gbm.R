# models/model_gbm.R
# fit_gbm(model_data): fit a Gradient Boosted Model for presence/absence
# model_data must contain: depth_m, sst, sss, dist_to_coast_km, MSFD_BBHT, presence

fit_gbm <- function(model_data) {
  cat("Fitting GBM...\n")
  df <- na.omit(model_data)

  if (is.factor(df$presence))
    df$presence <- as.numeric(as.character(df$presence))
  if (!all(df$presence %in% c(0, 1)))
    stop("GBM presence column must contain only 0 and 1.")
  if (length(unique(df$presence)) < 2)
    stop("Need both presence (1) and absence (0) records for GBM.")

  df$MSFD_BBHT <- as.factor(df$MSFD_BBHT)

  gbm_model <- gbm(
    presence ~ depth_m + sst + sss + dist_to_coast_km + MSFD_BBHT,
    data              = df,
    distribution      = "bernoulli",
    n.trees           = 7000,
    interaction.depth = 2,
    shrinkage         = 0.05,
    bag.fraction      = 0.5,
    train.fraction    = 0.8,
    n.minobsinnode    = 20,
    cv.folds          = 5,
    n.cores           = 1,
    verbose           = FALSE
  )

  best_iter <- gbm.perf(gbm_model, method = "cv", plot.it = FALSE)
  cat("Optimal trees (GBM):", best_iter, "\n")
  list(model = gbm_model, best_iter = best_iter)
}
