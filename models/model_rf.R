# models/model_rf.R
# fit_rf(model_data): fit a Random Forest presence/absence model
# model_data must contain: depth_m, sst, sss, dist_to_coast_km, MSFD_BBHT, presence

fit_rf <- function(model_data) {
  cat("Fitting Random Forest...\n")
  df            <- na.omit(model_data)
  df$presence   <- factor(df$presence, levels = c(0, 1))
  df$MSFD_BBHT  <- as.factor(df$MSFD_BBHT)

  if (length(levels(df$presence)) < 2)
    stop("Need both presence (1) and absence (0) records for RF.")

  rf_model <- randomForest(
    presence ~ depth_m + sst + sss + dist_to_coast_km + MSFD_BBHT,
    data       = df,
    ntree      = 500,
    mtry       = 2,
    maxnodes   = 20,
    nodesize   = 50,
    sampsize   = floor(0.6 * nrow(df)),
    importance = TRUE,
    proximity  = FALSE,
    na.action  = na.omit
  )
  rf_model
}
