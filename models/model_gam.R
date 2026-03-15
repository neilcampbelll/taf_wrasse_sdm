# models/model_gam.R
# fit_gam(model_data): fit a GAM for presence/absence using mgcv
# model_data must contain: depth_m, sst, sss, dist_to_coast_km, MSFD_BBHT, presence

fit_gam <- function(model_data) {
  cat("Fitting GAM...\n")
  df           <- na.omit(model_data)
  df$presence  <- as.integer(df$presence)   # binomial family requires numeric 0/1
  df$MSFD_BBHT <- as.factor(df$MSFD_BBHT)

  if (length(unique(df$presence)) < 2)
    stop("Need both presence (1) and absence (0) records for GAM.")

  gam_model <- gam(
    presence ~
      s(depth_m,         k = 3, fx = FALSE) +
      s(sst,             k = 4, fx = FALSE) +
      s(sss,             k = 3, fx = FALSE) +
      s(dist_to_coast_km, k = 4, fx = FALSE) +
      MSFD_BBHT,
    data      = df,
    family    = binomial,
    method    = "REML",
    select    = TRUE,
    gamma     = 1.4,
    na.action = na.omit
  )
  gam_model
}
