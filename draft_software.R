# Generate entries for each package
draft.software("sf", file = TRUE)
draft.software("dplyr", file = TRUE, append = TRUE)
draft.software("ggplot2", file = TRUE, append = TRUE)
draft.software("randomForest", file = TRUE, append = TRUE)
draft.software("gbm", file = TRUE, append = TRUE)
draft.software("dismo", file = TRUE, append = TRUE)
draft.software("raster", file = TRUE, append = TRUE)
draft.software("mgcv", file = TRUE, append = TRUE)
draft.software("httr", file = TRUE, append = TRUE)
draft.software("robis", file = TRUE, append = TRUE)
draft.software("terra", file = TRUE, append = TRUE)
draft.software("marmap", file = TRUE, append = TRUE)

# For GitHub packages, specify the GitHub reference
draft.software("nielshintzen/vmstools/vmstools@master", file = TRUE, append = TRUE)
draft.software("ices-tools-prod/icesVMS@v1.3.2", file = TRUE, append = TRUE)

# INLA might need special handling as not on githubs
draft.software("INLA", file = TRUE, append = TRUE)
