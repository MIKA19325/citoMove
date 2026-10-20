# Example workflow for createRandomSteps(), extractStepCovariates() and
# extractStepImagekernels()

# This example shows how to:
#   1. Create habitat layers
#   2. Simulate an animal track with simulateTrack()
#      (see simulation-example.R for a more detailed example with plots)
#   3. Generate random steps with createRandomSteps()
#   4. Attach habitat covariates with extractStepCovariates()
#      (tabular input, e.g. for dnn_ssf())
#   5. Attach image kernels with extractStepImagekernels()
#      (image input, e.g. for cnn_ssf())

set.seed(42)


# Step 1: Create habitat layers

# Starting point of the track, used as centre of all layers (EPSG:25833):
centerX <- 820778.84
centerY <- 5433014.65

# Continuous numeric layer (e.g. elevation, NDVI):
layer1 <- terra::rast(
  ncols = 300,
  nrows = 450,
  ext   = terra::ext(centerX - 100, centerX + 100,
                     centerY - 150, centerY + 150)
)
terra::crs(layer1) <- "EPSG:25833"

terra::values(layer1) <- stats::rnorm(terra::ncell(layer1))

layer1 <- terra::focal(
  layer1,
  w         = terra::focalMat(layer1, 3, "Gauss"),
  na.policy = "omit",
  expand    = TRUE
)

names(layer1) <- "elevation"


# Binary numeric layer (e.g. forest cover):
layer2 <- terra::rast(
  ncols = 300,
  nrows = 200,
  ext   = terra::ext(centerX - 150, centerX + 150,
                     centerY - 100, centerY + 100)
)
terra::crs(layer2) <- "EPSG:25833"

terra::values(layer2) <- stats::rnorm(terra::ncell(layer2))

layer2 <- terra::focal(
  layer2,
  w         = terra::focalMat(layer2, 15, "Gauss"),
  na.policy = "omit",
  expand    = TRUE
)

threshold <- stats::quantile(terra::values(layer2), 0.5, na.rm = TRUE)
layer2    <- as.numeric(layer2 >= threshold)

names(layer2) <- "forest"

# Combine the numeric layers into a SpatRasterCollection:
numLayers <- terra::sprc(layer1, layer2)


# Image layer (e.g. canopy structure derived from aerial imagery).
# With real data, use an image; here we simulate a layer as above.
dummyImage <- terra::rast(
  ncols = 25,
  nrows = 25,
  ext   = terra::ext(centerX - 200, centerX + 200,
                     centerY - 200, centerY + 200)
)
terra::crs(dummyImage) <- "EPSG:25833"

terra::values(dummyImage) <- stats::runif(terra::ncell(dummyImage))
names(dummyImage) <- "canopy"

imgLayers <- terra::sprc(dummyImage)


# Step 2: Simulate a track (alternatively, load your own amt track of class
# track_xyt)

# Short steps (mean step length shapeSL * scaleSL = 2 m) keep the track inside
# the area covered by all layers, so no covariates are missing in Step 4.
track <- simulateTrack(
  xStart                = centerX,
  yStart                = centerY,
  crs                   = "EPSG:25833",
  tStart                = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
  timeStep              = "hour",
  angleStart            = 0,
  nSteps                = 50,
  nChoiceSet            = 100,
  scaleSL               = 1,
  shapeSL               = 2,
  kappaTA               = 2,
  numericLayers         = numLayers,
  betas                 = c(2, -1),
  imageLayers           = imgLayers,
  imageKernelDimensions = c(3),
  imageKernelFunctions  = list(f1 = mean),
  imageBetas            = c(3),
  scaleNumericLayers    = TRUE,
  scaleImageLayers      = TRUE
)


# Step 3: Generate random steps with createRandomSteps()

# createRandomSteps() accepts track_xy(t) and steps_xy(t) objects; tracks are
# converted to steps internally with amt::steps().
#
# Three estimators of the movement kernel are available:
#   "density"          - bivariate kernel density estimate of step lengths and
#                        turning angles; captures correlated and multimodal
#                        movement distributions
#   "traditional"      - gamma distribution (step length) and von Mises
#                        distribution (turning angle), as in the classic iSSF
#   "density_advanced" - random forest density surface sampled with MCMC;
#                        most flexible, but slower

# Option A (default): KDE-based random steps:
randomSteps_kde <- createRandomSteps(
  x         = track,
  estimator = "density",
  n_control = 10         # number of random steps per observed step
)

# Option B: Traditional gamma + von Mises random steps:
randomSteps_trad <- createRandomSteps(
  x         = track,
  estimator = "traditional",
  n_control = 10
)

# Option C: Random forest + MCMC random steps (not run here, as the MCMC
# sampling takes considerably longer):
# randomSteps_rf <- createRandomSteps(
#   x            = track,
#   estimator    = "density_advanced",
#   n_control    = 10,
#   rf_ntree     = 500,
#   rf_n_bins    = 30,
#   sl_transform = "log1p",
#   mcmc_burn_in = 1000
# )

# All outputs are of class random_steps_prop (inheriting from random_steps).
# Each observed step (case_ = TRUE) is followed by its n_control random steps
# (case_ = FALSE), all sharing the same step_id_.
# The column 'density' holds the density of each step under the estimated
# movement kernel. dnn_ssf() uses this column automatically to correct for the
# non-uniform sampling of random steps; for cnn_ssf(), pass it as
# density = x$density.

head(randomSteps_kde)
summary(randomSteps_kde$density)


# Step 4: Attach habitat covariates with extractStepCovariates()

# extractStepCovariates() extracts covariate values at the end point
# (x2_, y2_) of each step (observed and random): the cell value for numeric
# layers, and the aggregated values of a window of cells around the end point
# for image layers.
#
# Column names are taken from names() of each layer, so make sure your layers
# are named (see Step 1). End points outside a layer's extent are set to NA
# with a warning; before fitting dnn_ssf(), remove complete strata
# (all rows of a step_id_) that contain NA.
#
# scaleNumericLayers / scaleImageLayers rescale each layer to the range 0 to 1
# before extraction, which makes coefficients comparable across layers.

stepsWithCovariates <- extractStepCovariates(
  x                     = randomSteps_kde,
  numericLayers         = numLayers,
  imageLayers           = imgLayers,
  imageKernelDimensions = c(3),
  imageKernelFunctions  = list(f1 = mean),
  scaleNumericLayers    = TRUE,
  scaleImageLayers      = TRUE
)

# The output has class steps_covariates and contains one additional column per
# layer:
head(stepsWithCovariates)


# Step 5: Attach image kernels with extractStepImagekernels()

# extractStepImagekernels() reads images from all GeoTIFF files in a folder
# (e.g. orthophoto tiles) and cuts out a square image section around the end
# point of every step. Here, we create a small artificial 3-band (RGB) image
# with 1 m resolution, split it into two tiles and write them to a temporary
# folder. With real data, tifDirectory simply points to the folder containing
# your image tiles.

ortho <- terra::rast(
  ncols = 400,
  nrows = 400,
  nlyrs = 3,
  ext   = terra::ext(centerX - 200, centerX + 200,
                     centerY - 200, centerY + 200),
  crs   = "EPSG:25833"
)
terra::values(ortho) <- round(stats::runif(terra::ncell(ortho) * 3, min = 0, max = 255))
names(ortho) <- c("red", "green", "blue")

tifDir <- file.path(tempdir(), "orthophoto")
dir.create(tifDir, showWarnings = FALSE)

westTile <- terra::crop(ortho, terra::ext(centerX - 200, centerX,
                                          centerY - 200, centerY + 200))
eastTile <- terra::crop(ortho, terra::ext(centerX, centerX + 200,
                                          centerY - 200, centerY + 200))
terra::writeRaster(westTile, file.path(tifDir, "tile_west.tif"),
                   datatype = "INT1U", overwrite = TRUE)
terra::writeRaster(eastTile, file.path(tifDir, "tile_east.tif"),
                   datatype = "INT1U", overwrite = TRUE)

# The tiles are combined into one virtual mosaic internally, so the kernels
# can also be cut out across tile borders.
stepsWithKernels <- extractStepImagekernels(
  x            = stepsWithCovariates,  # also works directly on randomSteps_kde
  tifDirectory = tifDir,
  kernelRadius = 5,                    # end point +- 5 m -> 10 x 10 cells
  imageScaling = c(0, 255),            # known value range of 8-bit images
  colName      = "ortho"
)

# Each element of the new column is an array [rows, columns, bands] with
# values between 0 and 1:
dim(stepsWithKernels$ortho[[1]])

# cnn_ssf() needs kernels of identical size. Check the kernel dimensions:
kernelDims <- sapply(stepsWithKernels$ortho,
                     function(a) paste(dim(a), collapse = " x "))
table(kernelDims)

# If all kernels have the same size, they can be combined into the array
# [steps, bands, rows, columns] expected by cnn_ssf():
if (length(unique(kernelDims)) == 1) {
  X <- aperm(simplify2array(stepsWithKernels$ortho), c(4, 3, 1, 2))
  dim(X)
}
