
library(testthat)
library(terra)

# Dummy raster and dummy SpatRasterCollection

make_raster <- function(ext = c(-10, 10, -10, 10), ncols = 10, nrows = 10) {
  r <- terra::rast(ncols = ncols, nrows = nrows, ext = terra::ext(ext))
  terra::values(r) <- seq_len(terra::ncell(r))
  r
  }

make_sprc <- function(...) {terra::sprc(...)}



# Function: cropScaleRaster

test_that("cropScaleRaster: output has correct extend", {
  r   <- make_raster()
  ext <- terra::ext(-5, 5, -5, 5)
  out <- cropScaleRaster(r, ext, scale = FALSE)

  expect_equal(as.numeric(terra::ext(out)[1]), -5)
  expect_equal(as.numeric(terra::ext(out)[2]),  5)
  expect_equal(as.numeric(terra::ext(out)[3]), -5)
  expect_equal(as.numeric(terra::ext(out)[4]),  5)
})

test_that("cropScaleRaster: scales values to [0, 1]", {
  r   <- make_raster()
  ext <- terra::ext(-10, 10, -10, 10)
  out <- cropScaleRaster(r, ext, scale = TRUE)

  vals <- terra::values(out, na.rm = TRUE)
  expect_gte(min(vals), 0)
  expect_lte(max(vals), 1)
  expect_equal(min(vals), 0)
  expect_equal(max(vals), 1)
})

test_that("cropScaleRaster: no scaling for constant rasters (no NA)", {
  r <- terra::rast(ncols = 5, nrows = 5, ext = terra::ext(-5, 5, -5, 5))
  terra::values(r) <- 7
  ext <- terra::ext(-5, 5, -5, 5)
  out <- cropScaleRaster(r, ext, scale = TRUE)

  vals <- terra::values(out, na.rm = TRUE)
  expect_false(any(is.nan(vals)))
  expect_true(all(vals == 7))
})

test_that("cropScaleRaster: scale = FALSE doesnt change values", {
  r    <- make_raster()
  ext  <- terra::ext(-10, 10, -10, 10)
  out  <- cropScaleRaster(r, ext, scale = FALSE)

  expect_equal(
    terra::values(out, na.rm = TRUE),
    terra::values(r,   na.rm = TRUE)
  )
})


# Function: extractKernelValues

test_that("extractKernelValues: output has correct length", {
  r   <- make_raster()
  xs  <- c(0, 2, -3)
  ys  <- c(0, 1,  4)
  out <- extractKernelValues(r, xs, ys, kernelDim = 3, kernelFun = mean)

  expect_length(out, length(xs))
})

test_that("extractKernelValues: kernelDim = 1 equals normal extraction", {
  r  <- make_raster()
  xs <- c(-5, 0, 5)
  ys <- c(-5, 0, 5)

  out_kernel <- extractKernelValues(r, xs, ys, kernelDim = 1, kernelFun = mean)
  out_direct <- terra::extract(r, cbind(xs, ys))[, 1]

  expect_equal(out_kernel, out_direct, tolerance = 1e-6)
})

test_that("extractKernelValues: Output is numeric and not NA", {
  r   <- make_raster()
  out <- extractKernelValues(r, 0, 0, kernelDim = 3, kernelFun = mean)

  expect_type(out, "double")
  expect_false(is.na(out))
})

test_that("extractKernelValues: toroidal borders work", {
  r <- make_raster()
  expect_no_error(
    extractKernelValues(r, xs = -9.9, ys = -9.9, kernelDim = 3, kernelFun = mean)
  )
})


# Function: computeImageValues

test_that("computeImageValues: empty vector if no imageRasters", {
  out <- computeImageValues(
    x = c(0, 1), y = c(0, 1),
    imageRasters          = list(),
    imageBetas            = c(),
    imageKernelDimensions = c(),
    imageKernelFunctions  = list()
  )

  expect_equal(out, c(0, 0))
})

test_that("computeImageValues: output has correct length", {
  r    <- make_raster()
  out  <- computeImageValues(
    x = c(-5, 0, 5), y = c(-5, 0, 5),
    imageRasters          = list(r),
    imageBetas            = 1,
    imageKernelDimensions = 3,
    imageKernelFunctions  = list(mean)
  )

  expect_length(out, 3)
})

test_that("computeImageValues: Beta = 0 results in empty vector", {
  r   <- make_raster()
  out <- computeImageValues(
    x = c(0), y = c(0),
    imageRasters          = list(r),
    imageBetas            = 0,
    imageKernelDimensions = 3,
    imageKernelFunctions  = list(mean)
  )

  expect_equal(out, 0)
})

test_that("computeImageValues: beta has correct effect", {
  r <- make_raster()
  x <- 0; y <- 0

  pos <- computeImageValues(x, y, list(r), imageBetas = 1,
                            imageKernelDimensions = 3,
                            imageKernelFunctions  = list(mean))
  neg <- computeImageValues(x, y, list(r), imageBetas = -1,
                            imageKernelDimensions = 3,
                            imageKernelFunctions  = list(mean))

  expect_equal(pos, -neg)
})


# Function: simulateTrack (input validation)

# basic raster fot tests
base_r    <- make_raster(ext = c(-100, 100, -100, 100), ncols = 50, nrows = 50)
base_sprc <- make_sprc(base_r)

test_that("simulateTrack: error if tStart not POSIXct", {
  expect_error(
    simulateTrack(numericLayers = base_sprc, betas = 1,
                  tStart = "2026-01-01"),
    "POSIXct"
  )
})

test_that("simulateTrack: error for wrong timeStep", {
  expect_error(
    simulateTrack(numericLayers = base_sprc, betas = 1,
                  timeStep = "night"),
    "timeStep"
  )
})

test_that("simulateTrack: error for angleSTart outside of (0, 360)", {
  expect_error(
    simulateTrack(numericLayers = base_sprc, betas = 1, angleStart = 400),
    "angleStart"
  )
  expect_error(
    simulateTrack(numericLayers = base_sprc, betas = 1, angleStart = -10),
    "angleStart"
  )
})

test_that("simulateTrack: error for negative nSteps", {
  expect_error(
    simulateTrack(numericLayers = base_sprc, betas = 1, nSteps = -5),
    "nSteps"
  )
  expect_error(
    simulateTrack(numericLayers = base_sprc, betas = 1, nSteps = 1.5),
    "nSteps"
  )
})

test_that("simulateTrack: error if starting coordinates outside of extend", {
  expect_error(
    simulateTrack(numericLayers = base_sprc, betas = 1,
                  xStart = 999, yStart = 0),
    "Starting coordinates"
  )
})

test_that("simulateTrack: error if length beta not same as number of layers", {
  r2     <- make_raster()
  sprc2  <- make_sprc(base_r, r2)

  expect_error(
    simulateTrack(numericLayers = sprc2, betas = c(1)),
    "betas"
  )
})

test_that("simulateTrack: error if imageKernelDimension is even", {
  img_sprc <- make_sprc(make_raster())
  expect_error(
    simulateTrack(numericLayers = base_sprc, betas = 1,
                  imageLayers = img_sprc, imageBetas = 1,
                  imageKernelDimensions = c(4)),
    "odd"
  )
})

test_that("simulateTrack: error if there is no layer", {
  expect_error(
    simulateTrack(numericLayers = NULL, imageLayers = NULL),
    "at least one layer"
  )
})


# Function: simulateTrack (output control)

test_that("simulateTrack: output as class amt track_xy", {
  track <- simulateTrack(
    numericLayers = base_sprc, betas = 1,
    nSteps = 10, nChoiceSet = 10
  )
  expect_s3_class(track, "track_xyt")
})

test_that("simulateTrack: track adds nSteps", {
  n     <- 15
  track <- simulateTrack(
    numericLayers = base_sprc, betas = 1,
    nSteps = n, nChoiceSet = 10
  )
  expect_equal(nrow(track), n + 1)
})

test_that("simulateTrack: starting coordinates are correct", {
  track <- simulateTrack(
    numericLayers = base_sprc, betas = 1,
    xStart = 5, yStart = -3,
    nSteps = 10, nChoiceSet = 10
  )
  expect_equal(track$x_[1], 5)
  expect_equal(track$y_[1], -3)
})

test_that("simulateTrack: time values are correct", {
  tStart <- as.POSIXct("2025-06-01 12:00:00", tz = "UTC")
  track  <- simulateTrack(
    numericLayers = base_sprc, betas = 1,
    tStart = tStart, timeStep = "hour",
    nSteps = 10, nChoiceSet = 10
  )
  expect_equal(track$t_[1], tStart)
  expect_length(track$t_, 11)
})

test_that("simulateTrack: track has no NA", {
  track <- simulateTrack(
    numericLayers = base_sprc, betas = 1,
    nSteps = 20, nChoiceSet = 10
  )
  expect_false(any(is.na(track$x_)))
  expect_false(any(is.na(track$y_)))
})

test_that("simulateTrack: also works with only imageLayers", {
  img_sprc <- make_sprc(make_raster(ext = c(-100, 100, -100, 100),
                                    ncols = 50, nrows = 50))
  track <- simulateTrack(
    numericLayers         = NULL,
    imageLayers           = img_sprc,
    imageBetas            = 1,
    imageKernelDimensions = 3,
    imageKernelFunctions  = list(mean),
    nSteps = 10, nChoiceSet = 10
  )
  expect_s3_class(track, "track_xyt")
})
