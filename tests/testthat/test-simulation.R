# Tests for simulateTrack()

# Dummy raster and SpatRasterCollection
make_raster <- function(ext = c(-10, 10, -10, 10), ncols = 10, nrows = 10) {
  r <- terra::rast(ncols = ncols, nrows = nrows, ext = terra::ext(ext))
  terra::values(r) <- seq_len(terra::ncell(r))
  r
}

make_sprc <- function(...) terra::sprc(...)

# Basic raster for tests
base_r    <- make_raster(ext = c(-100, 100, -100, 100), ncols = 50, nrows = 50)
base_sprc <- make_sprc(base_r)

set.seed(1)


# Function: simulateTrack (input validation)

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

test_that("simulateTrack: error for angleStart outside of [0, 360)", {
  expect_error(
    simulateTrack(numericLayers = base_sprc, betas = 1, angleStart = 400),
    "angleStart"
  )
  expect_error(
    simulateTrack(numericLayers = base_sprc, betas = 1, angleStart = -10),
    "angleStart"
  )
  expect_error(
    simulateTrack(numericLayers = base_sprc, betas = 1, angleStart = 360),
    "angleStart"
  )
})

test_that("simulateTrack: angleStart = 0 is allowed", {
  expect_no_error(
    simulateTrack(numericLayers = base_sprc, betas = 1, angleStart = 0,
                  nSteps = 5, nChoiceSet = 5)
  )
})

test_that("simulateTrack: error for negative or non-integer nSteps", {
  expect_error(
    simulateTrack(numericLayers = base_sprc, betas = 1, nSteps = -5),
    "nSteps"
  )
  expect_error(
    simulateTrack(numericLayers = base_sprc, betas = 1, nSteps = 1.5),
    "nSteps"
  )
})

test_that("simulateTrack: error if starting coordinates outside of extent", {
  expect_error(
    simulateTrack(numericLayers = base_sprc, betas = 1,
                  xStart = 999, yStart = 0),
    "Starting coordinates"
  )
})

test_that("simulateTrack: error if layers do not overlap", {
  r_west <- make_raster(ext = c(-100, -50, -100, 100))
  r_east <- make_raster(ext = c(50, 100, -100, 100))
  expect_error(
    simulateTrack(numericLayers = make_sprc(r_west, r_east), betas = c(1, 1)),
    "spatial overlap"
  )
})

test_that("simulateTrack: error if crs does not match the layers", {
  r <- make_raster(ext = c(-100, 100, -100, 100), ncols = 50, nrows = 50)
  terra::crs(r) <- "EPSG:25832"
  expect_error(
    simulateTrack(numericLayers = make_sprc(r), betas = 1, crs = 25833),
    "crs must match"
  )
})

test_that("simulateTrack: error if length of betas differs from number of layers", {
  r2    <- make_raster()
  sprc2 <- make_sprc(base_r, r2)

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

test_that("simulateTrack: output has class amt track_xyt", {
  track <- simulateTrack(
    numericLayers = base_sprc, betas = 1,
    nSteps = 10, nChoiceSet = 10
  )
  expect_s3_class(track, "track_xyt")
})

test_that("simulateTrack: track contains nSteps + 1 locations", {
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

test_that("simulateTrack: crs is assigned to the track", {
  r <- make_raster(ext = c(-100, 100, -100, 100), ncols = 50, nrows = 50)
  terra::crs(r) <- "EPSG:25832"
  track <- simulateTrack(
    numericLayers = make_sprc(r), betas = 1, crs = "EPSG:25832",
    nSteps = 5, nChoiceSet = 5
  )
  expect_false(is.na(amt::get_crs(track)))
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


# Function: simulateTrack (internal helpers, tested via their behaviour)

test_that("simulateTrack: constant layers work (no division by zero when scaling)", {
  r <- make_raster(ext = c(-100, 100, -100, 100), ncols = 50, nrows = 50)
  terra::values(r) <- 7
  track <- simulateTrack(
    numericLayers = make_sprc(r), betas = 1,
    nSteps = 10, nChoiceSet = 10
  )
  expect_false(any(is.na(track$x_)))
})

test_that("simulateTrack: image kernels at the layer edge are wrapped toroidally", {
  img_sprc <- make_sprc(make_raster(ext = c(-100, 100, -100, 100),
                                    ncols = 50, nrows = 50))
  expect_no_error(
    track <- simulateTrack(
      numericLayers         = NULL,
      imageLayers           = img_sprc,
      imageBetas            = 1,
      imageKernelDimensions = 5,
      imageKernelFunctions  = list(mean),
      xStart = -99, yStart = -99,
      nSteps = 10, nChoiceSet = 10
    )
  )
  expect_false(any(is.na(track$x_)))
})
