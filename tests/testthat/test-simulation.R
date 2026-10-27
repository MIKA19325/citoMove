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

test_that("simulateTrack: error if imageKernelDimensions is not a positive integer", {
  img_sprc <- make_sprc(make_raster())
  expect_error(
    simulateTrack(numericLayers = base_sprc, betas = 1,
                  imageLayers = img_sprc, imageBetas = 1,
                  imageKernelDimensions = c(2.5)),
    "positive integers"
  )
  expect_error(
    simulateTrack(numericLayers = base_sprc, betas = 1,
                  imageLayers = img_sprc, imageBetas = 1,
                  imageKernelDimensions = c(0)),
    "positive integers"
  )
})

test_that("simulateTrack: error if torus, kernelAsMatrix or returnChoiceSets is not TRUE/FALSE", {
  expect_error(
    simulateTrack(numericLayers = base_sprc, betas = 1, torus = "yes"),
    "torus"
  )
  expect_error(
    simulateTrack(numericLayers = base_sprc, betas = 1, kernelAsMatrix = NA),
    "kernelAsMatrix"
  )
  expect_error(
    simulateTrack(numericLayers = base_sprc, betas = 1, returnChoiceSets = c(TRUE, FALSE)),
    "returnChoiceSets"
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

# Function: simulateTrack (new options: even kernels, torus, kernelAsMatrix,
# returnChoiceSets)

test_that("simulateTrack: even imageKernelDimensions are accepted", {
  img_sprc <- make_sprc(make_raster(ext = c(-100, 100, -100, 100),
                                    ncols = 50, nrows = 50))
  expect_no_error(
    simulateTrack(
      numericLayers         = NULL,
      imageLayers           = img_sprc,
      imageBetas            = 1,
      imageKernelDimensions = 4,
      imageKernelFunctions  = list(mean),
      nSteps = 10, nChoiceSet = 10
    )
  )
})

test_that("simulateTrack: torus = FALSE keeps the track inside the layers", {
  set.seed(1)
  expect_no_warning(
    track <- simulateTrack(
      numericLayers = base_sprc, betas = 1,
      xStart = 95, yStart = 95,
      scaleSL = 5, kappaTA = 0, torus = FALSE,
      nSteps = 50, nChoiceSet = 50
    )
  )
  expect_equal(nrow(track), 51)
  expect_true(all(track$x_ >= -100 & track$x_ <= 100))
  expect_true(all(track$y_ >= -100 & track$y_ <= 100))
})

test_that("simulateTrack: track ends early with a warning if no candidate is valid", {
  small_sprc <- make_sprc(make_raster(ext = c(-1, 1, -1, 1)))
  expect_warning(
    track <- simulateTrack(
      numericLayers = small_sprc, betas = 1,
      scaleSL = 1000, torus = FALSE,
      nSteps = 10, nChoiceSet = 10
    ),
    "Track ended"
  )
  expect_equal(nrow(track), 1)
})

test_that("simulateTrack: kernelAsMatrix passes the window as a matrix", {
  img_sprc <- make_sprc(make_raster(ext = c(-100, 100, -100, 100),
                                    ncols = 50, nrows = 50))
  checkMatrix <- function(m) {
    stopifnot(is.matrix(m), nrow(m) == 4, ncol(m) == 4)
    mean(m)
  }
  expect_no_error(
    simulateTrack(
      numericLayers         = NULL,
      imageLayers           = img_sprc,
      imageBetas            = 1,
      imageKernelDimensions = 4,
      imageKernelFunctions  = list(checkMatrix),
      kernelAsMatrix        = TRUE,
      nSteps = 10, nChoiceSet = 10
    )
  )
})

test_that("simulateTrack: kernelAsMatrix has row 1 = north and column 1 = west", {
  img_r    <- make_raster(ext = c(-100, 100, -100, 100), ncols = 50, nrows = 50)
  img_sprc <- make_sprc(img_r)
  track <- simulateTrack(
    numericLayers         = NULL,
    imageLayers           = img_sprc,
    imageBetas            = 1,
    imageKernelDimensions = 3,
    imageKernelFunctions  = list(function(m) m[1, 1]),   # north-west cell
    kernelAsMatrix        = TRUE,
    returnChoiceSets      = TRUE,
    nSteps = 10, nChoiceSet = 10
  )
  cs       <- attr(track, "choiceSets")
  imgCol   <- cs[[grep("^img_", names(cs))]]
  cellSize <- terra::res(img_r)[1]
  # value one cell west and one cell north of each candidate, scaled to (0, 1)
  # in the same way as inside simulateTrack()
  expected <- terra::extract(img_r, cbind(cs$x - cellSize, cs$y + cellSize))[, 1]
  expected <- (expected - 1) / (terra::ncell(img_r) - 1)
  expect_equal(imgCol, expected)
})

test_that("simulateTrack: returnChoiceSets returns one row per candidate", {
  nS <- 5
  nC <- 10
  track <- simulateTrack(
    numericLayers = base_sprc, betas = 1,
    returnChoiceSets = TRUE,
    nSteps = nS, nChoiceSet = nC
  )
  cs <- attr(track, "choiceSets")
  expect_s3_class(cs, "data.frame")
  expect_equal(nrow(cs), nS * nC)
  expect_equal(sum(cs$chosen), nS)
  # the chosen candidates are exactly the locations of the track
  expect_equal(cs$x[cs$chosen], track$x_[-1])
  expect_equal(cs$y[cs$chosen], track$y_[-1])
})

test_that("simulateTrack: no choiceSets attribute by default", {
  track <- simulateTrack(
    numericLayers = base_sprc, betas = 1,
    nSteps = 5, nChoiceSet = 5
  )
  expect_null(attr(track, "choiceSets"))
})
