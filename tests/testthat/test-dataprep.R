
library(testthat)
library(terra)

# Dummy raster and dummy SpatRasterCollection
make_raster <- function(ext = c(-100, 100, -100, 100), ncols = 50, nrows = 50,
                        name = "layer") {
  r <- terra::rast(ncols = ncols, nrows = nrows, ext = terra::ext(ext))
  terra::values(r) <- seq_len(terra::ncell(r))
  names(r)  <- name
  r
  }

make_sprc <- function(...) terra::sprc(...)

# Dummy track
make_track <- function(n = 30) {
  r     <- make_raster()
  sprc  <- make_sprc(r)
  simulateTrack(
    numericLayers = sprc,
    betas         = 1,
    nSteps        = n,
    nChoiceSet    = 20
  )
}
base_track <- make_track(n = 30)

# Dummy random steps objects
base_rs_kde  <- createRandomSteps(base_track, estimator = "density",     n_control = 5)
base_rs_trad <- createRandomSteps(base_track, estimator = "traditional", n_control = 5)



# Function: createRandomSteps (input validation)

test_that("createRandomSteps: error if x is not an amt object", {
  expect_error(
    createRandomSteps(data.frame(x = 1:5, y = 1:5)),
    "amt"
  )
})

test_that("createRandomSteps: error if n_control is not a positive integer", {
  expect_error(
    createRandomSteps(base_track, n_control = 0),
    "n_control"
  )
  expect_error(
    createRandomSteps(base_track, n_control = 2.5),
    "n_control"
  )
  expect_error(
    createRandomSteps(base_track, n_control = -3),
    "n_control"
  )
})

test_that("createRandomSteps: error for unknown estimator", {
  expect_error(
    createRandomSteps(base_track, estimator = "Mika"),
    '"density", "traditional"'
  )
})



# Function: createRandomSteps (output control for estimator = density)

test_that("createRandomSteps (density): output has class random_steps_prop", {
  expect_s3_class(base_rs_kde, "random_steps_prop")
})

test_that("createRandomSteps (density): output also inherits random_steps", {
  expect_true(inherits(base_rs_kde, "random_steps"))
})

test_that("createRandomSteps (density): correct number of rows", {
  n_obs     <- sum(base_rs_kde$case_)
  n_control <- 5
  expect_equal(nrow(base_rs_kde), n_obs * (n_control + 1))
})

test_that("createRandomSteps (density): case_ column is logical", {
  expect_type(base_rs_kde$case_, "logical")
})

test_that("createRandomSteps (density): ratio observed to random is 1 : n_control", {
  n_true  <- sum( base_rs_kde$case_)
  n_false <- sum(!base_rs_kde$case_)
  expect_equal(n_false / n_true, 5)
})

test_that("createRandomSteps (density): probability column is positive numeric", {
  expect_type(base_rs_kde$probability, "double")
  expect_true(all(base_rs_kde$probability > 0, na.rm = TRUE))
})

test_that("createRandomSteps (density): step_id_ is present and groups correctly", {
  expect_true("step_id_" %in% names(base_rs_kde))
  counts <- table(base_rs_kde$step_id_)
  expect_true(all(counts == 6))
})

test_that("createRandomSteps (density): step lengths of random steps are positive", {
  random_sl <- base_rs_kde$sl_[!base_rs_kde$case_]
  expect_true(all(random_sl > 0, na.rm = TRUE))
})

test_that("createRandomSteps (density): turning angles of random steps are in (-pi, pi)", {
  random_ta <- base_rs_kde$ta_[!base_rs_kde$case_]
  expect_true(all(random_ta >= -pi & random_ta <= pi, na.rm = TRUE))
})

test_that("createRandomSteps (density): accepts steps_xyt as input directly", {
  steps_input <- amt::steps(base_track)
  out <- createRandomSteps(steps_input, estimator = "density", n_control = 5)
  expect_s3_class(out, "random_steps_prop")
})



# Function: createRandomSteps (output control for estimator = traditional)

test_that("createRandomSteps (traditional): output has class random_steps_prop", {
  expect_s3_class(base_rs_trad, "random_steps_prop")
})

test_that("createRandomSteps (traditional): correct ratio observed to random", {
  n_true  <- sum( base_rs_trad$case_)
  n_false <- sum(!base_rs_trad$case_)
  expect_equal(n_false / n_true, 5)
})

test_that("createRandomSteps (traditional): probability column is positive", {
  expect_true(all(base_rs_trad$probability > 0, na.rm = TRUE))
})

test_that("createRandomSteps (traditional): step_id_ groups have correct size", {
  counts <- table(base_rs_trad$step_id_)
  expect_true(all(counts == 6))
})



# Function: createRandomSteps (test n_control parameter)

test_that("createRandomSteps: n_control = 1 works and produces correct structure", {
  rs <- createRandomSteps(base_track, n_control = 1)
  n_true  <- sum( rs$case_)
  n_false <- sum(!rs$case_)
  expect_equal(n_false / n_true, 1)
})

test_that("createRandomSteps: larger n_control produces more rows", {
  rs_5  <- createRandomSteps(base_track, n_control = 5)
  rs_15 <- createRandomSteps(base_track, n_control = 15)
  expect_gt(nrow(rs_15), nrow(rs_5))
})


# Function: extractStepCovariates (input validation)

base_r    <- make_raster(name = "cov1")
base_sprc <- make_sprc(base_r)

test_that("extractStepCovariates: error if x is not random_steps_prop", {
  expect_error(
    extractStepCovariates(data.frame(x2_ = 0, y2_ = 0),
                          numericLayers = base_sprc),
    "random_steps_prop"
  )
})

test_that("extractStepCovariates: error if no layers provided", {
  expect_error(
    extractStepCovariates(base_rs_kde,
                          numericLayers = NULL,
                          imageLayers   = NULL),
    "least one"
  )
})

test_that("extractStepCovariates: error if numericLayers is not SpatRasterCollection", {
  expect_error(
    extractStepCovariates(base_rs_kde, numericLayers = base_r),
    "SpatRasterCollection"
  )
})

test_that("extractStepCovariates: error if imageLayers is not SpatRasterCollection", {
  expect_error(
    extractStepCovariates(base_rs_kde, imageLayers = base_r),
    "SpatRasterCollection"
  )
})

test_that("extractStepCovariates: error if imageKernelDimensions length mismatch", {
  img_sprc <- make_sprc(make_raster(name = "img1"))
  expect_error(
    extractStepCovariates(base_rs_kde,
                          imageLayers           = img_sprc,
                          imageKernelDimensions = c(3, 5)),
    "imageKernelDimensions"
  )
})

test_that("extractStepCovariates: error if imageKernelFunctions length mismatch", {
  img_sprc <- make_sprc(make_raster(name = "img1"))
  expect_error(
    extractStepCovariates(base_rs_kde,
                          imageLayers           = img_sprc,
                          imageKernelDimensions = c(3),
                          imageKernelFunctions  = list(mean, max)),
    "imageKernelFunctions"
  )
})

test_that("extractStepCovariates: error if imageKernelDimension is even", {
  img_sprc <- make_sprc(make_raster(name = "img1"))
  expect_error(
    extractStepCovariates(base_rs_kde,
                          imageLayers           = img_sprc,
                          imageKernelDimensions = c(4)),
    "odd"
  )
})


# Function: extractStepCovariates (output control)

test_that("extractStepCovariates: output has class steps_covariates", {
  out <- extractStepCovariates(base_rs_kde, numericLayers = base_sprc)
  expect_s3_class(out, "steps_covariates")
})

test_that("extractStepCovariates: inherits classes from input", {
  out <- extractStepCovariates(base_rs_kde, numericLayers = base_sprc)
  expect_true(inherits(out, "random_steps_prop"))
  expect_true(inherits(out, "random_steps"))
})

test_that("extractStepCovariates: number of rows unchanged", {
  out <- extractStepCovariates(base_rs_kde, numericLayers = base_sprc)
  expect_equal(nrow(out), nrow(base_rs_kde))
})

test_that("extractStepCovariates: numeric layer column added with correct name", {
  out <- extractStepCovariates(base_rs_kde, numericLayers = base_sprc)
  expect_true("cov1" %in% names(out))
})

test_that("extractStepCovariates: image layer column added with correct name", {
  img_sprc <- make_sprc(make_raster(name = "img1"))
  out <- extractStepCovariates(base_rs_kde,
                               imageLayers           = img_sprc,
                               imageKernelDimensions = c(3),
                               imageKernelFunctions  = list(mean))
  expect_true("img1" %in% names(out))
})

test_that("extractStepCovariates: multiple numeric layers all added", {
  r2    <- make_raster(name = "cov2")
  sprc2 <- make_sprc(base_r, r2)
  out   <- extractStepCovariates(base_rs_kde, numericLayers = sprc2)
  expect_true("cov1" %in% names(out))
  expect_true("cov2" %in% names(out))
})

test_that("extractStepCovariates: covariate values are numeric", {
  out <- extractStepCovariates(base_rs_kde, numericLayers = base_sprc)
  expect_type(out$cov1, "double")
})

test_that("extractStepCovariates: scaling to (0,1) works (no values outside range)", {
  out <- extractStepCovariates(base_rs_kde, numericLayers = base_sprc,
                               scaleNumericLayers = TRUE)
  vals <- out$cov1[!is.na(out$cov1)]
  expect_true(all(vals >= 0 & vals <= 1))
})

test_that("extractStepCovariates: scaleNumericLayers = FALSE does not clip to (0,1)", {
  out_raw    <- extractStepCovariates(base_rs_kde, numericLayers = base_sprc,
                                      scaleNumericLayers = FALSE)
  out_scaled <- extractStepCovariates(base_rs_kde, numericLayers = base_sprc,
                                      scaleNumericLayers = TRUE)
  expect_true(max(out_raw$cov1, na.rm = TRUE) > 1)
  expect_lte(max(out_scaled$cov1, na.rm = TRUE), 1)
})

test_that("extractStepCovariates: coordinates outside extent produce NA with warning", {
  small_r    <- make_raster(ext = c(50, 100, 50, 100), name = "small")
  small_sprc <- make_sprc(small_r)
  expect_warning(
    out <- extractStepCovariates(base_rs_kde, numericLayers = small_sprc),
    "extent"
  )
  expect_true(any(is.na(out$small)))
})

test_that("extractStepCovariates: works with only imageLayers (no numericLayers)", {
  img_sprc <- make_sprc(make_raster(name = "img1"))
  out <- extractStepCovariates(base_rs_kde,
                               numericLayers         = NULL,
                               imageLayers           = img_sprc,
                               imageKernelDimensions = c(3),
                               imageKernelFunctions  = list(mean))
  expect_s3_class(out, "steps_covariates")
  expect_true("img1" %in% names(out))
})

test_that("extractStepCovariates: works with both numeric and image layers", {
  img_sprc <- make_sprc(make_raster(name = "img1"))
  out <- extractStepCovariates(base_rs_kde,
                               numericLayers         = base_sprc,
                               imageLayers           = img_sprc,
                               imageKernelDimensions = c(3),
                               imageKernelFunctions  = list(mean))
  expect_true("cov1" %in% names(out))
  expect_true("img1" %in% names(out))
})

test_that("extractStepCovariates: also works with traditional random steps as input", {
  out <- extractStepCovariates(base_rs_trad, numericLayers = base_sprc)
  expect_s3_class(out, "steps_covariates")
  expect_true("cov1" %in% names(out))
})

test_that("extractStepCovariates: different kernel functions produce different results", {
  img_sprc <- make_sprc(make_raster(name = "img1"))
  out_mean <- extractStepCovariates(base_rs_kde,
                                    imageLayers           = img_sprc,
                                    imageKernelDimensions = c(3),
                                    imageKernelFunctions  = list(mean))
  out_max  <- extractStepCovariates(base_rs_kde,
                                    imageLayers           = img_sprc,
                                    imageKernelDimensions = c(3),
                                    imageKernelFunctions  = list(max))
  expect_true(all(out_max$img1 >= out_mean$img1, na.rm = TRUE))
})
